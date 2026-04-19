(* bst.ml
 *
 * Lock-free BST based on Natarajan and Mittal,
 * "Fast Concurrent Lock-Free Binary Search Trees", PPoPP 2014.
 *
 * Design decisions:
 * - External (leaf-oriented) BST: only leaf nodes store actual keys
 * - Edge-based marking: flag and tag bits are stored in AtomicFlagTag cells
 * - Keys are ints derived via Hashtbl.hash; caller must ensure no collisions
 * - Structural equality (=) used for value comparison at leaves
 * - Three sentinel keys: inf0 < inf1 < inf2 (as in the paper)
 *)
(* Sentinel keys — must be larger than any Hashtbl.hash result.        *)
let inf0 = max_int - 2
let inf1 = max_int - 1
let inf2 = max_int
(* let hash = Hashtbl.create 20 *)

module AFT = AtomicFlagTag

type 'a node =
  { item : 'a option
  ; key : int
  ; is_leaf : bool
  ; left : 'a edge
  ; right : 'a edge
  }

and 'a edge = 'a node AFT.t

type 'a t = 'a node

type 'a seek_record =
  { ancestor : 'a node (* last node before last untagged edge      *)
  ; successor : 'a node (* child of ancestor on access path         *)
  ; parent : 'a node (* second-to-last node on access path       *)
  ; leaf : 'a node (* last node on access path (a leaf)        *)
  }

let is_leaf node = node.is_leaf

(** Make a leaf node *)
let make_leaf key item =
  let dummy = AFT.make ~flag:false ~tag:false in
  (* leaf nodes — left/right are never followed *)
  { item; key; is_leaf = true; left = dummy (Obj.magic ()); right = dummy (Obj.magic ()) }
;;

(** Make an internal node with given children *)
let make_internal key left right =
  { item = None
  ; key
  ; is_leaf = false
  ; left = AFT.make ~flag:false ~tag:false left
  ; right = AFT.make ~flag:false ~tag:false right
  }
;;

(*        R (inf2)                                                      *)
(*       /        \                                                     *)
(*    S (inf1)   leaf(inf2)                                            *)
(*    /      \                                                          *)
(* leaf(inf0) leaf(inf1)                                               *)
(* ------------------------------------------------------------------ *)
(** [create ()] creates an empty lock-free BST with sentinel structure
    already installed. *)
let create () =
  (* failwith "Not implemented" *)
  let leaf_inf0 = make_leaf inf0 None in
  let leaf_inf1 = make_leaf inf1 None in
  let leaf_inf2 = make_leaf inf2 None in
  let s = make_internal inf1 leaf_inf0 leaf_inf1 in
  (* Root node*)
  let r = make_internal inf2 s leaf_inf2 in
  r
;;

let seek root value =
  (* failwith "Not implemented" *)
  let key = Hashtbl.hash value in
  let s = AFT.get_value root.left in
  let leaf_node = AFT.get_value s.left in
  let ancestor = ref root in
  let successor = ref s in
  let parent = ref s in
  let leaf = ref leaf_node in
  let parent_edge = ref s.left in
  (* field / edge*)
  let current_edge = ref leaf_node.left in
  (* field / edge *)
  let current = ref (AFT.get_value leaf_node.left) in
  (* node *)
  while not (is_leaf !current) do
    if not (AFT.get_tag !parent_edge)
    then (
      ancestor := !parent;
      successor := !current);
    parent := !leaf;
    leaf := !current;
    parent_edge := !current_edge;
    if key < !current.key
    then current_edge := !current.left
    else current_edge := !current.right;
    current := AFT.get_value !current_edge
  done;
  { ancestor = !ancestor; successor = !successor; parent = !parent; leaf = !leaf }
;;

(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. *)
let search _ _ = failwith "Not implemented"

(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already present. *)
let insert _ _ = failwith "Not implemented"

(** [inject record tree k] is the injection step of [delete].
    It flags the incoming edge of [record.leaf] — the edge
    [(record.parent, record.leaf)] — using an atomic CAS.

    Preconditions (from the seek phase, enforced by the caller):
      - [record.leaf] is a leaf
      - [record.leaf.key = k]

    Returns [true] if the CAS succeeds: this delete now "owns" the leaf
    and is guaranteed to eventually complete via [cleanup].
    Returns [false] otherwise, in which case the caller invokes [help]
    and restarts the seek in [Inject] mode. *)
let inject record _tree _k =
  (* Pick the edge of [parent] that points to [leaf].
     BST invariant: keys < parent.key sit on parent.left,
                    keys >= parent.key sit on parent.right. *)
  let edge =
    if record.leaf.key < record.parent.key
    then record.parent.left
    else record.parent.right
  in
  (* Atomically flip flag false -> true, but only if the edge still
     points to [leaf] and is currently unmarked. The value is preserved
     (still points to [leaf]) so that [cleanup] can find the leaf; only
     [cleanup]'s later CAS on [ancestor] actually unlinks it. *)
  AFT.cas
    edge
    ~exp_val:record.leaf
    ~exp_flag:false
    ~exp_tag:false
    ~new_val:record.leaf
    ~new_flag:true
    ~new_tag:false
;;

(** [cleanup record tree k] is the cleanup step of [delete].
    It completes a delete whose injection phase has already flagged the
    [(parent, leaf)] edge.  Two atomic steps:

    1. Tag the sibling edge of [leaf] at [parent] (guaranteed to succeed:
       once the other child edge is flagged, the sibling can only have its
       tag bit flipped, never its target).
    2. CAS [ancestor]'s child field to bypass [parent], replacing the
       edge [(ancestor, successor)] with an edge [(ancestor, sibling)]
       that preserves the sibling edge's flag bit (so a concurrent delete
       targeting the sibling does not lose its mark).

    Returns [true] if the unlink CAS succeeds — the delete is done.
    Returns [false] otherwise; the caller will re-seek and retry in
    [Cleanup] mode. *)
let cleanup record _tree _k =
  (* Step 0: figure out which side [leaf] is on at [parent], and
     which side [successor] is on at [ancestor]. Keys never change,
     so these comparisons are stable. *)
  let leaf_is_left = record.leaf.key < record.parent.key in
  let sibling_edge = if leaf_is_left then record.parent.right else record.parent.left in
  let ancestor_child_edge =
    if record.successor.key < record.ancestor.key
    then record.ancestor.left
    else record.ancestor.right
  in
  (* Step 1: tag the sibling edge. Guaranteed to succeed per the paper:
     no other op can replace this edge's target because the other child
     edge (leaf's edge) is already flagged, making [parent] ineligible
     as an injection point. set_tag leaves flag and value intact. *)
  AFT.set_tag sibling_edge true;
  (* Snapshot the sibling edge *after* tagging to read the node [S] and
     the flag bit we need to carry over. By this point the value and
     flag are frozen (no op can change them), so the snapshot is stable. *)
  let sibling = AFT.get sibling_edge in
  (* Step 2: swing [ancestor]'s child pointer past [parent] to [S].
     - exp_val = successor (what seek observed; if it changed, some
       other op raced ahead and our CAS correctly fails)
     - exp_flag = false, exp_tag = false: seek guarantees this edge was
       untagged, and if it has since been flagged or tagged, we lose
       the race and retry
     - new_val = sibling.value (the node [S])
     - new_flag = sibling.flag (carry over so a concurrent delete of S
       doesn't lose its injection mark)
     - new_tag = false (this is a fresh edge whose tail [ancestor]
       is not being removed by *this* operation). *)
  AFT.cas
    ancestor_child_edge
    ~exp_val:record.successor
    ~exp_flag:false
    ~exp_tag:false
    ~new_val:sibling.value
    ~new_flag:sibling.flag
    ~new_tag:false
;;

(** [help record tree k] is called after a modify operation's CAS fails.
    If a concurrent delete is mid-flight on the edge this op was trying
    to use, we push that delete forward by running its [cleanup] before
    we retry our own op.

    Helping is needed precisely when the [(parent, leaf)] edge:
      - still points to [record.leaf] (the delete hasn't moved on), AND
      - has been marked (flag or tag bit set — a delete has injected).

    If either condition fails, the situation has already resolved itself
    (someone else finished the delete, or the edge was replaced by a
    concurrent insert); we just return and let the caller re-seek. *)
let help record tree k =
  let edge =
    if record.leaf.key < record.parent.key
    then record.parent.left
    else record.parent.right
  in
  let snap = AFT.get edge in
  (* Atomic snapshot: value/flag/tag from one indivisible read. Using
     the snapshot (rather than three separate get_value/get_flag/get_tag
     calls) ensures we don't act on a torn view of the edge. *)
  if snap.value == record.leaf && (snap.flag || snap.tag)
  then
    (* Some delete has injected on this edge. Drive it through its
       cleanup. Our own [record] is a valid cleanup input: seek
       recorded the last untagged edge on the access path down to
       [parent]/[leaf], which is exactly what cleanup needs.
       We return cleanup's return value — although it doesn't really matter, for analysis purposes *)
    cleanup record tree k
  else false
;;

type mode =
  | Inject
  | Cleanup
  | Helping

(** [delete tree k] removes [k] from [tree] if present.
    Returns [true] if the tree changed, and [false] if [k] was not present. *)
let delete tree k =
  let record = seek tree k in
  let rec delete_in_mode mode record =
    match mode with
    | Inject ->
      if inject record tree k
      then delete_in_mode Cleanup record
      else delete_in_mode Helping record
    | Cleanup ->
      if cleanup record tree k then true else delete_in_mode Cleanup (seek tree k)
    | Helping ->
      ignore (help record tree k);
      delete_in_mode Inject (seek tree k)
  in
  delete_in_mode Inject record
;;

(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected tree size. *)
let size _ = failwith "Not implemented"
