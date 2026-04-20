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

let child_edge node key = if key < node.key then node.left else node.right

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

(** The seek phase return a [seek record], which consists of the addresses of four nodes:
    - [leaf node]
    - [parent node]
    - [successor node] : The head of the last untagged edge encountered on the access path before the parent node
    - [ancestor node] : The tail of the last untagged edge encountered on the access path before the parent node

All the nodes on the access path from the successor to the parent node are in the process of being removed *)
let seek root value =
  (* failwith "Not implemented" *)
  let key = Hashtbl.hash value in
  let s = AFT.get_value root.left in
  let leaf_node = AFT.get_value s.left in
  (* Initialize seek record*)
  let ancestor = ref root in
  let successor = ref s in
  let parent = ref s in
  let leaf = ref leaf_node in
  let rec traverse parent_field current_field current =
    if is_leaf current
    then { ancestor = !ancestor; successor = !successor; parent = !parent; leaf = !leaf }
    else (
      if not (AFT.get_tag parent_field)
      then (
        (* Update ancestor and successor as the edge is untagged*)
        ancestor := !parent;
        successor := !leaf);
      parent := !leaf;
      leaf := current;
      let new_current_field = child_edge current key in
      let new_current = AFT.get_value new_current_field in
      traverse current_field new_current_field new_current)
  in
  (* Initialize params*)
  let init_current_field = leaf_node.left in
  let init_current = AFT.get_value init_current_field in
  traverse s.left init_current_field init_current
;;

(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. 
   Calls seek to get the seek record of the access path *)
let search root value =
  (* failwith "Not implemented" *)
  let sr = seek root value in
  match sr.leaf.item with
  | Some v -> value = v
  | None -> false
;;

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
  let edge = child_edge record.parent record.leaf.key in
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
let cleanup record _tree =
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
let help record tree =
  let edge = child_edge record.parent record.leaf.key in
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
    cleanup record tree
  else false
;;

type mode =
  | Inject
  | Cleanup
  | Helping

(** [delete tree k] removes [k] from [tree] if present.
    Returns [true] if the tree changed, and [false] if [k] was not present. *)
let delete tree value =
  let k = Hashtbl.hash value in
  let record = seek tree k in
  let rec delete_in_mode mode record =
    match mode with
    | Inject ->
      if inject record tree k
      then delete_in_mode Cleanup record
      else delete_in_mode Helping record
    | Cleanup ->
      if cleanup record tree then true else delete_in_mode Cleanup (seek tree k)
    | Helping ->
      ignore (help record tree);
      delete_in_mode Inject (seek tree k)
  in
  delete_in_mode Inject record
;;

(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already
    present.

    Algorithm (Natarajan-Mittal §5.2):
      1. Seek to the leaf where [k] would sit.
      2. If that leaf already holds [k], the key is present — return false.
      3. Otherwise build a fresh subtree of two nodes:
           newInternal with key = max(k, leaf.key)
           newLeaf     with key = k
         Wired so the BST invariant is preserved at newInternal.
      4. CAS the [(parent, leaf)] edge to point at newInternal.
         On success we're done. On failure, help any concurrent delete
         on the same edge, then retry from the top — the tree has
         changed and our seek record is stale. *)
let rec insert tree value =
  let k = Hashtbl.hash value in
  let record = seek tree k in
  if record.leaf.key = k
  then false
  else (
    (* Execution phase: build the replacement subtree. *)
    let k' = record.leaf.key in
    let new_leaf = make_leaf k None in
    let new_internal =
      if k < k'
      then make_internal k' new_leaf record.leaf
      else make_internal k record.leaf new_leaf
    in
    (* Note on newInternal.key:
         - if k < k': max(k, k') = k', newLeaf is left  (smaller),
                                        oldLeaf is right (>= k').
         - if k > k': max(k, k') = k, oldLeaf is left (< k),
                                       newLeaf is right (= k, satisfies >=).
       k = k' is impossible here — caller branch already returned false. *)
    (* Pick the [(parent, leaf)] edge using the same key-based side
       selection as inject/cleanup. *)
    let edge = child_edge record.parent record.leaf.key in
    (* CAS the edge's target from [leaf] to [new_internal], requiring
       both flag and tag to be false. A set flag means a delete has
       injected on this edge — we must not trample it. A set tag means
       [parent] itself is being removed — same story. *)
    let success =
      AFT.cas
        edge
        ~exp_val:record.leaf
        ~exp_flag:false
        ~exp_tag:false
        ~new_val:new_internal
        ~new_flag:false
        ~new_tag:false
    in
    if success
    then true
    else (
      (* Push any in-progress delete on this edge forward, then retry
         from a fresh seek. Our stale [record] is fine as input to help
         — it captured a valid access path when we traversed it. *)
      ignore (help record tree);
      insert tree value))
;;

(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected tree size. *)
let size _ = failwith "Not implemented"

(** Pretty-printing for debugging.

    This is a lock-free snapshot: the tree may change while we print,
    so the output is a best-effort view of one recent state, not a
    synchronized dump.  Useful for manual inspection and test failures,
    not for anything the program's correctness depends on. *)

let sentinel_name k =
  if k = inf0
  then Some "\xe2\x88\x9e\xe2\x82\x80" (* ∞₀ *)
  else if k = inf1
  then Some "\xe2\x88\x9e\xe2\x82\x81" (* ∞₁ *)
  else if k = inf2
  then Some "\xe2\x88\x9e\xe2\x82\x82" (* ∞₂ *)
  else None
;;

let key_str k =
  match sentinel_name k with
  | Some s -> s
  | None -> string_of_int k
;;

(** [pp ?pp_item fmt tree] prints [tree] to [fmt] in a rotated ASCII
    layout. Internal nodes show [key]; leaves show [L:key] with the
    item (if any and if [pp_item] is supplied). Edge marks appear on
    their own line under the edge's head: [⚑] flagged, [⚐] tagged,
    [⚑⚐] both. *)
let pp ?pp_item fmt tree =
  let open Format in
  (* [edge_marks] returns a string like " ⚑", " ⚐", " ⚑⚐", or "". *)
  let edge_marks edge =
    let s = AFT.get edge in
    match s.flag, s.tag with
    | false, false -> ""
    | true, false -> " \xe2\x9a\x91" (* ⚑ *)
    | false, true -> " \xe2\x9a\x90" (* ⚐ *)
    | true, true -> " \xe2\x9a\x91\xe2\x9a\x90"
  in
  let item_str node =
    match node.item, pp_item with
    | Some v, Some p -> Printf.sprintf " (%s)" (p v)
    | Some _, None -> " <item>"
    | None, _ -> ""
  in
  (* [prefix] is the accumulated left-margin for this line; [branch]
     is the connector to draw right before this node ("├── ", "└── ",
     or "" for the root). [edge] is the incoming edge (None for root)
     so we can print its marks after the node line. *)
  let rec walk prefix branch edge node =
    let label =
      if is_leaf node
      then Printf.sprintf "L:%s%s" (key_str node.key) (item_str node)
      else key_str node.key
    in
    let marks =
      match edge with
      | Some e -> edge_marks e
      | None -> ""
    in
    fprintf fmt "%s%s[%s]%s@\n" prefix branch label marks;
    if not (is_leaf node)
    then (
      (* Snapshot children atomically-ish: we read each edge's value once.
         They can change between these reads, but we commit to whatever
         we saw for the rest of this subtree's rendering. *)
      let l = AFT.get_value node.left in
      let r = AFT.get_value node.right in
      let child_prefix =
        prefix
        ^
        if branch = ""
        then ""
        else if String.length branch >= 4 && String.sub branch 0 3 = "\xe2\x94\x94"
        then "    "
        else "\xe2\x94\x82   "
      in
      walk child_prefix "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 " (Some node.left) l;
      walk child_prefix "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 " (Some node.right) r)
  in
  walk "" "" None tree;
  pp_print_flush fmt ()
;;

(** [print ?pp_item tree] prints [tree] to stdout. Convenience wrapper
    for the REPL and test failures. *)
let print ?pp_item tree = pp ?pp_item Format.std_formatter tree

(** [to_string ?pp_item tree] returns the pretty-printed tree as a
    string. Useful for embedding tree dumps in test assertion messages. *)
let to_string ?pp_item tree =
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  pp ?pp_item fmt tree;
  Buffer.contents buf
;;
