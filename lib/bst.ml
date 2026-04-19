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
module AFT = AtomicFlagTag
type 'a node =
  { item : 'a option
  ; key : int
  ; is_leaf : bool
  ; left : 'a edge
  ; right : 'a edge
  } and 'a edge = 'a node AFT.t

type 'a t = 'a node

type 'a seek_record =
  { ancestor  : 'a node   (* last node before last untagged edge      *)
  ; successor : 'a node   (* child of ancestor on access path         *)
  ; parent    : 'a node   (* second-to-last node on access path       *)
  ; leaf      : 'a node   (* last node on access path (a leaf)        *)
  }

let is_leaf node = node.is_leaf
(** Make a leaf node *)
let make_leaf key item =
  let dummy = AFT.make ~flag:false ~tag:false in
  (* leaf nodes — left/right are never followed *)
  { item; key; is_leaf = true; left = dummy (Obj.magic ()); right = dummy (Obj.magic ()) }

(** Make an internal node with given children *)
let make_internal key left right =
  { item  = None
  ; key
  ; is_leaf = false
  ; left  = AFT.make ~flag:false ~tag:false left
  ; right = AFT.make ~flag:false ~tag:false right
  }
let child_edge node key =
  if key < node.key then node.left else node.right

(** [create ()] creates an empty lock-free BST with sentinel structure
    already installed. *)
(*        R (inf2)                                                      *)
(*       /        \                                                     *)
(*    S (inf1)   leaf(inf2)                                            *)
(*    /      \                                                          *)
(* leaf(inf0) leaf(inf1)                                               *)
(* ------------------------------------------------------------------ *)
let create () = 
    (* failwith "Not implemented" *)
 let leaf_inf0 = make_leaf inf0 None in
  let leaf_inf1 = make_leaf inf1 None in
  let leaf_inf2 = make_leaf inf2 None in
  let s = make_internal inf1 leaf_inf0 leaf_inf1 in
  (* Root node*)
  let r = make_internal inf2 s leaf_inf2 in
  r

let seek root key = 
    (* failwith "Not implemented" *)
    let s = AFT.get_value root.left in
    let leaf_node = AFT.get_value s.left in

    (* Initialize seek record*)
    let ancestor = ref root in
    let successor = ref s in
    let parent = ref s in
    let leaf = ref leaf_node in

    let rec traverse parent_field current_field current = 
        if is_leaf current then 
            { ancestor = !ancestor; successor = !successor; parent = !parent; leaf = !leaf }
        else begin
            if not (AFT.get_tag parent_field) then begin
                ancestor := !parent;
                successor := !leaf;
            end;
            parent := !leaf;
            leaf := current;
            let new_current_field = child_edge current key in
            let new_current = AFT.get_value new_current_field in
            traverse current_field new_current_field new_current 

        end in
    
    (* Initialize params*)
    let init_current_field = leaf_node.left in
    let init_current = AFT.get_value init_current_field in
    traverse s.left init_current_field init_current

(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. *)
let search root v =
     (* failwith "Not implemented" *)
     let key = Hashtbl.hash v in
     let sr = seek root key in
     
     if sr.leaf.key = key && Option.is_some sr.leaf.item then
        true
     else false


(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already present. *)
let insert _ _ = failwith "Not implemented"

(** [delete tree k] removes [k] from [tree] if present.
    Returns [true] if the tree changed, and [false] if [k] was not present. *)
let delete _ _ = failwith "Not implemented"

(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected tree size. *)
let size _ = failwith "Not implemented"
