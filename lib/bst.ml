(* bst.ml
 * 
 * Minimal lock-free BST implementation stub.
 * This file provides only the definitions required by bst.mli so that
 * the project builds cleanly. The actual algorithm is left unimplemented.
 *)

type 'a node =
  { item : 'a option
  ; key : int
  ; left : 'a
  }

type 'a t = 'a node

(** [create ()] creates an empty lock-free BST with sentinel structure
    already installed. *)
let create () = failwith "Not implemented"

(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. *)
let search _ _ = failwith "Not implemented"

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
