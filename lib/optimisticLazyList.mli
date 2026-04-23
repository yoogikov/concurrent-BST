(** Optimistic lazy BST interface.
    Based on an external (leaf-oriented) BST with optimistic traversal,
    per-node locking, and lazy deletion via marking.

    The filename/module name is kept as [optimisticLazyList] to match the
    existing project layout, but the data structure implemented here is a BST. *)

(** The type of a concurrent optimistic lazy BST. *)
type 'a t

(** [create hash_fn to_string_fn] creates an empty optimistic lazy BST with the
    provided hash function and pretty-printer.

    The hash function maps values to integer keys.

    For integers: use [fun x -> x]
    For other types: use [Hashtbl.hash] or a custom function *)
val create : ('a -> int) -> ('a -> string) -> 'a t

(** [search tree x] returns [true] if [x] is present in [tree],
    and [false] otherwise. This operation traverses the tree without locking
    and ignores logically deleted leaves. *)
val search : 'a t -> 'a -> bool

(** [insert tree x] inserts [x] into [tree] if it is not already present.
    Returns [true] if the set changed, and [false] if [x] was already present. *)
val insert : 'a t -> 'a -> bool

(** [delete tree x] removes [x] from [tree] if present.
    Returns [true] if the set changed, and [false] if [x] was not present. *)
val delete : 'a t -> 'a -> bool

(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected size. *)
val size : 'a t -> int

(** [to_string tree] returns a pretty-printed form of the tree. *)
val to_string : 'a t -> string
