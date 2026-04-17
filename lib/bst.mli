(** Lock-free binary search tree interface.
    Based on Natarajan and Mittal, "Fast Concurrent Lock-Free Binary Search Trees".

    The implementation uses sentinel nodes and lock-free edge-based
    modification operations. Search, insert, and delete are provided as
    concurrent operations. *)

(** The type of a concurrent lock-free binary search tree. *)
type 'a t

(** [create ()] creates an empty lock-free BST with sentinel structure
    already installed. *)
val create : unit -> 'a t

(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. *)
val search : 'a t -> 'a -> bool

(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already present. *)
val insert : 'a t -> 'a -> bool

(** [delete tree k] removes [k] from [tree] if present.
    Returns [true] if the tree changed, and [false] if [k] was not present. *)
val delete : 'a t -> 'a -> bool

(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected tree size. *)
val size : 'a t -> int

