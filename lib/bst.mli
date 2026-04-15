(** Lock-free binary search tree interface.
    Based on Natarajan and Mittal, "Fast Concurrent Lock-Free Binary Search Trees".

    The implementation uses sentinel nodes and lock-free edge-based
    modification operations. Search, insert, and delete are provided as
    concurrent operations. *)

(** The type of keys stored in the BST. *)
type key = int

(** The type of a concurrent lock-free binary search tree. *)
type t

val create : unit -> t
(** [create ()] creates an empty lock-free BST with sentinel structure
    already installed. *)

val search : t -> key -> bool
(** [search tree k] returns [true] if [k] is present in [tree],
    and [false] otherwise. This is a lock-free search operation. *)

val insert : t -> key -> bool
(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already present. *)

val delete : t -> key -> bool
(** [delete tree k] removes [k] from [tree] if present.
    Returns [true] if the tree changed, and [false] if [k] was not present. *)

val size : t -> int
(** [size tree] returns the current number of keys stored in [tree].
    This helper is useful for manual and concurrent tests to verify that
    insert/delete operations preserve the expected tree size. *)
