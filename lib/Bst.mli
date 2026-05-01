(** Lock-free binary search tree interface. Based on Natarajan and Mittal, "Fast
    Concurrent Lock-Free Binary Search Trees".

    The implementation uses sentinel nodes and lock-free edge-based modification
    operations. Search, insert, and delete are provided as concurrent
    operations. *)

type 'a t
(** The type of a concurrent lock-free binary search tree. *)

val create : ('a -> int) -> ('a -> string) -> 'a t
(** [create hash_fn] creates an empty lock-free BST with the provided hash
    function. The hash function maps values to integer keys.

    For integers: use [fun x -> x] For other types: use [Hashtbl.hash] or a
    custom function *)

val search : 'a t -> 'a -> bool
(** [search tree k] returns [true] if [k] is present in [tree], and [false]
    otherwise. This is a lock-free search operation. *)

val insert : 'a t -> 'a -> bool
(** [insert tree k] inserts [k] into [tree] if it is not already present.
    Returns [true] if the tree changed, and [false] if [k] was already present.
*)

val delete : 'a t -> 'a -> bool
(** [delete tree k] removes [k] from [tree] if present. Returns [true] if the
    tree changed, and [false] if [k] was not present. *)

val size : 'a t -> int
(** [size tree] returns the current number of keys stored in [tree]. This helper
    is useful for manual and concurrent tests to verify that insert/delete
    operations preserve the expected tree size. *)

val to_string : 'a t -> string
(** [to_string tree] returns a string which contains a pretty printed form of
    the tree *)

val sequential_stats : 'a t -> int * int * float
(** [sequential_stats tree] must be called with no concurrent operations
    running. Returns [(n, height, ratio)] where [n] is the number of data
    nodes (excluding sentinel leaves), [height] is the depth of the deepest
    data leaf from the root, and [ratio] is [height /. log2(n+1)]. *)

val reset_iter_counts : unit -> unit
(** additional functions for benchmarking**)

val get_insert_iters : unit -> int
val get_delete_iters : unit -> int
