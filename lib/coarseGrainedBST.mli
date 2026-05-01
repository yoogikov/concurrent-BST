(*
 * Coarse-grained BST interface.
 *)

(** The type of keys stored in the BST. *)

type 'a t
(** The type of a concurrent lock-free binary search tree. *)

val create : ('a -> int) -> ('a -> string) -> 'a t
(** [create ()] creates an empty lock-free BST with sentinel structure already
    installed. *)

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

val sequential_stats : 'a t -> int * int * float
(** [sequential_stats tree] must be called with no concurrent operations
    running. Returns [(n, height, ratio)] where [n] is the number of nodes,
    [height] is the height of the tree (number of levels), and [ratio] is
    [height /. log2(n+1)]. *)

val to_string : 'a t -> string
(** [to_string tree] returns a string which contains a pretty printed form of
    the tree *)
