(** Lock-Free Skip List *)

type t
(** The type of a lock-free skip list. *)

val create : int -> t
(** [create max_level] creates an empty lock-free skip list with maximum level
    [max_level].
    @raise Invalid_argument if [max_level < 0] *)

val insert : t -> int -> bool
(** [add s x] inserts key [x] into [s] if it is not already present. Returns
    [true] if [x] was added, and [false] if [x] was already present. *)

val delete : t -> int -> bool
(** [remove s x] removes key [x] from [s] if it is present. Returns [true] if
    [x] was removed, and [false] if [x] was not present. *)

val search : t -> int -> bool
(** [contains s x] returns [true] if key [x] is present in [s], and [false]
    otherwise. *)
