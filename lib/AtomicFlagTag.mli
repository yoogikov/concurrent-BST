(** Atomic record combining two bit fields — [flag] and [tag] — with a
    polymorphic payload [value].  All three fields are packed into a single
    atomic cell so that every read and every compare-and-swap operates on the
    triple as one indivisible unit. *)

(** The snapshot of an atomic record at a single point in time. *)
type 'a snapshot =
  { flag : bool (** single-bit boolean flag *)
  ; tag : bool (** single-bit boolean tag  *)
  ; value : 'a (** polymorphic payload      *)
  }

(** An atomic record holding a [flag] bit, a [tag] bit, and a value of
    type ['a].  The representation is abstract; access is only possible
    through the operations below. *)
type 'a t

(** {1 Construction} *)

(** [make ~flag ~tag v] creates a fresh atomic record initialised with the
    given [flag], [tag], and payload [v]. *)
val make : flag:bool -> tag:bool -> 'a -> 'a t

(** {1 Whole-record operations} *)

(** [get r] atomically reads the current [flag], [tag], and [value] of [r]
    and returns them as a {!snapshot}. *)
val get : 'a t -> 'a snapshot

(** [set r ~flag ~tag v] atomically replaces the contents of [r] with the
    given [flag], [tag], and payload [v]. *)
val set : 'a t -> flag:bool -> tag:bool -> 'a -> unit

(** [cas r expected ~flag ~tag v] performs an atomic compare-and-swap on [r].

    If the current contents of [r] are physically equal to [expected] in all
    three fields ([flag], [tag], and [value]) the record is atomically updated
    to the new [flag], [tag], and [v] and [true] is returned; otherwise the
    record is left unchanged and [false] is returned. *)
val cas : 'a t -> 'a snapshot -> flag:bool -> tag:bool -> 'a -> bool

(** {1 Field-level accessors}

    Each accessor reads the whole atomic cell and projects the requested
    field; they are convenience wrappers around {!get}. *)

(** [get_flag r] returns the current value of the [flag] bit of [r]. *)
val get_flag : 'a t -> bool

(** [get_tag r] returns the current value of the [tag] bit of [r]. *)
val get_tag : 'a t -> bool

(** [get_value r] returns the current payload of [r]. *)
val get_value : 'a t -> 'a

(** {1 Field-level mutators}

    Each mutator performs a read-modify-write loop (using {!cas} internally)
    that retries until it succeeds, leaving the two fields it did not target
    unchanged.  Because the underlying cell is a single atomic, a concurrent
    writer may cause arbitrarily many retries; the call always terminates
    provided at least one thread makes progress (lock-free, not wait-free). *)

(** [set_flag r b] atomically sets the [flag] bit of [r] to [b], leaving
    [tag] and [value] unchanged. *)
val set_flag : 'a t -> bool -> unit

(** [set_tag r b] atomically sets the [tag] bit of [r] to [b], leaving
    [flag] and [value] unchanged. *)
val set_tag : 'a t -> bool -> unit

(** [set_value r v] atomically sets the payload of [r] to [v], leaving
    [flag] and [tag] unchanged. *)
val set_value : 'a t -> 'a -> unit

