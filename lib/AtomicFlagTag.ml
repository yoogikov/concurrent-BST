type 'a snapshot =
  { flag : bool
  ; tag : bool
  ; value : 'a
  }

type 'a t = 'a snapshot Atomic.t
(* placeholder — replace with real representation *)

(* Construction *)

(** Create a new snapshot with params [flag], [tag], and [value]*)
let make ~flag ~tag value = Atomic.make { flag; tag; value }

(* Whole-record operations *)

(** [get r] atomically reads the current [flag], [tag], and [value] of [r]
    and returns them as a {!snapshot}. *)
let get r =
  (* not_implemented "get" *)
  Atomic.get r
;;

(** [set r flag tag value] replaces the contents of [r] with the given 
 [flag] [tag] and payload [value]*)
let set r ~flag ~tag value =
  (* not_implemented "set" *)
  Atomic.set r { flag; tag; value }
;;

(** Return [true] on a successful cas and [false] otherwise*)
let cas
      (snapshot : 'a t)
      ~exp_flag
      ~exp_tag
      ~(exp_val : 'a)
      ~new_flag
      ~new_tag
      ~(new_val : 'a)
  =
  let rec loop () =
    let current = Atomic.get snapshot in
    if current.flag <> exp_flag || current.tag <> exp_tag
       || current.value <> exp_val
    then false
    else
      let new_snapshot = { flag = new_flag; tag = new_tag; value = new_val } in
      if Atomic.compare_and_set snapshot current new_snapshot then true
      else loop ()
  in
  loop ()
;;

(* Field-level accessors *)

let get_flag r =
  (* not_implemented "get_flag" *)
  let snapshot = Atomic.get r in
  snapshot.flag
;;

let get_tag r =
  (* not_implemented "get_tag" *)
  let snapshot = Atomic.get r in
  snapshot.tag
;;

let get_value r =
  (* not_implemented "get_value" *)
  let snapshot = Atomic.get r in
  snapshot.value
;;

(* Field-level mutators *)
let set_flag r flag =
  let rec loop () =
    let snapshot = Atomic.get r in
    if snapshot.flag = flag then ()
    else
      let new_snapshot = { snapshot with flag } in
      if Atomic.compare_and_set r snapshot new_snapshot then ()
      else loop ()
  in
  loop ()
;;

let set_tag r tag =
  let rec loop () =
    let snapshot = Atomic.get r in
    if snapshot.tag = tag then ()
    else
      let new_snapshot = { snapshot with tag } in
      if Atomic.compare_and_set r snapshot new_snapshot then ()
      else loop ()
  in
  loop ()
;;

let set_value r value =
  let rec loop () =
    let snapshot = Atomic.get r in
    if snapshot.value = value then ()
    else
      let new_snapshot = { snapshot with value } in
      if Atomic.compare_and_set r snapshot new_snapshot then ()
      else loop ()
  in
  loop ()
;;