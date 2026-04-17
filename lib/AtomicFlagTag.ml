type 'a snapshot =
  { flag : bool
  ; tag : bool
  ; value : 'a
  }

type 'a t = unit (* placeholder — replace with real representation *)

exception Not_implemented of string

let not_implemented name = raise (Not_implemented name)

(* Construction *)

let make ~flag:_ ~tag:_ _ = not_implemented "make"

(* Whole-record operations *)

let get _ = not_implemented "get"
let set _ ~flag:_ ~tag:_ _ = not_implemented "set"
let cas _ _ ~flag:_ ~tag:_ _ = not_implemented "cas"

(* Field-level accessors *)

let get_flag _ = not_implemented "get_flag"
let get_tag _ = not_implemented "get_tag"
let get_value _ = not_implemented "get_value"

(* Field-level mutators *)

let set_flag _ _ = not_implemented "set_flag"
let set_tag _ _ = not_implemented "set_tag"
let set_value _ _ = not_implemented "set_value"
