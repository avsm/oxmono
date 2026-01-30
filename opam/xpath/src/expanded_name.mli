open! Core

type t =
  { namespace : string option
  ; name : string
  }
[@@deriving sexp_of, compare]

(** Helper that interprets the namespace "" as None. *)
val create : namespace:string -> name:string -> t

val of_qualified_name_exn : Qualified_name.t -> prefixes:string String.Map.t -> t

include Comparable.S_plain with type t := t
