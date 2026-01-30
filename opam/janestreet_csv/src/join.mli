open! Core

module Row : sig
  type t = string list
end

type t =
  | Full
  | Inner
  | Left
[@@deriving compare, enumerate, sexp]

val param : t Command.Param.t

val join
  :  t
  -> string list (* file names *)
  -> key_fields:string array
  -> sep:char
  -> Row.t Sequence.t
