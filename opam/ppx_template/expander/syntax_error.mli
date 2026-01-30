open! Stdppx

type t

val createf
  :  loc:Ppxlib.Location.t
  -> ('a, Stdlib.Format.formatter, unit, t) format4
  -> 'a

val combine : t -> t list -> t
val to_extension : t -> Ppxlib.extension
