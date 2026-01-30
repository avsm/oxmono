(* $MDX part-begin=rebind *)
open! Core

(* Just rebinding the names to work with the PPX. [%template] here is just shorthand for
   [struct ... end], with the PPX enabled. Another totally acceptable way to write this is
   [open%template struct ... end]. *)
open
  [%template
  module [@kind value] Float = Float (* alternatively, [@@kind value] *)
  module [@kind float64] Float = Float_u]
(* $MDX part-end *)

(* $MDX part-begin=custom-intf *)
module%template [@kind k = (value, float64)] Float : sig
  type t : k

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end =
  Float
  [@kind k]
(* $MDX part-end *)
