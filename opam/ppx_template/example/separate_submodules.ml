open! Core
open! Import

open struct
  module Float = Each_binding

  type%template float = (Float.float[@kind k]) [@@kind k = (value, float64)]
end

(* $MDX part-begin=arity1 *)
module type S1 = sig
  type t : any

  val round_up : t -> t
  val round_down : t -> t
  val iround_up_exn : t -> int
  val iround_down_exn : t -> int
end

type ('a : any) module1 = (module S1 with type t = 'a)

let round_up1 (type a : any) ((module M) : a module1) = M.round_up
let round_down1 (type a : any) ((module M) : a module1) = M.round_down
let iround_up1_exn (type a : any) ((module M) : a module1) = M.iround_up_exn
let iround_down1_exn (type a : any) ((module M) : a module1) = M.iround_down_exn
(* $MDX part-end *)

(* $MDX part-begin=arity0 *)
[%%template
[@@@kind.default k = (value, float64)]

module type S0 = sig
  type t : k

  include S1 with type t := t
end

module M : S0 [@kind k] with type t = (float[@kind k]) = struct
  type t = (float[@kind k])

  let round_up = (Float.round_up [@kind k])
  let round_down = (Float.round_down [@kind k])
  let iround_up_exn = (Float.iround_up_exn [@kind k])
  let iround_down_exn = (Float.iround_down_exn [@kind k])
end
(* $MDX part-end *)

(* $MDX part-begin=arity0-of-arity1 *)
type module0 = ((module S0 with type t = (float[@kind k]))[@kind k])

let module0 : (module0[@kind k]) = (module M [@kind k])
let round_up0 x = round_up1 (module0 [@kind k]) x
let round_down0 x = round_down1 (module0 [@kind k]) x
let iround_up0_exn x = iround_up1_exn (module0 [@kind k]) x
let iround_down0_exn x = iround_down1_exn (module0 [@kind k]) x]
(* $MDX part-end *)
