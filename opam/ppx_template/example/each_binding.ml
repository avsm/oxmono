open! Core
open! Import

(* $MDX part-begin=float *)
[%%template
[@@@kind.default k = (value, float64)]

open Float [@kind k]

type float = t

let round_up = round_up
let round_down = round_up
let iround_up_exn = iround_up_exn
let iround_down_exn = iround_down_exn]
(* $MDX part-end *)
