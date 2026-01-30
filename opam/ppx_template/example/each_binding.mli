open! Core
open! Import

(* $MDX part-begin=float *)
[%%template:
[@@@kind.default k = (value, float64)]

type float : k

val round_up : (float[@kind k]) -> (float[@kind k])
val round_down : (float[@kind k]) -> (float[@kind k])
val iround_up_exn : (float[@kind k]) -> int
val iround_down_exn : (float[@kind k]) -> int]
(* $MDX part-end *)
