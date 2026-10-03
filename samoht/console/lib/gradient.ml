(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type spread = [ `Pad | `Reflect | `Repeat ]

type t = {
  stops : Color.t array;
  direction : int * int;
  length : int;
  spread : spread;
  origin : int * int;
}

let v ?(spread = `Pad) ?(direction = (1, 0)) ~length stops =
  if stops = [] then invalid_arg "Console.Gradient.v: no colour stop";
  if length <= 0 then invalid_arg "Console.Gradient.v: non-positive length";
  { stops = Array.of_list stops; direction; length; spread; origin = (0, 0) }

let stops g = Array.to_list g.stops

let at g p =
  let n = Array.length g.stops - 1 in
  if n = 0 then g.stops.(0)
  else
    let p = Float.max 0. (Float.min 1. p) in
    let x = p *. float_of_int n in
    let i = min (n - 1) (int_of_float x) in
    Color.blend g.stops.(i) g.stops.(i + 1) (x -. float_of_int i)

(* [k] modulo [period], in [0, period). *)
let modulo k period =
  let r = k mod period in
  if r < 0 then r + period else r

let shift ~row ~column g =
  let r, c = g.origin in
  { g with origin = (r + row, c + column) }

let cell g ~row ~column =
  let dx, dy = g.direction in
  let row = row + fst g.origin and column = column + snd g.origin in
  let k = (dx * column) + (dy * row) in
  let p =
    match g.spread with
    | `Pad -> float_of_int k /. float_of_int g.length
    | `Repeat -> float_of_int (modulo k g.length) /. float_of_int g.length
    | `Reflect ->
        let period = 2 * g.length in
        let x = float_of_int (modulo k period) /. float_of_int period in
        1. -. Float.abs ((2. *. x) -. 1.)
  in
  at g p

let map f g = { g with stops = Array.map f g.stops }

let equal a b =
  Array.length a.stops = Array.length b.stops
  && Array.for_all2 Color.equal a.stops b.stops
  && a.direction = b.direction && a.length = b.length && a.spread = b.spread
  && a.origin = b.origin

let pp ppf g =
  let dx, dy = g.direction in
  Fmt.pf ppf "gradient(%a; (%d, %d); %d; %s)"
    Fmt.(list ~sep:(any ", ") Color.pp)
    (stops g) dx dy g.length
    (match g.spread with
    | `Pad -> "pad"
    | `Reflect -> "reflect"
    | `Repeat -> "repeat")
