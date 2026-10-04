(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The geometry of the notes timeline.

   The timeline is one SVG spine that snakes down the page, swinging between
   the left and the right of a lane, with an exit curve from it to the node of
   each entry. To draw the spine without measuring the page, every entry is a
   row of fixed height and its text is clamped, so the position of every entry
   is known when the page is made.

   Lengths are in em, so that the whole timeline scales with the font size of
   the page. The origin is the top left of the timeline. *)

(** The lane the spine swings in, and the length of one swing. *)
let xl = 1.2

let xr = 3.4
let seg = 13.0

(** The column of nodes, and the width of the svg that holds the spine and the
    exits. *)
let node_x = 6.8

let svg_width = 5.7

type kind = Note | Week | Release | Quiet

let height = function
  | Note -> 7.8
  | Week -> 4.8
  | Release -> 2.3
  | Quiet -> 1.9

(** The height from the top of a row to the centre of its node. *)
let center = function
  | Note -> 3.2
  | Week -> 2.4
  | Release -> 1.15
  | Quiet -> 0.95

let radius = function Note -> 1.9 | Week -> 1.9 | Release -> 0.6 | Quiet -> 0.

(** How far above its node an exit leaves the spine. *)
let drop = function Note -> 1.6 | Week -> 1.6 | Release -> 1.1 | Quiet -> 0.

(** The left of the text of every kind of row, so that the text of notes,
    weeknotes and releases lines up in one column. *)
let text_left _ = node_x +. 2.5

let month_height = 3.6
let month_gap = 0.5

(* One swing is a cubic Bezier whose two control points are level with the
   middle of the swing. Its height is [seg * f t] and its distance across is
   [smoothstep t], so both can be had from the parameter [t]. *)
let height_of t = (1.5 *. t *. (1. -. t)) +. (t *. t *. t)
let across t = (3. *. t *. t) -. (2. *. t *. t *. t)

(* [height_of] rises from 0 to 1 and never falls, so a height has one [t]. *)
let t_at target =
  let rec go lo hi n =
    if n = 0 then (lo +. hi) /. 2.
    else
      let mid = (lo +. hi) /. 2. in
      if height_of mid < target then go mid hi (n - 1) else go lo mid (n - 1)
  in
  go 0. 1. 60

let spine_x y =
  let y = Float.max 0. y in
  let k = int_of_float (y /. seg) in
  let y0 = float_of_int k *. seg in
  let a, b = if k mod 2 = 0 then (xl, xr) else (xr, xl) in
  a +. ((b -. a) *. across (t_at ((y -. y0) /. seg)))

let spine_path ~height =
  let swings = int_of_float (Float.ceil (height /. seg)) + 1 in
  let b = Buffer.create 256 in
  Buffer.add_string b (Printf.sprintf "M %.3f 0" xl);
  for k = 0 to swings - 1 do
    let y0 = float_of_int k *. seg in
    let a, c = if k mod 2 = 0 then (xl, xr) else (xr, xl) in
    Buffer.add_string b
      (Printf.sprintf " C %.3f %.3f %.3f %.3f %.3f %.3f" a
         (y0 +. (seg /. 2.)) c (y0 +. (seg /. 2.)) c (y0 +. seg))
  done;
  Buffer.contents b

type exit_ = {
  start_x : float;
  start_y : float;
  end_x : float;
  end_y : float;
  path : string;
  lane : string;
}

(* The stretch of the spine around [y] that an exit merges from, as a path in
   coordinates relative to the row that begins [y_abs]. It is sampled, which is
   exact enough for a stroke as thin as the spine. *)
let lane_path ~y_abs ~start_y =
  let b = Buffer.create 128 in
  let step = 0.2 in
  let n = int_of_float (3.2 /. step) in
  for i = 0 to n do
    let y = start_y -. 2.0 +. (float_of_int i *. step) in
    Buffer.add_string b
      (Printf.sprintf "%s %.3f %.3f" (if i = 0 then "M" else " L")
         (spine_x (y_abs +. y)) y)
  done;
  Buffer.contents b

(* The exit of the entry whose row begins [y_abs] down the timeline. Its
   coordinates are relative to the top of the row. It leaves the spine along the
   spine's own direction, turns in one smooth elbow, and runs level into its
   node, so that the exits read as rails off a main line. *)
let exit_ ~kind ~y_abs =
  let end_y = center kind in
  let start_y = end_y -. drop kind in
  let start_x = spine_x (y_abs +. start_y) in
  let end_x = node_x -. radius kind in
  let slope =
    (spine_x (y_abs +. start_y +. 0.05) -. spine_x (y_abs +. start_y -. 0.05))
    /. 0.1
  in
  let w = Float.min 1.8 (0.65 *. (end_x -. start_x)) in
  let turn_x = start_x +. w in
  let path =
    Printf.sprintf "M %.3f %.3f C %.3f %.3f %.3f %.3f %.3f %.3f L %.3f %.3f"
      start_x start_y
      (start_x +. (slope *. 0.5 *. drop kind))
      (start_y +. (0.55 *. drop kind))
      (turn_x -. (0.6 *. w))
      end_y turn_x end_y end_x end_y
  in
  { start_x; start_y; end_x; end_y; path; lane = lane_path ~y_abs ~start_y }
