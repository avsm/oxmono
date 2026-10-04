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
let xl = 1.0

let xr = 3.0
let seg = 13.0

(** The left edge of every entry's card, where its exit arrives, and the width
    of the thumbnail that begins the card. *)
let card_x = 4.4

let thumb_w = 4.6
let svg_width = 5.7

type kind = Note | Week | Release | Quiet

let height = function
  | Note -> 4.8
  | Week -> 4.8
  | Release -> 2.3
  | Quiet -> 1.9

(** The height from the top of a row to the centre of its node. *)
let center = function
  | Note -> 2.4
  | Week -> 2.4
  | Release -> 1.15
  | Quiet -> 0.95

(** The size of the node of a row: the thumbnail of a card, or the dot of a
    release. *)
let node_width = function Note | Week -> thumb_w | Release | Quiet -> 0.

let node_height = function
  | Note -> 3.8
  | Week -> 3.8
  | Release | Quiet -> 0.

(** The x of the left edge of a node. *)
let node_left _ = card_x

(** The left of the text of every kind of row, so that the text of notes,
    weeknotes and releases lines up in one column. *)
let text_left _ = card_x +. thumb_w +. 0.9

(** [arrive ~plain kind] is the x where the exit of a row ends: the edge of its
    thumbnail, or just short of its text when it has none, as a release never
    does ([plain]), so that the line runs on to the entry itself. *)
let arrive ~plain kind =
  match kind with
  | Release -> text_left kind -. 0.6
  | _ when plain -> text_left kind -. 0.6
  | _ -> node_left kind

(** How far above its node an exit leaves the spine. *)
let drop = function Note -> 1.6 | Week -> 1.6 | Release -> 1.1 | Quiet -> 0.

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
let exit_ ~plain ~kind ~y_abs =
  let end_y = center kind in
  let start_y = end_y -. drop kind in
  let start_x = spine_x (y_abs +. start_y) in
  let end_x = arrive ~plain kind in
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

(* The seasons. Each month's stretch of the timeline carries a few small
   vector motifs beside the spine, so that the page changes with the year as
   it is read downwards. They are scattered by a generator seeded from the
   month, so that a page renders the same every time, and they keep clear of
   the spine and inside the lane. *)

type season = Winter | Spring | Summer | Autumn

(** [season_of_month m] is the northern season of month [m], 1 to 12. *)
let season_of_month = function
  | 12 | 1 | 2 -> Winter
  | 3 | 4 | 5 -> Spring
  | 6 | 7 | 8 -> Summer
  | _ -> Autumn

let season_name = function
  | Winter -> "winter"
  | Spring -> "spring"
  | Summer -> "summer"
  | Autumn -> "autumn"

(** The width of the strip that motifs fill, and how near the spine one may
    be. *)
let season_width = card_x -. 0.3

let season_clear = 0.6

type motif = { cx : float; cy : float; r : float; a : float }

(** [motifs season ~seed ~y0 ~height] is the motifs of a month's strip, which
    begins [y0] down the timeline and is [height] tall. Each lies inside the
    strip and at least [season_clear] from the spine. *)
let motifs season ~seed ~y0 ~height =
  let state = ref ((seed * 7919) + 104729) in
  let next () =
    state := ((!state * 1103515245) + 12345) land 0x3fffffff;
    float_of_int ((!state lsr 6) land 0xffff) /. 65536.
  in
  let rmin, rmax =
    match season with
    | Winter -> (0.3, 0.5)
    | Spring -> (0.24, 0.38)
    | Summer -> (0.26, 0.42)
    | Autumn -> (0.34, 0.55)
  in
  let pitch = 1.9 in
  let n = int_of_float ((height -. month_height -. 1.) /. pitch) in
  let out = ref [] in
  for k = 0 to n - 1 do
    let cy =
      month_height +. 1. +. (float_of_int k *. pitch) +. (next () *. 1.2)
    in
    let r = rmin +. (next () *. (rmax -. rmin)) in
    let a = next () *. Float.pi in
    let x = 0.3 +. (next () *. (season_width -. 0.6)) in
    let skip = next () < 0.2 in
    let s = spine_x (y0 +. cy) in
    let x =
      if Float.abs (x -. s) >= season_clear then Some x
      else if s +. season_clear +. r < season_width -. 0.1 then
        Some (s +. season_clear)
      else if s -. season_clear -. r > 0.1 then Some (s -. season_clear)
      else None
    in
    match x with
    | Some cx
      when (not skip) && cy +. r < height && cx -. r >= 0.05
           && cx +. r <= season_width -. 0.05 ->
      out := { cx; cy; r; a } :: !out
    | _ -> ()
  done;
  List.rev !out

(** [season_path season ~seed ~y0 ~height] is one svg path drawing the motifs
    of [motifs]: snowflakes, blossoms, sun rays or leaves. *)
let season_path season ~seed ~y0 ~height =
  let b = Buffer.create 512 in
  let line x1 y1 x2 y2 =
    Buffer.add_string b (Printf.sprintf "M%.2f %.2f L%.2f %.2f " x1 y1 x2 y2)
  in
  let spokes m ~count ~inner =
    for k = 0 to count - 1 do
      let turn = 2. *. Float.pi /. float_of_int count in
      let ang = m.a +. (float_of_int k *. turn) in
      let dx = Float.cos ang and dy = Float.sin ang in
      line (m.cx +. (inner *. m.r *. dx)) (m.cy +. (inner *. m.r *. dy))
        (m.cx +. (m.r *. dx)) (m.cy +. (m.r *. dy))
    done
  in
  List.iter (fun m ->
    match season with
    | Winter ->
      for k = 0 to 2 do
        let ang = m.a +. (float_of_int k *. Float.pi /. 3.) in
        let dx = m.r *. Float.cos ang and dy = m.r *. Float.sin ang in
        line (m.cx -. dx) (m.cy -. dy) (m.cx +. dx) (m.cy +. dy)
      done
    | Spring ->
      spokes m ~count:5 ~inner:0.35;
      line m.cx m.cy m.cx m.cy
    | Summer ->
      spokes m ~count:8 ~inner:0.55;
      line m.cx m.cy m.cx m.cy
    | Autumn ->
      let dx = m.r *. Float.cos m.a and dy = m.r *. Float.sin m.a in
      let nx = -0.7 *. dy and ny = 0.7 *. dx in
      Buffer.add_string b
        (Printf.sprintf
           "M%.2f %.2f Q%.2f %.2f %.2f %.2f Q%.2f %.2f %.2f %.2f Z "
           (m.cx -. dx) (m.cy -. dy) (m.cx +. nx) (m.cy +. ny) (m.cx +. dx)
           (m.cy +. dy) (m.cx -. nx) (m.cy -. ny) (m.cx -. dx) (m.cy -. dy)))
    (motifs season ~seed ~y0 ~height);
  String.trim (Buffer.contents b)
