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

(** The size of the node of a row, which is the thumbnail of a note or weeknote.
    A release has none. *)
let node_width = function Note | Week -> thumb_w | Release | Quiet -> 0.

let node_height = function
  | Note -> 3.8
  | Week -> 3.8
  | Release | Quiet -> 0.

(** The x of the left edge of a node. *)
let node_left = card_x

(** The left of the text of every kind of row, so that the text of notes,
    weeknotes and releases lines up in one column. *)
let text_left = card_x +. thumb_w +. 0.9

(** [arrive ~plain kind] is the x where the exit of a row ends: the edge of its
    thumbnail, or just short of its text when it has none, as a release never
    does ([plain]), so that the line runs on to the entry itself. *)
let arrive ~plain kind =
  match kind with
  | Release -> text_left -. 0.6
  | _ when plain -> text_left -. 0.6
  | _ -> node_left

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
   it is read downwards. Each season has four motifs. They are scattered by a
   generator seeded from the month, so that a page renders the same every
   time, and they keep clear of the spine and inside the lane. The first and
   last month of a season mix in the motifs of the season beside it, thinning
   out away from the shared edge, so that one season runs into the next. *)

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

(** The width of the strip that motifs fill. *)
let season_width = card_x -. 0.3

(** How far the edge of a motif keeps from the line of the spine. *)
let season_gap = 0.15

type motif = { cx : float; cy : float; r : float; a : float; season : season;
               variant : int }

(* Months run Dec Jan Feb, Mar Apr May, and so on: [position m] is 0 for the
   first month of a season in time, and 2 for the last. *)
let position m = m mod 12 mod 3

let previous_month m = ((m + 10) mod 12) + 1
let next_month m = (m mod 12) + 1

(** [motifs ~month ~seed ~y0 ~height] is the motifs of the strip of [month],
    which begins [y0] down the timeline and is [height] tall. Each lies inside
    the strip, and no part of it comes within [season_gap] of the spine. The
    page runs newest first, so the top of a month meets the month after it. *)
let motifs ~month ~seed ~y0 ~height =
  let state = ref ((seed * 7919) + 104729) in
  let next () =
    state := ((!state * 1103515245) + 12345) land 0x3fffffff;
    float_of_int ((!state lsr 6) land 0xffff) /. 65536.
  in
  let own = season_of_month month in
  let pitch = 1.9 in
  let n = int_of_float ((height -. month_height -. 1.) /. pitch) in
  let out = ref [] in
  for k = 0 to n - 1 do
    let cy =
      month_height +. 1. +. (float_of_int k *. pitch) +. (next () *. 1.2)
    in
    let u = cy /. height in
    let chance =
      (* The share of motifs that belong to the neighbouring season. *)
      match position month with
      | 2 -> Float.max 0. (0.5 *. (1. -. (u /. 0.6)))
      | 0 -> Float.max 0. (0.5 *. (1. -. ((1. -. u) /. 0.6)))
      | _ -> 0.
    in
    let season =
      let other =
        if position month = 2 then season_of_month (next_month month)
        else season_of_month (previous_month month)
      in
      if next () < chance then other else own
    in
    let rmin, rmax =
      match season with
      | Winter -> (0.34, 0.52)
      | Spring -> (0.3, 0.46)
      | Summer -> (0.3, 0.46)
      | Autumn -> (0.34, 0.52)
    in
    let r = rmin +. (next () *. (rmax -. rmin)) in
    let a = next () *. Float.pi in
    let variant = int_of_float (next () *. 4.) mod 4 in
    let x = 0.3 +. (next () *. (season_width -. 0.6)) in
    let skip = next () < 0.2 in
    (* The spine moves sideways across the height of a motif, so the motif is
       placed against the nearest the spine comes to it on either side. *)
    let near, far =
      List.fold_left (fun (lo, hi) k ->
        let sx = spine_x (y0 +. cy +. (r *. float_of_int k /. 4.)) in
        (Float.min lo sx, Float.max hi sx)) (infinity, neg_infinity)
        [ -4; -3; -2; -1; 0; 1; 2; 3; 4 ]
    in
    let reach = r +. season_gap in
    let x =
      if x <= near -. reach || x >= far +. reach then Some x
      else if far +. reach +. r <= season_width -. 0.05 then
        Some (far +. reach)
      else if near -. reach -. r >= 0.05 then Some (near -. reach)
      else None
    in
    match x with
    | Some cx
      when (not skip) && cy +. r < height && cx -. r >= 0.05
           && cx +. r <= season_width -. 0.05 ->
      out := { cx; cy; r; a; season; variant } :: !out
    | _ -> ()
  done;
  List.rev !out

(* A motif is drawn in a unit square about its centre, turned by its angle and
   scaled by its radius. A motif is some strokes and some filled shapes. *)
let draw m ~stroke ~fill =
  let c = Float.cos m.a and s = Float.sin m.a in
  let pt (x, y) =
    (m.cx +. (m.r *. ((x *. c) -. (y *. s))),
     m.cy +. (m.r *. ((x *. s) +. (y *. c))))
  in
  let move b p = let x, y = pt p in
    Buffer.add_string b (Printf.sprintf "M%.2f %.2f " x y) in
  let line b p = let x, y = pt p in
    Buffer.add_string b (Printf.sprintf "L%.2f %.2f " x y) in
  let quad b q p =
    let qx, qy = pt q and x, y = pt p in
    Buffer.add_string b (Printf.sprintf "Q%.2f %.2f %.2f %.2f " qx qy x y) in
  let cubic b c1 c2 p =
    let ax, ay = pt c1 and bx, by = pt c2 and x, y = pt p in
    Buffer.add_string b
      (Printf.sprintf "C%.2f %.2f %.2f %.2f %.2f %.2f " ax ay bx by x y) in
  let close b = Buffer.add_string b "Z " in
  let polar ang d = (d *. Float.cos ang, d *. Float.sin ang) in
  let spokes b ~count ~inner ~outer ~phase =
    for k = 0 to count - 1 do
      let turn = 2. *. Float.pi /. float_of_int count in
      let ang = phase +. (float_of_int k *. turn) in
      move b (polar ang inner);
      line b (polar ang outer)
    done
  in
  let dot b p = move b p; line b p in
  let disc b (x, y) rr =
    let px, py = pt (x -. rr, y) and qx, qy = pt (x +. rr, y) in
    let ar = m.r *. rr in
    Buffer.add_string b
      (Printf.sprintf
         "M%.2f %.2f A%.2f %.2f 0 1 0 %.2f %.2f A%.2f %.2f 0 1 0 %.2f %.2f Z "
         px py ar ar qx qy ar ar px py)
  in
  (* A lens from [p0] to [p1], [bend] thick. *)
  let lens b (x0, y0) (x1, y1) bend =
    let mx = (x0 +. x1) /. 2. and my = (y0 +. y1) /. 2. in
    let nx = -.(y1 -. y0) *. bend and ny = (x1 -. x0) *. bend in
    move b (x0, y0);
    quad b (mx +. nx, my +. ny) (x1, y1);
    quad b (mx -. nx, my -. ny) (x0, y0);
    close b
  in
  match (m.season, m.variant) with
  | Winter, 0 ->
    for k = 0 to 2 do
      let ang = float_of_int k *. Float.pi /. 3. in
      move stroke (polar ang (-1.));
      line stroke (polar ang 1.)
    done
  | Winter, 1 ->
    for k = 0 to 5 do
      let ang = float_of_int k *. Float.pi /. 3. in
      move stroke (0., 0.);
      line stroke (polar ang 1.);
      move stroke (polar ang 0.6);
      line stroke (polar (ang +. 0.7) 0.85);
      move stroke (polar ang 0.6);
      line stroke (polar (ang -. 0.7) 0.85)
    done
  | Winter, 2 ->
    move fill (0., -1.);
    quad fill (0.14, -0.14) (1., 0.);
    quad fill (0.14, 0.14) (0., 1.);
    quad fill (-0.14, 0.14) (-1., 0.);
    quad fill (-0.14, -0.14) (0., -1.);
    close fill
  | Winter, _ ->
    List.iter (fun (w, y) ->
      move stroke (-.w, y +. 0.5);
      line stroke (0., y);
      line stroke (w, y +. 0.5)) [ (0.55, -0.85); (0.8, -0.3); (1., 0.25) ];
    move stroke (0., 0.7);
    line stroke (0., 1.)
  | Spring, 0 ->
    for k = 0 to 4 do
      let ang = float_of_int k *. 2. *. Float.pi /. 5. in
      lens fill (0., 0.) (polar ang 1.) 0.55
    done
  | Spring, 1 ->
    move fill (0., -1.);
    cubic fill (0.15, -0.55) (0.65, -0.05) (0.65, 0.35);
    cubic fill (0.65, 0.75) (0.35, 1.) (0., 1.);
    cubic fill (-0.35, 1.) (-0.65, 0.75) (-0.65, 0.35);
    cubic fill (-0.65, -0.05) (-0.15, -0.55) (0., -1.);
    close fill
  | Spring, 2 ->
    move stroke (0., 1.);
    quad stroke (0.2, 0.) (0., -0.95);
    lens fill (0.05, 0.4) (0.95, -0.05) 0.4;
    lens fill (0.02, -0.15) (-0.85, -0.55) 0.4
  | Spring, _ ->
    spokes stroke ~count:8 ~inner:0.45 ~outer:1. ~phase:0.;
    dot stroke (0., 0.)
  | Summer, 0 ->
    disc fill (0., 0.) 0.4;
    spokes stroke ~count:8 ~inner:0.65 ~outer:1. ~phase:0.
  | Summer, 1 ->
    List.iter (fun dy ->
      move stroke (-1., dy +. 0.15);
      quad stroke (-0.75, dy -. 0.45) (-0.5, dy +. 0.15);
      quad stroke (-0.25, dy +. 0.75) (0., dy +. 0.15);
      quad stroke (0.25, dy -. 0.45) (0.5, dy +. 0.15);
      quad stroke (0.75, dy +. 0.75) (1., dy +. 0.15)) [ -0.45; 0.35 ]
  | Summer, 2 ->
    move stroke (-1., 0.2);
    quad stroke (-0.5, -0.5) (0., 0.2);
    quad stroke (0.5, -0.5) (1., 0.2);
    move stroke (0.15, -0.75);
    quad stroke (0.45, -1.) (0.7, -0.75);
    quad stroke (0.95, -1.) (1., -0.75)
  | Summer, _ ->
    move fill (-0.6, 0.35);
    cubic fill (-0.6, -0.4) (0.6, -0.4) (0.6, 0.35);
    close fill;
    move stroke (-1., 0.55);
    line stroke (1., 0.55);
    List.iter (fun ang ->
      move stroke (polar ang 0.8);
      line stroke (polar ang 1.05)) [ -1.57; -0.8; -2.34 ]
  | Autumn, 0 -> lens fill (-1., 0.) (1., 0.) 0.35
  | Autumn, 1 ->
    move fill (-0.45, 0.05);
    quad fill (-0.45, 1.) (0., 1.);
    quad fill (0.45, 1.) (0.45, 0.05);
    close fill;
    move fill (-0.6, -0.05);
    quad fill (0., -0.8) (0.6, -0.05);
    close fill;
    move stroke (0., -0.55);
    line stroke (0.1, -1.)
  | Autumn, 2 ->
    move fill (-0.95, 0.1);
    quad fill (-0.95, -0.95) (0., -0.95);
    quad fill (0.95, -0.95) (0.95, 0.1);
    close fill;
    move fill (-0.28, 0.25);
    line fill (-0.22, 1.);
    line fill (0.22, 1.);
    line fill (0.28, 0.25);
    close fill
  | Autumn, _ ->
    move fill (0., -0.95);
    quad fill (0.95, -0.35) (0., 0.7);
    quad fill (-0.95, -0.35) (0., -0.95);
    close fill;
    move stroke (0., 0.7);
    line stroke (0.12, 1.)

(** [season_paths ~month ~seed ~y0 ~height] is the svg paths that draw the
    motifs of [motifs], as [(season, filled, d)]: a path for each season and
    each of stroke and fill that is used. *)
let season_paths ~month ~seed ~y0 ~height =
  let ms = motifs ~month ~seed ~y0 ~height in
  List.concat_map (fun season ->
    let stroke = Buffer.create 256 and fill = Buffer.create 256 in
    List.iter (fun m -> if m.season = season then draw m ~stroke ~fill) ms;
    let item filled b =
      if Buffer.length b = 0 then []
      else [ (season, filled, String.trim (Buffer.contents b)) ]
    in
    item false stroke @ item true fill) [ Winter; Spring; Summer; Autumn ]

(** [season_stops marks ~total] is the colour stops of the spine for months laid
    out as [marks], each [(month, top, height)] in order down a timeline
    [total] tall. A stop sits at the middle of each month, as an offset from 0
    to 1 with its season, so that the spine blends from one season into the
    next. *)
let season_stops marks ~total =
  List.map (fun (month, top, h) ->
    (Float.min 1. (Float.max 0. ((top +. (h /. 2.)) /. total)),
     season_of_month month)) marks
