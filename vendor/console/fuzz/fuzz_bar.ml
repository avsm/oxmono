(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* A bar is exactly [width] cells, two more with brackets, for any fraction;
   a negative width is refused. *)
let test_width n f b =
  let width = n mod 100 in
  match Bar.horizontal ~brackets:b ~width f with
  | bar ->
      if width < 0 then fail "negative width accepted";
      check_eq ~pp:Fmt.int
        (width + if b then 2 else 0)
        (Width.string_width (View.to_string bar))
  | exception Invalid_argument _ -> if width >= 0 then fail "width refused"

(* A vertical bar is one cell wide and exactly as tall as its room, at any
   fraction. *)
let test_vertical_height h frac utf_8 =
  let height = 1 + (abs h mod 40) in
  let s =
    View_harness.at ~utf_8 ~time:0. (View.sized ~height (Bar.vertical frac))
  in
  let lines = String.split_on_char '\n' s in
  check_eq ~pp:Fmt.int height (List.length lines);
  List.iter (fun l -> check_eq ~pp:Fmt.int 1 (View.width (View.text l))) lines

let check_room width height v =
  let v = View.sized ~width ~height v in
  check (View.width v <= width);
  check (View.height v <= height)

let test_horizontal_room n requested brackets frac =
  let width = 1 + (n mod 200) in
  let requested = requested mod 240 in
  check_room width 1 (Bar.horizontal ~width:requested ~brackets frac);
  check_room width 1 (Bar.horizontal ~brackets frac)

let test_vertical_room n h requested frac =
  check_room
    (1 + (n mod 200))
    (1 + (h mod 30))
    (Bar.vertical ~width:(requested mod 240) frac)

let test_sparkline_room n values =
  check_room (1 + (n mod 200)) 1 (Bar.sparkline values)

let test_chart_room n h labels =
  let bars = List.mapi (fun i label -> (label, float_of_int (i + 1))) labels in
  check_room (1 + (n mod 200)) (1 + (h mod 30)) (Bar.chart bars)

let suite =
  ( "bar",
    [
      test_case "horizontal bars keep to their room"
        [ uint8; uint8; bool; float ]
        test_horizontal_room;
      test_case "fixed vertical bars keep to their room"
        [ uint8; uint8; uint8; float ]
        test_vertical_room;
      test_case "sparklines keep to their room"
        [ uint8; list float ]
        test_sparkline_room;
      test_case "charts keep to their room"
        [ uint8; uint8; list bytes ]
        test_chart_room;
      test_case "width" [ int; float; bool ] test_width;
      test_case "a vertical bar is as tall as its room" [ int; float; bool ]
        test_vertical_height;
    ] )
