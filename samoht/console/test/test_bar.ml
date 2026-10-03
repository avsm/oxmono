(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* Count non-overlapping occurrences of [sub] in [s]. ANSI escapes never contain
   the block glyphs, so counting them measures the visible fill directly. *)
let count sub s =
  let n = String.length sub and m = String.length s in
  let c = ref 0 and i = ref 0 in
  while !i + n <= m do
    if String.sub s !i n = sub then begin
      incr c;
      i := !i + n
    end
    else incr i
  done;
  !c

let full = count "█"
let track = count "░"

(* Blocky fills [pct]% of the [width] cells with full blocks and the rest with a
   shaded track. *)
let test_blocky_fill () =
  let s = Bar.render ~style:`Blocky ~width:10 ~pct:30 () in
  Alcotest.(check int) "three full blocks" 3 (full s);
  Alcotest.(check int) "seven track cells" 7 (track s)

let test_blocky_extremes () =
  let e = Bar.render ~style:`Blocky ~width:10 ~pct:0 () in
  Alcotest.(check int) "empty has no fill" 0 (full e);
  Alcotest.(check int) "empty is all track" 10 (track e);
  let f = Bar.render ~style:`Blocky ~width:10 ~pct:100 () in
  Alcotest.(check int) "full is all fill" 10 (full f);
  Alcotest.(check int) "full has no track" 0 (track f)

(* [pct] is clamped to 0..100: out-of-range values render as the endpoints. *)
let test_pct_clamped () =
  Alcotest.(check string)
    "negative clamps to 0"
    (Bar.render ~style:`Blocky ~width:8 ~pct:0 ())
    (Bar.render ~style:`Blocky ~width:8 ~pct:(-50) ());
  Alcotest.(check string)
    "over 100 clamps to 100"
    (Bar.render ~style:`Blocky ~width:8 ~pct:100 ())
    (Bar.render ~style:`Blocky ~width:8 ~pct:200 ())

(* Smooth uses whole eighth-cell fills: 50% of 10 cells is exactly 5 full cells
   (80 eighths * 50% = 40 = 5 cells, no partial). *)
let test_smooth_fill () =
  let s = Bar.render ~style:`Smooth ~width:10 ~pct:50 () in
  Alcotest.(check int) "five full cells" 5 (full s)

(* A single cell at 50% is a half-cell glyph (8 eighths * 50% = 4 = the "▌"). *)
let test_smooth_partial () =
  let s = Bar.render ~style:`Smooth ~width:1 ~pct:50 () in
  Alcotest.(check int) "one half-cell glyph" 1 (count "▌" s)

(* Style and colour resolve from the theme when not given, and an explicit style
   overrides the theme's. *)
let test_style_resolution () =
  Alcotest.(check int)
    "dos theme gives a blocky bar" 5
    (full (Bar.render ~theme:Theme.dos ~width:10 ~pct:50 ()));
  Alcotest.(check int)
    "explicit smooth overrides the theme" 5
    (full (Bar.render ~theme:Theme.dos ~style:`Smooth ~width:10 ~pct:50 ()))

(* The indeterminate bar sweeps a triangle wave up and back over two seconds. At
   width 100 the blocky fill count equals the percentage. *)
let test_at_triangle () =
  let pct elapsed = full (Bar.at ~style:`Blocky ~width:100 ~elapsed ()) in
  Alcotest.(check int) "t=0 empty" 0 (pct 0.);
  Alcotest.(check int) "t=0.5 half, rising" 50 (pct 0.5);
  Alcotest.(check int) "t=1 full at the peak" 100 (pct 1.0);
  Alcotest.(check int) "t=1.5 half, falling" 50 (pct 1.5);
  Alcotest.(check int) "t=2 back to empty" 0 (pct 2.0)

let test_invalid_width () =
  Alcotest.check_raises "render width"
    (Invalid_argument "Console.Bar.render: negative width") (fun () ->
      ignore (Bar.render ~width:(-1) ~pct:0 ()));
  Alcotest.check_raises "animation width"
    (Invalid_argument "Console.Bar.at: negative width") (fun () ->
      ignore (Bar.at ~width:(-1) ~elapsed:0. ()))

let suite =
  ( "bar",
    [
      Alcotest.test_case "blocky fill" `Quick test_blocky_fill;
      Alcotest.test_case "blocky extremes" `Quick test_blocky_extremes;
      Alcotest.test_case "pct clamped" `Quick test_pct_clamped;
      Alcotest.test_case "smooth fill" `Quick test_smooth_fill;
      Alcotest.test_case "smooth partial" `Quick test_smooth_partial;
      Alcotest.test_case "style resolution" `Quick test_style_resolution;
      Alcotest.test_case "at triangle wave" `Quick test_at_triangle;
      Alcotest.test_case "invalid width" `Quick test_invalid_width;
    ] )
