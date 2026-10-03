(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let red = Color.rgb 255 0 0
let blue = Color.rgb 0 0 255

let with_depth depth f =
  Color.set_depth depth;
  Fun.protect ~finally:(fun () -> Color.set_depth `True_color) f

let test_cell () =
  let refused g =
    Alcotest.check_raises g
      (Invalid_argument "Console.Canvas.cell: not one terminal cell") (fun () ->
        ignore (Canvas.cell g))
  in
  refused "ab";
  refused "\xe7\x95\x8c";
  refused "\n";
  refused "";
  Alcotest.(check string)
    "a half block" "\xe2\x96\x80"
    (Canvas.glyph (Canvas.cell "\xe2\x96\x80"))

(* A combining mark joins its base, a wide glyph becomes U+FFFD, a control a
   space: one cell per column. *)
let test_cells () =
  Alcotest.(check (list string))
    "glyphs"
    [ "e\xcc\x81"; "x"; "\xef\xbf\xbd"; " " ]
    (List.map Canvas.glyph (Canvas.cells "e\xcc\x81x\xe7\x95\x8c\027"))

let test_shape () =
  Alcotest.check_raises "ragged"
    (Invalid_argument "Console.Canvas.of_rows: rows of different lengths")
    (fun () -> ignore (Canvas.of_rows [ Canvas.cells "ab"; Canvas.cells "a" ]));
  let c =
    Canvas.v ~width:3 ~height:2 (fun ~row ~column ->
        Canvas.cell (string_of_int ((row * 3) + column)))
  in
  Alcotest.(check (pair int int)) "size" (3, 2) (Canvas.width c, Canvas.height c);
  Alcotest.(check string)
    "get" "5"
    (Canvas.glyph (Canvas.get c ~row:1 ~column:2));
  Alcotest.check_raises "outside"
    (Invalid_argument "Console.Canvas.get: position outside the canvas")
    (fun () -> ignore (Canvas.get c ~row:2 ~column:0));
  let flipped =
    Canvas.map
      (fun ~row ~column _ -> Canvas.cell (string_of_int ((column * 2) + row)))
      c
  in
  Alcotest.(check string)
    "map" "3"
    (Canvas.glyph (Canvas.get flipped ~row:1 ~column:1))

let ansi rows = List.map (fun s -> Span.to_ansi_string s) rows

(* A run is cut where the drawn sequence changes, not where the style value
   does: two reds the xterm-256 palette cannot tell apart are one run. *)
let test_runs () =
  let row =
    [
      Canvas.cell ~style:(Style.fg red) "a";
      Canvas.cell ~style:(Style.fg (Color.rgb 250 0 0)) "b";
      Canvas.cell "c";
      Canvas.cell ~style:(Style.bg blue) "d";
    ]
  in
  let c = Canvas.of_rows [ row ] in
  with_depth `Ansi_256 (fun () ->
      Alcotest.(check (list string))
        "256"
        [ "\027[38;5;196mab\027[0mc\027[48;5;21md\027[0m" ]
        (ansi (Canvas.rows c)));
  Alcotest.(check (list string))
    "true colour"
    [
      "\027[38;2;255;0;0ma\027[0m\027[38;2;250;0;0mb\027[0mc\027[48;2;0;0;255md\027[0m";
    ]
    (ansi (Canvas.rows c))

(* A gradient takes its colour on the canvas cell, row and column. *)
let test_gradient () =
  let g = Gradient.v ~direction:(0, 1) ~length:1 [ red; blue ] in
  let c =
    Canvas.v ~width:2 ~height:2 (fun ~row:_ ~column:_ ->
        Canvas.cell ~style:(Style.bg_gradient g) " ")
  in
  Alcotest.(check (list string))
    "rows"
    [ "\027[48;2;255;0;0m  \027[0m"; "\027[48;2;0;0;255m  \027[0m" ]
    (ansi (Canvas.rows c))

let test_pp () =
  let c = Canvas.of_rows [ Canvas.cells "abcdef"; Canvas.cells "ghijkl" ] in
  let buffer = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf 4;
  Canvas.pp ppf c;
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "cut to the margin" "abcd\nghij" (Buffer.contents buffer)

let suite =
  ( "canvas",
    [
      Alcotest.test_case "cell" `Quick test_cell;
      Alcotest.test_case "cells" `Quick test_cells;
      Alcotest.test_case "shape" `Quick test_shape;
      Alcotest.test_case "runs" `Quick test_runs;
      Alcotest.test_case "gradient" `Quick test_gradient;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
