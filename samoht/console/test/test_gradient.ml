(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let color = Alcotest.testable Color.pp Color.equal
let black = Color.rgb 0 0 0
let white = Color.rgb 255 255 255
let red = Color.rgb 255 0 0
let blue = Color.rgb 0 0 255

let test_invalid () =
  Alcotest.check_raises "no stop"
    (Invalid_argument "Console.Gradient.v: no colour stop") (fun () ->
      ignore (Gradient.v ~length:1 []));
  Alcotest.check_raises "zero length"
    (Invalid_argument "Console.Gradient.v: non-positive length") (fun () ->
      ignore (Gradient.v ~length:0 [ red ]))

(* Evenly spaced stops, each pair blended component by component. *)
let test_at () =
  let g = Gradient.v ~length:1 [ black; white ] in
  Alcotest.check color "midpoint" (Color.rgb 128 128 128) (Gradient.at g 0.5);
  Alcotest.check color "clamped below" black (Gradient.at g (-1.));
  Alcotest.check color "clamped above" white (Gradient.at g 2.);
  let three = Gradient.v ~length:1 [ red; black; blue ] in
  Alcotest.check color "second half" (Color.rgb 0 0 128)
    (Gradient.at three 0.75);
  Alcotest.check color "stop in the middle" black (Gradient.at three 0.5);
  let one = Gradient.v ~length:1 [ red ] in
  Alcotest.check color "one stop" red (Gradient.at one 0.3)

(* SVG 1.1 13.2.2 spreadMethod, over [length] cells along the direction. *)
let test_spread () =
  let at spread ?direction ~row ~column () =
    Gradient.cell
      (Gradient.v ~spread ?direction ~length:4 [ black; white ])
      ~row ~column
  in
  let grey v = Color.rgb v v v in
  Alcotest.check color "pad inside" (grey 128) (at `Pad ~row:0 ~column:2 ());
  Alcotest.check color "pad past the end" white (at `Pad ~row:0 ~column:9 ());
  Alcotest.check color "repeat" (grey 64) (at `Repeat ~row:0 ~column:5 ());
  Alcotest.check color "repeat below zero" (grey 191)
    (at `Repeat ~row:0 ~column:(-1) ());
  Alcotest.check color "reflect on the way back" (grey 128)
    (at `Reflect ~row:0 ~column:6 ());
  Alcotest.check color "reflect at the turn" white
    (at `Reflect ~row:0 ~column:4 ());
  Alcotest.check color "rows count by direction" (grey 128)
    (at `Pad ~direction:(1, 2) ~row:1 ~column:0 ());
  Alcotest.check color "a vertical gradient ignores columns" black
    (at `Pad ~direction:(0, 1) ~row:0 ~column:3 ())

let test_shift () =
  let g =
    Gradient.v ~spread:`Reflect ~direction:(1, 2) ~length:4 [ black; white ]
  in
  let shifted = Gradient.shift ~row:1 ~column:2 g in
  Alcotest.check color "moved origin"
    (Gradient.cell g ~row:1 ~column:2)
    (Gradient.cell shifted ~row:0 ~column:0);
  Alcotest.check color "a cell further"
    (Gradient.cell g ~row:3 ~column:3)
    (Gradient.cell shifted ~row:2 ~column:1);
  Alcotest.(check bool)
    "shifts compose" true
    (Gradient.equal shifted
       (Gradient.shift ~row:1 ~column:0 (Gradient.shift ~row:0 ~column:2 g)));
  Alcotest.(check bool)
    "a shift is part of equality" false (Gradient.equal g shifted)

let test_map_equal () =
  let g = Gradient.v ~length:2 [ black; white ] in
  Alcotest.(check bool)
    "equal" true
    (Gradient.equal g (Gradient.v ~length:2 [ black; white ]));
  Alcotest.(check bool)
    "length differs" false
    (Gradient.equal g (Gradient.v ~length:3 [ black; white ]));
  Alcotest.(check bool)
    "spread differs" false
    (Gradient.equal g (Gradient.v ~spread:`Repeat ~length:2 [ black; white ]));
  Alcotest.(check bool)
    "direction differs" false
    (Gradient.equal g (Gradient.v ~direction:(0, 1) ~length:2 [ black; white ]));
  Alcotest.(check (list color))
    "map" [ white; white ]
    (Gradient.stops (Gradient.map (fun _ -> white) g))

let suite =
  ( "gradient",
    [
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "at" `Quick test_at;
      Alcotest.test_case "spread" `Quick test_spread;
      Alcotest.test_case "map and equal" `Quick test_map_equal;
      Alcotest.test_case "shift" `Quick test_shift;
    ] )
