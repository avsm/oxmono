(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let frames = [ "a"; "b"; "c" ]

let at ?theme ?utf_8 ?frames time =
  View_harness.at ?theme ?utf_8 ~time (Spinner.view ?frames ())

let test_empty () =
  Alcotest.check_raises "empty frames rejected"
    (Invalid_argument "Console.Spinner.view: no frames") (fun () ->
      ignore (Spinner.view ~frames:[] ()))

let test_frame () =
  let at time = at ~frames time in
  Alcotest.(check (list string))
    "ten frames a second, cycling"
    [ "a"; "a"; "b"; "c"; "a" ]
    [ at 0.; at 0.05; at 0.1; at 0.2; at 0.3 ];
  Alcotest.(check string) "negative time" "a" (at (-1.));
  Alcotest.(check string) "infinite time" "a" (at Float.infinity);
  Alcotest.(check string) "nan" "a" (at Float.nan)

(* Printed once, a spinner is still: its first frame. *)
let test_printed () =
  Alcotest.(check string)
    "first frame" "a"
    (View.to_string (Spinner.view ~frames ()));
  Alcotest.(check string) "the theme's" "⠋" (View.to_string (Spinner.view ()))

let cycle ?theme ?utf_8 n =
  List.init n (fun i -> at ?theme ?utf_8 (float i /. 10.))

let test_predefined () =
  Alcotest.(check (list string))
    "braille"
    [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏"; "⠋" ]
    (cycle 11);
  Alcotest.(check (list string))
    "ascii theme"
    [ "|"; "/"; "-"; "\\"; "|" ]
    (cycle ~theme:Theme.ascii 5);
  Alcotest.(check (list string))
    "ascii formatter"
    [ "|"; "/"; "-"; "\\"; "|" ]
    (cycle ~utf_8:false 5);
  Alcotest.(check (list string))
    "the lists" [ "|"; "/"; "-"; "\\" ] Spinner.ascii;
  Alcotest.(check int) "braille" 10 (List.length Spinner.braille)

let test_still () =
  List.iter
    (fun time ->
      Alcotest.(check string)
        "one frame" "*"
        (at ~theme:(Theme.still Theme.default) time))
    [ 0.; 0.1; 0.25; 1.; 3600. ]

(* A spinner is as wide as its widest frame at every time. *)
let test_width () =
  let frames = [ "."; "..."; "\u{754c}" ] in
  List.iter
    (fun time ->
      Alcotest.(check int) "widest" 3 (View.width (View.text (at ~frames time))))
    [ 0.; 0.1; 0.2 ];
  Alcotest.(check int) "measured" 3 (View.width (Spinner.view ~frames ()))

let test_frames_print () =
  Alcotest.(check string)
    "control characters are spaces" "a b"
    (at ~frames:[ "a\027b" ] 0.)

let test_spinner_room () =
  let v = View.sized ~width:2 (Spinner.view ~frames:[ "a"; "long" ] ()) in
  let terminal = Console_vte.v ~rows:4 ~cols:2 () in
  Console_vte.feed terminal (View.to_string v);
  Alcotest.(check int)
    "one terminal row" 0
    (fst (Console_vte.cursor_pos terminal));
  Alcotest.(check bool) "within its room" true (View.width v <= 2)

let suite =
  ( "spinner",
    [
      Alcotest.test_case "custom spinners keep to their room" `Quick
        test_spinner_room;
      Alcotest.test_case "still does not move" `Quick test_still;
      Alcotest.test_case "frames print" `Quick test_frames_print;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "frame" `Quick test_frame;
      Alcotest.test_case "printed once" `Quick test_printed;
      Alcotest.test_case "as wide as its widest frame" `Quick test_width;
      Alcotest.test_case "predefined" `Quick test_predefined;
    ] )
