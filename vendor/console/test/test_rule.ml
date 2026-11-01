(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let at width rule =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf width;
  View.pp ppf rule;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_plain () =
  Alcotest.(check string) "unlabelled" "-----" (at 5 (Rule.view ~glyph:"-" ()))

let test_label () =
  let label = View.styled Style.bold (View.text "Build") in
  Alcotest.(check string)
    "center" "-- Build --"
    (at 11 (Rule.view ~glyph:"-" ~label ()));
  Alcotest.(check string)
    "left" "Build -----"
    (at 11 (Rule.view ~glyph:"-" ~align:`Left ~label ()));
  Alcotest.(check string)
    "right" "----- Build"
    (at 11 (Rule.view ~glyph:"-" ~align:`Right ~label ()))

(* A label wider than the line is cut to it, not refused. *)
let test_long_label_is_cut () =
  Alcotest.(check string)
    "cut to the margin" "lo…"
    (at 3 (Rule.view ~label:(View.text "long") ()))

(* [to_string] draws a rule as [pp] does at a margin of [width], in
   {!Theme.default} and UTF-8. *)
let test_to_string () =
  let rule = Rule.view ~label:(View.text "Build") () in
  Alcotest.(check string)
    "plain"
    "\u{2500}\u{2500}\u{2500}\u{2500} Build \u{2500}\u{2500}\u{2500}\u{2500}"
    (View.to_string (View.sized ~width:15 rule));
  Alcotest.(check string)
    "as pp"
    (at 15 (Rule.view ~glyph:"=" ()))
    (View.to_string (View.sized ~width:15 (Rule.view ~glyph:"=" ())));
  Alcotest.(check string)
    "no room" ""
    (View.to_string (View.sized ~width:0 rule));
  let styled =
    View.to_string ~styled:true (View.sized ~width:5 (Rule.view ()))
  in
  Alcotest.(check bool)
    "in the theme's muted colour" true
    (String.starts_with ~prefix:"\027[" styled)

let test_invalid () =
  Alcotest.check_raises "wide glyph"
    (Invalid_argument "Console.Rule.view: glyph must occupy one cell")
    (fun () -> ignore (Rule.view ~glyph:"--" ()));
  Alcotest.check_raises "a control is a space"
    (Invalid_argument "Console.Rule.view: glyph must occupy one cell")
    (fun () -> ignore (Rule.view ~glyph:"-\027" ()))

let suite =
  ( "rule",
    [
      Alcotest.test_case "to_string" `Quick test_to_string;
      Alcotest.test_case "plain" `Quick test_plain;
      Alcotest.test_case "label" `Quick test_label;
      Alcotest.test_case "long label is cut" `Quick test_long_label_is_cut;
      Alcotest.test_case "invalid" `Quick test_invalid;
    ] )
