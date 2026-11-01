(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_basic () =
  let panel = Panel.view (View.text "content") in
  let output = View.to_string panel in
  Alcotest.(check bool) "contains content" true (contains "content" output)

let test_with_title () =
  let panel = Panel.view ~title:(View.text "Title") (View.text "body") in
  let output = View.to_string panel in
  Alcotest.(check bool) "contains title" true (contains "Title" output);
  Alcotest.(check bool) "contains body" true (contains "body" output)

let render renderer panel =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  View.pp ppf panel;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_multiline_preserves_styles () =
  let content =
    View.(
      styled Style.(fg Color.red) (text "red")
      ++ text "\n"
      ++ styled Style.(fg Color.blue) (text "blue"))
  in
  let panel = Panel.view content in
  let ansi = render `Ansi_tty panel in
  Alcotest.(check bool) "first line is styled" true (contains "\027[31m" ansi);
  Alcotest.(check bool) "second line is styled" true (contains "\027[34m" ansi);
  Alcotest.(check bool)
    "plain formatter has no escapes" false
    (String.contains (render `None panel) '\027')

let test_invalid_geometry () =
  Alcotest.check_raises "negative padding"
    (Invalid_argument "Console.Panel.view: negative padding") (fun () ->
      ignore (Panel.view ~padding:(-1) (View.text "x")));
  Alcotest.(check string)
    "a room too small cuts the frame" "╭─…\n│ …\n╰─…"
    (View.to_string
       (View.sized ~width:3
          (Panel.view ~padding:2 ~expand:true (View.text "x"))))

let test_fixed_width_truncates () =
  let panel =
    View.sized ~width:8
      (Panel.view ~padding:1 ~expand:true ~title:(View.text "long title")
         (View.text "abcdefgh"))
  in
  let rendered = View.to_string panel |> String.split_on_char '\n' in
  Alcotest.(check int) "three lines" 3 (List.length rendered);
  List.iter
    (fun line ->
      Alcotest.(check int)
        "fixed width" 8
        (Width.string_width (Console_text.strip line)))
    rendered;
  Alcotest.(check bool)
    "content was clipped" false
    (contains "abcdefgh" (String.concat "\n" rendered))

let lines s = String.split_on_char '\n' s

let render_at margin panel =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  View.pp ppf panel;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The formatter's margin is a ceiling on the box: a line wider than the room
   wraps at its words, each continuation hanging under the text after the
   line's first word, and the box is exactly the margin wide. *)
let test_margin_wraps () =
  let panel =
    Panel.view
      (View.text
         "REFUSED main has no baseline and the gate worktree does not exist\nok")
  in
  Alcotest.(check string)
    "wrapped box"
    (String.concat "\n"
       [
         "╭────────────────────────────╮";
         "│ REFUSED main has no        │";
         "│         baseline and the   │";
         "│         gate worktree does │";
         "│         not exist          │";
         "│ ok                         │";
         "╰────────────────────────────╯";
       ])
    (render_at 30 panel)

(* A box that fits keeps its natural width, and [to_string] is natural. *)
let test_margin_natural () =
  Alcotest.(check string)
    "fits" "╭────╮\n│ ok │\n╰────╯"
    (render_at 30 (Panel.view (View.text "ok")));
  let long = String.make 100 'x' in
  Alcotest.(check int)
    "to_string is natural" 104
    (Width.string_width
       (List.hd (lines (View.to_string (Panel.view (View.text long))))))

(* A styled border opens one sequence for each run of border glyphs. *)
let test_styled_border () =
  let red = Color.rgb 255 0 0 in
  let panel =
    Panel.view ~padding:0
      ~border:(Border.with_style (Style.fg red) Border.single)
      (View.text "x")
  in
  let r = "\027[38;2;255;0;0m" and reset = "\027[0m" in
  Alcotest.(check string)
    "runs"
    (String.concat ""
       [
         r;
         "\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x90";
         reset;
         "\n";
         r;
         "\xe2\x94\x82";
         reset;
         "x";
         r;
         "\xe2\x94\x82";
         reset;
         "\n";
         r;
         "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x98";
         reset;
       ])
    (View.to_string ~styled:true panel)

let test_wide_border () =
  let border =
    Border.v
      Border.
        {
          top_left = "\u{754c}";
          top = "\u{754c}";
          top_right = "\u{754c}";
          left = "\u{754c}";
          right = "\u{754c}";
          bottom_left = "\u{754c}";
          bottom = "\u{754c}";
          bottom_right = "\u{754c}";
          vertical = "\u{754c}";
          horizontal = "\u{754c}";
          cross = "\u{754c}";
          top_cross = "\u{754c}";
          bottom_cross = "\u{754c}";
          left_cross = "\u{754c}";
          right_cross = "\u{754c}";
        }
  in
  let out =
    View.to_string
      (View.sized ~width:12 (Panel.view ~border ~expand:true (View.text "x")))
  in
  let widths =
    List.map (fun l -> View.width (View.text l)) (String.split_on_char '\n' out)
  in
  Alcotest.(check (list int))
    "all edges respect fixed width" [ 12; 12; 12 ] widths

let test_to_string_width () =
  List.iter
    (fun line ->
      Alcotest.(check bool)
        ("within 12 cells: " ^ line)
        true
        (View.width (View.text line) <= 12))
    (String.split_on_char '\n'
       (View.to_string
          (View.sized ~width:12
             (Panel.view (View.text "a line longer than twelve cells")))))

let test_overflowing_padding () =
  Alcotest.check_raises "no line can hold this padding"
    (Invalid_argument "Console.Panel.view: padding too large") (fun () ->
      ignore (Panel.view ~padding:max_int (View.text "x")))

let test_title_follows_state () =
  let name = ref "one" in
  let label = View.fit (fun _ -> View.text !name) in
  let panel = Panel.view ~title:label ~subtitle:label (View.text "body") in
  let first = View.to_string panel in
  name := "two";
  let second = View.to_string panel in
  Alcotest.(check bool) "first title" true (contains "one" first);
  Alcotest.(check bool) "title follows" true (contains " two " second);
  Alcotest.(check bool) "old title gone" false (contains "one" second)

let suite =
  ( "panel",
    [
      Alcotest.test_case "a fit title follows the state" `Quick
        test_title_follows_state;
      Alcotest.test_case "huge padding is refused before drawing" `Quick
        test_overflowing_padding;
      Alcotest.test_case "to_string within a width" `Quick test_to_string_width;
      Alcotest.test_case "two-cell border respects fixed width" `Quick
        test_wide_border;
      Alcotest.test_case "styled border" `Quick test_styled_border;
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "with title" `Quick test_with_title;
      Alcotest.test_case "multiline styles" `Quick
        test_multiline_preserves_styles;
      Alcotest.test_case "invalid geometry" `Quick test_invalid_geometry;
      Alcotest.test_case "fixed width truncates" `Quick
        test_fixed_width_truncates;
      Alcotest.test_case "margin wraps" `Quick test_margin_wraps;
      Alcotest.test_case "margin natural" `Quick test_margin_natural;
    ] )
