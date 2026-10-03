(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_none () =
  let ansi = Style.to_ansi Style.none in
  Alcotest.(check string) "none produces empty" "" ansi

let test_bold () =
  let ansi = Style.to_ansi Style.bold in
  Alcotest.(check string) "bold ANSI code" "\027[1m" ansi

let test_composition () =
  let style = Style.(bold + fg Color.red + underline) in
  Alcotest.(check bool) "composed style not none" false (Style.is_none style)

let render renderer pp v =
  let buf = Buffer.create 32 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let test_styled_honours_formatter () =
  let pp = Style.styled Style.bold Fmt.string in
  Alcotest.(check string) "plain formatter" "hello" (render `None pp "hello");
  Alcotest.(check bool)
    "ANSI formatter" true
    (String.contains (render `Ansi_tty pp "hello") '\027')

let red = Color.rgb 255 0 0
let blue = Color.rgb 0 0 255
let ramp = Gradient.v ~length:1 [ red; blue ]

let test_gradient_style () =
  let s = Style.(bold + fg_gradient ramp) in
  Alcotest.(check bool) "has a gradient" true (Style.has_gradient s);
  Alcotest.(check bool)
    "a colour is no gradient" false
    (Style.has_gradient Style.(bold + fg red + bg blue));
  Alcotest.(check bool)
    "background gradient" true
    (Style.has_gradient (Style.bg_gradient ramp));
  Alcotest.(check bool)
    "resolved on a cell" true
    (Style.equal Style.(bold + fg blue) (Style.at ~row:0 ~column:1 s));
  Alcotest.(check (option string))
    "foreground at the origin" (Some "#ff0000")
    (Option.map (Fmt.str "%a" Color.pp) (Style.foreground s));
  Alcotest.(check string)
    "to_ansi at the origin" "\027[1;38;2;255;0;0m" (Style.to_ansi s);
  Alcotest.(check bool)
    "gradients compare by value" false
    (Style.equal s
       Style.(bold + fg_gradient (Gradient.v ~length:2 [ red; blue ])))

let test_shows_on_space () =
  let check label expected s =
    Alcotest.(check bool) label expected (Style.shows_on_space s)
  in
  check "foreground" false Style.(bold + faint + italic + blink + fg red);
  check "background" true (Style.bg red);
  check "background gradient" true (Style.bg_gradient ramp);
  check "reverse" true Style.reverse;
  check "underline" true Style.underline;
  check "strikethrough" true Style.strikethrough

let test_map_color () =
  let s = Style.(fg red + bg_gradient ramp) in
  let mapped = Style.map_color ~bg:(fun _ -> red) s in
  Alcotest.(check bool)
    "background stops mapped, foreground kept" true
    (Style.equal mapped
       Style.(fg red + bg_gradient (Gradient.v ~length:1 [ red; red ])));
  Alcotest.(check bool)
    "foreground mapped" true
    (Style.equal
       (Style.map_color ~fg:(fun _ -> blue) (Style.fg red))
       (Style.fg blue))

let suite =
  ( "style",
    [
      Alcotest.test_case "gradient style" `Quick test_gradient_style;
      Alcotest.test_case "map colour" `Quick test_map_color;
      Alcotest.test_case "shows on space" `Quick test_shows_on_space;
      Alcotest.test_case "none" `Quick test_none;
      Alcotest.test_case "bold" `Quick test_bold;
      Alcotest.test_case "composition" `Quick test_composition;
      Alcotest.test_case "formatter controls styling" `Quick
        test_styled_honours_formatter;
    ] )
