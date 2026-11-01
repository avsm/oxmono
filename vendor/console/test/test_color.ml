(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let hex s = Result.get_ok (Color.of_hex s)

let test_hex_6digit () =
  let color = hex "#FF5500" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "6-digit hex" true (Color.equal color expected)

let test_hex_3digit () =
  let color = hex "#F50" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "3-digit hex" true (Color.equal color expected)

let test_hex_no_hash () =
  let color = hex "FF5500" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "hex without #" true (Color.equal color expected)

let test_predefined () =
  Alcotest.(check bool) "red equals red" true (Color.equal Color.red Color.red);
  Alcotest.(check bool)
    "red not equal blue" false
    (Color.equal Color.red Color.blue)

let test_rgb_range () =
  let rejected label rgb =
    Alcotest.check_raises label
      (Invalid_argument "Console.Color.rgb: component outside 0..255") rgb
  in
  rejected "negative component" (fun () -> ignore (Color.rgb (-1) 0 0));
  rejected "component above 255" (fun () -> ignore (Color.rgb 0 0 256))

let test_of_hex () =
  let parsed s =
    match Color.of_hex s with
    | Ok c -> Some (Color.to_rgb c)
    | Error (`Msg _) -> None
  in
  let check = Alcotest.(check (option (triple int int int))) in
  check "six digits" (Some (255, 85, 0)) (parsed "#FF5500");
  check "three digits" (Some (255, 85, 0)) (parsed "f50");
  check "four digits" None (parsed "1234");
  check "not a digit" None (parsed "12x");
  check "empty" None (parsed "");
  check "a lone hash" None (parsed "#")

let test_invalid_constructors () =
  Alcotest.check_raises "palette below range"
    (Invalid_argument "Console.Color.ansi_256: index outside 0..255") (fun () ->
      ignore (Color.ansi_256 (-1)));
  Alcotest.check_raises "palette above range"
    (Invalid_argument "Console.Color.ansi_256: index outside 0..255") (fun () ->
      ignore (Color.ansi_256 256))

(* The parameters of the escape code that selects a colour, as a span in
   that colour prints it. *)
let code style c =
  let s = View.to_string ~styled:true (View.styled (style c) (View.text "x")) in
  String.sub s 2 (String.index s 'm' - 2)

let to_fg_code = code Style.fg
let to_bg_code = code Style.bg

let test_codes () =
  let check = Alcotest.(check string) in
  check "red foreground" "31" (to_fg_code Color.red);
  check "bright cyan background" "106" (to_bg_code Color.bright_cyan);
  check "RGB foreground" "38;2;1;2;3" (to_fg_code (Color.rgb 1 2 3));
  check "palette background" "48;5;42" (to_bg_code (Color.ansi_256 42))

let color = Alcotest.testable Color.pp Color.equal

let depth =
  Alcotest.testable
    (fun ppf d ->
      Fmt.string ppf
        (match d with
        | `Ansi_16 -> "16"
        | `Ansi_256 -> "256"
        | `True_color -> "true colour"))
    ( = )

(* A formatter keeps its own depth, 16 colours until it is set: the colours
   every terminal shows. *)
let test_formatter_depth () =
  let a = Format.formatter_of_buffer (Buffer.create 1) in
  let b = Format.formatter_of_buffer (Buffer.create 1) in
  Alcotest.check depth "default" `Ansi_16 (Color.depth a);
  Color.set_depth a `Ansi_256;
  Fmt.set_style_renderer a `Ansi_tty;
  Alcotest.check depth "set" `Ansi_256 (Color.depth a);
  Alcotest.check depth "kept across Fmt's own settings" `Ansi_256
    (Color.depth a);
  Color.set_depth a `True_color;
  Alcotest.check depth "set again" `True_color (Color.depth a);
  Alcotest.check depth "another formatter" `Ansi_16 (Color.depth b)

(* xterm-256 entries whose RGB values are the palette's own: the cube, the grey
   ramp, and a colour nearer a grey than any cube entry. *)
let test_downsample_256 () =
  let check label expected c =
    Alcotest.check color label (Color.ansi_256 expected)
      (Color.downsample `Ansi_256 c)
  in
  check "cube corner" 196 (Color.rgb 255 0 0);
  check "cube entry" 67 (Color.rgb 95 135 175);
  check "grey ramp" 244 (Color.rgb 128 128 128);
  check "near a cube level" 196 (Color.rgb 250 3 0);
  Alcotest.check color "a named colour stays" Color.red
    (Color.downsample `Ansi_256 Color.red);
  Alcotest.check color "true colour keeps RGB" (Color.rgb 1 2 3)
    (Color.downsample `True_color (Color.rgb 1 2 3))

let test_downsample_16 () =
  let check label expected c =
    Alcotest.check color label expected (Color.downsample `Ansi_16 c)
  in
  check "bright red" Color.bright_red (Color.rgb 250 10 10);
  check "red" Color.red (Color.rgb 200 0 0);
  check "grey" Color.bright_black (Color.rgb 130 130 130);
  check "low palette entry" Color.bright_red (Color.ansi_256 9);
  check "high palette entry" Color.bright_red (Color.ansi_256 196);
  check "named stays" Color.cyan Color.cyan

let test_rgb () =
  let rgb = Alcotest.(triple int int int) in
  Alcotest.check rgb "cube" (95, 135, 175) (Color.to_rgb (Color.ansi_256 67));
  Alcotest.check rgb "grey" (128, 128, 128) (Color.to_rgb (Color.ansi_256 244));
  Alcotest.check rgb "named" (0, 0, 238) (Color.to_rgb Color.blue);
  Alcotest.check rgb "low palette" (0, 0, 238) (Color.to_rgb (Color.ansi_256 4));
  Alcotest.check color "blend" (Color.rgb 128 128 128)
    (Color.blend (Color.rgb 0 0 0) (Color.rgb 255 255 255) 0.5);
  Alcotest.check color "blend clamps" (Color.rgb 255 255 255)
    (Color.blend (Color.rgb 0 0 0) (Color.rgb 255 255 255) 2.);
  Alcotest.check color "scale" (Color.rgb 128 50 2)
    (Color.scale 0.5 (Color.rgb 255 100 3));
  Alcotest.check color "scale clamps" (Color.rgb 255 255 255)
    (Color.scale 3. (Color.rgb 100 100 100))

let test_codes_at_depth () =
  let at d c = Color.downsample d c in
  Alcotest.(check string)
    "foreground" "38;5;196"
    (to_fg_code (at `Ansi_256 (Color.rgb 255 0 0)));
  Alcotest.(check string)
    "background" "48;5;21"
    (to_bg_code (at `Ansi_256 (Color.rgb 0 0 255)));
  Alcotest.(check string)
    "as given" "38;2;255;0;0"
    (to_fg_code (Color.rgb 255 0 0))

let suite =
  ( "color",
    [
      Alcotest.test_case "hex 6-digit" `Quick test_hex_6digit;
      Alcotest.test_case "hex 3-digit" `Quick test_hex_3digit;
      Alcotest.test_case "hex no hash" `Quick test_hex_no_hash;
      Alcotest.test_case "of_hex" `Quick test_of_hex;
      Alcotest.test_case "predefined" `Quick test_predefined;
      Alcotest.test_case "rgb range" `Quick test_rgb_range;
      Alcotest.test_case "invalid constructors" `Quick test_invalid_constructors;
      Alcotest.test_case "ANSI codes" `Quick test_codes;
      Alcotest.test_case "formatter depth" `Quick test_formatter_depth;
      Alcotest.test_case "downsample 256" `Quick test_downsample_256;
      Alcotest.test_case "downsample 16" `Quick test_downsample_16;
      Alcotest.test_case "rgb" `Quick test_rgb;
      Alcotest.test_case "codes at depth" `Quick test_codes_at_depth;
    ] )
