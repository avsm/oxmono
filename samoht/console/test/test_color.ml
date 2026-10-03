(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_hex_6digit () =
  let color = Color.hex "#FF5500" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "6-digit hex" true (Color.equal color expected)

let test_hex_3digit () =
  let color = Color.hex "#F50" in
  let expected = Color.rgb 255 85 0 in
  Alcotest.(check bool) "3-digit hex" true (Color.equal color expected)

let test_hex_no_hash () =
  let color = Color.hex "FF5500" in
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

let test_invalid_constructors () =
  Alcotest.check_raises "malformed hex"
    (Invalid_argument "Console.Color.hex: expected 3 or 6 hex digits")
    (fun () -> ignore (Color.hex "1234"));
  Alcotest.check_raises "invalid hex digit"
    (Invalid_argument "Console.Color.hex: invalid hex character") (fun () ->
      ignore (Color.hex "12x"));
  Alcotest.check_raises "palette below range"
    (Invalid_argument "Console.Color.palette: index outside 0..255") (fun () ->
      ignore (Color.palette (-1)));
  Alcotest.check_raises "palette above range"
    (Invalid_argument "Console.Color.palette: index outside 0..255") (fun () ->
      ignore (Color.palette 256))

let test_codes () =
  let check = Alcotest.(check string) in
  check "red foreground" "31" (Color.to_fg_code Color.red);
  check "bright cyan background" "106" (Color.to_bg_code Color.bright_cyan);
  check "RGB foreground" "38;2;1;2;3" (Color.to_fg_code (Color.rgb 1 2 3));
  check "palette background" "48;5;42" (Color.to_bg_code (Color.palette 42))

let color = Alcotest.testable Color.pp Color.equal
let env pairs name = List.assoc_opt name pairs

let depth =
  Alcotest.testable
    (fun ppf d ->
      Fmt.string ppf
        (match d with
        | `Ansi_16 -> "16"
        | `Ansi_256 -> "256"
        | `True_color -> "true colour"))
    ( = )

let test_depth_of_env () =
  let check label expected pairs =
    Alcotest.check depth label expected (Color.depth_of_env (env pairs))
  in
  check "COLORTERM truecolor" `True_color
    [ ("COLORTERM", "truecolor"); ("TERM", "xterm") ];
  check "COLORTERM 24bit" `True_color [ ("COLORTERM", "24bit") ];
  check "TERM direct" `True_color [ ("TERM", "xterm-direct") ];
  check "TERM 256color" `Ansi_256 [ ("TERM", "screen-256color") ];
  check "other COLORTERM" `Ansi_256
    [ ("COLORTERM", "yes"); ("TERM", "xterm-256color") ];
  check "plain xterm" `Ansi_16 [ ("TERM", "xterm") ];
  check "nothing" `Ansi_16 []

(* xterm-256 entries whose RGB values are the palette's own: the cube, the grey
   ramp, and a colour nearer a grey than any cube entry. *)
let test_downsample_256 () =
  let check label expected c =
    Alcotest.check color label (Color.palette expected)
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
  check "low palette entry" Color.bright_red (Color.palette 9);
  check "high palette entry" Color.bright_red (Color.palette 196);
  check "named stays" Color.cyan Color.cyan

let test_rgb () =
  let rgb = Alcotest.(triple int int int) in
  Alcotest.check rgb "cube" (95, 135, 175) (Color.to_rgb (Color.palette 67));
  Alcotest.check rgb "grey" (128, 128, 128) (Color.to_rgb (Color.palette 244));
  Alcotest.check rgb "named" (0, 0, 238) (Color.to_rgb Color.blue);
  Alcotest.check rgb "low palette" (0, 0, 238) (Color.to_rgb (Color.palette 4));
  Alcotest.check color "blend" (Color.rgb 128 128 128)
    (Color.blend (Color.rgb 0 0 0) (Color.rgb 255 255 255) 0.5);
  Alcotest.check color "blend clamps" (Color.rgb 255 255 255)
    (Color.blend (Color.rgb 0 0 0) (Color.rgb 255 255 255) 2.);
  Alcotest.check color "scale" (Color.rgb 128 50 2)
    (Color.scale 0.5 (Color.rgb 255 100 3));
  Alcotest.check color "scale clamps" (Color.rgb 255 255 255)
    (Color.scale 3. (Color.rgb 100 100 100))

(* A colour is written at the process's depth. *)
let test_codes_at_depth () =
  Color.set_depth `Ansi_256;
  Fun.protect
    ~finally:(fun () -> Color.set_depth `True_color)
    (fun () ->
      Alcotest.(check string)
        "foreground" "38;5;196"
        (Color.to_fg_code (Color.rgb 255 0 0));
      Alcotest.(check string)
        "background" "48;5;21"
        (Color.to_bg_code (Color.rgb 0 0 255)))

let suite =
  ( "color",
    [
      Alcotest.test_case "hex 6-digit" `Quick test_hex_6digit;
      Alcotest.test_case "hex 3-digit" `Quick test_hex_3digit;
      Alcotest.test_case "hex no hash" `Quick test_hex_no_hash;
      Alcotest.test_case "predefined" `Quick test_predefined;
      Alcotest.test_case "rgb range" `Quick test_rgb_range;
      Alcotest.test_case "invalid constructors" `Quick test_invalid_constructors;
      Alcotest.test_case "ANSI codes" `Quick test_codes;
      Alcotest.test_case "depth of env" `Quick test_depth_of_env;
      Alcotest.test_case "downsample 256" `Quick test_downsample_256;
      Alcotest.test_case "downsample 16" `Quick test_downsample_16;
      Alcotest.test_case "rgb" `Quick test_rgb;
      Alcotest.test_case "codes at depth" `Quick test_codes_at_depth;
    ] )
