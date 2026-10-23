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

(* Keep the original conversion and palette search as a rounding oracle. *)
let named =
  [|
    (Color.black, (0, 0, 0));
    (Color.red, (205, 0, 0));
    (Color.green, (0, 205, 0));
    (Color.yellow, (205, 205, 0));
    (Color.blue, (0, 0, 238));
    (Color.magenta, (205, 0, 205));
    (Color.cyan, (0, 205, 205));
    (Color.white, (229, 229, 229));
    (Color.bright_black, (127, 127, 127));
    (Color.bright_red, (255, 0, 0));
    (Color.bright_green, (0, 255, 0));
    (Color.bright_yellow, (255, 255, 0));
    (Color.bright_blue, (92, 92, 255));
    (Color.bright_magenta, (255, 0, 255));
    (Color.bright_cyan, (0, 255, 255));
    (Color.bright_white, (255, 255, 255));
  |]

let reference_lab (r, g, b) =
  let linear c =
    let c = float_of_int c /. 255. in
    if c <= 0.04045 then c /. 12.92
    else Float.pow ((c +. 0.055) /. 1.055) 2.4
  in
  let r = linear r and g = linear g and b = linear b in
  let x = ((0.4124 *. r) +. (0.3576 *. g) +. (0.1805 *. b)) /. 0.95047
  and y = (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)
  and z = ((0.0193 *. r) +. (0.1192 *. g) +. (0.9505 *. b)) /. 1.08883 in
  let f t =
    if t > 0.008856 then Float.cbrt t
    else (7.787 *. t) +. (16. /. 116.)
  in
  let fx = f x and fy = f y and fz = f z in
  ((116. *. fy) -. 16., 500. *. (fx -. fy), 200. *. (fy -. fz))

let reference_nearest rgb =
  let distance rgb rgb' =
    let l, a, b = reference_lab rgb in
    let l', a', b' = reference_lab rgb' in
    ((l -. l') *. (l -. l'))
    +. ((a -. a') *. (a -. a'))
    +. ((b -. b') *. (b -. b'))
  in
  let best = ref named.(0) in
  Array.iter
    (fun ((_, rgb') as entry) ->
      if distance rgb rgb' < distance rgb (snd !best) then best := entry)
    named;
  fst !best

let test_downsample_reference () =
  let check c =
    let rgb = Color.to_rgb c in
    let expected = reference_nearest rgb in
    let actual = Color.downsample `Ansi_16 c in
    if not (Color.equal expected actual) then
      Alcotest.check color "reference palette match" expected actual
  in
  for r = 0 to 15 do
    for g = 0 to 15 do
      for b = 0 to 15 do
        check (Color.rgb (17 * r) (17 * g) (17 * b))
      done
    done
  done;
  for i = 0 to 255 do
    check (Color.rgb i i i);
    check (Color.palette i)
  done;
  Array.iter
    (fun (c, rgb) ->
      Alcotest.(check (triple int int int)) "named RGB" rgb (Color.to_rgb c))
    named

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

let test_blend_scale_reference () =
  let cube = [| 0; 95; 135; 175; 215; 255 |] in
  let palette n =
    if n < 16 then snd named.(n)
    else if n < 232 then
      let n = n - 16 in
      (cube.(n / 36), cube.(n / 6 mod 6), cube.(n mod 6))
    else
      let v = 8 + (10 * (n - 232)) in
      (v, v, v)
  in
  let colors =
    Array.concat
      [
        named;
        Array.init 256 (fun i -> (Color.palette i, palette i));
        [|
          (Color.rgb 20 40 60, (20, 40, 60));
          (Color.rgb 100 160 240, (100, 160, 240));
        |];
      ]
  in
  let fractions =
    [
      Float.neg_infinity; -1.; -0.; 0.; 0.001; 0.25; 0.499; 0.5; 0.501;
      0.99; 1.; 2.; Float.infinity; Float.nan;
    ]
  in
  let check label expected c =
    let actual = Color.to_rgb c in
    if actual <> expected then
      Alcotest.(check (triple int int int)) label expected actual
  in
  Array.iter
    (fun (c0, ((r, g, b) as rgb)) ->
      check "RGB conversion" rgb c0;
      List.iter
        (fun f ->
          let scale v =
            max 0 (min 255 (int_of_float (Float.round (float_of_int v *. f))))
          in
          check "scale reference" (scale r, scale g, scale b)
            (Color.scale f c0))
        fractions;
      Array.iter
        (fun (c1, (r', g', b')) ->
          List.iter
            (fun t ->
              let clamped = Float.max 0. (Float.min 1. t) in
              let mix a a' =
                a
                + int_of_float
                    (Float.round (clamped *. float_of_int (a' - a)))
              in
              check "blend reference" (mix r r', mix g g', mix b b')
                (Color.blend c0 c1 t))
            fractions)
        colors)
    colors

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
      Alcotest.test_case "downsample reference" `Quick
        test_downsample_reference;
      Alcotest.test_case "rgb" `Quick test_rgb;
      Alcotest.test_case "blend and scale reference" `Quick
        test_blend_scale_reference;
      Alcotest.test_case "codes at depth" `Quick test_codes_at_depth;
    ] )
