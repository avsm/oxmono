(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type ansi =
  [ `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `Bright_black
  | `Bright_red
  | `Bright_green
  | `Bright_yellow
  | `Bright_blue
  | `Bright_magenta
  | `Bright_cyan
  | `Bright_white ]

type t = Ansi of ansi | Rgb of int * int * int | Palette of int

let ansi color = Ansi color

let rgb r g b =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
    invalid_arg "Console.Color.rgb: component outside 0..255";
  Rgb (r, g, b)

let hex str =
  let str =
    if String.length str > 0 && str.[0] = '#' then
      String.sub str 1 (String.length str - 1)
    else str
  in
  let len = String.length str in
  let hex_digit ch =
    match ch with
    | '0' .. '9' -> Char.code ch - Char.code '0'
    | 'a' .. 'f' -> Char.code ch - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code ch - Char.code 'A' + 10
    | _ -> invalid_arg "Console.Color.hex: invalid hex character"
  in
  let parse_hex2 i =
    let hi = str.[i] and lo = str.[i + 1] in
    (hex_digit hi * 16) + hex_digit lo
  in
  let parse_hex1 i =
    let d = hex_digit str.[i] in
    (d * 16) + d
  in
  match len with
  | 6 -> Rgb (parse_hex2 0, parse_hex2 2, parse_hex2 4)
  | 3 -> Rgb (parse_hex1 0, parse_hex1 1, parse_hex1 2)
  | _ -> invalid_arg "Console.Color.hex: expected 3 or 6 hex digits"

let palette index =
  if index < 0 || index > 255 then
    invalid_arg "Console.Color.palette: index outside 0..255";
  Palette index

(* Predefined colors *)
let black = Ansi `Black
let red = Ansi `Red
let green = Ansi `Green
let yellow = Ansi `Yellow
let blue = Ansi `Blue
let magenta = Ansi `Magenta
let cyan = Ansi `Cyan
let white = Ansi `White
let bright_black = Ansi `Bright_black
let bright_red = Ansi `Bright_red
let bright_green = Ansi `Bright_green
let bright_yellow = Ansi `Bright_yellow
let bright_blue = Ansi `Bright_blue
let bright_magenta = Ansi `Bright_magenta
let bright_cyan = Ansi `Bright_cyan
let bright_white = Ansi `Bright_white

let ansi_position = function
  | `Black | `Bright_black -> 0
  | `Red | `Bright_red -> 1
  | `Green | `Bright_green -> 2
  | `Yellow | `Bright_yellow -> 3
  | `Blue | `Bright_blue -> 4
  | `Magenta | `Bright_magenta -> 5
  | `Cyan | `Bright_cyan -> 6
  | `White | `Bright_white -> 7

let is_bright = function
  | `Bright_black | `Bright_red | `Bright_green | `Bright_yellow | `Bright_blue
  | `Bright_magenta | `Bright_cyan | `Bright_white ->
      true
  | _ -> false

type depth = [ `Ansi_16 | `Ansi_256 | `True_color ]

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

let ends_with ~suffix s =
  let n = String.length suffix and m = String.length s in
  m >= n && String.sub s (m - n) n = suffix

let depth_of_env getenv =
  match (getenv "COLORTERM", getenv "TERM") with
  | Some ("truecolor" | "24bit"), _ -> `True_color
  | _, Some term when ends_with ~suffix:"-direct" term -> `True_color
  | _, Some term when contains ~sub:"256color" term -> `Ansi_256
  | _ -> `Ansi_16

let current_depth : depth Atomic.t = Atomic.make `True_color
let depth () = Atomic.get current_depth
let set_depth d = Atomic.set current_depth d

(* The xterm defaults of the 16 named colours, in palette order. *)
let named =
  [|
    (`Black, (0, 0, 0));
    (`Red, (205, 0, 0));
    (`Green, (0, 205, 0));
    (`Yellow, (205, 205, 0));
    (`Blue, (0, 0, 238));
    (`Magenta, (205, 0, 205));
    (`Cyan, (0, 205, 205));
    (`White, (229, 229, 229));
    (`Bright_black, (127, 127, 127));
    (`Bright_red, (255, 0, 0));
    (`Bright_green, (0, 255, 0));
    (`Bright_yellow, (255, 255, 0));
    (`Bright_blue, (92, 92, 255));
    (`Bright_magenta, (255, 0, 255));
    (`Bright_cyan, (0, 255, 255));
    (`Bright_white, (255, 255, 255));
  |]

let cube = [| 0; 95; 135; 175; 215; 255 |]

let to_rgb = function
  | Rgb (r, g, b) -> (r, g, b)
  | Ansi a -> snd (List.find (fun (a', _) -> a' = a) (Array.to_list named))
  | Palette n when n < 16 -> snd named.(n)
  | Palette n when n < 232 ->
      let n = n - 16 in
      (cube.(n / 36), cube.(n / 6 mod 6), cube.(n mod 6))
  | Palette n ->
      let v = 8 + (10 * (n - 232)) in
      (v, v, v)

let nearest_level v =
  let best = ref 0 in
  Array.iteri
    (fun i level -> if abs (level - v) < abs (cube.(!best) - v) then best := i)
    cube;
  !best

let distance (r, g, b) (r', g', b') =
  ((r - r') * (r - r')) + ((g - g') * (g - g')) + ((b - b') * (b - b'))

(* tmux's colour_find_rgb: the nearest cube entry or grey, whichever is
   nearer. *)
let xterm_256 (r, g, b) =
  let ri = nearest_level r and gi = nearest_level g and bi = nearest_level b in
  let cube_rgb = (cube.(ri), cube.(gi), cube.(bi)) in
  let grey_index = max 0 (min 23 ((((r + g + b) / 3) - 8 + 5) / 10)) in
  let grey = 8 + (10 * grey_index) in
  if distance (r, g, b) (grey, grey, grey) < distance (r, g, b) cube_rgb then
    232 + grey_index
  else 16 + (36 * ri) + (6 * gi) + bi

(* CIE 1976 L*a*b* of an sRGB colour, under the D65 white point. *)
let lab (r, g, b) =
  let linear c =
    let c = float_of_int c /. 255. in
    if c <= 0.04045 then c /. 12.92 else Float.pow ((c +. 0.055) /. 1.055) 2.4
  in
  let r = linear r and g = linear g and b = linear b in
  let x = ((0.4124 *. r) +. (0.3576 *. g) +. (0.1805 *. b)) /. 0.95047
  and y = (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)
  and z = ((0.0193 *. r) +. (0.1192 *. g) +. (0.9505 *. b)) /. 1.08883 in
  let f t =
    if t > 0.008856 then Float.cbrt t else (7.787 *. t) +. (16. /. 116.)
  in
  let fx = f x and fy = f y and fz = f z in
  ((116. *. fy) -. 16., 500. *. (fx -. fy), 200. *. (fy -. fz))

(* CIE76 colour difference, squared. *)
let delta_e c c' =
  let l, a, b = lab c and l', a', b' = lab c' in
  ((l -. l') *. (l -. l'))
  +. ((a -. a') *. (a -. a'))
  +. ((b -. b') *. (b -. b'))

let nearest_named rgb =
  let best = ref named.(0) in
  Array.iter
    (fun ((_, rgb') as entry) ->
      if delta_e rgb rgb' < delta_e rgb (snd !best) then best := entry)
    named;
  fst !best

let downsample depth c =
  match (depth, c) with
  | `True_color, c
  | `Ansi_256, ((Ansi _ | Palette _) as c)
  | `Ansi_16, (Ansi _ as c) ->
      c
  | `Ansi_256, Rgb (r, g, b) -> Palette (xterm_256 (r, g, b))
  | `Ansi_16, c -> Ansi (nearest_named (to_rgb c))

let clamp v = max 0 (min 255 v)

let blend c0 c1 t =
  let t = Float.max 0. (Float.min 1. t) in
  let r, g, b = to_rgb c0 and r', g', b' = to_rgb c1 in
  let mix a a' = a + int_of_float (Float.round (t *. float_of_int (a' - a))) in
  Rgb (mix r r', mix g g', mix b b')

let scale f c =
  let r, g, b = to_rgb c in
  let s v = clamp (int_of_float (Float.round (float_of_int v *. f))) in
  Rgb (s r, s g, s b)

let to_code ~normal ~bright ~extended c =
  match downsample (depth ()) c with
  | Ansi color ->
      string_of_int
        ((if is_bright color then bright else normal) + ansi_position color)
  | Rgb (r, g, b) -> Fmt.str "%d;2;%d;%d;%d" extended r g b
  | Palette index -> Fmt.str "%d;5;%d" extended index

let to_fg_code = to_code ~normal:30 ~bright:90 ~extended:38
let to_bg_code = to_code ~normal:40 ~bright:100 ~extended:48

let equal left right =
  match (left, right) with
  | Ansi l, Ansi r -> l = r
  | Rgb (r1, g1, b1), Rgb (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | Palette l, Palette r -> l = r
  | _ -> false

let pp ppf = function
  | Ansi `Black -> Fmt.pf ppf "black"
  | Ansi `Red -> Fmt.pf ppf "red"
  | Ansi `Green -> Fmt.pf ppf "green"
  | Ansi `Yellow -> Fmt.pf ppf "yellow"
  | Ansi `Blue -> Fmt.pf ppf "blue"
  | Ansi `Magenta -> Fmt.pf ppf "magenta"
  | Ansi `Cyan -> Fmt.pf ppf "cyan"
  | Ansi `White -> Fmt.pf ppf "white"
  | Ansi `Bright_black -> Fmt.pf ppf "bright_black"
  | Ansi `Bright_red -> Fmt.pf ppf "bright_red"
  | Ansi `Bright_green -> Fmt.pf ppf "bright_green"
  | Ansi `Bright_yellow -> Fmt.pf ppf "bright_yellow"
  | Ansi `Bright_blue -> Fmt.pf ppf "bright_blue"
  | Ansi `Bright_magenta -> Fmt.pf ppf "bright_magenta"
  | Ansi `Bright_cyan -> Fmt.pf ppf "bright_cyan"
  | Ansi `Bright_white -> Fmt.pf ppf "bright_white"
  | Rgb (r, g, b) -> Fmt.pf ppf "#%02x%02x%02x" r g b
  | Palette index -> Fmt.pf ppf "palette(%d)" index
