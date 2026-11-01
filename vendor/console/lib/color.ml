(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Sgr.color =
  | Ansi of Sgr.ansi
  | Rgb of int * int * int
  | Palette of int

let rgb r g b =
  if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
    invalid_arg "Console.Color.rgb: component outside 0..255";
  Rgb (r, g, b)

let of_hex s =
  let s =
    if String.starts_with ~prefix:"#" s then String.sub s 1 (String.length s - 1)
    else s
  in
  if not (String.for_all Char.Ascii.is_hex_digit s) then
    Error (`Msg "invalid hex character")
  else
    let d i = Char.Ascii.hex_digit_to_int s.[i] in
    match String.length s with
    | 6 -> Ok (Rgb ((d 0 * 16) + d 1, (d 2 * 16) + d 3, (d 4 * 16) + d 5))
    | 3 -> Ok (Rgb (d 0 * 17, d 1 * 17, d 2 * 17))
    | _ -> Error (`Msg "expected 3 or 6 hex digits")

let ansi_256 index =
  if index < 0 || index > 255 then
    invalid_arg "Console.Color.ansi_256: index outside 0..255";
  Palette index

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

type depth = [ `Ansi_16 | `Ansi_256 | `True_color ]

let depth_slot = Slot.v ()
let depth ppf = Option.value (Slot.find depth_slot ppf) ~default:`Ansi_16
let set_depth ppf d = Slot.set depth_slot ppf d

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
  | Ansi a ->
      Option.get
        (Array.find_map
           (fun (a', rgb) -> if a' = a then Some rgb else None)
           named)
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

(* After tmux's colour_find_rgb: the nearest cube entry or grey, whichever is
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
module Float_u = Stdlib_upstream_compatible.Float_u

let[@zero_alloc] lab (r, g, b) =
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
  #( Float_u.of_float ((116. *. fy) -. 16.),
     Float_u.of_float (500. *. (fx -. fy)),
     Float_u.of_float (200. *. (fy -. fz)) )

type lab = { lightness : float#; green_red : float#; blue_yellow : float# }

let named_lab =
  Array.map
    (fun (_, rgb) ->
      let #(lightness, green_red, blue_yellow) = lab rgb in
      { lightness; green_red; blue_yellow })
    named

(* CIE76 colour difference, squared. *)
let[@zero_alloc] delta_e #(l, a, b) other =
  let open Float_u in
  let dl = sub l other.lightness in
  let da = sub a other.green_red in
  let db = sub b other.blue_yellow in
  add (add (mul dl dl) (mul da da)) (mul db db)

let[@zero_alloc] nearest_named rgb =
  let lab = lab rgb in
  let mutable best = 0 in
  let mutable distance = delta_e lab named_lab.(0) in
  for i = 1 to Array.length named_lab - 1 do
    let candidate = delta_e lab named_lab.(i) in
    if Float_u.compare candidate distance < 0 then (
      best <- i;
      distance <- candidate)
  done;
  fst named.(best)

let downsample depth c =
  match (depth, c) with
  | `True_color, c
  | `Ansi_256, ((Ansi _ | Palette _) as c)
  | `Ansi_16, (Ansi _ as c) ->
      c
  | `Ansi_256, Rgb (r, g, b) -> Palette (xterm_256 (r, g, b))
  | `Ansi_16, c -> Ansi (nearest_named (to_rgb c))

let blend c0 c1 f =
  let f = if Float.is_nan f then 0. else Float.max 0. (Float.min 1. f) in
  let r, g, b = to_rgb c0 and r', g', b' = to_rgb c1 in
  let mix a a' = a + int_of_float (Float.round (f *. float_of_int (a' - a))) in
  Rgb (mix r r', mix g g', mix b b')

let scale f c =
  let r, g, b = to_rgb c in
  (* Clamped as a float: a large factor overflows [int_of_float]. *)
  let s v =
    let x = Float.round (float_of_int v *. f) in
    if Float.is_nan x then 0 else int_of_float (Float.max 0. (Float.min 255. x))
  in
  Rgb (s r, s g, s b)

let equal left right =
  match (left, right) with
  | Ansi l, Ansi r -> l = r
  | Rgb (r1, g1, b1), Rgb (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | Palette l, Palette r -> l = r
  | _ -> false

(* A colour is plain data, so the structural order is compatible with
   [equal]. *)
let compare (left : t) right = Stdlib.compare left right

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
  | Palette index -> Fmt.pf ppf "ansi_256(%d)" index

let sgr depth c = downsample depth c
