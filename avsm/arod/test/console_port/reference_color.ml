(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Pristine excerpts from ocaml-console de6681a6956fd57f759d761b6eecaf515e41159c. *)
type ansi = [ `Black | `Red | `Green | `Yellow | `Blue | `Magenta | `Cyan | `White | `Bright_black | `Bright_red | `Bright_green | `Bright_yellow | `Bright_blue | `Bright_magenta | `Bright_cyan | `Bright_white ]

let named : (ansi * (int * int * int)) array =
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

