(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ASCII alphabet the rain flickers through. *)
let glyphs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ:.=*+-<>|/"

(* A deterministic glyph for border cell [i] at animation [frame]: the same (i,
   frame) always picks the same glyph, so a given elapsed renders a stable frame
   (testable, and flicker-free under re-paint). *)
let glyph i frame =
  (* 507224497 is the golden-ratio hash constant 2654435761 (0x9e3779b1) reduced
     mod 2^30: under the 30-bit mask the multiplier's high bits never affect the
     result, so this is bit-identical to multiplying by 2654435761, and it stays
     a small immediate that fits a 31-bit int (js_of_ocaml, wasm_of_ocaml). *)
  let h = ((i * 507224497) + (frame * 40503)) land 0x3fffffff in
  String.make 1 glyphs.[h mod String.length glyphs]

(* A border cell at perimeter index [pidx]: a green ASCII glyph whose brightness
   falls off with the cyclic distance to the travelling [head] -- bold at the
   head, plain just behind it, dim elsewhere. *)
let cell ~head ~perim ~frame pidx =
  let d0 = abs (pidx - head) in
  let d = min d0 (perim - d0) in
  let code =
    if d = 0 then Ansi.bold ^ Ansi.color_code Color.green
    else if d <= 3 then Ansi.color_code Color.green
    else Ansi.dim ^ Ansi.color_code Color.green
  in
  code ^ glyph pidx frame ^ Ansi.reset_code

(* Perimeter cells are numbered clockwise from the top-left -- top edge, right
   edge, bottom edge (right to left), left edge (bottom to top) -- so the head
   sweeps round the box. *)
let frame ?(char_width = Width.default_char_width) lines ~elapsed =
  let elapsed = Float.max 0. elapsed in
  let iw =
    List.fold_left (fun a l -> max a (Width.string_width ~char_width l)) 0 lines
  in
  let width = iw + 2 in
  let h = List.length lines in
  let perim = (2 * width) + (2 * h) in
  let fr = int_of_float (elapsed *. 10.) in
  let head = if perim = 0 then 0 else int_of_float (elapsed *. 12.) mod perim in
  let cell = cell ~head ~perim ~frame:fr in
  let buf = Buffer.create ((width + 16) * (h + 2)) in
  for c = 0 to width - 1 do
    Buffer.add_string buf (cell c)
  done;
  Buffer.add_char buf '\n';
  List.iteri
    (fun r line ->
      let left_pidx = (2 * width) + h + (h - 1 - r) in
      let right_pidx = width + r in
      Buffer.add_string buf (cell left_pidx);
      Buffer.add_string buf (Width.pad_right ~char_width iw line);
      Buffer.add_string buf (cell right_pidx);
      Buffer.add_char buf '\n')
    lines;
  for c = 0 to width - 1 do
    Buffer.add_string buf (cell (width + h + (width - 1 - c)))
  done;
  Buffer.contents buf
