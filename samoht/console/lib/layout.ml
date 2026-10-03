(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let block_width col =
  Array.fold_left (fun acc l -> max acc (Width.string_width l)) 0 col

(* The line a column shows on row [r], accounting for vertical alignment: a
   block shorter than the tallest column floats at the top ([`Top], blank rows
   below) or the bottom ([`Bottom], blank rows above). Rows outside the block
   are blank. *)
let line_at ~align ~height col r =
  let len = Array.length col in
  let offset = match align with `Top -> 0 | `Bottom -> height - len in
  let idx = r - offset in
  if idx >= 0 && idx < len then col.(idx) else ""

(* The last column index whose cell is non-empty on this row, or [-1]. Padding
   and gutters past it would be trailing whitespace, so the row stops here. *)
let last_filled cells =
  let last = ref (-1) in
  Array.iteri (fun i c -> if String.length c > 0 then last := i) cells;
  !last

let hcat ?(gutter = 1) ?(align = `Top) blocks =
  if gutter < 0 then invalid_arg "Console.Layout.hcat: negative gutter";
  match blocks with
  | [] -> ""
  | _ ->
      let cols =
        Array.of_list
          (List.map
             (fun b -> Array.of_list (String.split_on_char '\n' b))
             blocks)
      in
      let widths = Array.map block_width cols in
      let height = Array.fold_left (fun a c -> max a (Array.length c)) 0 cols in
      let gutter = String.make gutter ' ' in
      let row r =
        let cells = Array.map (fun c -> line_at ~align ~height c r) cols in
        let last = last_filled cells in
        let buf = Buffer.create 80 in
        for i = 0 to last do
          if i = last then Buffer.add_string buf cells.(i)
          else (
            Buffer.add_string buf (Width.pad_right widths.(i) cells.(i));
            Buffer.add_string buf gutter)
        done;
        Buffer.contents buf
      in
      String.concat "\n" (List.init height row)

let hcat_anim ?gutter ?align blocks =
  Anim.map (hcat ?gutter ?align) (Anim.all blocks)

let vcat ?(gutter = 0) blocks =
  if gutter < 0 then invalid_arg "Console.Layout.vcat: negative gutter";
  String.concat (String.make (gutter + 1) '\n') blocks

let vcat_anim ?gutter blocks = Anim.map (vcat ?gutter) (Anim.all blocks)
