(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type look = Look.t

(* A formatter that is not UTF-8 shows ASCII alone: glyphs that are not are
   replaced, all of a part at once so that its pieces still join; a piece a
   border leaves out stays out. *)
let shows look glyphs = look.Look.utf_8 || List.for_all Width.is_ascii glyphs

let muted look =
  match Theme.muted look.Look.theme with
  | Some c -> Style.fg c
  | None -> Style.faint

let accent look = Style.fg (Theme.accent look.Look.theme)

(* A border or a guide in no style of its own is drawn in the muted colour. *)
let border look border =
  let border = Option.value border ~default:(Theme.border look.Look.theme) in
  let border =
    if Style.equal (Border.style border) Style.none then
      Border.with_style (muted look) border
    else border
  in
  let c = Border.chars border in
  if
    shows look
      [
        c.top_left;
        c.top;
        c.top_right;
        c.left;
        c.right;
        c.bottom_left;
        c.bottom;
        c.bottom_right;
        c.vertical;
        c.horizontal;
        c.cross;
        c.top_cross;
        c.bottom_cross;
        c.left_cross;
        c.right_cross;
      ]
  then border
  else
    let a = Border.chars Border.ascii in
    let keep g a = if g = "" then "" else a in
    Border.v ~style:(Border.style border)
      {
        top_left = keep c.top_left a.top_left;
        top = keep c.top a.top;
        top_right = keep c.top_right a.top_right;
        left = keep c.left a.left;
        right = keep c.right a.right;
        bottom_left = keep c.bottom_left a.bottom_left;
        bottom = keep c.bottom a.bottom;
        bottom_right = keep c.bottom_right a.bottom_right;
        vertical = keep c.vertical a.vertical;
        horizontal = keep c.horizontal a.horizontal;
        cross = keep c.cross a.cross;
        top_cross = keep c.top_cross a.top_cross;
        bottom_cross = keep c.bottom_cross a.bottom_cross;
        left_cross = keep c.left_cross a.left_cross;
        right_cross = keep c.right_cross a.right_cross;
      }

let guide look guide =
  let guide = Option.value guide ~default:(Theme.guide look.Look.theme) in
  let guide =
    if Style.equal (Guide.style guide) Style.none then
      Guide.with_style (muted look) guide
    else guide
  in
  let g = Guide.[ branch guide; last guide; pipe guide; space guide ] in
  if shows look g then guide
  else Guide.with_style (Guide.style guide) Guide.ascii

let glyph look g ~ascii = if shows look [ g ] then g else ascii
let ascii_theme = Look.ascii_theme
let line look g = if ascii_theme look then "-" else glyph look g ~ascii:"-"

let spinner look ~time =
  glyph look
    (Frames.at (Theme.spinner look.Look.theme) ~time)
    ~ascii:(Frames.at Frames.ascii ~time)

let mark look m =
  glyph look (Theme.mark look.Look.theme m) ~ascii:(Theme.mark Theme.ascii m)

let bar look = if look.Look.utf_8 then Theme.bar look.Look.theme else `Ascii
let ellipsis = Look.ellipsis
let truncate look w s = View_core.truncate ~ellipsis:(ellipsis look) w s

let one_line span =
  View_core.concat ~sep:(View_core.text " ") (View_core.lines span)

let cells = Width.string_width
let widest glyphs = List.fold_left (fun n g -> max n (cells g)) 0 glyphs

let fill n glyph =
  if n <= 0 then ""
  else
    let gw = cells glyph in
    if gw = 0 then String.make n ' '
    else
      let rest = n mod gw in
      let buf = Buffer.create ((n / gw * String.length glyph) + rest) in
      for _ = 1 to n / gw do
        Buffer.add_string buf glyph
      done;
      if rest > 0 then begin
        let used = ref 0 and full = ref false in
        Width.iter_clusters
          (fun ~offset ~len ~cells:w ->
            if (not !full) && !used + w <= rest then begin
              Buffer.add_substring buf glyph offset len;
              used := !used + w
            end
            else full := true)
          glyph;
        for _ = 1 to rest - !used do
          Buffer.add_char buf ' '
        done
      end;
      Buffer.contents buf

let rails g lasts =
  String.concat ""
    (List.rev_map
       (fun last -> if last then Guide.space g else Guide.pipe g)
       lasts)

let branch g = function
  | [] -> ""
  | own :: up -> rails g up ^ if own then Guide.last g else Guide.branch g

let pad_glyph n glyph = glyph ^ String.make (max 0 (n - cells glyph)) ' '
let pad n = View_core.text (String.make (max 0 n) ' ')

let framed_kind parts look =
  let k = View_core.kinds parts look in
  if look.Look.live && Option.is_some (Theme.motion look.theme) then
    View_core.join k View_core.timed
  else k
