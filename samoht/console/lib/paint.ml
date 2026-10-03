(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  ppf : Format.formatter;
  styling : bool;
  mutable row : int;
  mutable column : int;
  mutable open_ : (Style.t * string) option;
      (** The resolved style and SGR sequence of the run being written. *)
}

let v ppf =
  {
    ppf;
    styling = Fmt.style_renderer ppf = `Ansi_tty;
    row = 0;
    column = 0;
    open_ = None;
  }

let close p =
  match p.open_ with
  | None -> ()
  | Some _ ->
      Fmt.string p.ppf Style.reset;
      p.open_ <- None

(* Two styles that differ only in their colours: the second's sequence sets
   every colour the first set, so it follows without a reset. *)
let recolours a b =
  let flat =
    Style.map_color ~fg:(Fun.const Color.black) ~bg:(Fun.const Color.black)
  in
  Style.equal (flat a) (flat b)

let ink_cell p style glyph width =
  let style = Style.at ~row:p.row ~column:p.column style in
  let sgr = Style.to_ansi style in
  (match p.open_ with
  | _ when sgr = "" || (glyph = " " && not (Style.shows_on_space style)) ->
      close p
  | Some (_, open_sgr) when open_sgr = sgr -> ()
  | Some (open_style, _) when recolours open_style style ->
      Fmt.string p.ppf sgr;
      p.open_ <- Some (style, sgr)
  | _ ->
      close p;
      Fmt.string p.ppf sgr;
      p.open_ <- Some (style, sgr));
  Fmt.string p.ppf glyph;
  p.column <- p.column + width

let ink p style glyphs =
  if (not p.styling) || Style.is_none style then begin
    close p;
    Fmt.string p.ppf glyphs;
    p.column <- p.column + Width.string_width glyphs
  end
  else
    let n = String.length glyphs in
    let rec go i =
      if i < n then begin
        let d = String.get_utf_8_uchar glyphs i in
        let len = Uchar.utf_decode_length d in
        let width =
          if Uchar.utf_decode_is_valid d then
            Width.default_char_width (Uchar.utf_decode_uchar d)
          else 1
        in
        ink_cell p style (String.sub glyphs i len) width;
        go (i + len)
      end
    in
    go 0

let text p s =
  close p;
  Fmt.string p.ppf s;
  p.column <- p.column + Width.string_width s

let newline p =
  close p;
  Render.newline p.ppf;
  p.row <- p.row + 1;
  p.column <- 0
