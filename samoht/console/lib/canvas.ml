(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type cell = { glyph : string; style : Style.t }

let cell ?(style = Style.none) glyph =
  if
    Width.string_width glyph <> 1
    || Render.sanitize ~keep_newlines:false glyph <> glyph
    || String.contains glyph '\027'
  then invalid_arg "Console.Canvas.cell: not one terminal cell";
  { glyph; style }

let replacement = "\xef\xbf\xbd"

let cells ?(style = Style.none) s =
  let s = Render.sanitize ~keep_newlines:false s in
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let d = String.get_utf_8_uchar s i in
      let len = Uchar.utf_decode_length d in
      let g = String.sub s i len in
      let w = Width.default_char_width (Uchar.utf_decode_uchar d) in
      match (w, acc) with
      | 0, c :: rest -> go (i + len) ({ c with glyph = c.glyph ^ g } :: rest)
      | 0, [] -> go (i + len) acc
      | 1, _ -> go (i + len) ({ glyph = g; style } :: acc)
      | _ -> go (i + len) ({ glyph = replacement; style } :: acc)
  in
  go 0 []

let glyph c = c.glyph
let style c = c.style
let with_style style c = { c with style }

type t = { width : int; cells : cell array array }

let v ~width ~height f =
  if width < 0 || height < 0 then
    invalid_arg "Console.Canvas.v: negative dimension";
  {
    width;
    cells =
      Array.init height (fun row ->
          Array.init width (fun column -> f ~row ~column));
  }

let of_rows rows =
  let width = match rows with [] -> 0 | row :: _ -> List.length row in
  if List.exists (fun row -> List.length row <> width) rows then
    invalid_arg "Console.Canvas.of_rows: rows of different lengths";
  { width; cells = Array.of_list (List.map Array.of_list rows) }

let width c = c.width
let height c = Array.length c.cells

let get c ~row ~column =
  if row < 0 || row >= height c || column < 0 || column >= c.width then
    invalid_arg "Console.Canvas.get: position outside the canvas";
  c.cells.(row).(column)

let map f c =
  {
    c with
    cells =
      Array.mapi
        (fun row cells -> Array.mapi (fun column -> f ~row ~column) cells)
        c.cells;
  }

(* A row as runs of cells that draw the same sequence. *)
let row_span row cells =
  if Array.length cells = 0 then Span.empty
  else begin
    let text = Buffer.create 80 in
    let mutable style = Style.at ~row ~column:0 cells.(0).style in
    let mutable sgr = Style.to_ansi style in
    let mutable runs = [] in
    Buffer.add_string text cells.(0).glyph;
    for column = 1 to Array.length cells - 1 do
      let cell = cells.(column) in
      let next_style = Style.at ~row ~column cell.style in
      let next_sgr = Style.to_ansi next_style in
      if not (String.equal sgr next_sgr) then begin
        runs <- Span.styled style (Buffer.contents text) :: runs;
        Buffer.clear text;
        style <- next_style;
        sgr <- next_sgr
      end;
      Buffer.add_string text cell.glyph
    done;
    Span.concat
      (List.rev (Span.styled style (Buffer.contents text) :: runs))
  end

let rows c = Array.to_list (Array.mapi row_span c.cells)

let pp ppf c =
  let margin = Format.pp_get_margin ppf () in
  List.iteri
    (fun i row ->
      if i > 0 then Render.newline ppf;
      Span.pp ppf (Span.truncate margin row))
    (rows c)
