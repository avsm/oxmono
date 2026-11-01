(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type overflow = [ `Wrap | `Truncate ]

type column = {
  header : View_core.t;
  align : View_core.align;
  width : int option;
  min_width : int option;
  max_width : int option;
  overflow : overflow;
  shrinkable : bool;
  style : Style.t;
}

(* The cells of the frame's three columns: its left edge, the rule between
   two cells and its right edge, each as wide as its widest glyph. *)
type frame = { left : int; mid : int; right : int }

(* A cell is kept as its lines at its natural size, split once, the width
   below which it should not be laid out, and as the view it is, laid out
   again at the width of its column when it is not flat. *)
type cell = { view : View_core.t; lines : View_core.t list; word : int }

type drawn = {
  ctx : Ctx.t;
  look : Widget.look;
  border : Border.t;
  frame : frame;
  gap : int;
  header_style : Style.t;
  rules : bool;
  columns : column array;
  headers : cell array;
  rows : cell array list;
}

let column ?(align = `Left) ?width ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  let negative = function Some w -> w < 0 | None -> false in
  if negative width then invalid_arg "Console.Table.column: negative width";
  if negative min_width then
    invalid_arg "Console.Table.column: negative min_width";
  if negative max_width then
    invalid_arg "Console.Table.column: negative max_width";
  (match (min_width, max_width) with
  | Some min, Some max when min > max ->
      invalid_arg "Console.Table.column: min_width exceeds max_width"
  | _ -> ());
  { header; align; width; min_width; max_width; overflow; shrinkable; style }

let frame border =
  let c = Border.chars border in
  {
    left = Widget.widest [ c.top_left; c.left; c.left_cross; c.bottom_left ];
    mid = Widget.widest [ c.top_cross; c.vertical; c.cross; c.bottom_cross ];
    right =
      Widget.widest [ c.top_right; c.right; c.right_cross; c.bottom_right ];
  }

let chars t = Border.chars t.border

(* A border whose three columns have no glyph has no edge: no padding
   outside the first and the last cell, the last cell not padded out to its
   column, and a line ends at its last cell that is not empty. A column with
   a glyph anywhere is drawn on every line, padded where its glyph is empty. *)
let edged t = t.frame.left > 0 || t.frame.mid > 0 || t.frame.right > 0
let separator_width t = if edged t then t.gap + t.frame.mid else t.gap

let overhead t count =
  if count = 0 then 0
  else if not (edged t) then (count - 1) * t.gap
  else
    (count * t.gap) + t.frame.left + t.frame.right + ((count - 1) * t.frame.mid)

(* Widths add up without overflow: a sum past [max_int] is [max_int]. *)
let ( +| ) a b = if a > max_int - b then max_int else a + b

let rendered_width t widths =
  Array.fold_left ( +| ) (overhead t (Array.length widths)) widths

let lines_width c =
  List.fold_left (fun n l -> max n (View_core.width l)) 0 c.lines

(* The widest word of a cell, or the width a cell laid out in its room says
   it needs: what a column keeps while another column can still give room,
   so that an identifier stays whole. *)
let word_width c = c.word

(* A short row's last cell spans the columns it leaves and is measured
   against none of them. *)
let measured num_cols row =
  let n = Array.length row in
  if n >= num_cols then n else n - 1

let widen measure t widths =
  let num_cols = Array.length widths in
  Array.iteri (fun i h -> widths.(i) <- max widths.(i) (measure h)) t.headers;
  List.iter
    (fun row ->
      for i = 0 to measured num_cols row - 1 do
        widths.(i) <- max widths.(i) (measure row.(i))
      done)
    t.rows

(* Columns narrow, the widest first, until the table fits [max_width]: the
   shrinkable ones down to their widest word, then all of them, then down to
   their minimum, then to nothing. *)
let shrink ~max_width t words widths =
  let cols = t.columns in
  let min_of i = Option.value cols.(i).min_width ~default:1 in
  let word_min i =
    let token =
      match cols.(i).overflow with `Truncate -> 0 | `Wrap -> words.(i)
    in
    min widths.(i) (max (min_of i) token)
  in
  let pass can floor =
    let stuck = ref false in
    while rendered_width t widths > max_width && not !stuck do
      let widest = ref (-1) in
      Array.iteri
        (fun i w ->
          if can i && w > floor i && (!widest < 0 || w > widths.(!widest)) then
            widest := i)
        widths;
      if !widest < 0 then stuck := true
      else
        let i = !widest in
        let excess = rendered_width t widths - max_width in
        widths.(i) <- widths.(i) - min excess (widths.(i) - floor i)
    done
  in
  pass (fun i -> cols.(i).shrinkable) word_min;
  pass (fun _ -> true) word_min;
  pass (fun i -> cols.(i).shrinkable) min_of;
  pass (fun _ -> true) min_of;
  pass (fun _ -> true) (fun _ -> 0)

let widths ?max_width t =
  let n = Array.length t.columns in
  let widths = Array.make n 0 and words = Array.make n 0 in
  widen lines_width t widths;
  widen word_width t words;
  Array.iteri
    (fun i col ->
      Option.iter (fun w -> widths.(i) <- w) col.width;
      Option.iter (fun m -> widths.(i) <- max widths.(i) m) col.min_width;
      Option.iter (fun m -> widths.(i) <- min widths.(i) m) col.max_width;
      (* No column is wider than the whole table may be. *)
      Option.iter (fun m -> widths.(i) <- min widths.(i) (max 0 m)) max_width)
    t.columns;
  Option.iter (fun max_width -> shrink ~max_width t words widths) max_width;
  widths

(* A rule across a table without edges runs under its columns and the gaps
   between them. *)
let rule t widths ~left ~mid ~right ~line =
  if line = "" then []
  else if not (edged t) then
    [
      View_core.ink (Border.style t.border)
        (Widget.fill (rendered_width t widths) line);
    ]
  else
    let style = Border.style t.border in
    let last = Array.length widths - 1 in
    let parts =
      Array.to_list
        (Array.mapi
           (fun i w ->
             Widget.fill (w + t.gap) line
             ^ if i < last then Widget.pad_glyph t.frame.mid mid else "")
           widths)
    in
    let left = Widget.pad_glyph t.frame.left left
    and right = Widget.pad_glyph t.frame.right right in
    [ View_core.ink style (String.concat "" ((left :: parts) @ [ right ])) ]

let pad = Widget.pad

let align_span ~pad_right align target span =
  let padding = target - View_core.width span in
  if padding <= 0 then span
  else
    let left, right =
      match align with
      | `Left -> (0, padding)
      | `Right -> (padding, 0)
      | `Center -> (padding / 2, padding - (padding / 2))
    in
    View_core.(pad left ++ span ++ if pad_right then pad right else empty)

(* The physical lines of a cell in [width] cells; [None] is a cell nothing
   bounds, the last of a line without edges and no width. *)
let fit t overflow width cell =
  match width with
  | None -> cell.lines
  | Some width when width <= 0 -> [ View_core.empty ]
  | Some width -> (
      let lines =
        if View_core.is_flat cell.view then cell.lines
        else
          View_core.lines
            (View_core.resolve (Ctx.with_width (Some width) t.ctx) cell.view)
      in
      match overflow with
      | `Wrap -> List.concat_map (View_core.wrap width) lines
      | `Truncate -> List.map (Widget.truncate t.look width) lines)

(* The width each cell of [row] fills. A short row's last cell fills the
   columns it spans and the separators between them, and in a table without
   edges runs to the end of the line: to [max_width], or unbounded. *)
let slot_width ?max_width t widths row i =
  let num_cols = Array.length widths and n = Array.length row in
  if i < n - 1 || n = num_cols then Some widths.(i)
  else if edged t then begin
    let spanned = ref 0 in
    for j = i to num_cols - 1 do
      spanned := !spanned + widths.(j)
    done;
    Some (!spanned + ((num_cols - 1 - i) * separator_width t))
  end
  else
    let before = ref 0 in
    for j = 0 to i - 1 do
      before := !before + widths.(j) + t.gap
    done;
    Option.map (fun w -> max 0 (w - !before)) max_width

let at lines k = if k < Array.length lines then lines.(k) else View_core.empty
let edge t s = View_core.ink (Border.style t.border) s

(* Line [k] of a row of [(column, width, lines)] cells in [style]; a cell
   has the half of the gap on its left, the rest on its right. *)
let edged_line t style cells k =
  let chars = chars t and last = Array.length cells - 1 in
  let before = pad (t.gap / 2) and after = pad (t.gap - (t.gap / 2)) in
  let parts =
    Array.to_list
      (Array.mapi
         (fun i (col, w, ls) ->
           View_core.(
             before
             ++ styled
                  Style.(style + col.style)
                  (align_span ~pad_right:true col.align w (at ls k))
             ++ after
             ++
             if i < last then
               edge t (Widget.pad_glyph t.frame.mid chars.vertical)
             else empty))
         cells)
  in
  View_core.concat
    ((edge t (Widget.pad_glyph t.frame.left chars.left) :: parts)
    @ [ edge t (Widget.pad_glyph t.frame.right chars.right) ])

(* Without edges, a line ends at its last cell that is not empty, unpadded. *)
let bare_line t style cells k =
  let shown = ref 0 in
  Array.iteri
    (fun i (_, _, lines) -> if View_core.width (at lines k) > 0 then shown := i)
    cells;
  let parts =
    List.filteri (fun i _ -> i <= !shown) (Array.to_list cells)
    |> List.mapi (fun i (col, w, ls) ->
        View_core.(
          (if i > 0 then pad t.gap else empty)
          ++ styled
               Style.(style + col.style)
               (align_span ~pad_right:(i < !shown) col.align w (at ls k))))
  in
  View_core.concat parts

let row_lines ?max_width t widths style row =
  let cells =
    Array.mapi
      (fun i cell ->
        let col = t.columns.(i) in
        let width = slot_width ?max_width t widths row i in
        let lines = Array.of_list (fit t col.overflow width cell) in
        (col, Option.value width ~default:0, lines))
      row
  in
  let height =
    Array.fold_left (fun h (_, _, l) -> max h (Array.length l)) 0 cells
  in
  let line = if edged t then edged_line t else bare_line t in
  List.init height (line style cells)

let lines ?max_width t =
  if Array.length t.columns = 0 then []
  else
    let c = chars t in
    let widths = widths ?max_width t in
    let row style r = row_lines ?max_width t widths style r in
    let has_headers = Array.exists (fun h -> lines_width h > 0) t.headers in
    let between =
      rule t widths ~left:c.left_cross ~mid:c.cross ~right:c.right_cross
        ~line:c.horizontal
    in
    let body =
      List.concat
        (List.mapi
           (fun i r ->
             (if i > 0 && t.rules then between else []) @ row Style.none r)
           t.rows)
    in
    let lines =
      rule t widths ~left:c.top_left ~mid:c.top_cross ~right:c.top_right
        ~line:c.top
      @ (if has_headers then
           row t.header_style t.headers @ if t.rows <> [] then between else []
         else [])
      @ body
      @ rule t widths ~left:c.bottom_left ~mid:c.bottom_cross
          ~right:c.bottom_right ~line:c.bottom
    in
    (* Columns at their narrowest may still not fit beside the border: each
       line is then cut, visibly, rather than a column left out. *)
    match max_width with
    | Some w when rendered_width t widths > w ->
        List.map (Widget.truncate t.look w) lines
    | Some w when not (edged t && c.top <> "") ->
        List.map (Widget.truncate t.look w) lines
    | None when not (edged t && c.top <> "") -> lines
    | _ ->
        Border_motion.frame t.look ~time:(Ctx.time t.ctx) t.border
          ~left:t.frame.left ~right:t.frame.right lines

let view ?border ?(gap = 2) ?header_style ?(rules = false) columns rows =
  if gap < 0 then invalid_arg "Console.Table.view: negative gap";
  let expected = List.length columns in
  List.iter
    (fun cells ->
      let got = List.length cells in
      if got > expected || (got = 0 && expected > 0) then
        Fmt.invalid_arg "Console.Table.view: expected 1 to %d cells, got %d"
          expected got)
    rows;
  let columns = Array.of_list columns in
  let rows = List.map Array.of_list rows in
  let parts =
    Array.to_list (Array.map (fun c -> c.header) columns)
    @ List.concat_map Array.to_list rows
  in
  View_core.deferred ~kind:(Widget.framed_kind parts) (fun ctx ->
      let look = Ctx.look ctx in
      let natural = Ctx.natural ctx in
      let cell view =
        let lines = View_core.lines (View_core.resolve natural view) in
        { view; lines; word = View_core.min_width natural view }
      in
      let border = Widget.border look border in
      let header_style =
        Option.value header_style ~default:(Theme.header look.theme)
      in
      let t =
        {
          ctx;
          look;
          border;
          frame = frame border;
          gap;
          header_style;
          rules;
          columns;
          headers = Array.map (fun c -> cell c.header) columns;
          rows = List.map (Array.map cell) rows;
        }
      in
      View_core.block (lines ?max_width:(Ctx.width ctx) t))
