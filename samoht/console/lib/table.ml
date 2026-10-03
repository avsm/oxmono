(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type align = [ `Left | `Center | `Right ]
type overflow = [ `Truncate | `Wrap ]

type column = {
  header : Span.t;
  align : align;
  min_width : int option;
  max_width : int option;
  overflow : overflow;
  shrinkable : bool;
  style : Style.t;
}

type t = {
  border : Border.t;
  row_separators : bool;
  animated : bool;
  header_style : Style.t;
  columns : column list;
  rows : Span.t list list;
}

let check_widths min_width max_width =
  (match min_width with
  | Some width when width < 0 ->
      invalid_arg "Console.Table.column: negative min_width"
  | _ -> ());
  (match max_width with
  | Some width when width < 0 ->
      invalid_arg "Console.Table.column: negative max_width"
  | _ -> ());
  match (min_width, max_width) with
  | Some min, Some max when min > max ->
      invalid_arg "Console.Table.column: min_width exceeds max_width"
  | _ -> ()

let column_span ?(align = `Left) ?min_width ?max_width ?(overflow = `Wrap)
    ?(shrinkable = true) ?(style = Style.none) header =
  check_widths min_width max_width;
  { header; align; min_width; max_width; overflow; shrinkable; style }

let column ?align ?min_width ?max_width ?overflow ?shrinkable ?style header =
  column_span ?align ?min_width ?max_width ?overflow ?shrinkable ?style
    (Span.text header)

let v ?theme ?border ?(header_style = Style.bold) columns =
  {
    border = Widget.border ?theme ?border ~default:Border.single ();
    row_separators =
      Option.fold ~none:false ~some:Theme.table_row_separators theme;
    animated = Widget.animated theme;
    header_style;
    columns;
    rows = [];
  }

let add_row cells t =
  let expected = List.length t.columns and got = List.length cells in
  if got <> expected then
    Fmt.invalid_arg "Console.Table.add_row: expected %d cells, got %d" expected
      got;
  { t with rows = cells :: t.rows }

let add_row_strings strings t = add_row (List.map Span.text strings) t

let of_rows ?theme ?border ?header_style columns rows =
  let t = v ?theme ?border ?header_style columns in
  List.fold_left (fun t row -> add_row row t) t rows

let of_string_rows ?theme ?border ?header_style columns rows =
  of_rows ?theme ?border ?header_style columns
    (List.map (List.map Span.text) rows)

(* Calculate column widths. A logical cell may contain explicit line feeds;
   its natural width is its widest physical line, not the sum of all lines. *)
let span_lines span =
  span |> Span.sanitize ~keep_newlines:true |> Span.split_lines

let span_width ~char_width span =
  span_lines span
  |> List.fold_left
       (fun width line -> max width (Span.width ~char_width line))
       0

let apply_header_widths ~char_width t widths =
  List.iteri
    (fun i col ->
      widths.(i) <- max widths.(i) (span_width ~char_width col.header))
    t.columns

let apply_row_widths ~char_width t widths =
  let num_cols = Array.length widths in
  List.iter
    (fun row ->
      List.iteri
        (fun i cell ->
          if i < num_cols then
            widths.(i) <- max widths.(i) (span_width ~char_width cell))
        row)
    t.rows

(* A whitespace-delimited token is kept intact while any column still has a
   cheaper whitespace break available. This makes a URL or identifier remain
   selectable without making it an absolute geometry escape hatch: the hard
   terminal width still wins when every such break has been exhausted. Slashes
   are intentionally part of the token here even though [Span.wrap] may use
   them as the final fallback. *)
let span_unbroken_width ~char_width span =
  let word_widths line =
    Span.to_string line |> String.split_on_char ' '
    |> List.map (Width.string_width ~char_width)
  in
  span_lines span |> List.concat_map word_widths |> List.fold_left max 0

let apply_header_unbroken_widths ~char_width t widths =
  List.iteri
    (fun i col ->
      widths.(i) <- max widths.(i) (span_unbroken_width ~char_width col.header))
    t.columns

let apply_row_unbroken_widths ~char_width t widths =
  let num_cols = Array.length widths in
  List.iter
    (fun row ->
      List.iteri
        (fun i cell ->
          if i < num_cols then
            widths.(i) <- max widths.(i) (span_unbroken_width ~char_width cell))
        row)
    t.rows

let apply_width_constraints t widths =
  List.iteri
    (fun i col ->
      (match col.min_width with
      | Some min -> widths.(i) <- max widths.(i) min
      | None -> ());
      match col.max_width with
      | Some max -> widths.(i) <- min widths.(i) max
      | None -> ())
    t.columns

(* A border with no glyphs has no edge: no padding outside the first and the
   last cell, and the last cell not padded out to its column. *)
let edged t = (Border.chars t.border).left <> ""

let structural_overhead t count =
  if count = 0 then 0
  else
    let chars = Border.chars t.border in
    if not (edged t) then (count - 1) * 2
    else
      (count * 2)
      + Width.string_width chars.left
      + Width.string_width chars.right
      + ((count - 1) * Width.string_width chars.right)

let rendered_width t widths =
  Array.fold_left ( + ) (structural_overhead t (Array.length widths)) widths

(* Shrink columns to fit [max_width] columns when given. With no width to fit --
   a pipe or file -- squeezing would wrap or clip cell text (e.g. a long event
   name to "sp"), corrupting output meant to be read or machine-ingested, so
   the natural widths are kept. *)
let shrink_widths_to_terminal ?max_width t unbroken_widths widths =
  match max_width with
  | None -> widths
  | Some term_w ->
      let cols = Array.of_list t.columns in
      let min_of i = match cols.(i).min_width with Some m -> m | None -> 1 in
      let semantic_min i =
        let token =
          match cols.(i).overflow with
          | `Truncate -> 0
          | `Wrap -> unbroken_widths.(i)
        in
        min widths.(i) (max (min_of i) token)
      in
      let shrink_while can_shrink floor =
        let done_ = ref false in
        while rendered_width t widths > term_w && not !done_ do
          let max_i = ref (-1) in
          let max_w = ref (-1) in
          Array.iteri
            (fun i w ->
              if can_shrink i && w > !max_w then (
                max_w := w;
                max_i := i))
            widths;
          if !max_i < 0 then done_ := true
          else
            let excess = rendered_width t widths - term_w in
            let lower = floor !max_i in
            let shrink = min excess (widths.(!max_i) - lower) in
            widths.(!max_i) <- widths.(!max_i) - shrink
        done
      in
      (* Preferences choose which columns yield first. They are deliberately
         not escape hatches from the geometry contract: when the preferred
         pass is exhausted, every column may shrink to zero before a terminal
         row is allowed to exceed [term_w]. *)
      shrink_while
        (fun i -> cols.(i).shrinkable && widths.(i) > semantic_min i)
        semantic_min;
      shrink_while (fun i -> widths.(i) > semantic_min i) semantic_min;
      shrink_while
        (fun i -> cols.(i).shrinkable && widths.(i) > min_of i)
        min_of;
      shrink_while (fun i -> widths.(i) > min_of i) min_of;
      shrink_while (fun i -> widths.(i) > 0) (fun _ -> 0);
      widths

let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

(* A border and cell padding need a minimum number of cells even when every
   value is hidden. At extremely narrow widths, keep the leading columns that
   structurally fit. This is preferable to silently wrapping every right edge
   onto a second terminal row. *)
let fit_columns max_width t =
  let rec count_fit count =
    if count >= List.length t.columns then count
    else if structural_overhead t (count + 1) <= max_width then
      count_fit (count + 1)
    else count
  in
  let count = count_fit 0 in
  if count = List.length t.columns then t
  else
    {
      t with
      columns = take count t.columns;
      rows = List.map (take count) t.rows;
    }

let calc_col_widths ~char_width ?max_width t =
  let widths = Array.make (List.length t.columns) 0 in
  let unbroken_widths = Array.make (List.length t.columns) 0 in
  apply_header_widths ~char_width t widths;
  apply_row_widths ~char_width t widths;
  apply_header_unbroken_widths ~char_width t unbroken_widths;
  apply_row_unbroken_widths ~char_width t unbroken_widths;
  apply_width_constraints t widths;
  shrink_widths_to_terminal ?max_width t unbroken_widths widths |> Array.to_list

let render_horizontal p t widths left_char mid_char right_char horiz_char =
  let chars = Border.chars t.border in
  let put = Paint.ink p (Border.style t.border) in
  if chars.top = "" then () (* No border *)
  else (
    put left_char;
    List.iteri
      (fun i col_width ->
        for _ = 1 to col_width + 2 do
          (* +2 for padding *)
          put horiz_char
        done;
        if i < List.length widths - 1 then put mid_char)
      widths;
    put right_char;
    Paint.newline p)

let align_span ~char_width ~pad_right align target span =
  let width = Span.width ~char_width span in
  if width >= target then span
  else
    let padding = target - width in
    let left, right =
      match align with
      | `Left -> (0, padding)
      | `Right -> (padding, 0)
      | `Center -> (padding / 2, padding - (padding / 2))
    in
    let right = if pad_right then right else 0 in
    Span.(text (String.make left ' ') ++ span ++ text (String.make right ' '))

let process_cell ~char_width ~styled ~style ~pad_right overflow align width span
    =
  let process_line span =
    if width <= 0 then [ Span.empty ]
    else
      match overflow with
      | `Truncate -> (
          let cell_width = Span.width ~char_width span in
          if cell_width <= width then [ span ]
          else
            (* Whole words from the start, never a word cut; a value whose
               first word does not fit is printed whole, over several lines. *)
            match Width.shorten ~char_width width (Span.to_string span) with
            | "" -> Span.wrap ~char_width width span
            | kept ->
                [
                  Span.truncate ~char_width
                    (Width.string_width ~char_width kept)
                    span;
                ])
      | `Wrap -> Span.wrap ~char_width width span
  in
  let lines = span_lines span |> List.concat_map process_line in
  List.map
    (fun line ->
      let line = align_span ~char_width ~pad_right align width line in
      if styled then Span.to_ansi_string ~style line else Span.to_string line)
    lines

let render_physical_row p t widths line_cells =
  let chars = Border.chars t.border in
  let num_cols = List.length widths in
  let put = Paint.ink p (Border.style t.border) in
  let edged = edged t in
  if edged then put chars.left;
  List.iteri
    (fun i ((_col, _col_width), line) ->
      let before = if edged || i > 0 then " " else "" in
      let after = if edged || i < num_cols - 1 then " " else "" in
      Paint.text p (before ^ line ^ after);
      if i < num_cols - 1 && edged then put chars.right)
    (List.combine (List.combine t.columns widths) line_cells);
  if chars.right <> "" then put chars.right;
  Paint.newline p

let render_row ~char_width ~styled p t widths style cells =
  (* Index cells and widths by column once: the per-column [List.nth]/
     [List.length] this replaced made rendering a row quadratic in the column
     count. *)
  let cells_arr = Array.of_list cells in
  let ncells = Array.length cells_arr in
  let widths_arr = Array.of_list widths in
  let last = List.length widths - 1 in
  let pad_right i = edged t || i < last in
  (* Process each cell to get list of lines *)
  let cell_lines =
    List.mapi
      (fun i (col, col_width) ->
        let cell = if i < ncells then cells_arr.(i) else Span.empty in
        let style = Style.(style + col.style) in
        process_cell ~char_width ~styled ~style ~pad_right:(pad_right i)
          col.overflow col.align col_width cell)
      (List.combine t.columns widths)
  in
  (* Find max number of lines *)
  let max_lines =
    List.fold_left (fun acc lines -> max acc (List.length lines)) 0 cell_lines
  in
  (* Render each physical row *)
  for line_idx = 0 to max_lines - 1 do
    let line_cells =
      List.mapi
        (fun i lines ->
          if line_idx < List.length lines then List.nth lines line_idx
          else if pad_right i then String.make widths_arr.(i) ' '
          else "")
        cell_lines
    in
    render_physical_row p t widths line_cells
  done

let pp_with ~char_width ?max_width ppf t =
  let t =
    match max_width with None -> t | Some width -> fit_columns (max 0 width) t
  in
  if List.length t.columns = 0 then ()
  else
    let styled = Fmt.style_renderer ppf = `Ansi_tty in
    let p = Paint.v ppf in
    let chars = Border.chars t.border in
    let widths = calc_col_widths ~char_width ?max_width t in

    (* Top border *)
    render_horizontal p t widths chars.top_left chars.top_cross chars.top_right
      chars.top;

    (* Header row (skip if all headers are empty) *)
    let headers = List.map (fun col -> col.header) t.columns in
    let has_headers = List.exists (fun h -> Span.to_string h <> "") headers in
    if has_headers then (
      render_row ~char_width ~styled p t widths t.header_style headers;
      if List.length t.rows > 0 then
        render_horizontal p t widths chars.left_cross chars.cross
          chars.right_cross chars.top);

    (* Data rows *)
    let rec render_rows = function
      | [] -> ()
      | [ row ] -> render_row ~char_width ~styled p t widths Style.none row
      | row :: rows ->
          render_row ~char_width ~styled p t widths Style.none row;
          if t.row_separators then
            render_horizontal p t widths chars.left_cross chars.cross
              chars.right_cross chars.top;
          render_rows rows
    in
    render_rows (List.rev t.rows);

    (* Bottom border *)
    render_horizontal p t widths chars.bottom_left chars.bottom_cross
      chars.bottom_right chars.bottom;
    Paint.close p

let pp ppf t =
  let max_width = Format.pp_get_margin ppf () in
  pp_with ~char_width:Width.default_char_width ~max_width ppf t

let to_string ?width ?(char_width = Width.default_char_width) t =
  Render.to_string (pp_with ~char_width ?max_width:width) t

let to_ansi_string ?width ?(char_width = Width.default_char_width) t =
  Render.to_string ~style_renderer:`Ansi_tty
    (pp_with ~char_width ?max_width:width)
    t

(* Frame the rendered table with a Matrix rain border. *)
let rain ?width ?(char_width = Width.default_char_width) t =
  Anim.v (fun ~elapsed ->
      let body =
        to_ansi_string ?width ~char_width t
        |> String.split_on_char '\n'
        |> List.filter (fun l -> l <> "")
      in
      Rain.frame ~char_width body ~elapsed)

(* A still frame, unless the theme asked for an animated border. *)
let anim ?width ?char_width t =
  if t.animated then rain ?width ?char_width t
  else Anim.const (to_ansi_string ?width ?char_width t)
