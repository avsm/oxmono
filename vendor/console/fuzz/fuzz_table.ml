(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* Cell text whose bytes, clusters and cells do not line up. *)
let pool =
  [|
    "x";
    "abc";
    "\xe7\x95\x8c\xe7\x95\x8c";
    "e\xcc\x81";
    "\xf0\x9f\x8e\x89 ok";
    "a b c d e f";
    "src/lib/a.ml";
    "\xff\xfe";
    "\t\007bell";
    "";
    "a long cell that will not fit in a narrow column at all";
  |]

let pick s i =
  let s = s ^ "\000" in
  pool.((Char.code s.[i mod String.length s] + i) mod Array.length pool)

(* What [pp] prints at [margin], in colour, as lines without their escapes. *)
let render ?theme pp margin v =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Option.iter (Theme.set ppf) theme;
  Format.pp_set_margin ppf margin;
  Fmt.set_style_renderer ppf `Ansi_tty;
  pp ppf v;
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buf in
  (raw, List.map Console_text.strip (String.split_on_char (Char.chr 10) raw))

let cells s = Width.string_width s

let check_fits margin lines =
  List.iter
    (fun l ->
      if cells l > margin then
        failf "%d cells at margin %d: %S" (cells l) margin l)
    lines

(* Every style a widget turns on, it turns off before the line ends. *)
let check_closed raw =
  List.iter
    (fun l ->
      match String.rindex_opt l (Char.chr 27) with
      | None -> ()
      | Some i ->
          let rest = String.sub l i (String.length l - i) in
          if
            not
              (String.starts_with ~prefix:"\027[0m" rest
              || String.starts_with ~prefix:"\027]8;;\027" rest)
          then failf "style left on: %S" l)
    (String.split_on_char (Char.chr 10) raw)

let table ?(border = Border.rounded) s =
  let ncol = 1 + (Char.code (String.get (s ^ "\001") 0) mod 4) in
  let columns =
    List.init ncol (fun i ->
        Table.column
          ~align:(match i mod 3 with 0 -> `Left | 1 -> `Right | _ -> `Center)
          ~overflow:(if i mod 2 = 0 then `Wrap else `Truncate)
          (View.text (pick s (i + 1))))
  in
  let rows =
    List.init 3 (fun r ->
        List.init ncol (fun c -> View.text (pick s (2 + (r * ncol) + c))))
  in
  (ncol, Table.view ~border columns rows)

(* No line is wider than the margin, every style is turned off, and a framed
   table that fits has all its lines as wide as one another. *)
let test_fits s n =
  let margin = 4 + (abs n mod 80) in
  let ncol, t = table s in
  let raw, lines = render View.pp margin t in
  check_fits margin lines;
  check_closed raw;
  if margin >= (ncol * 3) + 1 then
    match lines with
    | first :: rest ->
        List.iter (fun l -> check_eq ~pp:Fmt.int (cells first) (cells l)) rest
    | [] -> fail "no line"

(* A table in [Border.simple] fits the margin, and once its columns fit,
   the rule under a header runs across it: no line is wider. *)
let test_simple s n =
  let margin = 4 + (abs n mod 80) in
  let ncol, t = table ~border:Border.simple s in
  let raw, lines = render View.pp margin t in
  check_fits margin lines;
  check_closed raw;
  let headed =
    List.exists (fun i -> pick s (i + 1) <> "") (List.init ncol Fun.id)
  in
  let is_rule l =
    l <> "" && String.concat "" (List.init (cells l) (Fun.const "\u{2500}")) = l
  in
  if headed && margin >= ncol * 3 then
    match List.filter is_rule lines with
    | [] -> fail "no header rule"
    | rule :: _ ->
        List.iter
          (fun l ->
            if cells l > cells rule then failf "%S is wider than the rule" l)
          lines

(* A right-aligned column's cells end in the same column. *)
let test_right_aligned s =
  let cells_of i = View.text (pick s i) in
  let rows = List.init 4 (fun i -> [ cells_of i; View.text "|" ]) in
  let t =
    Table.view ~border:Border.none
      [
        Table.column ~align:`Right (View.text "a"); Table.column (View.text "b");
      ]
      rows
  in
  let firsts = View.text "a" :: List.map List.hd rows in
  let w = List.fold_left (fun n c -> max n (View.width c)) 0 firsts in
  List.iter2
    (fun cell line ->
      let text = View.to_string cell in
      if not (String.contains text '\n') then begin
        let expected = String.make (w - View.width cell) ' ' ^ text in
        if not (String.starts_with ~prefix:expected line) then
          failf "%S does not start with %S" line expected
      end)
    firsts
    (String.split_on_char '\n' (View.to_string t))

(* A row with more cells than columns, or none, is refused. *)
let test_shape s n =
  let k = abs n mod 4 in
  let rows = [ List.init k (fun i -> View.text (pick s i)) ] in
  match
    Table.view
      [ Table.column (View.text "a"); Table.column (View.text "b") ]
      rows
  with
  | _ -> if k = 0 || k > 2 then fail "bad row accepted"
  | exception Invalid_argument _ -> if k >= 1 && k <= 2 then fail "row refused"

let test_huge_min_width n =
  let minimum = max_int - (n mod 1024) in
  let t =
    Table.view
      [
        Table.column ~min_width:minimum (View.text "A");
        Table.column (View.text "B");
      ]
      [ [ View.text "x"; View.text "y" ] ]
  in
  let out = View.to_string (View.sized ~width:20 t) in
  check_fits 20 (String.split_on_char '\n' out)

let test_wide_border n =
  let glyph = String.make (2 + (n mod 3)) '#' in
  let border =
    Border.v
      Border.
        {
          top_left = glyph;
          top = glyph;
          top_right = glyph;
          left = glyph;
          right = glyph;
          bottom_left = glyph;
          bottom = glyph;
          bottom_right = glyph;
          vertical = glyph;
          horizontal = glyph;
          cross = glyph;
          top_cross = glyph;
          bottom_cross = glyph;
          left_cross = glyph;
          right_cross = glyph;
        }
  in
  let t =
    Table.view ~border
      [ Table.column (View.text "A"); Table.column (View.text "B") ]
      [ [ View.text "x"; View.text "y" ] ]
  in
  let lines =
    String.split_on_char '\n' (View.to_string (View.sized ~width:40 t))
  in
  check_fits 40 lines;
  let widths = List.map cells lines in
  List.iter (fun w -> check_eq ~pp:Fmt.int (List.hd widths) w) widths

let themes = List.map snd Theme.all

let check_marker_columns mixed s =
  (* A marker in the next column gives an observable column boundary. *)
  let t =
    Table.view ~border:Border.none
      [
        Table.column ~align:`Right (View.text ""); Table.column (View.text "|");
      ]
      (List.map
         (fun cell -> [ cell; View.text "|" ])
         [
           mixed;
           View.text (Console_text.sanitize ~keep_newlines:false s);
           View.empty;
         ])
  in
  let lines = String.split_on_char '\n' (View.to_string t) in
  let positions =
    List.map
      (fun line ->
        let i = String.rindex line '|' in
        cells (String.sub line 0 i))
      lines
  in
  List.iter (fun p -> check_eq ~pp:Fmt.int (List.hd positions) p) positions

let test_mixed_script_themes s n =
  let mixed =
    View.(
      styled Style.bold (text "\u{754c}e")
      ++ styled (Style.fg Color.red) (text "\u{0301}\u{1f469}\u{200d}\u{1f4bb}")
      ++ text " \u{0645}\u{0631}\u{062d}\u{0628}\u{0627} \u{1f1eb}\u{1f1f7}")
  in
  let rows =
    [
      [ mixed; View.text "1" ];
      [ View.text s; View.text "999" ];
      [ View.empty; View.text "42" ];
      [ View.text "\n\n"; View.empty ];
    ]
  in
  let margin = 12 + (n mod 69) in
  List.iter
    (fun theme ->
      let t =
        Table.view
          [
            Table.column (View.text "name");
            Table.column ~align:`Right (View.text "size");
          ]
          rows
      in
      let raw, lines = render ~theme View.pp margin t in
      check (String.is_valid_utf_8 raw);
      check_fits margin lines;
      check_closed raw;
      let widths = List.map cells lines in
      List.iter (fun w -> check_eq ~pp:Fmt.int (List.hd widths) w) widths)
    themes;
  check_marker_columns mixed s

(* [rules] draws a rule between each two rows, in the glyphs of the
   header's rule: with the header's, as many rules as rows. *)
let test_rules s n =
  let ncol = 1 + (Char.code (String.get (s ^ "\001") 0) mod 4) in
  let nrows = 1 + (abs n mod 6) in
  let columns = List.init ncol (fun _ -> Table.column (View.text "h")) in
  let rows =
    List.init nrows (fun r ->
        List.init ncol (fun c -> View.text (pick s (2 + (r * ncol) + c))))
  in
  let t = Table.view ~rules:true ~border:Border.rounded columns rows in
  let _, lines = render View.pp 1000 t in
  let rules = List.filter (String.starts_with ~prefix:"\u{251c}") lines in
  check_eq ~pp:Fmt.int nrows (List.length rules);
  List.iter (fun r -> check_eq ~pp:Fmt.Dump.string (List.hd rules) r) rules

(* Border fields are independent: even an empty body edge occupies the
   column of its nonempty corners and crossings. *)
let test_independent_edges a b empty_right =
  let a = 1 + (a mod 8) and b = 1 + (b mod 8) in
  let border =
    Border.v
      {
        (Border.chars Border.single) with
        left = "";
        right = (if empty_right then "" else "│");
      }
  in
  let table =
    Table.view ~border
      [ Table.column (View.text "A"); Table.column (View.text "B") ]
      [ [ View.text (String.make a 'x'); View.text (String.make b 'y') ] ]
  in
  let expected_width = a + b + 7 in
  let raw, lines = render View.pp 80 table in
  check_closed raw;
  List.iter
    (fun line ->
      let term = Console_vte.v ~raw:true ~rows:2 ~cols:80 () in
      Console_vte.feed term line;
      check_eq ~pp:Fmt.int expected_width (snd (Console_vte.cursor_pos term));
      check_eq ~pp:Fmt.int expected_width (cells line))
    lines

let test_gap_width_room n gap k =
  let width = 1 + (n mod 80) and gap = gap mod 9 in
  let count = 1 + (k mod 4) in
  let framed = k land 4 = 0 in
  let border = if framed then Border.rounded else Border.simple in
  let columns =
    List.init count (fun i ->
        Table.column
          ~width:(1 + ((k + i) mod 15))
          ~align:[| `Left; `Center; `Right |].(i mod 3)
          ~overflow:(if k land 8 = 0 then `Wrap else `Truncate)
          (View.text "head"))
  in
  let rows =
    [
      List.init count (fun _ -> View.text "界界界");
      List.init count (fun _ -> View.text "e\u{301} 👩‍💻 text");
    ]
  in
  let raw, lines =
    render View.pp width (Table.view ~border ~gap columns rows)
  in
  check (String.is_valid_utf_8 raw);
  check_fits width lines;
  check_closed raw;
  if framed && width >= (count * gap) + count + 1 then
    List.iter
      (fun line -> check_eq ~pp:Fmt.int (cells (List.hd lines)) (cells line))
      lines

let suite =
  ( "table",
    [
      test_case "gaps and preferred widths keep narrow tables in their room"
        [ uint8; uint8; uint8 ] test_gap_width_room;
      test_case "independent empty edges keep a rectangular frame"
        [ uint8; uint8; bool ] test_independent_edges;
      test_case "a rule between each two rows" [ bytes; int ] test_rules;
      test_case "mixed-script columns align in every theme" [ bytes; uint8 ]
        test_mixed_script_themes;
      test_case "multi-cell borders stay rectangular" [ uint8 ] test_wide_border;
      test_case "huge minimum narrows to available width" [ uint8 ]
        test_huge_min_width;
      test_case "fits the margin" [ bytes; int ] test_fits;
      test_case "a simple table's rule runs across it" [ bytes; int ]
        test_simple;
      test_case "right-aligned cells line up" [ bytes ] test_right_aligned;
      test_case "row shape" [ bytes; int ] test_shape;
    ] )
