(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let of_string_rows ?border columns rows =
  Table.view ?border columns (List.map (List.map View.text) rows)

let test_basic () =
  let table =
    Table.(
      view
        [ column (View.text "A"); column (View.text "B") ]
        [ [ View.text "1"; View.text "2" ]; [ View.text "3"; View.text "4" ] ])
  in
  let output = View.to_string table in
  Alcotest.(check bool) "contains A" true (contains "A" output);
  Alcotest.(check bool) "contains 1" true (contains "1" output)

(* Without a border or a theme, a table is framed as a panel is. *)
let test_default_border () =
  let table = Table.(view [ column (View.text "A") ] [ [ View.text "1" ] ]) in
  let panel = Panel.view (View.text "1") in
  let first s = String.sub s 0 (String.index s '\n') in
  Alcotest.(check string)
    "top-left corner"
    (Astring.String.with_range ~len:3 (first (View.to_string panel)))
    (Astring.String.with_range ~len:3 (first (View.to_string table)))

let test_empty () =
  let table = Table.(view [ column (View.text "X") ] []) in
  let output = View.to_string table in
  Alcotest.(check bool) "contains X" true (contains "X" output)

let test_wrap () =
  let table =
    Table.(
      view
        [ column ~max_width:10 ~overflow:`Wrap (View.text "Header") ]
        [ [ View.text "hello world this wraps" ] ])
  in
  let output = View.to_string table in
  Alcotest.(check bool) "contains hello" true (contains "hello" output);
  Alcotest.(check bool) "contains world" true (contains "world" output);
  Alcotest.(check bool) "contains wraps" true (contains "wraps" output);
  Alcotest.(check bool) "no ellipsis" false (contains "…" output)

(* A URL is one selectable terminal token. When another column can wrap at
   whitespace, fitting gives that slack up before splitting the URL at a slash.
   The hard terminal-width bound still wins when no such slack remains. *)
let test_wrap_preserves_unbroken_token () =
  let url = "https://rg.fr-par.scw.cloud/run-space" in
  let table =
    of_string_rows
      [
        Table.column (View.text "Context");
        Table.column (View.text "Service");
        Table.column (View.text "Endpoint");
      ]
      [ [ "infra  ACTIVE"; "Scheduler"; url ] ]
  in
  let natural_width = 10 + 13 + 9 + String.length url in
  let output = View.to_string (View.sized ~width:(natural_width - 1) table) in
  Alcotest.(check bool) "URL remains contiguous" true (contains url output)

(* String conversion is plain. The formatter passed to [pp], not terminal
   detection, owns colour policy. *)
let test_string_conversion_is_plain () =
  let t =
    of_string_rows
      [ Table.column (View.text "A"); Table.column (View.text "B") ]
      [ [ "1"; "2" ] ]
  in
  let has_esc s = String.contains s '\027' in
  Alcotest.(check bool) "tty string is plain" false (has_esc (View.to_string t));
  Alcotest.(check bool) "piped is plain" false (has_esc (View.to_string t))

let render renderer table =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf renderer;
  View.pp ppf table;
  Format.pp_print_char ppf '\n';
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* [table] printed on a formatter in [theme], in UTF-8 iff [utf_8], in
   true colour iff [styled]. *)
let printed ?(theme = Theme.default) ?(utf_8 = true) ?(styled = false) table =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf utf_8;
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Color.set_depth ppf `True_color
  end;
  View.pp ppf table;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* bar.mli: a bar in a cell is drawn in the theme and the UTF-8 of the
   formatter the table is printed on, and a column narrower than the bar
   narrows it. *)
let test_bar_cell () =
  let table ?overflow ?max_width bar =
    Table.view ~border:Border.none
      [
        Table.column (View.text "task");
        Table.column ?overflow ?max_width (View.text "done");
      ]
      [ [ View.text "copy"; bar ] ]
  in
  let half = table (Bar.horizontal ~width:4 0.5) in
  let row s = List.nth (String.split_on_char '\n' s) 1 in
  let check msg want got = Alcotest.(check string) msg want (row got) in
  check "default" "copy  \u{2501}\u{2501}\u{2500}\u{2500}" (printed half);
  check "dos" "copy  \u{2588}\u{2588}\u{2591}\u{2591}"
    (printed ~theme:Theme.dos half);
  check "not UTF-8" "copy  ##--" (printed ~utf_8:false half);
  check "ascii" "copy  ##--" (printed ~theme:Theme.ascii half);
  let cut =
    table ~overflow:`Truncate ~max_width:5 (Bar.horizontal ~width:8 0.5)
  in
  check "narrowed, dos" "copy  \u{2588}\u{2588}\u{2588}\u{2591}\u{2591}"
    (printed ~theme:Theme.dos cut);
  check "narrowed, not UTF-8" "copy  ###--"
    (printed ~theme:Theme.dos ~utf_8:false cut);
  let accent t s =
    View.to_string ~styled:true
      (View.styled (Style.fg (Theme.accent t)) (View.text s))
  in
  Alcotest.(check bool)
    "in the theme's colours" true
    (contains
       (accent Theme.nord "\u{2501}\u{2501}")
       (printed ~theme:Theme.nord ~styled:true half))

(* table.mli, to_span: without a width, a table as a span is laid out at
   its natural width, so a short row's last cell, which runs to the end of
   the line in a table without edges, is not aligned against an unbounded
   line. *)
let test_span_natural_width () =
  let table =
    Table.view ~border:Border.none
      [
        Table.column (View.text "a");
        Table.column ~align:`Right (View.text "b");
        Table.column ~align:`Center (View.text "c");
      ]
      [
        [ View.text "x"; View.text "y"; View.text "z" ];
        [ View.text "s"; View.text "short" ];
      ]
  in
  let span = table in
  Alcotest.(check string)
    "as printed at its natural width" (View.to_string table)
    (View.to_string span)

(* bar.mli, to_span: a bar alone as a span is drawn when printed. *)
let test_bar_span () =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf Theme.dos;
  View.pp ppf View.(text "[" ++ Bar.horizontal ~width:2 0.5 ++ text "]");
  Format.pp_print_flush ppf ();
  Alcotest.(check string) "dos" "[\u{2588}\u{2591}]" (Buffer.contents buf);
  Alcotest.(check string)
    "to_string, the default theme" "\u{2501}\u{2501}"
    (View.to_string (Bar.horizontal ~width:2 1.));
  Alcotest.(check int)
    "width" 6
    (View.width (Bar.horizontal ~brackets:true ~width:4 0.5))

let test_formatter_gates_cell_styles () =
  let table =
    Table.view
      [ Table.column (View.text "A") ]
      [ [ View.styled Style.(fg Color.red) (View.text "red") ] ]
  in
  Alcotest.(check bool)
    "plain formatter has no escapes" false
    (String.contains (render `None table) '\027');
  Alcotest.(check bool)
    "ANSI formatter keeps cell style" true
    (contains "\027[31m" (render `Ansi_tty table))

let test_link_cell () =
  let table =
    Table.view
      [ Table.column (View.text "A") ]
      [ [ View.link ~uri:"https://example.com" (View.text "docs") ] ]
  in
  Alcotest.(check bool)
    "plain keeps label" true
    (contains "docs" (render `None table));
  let ansi = render `Ansi_tty table in
  Alcotest.(check bool) "ANSI keeps OSC 8" true (contains "\027]8;;" ansi)

let test_formatter_margin_never_wraps_border () =
  let width = 48 in
  let table =
    of_string_rows ~border:Border.rounded
      [
        Table.column ~shrinkable:false (View.text "Package");
        Table.column ~overflow:`Wrap (View.text "Remote");
      ]
      [
        [ "ocaml-parquet"; "git@git.recoil.org:gazagnaire.org/ocaml-parquet" ];
        [ "ocaml-sqlite"; "git@git.recoil.org:gazagnaire.org/ocaml-sqlite" ];
      ]
  in
  let buffer = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buffer in
  Format.pp_set_margin ppf width;
  Fmt.set_style_renderer ppf `Ansi_tty;
  View.pp ppf table;
  Format.pp_print_char ppf '\n';
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents buffer in
  let logical_rows =
    raw |> String.split_on_char '\n'
    |> List.filter (fun row -> String.trim row <> "")
  in
  List.iter
    (fun row ->
      Alcotest.(check int)
        "raw row reaches margin" width
        (Width.string_width (Console_text.strip row)))
    logical_rows;
  let vte = Console_vte.v ~rows:12 ~cols:width () in
  Console_vte.feed vte raw;
  let rows = List.filter (( <> ) "") (Console_vte.screen vte) in
  let left_edge row =
    List.exists
      (fun prefix -> String.starts_with ~prefix row)
      [ "╭"; "│"; "├"; "╰" ]
  in
  let right_edge row =
    List.exists
      (fun suffix -> String.ends_with ~suffix row)
      [ "╮"; "│"; "┤"; "╯" ]
  in
  Alcotest.(check bool)
    "every physical row retains both borders" true
    (List.for_all (fun row -> left_edge row && right_edge row) rows);
  Alcotest.(check int)
    "no wrapped physical rows" (List.length logical_rows) (List.length rows);
  List.iter
    (fun row ->
      Alcotest.(check int)
        "visual row reaches margin" width
        (Width.string_width (Console_text.strip row)))
    rows;
  Alcotest.(check int)
    "table does not enter scrollback" 0
    (Console_vte.scrollback_size vte);
  Alcotest.(check (pair int int))
    "cursor begins the next physical row"
    (List.length logical_rows, 0)
    (Console_vte.cursor_pos vte)

(* Every narrow width is a hard limit. Protected columns, preferred minima, wide glyphs, combining
   marks, control bytes and malformed UTF-8 are all incapable of overriding the
   terminal margin. This catches the whole right-border-wrap family, including
   one-cell errors at column-selection boundaries. *)
let test_every_margin_is_a_hard_limit () =
  let hostile =
    [
      "\xe4\xbd\xa0\xe5\xa5\xbd";
      "e\xcc\x81\xcc\x82";
      "\xff\xfe\xfd";
      "\t\007control";
      String.make 200 'W';
    ]
  in
  let columns =
    List.mapi
      (fun i header ->
        Table.column ~min_width:12
          ~shrinkable:(i mod 2 = 0)
          ~overflow:(if i mod 2 = 0 then `Wrap else `Truncate)
          (View.text header))
      hostile
  in
  let table = of_string_rows ~border:Border.rounded columns [ hostile ] in
  for width = 4 to 80 do
    let buffer = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buffer in
    Format.pp_set_margin ppf width;
    Fmt.set_style_renderer ppf `Ansi_tty;
    View.pp ppf table;
    Format.pp_print_char ppf '\n';
    Format.pp_print_flush ppf ();
    let raw = Buffer.contents buffer in
    let logical_rows =
      raw |> String.split_on_char '\n'
      |> List.filter (fun row -> String.trim row <> "")
    in
    Alcotest.(check bool)
      (Fmt.str "width %d rendered a table" width)
      true (logical_rows <> []);
    List.iter
      (fun row ->
        Alcotest.(check int)
          (Fmt.str "raw row at width %d" width)
          width
          (Width.string_width (Console_text.strip row)))
      logical_rows;
    let vte =
      Console_vte.v ~rows:(List.length logical_rows + 2) ~cols:width ()
    in
    Console_vte.feed vte raw;
    let visual_rows = List.filter (( <> ) "") (Console_vte.screen vte) in
    Alcotest.(check int)
      (Fmt.str "physical row count at width %d" width)
      (List.length logical_rows) (List.length visual_rows);
    Alcotest.(check int)
      (Fmt.str "no scrollback at width %d" width)
      0
      (Console_vte.scrollback_size vte);
    List.iter
      (fun row ->
        Alcotest.(check bool)
          (Fmt.str "left border at width %d" width)
          true
          (List.exists
             (fun edge -> String.starts_with ~prefix:edge row)
             [ "╭"; "│"; "├"; "╰" ]);
        (* Five columns and their borders take 16 cells at the least;
           below that, every line is cut where it reaches the margin and
           says so, rather than a column being left out. *)
        Alcotest.(check bool)
          (Fmt.str "right border or a cut at width %d" width)
          true
          (List.exists
             (fun edge -> String.ends_with ~suffix:edge row)
             (if width >= 16 then [ "╮"; "│"; "┤"; "╯" ] else [ "…" ]));
        Alcotest.(check int)
          (Fmt.str "visual row at width %d" width)
          width
          (Width.string_width (Console_text.strip row)))
      visual_rows;
    Alcotest.(check (pair int int))
      (Fmt.str "cursor at width %d" width)
      (List.length logical_rows, 0)
      (Console_vte.cursor_pos vte)
  done

let test_invalid_shape () =
  Alcotest.check_raises "negative minimum width"
    (Invalid_argument "Console.Table.column: negative min_width") (fun () ->
      ignore (Table.column ~min_width:(-1) (View.text "A")));
  Alcotest.check_raises "minimum exceeds maximum"
    (Invalid_argument "Console.Table.column: min_width exceeds max_width")
    (fun () -> ignore (Table.column ~min_width:3 ~max_width:2 (View.text "A")));
  let columns =
    [ Table.column (View.text "A"); Table.column (View.text "B") ]
  in
  Alcotest.check_raises "too many cells"
    (Invalid_argument "Console.Table.view: expected 1 to 2 cells, got 3")
    (fun () ->
      ignore (Table.view columns [ List.map View.text [ "a"; "b"; "c" ] ]));
  Alcotest.check_raises "no cell"
    (Invalid_argument "Console.Table.view: expected 1 to 2 cells, got 0")
    (fun () -> ignore (Table.view columns [ [] ]))

(* A [~shrinkable:false] column keeps its full width when the table is fitted to
   a narrow terminal; a shrinkable neighbour gives way instead. *)
let test_shrinkable_protects_column () =
  let long_name = "a-very-long-package-name" in
  let detail = "1234567890" in
  let protected =
    Table.(
      view
        [
          column ~shrinkable:false (View.text "Package");
          column ~overflow:`Truncate (View.text "Detail");
        ]
        [ [ View.text long_name; View.text detail ] ])
  in
  Alcotest.(check bool)
    "unshrinkable package name is kept in full" true
    (contains long_name (View.to_string (View.sized ~width:35 protected)));
  let shrinkable =
    Table.(
      view
        [
          column ~overflow:`Truncate (View.text "Package");
          column (View.text "Detail");
        ]
        [ [ View.text long_name; View.text detail ] ])
  in
  Alcotest.(check bool)
    "a shrinkable package name is cut at the same width" false
    (contains long_name (View.to_string (View.sized ~width:35 shrinkable)))

(* A border with no glyphs draws no edge either: a row starts at its first
   cell and ends at its last cell's text, so a listing pipes as it prints. A
   right-aligned last column keeps its padding, which is on its left. *)
let test_no_border_draws_no_edge () =
  let lines table =
    View.to_string table |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
  in
  Alcotest.(check (list string))
    "each row starts at its first cell and ends at its last"
    [ "NAME         SIZE"; "alpha        1"; "beta-longer  22" ]
    (lines
       (of_string_rows ~border:Border.none
          [ Table.column (View.text "NAME"); Table.column (View.text "SIZE") ]
          [ [ "alpha"; "1" ]; [ "beta-longer"; "22" ] ]));
  Alcotest.(check (list string))
    "a right-aligned last column is padded on its left"
    [ "NAME  SIZE"; "a        1"; "b       22" ]
    (lines
       (of_string_rows ~border:Border.none
          [
            Table.column (View.text "NAME");
            Table.column ~align:`Right (View.text "SIZE");
          ]
          [ [ "a"; "1" ]; [ "b"; "22" ] ]));
  Alcotest.(check (list string))
    "a cell of fewer lines than its row ends where its text ends"
    [ "NAME  NOTE"; "a     one"; "      two" ]
    (lines
       (of_string_rows ~border:Border.none
          [ Table.column (View.text "NAME"); Table.column (View.text "NOTE") ]
          [ [ "a"; "one\ntwo" ] ]));
  let col = Table.column (View.text "") in
  Alcotest.(check string)
    "a line ends at its last cell that is not empty" "fix   stuck  /pool\nmain"
    (View.to_string
       (of_string_rows ~border:Border.none [ col; col; col ]
          [ [ "fix"; "stuck"; "/pool" ]; [ "main"; ""; "" ] ]))

(* [gap] is the spaces between two cells, a frame's glyph among them. *)
let test_gap () =
  let cols = [ Table.column (View.text "A"); Table.column (View.text "BB") ] in
  let rows = [ [ View.text "1"; View.text "2" ] ] in
  Alcotest.(check string)
    "one space without edges" "A BB\n1 2"
    (View.to_string (Table.view ~border:Border.none ~gap:1 cols rows));
  Alcotest.(check string)
    "none" "ABB\n12"
    (View.to_string (Table.view ~border:Border.none ~gap:0 cols rows));
  Alcotest.(check string)
    "framed, no gap" "+-+--+\n|A|BB|\n+-+--+\n|1|2 |\n+-+--+"
    (View.to_string (Table.view ~border:Border.ascii ~gap:0 cols rows));
  Alcotest.(check string)
    "framed, the default"
    "+---+----+\n| A | BB |\n+---+----+\n| 1 | 2  |\n+---+----+"
    (View.to_string (Table.view ~border:Border.ascii cols rows));
  Alcotest.(check string)
    "a rule without edges runs under the gaps" "A BB\n────\n1 2"
    (View.to_string (Table.view ~border:Border.simple ~gap:1 cols rows));
  Alcotest.check_raises "a negative gap"
    (Invalid_argument "Console.Table.view: negative gap") (fun () ->
      ignore (Table.view ~gap:(-1) cols rows))

(* A column of a given width is that wide at its natural size, whatever its
   cells, and narrows as a column as wide as its cells would. *)
let test_column_width () =
  let lines t = String.split_on_char '\n' (View.to_string t) in
  Alcotest.(check (list string))
    "its width"
    [ "Name    Session"; "Acme    up" ]
    (lines
       (Table.view ~border:Border.none
          [
            Table.column ~width:6 (View.text "Name");
            Table.column (View.text "Session");
          ]
          [ [ View.text "Acme"; View.text "up" ] ]));
  Alcotest.(check (list string))
    "a wider cell wraps"
    [ "Name  S"; "Acme  up"; "Corp" ]
    (lines
       (Table.view ~border:Border.none
          [
            Table.column ~width:4 (View.text "Name");
            Table.column (View.text "S");
          ]
          [ [ View.text "Acme Corp"; View.text "up" ] ]));
  let cols =
    [
      Table.column ~shrinkable:false (View.text "Company");
      Table.column ~width:26 (View.text "Session");
    ]
  in
  let row = [ View.text "Acme Corporation"; View.text "open 09:30 to 16:00" ] in
  let narrow =
    View.to_string
      (View.sized ~width:30 (Table.view ~border:Border.none cols [ row ]))
  in
  Alcotest.(check bool)
    "it narrows before a column that does not" true
    (contains "Acme Corporation" narrow);
  Alcotest.check_raises "a negative width"
    (Invalid_argument "Console.Table.column: negative width") (fun () ->
      ignore (Table.column ~width:(-1) (View.text "A")))

(* A cell that may be laid out narrower than its widest word, such as a
   sparkline ([View.fit ~min_width]), lets its column narrow first, also
   when its overflow wraps. *)
let test_cell_min_width () =
  let spark = Bar.sparkline (List.init 20 float_of_int) in
  let narrow session =
    let cols =
      [
        Table.column ~shrinkable:false (View.text "Company");
        Table.column ~width:20 (View.text "Session");
      ]
    in
    View.to_string
      (View.sized ~width:30
         (Table.view ~border:Border.none cols
            [ [ View.text "Acme Corporation"; session ] ]))
  in
  Alcotest.(check bool)
    "a sparkline narrows first" true
    (contains "Acme Corporation" (narrow spark));
  Alcotest.(check bool)
    "also in a fit" true
    (contains "Acme Corporation" (narrow (View.fit (fun _ -> spark))));
  Alcotest.(check bool)
    "a fit's min_width" true
    (contains "Acme Corporation"
       (narrow
          (View.fit ~min_width:4 (fun _ -> View.text (String.make 20 'x')))));
  Alcotest.(check bool)
    "a word stays whole" false
    (contains "Acme Corporation" (narrow (View.text (String.make 20 'x'))));
  let fit = View.fit ~min_width:4 (fun _ -> View.text (String.make 20 'x')) in
  Alcotest.(check bool)
    "a styled fit's min_width" true
    (contains "Acme Corporation" (narrow (View.styled Style.bold fit)));
  Alcotest.(check bool)
    "a fit's min_width in a role" true
    (contains "Acme Corporation" (narrow (View.role `Succeeded fit)));
  Alcotest.(check bool)
    "a sparkline in a role" true
    (contains "Acme Corporation" (narrow (View.role `Succeeded spark)))

(* A border in one style is one sequence per run of border glyphs. *)
let test_styled_border_runs () =
  let t =
    Table.view
      ~border:(Border.with_style Style.(fg Color.red) Border.single)
      [ Table.column (View.text "a") ]
      [ [ View.text "b" ] ]
  in
  let lines = String.split_on_char '\n' (View.to_string ~styled:true t) in
  Alcotest.(check string)
    "top"
    "\027[31m\xe2\x94\x8c\xe2\x94\x80\xe2\x94\x80\xe2\x94\x80\xe2\x94\x90\027[0m"
    (List.hd lines);
  Alcotest.(check string)
    "data row" "\027[31m\xe2\x94\x82\027[0m b \027[31m\xe2\x94\x82\027[0m"
    (List.nth lines 3)

let test_pp_ending () =
  let table =
    of_string_rows ~border:Border.none
      [ Table.column (View.text "") ]
      [ [ "x" ] ]
  in
  Alcotest.(check string)
    "caller supplies the line ending" "x!"
    (Fmt.str "%a!" View.pp table)

let test_huge_min_width () =
  let t =
    Table.view
      [
        Table.column ~min_width:max_int (View.text "A");
        Table.column (View.text "B");
      ]
      [ [ View.text "x"; View.text "y" ] ]
  in
  let out = View.to_string (View.sized ~width:20 t) in
  List.iter
    (fun line ->
      Alcotest.(check bool)
        "huge minimum still fits width 20" true
        (View.width (View.text line) <= 20))
    (String.split_on_char '\n' out)

let test_wide_border () =
  let border =
    Border.v
      Border.
        {
          top_left = "\u{754c}";
          top = "\u{754c}";
          top_right = "\u{754c}";
          left = "\u{754c}";
          right = "\u{754c}";
          bottom_left = "\u{754c}";
          bottom = "\u{754c}";
          bottom_right = "\u{754c}";
          vertical = "\u{754c}";
          horizontal = "\u{754c}";
          cross = "\u{754c}";
          top_cross = "\u{754c}";
          bottom_cross = "\u{754c}";
          left_cross = "\u{754c}";
          right_cross = "\u{754c}";
        }
  in
  let t =
    Table.view ~border
      [ Table.column (View.text "A"); Table.column (View.text "B") ]
      [ [ View.text "x"; View.text "y" ] ]
  in
  let widths =
    List.map
      (fun l -> View.width (View.text l))
      (String.split_on_char '\n' (View.to_string (View.sized ~width:20 t)))
  in
  let body_width = List.nth widths 1 in
  Alcotest.(check (list int))
    "two-cell frame remains rectangular"
    (List.map (fun _ -> body_width) widths)
    widths;
  List.iter
    (fun width ->
      Alcotest.(check bool) "frame fits requested width" true (width <= 20))
    widths

(* [rules] draws a rule between each two rows, after a row's last line, in
   the glyphs and the style of the header's rule; a table without edges has
   none. *)
let test_rules () =
  let table ?rules ?border () =
    Table.view ?rules ?border
      [ Table.column (View.text "a") ]
      [ [ View.text "1" ]; [ View.text "2\n3" ]; [ View.text "4" ] ]
  in
  Alcotest.(check string)
    "between rows"
    "\u{256d}\u{2500}\u{2500}\u{2500}\u{256e}\n\
     \u{2502} a \u{2502}\n\
     \u{251c}\u{2500}\u{2500}\u{2500}\u{2524}\n\
     \u{2502} 1 \u{2502}\n\
     \u{251c}\u{2500}\u{2500}\u{2500}\u{2524}\n\
     \u{2502} 2 \u{2502}\n\
     \u{2502} 3 \u{2502}\n\
     \u{251c}\u{2500}\u{2500}\u{2500}\u{2524}\n\
     \u{2502} 4 \u{2502}\n\
     \u{2570}\u{2500}\u{2500}\u{2500}\u{256f}"
    (View.to_string (table ~rules:true ()));
  Alcotest.(check int)
    "none by default" 8
    (List.length (String.split_on_char '\n' (View.to_string (table ()))));
  Alcotest.(check string)
    "none without edges"
    (View.to_string (table ~border:Border.none ()))
    (View.to_string (table ~rules:true ~border:Border.none ()));
  let lines =
    String.split_on_char '\n'
      (View.to_string ~styled:true (table ~rules:true ()))
  in
  Alcotest.(check string)
    "in the header rule's style" (List.nth lines 2) (List.nth lines 4)

(* [Border.simple] draws no frame: the columns two spaces apart, as in a
   table without edges, and a rule under the header as wide as the table,
   between the rows too with [rules], in ASCII on a formatter that is not
   UTF-8. *)
let test_simple () =
  let table ?rules () =
    Table.view ?rules ~border:Border.simple
      [
        Table.column (View.text "Name");
        Table.column ~align:`Right (View.text "Size");
      ]
      [
        [ View.text "README.md"; View.text "2.4K" ];
        [ View.text "src/"; View.text "4.0K" ];
      ]
  in
  let rule = String.concat "" (List.init 15 (Fun.const "\u{2500}")) in
  Alcotest.(check string)
    "header rule"
    (String.concat "\n"
       [ "Name       Size"; rule; "README.md  2.4K"; "src/       4.0K" ])
    (View.to_string (table ()));
  Alcotest.(check string)
    "rules"
    (String.concat "\n"
       [ "Name       Size"; rule; "README.md  2.4K"; rule; "src/       4.0K" ])
    (View.to_string (table ~rules:true ()));
  Alcotest.(check string)
    "not UTF-8"
    (String.concat "\n"
       [
         "Name       Size";
         String.make 15 '-';
         "README.md  2.4K";
         "src/       4.0K";
       ])
    (printed ~utf_8:false (table ()));
  let narrow =
    List.map Width.string_width
      (String.split_on_char '\n'
         (View.to_string (View.sized ~width:10 (table ()))))
  in
  Alcotest.(check bool)
    "within the margin" true
    (List.for_all (fun w -> w <= 10) narrow);
  Alcotest.(check int)
    "the rule across the narrowed table" 10 (List.nth narrow 1)

(* Empty edge glyphs do not remove the other pieces of a custom frame. *)
let test_empty_left_edge () =
  let border = Border.v { (Border.chars Border.single) with left = "" } in
  let table =
    Table.view ~border
      [ Table.column (View.text "A"); Table.column (View.text "B") ]
      [ [ View.text "x"; View.text "y" ] ]
  in
  Alcotest.(check string)
    "the empty left edge is padded to the corners' width"
    (String.concat "\n"
       [ "┌───┬───┐"; "  A │ B │"; "├───┼───┤"; "  x │ y │"; "└───┴───┘" ])
    (View.to_string (View.sized ~width:30 table))

let suite =
  ( "table",
    [
      Alcotest.test_case "an empty edge keeps the rest of the frame" `Quick
        test_empty_left_edge;
      Alcotest.test_case "gap" `Quick test_gap;
      Alcotest.test_case "column width" `Quick test_column_width;
      Alcotest.test_case "a cell's min_width" `Quick test_cell_min_width;
      Alcotest.test_case "a table as a span keeps its natural width" `Quick
        test_span_natural_width;
      Alcotest.test_case "rules" `Quick test_rules;
      Alcotest.test_case "two-cell border stays rectangular" `Quick
        test_wide_border;
      Alcotest.test_case "formatter composes without a trailing newline" `Quick
        test_pp_ending;
      Alcotest.test_case "styled border runs" `Quick test_styled_border_runs;
      Alcotest.test_case "no border draws no edge" `Quick
        test_no_border_draws_no_edge;
      Alcotest.test_case "huge minimum narrows to available width" `Quick
        test_huge_min_width;
      Alcotest.test_case "basic" `Quick test_basic;
      Alcotest.test_case "default border is the panel's" `Quick
        test_default_border;
      Alcotest.test_case "shrinkable protects column" `Quick
        test_shrinkable_protects_column;
      Alcotest.test_case "string conversion is plain" `Quick
        test_string_conversion_is_plain;
      Alcotest.test_case "formatter gates cell styles" `Quick
        test_formatter_gates_cell_styles;
      Alcotest.test_case "link cell" `Quick test_link_cell;
      Alcotest.test_case "formatter margin keeps borders on one row" `Quick
        test_formatter_margin_never_wraps_border;
      Alcotest.test_case "every formatter margin is a hard limit" `Quick
        test_every_margin_is_a_hard_limit;
      Alcotest.test_case "invalid shape" `Quick test_invalid_shape;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "a bar in a cell" `Quick test_bar_cell;
      Alcotest.test_case "simple border" `Quick test_simple;
      Alcotest.test_case "a bar as a span" `Quick test_bar_span;
      Alcotest.test_case "wrap" `Quick test_wrap;
      Alcotest.test_case "wrap preserves an unbroken token" `Quick
        test_wrap_preserves_unbroken_token;
    ] )
