(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Guide" (module Console.Guide : Surface);
  exposed "Rule" (module Console.Rule : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface)

(* [sanitize] is the widgets' rule over a plain string: malformed UTF-8 is the
   replacement glyph, and no control moves the cursor. *)
let test_sanitize () =
  Alcotest.(check string)
    "a tab, a newline and an escape all become spaces" "a b c d"
    (Console_text.sanitize ~keep_newlines:false "a\tb\nc\027d");
  Alcotest.(check string)
    "a newline is a row boundary, so it is kept by default" "a b\nc d"
    (Console_text.sanitize "a\tb\nc\027d");
  Alcotest.(check string)
    "a malformed sequence becomes the replacement glyph" "a\u{FFFD}b"
    (Console_text.sanitize "a\255b");
  Alcotest.(check string)
    "a C1 control, which UTF-8 hides behind two bytes, becomes a space" "a b"
    (Console_text.sanitize "a\u{009B}b");
  Alcotest.(check string)
    "DEL becomes a space" "a b"
    (Console_text.sanitize "a\127b");
  Alcotest.(check string)
    "well-formed text, wide glyphs included, is returned unchanged"
    "\u{4E16}\u{754C} ok"
    (Console_text.sanitize "\u{4E16}\u{754C} ok")

(* SGR (ECMA-48 8.3.117) is the one control sequence that cannot move the
   cursor, erase a region or name a resource, so the only one [~keep_styles]
   forwards; its parameter bytes are the digits and ';' alone. *)
let styles s = Console_text.strip ~keep_styles:true s

(* Everything that is not a strictly-parsed SGR is removed, as
   {!Console_text.strip} removes it, and one scanner decides that for both. *)
let neutralised what s =
  Alcotest.(check string) what (Console_text.strip s) (styles s)

let test_keep_styles_passes_sgr () =
  Alcotest.(check string)
    "a colour and its reset survive byte for byte" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "so does a 24-bit colour, which is eleven numeric parameters"
    "\027[38;2;255;0;0mred\027[0m"
    (styles "\027[38;2;255;0;0mred\027[0m");
  Alcotest.(check string)
    "an empty parameter list is SGR 0 and survives" "\027[mplain"
    (styles "\027[mplain");
  Alcotest.(check string)
    "a control inside styled text still becomes a space" "\027[31ma b\027[0m"
    (styles "\027[31ma\tb\027[0m");
  Alcotest.(check string)
    "and a newline still goes when it is not wanted" "\027[31ma b\027[0m"
    (Console_text.strip ~keep_newlines:false ~keep_styles:true
       "\027[31ma\nb\027[0m")

(* The parameter run is bounded, because an unbounded scan over untrusted bytes
   is a scan whoever wrote the bytes sizes. *)
let test_keep_styles_bounds_the_parameters () =
  let params n = String.make n '1' in
  Alcotest.(check string)
    "sixty-four parameter bytes are within the bound"
    ("\027[" ^ params 64 ^ "mx\027[0m")
    (styles ("\027[" ^ params 64 ^ "mx\027[0m"));
  neutralised "sixty-five are not, and neutralise" ("\027[" ^ params 65 ^ "mx");
  neutralised "nor does a sequence the string ends in the middle of"
    "tail\027[31"

let test_keep_styles_neutralises_the_rest () =
  neutralised "erase in display moves nothing through" "\027[2Jgone";
  neutralised "nor does cursor position" "\027[10;20Hgone";
  neutralised "nor a private parameter byte, whatever its final byte"
    "\027[?25lgone";
  neutralised "nor a colon sub-parameter, which is not the accepted grammar"
    "\027[38:2:255:0:0mred";
  neutralised "an OSC 8 hyperlink is a spoofing surface, not styling"
    "\027]8;;https://example.invalid/\027\\click\027]8;;\027\\";
  neutralised "a device control string carries whatever it likes"
    "\027Pq#0;2;0;0;0\027\\";
  neutralised "an escape that is not a control sequence at all" "\027(Bgone";
  neutralised "the C1 introducer does not open an SGR" "\u{009B}31mred"

(* Styled text leaves no style on after it: a sequence whose last parameter is
   not 0, or empty, is closed. *)
let test_keep_styles_closes_what_it_opens () =
  Alcotest.(check string)
    "an unterminated colour is closed" "\027[31mred\027[0m"
    (styles "\027[31mred");
  Alcotest.(check string)
    "so is one whose last parameter sets an attribute" "\027[0;31mred\027[0m"
    (styles "\027[0;31mred");
  Alcotest.(check string)
    "a trailing reset is not doubled" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "nor is a reset that arrives as the last parameter" "\027[31;0mred"
    (styles "\027[31;0mred");
  let s = "\027[31mred\027[2Jgone\tand\u{009B}31m" in
  Alcotest.(check string)
    "and sanitizing twice changes nothing the first pass did not" (styles s)
    (styles (styles s))

open Console

let lines s = String.split_on_char '\n' s
let side ?gap spans = View.to_string (View.hcat ?gap spans)
let text = View.text

(* [hcat] puts spans side by side, [gap] spaces apart, each padded to its
   widest line and a shorter one with blank lines, a line ending at its last
   span that has text there, and a span with no text left out. *)
let test_hcat () =
  Alcotest.(check string)
    "side by side" "a    1\nbbb  2\n     3"
    (side [ text "a\nbbb"; text "1\n2\n3" ]);
  Alcotest.(check string)
    "no trailing space" "aa 1\nb\nc"
    (side ~gap:1 [ text "aa\nb\nc"; text "1" ]);
  Alcotest.(check string)
    "a blank line" "a  1\n   2"
    (side [ text "a\n"; text "1\n2" ]);
  Alcotest.(check string) "one span" "a\nb" (side [ text "a\nb" ]);
  Alcotest.(check string) "none" "" (side []);
  Alcotest.(check string)
    "an empty span takes no gap" "a  b"
    (side [ text "a"; View.empty; text "b" ]);
  let panel = Panel.view (View.text "p") and tree = Tree.v (View.text "t") [] in
  let panel_lines = lines (View.to_string panel) in
  Alcotest.(check (list string))
    "widgets"
    [
      List.nth panel_lines 0 ^ "  t";
      List.nth panel_lines 1;
      List.nth panel_lines 2;
    ]
    (lines (side [ panel; Tree.view tree ]));
  Display_harness.check_invalid "Console.View.hcat: negative gap" (fun () ->
      ignore (View.hcat ~gap:(-1) [] : View.t))

(* Each widget is as wide as the width it is given, and each span keeps
   its colours. *)
let test_hcat_widths () =
  let rule = View.sized ~width:9 (Rule.view ()) in
  Alcotest.(check (list int))
    "two rules of 9 cells, 3 apart" [ 21 ]
    (List.map Width.string_width (lines (side ~gap:3 [ rule; rule ])));
  let red = View.styled (Style.fg Color.red) (View.text "red")
  and blue = View.styled (Style.fg Color.blue) (View.text "blue") in
  Alcotest.(check string)
    "colours"
    (View.to_string ~styled:true red ^ "  " ^ View.to_string ~styled:true blue)
    (View.to_string ~styled:true (View.hcat [ red; blue ]))

(* Spans side by side are a span: they go in a panel, as any span does, in
   the theme of the formatter that prints it. *)
let test_hcat_nests () =
  let tree = Tree.v (View.text "src") [ Tree.v (View.text "lib") [] ] in
  let inner = View.hcat [ Tree.view tree; View.text "2 files" ] in
  let want = [ "src      2 files"; "\u{2514}\u{2500}\u{2500} lib" ] in
  let panel = Panel.view ~border:Border.ascii inner in
  Alcotest.(check (list string))
    "in a panel"
    ([ "+------------------+" ]
    @ List.map
        (fun l -> "| " ^ l ^ String.make (16 - Width.string_width l) ' ' ^ " |")
        want
    @ [ "+------------------+" ])
    (lines (View.to_string panel))

(* [vcat] puts spans one under the other; a span with no text takes no
   line, and [height] counts the lines. *)
let test_vcat () =
  Alcotest.(check string)
    "one under the other" "a\nbb\nc"
    (View.to_string (View.vcat [ text "a\nbb"; View.empty; text "c" ]));
  Alcotest.(check string) "none" "" (View.to_string (View.vcat []));
  Alcotest.(check string)
    "a line feed is two blank lines" "a\n\n\nb"
    (View.to_string (View.vcat [ text "a"; text "\n"; text "b" ]));
  Alcotest.(check (list int))
    "heights" [ 0; 1; 2; 2; 3 ]
    (List.map View.height
       [
         View.empty;
         text "a";
         text "a\nb";
         text "\n";
         View.vcat [ text "a"; text "b\nc" ];
       ]);
  let panel = Panel.view (View.text "p") and rule = Rule.view () in
  Alcotest.(check (list string))
    "widgets"
    (lines (View.to_string panel)
    @ [ View.to_string (View.sized ~width:5 rule) ])
    (lines (View.to_string (View.vcat [ panel; View.sized ~width:5 rule ])))

(* [shown theme ~margin pp] is what [pp] prints on a formatter in [theme] at
   [margin], without colours. *)
let shown ?(utf_8 = true) theme ~margin pp =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Fmt.set_utf_8 ppf utf_8;
  Format.pp_set_margin ppf margin;
  pp ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let steps =
  Table.view
    [ Table.column (View.text "step"); Table.column (View.text "time") ]
    [
      [ View.text "fetch"; View.text "1.2s" ];
      [ View.text "build the whole tree"; View.text "7.4s" ];
    ]

(* A widget as a span is laid out where it is printed, in that formatter's
   theme and UTF-8, at the width it was given, as its [pp] lays it out. *)
let test_to_span () =
  let tree = Tree.v (View.text "src") [ Tree.v (View.text "lib") [] ] in
  let panel =
    Panel.view ~title:(View.text "t") (View.text "a long line of text")
  in
  let rule = Rule.view ~label:(View.text "r") () in
  List.iter
    (fun (theme, utf_8) ->
      let same what ~width pp span =
        Alcotest.(check string)
          (Theme.name theme ^ ": " ^ what)
          (shown ~utf_8 theme ~margin:width pp)
          (shown ~utf_8 theme ~margin:200 (fun ppf -> View.pp ppf span))
      in
      List.iter
        (fun width ->
          same "a table" ~width
            (fun ppf -> View.pp ppf steps)
            (View.sized ~width steps);
          same "a tree" ~width
            (fun ppf -> View.pp ppf (Tree.view tree))
            (View.sized ~width (Tree.view tree));
          same "a panel" ~width
            (fun ppf -> View.pp ppf panel)
            (View.sized ~width panel);
          same "a rule" ~width
            (fun ppf -> View.pp ppf rule)
            (View.sized ~width rule))
        [ 12; 20; 40 ])
    [
      (Theme.default, true);
      (Theme.dos, true);
      (Theme.ascii, true);
      (Theme.default, false);
    ];
  Alcotest.(check string)
    "natural width" (View.to_string steps) (View.to_string steps)

(* Widgets nest as spans: a table in a panel in a table cell, drawn in the
   theme of the formatter that prints the outermost. *)
let test_nested () =
  let inner = Panel.view steps in
  let outer =
    Table.view
      [ Table.column (View.text "a"); Table.column (View.text "panel") ]
      [ [ View.text "x"; inner ] ]
  in
  let lines s = String.split_on_char '\n' s in
  let panel = lines (View.to_string inner) in
  let got = lines (View.to_string outer) in
  List.iter
    (fun line ->
      Alcotest.(check bool)
        ("the panel's line " ^ line)
        true
        (List.exists (Astring.String.is_infix ~affix:line) got))
    panel;
  let dos = shown Theme.dos ~margin:200 (fun ppf -> View.pp ppf outer) in
  let corners = List.length (Astring.String.cuts ~sep:"\u{2554}" dos) - 1 in
  Alcotest.(check int) "three frames in the printing theme" 3 corners

(* A theme whose frame takes two cells lays a widget out wider than the
   default one: a view is laid out in the theme it is printed in, each of its
   lines as wide as the others. *)
let test_to_span_cells () =
  let wide =
    Theme.v
      ~border:
        (Border.v
           {
             (Border.chars Border.single) with
             left = "\u{2502}\u{2502}";
             right = "\u{2502}\u{2502}";
             top_left = "\u{250c}\u{2500}";
             top_right = "\u{2500}\u{2510}";
             bottom_left = "\u{2514}\u{2500}";
             bottom_right = "\u{2500}\u{2518}";
             left_cross = "\u{251c}\u{2500}";
             right_cross = "\u{2500}\u{2524}";
           })
      ()
  in
  let span = steps in
  let widths s = List.map Width.string_width (String.split_on_char '\n' s) in
  Alcotest.(check (list int))
    "two cells wider on each line"
    (List.map (fun w -> w + 2) (widths (View.to_string steps)))
    (widths (shown wide ~margin:200 (fun ppf -> View.pp ppf span)))

(* A span of no cell is still text: here its accent belongs to the span
   before it when there is no gap. *)
let test_hcat_combining_suffix () =
  Alcotest.(check string)
    "the accent survives composition" "e\u{0301}"
    (side ~gap:0 [ text "e"; text "\u{0301}" ])

(* [pp] at a margin of [margin], in colour iff [styled], its lines. *)
let lines ?(styled = false) margin pp =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  if styled then begin
    Fmt.set_style_renderer ppf `Ansi_tty;
    Console.Color.set_depth ppf `True_color
  end;
  pp ppf ();
  Format.pp_print_flush ppf ();
  String.split_on_char '\n' (Buffer.contents buf)

let check_fits name margin pp =
  List.iter
    (fun l ->
      if Width.string_width l > margin then
        Alcotest.failf "%s: %d cells at a margin of %d: %S" name
          (Width.string_width l) margin l)
    (lines margin pp)

(* Cases the properties of widgets put together found, and nested widgets
   each guarantee holds in. *)
let test_put_together () =
  let open Console in
  let emoji =
    "1\u{fe0f}\u{20e3}a\u{fe0f} x\u{1f44d}\u{1f3fd}x\u{940}\u{2764}\u{fe0f}"
  in
  (* A gradient wrapped in a cell, cut where its drawing has the cells its
     text is counted at. *)
  check_fits "a gradient in a cell" 14
    (Fmt.const View.pp
       (Table.view
          [ Table.column (View.text "a"); Table.column (View.text "b") ]
          [ [ View.gradient (View.text (emoji ^ " " ^ emoji)); View.text "x" ] ]));
  (* A panel's hanging lines have less room than its first one. *)
  check_fits "a narrow panel" 4
    (Fmt.const View.pp
       (Panel.view ~border:Border.ascii ~padding:0
          (View.text
             "\u{1f468}\u{200d}\u{1f469}1\u{fe0f}\u{20e3}x 1\u{fe0f}\u{20e3}")));
  (* A table and a tree side by side, each at half of less room than they
     need. *)
  let half m = max 1 ((m - 1) / 2) in
  let side_by_side m =
    Fmt.const View.pp
      (View.hcat ~gap:1
         [
           View.sized ~width:(half m)
             (Table.view
                [ Table.column (View.text "h") ]
                [ [ View.text "\u{754c}\u{754c}" ] ]);
           View.sized ~width:(half m)
             (Tree.view
                (Tree.v (View.text "root") [ Tree.v (View.text "leaf") [] ]));
         ])
  in
  List.iter
    (fun m -> check_fits "a table by a tree" m (side_by_side m))
    [ 3; 5; 9 ];
  (* In colour, a panel with a gradient title by a rule with a bar in its
     label is its plain text with escape codes. *)
  let scene m =
    Fmt.const View.pp
      (View.hcat
         [
           View.sized
             ~width:(half (m - 1))
             (Panel.view
                ~title:(View.gradient (View.text "title"))
                (View.text emoji));
           View.sized
             ~width:(half (m - 1))
             (Rule.view ~label:(Bar.horizontal ~width:4 0.5) ());
         ])
  in
  List.iter
    (fun m ->
      check_fits "a panel by a rule" m (scene m);
      Alcotest.(check (list string))
        "plain is styled without escapes"
        (lines m (scene m))
        (List.map Console_text.strip (lines ~styled:true m (scene m))))
    [ 4; 20; 60 ]

(* The ASCII look draws a three-cell ellipsis. Splitting a deferred widget
   into spans must keep all three dots, even where its UTF-8 layout had a
   space after a one-cell ellipsis. *)
let test_nested_wrap_ascii () =
  let open Console in
  let rule =
    View.sized ~width:4 (Rule.view ~label:(View.text "\u{754c}\n\u{754c}") ())
  in
  let source = Table.view [ Table.column (View.text "h") ] [ [ rule ] ] in
  let render span =
    shown ~utf_8:false Theme.default ~margin:80 (fun ppf -> View.pp ppf span)
  in
  let compact s =
    String.to_seq s
    |> Seq.filter (fun c -> c <> ' ' && c <> '\n')
    |> String.of_seq
  in
  let expected = "+------+|h|+------+|...|+------+" in
  Alcotest.(check string)
    "the unwrapped widget has three ellipsis dots" expected
    (compact (render source));
  let pieces = View.wrap 2 source in
  Alcotest.(check string)
    "wrapping preserves every non-space character" expected
    (compact (String.concat "" (List.map render pieces)))

let suite =
  ( "console",
    [
      Alcotest.test_case "wrapping nested widgets keeps an ASCII ellipsis"
        `Quick test_nested_wrap_ascii;
      Alcotest.test_case "side by side keeps a combining suffix" `Quick
        test_hcat_combining_suffix;
      Alcotest.test_case "public surface" `Quick test_public_surface;
      Alcotest.test_case "widgets put together" `Quick test_put_together;
      Alcotest.test_case "side by side" `Quick test_hcat;
      Alcotest.test_case "one under the other" `Quick test_vcat;
      Alcotest.test_case "widgets as views" `Quick test_to_span;
      Alcotest.test_case "widgets nest as views" `Quick test_nested;
      Alcotest.test_case "a widget is laid out in its theme" `Quick
        test_to_span_cells;
      Alcotest.test_case "side by side goes in a panel" `Quick test_hcat_nests;
      Alcotest.test_case "side by side, the widths given" `Quick
        test_hcat_widths;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "strip ~keep_styles passes SGR" `Quick
        test_keep_styles_passes_sgr;
      Alcotest.test_case "strip ~keep_styles bounds the parameters" `Quick
        test_keep_styles_bounds_the_parameters;
      Alcotest.test_case "strip ~keep_styles neutralises the rest" `Quick
        test_keep_styles_neutralises_the_rest;
      Alcotest.test_case "strip ~keep_styles closes what it opens" `Quick
        test_keep_styles_closes_what_it_opens;
    ] )
