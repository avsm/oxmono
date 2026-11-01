(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness
open View_harness

let strings = List.map View.to_string

let test_text () =
  Alcotest.(check int) "span width" 5 (View.width (View.text "hello"));
  Alcotest.(check int)
    "concat width" 11
    (View.width View.(text "hello" ++ text " world"));
  Alcotest.(check string)
    "plain string" "hello"
    (View.to_string (View.styled Style.bold (View.text "hello")))

(* A span holds only text that prints: a control is a space, malformed UTF-8
   the replacement character, and a line feed starts a line. *)
let test_sanitized () =
  let span = View.styled Style.bold (View.text "a\tb\nc\027[2Jd\255") in
  Alcotest.(check string)
    "controls are spaces" "a b\nc [2Jd\u{FFFD}" (View.to_string span);
  Alcotest.(check bool)
    "no escape from the caller" false
    (contains "[2J\027" (View.to_string ~styled:true span));
  Alcotest.(check int) "widest line" 7 (View.width span)

let test_width_boundaries () =
  let suffix = "\xea\xa8\xb4" in
  let joined = View.text ("x" ^ suffix) in
  let split = View.(text "x" ++ styled Style.bold (text suffix)) in
  Alcotest.(check int)
    "a spacing mark adds a cell to its cluster" 2 (View.width joined);
  Alcotest.(check int) "style changes do not add cells" 2 (View.width split);
  Alcotest.(check int) "wide" 4 (View.width (View.text "界界"))

let test_truncate () =
  Alcotest.(check string)
    "fits" "hello"
    (View.to_string (View.truncate 5 (View.text "hello")));
  Alcotest.(check string)
    "ellipsis" "hel…"
    (View.to_string (View.truncate 4 (View.text "hello")));
  Alcotest.(check string)
    "one cell" "…"
    (View.to_string (View.truncate 1 (View.text "hello")));
  Alcotest.(check string)
    "nothing" ""
    (View.to_string (View.truncate 0 (View.text "hello")));
  Alcotest.(check string)
    "a wide character is not cut" "界…"
    (View.to_string (View.truncate 4 (View.text "界界界")));
  (* A VS16 stays with its letter, at whatever width they take. *)
  let cut = View.truncate 3 (View.text "a\u{fe0f}bcdef") in
  Alcotest.(check bool)
    "a variation selector stays with its letter" true
    (View.width cut <= 3
    && List.mem (View.to_string cut) [ "a\u{fe0f}…"; "a\u{fe0f}b…" ]);
  Alcotest.(check string)
    "every line" "ab…\nc"
    (View.to_string (View.truncate 3 (View.text "abcd\nc")));
  Alcotest.(check string)
    "the style is closed" "\027[1mhe…\027[0m"
    (View.to_string ~styled:true
       (View.truncate 3 (View.styled Style.bold (View.text "hello"))))

let test_crop () =
  let crop ?left ?width s =
    View.to_string (View.crop ?left ?width (View.text s))
  in
  Alcotest.(check string) "a window" "cde" (crop ~left:2 ~width:3 "abcdefg");
  Alcotest.(check string) "from the left" "abc" (crop ~width:3 "abcdefg");
  Alcotest.(check string) "to the end" "efg" (crop ~left:4 "abcdefg");
  Alcotest.(check string) "past the end" "" (crop ~left:9 ~width:3 "abc");
  Alcotest.(check string)
    "every line" "bc\ne"
    (crop ~left:1 ~width:2 "abcd\nde");
  Alcotest.(check string)
    "a wide character cut on the left leaves a blank" " \u{754c}"
    (crop ~left:1 ~width:3 "\u{754c}\u{754c}\u{754c}");
  Alcotest.(check string)
    "and is left out on the right" "\u{754c}"
    (crop ~width:3 "\u{754c}\u{754c}\u{754c}");
  Alcotest.(check string) "no width" "" (crop ~width:0 "abc");
  Alcotest.(check string)
    "styles and links are kept" "\027[1mel\027[0m"
    (View.to_string ~styled:true
       (View.crop ~left:1 ~width:2 (View.styled Style.bold (View.text "hello"))));
  Alcotest.check_raises "a negative left"
    (Invalid_argument "Console.View.crop: negative left") (fun () ->
      ignore (View.crop ~left:(-1) (View.text "x")))

let test_vcat_gap () =
  let a = View.text "a" and b = View.text "b" in
  Alcotest.(check string) "no gap" "a\nb" (View.to_string (View.vcat [ a; b ]));
  Alcotest.(check string)
    "one blank line" "a\n\nb"
    (View.to_string (View.vcat ~gap:1 [ a; View.empty; b ]));
  Alcotest.(check int) "its height" 4 (View.height (View.vcat ~gap:2 [ a; b ]));
  Alcotest.(check string)
    "nothing around one view" "a"
    (View.to_string (View.vcat ~gap:1 [ View.empty; a ]));
  Alcotest.(check string)
    "no line for no text" "a\nb"
    (View.to_string (View.vcat [ a; View.text ""; b ]));
  Alcotest.(check string)
    "a blank line first" "\na\n\nb"
    (View.to_string View.(text "\n" ++ vcat ~gap:1 [ a; b ]));
  Alcotest.(check string)
    "lines, a blank one too" "a\n\nb"
    (View.to_string (View.concat ~sep:(View.text "\n") [ a; View.empty; b ]));
  Alcotest.check_raises "a negative gap"
    (Invalid_argument "Console.View.vcat: negative gap") (fun () ->
      ignore (View.vcat ~gap:(-1) [ a ]))

let test_truncate_preserves_link () =
  let span = View.link ~uri:"https://example.com" (View.text "documentation") in
  let shown = View.truncate 5 span in
  Alcotest.(check string) "visible prefix" "docu…" (View.to_string shown);
  Alcotest.(check bool)
    "link metadata remains" true
    (contains "\027]8;;https://example.com" (View.to_string ~styled:true shown))

let test_wrap () =
  let wrap w s = strings (View.wrap w (View.text s)) in
  Alcotest.(check (list string))
    "words" [ "hello"; "world" ] (wrap 6 "hello world");
  Alcotest.(check (list string))
    "punctuation stays with its word" [ "abc,"; "def" ] (wrap 4 "abc, def");
  Alcotest.(check (list string))
    "spaces inside a line are kept" [ "a   b"; "c" ] (wrap 5 "a   b c");
  Alcotest.(check (list string))
    "leading spaces are kept" [ "  ab"; "cd" ] (wrap 4 "  ab cd");
  Alcotest.(check (list string))
    "a long word goes on" [ "abc"; "def"; "g" ] (wrap 3 "abcdefg");
  Alcotest.(check (list string))
    "after a slash" [ "src/"; "lib/"; "a.ml" ] (wrap 5 "src/lib/a.ml");
  Alcotest.(check (list string)) "empty" [ "" ] (wrap 3 "");
  Alcotest.(check (list string)) "lines" [ "a"; "b" ] (wrap 3 "a\nb");
  Alcotest.check_raises "zero width"
    (Invalid_argument "Console.View.wrap: non-positive width") (fun () ->
      ignore (View.wrap 0 (View.text "a")))

let test_wrap_malformed () =
  Alcotest.(check (list string))
    "each byte sequence remains visible" [ "\u{FFFD}"; "a" ]
    (strings (View.wrap 1 (View.text "\255a")))

let test_wrap_oversized () =
  (* The two-cell glyph cannot fit, but the following ASCII character can. *)
  Alcotest.(check (list string))
    "retain the suffix" [ "a" ]
    (strings (View.wrap 1 (View.text "界a")))

let test_wrap_preserves_styles () =
  let span =
    View.(
      styled Style.(fg Color.red) (text "hello ")
      ++ styled Style.(fg Color.blue) (text "world"))
  in
  let lines = View.wrap 6 span in
  Alcotest.(check (list string))
    "visible lines" [ "hello"; "world" ] (strings lines);
  let ansi = List.map (View.to_string ~styled:true) lines in
  Alcotest.(check bool)
    "red first line" true
    (contains "\027[31m" (List.nth ansi 0));
  Alcotest.(check bool)
    "blue second line" true
    (contains "\027[34m" (List.nth ansi 1))

let test_link () =
  let span =
    View.link ~uri:"https://example.com"
      (View.styled Style.underline (View.text "docs"))
  in
  Alcotest.(check string) "plain label" "docs" (View.to_string span);
  let ansi = View.to_string ~styled:true span in
  Alcotest.(check bool)
    "OSC 8 open" true
    (String.starts_with ~prefix:"\027]8;;https://example.com\027\\" ansi);
  Alcotest.(check bool)
    "OSC 8 close" true
    (String.ends_with ~suffix:"\027]8;;\027\\" ansi);
  (* RFC 3986, section 2.1: a byte that may not appear in a URI is
     percent-encoded, as two upper-case hexadecimal digits. *)
  let target uri =
    let ansi = View.to_string ~styled:true (View.link ~uri (View.text "x")) in
    match String.index_opt ansi '\027' with
    | Some 0 when String.starts_with ~prefix:"\027]8;;" ansi ->
        let rest = String.sub ansi 5 (String.length ansi - 5) in
        String.sub rest 0 (String.index rest '\027')
    | _ -> ""
  in
  let check msg uri expected =
    Alcotest.(check string) msg expected (target uri)
  in
  check "an escape" "https://example.com\027bad" "https://example.com%1Bbad";
  check "a space" "https://example.com/a b" "https://example.com/a%20b";
  check "a BEL" "https://example.com/\007" "https://example.com/%07";
  check "a DEL" "https://example.com/\127" "https://example.com/%7F";
  check "UTF-8" "https://example.com/caf\u{e9}" "https://example.com/caf%C3%A9";
  check "characters RFC 3986 leaves out" "https://example.com/<a|b>{\"^`}\\"
    "https://example.com/%3Ca%7Cb%3E%7B%22%5E%60%7D%5C";
  check "reserved, unreserved and escapes kept"
    "https://u:p@example.com:8/a-b._~c/!$&'()*+,;=?q=[1]#f%20"
    "https://u:p@example.com:8/a-b._~c/!$&'()*+,;=?q=[1]#f%20";
  Alcotest.(check string)
    "an empty URI links nowhere" "x"
    (View.to_string ~styled:true (View.link ~uri:"" (View.text "x")));
  (* A form feed ends a line, as a line feed does: such a label has no text
     to click, and the link is written over the lines that have some. *)
  Alcotest.(check string)
    "a label of line ends links nothing" "\n\n"
    (View.to_string ~styled:true
       (View.link ~uri:"https://x" (View.text "\012\n")));
  Alcotest.(check string)
    "only the lines with text" "\n\027]8;;https://x\027\\a\027]8;;\027\\"
    (View.to_string ~styled:true
       (View.link ~uri:"https://x" (View.text "\012a")))

(* One run of a style is written under one escape code. *)
let test_runs () =
  Alcotest.(check string)
    "merged" "\027[1mab\027[0m"
    (View.to_string ~styled:true
       View.(styled Style.bold (text "a") ++ styled Style.bold (text "b")))

(* The colours of a formatter's depth. *)
let test_depth () =
  let render depth =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Option.iter (Color.set_depth ppf) depth;
    View.pp ppf (View.styled (Style.fg (Color.rgb 255 0 0)) (View.text "x"));
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check string)
    "16 colours by default" "\027[91mx\027[0m" (render None);
  Alcotest.(check string)
    "true colour" "\027[38;2;255;0;0mx\027[0m"
    (render (Some `True_color));
  Alcotest.(check string)
    "256 colours" "\027[38;5;196mx\027[0m" (render (Some `Ansi_256));
  Alcotest.(check string)
    "16 colours" "\027[91mx\027[0m" (render (Some `Ansi_16))

let test_multiline_resets () =
  let output =
    View.to_string ~styled:true (View.styled Style.bold (View.text "a\nb"))
  in
  Alcotest.(check string)
    "close before LF and reopen on the next line"
    "\027[1ma\027[0m\n\027[1mb\027[0m" output

let test_zero_width_blank_lines () =
  Alcotest.(check string)
    "zero width returns empty even for blank lines" ""
    (View.to_string (View.truncate 0 (View.text "\n")))

(* [span] printed on a formatter in [theme] at [depth], in colour iff
   [depth] is given. *)
let printed ?(theme = Theme.default) ?depth span =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Option.iter
    (fun depth ->
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf depth)
    depth;
  View.pp ppf span;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let fg c s = View.styled (Style.fg c) (View.text s)

(* Each grapheme cluster of a line takes the blend of the stops at its
   place, the first cluster the first stop and the last the last one. *)
let test_gradient () =
  let red = Color.rgb 255 0 0 and blue = Color.rgb 0 0 255 in
  let check what want got =
    Alcotest.(check string)
      what
      (printed ~depth:`True_color want)
      (printed ~depth:`True_color got)
  in
  check "three stops of two colours"
    View.(fg red "a" ++ fg (Color.blend red blue 0.5) "b" ++ fg blue "c")
    (View.gradient ~colors:[ red; blue ] (View.text "abc"));
  let green = Color.rgb 0 255 0 in
  check "five clusters, three stops"
    View.(
      concat
        [
          fg red "a";
          fg (Color.blend red green 0.5) "b";
          fg green "c";
          fg (Color.blend green blue 0.5) "d";
          fg blue "e";
        ])
    (View.gradient ~colors:[ red; green; blue ] (View.text "abcde"));
  check "a cluster is one colour"
    View.(fg red "e\u{301}" ++ fg blue "\u{754c}")
    (View.gradient ~colors:[ red; blue ] (View.text "e\u{301}\u{754c}"));
  check "one colour" (fg red "ab")
    (View.gradient ~colors:[ red ] (View.text "ab"));
  check "each line its own gradient"
    View.(fg red "a" ++ fg blue "b" ++ text "\n" ++ fg red "c" ++ fg blue "d")
    (View.gradient ~colors:[ red; blue ] (View.text "ab\ncd"));
  Alcotest.(check int)
    "as wide as its text" 5
    (View.width
       (View.gradient ~colors:[ red; blue ] (View.text "x\u{754c}e\u{301}y")));
  Alcotest.(check string)
    "plain without colour" "x\u{754c}y"
    (printed (View.gradient ~colors:[ red; blue ] (View.text "x\u{754c}y")));
  Alcotest.(check string)
    "sanitized" "a b"
    (View.to_string (View.gradient (View.text "a\027b")));
  Alcotest.(check int) "empty" 0 (View.width (View.gradient (View.text "")))

(* Without colours, a gradient takes the palette of the theme it is printed
   in, at the formatter's depth. *)
let test_gradient_theme () =
  let span = View.gradient (View.text "abcdef") in
  let palette = Theme.palette Theme.rainbow in
  let want =
    View.concat
      (List.mapi (fun i c -> fg c (String.make 1 "abcdef".[i])) palette)
  in
  List.iter
    (fun depth ->
      Alcotest.(check string)
        "the rainbow's palette"
        (printed ~theme:Theme.rainbow ~depth want)
        (printed ~theme:Theme.rainbow ~depth span))
    [ `Ansi_16; `Ansi_256; `True_color ];
  Alcotest.(check string)
    "a theme of one colour"
    (printed ~depth:`True_color (fg (Theme.accent Theme.default) "abcdef"))
    (printed ~depth:`True_color span);
  Alcotest.(check string)
    "plain without colour" "abcdef"
    (printed ~theme:Theme.rainbow span)

(* A role takes its style from the theme of the formatter it is printed on,
   when it is printed. *)
let test_role () =
  let check what theme want got =
    Alcotest.(check string)
      what
      (printed ~theme ~depth:`True_color want)
      (printed ~theme ~depth:`True_color got)
  in
  List.iter
    (fun theme ->
      let name = Theme.name theme in
      check (name ^ " accent") theme
        (fg (Theme.accent theme) "x")
        (View.role `Accent (View.text "x"));
      check (name ^ " ok") theme
        (fg (Theme.succeeded theme) "x")
        (View.role `Succeeded (View.text "x"));
      check (name ^ " warn") theme
        (fg (Theme.warning theme) "x")
        (View.role `Warning (View.text "x"));
      check (name ^ " fail") theme
        (fg (Theme.failed theme) "x")
        (View.role `Failed (View.text "x"));
      check (name ^ " muted") theme
        (View.styled
           (match Theme.muted theme with
           | Some c -> Style.fg c
           | None -> Style.faint)
           (View.text "x"))
        (View.role `Muted (View.text "x"));
      check (name ^ " header") theme
        (View.styled (Theme.header theme) (View.text "x"))
        (View.role `Header (View.text "x")))
    [ Theme.default; Theme.nord; Theme.dos ];
  let nord = Theme.succeeded Theme.nord in
  check "under a style" Theme.nord
    (View.styled Style.(bold + fg nord) (View.text "passed"))
    (View.styled Style.bold (View.role `Succeeded (View.text "passed")));
  check "each line" Theme.nord (fg nord "a\nb")
    (View.role `Succeeded (View.text "a\nb"));
  check "cut" Theme.nord
    (View.truncate 3 (fg nord "abcdef"))
    (View.truncate 3 (View.role `Succeeded (View.text "abcdef")));
  Alcotest.(check string)
    "plain without colour" "passed"
    (printed ~theme:Theme.nord (View.role `Succeeded (View.text "passed")));
  Alcotest.(check string)
    "to_string in the default theme"
    (View.to_string ~styled:true (fg (Theme.succeeded Theme.default) "x"))
    (View.to_string ~styled:true (View.role `Succeeded (View.text "x")));
  Alcotest.(check int)
    "as wide as its text" 3
    (View.width (View.role `Succeeded (View.text "a\u{754c}")));
  Alcotest.(check string)
    "sanitized" "a b"
    (View.to_string (View.role `Failed (View.text "a\027b")))

(* A role laid under a span: its own styles and links win, a line of it at
   a time. *)
let test_with_role () =
  let check what want got =
    Alcotest.(check string)
      what
      (printed ~theme:Theme.nord ~depth:`True_color want)
      (printed ~theme:Theme.nord ~depth:`True_color got)
  in
  let nord = Theme.succeeded Theme.nord in
  check "a role is a role laid under text"
    (View.role `Succeeded (View.text "a\nb"))
    (View.role `Succeeded (View.text "a\nb"));
  check "a link in a role"
    (View.link ~uri:"https://ocaml.org"
       (View.styled (Style.fg nord) (View.text "docs")))
    (View.role `Succeeded
       (View.link ~uri:"https://ocaml.org" (View.text "docs")));
  check "the span's own colour wins"
    View.(
      styled Style.(fg nord) (text "a")
      ++ styled Style.(fg Color.red) (text "b"))
    (View.role `Succeeded
       View.(text "a" ++ styled Style.(fg Color.red) (text "b")));
  Alcotest.(check string)
    "plain without colour" "ab"
    (printed ~theme:Theme.nord
       (View.role `Succeeded View.(text "a" ++ styled Style.bold (text "b"))))

let pp_at margin v =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  View.pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* A [fit] part is laid out in the room it is printed in: the margin under
   [pp], none where it is measured or printed whole. *)
let test_fit_room () =
  let room =
    View.fit (fun r ->
        View.text
          (match Room.width r with
          | Some w -> Fmt.str "%d at %.1f" w (Room.time r)
          | None -> "natural"))
  in
  Alcotest.(check string) "the margin" "30 at 0.0" (pp_at 30 room);
  Alcotest.(check string) "measured" "natural" (View.to_string room);
  Alcotest.(check int) "its natural width" 7 (View.width room);
  Alcotest.(check string)
    "sized" "12 at 0.0"
    (View.to_string (View.sized ~width:12 room));
  Alcotest.(check string)
    "sized within the room" "8 at 0.0"
    (pp_at 8 (View.sized ~width:12 room));
  Alcotest.(check string)
    "at a display's time" "5 at 1.5"
    (at ~width:5 ~time:1.5 room);
  let theme =
    View.fit (fun r ->
        View.text (Fmt.str "%s %b" (Theme.name (Room.theme r)) (Room.utf_8 r)))
  in
  Alcotest.(check string)
    "its theme and character set" "ascii false"
    (at ~theme:Theme.ascii ~utf_8:false ~time:0. theme)

(* A view's size depends on its room, not on its time: each frame of a
   [fit] is cut or padded to its frame at time 0. *)
let test_fit_still_size () =
  let growing =
    View.fit (fun r ->
        View.text (String.make (1 + int_of_float (Room.time r)) '#'))
  in
  Alcotest.(check (list string))
    "each frame as wide as the first" [ "#"; "#"; "#" ]
    (List.map (fun time -> at ~time growing) [ 0.; 1.; 5. ]);
  let shrinking =
    View.fit (fun r ->
        if Room.time r = 0. then View.text "abc\nd" else View.text "x")
  in
  Alcotest.(check string) "padded" "x  \n " (at ~time:2. shrinking)

(* Under a room, the parts of [hcat] that do not grow take what they need,
   and a bar of no width grows to the rest. *)
let test_hcat_grow () =
  let row =
    View.(hcat ~gap:1 [ text "label"; Bar.horizontal 0.5; text "50%" ])
  in
  Alcotest.(check string)
    "the bar takes the rest" "label ━━━━━━━─────── 50%" (pp_at 24 row);
  Alcotest.(check string)
    "at its natural size, ten cells" "label ━━━━━───── 50%" (View.to_string row);
  let two = View.(hcat ~gap:0 [ Bar.horizontal 1.; Rule.view () ]) in
  Alcotest.(check int)
    "two growers share the room" 9
    (View.width (View.text (pp_at 9 two)));
  let styled name style =
    let row =
      View.(
        hcat ~gap:1 [ text "label"; style (Bar.horizontal 0.5); text "50%" ])
    in
    Alcotest.(check string)
      (name ^ " still grows") "label ━━━━━━━─────── 50%" (pp_at 24 row)
  in
  styled "styled" (View.styled Style.bold);
  styled "in a role" (View.role `Succeeded);
  styled "linked" (View.link ~uri:"https://ocaml.org");
  styled "in a gradient" (View.gradient ?colors:None)

(* [place] pads or cuts each line to its box, at its alignment. *)
let test_place () =
  let v = View.text "ab\ncdef" in
  Alcotest.(check string)
    "right" "  ab\ncdef"
    (View.to_string (View.place ~align:`Right v));
  Alcotest.(check string)
    "centred and wider" "  ab  \n cdef "
    (View.to_string (View.place ~align:`Center ~width:6 v));
  Alcotest.(check string)
    "cut" "ab \ncd\u{2026}"
    (View.to_string (View.place ~width:3 v));
  Alcotest.(check (list string))
    "three lines" [ "ab  "; "cdef"; "    " ]
    (String.split_on_char '\n' (View.to_string (View.place ~height:3 v)));
  Alcotest.(check string)
    "one line" "ab  "
    (View.to_string (View.place ~height:1 v))

let test_place_wide_cut () =
  List.iter
    (fun (align, expected) ->
      let placed = View.place ~align ~width:4 (View.text "界界界") in
      let raw = View.to_string View.(placed ++ text "|") in
      let terminal = Console_vte.v ~rows:2 ~cols:12 () in
      Console_vte.feed terminal raw;
      Alcotest.(check (list string))
        "the next field starts after the four-cell box"
        [ expected ^ "|" ]
        (Console_vte.screen terminal);
      Alcotest.(check int) "the box is four cells" 4 (View.width placed))
    [ (`Left, "界… "); (`Center, "界… "); (`Right, " 界…") ]

(* A cut line ends with the ellipsis of the formatter it is printed on. *)
let test_cut_ellipsis () =
  let v = View.text "abcdefgh" in
  let ascii = at ~utf_8:false ~live:false ~time:0. in
  Alcotest.(check string) "truncate, ASCII" "ab..." (ascii (View.truncate 5 v));
  Alcotest.(check string) "place, ASCII" "ab..." (ascii (View.place ~width:5 v));
  Alcotest.(check string)
    "an ASCII theme" "ab..."
    (at ~theme:Theme.ascii ~live:false ~time:0. (View.truncate 5 v));
  Alcotest.(check string)
    "UTF-8" "abcd\u{2026}"
    (at ~live:false ~time:0. (View.truncate 5 v));
  Alcotest.(check string)
    "a line that fits" "abc  "
    (ascii (View.place ~width:5 (View.truncate 5 (View.text "abc"))))

(* A view that reads nothing but its room is laid out once for each room it
   is printed in; one that reads state is laid out each time. *)
let test_cached () =
  let v, calls = laid () in
  ignore (at ~width:4 ~time:0. v);
  ignore (at ~width:4 ~time:1. v);
  Alcotest.(check int) "one room, one layout" 1 !calls;
  ignore (at ~width:5 ~time:1. v);
  Alcotest.(check int) "another room, another" 2 !calls;
  let v, calls = laid ~live:true () in
  ignore (at ~width:4 ~time:0. v);
  ignore (at ~width:4 ~time:0. v);
  Alcotest.(check int) "state is read again" 2 !calls

(* The matrix theme's frame moves in a display that draws in colour, and
   stands still where it is printed once. *)
let test_matrix_moves () =
  let table =
    Table.view [ Table.column (View.text "Name") ] [ [ View.text "Alice" ] ]
  in
  let frame time = at ~theme:Theme.matrix ~time table in
  Alcotest.(check bool) "it moves" true (frame 0.3 <> frame 0.9);
  Alcotest.(check string)
    "printed once, still"
    (at ~live:false ~theme:Theme.matrix ~time:0. table)
    (at ~live:false ~theme:Theme.matrix ~time:0.9 table)

let test_weighted_room () =
  let part c grow =
    View.fit ~grow (fun r ->
        View.text (String.make (Option.value ~default:1 (Room.width r)) c))
  in
  let v = View.hcat ~gap:0 [ part 'a' 2; part 'b' 1 ] in
  let raw = pp_at 11 v in
  let terminal = Console_vte.v ~rows:4 ~cols:11 () in
  Console_vte.feed terminal raw;
  Alcotest.(check int)
    "one terminal row" 0
    (fst (Console_vte.cursor_pos terminal));
  Alcotest.(check int) "all eleven cells shared" 11 (View.width (View.text raw))

(* Weights share the room by their ratio, however large, and a negative
   weight is refused. *)
let test_weights () =
  let part c grow =
    View.fit ~grow (fun r ->
        View.text (String.make (Option.value ~default:1 (Room.width r)) c))
  in
  Alcotest.(check string)
    "huge weights" "aaaaabbbbb"
    (View.to_string
       (View.sized ~width:10
          (View.hcat ~gap:0 [ part 'a' max_int; part 'b' max_int ])));
  Alcotest.(check string)
    "the largest remainder" "aaabbbbbbb"
    (View.to_string
       (View.sized ~width:10 (View.hcat ~gap:0 [ part 'a' 1; part 'b' 2 ])));
  Alcotest.(check string)
    "the first on a tie" "aaaabbbccc"
    (View.to_string
       (View.sized ~width:10
          (View.hcat ~gap:0 [ part 'a' 1; part 'b' 1; part 'c' 1 ])));
  Alcotest.check_raises "negative grow"
    (Invalid_argument "Console.View.fit: negative grow") (fun () ->
      ignore (View.fit ~grow:(-1) (fun _ -> View.empty)))

let suite =
  ( "view",
    [
      Alcotest.test_case "weights share the room" `Quick test_weights;
      Alcotest.test_case "weighted hcat keeps to its room" `Quick
        test_weighted_room;
      Alcotest.test_case "zero-width truncation of blank lines is empty" `Quick
        test_zero_width_blank_lines;
      Alcotest.test_case "multiline styles reset before LF" `Quick
        test_multiline_resets;
      Alcotest.test_case "text" `Quick test_text;
      Alcotest.test_case "sanitized" `Quick test_sanitized;
      Alcotest.test_case "width is independent of styling boundaries" `Quick
        test_width_boundaries;
      Alcotest.test_case "truncate" `Quick test_truncate;
      Alcotest.test_case "crop" `Quick test_crop;
      Alcotest.test_case "vcat gap" `Quick test_vcat_gap;
      Alcotest.test_case "truncate preserves link" `Quick
        test_truncate_preserves_link;
      Alcotest.test_case "wrap" `Quick test_wrap;
      Alcotest.test_case "wrapping replaces malformed UTF-8" `Quick
        test_wrap_malformed;
      Alcotest.test_case "wrapping retains text after an oversized glyph" `Quick
        test_wrap_oversized;
      Alcotest.test_case "wrap preserves styles" `Quick
        test_wrap_preserves_styles;
      Alcotest.test_case "link" `Quick test_link;
      Alcotest.test_case "runs" `Quick test_runs;
      Alcotest.test_case "depth" `Quick test_depth;
      Alcotest.test_case "gradient" `Quick test_gradient;
      Alcotest.test_case "role" `Quick test_role;
      Alcotest.test_case "with_role" `Quick test_with_role;
      Alcotest.test_case "gradient in the theme's palette" `Quick
        test_gradient_theme;
      Alcotest.test_case "a fit part is laid out in its room" `Quick
        test_fit_room;
      Alcotest.test_case "a fit part keeps its still size" `Quick
        test_fit_still_size;
      Alcotest.test_case "hcat shares its room" `Quick test_hcat_grow;
      Alcotest.test_case "place" `Quick test_place;
      Alcotest.test_case "a placed wide cut fills its box" `Quick
        test_place_wide_cut;
      Alcotest.test_case "a cut ends in the formatter's ellipsis" `Quick
        test_cut_ellipsis;
      Alcotest.test_case "a still view is laid out once a room" `Quick
        test_cached;
      Alcotest.test_case "the matrix frame moves" `Quick test_matrix_moves;
    ] )
