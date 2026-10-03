(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let test_ascii () =
  let width = Width.string_width "hello" in
  Alcotest.(check int) "ASCII width" 5 width

let test_empty () =
  let width = Width.string_width "" in
  Alcotest.(check int) "empty width" 0 width

let test_malformed_utf8 () =
  Alcotest.(check int) "replacement cell" 1 (Width.string_width "\xff");
  Alcotest.(check string)
    "truncate emits replacement" "\xef\xbf\xbd" (Width.truncate 1 "\xff")

let test_spacing_combining_mark () =
  Alcotest.(check int)
    "spacing combining mark attaches to preceding cell" 1
    (Width.string_width "x\xe0\xa5\x80")

let test_ansi_ignored () =
  let styled = "\027[1mhello\027[0m" in
  let width = Width.string_width styled in
  Alcotest.(check int) "ANSI codes ignored" 5 width

let test_ansi_does_not_break_grapheme () =
  let combining = "\xea\xa8\xb4" in
  Alcotest.(check int)
    "styled cluster has the same width"
    (Width.string_width ("x" ^ combining))
    (Width.string_width ("x\027[31m" ^ combining ^ "\027[0m"))

let test_osc_ignored () =
  let link = "\027]8;;https://example.com\027\\docs\027]8;;\027\\" in
  Alcotest.(check int) "OSC ignored" 4 (Width.string_width link);
  Alcotest.(check string)
    "OSC preserved by truncation"
    "\027]8;;https://example.com\027\\do\027]8;;\027\\" (Width.truncate 2 link)

(* A value cut to fit reads as a value that wraps unless the cut is marked, and
   a mark at one end throws away the other: a hash is identified by its leading
   characters and a path named by its trailing ones. The mark goes in the
   middle, and the result still fits. *)
let digest =
  "sha256:a376ef543f48c9b17ed2b1cbe32b4d1e6a5f0c9d8e7b6a5948372615f0e1d2c3"

let test_ellipsize_keeps_both_ends () =
  let cut = Width.ellipsize 21 digest in
  Alcotest.(check string)
    "the middle is what goes" "sha256:a37\xe2\x80\xa615f0e1d2c3" cut;
  Alcotest.(check int) "and the result fits" 21 (Width.string_width cut)

(* Where the cut falls is the caller's, because which end carries the identity
   is: a hash is read from its head, so [`End] keeps the head alone and says at
   the right-hand edge that there was more. The two forms of the same value
   agree on nothing past the head. *)
let test_ellipsize_at_end () =
  let cut = Width.ellipsize ~at:`End 21 digest in
  Alcotest.(check string)
    "the tail is what goes" "sha256:a376ef543f48c\xe2\x80\xa6" cut;
  Alcotest.(check int) "and the result fits" 21 (Width.string_width cut);
  Alcotest.(check bool)
    "the middle form of the same value differs" false
    (String.equal cut (Width.ellipsize 21 digest));
  Alcotest.(check string)
    "the default is still the middle"
    (Width.ellipsize 21 digest)
    (Width.ellipsize ~at:`Middle 21 digest)

(* An end cut closes what it left open, exactly as a middle cut does: the reset
   stands between the kept head and the ellipsis, so the mark is not drawn in
   the value's style and nothing after the cut inherits it. *)
let test_ellipsize_at_end_closes_style () =
  let cut = Width.ellipsize ~at:`End 5 "\027[1mabcdefghij\027[0m" in
  Alcotest.(check int) "styling occupies no column" 5 (Width.string_width cut);
  Alcotest.(check string)
    "the style is closed before the mark" "\027[1mabcd\027[0m\xe2\x80\xa6" cut

let test_ellipsize_short_enough () =
  Alcotest.(check string)
    "a value that fits is untouched" "abc" (Width.ellipsize 3 "abc");
  Alcotest.(check string)
    "and so is a shorter one" "abc" (Width.ellipsize 8 "abc")

let test_ellipsize_no_room () =
  Alcotest.(check string)
    "nothing fits in nothing" "" (Width.ellipsize 0 digest);
  Alcotest.(check string)
    "one column says only that it was cut" "\xe2\x80\xa6"
    (Width.ellipsize 1 digest)

let test_ellipsize_ansi_costs_nothing () =
  let styled = "\027[1mabcdefghij\027[0m" in
  let cut = Width.ellipsize 5 styled in
  Alcotest.(check int) "styling occupies no column" 5 (Width.string_width cut);
  Alcotest.(check bool)
    "the head's style is closed before the mark" true
    (String.starts_with ~prefix:"\027[1mab\027[0m\xe2\x80\xa6" cut)

(* A row that does not fit is shortened by words, never
   by characters, and a path, an id or a digest is always printed whole. *)
let waiting = "Target: waiting for op-hello's boot verdict (300s bound)"

let test_shorten_drops_words_from_the_end () =
  Alcotest.(check string)
    "the bracketed group goes as one word"
    "Target: waiting for op-hello's boot verdict" (Width.shorten 50 waiting);
  Alcotest.(check string)
    "and then the words before it, never ending on one that leads into what \
     went"
    "Target: waiting" (Width.shorten 22 waiting);
  Alcotest.(check string)
    "nor on the colon that introduced it" "Target" (Width.shorten 10 waiting);
  Alcotest.(check string)
    "a value that fits is untouched" waiting
    (Width.shorten 200 waiting);
  Alcotest.(check string)
    "a separator left at the end goes with its word" "kept 76 of 96 frames"
    (Width.shorten 22 "kept 76 of 96 frames, dropped 20")

let test_shorten_never_cuts_a_word () =
  Alcotest.(check string)
    "a digest is kept whole or not at all" "" (Width.shorten 21 digest);
  Alcotest.(check string)
    "and so is a path after a word" "pushed"
    (Width.shorten 40 ("pushed " ^ digest))

let test_shorten_closes_a_style () =
  let styled = "\027[1mone two three\027[0m" in
  Alcotest.(check string)
    "the style open at the cut is closed" "\027[1mone two\027[0m"
    (Width.shorten 9 styled)

let test_fold_breaks_at_spaces () =
  Alcotest.(check (list string))
    "each line fits, broken where a space was"
    [ "Target: waiting for"; "op-hello's boot"; "verdict (300s bound)" ]
    (Width.fold 20 waiting);
  Alcotest.(check (list string))
    "a value that fits is one line" [ waiting ] (Width.fold 80 waiting)

let test_fold_keeps_a_word_whole () =
  Alcotest.(check (list string))
    "a digest wider than the line stands whole on its own"
    [ "pushed"; digest; "to the registry" ]
    (Width.fold 20 ("pushed " ^ digest ^ " to the registry"));
  let split = Width.fold ~split:true 20 digest in
  Alcotest.(check string)
    "split, every character is still printed" digest (String.concat "" split);
  List.iter
    (fun line ->
      Alcotest.(check bool)
        "and every line fits" true
        (Width.string_width line <= 20))
    split

let test_fold_reopens_a_style () =
  Alcotest.(check (list string))
    "a style open at a break is closed and opened again"
    [ "\027[1mone\027[0m"; "\027[1mtwo\027[0m" ]
    (Width.fold 4 "\027[1mone two\027[0m")

let test_wrap_basic () =
  let result = Width.wrap 20 "hello world foo bar baz" in
  Alcotest.(check string) "wraps at width" "hello world foo bar\nbaz" result

let test_wrap_with_indent () =
  let result = Width.wrap ~indent:2 20 "hello world foo bar baz" in
  Alcotest.(check string)
    "wraps with indent" "  hello world foo\n  bar baz" result

let test_wrap_short () =
  let result = Width.wrap 80 "short text" in
  Alcotest.(check string) "no wrapping needed" "short text" result

let test_wrap_single_long_word () =
  let result = Width.wrap 5 "superlongword next" in
  Alcotest.(check string) "long word on own line" "superlongword\nnext" result

(* A backquoted run is a command the reader copies back whole, so a row never
   ends inside one; one wider than the row takes a row of its own. *)
let test_wrap_keeps_backquoted_run () =
  Alcotest.(check string)
    "run on one row" "use\n`space run a/b.yaml`,\nthen go"
    (Width.wrap 12 "use `space run a/b.yaml`, then go")

(* A word that opens and closes its own run groups nothing after it. *)
let test_wrap_closed_backquote () =
  Alcotest.(check string)
    "closed in one word" "`a`\nb c\n`d`"
    (Width.wrap 4 "`a` b c `d`")

(* A backquote that never closes groups nothing, and every space is a break. *)
let test_wrap_unclosed_backquote () =
  Alcotest.(check string) "unclosed" "a `b c\nd e" (Width.wrap 6 "a `b c d e")

let test_wrap_multiline_input () =
  let result = Width.wrap 20 "line one\nline two\nline three" in
  Alcotest.(check string)
    "normalizes newlines" "line one line two\nline three" result

let test_wrap_empty () =
  let result = Width.wrap 20 "" in
  Alcotest.(check string) "empty string" "" result

let test_wrap_uses_terminal_cells () =
  let wide u = if Uchar.to_int u = 0x754c then 2 else 1 in
  Alcotest.(check string)
    "uses terminal cells" "界 a"
    (Width.wrap ~char_width:wide 4 "界 a")

(* Grapheme clusters measure as a terminal renders them, not as the sum of their
   code points: a ZWJ emoji sequence (a family) is one 2-wide cell, not 6.
   Over-counting made the display and tables reserve too much space. *)
let test_zwj_emoji_width () =
  let width = Width.string_width in
  Alcotest.(check int)
    "ZWJ family is 2 wide" 2
    (width
       "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7");
  Alcotest.(check int) "plain emoji is 2 wide" 2 (width "\xf0\x9f\x8e\x89");
  Alcotest.(check int) "combining mark adds nothing" 1 (width "e\xcc\x81")

let suite =
  ( "width",
    [
      Alcotest.test_case "ASCII" `Quick test_ascii;
      Alcotest.test_case "zwj emoji width" `Quick test_zwj_emoji_width;
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "malformed UTF-8" `Quick test_malformed_utf8;
      Alcotest.test_case "spacing combining mark" `Quick
        test_spacing_combining_mark;
      Alcotest.test_case "ANSI ignored" `Quick test_ansi_ignored;
      Alcotest.test_case "ANSI preserves grapheme" `Quick
        test_ansi_does_not_break_grapheme;
      Alcotest.test_case "OSC ignored" `Quick test_osc_ignored;
      Alcotest.test_case "ellipsize keeps both ends" `Quick
        test_ellipsize_keeps_both_ends;
      Alcotest.test_case "ellipsize at the end" `Quick test_ellipsize_at_end;
      Alcotest.test_case "ellipsize at the end closes a style" `Quick
        test_ellipsize_at_end_closes_style;
      Alcotest.test_case "ellipsize leaves a short value" `Quick
        test_ellipsize_short_enough;
      Alcotest.test_case "ellipsize with no room" `Quick test_ellipsize_no_room;
      Alcotest.test_case "ellipsize counts no escape" `Quick
        test_ellipsize_ansi_costs_nothing;
      Alcotest.test_case "shorten drops words from the end" `Quick
        test_shorten_drops_words_from_the_end;
      Alcotest.test_case "shorten never cuts a word" `Quick
        test_shorten_never_cuts_a_word;
      Alcotest.test_case "shorten closes a style" `Quick
        test_shorten_closes_a_style;
      Alcotest.test_case "fold breaks at spaces" `Quick
        test_fold_breaks_at_spaces;
      Alcotest.test_case "fold keeps a word whole" `Quick
        test_fold_keeps_a_word_whole;
      Alcotest.test_case "fold reopens a style" `Quick test_fold_reopens_a_style;
      Alcotest.test_case "wrap basic" `Quick test_wrap_basic;
      Alcotest.test_case "wrap with indent" `Quick test_wrap_with_indent;
      Alcotest.test_case "wrap short" `Quick test_wrap_short;
      Alcotest.test_case "wrap single long word" `Quick
        test_wrap_single_long_word;
      Alcotest.test_case "wrap keeps backquoted run" `Quick
        test_wrap_keeps_backquoted_run;
      Alcotest.test_case "wrap closed backquote" `Quick
        test_wrap_closed_backquote;
      Alcotest.test_case "wrap unclosed backquote" `Quick
        test_wrap_unclosed_backquote;
      Alcotest.test_case "wrap multiline input" `Quick test_wrap_multiline_input;
      Alcotest.test_case "wrap empty" `Quick test_wrap_empty;
      Alcotest.test_case "wrap terminal cells" `Quick
        test_wrap_uses_terminal_cells;
    ] )
