(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* What a terminal shows of a string: ECMA-48 (5th ed.) gives the grammar of
   everything else. Each case is [(what, raw, visible)]; a sequence a terminal
   consumes is gone whole, a malformed UTF-8 sequence is the replacement glyph
   and a tab is a space, as {!Console_text.sanitize} makes it. *)
let stripped =
  [
    (* Annex B.1, CURSOR RIGHT by one in 7-bit and 8-bit codes, 01/11 05/11
       03/01 04/03 and 09/11 03/01 04/03; 8.3.16 gives CSI as 09/11, which
       UTF-8 writes as U+009B. Leading zeros and the default are B.1's own. *)
    ("CUF, 7-bit", "a\027[1Cb", "ab");
    ("CUF, 8-bit", "a\u{009B}1Cb", "ab");
    ("CUF, leading zero", "a\027[01Cb", "ab");
    ("CUF, default", "a\u{009B}Cb", "ab");
    (* B.1, SCROLL RIGHT by 28: an intermediate byte 02/00 before F. *)
    ("SR, 7-bit", "a\027[28 Ab", "ab");
    ("SR, 8-bit", "a\u{009B}28 Ab", "ab");
    (* B.1, DEFINE AREA QUALIFICATION: F 06/15 is lowercase. *)
    ("DAQ", "a\027[3;4ob", "ab");
    (* 8.3.117 SGR, the sequence most text carries. *)
    ("SGR", "\027[1;31mred\027[0m", "red");
    (* 5.4 b: a private parameter string (B.2, =3) is still parameters. *)
    ("private parameters", "a\027[?25lb", "ab");
    (* 5.6 and 8.3.89: OSC opens a control string that ST (8.3.143, 09/12 or
       ESC 05/12) closes; xterm's OSC 8 hyperlink is one, and xterm also
       closes an OSC at BEL. *)
    ( "OSC 8 hyperlink",
      "\027]8;;https://example.com\027\\link\027]8;;\027\\",
      "link" );
    ("OSC, BEL", "\027]0;title\007text", "text");
    ("OSC, 8-bit", "\u{009D}0;title\u{009C}text", "text");
    (* 5.6: a command string holds 00/08 to 00/13, so a line feed inside an
       OSC is the OSC's. *)
    ("OSC over a line feed", "\027]0;a\nb\007c", "c");
    (* 8.3.27 DCS, in both codes; BEL is outside its command string and
       closes only an OSC. *)
    ("DCS, 7-bit", "a\027P1$r0m\027\\b", "ab");
    ("DCS, 8-bit", "a\u{0090}1$r0m\u{009C}b", "ab");
    ("BEL does not close a DCS", "a\027Pq\007#0\027\\b", "ab");
    (* 8.3.128 SOS, 8.3.94 PM, 8.3.2 APC, in both codes; an SOS character
       string may hold any bit combination but SOS and ST. *)
    ("SOS", "a\027Xs\nos\027\\b", "ab");
    ("SOS, 8-bit", "a\u{0098}sos\u{009C}b", "ab");
    ("PM", "a\027^pm\027\\b", "ab");
    ("PM, 8-bit", "a\u{009E}pm\u{009C}b", "ab");
    ("APC", "a\027_apc\027\\b", "ab");
    ("APC, 8-bit", "a\u{009F}apc\u{009C}b", "ab");
    (* A control string the text ends inside is the terminal's to the end. *)
    ("unterminated OSC", "a\027]0;title", "a");
    ("unterminated CSI", "a\027[31", "a");
    ("lone ESC", "a\027", "a");
    (* An ESC inside a control string ends it and opens what follows. *)
    ("ESC ends a string", "a\027]0;t\027[31mb", "ab");
    (* 8.3.6 CAN and 8.3.148 SUB: what precedes it is in error and ignored;
       the DEC parser xterm follows discards them, in a sequence or not. *)
    ("CAN ends a string", "a\027]0;t\024b", "ab");
    ("CAN ends a CSI", "a\027[3\024b", "ab");
    ("SUB ends a CSI", "a\027[3\026b", "ab");
    ("CAN ends an escape", "a\027(\024b", "ab");
    ("lone CAN", "a\024b", "ab");
    ("lone SUB", "a\026b", "ab");
    (* 5.3: the other C1 controls, ESC Fe and their 8-bit forms, and the
       ECMA-35 escape sequences ESC Fp, ESC Fs and ESC I F. *)
    ("RI, 7-bit", "a\027Mb", "ab");
    (* 8.3.86 NEXT LINE ends a line, in both codes. *)
    ("NEL, 8-bit", "a\u{0085}b", "a\nb");
    ("NEL, 7-bit", "a\027Eb", "a\nb");
    ("lone ST", "a\u{009C}b", "ab");
    ("ESC Fp", "a\0277b", "ab");
    ("ESC Fs", "a\027cb", "ab");
    ("ESC I F", "a\027(Bb", "ab");
    (* A C0 control inside a control sequence acts and the sequence goes on,
       as the DEC parser xterm follows does. *)
    ("C0 inside CSI", "a\027[3\n1mb", "a\nb");
    (* A character outside 5.4's ranges ends the sequence, and is text. *)
    ("CSI cut short", "a\027[3\u{00E9}b", "a\u{00E9}b");
    (* Malformed UTF-8 is the replacement glyph, a lone 09/11 byte too: in a
       UTF-8 stream it is no CSI. *)
    ("malformed UTF-8", "a\255b", "a\u{FFFD}b");
    ("raw 09/11 byte", "a\15531mb", "a\u{FFFD}31mb");
    (* The rest is {!Console_text.sanitize}'s. *)
    ("tab and DEL", "a\tb\127c", "a bc");
    ("plain text", "\u{4E16}\u{754C} ok", "\u{4E16}\u{754C} ok");
  ]

let test_strip () =
  List.iter
    (fun (what, raw, visible) ->
      Alcotest.(check string) what visible (Console_text.strip raw))
    stripped;
  Alcotest.(check string)
    "a line feed becomes a space without keep_newlines" "a b"
    (Console_text.strip ~keep_newlines:false "a\027[3\n1mb")

(* A terminal acts on the C0 controls other than the line ends, HT and ESC,
   and on DEL, without printing anything, in a sequence or not; a tab leaves
   blank cells. [strip] keeps what is printed; [sanitize] keeps a cell for
   each. *)
let test_strip_silent () =
  let silent =
    List.filter
      (fun c -> not (List.mem c [ 0x09; 0x0a; 0x0b; 0x0c; 0x0d; 0x1b ]))
      (List.init 0x20 Fun.id)
    @ [ 0x7f ]
  in
  List.iter
    (fun c ->
      let c = String.make 1 (Char.chr c) in
      let what = String.escaped c in
      Alcotest.(check string) what "ab" (Console_text.strip ("a" ^ c ^ "b"));
      (* CAN and SUB end the sequence instead. *)
      if c <> "\024" && c <> "\026" then
        Alcotest.(check string)
          (what ^ " inside CSI") "ab"
          (Console_text.strip ("a\027[3" ^ c ^ "1mb"));
      Alcotest.(check string)
        (what ^ " between CR and LF")
        "a\nb"
        (Console_text.strip ("a\r" ^ c ^ "\nb"));
      Alcotest.(check string)
        (what ^ ", sanitize") "a b"
        (Console_text.sanitize ("a" ^ c ^ "b")))
    silent;
  Alcotest.(check string) "tab" "a b" (Console_text.strip "a\tb")

(* Whatever the input, no ESC and no C1 control reaches the output, and it is
   UTF-8. *)
let test_strip_leaves_no_control () =
  List.iter
    (fun (what, raw, _) ->
      let s = Console_text.strip raw in
      Alcotest.(check bool) (what ^ ": UTF-8") true (String.is_valid_utf_8 s);
      let rec scan i =
        if i < String.length s then begin
          let d = String.get_utf_8_uchar s i in
          let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
          if (c < 0x20 && c <> 0x0a) || (c >= 0x7f && c <= 0x9f) then
            Alcotest.failf "%s: U+%04X survives in %S" what c s;
          scan (i + Uchar.utf_decode_length d)
        end
      in
      scan 0)
    stripped

let test_truecolour_reset () =
  let input = "\027[38;2;255;0;0mX" in
  Alcotest.(check string)
    "red must not leak into subsequent output" (input ^ "\027[0m")
    (Console_text.strip ~keep_styles:true input)

(* 8.3.117: 38, 48 and 58 take their colour as the parameters after them. A
   head with too few of them is no reset, so what came before it is still on
   and is turned off at the end. *)
let test_truncated_colour () =
  List.iter
    (fun tail ->
      let input = "\027[31mred" ^ tail in
      Alcotest.(check string)
        (String.escaped tail) (input ^ "\027[0m")
        (Console_text.strip ~keep_styles:true input))
    [ "\027[38;0m"; "\027[38;2;0;0m"; "\027[48;2;255;0m"; "\027[58;5m" ]

(* A line ends at a line feed, a carriage return, or the two together, which
   end one line: none leaves a carriage return, as a space, in a column of
   its own. Without [keep_newlines] a line end is one space. *)
let test_line_ends () =
  let check what expected f input =
    Alcotest.(check string) what expected (f input)
  in
  let strip = Console_text.strip ?keep_newlines:None in
  check "CR LF" "a\nb" strip "a\r\nb";
  check "CR" "a\nb" strip "a\rb";
  check "CR CR LF" "a\n\nb" strip "a\r\r\nb";
  check "LF CR" "a\n\nb" strip "a\n\rb";
  check "CR, an escape, LF" "a\nb" strip "a\r\027[K\nb";
  check "CR, a bidirectional mark, LF" "a\nb" Console_text.sanitize
    "a\r\u{200F}\nb";
  check "NEL" "a\nb" Console_text.sanitize "a\u{0085}b";
  (* 8.3.161 VT and 8.3.51 FF move to the next line, as a terminal's LF. *)
  check "VT" "a\nb" strip "a\011b";
  check "FF" "a\nb" Console_text.sanitize "a\012b";
  check "CR FF" "a\nb" strip "a\r\012b";
  check "CR NEL" "a\nb" strip "a\r\u{0085}b";
  check "NEL ends a sequence" "a\nb" strip "a\027[3\u{0085}b";
  check "NEL without keep_newlines" "a b"
    (Console_text.strip ~keep_newlines:false)
    "a\u{0085}b";
  check "sanitize, CR LF" "a\nb" Console_text.sanitize "a\r\nb";
  check "strip ~keep_styles, CR LF" "a\nb"
    (Console_text.strip ~keep_styles:true)
    "a\r\nb";
  check "CR LF without keep_newlines" "a b"
    (Console_text.sanitize ~keep_newlines:false)
    "a\r\nb";
  check "CR without keep_newlines" "a b"
    (Console_text.strip ~keep_newlines:false)
    "a\rb"

(* Characters that reorder or hide the text around them, or that are
   invisible text, are removed: the bidirectional embeddings, overrides,
   isolates and marks, the interlinear annotation characters, the tag
   characters, and the zero-width space, word joiner, invisible operators
   and zero-width no-break space. *)
let test_reordering () =
  let hidden =
    [
      "\u{061C}";
      "\u{200E}";
      "\u{200F}";
      "\u{202A}";
      "\u{202B}";
      "\u{202C}";
      "\u{202D}";
      "\u{202E}";
      "\u{2066}";
      "\u{2067}";
      "\u{2068}";
      "\u{2069}";
      "\u{FFF9}";
      "\u{FFFA}";
      "\u{FFFB}";
      "\u{E0001}";
      "\u{E0041}";
      "\u{E007F}";
      "\u{200B}";
      "\u{2060}";
      "\u{2064}";
      "\u{FEFF}";
    ]
  in
  List.iter
    (fun c ->
      Alcotest.(check string)
        (String.escaped c ^ " removed")
        "ab"
        (Console_text.sanitize ("a" ^ c ^ "b"));
      Alcotest.(check string)
        (String.escaped c ^ " removed by strip")
        "ab"
        (Console_text.strip ("a" ^ c ^ "b")))
    hidden;
  Alcotest.(check string)
    "joiners kept" "\u{1F468}\u{200D}\u{1F469} \u{200C}"
    (Console_text.sanitize "\u{1F468}\u{200D}\u{1F469} \u{200C}")

(* A text read in pieces is cut where no character and no escape code is
   under way; an escape code that holds all of it is cut between two
   characters. *)
let test_boundary () =
  List.iter
    (fun (what, s, k) -> Alcotest.(check int) what k (Console_text.boundary s))
    [
      ("empty", "", 0);
      ("text", "abc", 3);
      ("a whole character", "a\u{2764}", 4);
      ("two bytes of three", "a\xe2\x9d", 1);
      ("a lone lead byte", "ab\xf0", 2);
      ("an invalid byte", "a\x9d", 1);
      ("an invalid byte before", "\x9da", 2);
      ("a whole escape", "a\027[31mb", 7);
      ("an escape under way", "ab\027[31", 2);
      ("a lone ESC", "ab\027", 2);
      ("a 8-bit CSI under way", "ab\u{009B}3", 2);
      ("a control string under way", "ab\027]8;;http", 2);
      ("a closed control string", "\027]0;t\007x", 7);
      ("all one escape", "\027]0;t\xe2\x9d", 5);
    ]

(* An emoji tag sequence, such as the flag of Scotland (U+1F3F4, the tags of
   "gbsct", then U+E007F), is one picture: sanitize keeps its tags, where
   it removes a tag outside such a sequence. *)
let test_tag_sequence () =
  let scotland =
    "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}"
  in
  Alcotest.(check string)
    "the flag whole"
    ("a" ^ scotland ^ "b")
    (Console_text.sanitize ("a" ^ scotland ^ "b"));
  Alcotest.(check string)
    "a stray tag removed" "ab"
    (Console_text.sanitize "a\u{E0067}b");
  Alcotest.(check string)
    "the flag whole through strip"
    ("a" ^ scotland ^ "b")
    (Console_text.strip ("a" ^ scotland ^ "b"));
  Alcotest.(check string)
    "tags without a cancel tag removed" "\u{1F3F4}b"
    (Console_text.sanitize "\u{1F3F4}\u{E0067}\u{E0062}b");
  Alcotest.(check string)
    "a cancel tag alone removed" "\u{1F3F4}b"
    (Console_text.sanitize "\u{1F3F4}\u{E007F}b");
  Alcotest.(check string)
    "tags on another base removed" "xb"
    (Console_text.sanitize "x\u{E0067}\u{E0062}\u{E007F}b");
  let tags s =
    let b = Buffer.create 64 in
    Buffer.add_utf_8_uchar b (Uchar.of_int 0x1F3F4);
    String.iter
      (fun c -> Buffer.add_utf_8_uchar b (Uchar.of_int (0xE0000 + Char.code c)))
      s;
    Buffer.add_utf_8_uchar b (Uchar.of_int 0xE007F);
    Buffer.contents b
  in
  List.iter
    (fun f ->
      Alcotest.(check string)
        (f ^ " kept") (tags f)
        (Console_text.sanitize (tags f)))
    [ "gbeng"; "gbsct"; "gbwls" ];
  List.iter
    (fun hidden ->
      Alcotest.(check string)
        (hidden ^ " stripped from the flag")
        "\u{1F3F4}b"
        (Console_text.sanitize (tags hidden ^ "b")))
    [ "usca"; "gbabc"; "ignore previous instructions"; "gbsctx" ]

let test_default_ignorable () =
  List.iter
    (fun c ->
      Alcotest.(check string)
        (String.escaped c ^ " removed")
        "ab"
        (Console_text.sanitize ("a" ^ c ^ "b")))
    [
      "\u{00AD}";
      "\u{034F}";
      "\u{115F}";
      "\u{1160}";
      "\u{17B4}";
      "\u{180E}";
      "\u{206A}";
      "\u{206F}";
      "\u{3164}";
      "\u{FFA0}";
      "\u{1D173}";
      "\u{E0001}";
    ];
  Alcotest.(check string)
    "a Mongolian variation selector after a base kept" "\u{1820}\u{180B}"
    (Console_text.sanitize "\u{1820}\u{180B}")

(* A variation selector, VS1 to VS16 or VS17 to VS256, selects a glyph of
   the character before it: one after a base is kept, and one after no
   base, such as another selector, is removed, so that a run of them cannot
   hide text. At the start of the text it is kept: its base may end the
   text before, as when a line editor takes a character at a time. *)
let test_variation_selectors () =
  let check what expected s =
    Alcotest.(check string) what expected (Console_text.sanitize s);
    Alcotest.(check string) (what ^ ", strip") expected (Console_text.strip s)
  in
  check "VS16 after a heart" "\u{2764}\u{FE0F}" "\u{2764}\u{FE0F}";
  check "an ideographic variation sequence" "\u{845B}\u{E0100}"
    "\u{845B}\u{E0100}";
  check "a keycap" "1\u{FE0F}\u{20E3}" "1\u{FE0F}\u{20E3}";
  check "one selector of a run" "a\u{E0100}b" "a\u{E0100}\u{E0101}\u{E01EF}b";
  check "VS16 twice" "\u{2764}\u{FE0F}" "\u{2764}\u{FE0F}\u{FE0F}";
  check "at the start" "\u{E0100}a" "\u{E0100}a";
  check "a run at the start" "\u{FE0F}a" "\u{FE0F}\u{FE0F}a";
  check "after a space" "a b" "a \u{FE0F}b";
  check "after a line end" "a\nb" "a\n\u{E0100}b";
  check "after a joiner" "\u{1F468}\u{200D}b" "\u{1F468}\u{200D}\u{FE0F}b"

let suite =
  ( "console_text",
    [
      Alcotest.test_case "boundary" `Quick test_boundary;
      Alcotest.test_case "reordering characters" `Quick test_reordering;
      Alcotest.test_case "truecolour is reset after text" `Quick
        test_truecolour_reset;
      Alcotest.test_case "a truncated colour is no reset" `Quick
        test_truncated_colour;
      Alcotest.test_case "line ends" `Quick test_line_ends;
      Alcotest.test_case "strip" `Quick test_strip;
      Alcotest.test_case "an emoji tag sequence is kept" `Quick
        test_tag_sequence;
      Alcotest.test_case "variation selectors" `Quick test_variation_selectors;
      Alcotest.test_case "default ignorable characters" `Quick
        test_default_ignorable;
      Alcotest.test_case "strip drops what prints nothing" `Quick
        test_strip_silent;
      Alcotest.test_case "strip leaves no control" `Quick
        test_strip_leaves_no_control;
    ] )
