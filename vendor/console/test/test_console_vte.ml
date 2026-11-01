(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* One record of the byte stream BSD script(1) -r writes: the payload length,
   the wall clock, the kind, then the payload, little-endian throughout. The
   seconds and microseconds are given apart so the fixture states exact times
   rather than rounding a float into a field. *)
let record ~kind ~sec ~usec payload =
  let buffer = Buffer.create (24 + String.length payload) in
  let add64 v =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 (Int64.of_int v);
    Buffer.add_bytes buffer b
  in
  let add32 v =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int v);
    Buffer.add_bytes buffer b
  in
  add64 (String.length payload);
  add64 sec;
  add32 usec;
  add32 (Char.code kind);
  Buffer.add_string buffer payload;
  Buffer.contents buffer

(* A session whose third write redraws the second line rather than adding a
   third: the cursor goes up, returns to column zero and overwrites. Counting
   the lines in the byte stream reports three; the screen holds two. *)
let redraw_session =
  String.concat ""
    [
      record ~kind:'s' ~sec:100 ~usec:0 "\n";
      record ~kind:'o' ~sec:100 ~usec:0 "first\r\n";
      record ~kind:'o' ~sec:100 ~usec:500_000 "second\r\n";
      record ~kind:'o' ~sec:102 ~usec:250_000 "\027[1A\rthird \r\n";
    ]

let frames = Alcotest.(list (pair (float 1e-9) (list string)))

(* The screen after each write, with the seconds since the recording started.
   The header record makes no frame, the redraw replaces the row it landed on,
   and the space "third " left behind "second" is not part of what a reader
   sees. *)
let test_frames_are_the_screen_after_each_write () =
  Alcotest.check frames "three writes, two rows"
    [
      (0., [ "first" ]);
      (0.5, [ "first"; "second" ]);
      (2.25, [ "first"; "third" ]);
    ]
    (Console_vte.frames ~rows:6 ~cols:20 redraw_session)

(* Every screen carries the whole session, so the emulator is fed once rather
   than rebuilt per record: a fresh emulator per write would lose "first". *)
let test_frames_keep_the_screen_between_writes () =
  match Console_vte.frames ~rows:6 ~cols:20 redraw_session with
  | [ _; _; (_, rows) ] ->
      Alcotest.(check (list string))
        "the last screen still holds the first line" [ "first"; "third" ] rows
  | other -> Alcotest.failf "expected three frames, got %d" (List.length other)

(* A capture of a program that was still running ends mid-record. The stream
   stops there rather than raising, and every whole record before it stands. *)
let test_a_partial_record_ends_the_stream () =
  let truncated =
    let whole =
      redraw_session ^ record ~kind:'o' ~sec:103 ~usec:0 "never finished"
    in
    String.sub whole 0 (String.length whole - 4)
  in
  Alcotest.(check int)
    "the partial record makes no frame" 3
    (List.length (Console_vte.frames ~rows:6 ~cols:20 truncated));
  Alcotest.(check int)
    "and a header alone makes none either" 0
    (List.length
       (Console_vte.frames ~rows:6 ~cols:20
          (record ~kind:'s' ~sec:1 ~usec:0 "\n")))

(* A header whose length is near [max_int] promises more than the stream
   holds; the bound is checked without overflowing, so the stream ends. *)
let test_a_huge_length_ends_the_stream () =
  let huge =
    let header = record ~kind:'o' ~sec:1 ~usec:0 "" in
    let b = Bytes.of_string header in
    Bytes.set_int64_le b 0 (Int64.of_int max_int);
    Bytes.to_string b ^ "payload"
  in
  Alcotest.(check int) "no record" 0 (List.length (Console_vte.records huge))

(* The timestamp is seconds from the first record, which is what makes two
   captures of one command comparable; [records] carries the epoch seconds the
   capture stamped, which is what makes it a faithful read of the file. *)
let test_records_carry_the_wall_clock () =
  Alcotest.(check (list (pair (float 1e-9) string)))
    "three writes, stamped as recorded"
    [
      (100., "first\r\n"); (100.5, "second\r\n"); (102.25, "\027[1A\rthird \r\n");
    ]
    (Console_vte.records redraw_session)

(* [screen] is what a reader sees: no trailing blanks on a row, no blank rows
   under the last one, and the scrollback in front only when asked. *)
let test_screen_trims_what_no_reader_sees () =
  let vte = Console_vte.v ~rows:6 ~cols:20 () in
  Console_vte.feed vte "padded   \r\n";
  Alcotest.(check (list string))
    "the row is trimmed and the blank rows dropped" [ "padded" ]
    (Console_vte.screen vte)

let read ?raw ?(cols = 10) writes =
  let t = Console_vte.v ?raw ~rows:3 ~cols () in
  List.iter (Console_vte.feed t) writes;
  t

let check_screen msg expected t =
  Alcotest.(check (list string)) msg expected (Console_vte.screen t)

let check_cursor msg expected t =
  Alcotest.(check (pair int int)) msg expected (Console_vte.cursor_pos t)

(* A backspace moves the cursor and erases nothing, which is how a line
   editor steps back over the text after its cursor. *)
let test_backspace_moves () =
  let t = read [ "abc\b\b" ] in
  check_screen "the text stays" [ "abc" ] t;
  check_cursor "one cell right of a" (0, 1) t

let foreground t ~x ~y =
  let g = Console_vte.grid t in
  let i = Grid.idx g ~x ~y in
  Ansi.Color.to_rgb (Console_vte.fg g i)

(* SGR 39 and 49 give back the terminal's own colours. *)
let test_default_colours () =
  let plain = foreground (read [ "X" ]) ~x:0 ~y:0 in
  let reset = foreground (read [ "\027[31;41m\027[39;49mX" ]) ~x:0 ~y:0 in
  Alcotest.(check (triple int int int)) "the default foreground" plain reset;
  let palette = foreground (read [ "\027[38;5;39mX" ]) ~x:0 ~y:0 in
  Alcotest.(check bool)
    "colour 39 of the palette is a colour" false (palette = plain)

(* A character of no width joins the cell before it, in the same write as
   its letter or in the next one. *)
let test_zero_width_kept () =
  check_screen "a combining mark" [ "e\u{301}x" ] (read [ "e\u{301}x" ]);
  check_screen "in the next write" [ "e\u{301}" ] (read [ "e"; "\u{301}" ]);
  check_screen "a bidirectional override" [ "a\u{202e}b" ]
    (read [ "a\u{202e}b" ])

(* After a character in the last column, a cursor control acts from that
   column. *)
let test_last_column () =
  let full = "0123456789" in
  check_screen "erasing to the end takes the last cell" [ "012345678" ]
    (read [ full ^ "\027[K" ]);
  check_cursor "two left of the last column" (0, 7) (read [ full ^ "\027[2D" ]);
  check_screen "a backspace steps back from it" [ "0123456789" ]
    (read [ full ^ "\b" ]);
  check_cursor "to the cell before it" (0, 8) (read [ full ^ "\b" ]);
  check_screen "up a row in the last column" [ "         X"; full ]
    (read [ "\n" ^ full ^ "\027[AX" ])

(* A line feed keeps the column with output processing off. *)
let test_raw_line_feed () =
  check_screen "a staircase" [ "ab"; "  cd" ] (read ~raw:true [ "ab\ncd" ]);
  check_screen "a returned line" [ "ab"; "cd" ] (read [ "ab\ncd" ])

(* A sequence split across two writes is read whole. *)
let test_split_sequence () =
  check_screen "erased" [ "a" ] (read [ "abc\r\027[1"; "C\027["; "K" ])

let skip = Display_harness.skip_on_matrix_0_1

let suite =
  ( "console_vte",
    [
      Alcotest.test_case "frames are the screen after each write" `Quick
        test_frames_are_the_screen_after_each_write;
      Alcotest.test_case "frames keep the screen between writes" `Quick
        test_frames_keep_the_screen_between_writes;
      Alcotest.test_case "a partial record ends the stream" `Quick
        test_a_partial_record_ends_the_stream;
      Alcotest.test_case "a huge length ends the stream" `Quick
        test_a_huge_length_ends_the_stream;
      Alcotest.test_case "records carry the wall clock" `Quick
        test_records_carry_the_wall_clock;
      Alcotest.test_case "screen trims what no reader sees" `Quick
        test_screen_trims_what_no_reader_sees;
      Alcotest.test_case "a backspace moves" `Quick test_backspace_moves;
      Alcotest.test_case "SGR 39 and 49 are the default colours" `Quick
        (skip "invariant-hq/mosaic@70316cb9 (default colours)"
           test_default_colours);
      Alcotest.test_case "a character of no width is kept" `Quick
        (skip
           "invariant-hq/mosaic@65da7988, @311b71f8 and #79 (zero-width \
            characters)"
           test_zero_width_kept);
      Alcotest.test_case "controls from the last column" `Quick
        (skip "invariant-hq/mosaic@62ae9fc1 (pending wrap)" test_last_column);
      Alcotest.test_case "a raw line feed keeps the column" `Quick
        (skip "invariant-hq/mosaic@2dd1c517 (LF keeps the column)"
           test_raw_line_feed);
      Alcotest.test_case "a sequence across writes" `Quick test_split_sequence;
    ] )
