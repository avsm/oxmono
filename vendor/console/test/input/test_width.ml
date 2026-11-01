(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let check msg expected s =
  Alcotest.(check int) msg expected (Width.string_width s)

let test_widths () =
  check "ASCII" 5 "hello";
  check "empty" 0 "";
  check "malformed UTF-8 is one replacement cell" 1 "\xff";
  check "a spacing mark takes a cell of its own" 2 "x\xe0\xa5\x80";
  check "a vowel sign after its consonant" 2 "\u{0915}\u{093F}";
  check "a spacing mark alone" 1 "\u{093F}";
  check "a wide spacing mark adds its two cells" 3 "x\u{302E}";
  check "a combining mark adds nothing" 1 "e\xcc\x81";
  check "CJK" 4 "\xe7\x95\x8c\xe7\x95\x8c";
  check "emoji" 2 "\xf0\x9f\x8e\x89";
  check "ZWJ family" 2
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9\xe2\x80\x8d\xf0\x9f\x91\xa7";
  check "flag" 2 "\xf0\x9f\x87\xab\xf0\x9f\x87\xb7";
  check "keycap" 2 "1\xef\xb8\x8f\xe2\x83\xa3";
  check "VS16 makes an emoji" 2 "\xe2\x9d\xa4\xef\xb8\x8f";
  check "an emoji tag sequence" 2
    "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}";
  Alcotest.(check int)
    "an emoji tag sequence is one cluster" 1
    (List.length
       (Width.clusters
          "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}"))

(* A cluster takes the cells of its widest character, not of its first
   one with any width. *)
let test_widest () =
  check "a bare heart is text" 1 "\u{2764}";
  check "a bare double exclamation mark is text" 1 "\u{203C}";
  check "a skin tone on a text pictograph" 2 "\u{261D}\u{1F3FD}";
  check "a ZWJ before a wide emoji" 2 "\u{2764}\u{200D}\u{1F469}";
  check "a prepended letter before a wide one" 2 "\u{0D4E}\u{754C}";
  check "a conjunct takes its widest consonant" 1 "\u{0915}\u{094D}\u{0937}";
  check "a VS16 after a combining mark selects nothing" 1
    "\u{2764}\u{0301}\u{FE0F}"

(* A spacing mark adds its cells once, after the first character of its
   cluster. *)
let test_marks () =
  check "a spacing mark after a mark of no width" 1 "\u{0301}\u{0940}";
  check "Thai SARA AM after its consonant" 2 "\u{0E01}\u{0E33}";
  check "Lao AM after its consonant" 2 "\u{0E81}\u{0EB3}";
  check "Thai SARA AM alone" 1 "\u{0E33}";
  Alcotest.(check bool)
    "SARA AM is a spacing mark" true
    (Width.is_spacing_mark 0x0E33);
  check "a wide spacing mark after a ZWJ" 2 "\u{200D}\u{302E}"

(* A VS16 that selects nothing still splits the text as it is: it does
   not join the clusters around it. *)
let test_stray_vs16_splits () =
  check "a prepended letter, a VS16, then CJK" 3 "\u{0D4E}\u{FE0F}\u{754C}";
  Alcotest.(check (list string))
    "its clusters"
    [ "\u{0D4E}\u{FE0F}"; "\u{754C}" ]
    (Width.clusters "\u{0D4E}\u{FE0F}\u{754C}")

let test_sub_width () =
  let s = "ab\u{754C}e\u{0301}\u{2764}\u{FE0F}" in
  for pos = 0 to String.length s do
    for len = 0 to String.length s - pos do
      Alcotest.(check int)
        (Fmt.str "%S at %d for %d" s pos len)
        (Width.string_width (String.sub s pos len))
        (Width.sub_width s ~pos ~len)
    done
  done

(* The width is the column a terminal's cursor reaches. *)
let test_matches_terminal () =
  List.iter
    (fun s ->
      let vte = Console_vte.v ~rows:1 ~cols:80 () in
      Console_vte.feed vte s;
      Alcotest.(check int)
        (String.escaped s)
        (snd (Console_vte.cursor_pos vte))
        (Width.string_width s))
    [
      "hello";
      "\xe7\x95\x8c\xe7\x95\x8c";
      "e\xcc\x81";
      "\xf0\x9f\x8e\x89 ok";
      "\u{0915}\u{093F} ok";
      "\u{FFFD}\u{302E} ok";
    ]

let utf_8 cps =
  let b = Buffer.create 16 in
  List.iter
    (fun cp ->
      Buffer.add_utf_8_uchar b (Uchar.of_int (int_of_string ("0x" ^ cp))))
    cps;
  Buffer.contents b

(* The rows of widths.txt: the string, the cells each terminal gives it,
   [None] where unknown, and console's. *)
let rows () =
  In_channel.with_open_text "widths.txt" In_channel.input_lines
  |> List.filter (fun l -> l <> "" && l.[0] <> '#')
  |> List.map (fun line ->
      match List.map String.trim (String.split_on_char '|' line) with
      | cps :: rest -> (
          let cells = function "?" -> None | n -> Some (int_of_string n) in
          match List.rev rest with
          | what :: console :: terminals ->
              ( what,
                utf_8 (String.split_on_char ' ' cps |> List.filter (( <> ) "")),
                List.rev_map cells terminals,
                int_of_string console )
          | _ -> Alcotest.failf "a short row: %S" line)
      | [] -> Alcotest.failf "an empty row: %S" line)

(* Console counts what its policy says, which is what the terminals show
   where they agree, and one of their counts where they do not. *)
let test_real_terminals () =
  let rows = rows () in
  Alcotest.(check bool) "the table has rows" true (List.length rows > 10);
  List.iter
    (fun (what, s, terminals, console) ->
      Alcotest.(check int) what console (Width.string_width s);
      let known = List.filter_map Fun.id terminals in
      (match List.sort_uniq Int.compare known with
      | [ all ] ->
          Alcotest.(check int) (what ^ ": as every terminal") all console
      | _ -> ());
      Alcotest.(check bool)
        (what ^ ": as some terminal")
        true (List.mem console known))
    rows

let suite =
  ( "width",
    [
      Alcotest.test_case "widths" `Quick test_widths;
      Alcotest.test_case "the widest character" `Quick test_widest;
      Alcotest.test_case "spacing marks" `Quick test_marks;
      Alcotest.test_case "a stray VS16 splits as it is" `Quick
        test_stray_vs16_splits;
      Alcotest.test_case "a part as a string" `Quick test_sub_width;
      Alcotest.test_case "matches the terminal" `Quick test_matches_terminal;
      Alcotest.test_case "as real terminals show it" `Quick test_real_terminals;
    ] )
