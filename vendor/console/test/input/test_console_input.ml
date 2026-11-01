(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let lines = Alcotest.(list string)

let test_printable () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc" in
  Alcotest.(check string) "echoes the typed bytes" "abc" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string)
    "pending holds the line" "abc" (Console_input.pending t)

(* A control sequence with intermediate bytes, such as DECSCUSR (ESC [ 1 SP
   q) or SR (ESC [ 2 SP A), ends at its final byte: none of it is typed, and
   its final byte is no arrow. *)
let test_csi_intermediate () =
  let t = Console_input.v () in
  let echo, _ = Console_input.feed t "ab\027[1 qc\027[2 Ad" in
  Alcotest.(check string) "echo" "abcd" echo;
  Alcotest.(check string) "the line" "abcd" (Console_input.pending t)

let test_submit_cr () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc\r" in
  Alcotest.(check string) "echo ends in CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "one completed line" [ "abc" ] ls;
  Alcotest.(check string) "pending cleared" "" (Console_input.pending t)

let test_submit_lf () =
  let t = Console_input.v () in
  let _, ls = Console_input.feed t "abc\n" in
  Alcotest.(check lines) "LF submits too" [ "abc" ] ls

let test_crlf_is_one_line () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "abc\r\n" in
  Alcotest.(check string) "echo is a single CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "CRLF yields one line, no empty second" [ "abc" ] ls

let test_utf8_passthrough () =
  let t = Console_input.v () in
  let echo, _ = Console_input.feed t "caf\xc3\xa9" in
  Alcotest.(check string) "high bytes echoed verbatim" "caf\xc3\xa9" echo;
  Alcotest.(check string) "and stored" "caf\xc3\xa9" (Console_input.pending t)

let test_backspace () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "ab\127c\r" in
  Alcotest.(check string)
    "erase echoes backspace-space-backspace" "ab\b \bc\r\n" echo;
  Alcotest.(check lines) "'b' erased, then 'c' appended" [ "ac" ] ls

let test_backspace_ctrl_h () =
  let t = Console_input.v () in
  let _, ls = Console_input.feed t "ab\008\r" in
  Alcotest.(check lines) "Ctrl-H also erases" [ "a" ] ls

let test_backspace_on_empty () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "\127" in
  Alcotest.(check string) "nothing to erase, no echo" "" echo;
  Alcotest.(check lines) "no line" [] ls

let test_control_chars_ignored () =
  let t = Console_input.v () in
  let echo, ls = Console_input.feed t "a\001\002b\r" in
  Alcotest.(check string)
    "control bytes neither echoed nor stored" "ab\r\n" echo;
  Alcotest.(check lines) "control bytes dropped" [ "ab" ] ls

let test_masked_input () =
  let t = Console_input.v ~mask:"*" () in
  let echo, ls = Console_input.feed t "s3cr3t" in
  Alcotest.(check string) "one mask per character" "******" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string)
    "secret retained for the caller" "s3cr3t" (Console_input.pending t);
  let echo, _ = Console_input.feed t "\127" in
  Alcotest.(check string) "backspace erases one mask" "\b \b" echo;
  let echo, ls = Console_input.feed t "\r" in
  Alcotest.(check string) "submit reveals nothing" "\r\n" echo;
  Alcotest.(check lines) "submitted value is unmasked" [ "s3cr3" ] ls;
  Alcotest.(check (list string))
    "secrets are never retained in history" [] (Console_input.history t)

(* A secret is the bytes typed, whatever they are: a bidi mark, a C1
   control, a byte that is no UTF-8, a mark with nothing to join, or a
   character cut short. Only masks are echoed, the cursor moved by
   backspaces. *)
let test_masked_keeps_bytes () =
  let secret typed =
    let t = Console_input.v ~mask:"*" () in
    let echo, ls = Console_input.feed t (typed ^ "\r") in
    String.iter
      (fun c ->
        if not (String.contains "*\b \r\n" c) then
          Alcotest.failf "%S echoes %C" typed c)
      echo;
    ls
  in
  List.iter
    (fun typed ->
      Alcotest.(check lines) (Fmt.str "%S" typed) [ typed ] (secret typed))
    [
      "p\u{202e}w";
      "\u{200f}pw";
      "p\xc2\x85w";
      "\xffpw\xfe";
      "p\x80\xbfw";
      "\u{0301}pw";
      "pw\xe2\x82";
      "p\xe2\x82w";
      "\xed\xa0\x80";
    ];
  Alcotest.(check lines)
    "backspace erases what was typed" [ "p\xff" ] (secret "p\xff\xfe\127")

let test_split_across_feeds () =
  let t = Console_input.v () in
  let _, ls1 = Console_input.feed t "he" in
  Alcotest.(check lines) "nothing yet" [] ls1;
  let _, ls2 = Console_input.feed t "llo\r" in
  Alcotest.(check lines) "line accumulated across feeds" [ "hello" ] ls2

let test_split_crlf_across_feeds () =
  let t = Console_input.v () in
  let _, ls1 = Console_input.feed t "a\r" in
  Alcotest.(check lines) "CR submits" [ "a" ] ls1;
  let _, ls2 = Console_input.feed t "\nb\r" in
  Alcotest.(check lines) "trailing LF swallowed" [ "b" ] ls2

let left = "\027[D"
let right = "\027[C"

let test_cursor_insert () =
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ "X") in
  Alcotest.(check string) "insert at the cursor" "aXb" (Console_input.pending t)

let test_cursor_backspace_midline () =
  let t = Console_input.v () in
  let _ = Console_input.feed t ("abc" ^ left ^ "\127") in
  Alcotest.(check string)
    "backspace deletes before the cursor" "ac" (Console_input.pending t)

let test_cursor_right_returns_to_end () =
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ right ^ "X") in
  Alcotest.(check string)
    "right cancels the left" "abX" (Console_input.pending t)

let test_cursor_left_clamps () =
  let t = Console_input.v () in
  let _ = Console_input.feed t ("ab" ^ left ^ left ^ left ^ "X") in
  Alcotest.(check string)
    "left clamps at the start" "Xab" (Console_input.pending t)

let test_utf8_backspace_whole_char () =
  let t = Console_input.v () in
  let echo, _ = Console_input.feed t "caf\xc3\xa9\127" in
  Alcotest.(check string) "one cell erased" "caf\xc3\xa9\b \b" echo;
  Alcotest.(check string) "whole character gone" "caf" (Console_input.pending t)

let test_utf8_split_across_feeds () =
  let t = Console_input.v () in
  let _, _ = Console_input.feed t "\xc3" in
  Alcotest.(check string)
    "incomplete character not yet in the line" "" (Console_input.pending t);
  let _, _ = Console_input.feed t "\xa9" in
  Alcotest.(check string)
    "completed once its bytes arrive" "\xc3\xa9" (Console_input.pending t)

(* A character two cells wide drives the cursor motion and the erase by its
   cells, not by the character count. *)
let test_wide_backspace_clears_two_cells () =
  let echo, _ = Console_input.feed (Console_input.v ()) "\xe4\xb8\xad\127" in
  Alcotest.(check string) "two cells cleared" "\xe4\xb8\xad\b\b  \b\b" echo

let test_wide_left_steps_two_cells () =
  let echo, _ =
    Console_input.feed (Console_input.v ())
      ("\xe4\xb8\xad\xe6\x96\x87" ^ "\027[D")
  in
  Alcotest.(check string)
    "left steps over two cells" "\xe4\xb8\xad\xe6\x96\x87\b\b" echo

(* The screen a terminal shows after [keys] typed in an editor, and the
   line the editor holds. *)
let typed ?mask keys =
  let t = Console_input.v ?mask () in
  let term = Console_vte.v ~raw:true ~rows:1 ~cols:40 () in
  List.iter (fun k -> Console_vte.feed term (fst (Console_input.feed t k))) keys;
  let shown = match Console_vte.screen term with [] -> "" | row :: _ -> row in
  (shown, Console_input.pending t)

let check_typed msg ?mask keys ~shown ~line =
  let s, l = typed ?mask keys in
  Alcotest.(check string) (msg ^ ": the screen") shown s;
  Alcotest.(check string) (msg ^ ": the line") line l

(* A backspace erases the cluster before the cursor, its marks with it, on
   the screen as in the line, however its characters were typed. *)
let test_cluster_backspace () =
  check_typed "a combining mark" [ "ae\u{0301}\127" ] ~shown:"a" ~line:"a";
  check_typed "typed apart" [ "ae"; "\u{0301}"; "\127" ] ~shown:"a" ~line:"a";
  check_typed "a spacing mark" [ "a\u{0915}\u{093F}\127" ] ~shown:"a" ~line:"a";
  check_typed "in the middle"
    [ "e\u{0301}x"; left; "\127" ]
    ~shown:"x" ~line:"x";
  check_typed ~mask:"*" "masked" [ "ae\u{0301}"; "\127" ] ~shown:"*" ~line:"a";
  check_typed "a mark with nothing to join" [ "\u{0301}a" ] ~shown:"a" ~line:"a"

(* The arrows step over a cluster whole, so that what is typed next goes
   before or after it, never inside. *)
let test_cluster_arrows () =
  check_typed "left over a cluster" [ "e\u{0301}"; left; "X" ]
    ~shown:"Xe\u{0301}" ~line:"Xe\u{0301}";
  check_typed "and back"
    [ "e\u{0301}"; left; right; "X" ]
    ~shown:"e\u{0301}X" ~line:"e\u{0301}X";
  check_typed "a spacing mark, two cells"
    [ "\u{0915}\u{093F}y"; left; left; "X" ]
    ~shown:"X\u{0915}\u{093F}y" ~line:"X\u{0915}\u{093F}y"

(* What is typed at the cursor can join the cluster after it, as a ZWJ
   joins two emoji, and what is erased can let its neighbours join. The
   cursor then goes past the joined cluster, never inside it, and the
   screen shows the line. *)
let test_cluster_joins_after () =
  check_typed "a ZWJ before an emoji"
    [ "\u{1F469}"; left; "\u{2764}"; "\u{200D}"; "\u{754C}"; "a" ]
    ~shown:"\u{2764}\u{200D}\u{1F469}\u{754C}a"
    ~line:"\u{2764}\u{200D}\u{1F469}\u{754C}a";
  check_typed "erasing what stood between them"
    [ "\u{2764}\u{200D}x\u{1F469}"; left; "\127"; "b" ]
    ~shown:"\u{2764}\u{200D}\u{1F469}b" ~line:"\u{2764}\u{200D}\u{1F469}b";
  check_typed "a flag's second half before its first"
    [ "\u{1F1F7}"; left; "\u{1F1EB}"; "x" ]
    ~shown:"\u{1F1EB}\u{1F1F7}x" ~line:"\u{1F1EB}\u{1F1F7}x";
  (* matrix 0.1.0's emulator draws U+2764 ZWJ U+1F469 in the one cell of
     U+2764, where console counts the two of U+1F469, so this reads the echo. *)
  let t = Console_input.v () in
  ignore (Console_input.feed t ("x\u{1F469}" ^ left ^ "\u{2764}\u{200D}"));
  Alcotest.(check string)
    "erasing the joined cluster" "\b\b  \b\b"
    (fst (Console_input.feed t "\127"));
  Alcotest.(check string)
    "erasing the joined cluster: the line" "x" (Console_input.pending t)

let up = "\027[A"
let down = "\027[B"

let test_history_up () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "abc\r" in
  let echo, ls = Console_input.feed t up in
  Alcotest.(check string)
    "recalled line echoed (empty buffer, no erase)" "abc" echo;
  Alcotest.(check lines) "no submit" [] ls;
  Alcotest.(check string)
    "pending is the recalled line" "abc" (Console_input.pending t)

let test_history_up_twice_clamps () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "one\r" in
  let _ = Console_input.feed t "two\r" in
  let _ = Console_input.feed t up in
  Alcotest.(check string) "first up = newest" "two" (Console_input.pending t);
  let _ = Console_input.feed t up in
  Alcotest.(check string) "second up = older" "one" (Console_input.pending t);
  let _ = Console_input.feed t up in
  Alcotest.(check string)
    "third up clamps at oldest" "one" (Console_input.pending t)

let test_history_down_restores_draft () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "old\r" in
  let _ = Console_input.feed t "dra" in
  let _ = Console_input.feed t up in
  Alcotest.(check string) "up recalls history" "old" (Console_input.pending t);
  let _ = Console_input.feed t down in
  Alcotest.(check string)
    "down restores the in-progress draft" "dra" (Console_input.pending t)

let test_history_erase_on_recall () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "abcd\r" in
  let _ = Console_input.feed t "xy" in
  let echo, _ = Console_input.feed t up in
  Alcotest.(check string)
    "erases the two draft chars then writes recalled line" "\b \b\b \babcd" echo

let test_history_dedup_consecutive () =
  let t = Console_input.v () in
  let _ = Console_input.feed t "x\r" in
  let _ = Console_input.feed t "x\r" in
  Alcotest.(check lines)
    "consecutive duplicate kept once" [ "x" ] (Console_input.history t)

let test_history_seed_and_order () =
  let t = Console_input.v ~history:[ "first"; "second" ] () in
  let _ = Console_input.feed t "third\r" in
  Alcotest.(check lines)
    "oldest first, then new"
    [ "first"; "second"; "third" ]
    (Console_input.history t)

let test_complete_single () =
  let t = Console_input.v ~complete:(fun _ -> [ "select" ]) () in
  let _ = Console_input.feed t "sel" in
  let echo, _ = Console_input.feed t "\t" in
  Alcotest.(check string) "tab echoes only the completion suffix" "ect" echo;
  Alcotest.(check string) "pending completed" "select" (Console_input.pending t)

let test_complete_common_prefix () =
  let t = Console_input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let _ = Console_input.feed t "co\t" in
  Alcotest.(check string)
    "extends to longest common prefix" "con" (Console_input.pending t)

let test_complete_lists_candidates () =
  let t = Console_input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let echo, _ = Console_input.feed t "co\t" in
  Alcotest.(check bool)
    "lists both candidates" true
    (Display_harness.contains "connections  config" echo)

let test_complete_last_word () =
  let complete w = if String.equal w "co" then [ "connections" ] else [] in
  let t = Console_input.v ~complete () in
  let _ = Console_input.feed t "SELECT * FROM co\t" in
  Alcotest.(check string)
    "completes only the trailing word" "SELECT * FROM connections"
    (Console_input.pending t)

let test_complete_none () =
  let t = Console_input.v ~complete:(fun _ -> []) () in
  let _ = Console_input.feed t "zz" in
  let echo, _ = Console_input.feed t "\t" in
  Alcotest.(check string) "no candidates, tab echoes nothing" "" echo;
  Alcotest.(check string) "pending unchanged" "zz" (Console_input.pending t)

let test_complete_utf8 () =
  let t = Console_input.v ~complete:(fun _ -> [ "é"; "ê" ]) () in
  ignore (Console_input.feed t "\t");
  Alcotest.(check string)
    "no complete character is shared" "" (Console_input.pending t)

(* Bytes that are not a character are replacement characters, one for each
   maximal part of a character that [String.get_utf_8_uchar] reads, as
   [Console_text.sanitize] shows them, in the line and on the screen, so the
   cursor stays where the terminal puts it. *)
let test_invalid_utf8 () =
  List.iter
    (fun bad ->
      let shown = Console_text.sanitize bad in
      let check t echo =
        Alcotest.(check string)
          (String.escaped bad) shown (Console_input.pending t);
        Alcotest.(check string) "echo" shown echo
      in
      let t = Console_input.v () in
      let echo, _ = Console_input.feed t bad in
      check t echo;
      let t = Console_input.v () in
      let echo =
        String.concat ""
          (List.init (String.length bad) (fun i ->
               fst (Console_input.feed t (String.make 1 bad.[i]))))
      in
      check t echo)
    [
      "\xed\xa0\x80";
      "\xc0\xaf";
      "\xf4\x90\x80\x80";
      "\xff";
      "a\x80b";
      "\xe2\x82a";
    ]

let test_echo_is_text () =
  let no_escape msg (echo, _) =
    Alcotest.(check bool) msg false (String.contains echo '\027')
  in
  let t =
    Console_input.v ~prompt:"\027[2J> " ~history:[ "\027]0;owned\007" ]
      ~complete:(fun _ -> [ "a\027[31m"; "ab\027[2J" ])
      ()
  in
  no_escape "completion and prompt" (Console_input.feed t "\t");
  no_escape "history" (Console_input.feed t "\027[A")

(* A typed character the terminal would act on, such as the one-byte CSI
   U+009B, is echoed and kept as a space. *)
let test_typed_c1 () =
  let t = Console_input.v () in
  let echo, submitted = Console_input.feed t "a\xc2\x9b2Jb\r" in
  Alcotest.(check bool)
    "no C1 control echoed" false
    (Display_harness.contains "\xc2\x9b" echo);
  Alcotest.check lines "line" [ "a 2Jb" ] submitted

(* A terminal in application cursor mode sends the arrows as ESC O A to
   ESC O D; they move as ESC [ A to ESC [ D do and insert nothing. *)
let test_application_arrows () =
  let t = Console_input.v ~history:[ "old" ] () in
  let _, submitted = Console_input.feed t "ab\027ODX\r" in
  Alcotest.check lines "left arrow" [ "aXb" ] submitted;
  let _, submitted = Console_input.feed t "\027OA\r" in
  Alcotest.check lines "up arrow" [ "aXb" ] submitted

(* A selector inserted before existing text must still join its base on
   the terminal, where the suffix has already been echoed. *)
let test_insert_emoji_before_text () =
  let editor = Console_input.v () in
  let input = "a\027[D\u{2764}\u{fe0f}" in
  let echo, _ = Console_input.feed editor input in
  Alcotest.(check string)
    "inserted before a" "\u{2764}\u{fe0f}a"
    (Console_input.pending editor);
  let actual = Console_vte.v ~raw:true ~rows:1 ~cols:20 ()
  and expected = Console_vte.v ~raw:true ~rows:1 ~cols:20 () in
  Console_vte.feed actual echo;
  Console_vte.feed expected "\u{2764}\u{fe0f}a";
  Alcotest.(check (list string))
    "the echo shows the selected emoji and its suffix"
    (Console_vte.screen expected)
    (Console_vte.screen actual);
  Alcotest.(check int)
    "cursor follows the two-cell emoji" 2
    (snd (Console_vte.cursor_pos actual))

let test_masked_history_seed () =
  let editor = Console_input.v ~mask:"*" ~history:[ "previous password" ] () in
  let _, submitted = Console_input.feed editor "new password\n" in
  Alcotest.(check lines)
    "the typed answer is returned" [ "new password" ] submitted;
  Alcotest.(check (list string))
    "a masked editor keeps no history, including its seed" []
    (Console_input.history editor)

let suite =
  ( "console_input",
    [
      Alcotest.test_case "a masked editor discards seeded history" `Quick
        test_masked_history_seed;
      Alcotest.test_case "insert an emoji before existing text" `Quick
        test_insert_emoji_before_text;
      Alcotest.test_case "typed C1 control is a space" `Quick test_typed_c1;
      Alcotest.test_case "application mode arrows" `Quick
        test_application_arrows;
      Alcotest.test_case "invalid UTF-8 is a replacement character" `Quick
        test_invalid_utf8;
      Alcotest.test_case "echo holds no escape from the caller" `Quick
        test_echo_is_text;
      Alcotest.test_case "completion inserts only complete UTF-8 characters"
        `Quick test_complete_utf8;
      Alcotest.test_case "printable" `Quick test_printable;
      Alcotest.test_case "a control sequence with intermediates" `Quick
        test_csi_intermediate;
      Alcotest.test_case "submit on CR" `Quick test_submit_cr;
      Alcotest.test_case "submit on LF" `Quick test_submit_lf;
      Alcotest.test_case "CRLF is one line" `Quick test_crlf_is_one_line;
      Alcotest.test_case "UTF-8 passthrough" `Quick test_utf8_passthrough;
      Alcotest.test_case "backspace" `Quick test_backspace;
      Alcotest.test_case "Ctrl-H backspace" `Quick test_backspace_ctrl_h;
      Alcotest.test_case "backspace on empty" `Quick test_backspace_on_empty;
      Alcotest.test_case "control chars ignored" `Quick
        test_control_chars_ignored;
      Alcotest.test_case "masked secret input" `Quick test_masked_input;
      Alcotest.test_case "a secret is the bytes typed" `Quick
        test_masked_keeps_bytes;
      Alcotest.test_case "split across feeds" `Quick test_split_across_feeds;
      Alcotest.test_case "split CRLF across feeds" `Quick
        test_split_crlf_across_feeds;
      Alcotest.test_case "cursor insert" `Quick test_cursor_insert;
      Alcotest.test_case "cursor backspace mid-line" `Quick
        test_cursor_backspace_midline;
      Alcotest.test_case "cursor right returns to end" `Quick
        test_cursor_right_returns_to_end;
      Alcotest.test_case "cursor left clamps" `Quick test_cursor_left_clamps;
      Alcotest.test_case "UTF-8 backspace whole char" `Quick
        test_utf8_backspace_whole_char;
      Alcotest.test_case "UTF-8 split across feeds" `Quick
        test_utf8_split_across_feeds;
      Alcotest.test_case "wide char backspace" `Quick
        test_wide_backspace_clears_two_cells;
      Alcotest.test_case "wide char left" `Quick test_wide_left_steps_two_cells;
      Alcotest.test_case "backspace erases a cluster" `Quick
        test_cluster_backspace;
      Alcotest.test_case "arrows step over a cluster" `Quick
        (Display_harness.skip_on_matrix_0_1
           "invariant-hq/mosaic@65da7988 (a combining mark after ASCII)"
           test_cluster_arrows);
      Alcotest.test_case "a cluster joins the one after the cursor" `Quick
        test_cluster_joins_after;
      Alcotest.test_case "history up" `Quick test_history_up;
      Alcotest.test_case "history up clamps" `Quick test_history_up_twice_clamps;
      Alcotest.test_case "history down restores draft" `Quick
        test_history_down_restores_draft;
      Alcotest.test_case "history erases on recall" `Quick
        test_history_erase_on_recall;
      Alcotest.test_case "history dedup consecutive" `Quick
        test_history_dedup_consecutive;
      Alcotest.test_case "history seed and order" `Quick
        test_history_seed_and_order;
      Alcotest.test_case "complete single" `Quick test_complete_single;
      Alcotest.test_case "complete common prefix" `Quick
        test_complete_common_prefix;
      Alcotest.test_case "complete lists candidates" `Quick
        test_complete_lists_candidates;
      Alcotest.test_case "complete last word" `Quick test_complete_last_word;
      Alcotest.test_case "complete none" `Quick test_complete_none;
    ] )
