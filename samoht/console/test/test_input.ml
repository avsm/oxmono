(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let lines = Alcotest.(list string)

(* ---- Line accumulation, echo, submit ----------------------------------- *)

let test_printable () =
  let t = Input.v () in
  let echo, ls = Input.feed t "abc" in
  Alcotest.(check string) "echoes the typed bytes" "abc" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string) "pending holds the line" "abc" (Input.pending t)

let test_submit_cr () =
  let t = Input.v () in
  let echo, ls = Input.feed t "abc\r" in
  Alcotest.(check string) "echo ends in CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "one completed line" [ "abc" ] ls;
  Alcotest.(check string) "pending cleared" "" (Input.pending t)

let test_submit_lf () =
  let t = Input.v () in
  let _, ls = Input.feed t "abc\n" in
  Alcotest.(check lines) "LF submits too" [ "abc" ] ls

let test_crlf_is_one_line () =
  let t = Input.v () in
  let echo, ls = Input.feed t "abc\r\n" in
  Alcotest.(check string) "echo is a single CRLF" "abc\r\n" echo;
  Alcotest.(check lines) "CRLF yields one line, no empty second" [ "abc" ] ls

let test_utf8_passthrough () =
  let t = Input.v () in
  let echo, _ = Input.feed t "caf\xc3\xa9" in
  Alcotest.(check string) "high bytes echoed verbatim" "caf\xc3\xa9" echo;
  Alcotest.(check string) "and stored" "caf\xc3\xa9" (Input.pending t)

(* ---- Backspace --------------------------------------------------------- *)

let test_backspace () =
  let t = Input.v () in
  let echo, ls = Input.feed t "ab\127c\r" in
  Alcotest.(check string)
    "erase echoes backspace-space-backspace" "ab\b \bc\r\n" echo;
  Alcotest.(check lines) "'b' erased, then 'c' appended" [ "ac" ] ls

let test_backspace_ctrl_h () =
  let t = Input.v () in
  let _, ls = Input.feed t "ab\008\r" in
  Alcotest.(check lines) "Ctrl-H also erases" [ "a" ] ls

let test_backspace_on_empty () =
  let t = Input.v () in
  let echo, ls = Input.feed t "\127" in
  Alcotest.(check string) "nothing to erase, no echo" "" echo;
  Alcotest.(check lines) "no line" [] ls

let test_control_chars_ignored () =
  let t = Input.v () in
  let echo, ls = Input.feed t "a\001\002b\r" in
  Alcotest.(check string)
    "control bytes neither echoed nor stored" "ab\r\n" echo;
  Alcotest.(check lines) "control bytes dropped" [ "ab" ] ls

let test_masked_input () =
  let t = Input.v ~mask:"*" () in
  let echo, ls = Input.feed t "s3cr3t" in
  Alcotest.(check string) "one mask per character" "******" echo;
  Alcotest.(check lines) "no line yet" [] ls;
  Alcotest.(check string)
    "secret retained for the caller" "s3cr3t" (Input.pending t);
  let echo, _ = Input.feed t "\127" in
  Alcotest.(check string) "backspace erases one mask" "\b \b" echo;
  let echo, ls = Input.feed t "\r" in
  Alcotest.(check string) "submit reveals nothing" "\r\n" echo;
  Alcotest.(check lines) "submitted value is unmasked" [ "s3cr3" ] ls;
  Alcotest.(check (list string))
    "secrets are never retained in history" [] (Input.history t)

(* ---- Feed boundaries --------------------------------------------------- *)

let test_split_across_feeds () =
  let t = Input.v () in
  let _, ls1 = Input.feed t "he" in
  Alcotest.(check lines) "nothing yet" [] ls1;
  let _, ls2 = Input.feed t "llo\r" in
  Alcotest.(check lines) "line accumulated across feeds" [ "hello" ] ls2

let test_split_crlf_across_feeds () =
  let t = Input.v () in
  let _, ls1 = Input.feed t "a\r" in
  Alcotest.(check lines) "CR submits" [ "a" ] ls1;
  let _, ls2 = Input.feed t "\nb\r" in
  Alcotest.(check lines) "trailing LF swallowed" [ "b" ] ls2

(* ---- Cursor movement and mid-line editing ------------------------------ *)

let left = "\027[D"
let right = "\027[C"

let test_cursor_insert () =
  (* "ab", Left (cursor before 'b'), insert 'X' -> "aXb" *)
  let t = Input.v () in
  let _ = Input.feed t ("ab" ^ left ^ "X") in
  Alcotest.(check string) "insert at the cursor" "aXb" (Input.pending t)

let test_cursor_backspace_midline () =
  (* "abc", Left (before 'c'), backspace deletes 'b' -> "ac" *)
  let t = Input.v () in
  let _ = Input.feed t ("abc" ^ left ^ "\127") in
  Alcotest.(check string)
    "backspace deletes before the cursor" "ac" (Input.pending t)

let test_cursor_right_returns_to_end () =
  (* "ab", Left then Right is back at the end, so 'X' appends -> "abX" *)
  let t = Input.v () in
  let _ = Input.feed t ("ab" ^ left ^ right ^ "X") in
  Alcotest.(check string) "right cancels the left" "abX" (Input.pending t)

let test_cursor_left_clamps () =
  (* Left past the start is a no-op; insert still goes at the front *)
  let t = Input.v () in
  let _ = Input.feed t ("ab" ^ left ^ left ^ left ^ "X") in
  Alcotest.(check string) "left clamps at the start" "Xab" (Input.pending t)

(* ---- UTF-8 ------------------------------------------------------------- *)

let test_utf8_backspace_whole_char () =
  (* backspace erases the whole two-byte 'e-acute', not one byte *)
  let t = Input.v () in
  let echo, _ = Input.feed t "caf\xc3\xa9\127" in
  Alcotest.(check string) "one cell erased" "caf\xc3\xa9\b \b" echo;
  Alcotest.(check string) "whole character gone" "caf" (Input.pending t)

let test_utf8_split_across_feeds () =
  (* the two bytes of a character arrive in separate feeds *)
  let t = Input.v () in
  let _, _ = Input.feed t "\xc3" in
  Alcotest.(check string)
    "incomplete character not yet in the line" "" (Input.pending t);
  let _, _ = Input.feed t "\xa9" in
  Alcotest.(check string)
    "completed once its bytes arrive" "\xc3\xa9" (Input.pending t)

(* ---- Wide characters (char_width hint) --------------------------------- *)

(* A hint of two cells per character must drive the cursor motion and the erase,
   not the character count. *)
let wide () = Input.v ~char_width:(fun _ -> 2) ()

let test_wide_backspace_clears_two_cells () =
  let echo, _ = Input.feed (wide ()) "a\127" in
  Alcotest.(check string) "two cells cleared" "a\b\b  \b\b" echo

let test_wide_left_steps_two_cells () =
  let echo, _ = Input.feed (wide ()) ("ab" ^ "\027[D") in
  Alcotest.(check string) "left steps over two cells" "ab\b\b" echo

(* ---- History ----------------------------------------------------------- *)

let up = "\027[A"
let down = "\027[B"

let test_history_up () =
  let t = Input.v () in
  let _ = Input.feed t "abc\r" in
  let echo, ls = Input.feed t up in
  Alcotest.(check string)
    "recalled line echoed (empty buffer, no erase)" "abc" echo;
  Alcotest.(check lines) "no submit" [] ls;
  Alcotest.(check string) "pending is the recalled line" "abc" (Input.pending t)

let test_history_up_twice_clamps () =
  let t = Input.v () in
  let _ = Input.feed t "one\r" in
  let _ = Input.feed t "two\r" in
  let _ = Input.feed t up in
  Alcotest.(check string) "first up = newest" "two" (Input.pending t);
  let _ = Input.feed t up in
  Alcotest.(check string) "second up = older" "one" (Input.pending t);
  let _ = Input.feed t up in
  Alcotest.(check string) "third up clamps at oldest" "one" (Input.pending t)

let test_history_down_restores_draft () =
  let t = Input.v () in
  let _ = Input.feed t "old\r" in
  let _ = Input.feed t "dra" in
  let _ = Input.feed t up in
  Alcotest.(check string) "up recalls history" "old" (Input.pending t);
  let _ = Input.feed t down in
  Alcotest.(check string)
    "down restores the in-progress draft" "dra" (Input.pending t)

let test_history_erase_on_recall () =
  let t = Input.v () in
  let _ = Input.feed t "abcd\r" in
  let _ = Input.feed t "xy" in
  let echo, _ = Input.feed t up in
  Alcotest.(check string)
    "erases the two draft chars then writes recalled line" "\b \b\b \babcd" echo

let test_history_dedup_consecutive () =
  let t = Input.v () in
  let _ = Input.feed t "x\r" in
  let _ = Input.feed t "x\r" in
  Alcotest.(check lines)
    "consecutive duplicate kept once" [ "x" ] (Input.history t)

let test_history_seed_and_order () =
  let t = Input.v ~history:[ "first"; "second" ] () in
  let _ = Input.feed t "third\r" in
  Alcotest.(check lines)
    "oldest first, then new"
    [ "first"; "second"; "third" ]
    (Input.history t)

(* ---- Tab completion ---------------------------------------------------- *)

let test_complete_single () =
  let t = Input.v ~complete:(fun _ -> [ "select" ]) () in
  let _ = Input.feed t "sel" in
  let echo, _ = Input.feed t "\t" in
  Alcotest.(check string) "tab echoes only the completion suffix" "ect" echo;
  Alcotest.(check string) "pending completed" "select" (Input.pending t)

let test_complete_common_prefix () =
  let t = Input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let _ = Input.feed t "co\t" in
  Alcotest.(check string)
    "extends to longest common prefix" "con" (Input.pending t)

let test_complete_lists_candidates () =
  let t = Input.v ~complete:(fun _ -> [ "connections"; "config" ]) () in
  let echo, _ = Input.feed t "co\t" in
  Alcotest.(check bool)
    "lists both candidates" true
    (let re = Re.compile (Re.str "connections  config") in
     Re.execp re echo)

let test_complete_last_word () =
  let complete w = if String.equal w "co" then [ "connections" ] else [] in
  let t = Input.v ~complete () in
  let _ = Input.feed t "SELECT * FROM co\t" in
  Alcotest.(check string)
    "completes only the trailing word" "SELECT * FROM connections"
    (Input.pending t)

let test_complete_none () =
  let t = Input.v ~complete:(fun _ -> []) () in
  let _ = Input.feed t "zz" in
  let echo, _ = Input.feed t "\t" in
  Alcotest.(check string) "no candidates, tab echoes nothing" "" echo;
  Alcotest.(check string) "pending unchanged" "zz" (Input.pending t)

let test_crlf () =
  Alcotest.(check string) "LF becomes CRLF" "a\r\nb\r\n" (Input.crlf "a\nb\n");
  Alcotest.(check string) "no LF unchanged" "abc" (Input.crlf "abc")

let suite =
  ( "input",
    [
      Alcotest.test_case "printable" `Quick test_printable;
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
      Alcotest.test_case "crlf output translation" `Quick test_crlf;
    ] )
