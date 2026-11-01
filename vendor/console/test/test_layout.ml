(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The region and the scrollback on a terminal: the summary last in every
   frame, a top task's tree moved whole when it ends, and a region that
   never scrolls the terminal. *)

open Console
open Display_harness

let check_lines msg expected actual =
  Alcotest.(check (list string)) msg expected actual

(* [squeeze l] is [l] with each run of spaces one space. *)
let squeeze = Re.replace_string (Re.Perl.compile_pat " +") ~by:" "

(* [untimed l] is [l] without the timer at its end. *)
let untimed = Re.replace_string (Re.Perl.compile_pat " +[0-9.]+s$") ~by:""

let last_row t =
  match List.rev (List.filter (( <> ) "") (screen t)) with
  | r :: _ -> r
  | [] -> ""

(* The summary is the last line of the region in every frame, whatever
   starts, ends or is printed, and the last line printed at close. *)
let test_summary_last () =
  let t = terminal ~width:40 ~height:10 ~label:"Build" () in
  let d = t.root in
  let check what =
    Display.redraw t.display;
    if not (contains "Build" (last_row t)) then
      Alcotest.failf "%s: the last row is %S" what (last_row t)
  in
  check "no task";
  let a = Task.start ~log_lines:2 d "a" in
  check "a task";
  let b = Task.start a "b" in
  check "a child";
  Task.log a "one";
  check "a line of output";
  Task.log d "outside";
  check "a line outside any task";
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "printed@.");
  check "a line printed past the display";
  Task.succeed b;
  check "the child ended";
  let c = Task.start d "c" in
  Task.succeed a;
  check "a top task ended";
  Task.fail c;
  check "the last task ended";
  close t;
  Alcotest.(check bool)
    "the last line printed" true
    (match List.rev (lines t) with
    | l :: _ -> contains "\u{2717} Build" l
    | [] -> false)

(* The tasks of the example: a step with a layer under it, which ends
   first. The layer waits in the region under its step, then goes to the
   scrollback with it, under it, and the summary is last. *)
let test_tree_moves () =
  let t = terminal ~width:60 ~height:10 ~label:"Building" () in
  let d = t.root in
  let step = Task.start d "RUN make" in
  let layer = Task.start step "sha256:abcd..." in
  Task.set_items step ~count:5 ~total:5;
  Task.succeed layer;
  Display.redraw t.display;
  Alcotest.(check int)
    "nothing in the scrollback yet" 0
    (Console_vte.scrollback_size t.vte);
  let bar = String.concat "" (List.init 22 (fun _ -> "\u{2501}")) in
  check_lines "the layer waits under its step"
    [
      "\u{280b} RUN make " ^ bar ^ " 5/5 0.0s";
      " \u{2514}\u{2500}\u{2500} \u{2713} sha256:abcd... 0.0s";
      "\u{2022} Building 0.0s (1/2)";
    ]
    (List.map squeeze (screen t));
  Task.succeed step;
  close t;
  check_lines "the step, its layer under it, the summary last"
    [
      "\u{2713} RUN make " ^ bar ^ " 5/5 0.0s";
      " \u{2514}\u{2500}\u{2500} \u{2713} sha256:abcd... 0.0s";
      "\u{2713} Building 0.0s (2/2)";
    ]
    (List.map squeeze (lines t))

(* Three levels, the endings interleaved: a top task moves whole when it
   ends, after the top tasks that ended before it, each in the order of its
   tree. *)
let test_three_levels () =
  let t = terminal ~width:40 ~height:12 ~label:"S" () in
  let d = t.root in
  let a = Task.start d "a" in
  let b = Task.start a "b" in
  let c = Task.start b "c" in
  let e = Task.start d "e" in
  let f = Task.start e "f" in
  let a2 = Task.start a "a2" in
  Task.succeed c;
  Task.fail f;
  Task.succeed e;
  Task.succeed a2;
  Display.redraw t.display;
  check_lines "e moved whole, a waits"
    [ "\u{2713} e"; "  \u{2514}\u{2500}\u{2500} \u{2717} f" ]
    (List.filteri (fun i _ -> i < 2) (List.map untimed (screen t)));
  Task.succeed b;
  Task.succeed a;
  close t;
  check_lines "each top task in the order of its tree"
    [
      "\u{2713} e";
      "  \u{2514}\u{2500}\u{2500} \u{2717} f";
      "\u{2713} a";
      "  \u{251c}\u{2500}\u{2500} \u{2713} b";
      "  \u{2502}   \u{2514}\u{2500}\u{2500} \u{2713} c";
      "  \u{2514}\u{2500}\u{2500} \u{2713} a2";
      "\u{2717} S 0.0s (6/6)";
    ]
    (List.map untimed (lines t))

(* A child that fails keeps its output, under its own line, in its place
   in the tree. *)
let test_failed_child_in_place () =
  let t = terminal ~width:40 ~height:12 () in
  let d = t.root in
  let a = Task.start d "a" in
  let b = Task.start ~log_lines:1 a "b" in
  let c = Task.start a "c" in
  Task.log b "b1";
  Task.log b "b2";
  Task.fail b;
  Task.succeed c;
  Task.succeed a;
  close t;
  check_lines "b's lines under b, c after them"
    [
      "\u{2713} a";
      "  \u{251c}\u{2500}\u{2500} \u{2717} b";
      "  \u{2502}   \u{203a} b1";
      "  \u{2502}   \u{203a} b2";
      "  \u{2514}\u{2500}\u{2500} \u{2713} c";
    ]
    (List.map untimed (lines t))

(* A tree too tall folds the tasks that ended under each task into one
   line, which says how many failed, and keeps every running one. *)
let test_fold () =
  let t = terminal ~width:40 ~height:6 ~label:"S" () in
  let d = t.root in
  let p = Task.start d "p" in
  for i = 1 to 6 do
    let c = Task.start p (Fmt.str "done %d" i) in
    if i mod 3 = 0 then Task.fail c
    else if i = 4 then Task.cancel c
    else Task.succeed c
  done;
  let r = Task.start p "running" in
  Display.redraw t.display;
  check_lines "folded"
    [
      "\u{280b} p";
      "  \u{251c}\u{2500}\u{2500} \u{2717} 6 done, 2 failed, 1 cancelled";
      "  \u{2514}\u{2500}\u{2500} \u{280b} running";
      "\u{2022} S 0.0s (6/8)";
    ]
    (List.map untimed (screen t));
  Alcotest.(check int) "nothing scrolled" 0 (Console_vte.scrollback_size t.vte);
  Task.succeed r;
  Task.succeed p;
  close t;
  Alcotest.(check int) "every task in the scrollback" 9 (List.length (lines t))

(* Output outside any task goes above the region at once and stays there,
   the region below it, the summary last. *)
let test_root_above () =
  let t = terminal ~width:40 ~height:10 ~label:"S" () in
  let d = t.root in
  let a = Task.start d "a" in
  Task.log d "outside";
  Display.redraw t.display;
  check_lines "above the region"
    [ "outside"; "\u{280b} a"; "\u{2022} S 0.0s (0/1)" ]
    (List.map untimed (screen t));
  Task.succeed a;
  close t;
  check_lines "it stays"
    [ "outside"; "\u{2713} a"; "\u{2713} S 0.0s (1/1)" ]
    (List.map untimed (lines t))

(* More running tasks than rows: no redraw scrolls the terminal, and the
   summary stays the last line. *)
let test_no_scroll () =
  let t = terminal ~width:40 ~height:6 ~label:"S" () in
  let d = t.root in
  let tasks =
    List.init 12 (fun i ->
        let task = Task.start ~log_lines:2 d (Fmt.str "t%d" i) in
        Task.log task "a line";
        Display.redraw t.display;
        Alcotest.(check int)
          "nothing scrolled" 0
          (Console_vte.scrollback_size t.vte);
        Alcotest.(check bool)
          "the summary last" true
          (match List.rev (screen t) with
          | l :: _ -> contains "S 0.0s" l
          | [] -> false);
        task)
  in
  Alcotest.(check bool)
    "within the screen less one" true
    (List.length (screen t) <= 5);
  List.iter Task.succeed tasks;
  close t

(* Window lines give way by age, including across a parent and its child. *)
let test_window_age () =
  let t = terminal ~width:60 ~height:4 () in
  let d = t.root in
  let parent = Task.start d "parent" in
  let child = Task.start parent "child" in
  Task.log child "older";
  t.advance 1.;
  Task.log parent "newer";
  Display.redraw t.display;
  let rows = screen t in
  close t;
  Alcotest.(check int) "the region fits" 3 (List.length rows);
  Alcotest.(check bool)
    "the newest window line stays" true
    (List.exists (contains "newer") rows);
  Alcotest.(check bool)
    "the oldest window line gives way" false
    (List.exists (contains "older") rows)

(* A single completed task is subject to the waiting-region bound too.
   Neither the caller nor the output sink retains the task or its label. *)
let test_oversized_waiting_task () =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) ignore in
  let root = Task.root ~now:(fun () -> 0.) () in
  let d = Progress.show ~size:(fun () -> (40, 8)) ppf root in
  let parent = Task.start root "parent" in
  let live_bytes () =
    Gc.full_major ();
    (Gc.stat ()).live_words * (Sys.word_size / 8)
  in
  let before = live_bytes () in
  Task.succeed (Task.start parent (String.make (4 * 1024 * 1024) 'x'));
  let retained = live_bytes () - before in
  Task.succeed (Task.start parent "replacement");
  Display.close d;
  if retained > 1_048_576 + 65_536 then
    Alcotest.failf "one completed child retains %d bytes (limit 1 MiB)" retained

let suite =
  ( "layout",
    [
      Alcotest.test_case "one oversized waiting task stays bounded" `Quick
        test_oversized_waiting_task;
      Alcotest.test_case "windows give way oldest first across tasks" `Quick
        test_window_age;
      Alcotest.test_case "the summary is the last line" `Quick test_summary_last;
      Alcotest.test_case "a tree moves whole" `Quick test_tree_moves;
      Alcotest.test_case "three levels, endings interleaved" `Quick
        test_three_levels;
      Alcotest.test_case "a failed child keeps its lines in place" `Quick
        test_failed_child_in_place;
      Alcotest.test_case "tasks that ended fold" `Quick test_fold;
      Alcotest.test_case "output outside a task goes above" `Quick
        test_root_above;
      Alcotest.test_case "no redraw scrolls the terminal" `Quick test_no_scroll;
    ] )
