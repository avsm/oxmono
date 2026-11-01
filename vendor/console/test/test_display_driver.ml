(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let check_lines msg expected actual =
  Alcotest.(check (list string)) msg expected actual

let theme = Theme.dos
let split = String.split_on_char '\n'

(* [untimed l] is [l] without the timer at its end. *)
let untimed = Re.replace_string (Re.Perl.compile_pat " +[0-9.]+s$") ~by:""

(* A Logs message inside a task is its output, as a line given to [log]
   is: it shows in the task's window, goes when the task succeeds, even
   once pushed out of the window, and is printed under the task's line,
   after the mark of its level, when it fails. *)
let test_logs_in_task () =
  let run ~fails =
    let t = terminal ~theme ~width:30 ~height:10 () in
    Task.run ~log_lines:1 t.root "job" (fun job ->
        Driver.report t.root `Warning "first";
        Task.log job "out";
        Display.redraw t.display;
        Alcotest.(check bool)
          "pushed out, not above" false
          (List.exists (contains "first") (screen t));
        Driver.report t.root `Info "last";
        if fails then Task.fail job);
    close t;
    List.map untimed (lines t)
  in
  check_lines "a success folds them away" [ "[+] job" ] (run ~fails:false);
  check_lines "a failure prints them all under its line"
    [ "[x] job"; "  ! first"; "  # out"; "  last" ]
    (run ~fails:true)

(* A Logs message with no current task goes above at once, a
   debug message after its own mark, an application or an information
   message after none. A task started with [task] is not current. *)
let test_logs_outside () =
  let report d =
    ignore (Task.start d "running");
    List.iter
      (fun (level, text) -> Driver.report d level text)
      [
        (`App, "app");
        (`Info, "info");
        (`Debug, "debug");
        (`Warning, "warn");
        (`Error, "error");
      ]
  in
  let expected = [ "app"; "info"; ". debug"; "! warn"; "x error" ] in
  let t = terminal ~theme ~width:30 ~height:10 () in
  report t.root;
  Display.redraw t.display;
  check_lines "above the rows, at once" expected
    (List.filteri (fun i _ -> i < 5) (screen t));
  close t;
  let (), out = plain ~theme (fun d _ -> report d) in
  check_lines "on a pipe"
    (("[-] running" :: expected) @ [ "[+] running  0.0s"; "" ])
    (split out);
  let (), out = plain (fun d _ -> Driver.report d `Debug "debug") in
  check_lines "the default theme's debug mark" [ "\u{00b7} debug"; "" ]
    (split out)

(* A driver's scope says which task is current, here a fiber's own. *)
let test_scope () =
  let current = ref None in
  let scope =
    Driver.scope
      ~current:(fun () -> !current)
      ~within:(fun task f ->
        let saved = !current in
        current := Some task;
        Fun.protect f ~finally:(fun () -> current := saved))
      ~cancelled:(fun _ -> false)
  in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d = Driver.root ~scope ~now:(fun () -> 0.) () in
  let display = Progress.show ppf d in
  let a = Task.start d "a" in
  current := Some a;
  Driver.report d `Info "in a";
  current := None;
  let is task = function Some c -> c == task | None -> false in
  Task.run d "b" (fun b ->
      Alcotest.(check bool) "current within" true (is b !current);
      Alcotest.(check bool)
        "the display's current task" true
        (is b (Driver.current d));
      Task.succeed b;
      Alcotest.(check bool)
        "none once it ended" true
        (Option.is_none (Driver.current d)));
  Alcotest.(check bool) "none outside" true (Option.is_none (Driver.current d));
  Display.close display;
  Format.pp_print_flush ppf ();
  check_lines "the scope's current task"
    [
      "[-] a"; "  a: in a"; "[-] b"; "[+] b  0.0s"; "[~] a  0.0s  cancelled"; "";
    ]
    (split (Buffer.contents buffer))

(* A signal handler runs at any safe point, such as in the middle of a
   write of the display. What it asks of the display through [when_idle]
   waits until that write is done: a suspension called from a write
   erases the region the screen shows, not one still to be written. *)
let test_when_idle () =
  let vte = Console_vte.v ~rows:8 ~cols:40 () in
  let buffer = Buffer.create 256 in
  let display = ref None and armed = ref false and seen = ref None in
  let suspend d =
    Console__.Display_driver.when_idle d (fun () ->
        Display.with_output d (fun _ -> seen := Some (Console_vte.screen vte)))
  in
  let out_flush () =
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    Console_vte.feed vte s;
    match !display with
    | Some d when !armed && s <> "" ->
        armed := false;
        suspend d
    | _ -> ()
  in
  let ppf = Format.make_formatter (Buffer.add_substring buffer) out_flush in
  let now, advance = clock () in
  let root = Task.root ~now () in
  let d = Progress.show ~size:(fun () -> (40, 8)) ppf root in
  display := Some d;
  let task = Task.start root "pausing" in
  advance 1.;
  armed := true;
  Display.redraw d;
  (match !seen with
  | None -> Alcotest.fail "no suspension"
  | Some screen ->
      Alcotest.(check bool)
        "the region is erased" false
        (List.exists (contains "pausing") screen));
  Alcotest.(check bool)
    "the region is drawn again" true
    (List.exists (contains "pausing") (Console_vte.screen vte));
  Task.succeed task;
  Task.succeed root;
  Display.close d

let suite =
  ( "display_driver",
    [
      Alcotest.test_case "a signal waits for the display" `Quick test_when_idle;
      Alcotest.test_case "Logs lines inside a task are its output" `Quick
        test_logs_in_task;
      Alcotest.test_case "Logs lines outside a task" `Quick test_logs_outside;
      Alcotest.test_case "a driver's scope" `Quick test_scope;
    ] )
