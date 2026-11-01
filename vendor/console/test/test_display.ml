(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness
open View_harness

let strings = Alcotest.(list string)
let check_lines msg expected actual = Alcotest.check strings msg expected actual
let theme = Theme.dos

(* [untimed l] is [l] without the timer at its end. *)
let untimed = Re.replace_string (Re.Perl.compile_pat " +[0-9.]+s$") ~by:""

(* [above top root] is the view of [top] above the default view, which gives
   way first when they do not fit. *)
let above top root = View.vcat [ top; Progress.view root ]

(* A root timed by [now] whose tasks show on [ppf], and the display. *)
let show ?progress ?size ?label ?layout ?view ?keep_ended ~now ppf =
  let root = Task.root ?label ~now () in
  let view = Option.map (fun f -> f root) view in
  (root, Progress.show ?progress ?size ?layout ?view ?keep_ended ppf root)

let test_plain_lines () =
  let (), out =
    plain ~theme ~label:"Building" (fun d advance ->
        let t = Task.start d "compile" in
        Task.set_items t ~count:2 ~total:2;
        Task.log t "compiled parser.ml";
        advance 1.5;
        Task.log t "slow";
        Task.succeed t;
        Driver.report d `Info "done")
  in
  check_lines "lines in turn, the header last"
    [
      "[-] compile";
      "  # compile: compiled parser.ml";
      "  # compile: slow  +1.5s";
      "[+] compile  2/2  1.5s";
      "done";
      "[+] Building 1.5s (1/1)";
      "";
    ]
    (String.split_on_char '\n' out)

(* A line the display prints starts at the first column, even after the
   program left a line unfinished on the same formatter. *)
let test_plain_column () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~now:(fun () -> 0.) ppf in
  Fmt.pf ppf "partial%!";
  Driver.report d `Info "next";
  Fmt.pf ppf "again";
  Task.succeed (Task.start d "t");
  Display.close display;
  Fmt.pf ppf "after@.";
  check_lines "each display line on its own"
    [ "partial"; "next"; "again"; "[-] t"; "[+] t  0.0s"; "after"; "" ]
    (String.split_on_char '\n' (Buffer.contents buffer))

(* A task still running when the work ends ends with it: it is cancelled
   when the work is, and succeeds when the work succeeds. *)
let test_plain_unfinished () =
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Task.start d "left");
        Task.cancel d)
  in
  check_lines "cancelled"
    [ "[-] left"; "[~] left  0.0s  cancelled"; "" ]
    (String.split_on_char '\n' out);
  let (), out =
    plain ~theme (fun d _ ->
        ignore (Task.start d "left");
        Task.succeed ~message:"done" d)
  in
  check_lines "succeeded with the work"
    [ "[-] left"; "[+] left  0.0s"; "[+] done"; "" ]
    (String.split_on_char '\n' out)

(* Without a terminal no colour may tell a failure from a success: the
   closing line of a task that did not succeed says how it ended, in the
   muted style of its time. *)
let test_plain_outcome_word () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.succeed (Task.start d "ok");
        let a = Task.start d "a" in
        Task.set_items a ~count:2 ~total:2;
        Task.fail a;
        Task.cancel (Task.start d "b"))
  in
  check_lines "the outcome after the time"
    [
      "[-] ok";
      "[+] ok  0.0s";
      "[-] a";
      "[x] a  2/2  0.0s  failed";
      "[-] b";
      "[~] b  0.0s  cancelled";
      "";
    ]
    (String.split_on_char '\n' out);
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_style_renderer ppf `Ansi_tty;
  let d, display = show ~now:(fun () -> 0.) ppf in
  Task.fail (Task.start d "a");
  Display.close display;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "muted with the time" true
    (contains "0.0s  failed\027[" (Buffer.contents buffer))

let test_plain_attribution () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start d "a" and b = Task.start d "b" in
        Task.log a "one";
        Task.log b "two";
        Task.log a "three";
        Task.fail a;
        Task.cancel b)
  in
  check_lines "each line says its task, in order"
    [
      "[-] a";
      "[-] b";
      "  # a: one";
      "  # b: two";
      "  # a: three";
      "[x] a  0.0s  failed";
      "[~] b  0.0s  cancelled";
      "";
    ]
    (String.split_on_char '\n' out)

(* A tab in a task's output goes to the next stop of eight columns from the
   start of its line, so tabbed columns stay aligned. *)
let test_tabs () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start d "a" in
        Task.log a "x\ty\n\u{754c}\tz\n12345678\tw";
        Task.fail a)
  in
  check_lines "tab stops"
    [
      "[-] a";
      "  # a: x       y";
      "  # a: \u{754c}      z";
      "  # a: 12345678        w";
      "[x] a  0.0s  failed";
      "";
    ]
    (String.split_on_char '\n' out)

let test_plain_sanitizes () =
  let (), out =
    plain ~label:"Build\027[2J" (fun d _ ->
        let task = Task.start d "a\tb" in
        Task.log task "x\007y";
        Task.succeed task)
  in
  Alcotest.(check bool) "no escape" false (String.contains out '\027');
  Alcotest.(check bool) "log text" true (contains "x y" out)

let test_plain_result () =
  let (), out =
    plain ~theme ~label:"Work" (fun d _ ->
        ignore (Task.start d "left running");
        Task.log d "see the log";
        Task.fail ~message:"it broke" d;
        Task.succeed ~message:"changed my mind" d)
  in
  check_lines "running tasks cancelled, verdict last"
    [
      "[-] left running";
      "see the log";
      "[~] left running  0.0s  cancelled";
      "[x] Work 0.0s (1/1)";
      "[x] it broke";
      "";
    ]
    (String.split_on_char '\n' out)

let test_empty_header_pending () =
  let t = terminal ~label:"Building" () in
  Alcotest.(check bool)
    "pending before any task" true
    (List.exists (contains "\xe2\x80\xa2 Building 0.0s (0/0)") (screen t));
  close t;
  Alcotest.(check bool)
    "succeeded once closed" true
    (List.exists (contains "\xe2\x9c\x93 Building 0.0s (0/0)") (lines t))

let test_region () =
  let t = terminal ~theme ~width:40 ~label:"Build" () in
  let a = Task.start t.root "fetch" in
  let b = Task.start t.root "compile" in
  Task.set_items b ~count:1 ~total:4;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool) "first row" true (contains "fetch" (List.nth rows 0));
  Alcotest.(check bool) "second row" true (contains "compile" (List.nth rows 1));
  Alcotest.(check bool) "its count" true (contains "1/4" (List.nth rows 1));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows 2));
  Task.succeed a;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the ended task is above the region" true
    (String.starts_with ~prefix:"[+] fetch" (List.nth rows 0));
  Alcotest.(check bool)
    "the summary still last" true
    (contains "[-] Build" (List.nth rows 2));
  Task.succeed b;
  close t;
  check_lines "final screen"
    [
      "[+] fetch                           0.0s";
      "[+] compile \
       \u{2588}\u{2588}\u{2588}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}\u{2591}     \
       1/4   0.0s";
      "[+] Build 0.0s (2/2)";
    ]
    (lines t);
  Alcotest.(check bool) "cursor shown" true (contains "\027[?25h" (written t));
  Alcotest.(check bool)
    "no bare line feed" false
    (Re.execp (Re.Perl.compile_pat "(^|[^\r])\n") (written t))

(* A view with a table taller than the room: the table loses its first
   lines, and the tasks and the summary stay. *)
let test_tall_header () =
  let t = terminal ~width:40 ~height:8 ~label:"S" () in
  let rows =
    View.vcat (List.init 20 (fun i -> View.text (Fmt.str "row %d" i)))
  in
  Display.set_view t.display (above rows t.root);
  let a = Task.start t.root "a" in
  Display.redraw t.display;
  let shown = List.map untimed (screen t) in
  Alcotest.(check int) "the screen less one" 7 (List.length shown);
  check_lines "the table's last lines"
    [ "row 15"; "row 16"; "row 17"; "row 18"; "row 19" ]
    (List.filteri (fun i _ -> i < 5) shown);
  Alcotest.(check bool) "the task" true (contains " a" (List.nth shown 5));
  Alcotest.(check bool) "the summary" true (contains "S" (List.nth shown 6));
  Task.succeed a;
  close t

(* With the tty in raw mode a line feed keeps the column, so a line that
   began with a bare line feed would start where the one above it ended: the
   screen of a raw tty is that of a cooked one. *)
let test_raw_tty () =
  let run raw =
    let t = terminal ~theme ~width:40 ~label:"Build" ~raw () in
    let a = Task.start t.root "fetch" in
    let b = Task.start t.root "compile" in
    Task.set_items b ~count:1 ~total:4;
    Display.redraw t.display;
    let live = screen t in
    Task.succeed a;
    Driver.report t.root `Info "note";
    Task.set_items b ~count:2 ~total:4;
    Display.redraw t.display;
    let later = screen t in
    Task.succeed b;
    close t;
    (live, later, lines t)
  in
  let live, later, final = run false in
  let live', later', final' = run true in
  check_lines "the region" live live';
  check_lines "the region after a line above it" later later';
  check_lines "the final screen" final final'

(* The host left "Fetching " unended. The region starts on the next row,
   so its first line, nearly as wide as the screen, does not wrap there and
   a repaint erases all of it. *)
let test_after_a_partial_line () =
  let run before =
    let t = terminal ~theme ~width:40 ~before ~label:(String.make 28 'h') () in
    Display.redraw t.display;
    let a = Task.start t.root "fetch" in
    Display.redraw t.display;
    let first = screen t in
    Task.succeed a;
    close t;
    (first, lines t)
  in
  let first, final = run "" in
  let first', final' = run "Fetching " in
  let used rows = List.rev (List.drop_while (( = ) "") (List.rev rows)) in
  check_lines "the region below the host's line" ("Fetching" :: used first)
    (used first');
  check_lines "the final screen" ("Fetching" :: final) final'

(* While a task runs, its window shows its latest lines of both kinds, a line
   too wide cut there and only there. *)
let test_window () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let wide = "wide " ^ String.make 40 'w' in
  Task.run ~log_lines:3 t.root "job" (fun job ->
      List.iter (Task.log job) [ "one"; "two" ];
      Driver.report t.root `Info "note";
      Task.log job wide;
      Display.redraw t.display;
      let rows = screen t in
      Alcotest.(check bool) "the row" true (contains "job" (List.nth rows 0));
      check_lines "the latest three lines"
        [ "  # two"; "  note"; "  # " ^ String.sub wide 0 25 ^ "\u{2026}" ]
        (List.filteri (fun i _ -> i > 0) rows));
  close t;
  check_lines "a success folds them all away" [ "[+] job   0.0s" ] (lines t)

(* The cell where [needle] starts in [row]. *)
let cell_of needle row =
  let rec find i =
    if i + String.length needle > String.length row then
      Alcotest.failf "%S is not in %S" needle row
    else if String.sub row i (String.length needle) = needle then i
    else find (i + 1)
  in
  View.width (View.text (String.sub row 0 (find 0)))

let test_alignment () =
  let t = terminal ~theme ~width:40 () in
  let a = Task.start t.root "\xe7\x95\x8c\xe7\x95\x8c wide" in
  let b =
    Task.start t.root "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9 emoji"
  in
  let c = Task.start t.root "plain" in
  Task.set_items a ~count:1 ~total:10;
  Task.set_items b ~count:10 ~total:100;
  Task.set_items c ~count:3 ~total:3;
  List.iter Task.succeed [ a; b; c ];
  close t;
  let rows = lines t in
  let one msg cols =
    Alcotest.(check int)
      (msg ^ " in\n" ^ String.concat "\n" rows)
      1
      (List.length (List.sort_uniq Int.compare cols))
  in
  one "counts end in one column"
    (List.map2
       (fun n r -> cell_of n r + String.length n)
       [ "1/10"; "10/100"; "3/3" ]
       rows);
  one "timers end in one column" (List.map (fun r -> cell_of "0.0s" r + 4) rows)

let test_label_truncated () =
  let t = terminal ~theme ~width:20 () in
  ignore (Task.start t.root "a label much too long for the row");
  Display.redraw t.display;
  let row = List.nth (screen t) 0 in
  Alcotest.(check bool) "ellipsis" true (contains "\xe2\x80\xa6" row);
  Alcotest.(check bool) "fits" true (View.width (View.text row) <= 20)

(* More running tasks than rows: the latest show, after a line that counts
   the others, and the summary stays last. No redraw scrolls the terminal. *)
let test_capped () =
  let t = terminal ~theme ~width:30 ~height:5 ~label:"H" () in
  let tasks =
    List.init 8 (fun i ->
        let task = Task.start t.root (Fmt.str "task %d" i) in
        Display.redraw t.display;
        task)
  in
  Display.redraw t.display;
  let rows = List.filter (( <> ) "") (screen t) in
  check_lines "no taller than the screen less one"
    [
      "  \u{2026} 6 more running";
      " |  task 6";
      " |  task 7";
      "[-] H 0.0s (0/8)";
    ]
    (List.map untimed rows);
  Alcotest.(check int) "nothing scrolled" 0 (Console_vte.scrollback_size t.vte);
  List.iter Task.cancel tasks;
  close t

(* Eight running tasks with a log line each fill a budget of eight lines
   exactly: the windows give way and every task shows. Two rows fewer, and
   only the latest tasks show, a line counting the others. *)
let test_marker_counts () =
  let run height =
    let t = terminal ~theme ~width:30 ~height ~label:"H" () in
    let tasks =
      List.init 8 (fun i ->
          let task = Task.start ~log_lines:1 t.root (Fmt.str "task %d" i) in
          Task.log task "line";
          task)
    in
    Display.redraw t.display;
    let rows = List.filter (( <> ) "") (screen t) in
    List.iter Task.cancel tasks;
    close t;
    List.map untimed rows
  in
  check_lines "the windows give way"
    (List.init 8 (Fmt.str " |  task %d") @ [ "[-] H 0.0s (0/8)" ])
    (run 10);
  check_lines "the latest tasks"
    [
      "  \u{2026} 3 more running";
      " |  task 3";
      " |  task 4";
      " |  task 5";
      " |  task 6";
      " |  task 7";
      "[-] H 0.0s (0/8)";
    ]
    (run 8)

let test_marker_fits () =
  let t = terminal ~theme ~width:12 ~height:4 () in
  let tasks = List.init 6 (fun i -> Task.start t.root (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  (* A row wider than the terminal would wrap onto a fourth. *)
  Alcotest.(check int)
    "three rows" 3
    (List.length (List.filter (( <> ) "") (screen t)));
  List.iter Task.cancel tasks;
  close t

let test_with_output () =
  let t = terminal ~theme ~width:30 () in
  let a = Task.start t.root "job" in
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "hello@.");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check string) "printed above" "hello" (List.nth rows 0);
  Alcotest.(check bool) "region below" true (contains "job" (List.nth rows 1));
  (match Display.with_output t.display (fun _ -> failwith "boom") with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  Display.redraw t.display;
  Alcotest.(check bool)
    "region drawn again after a raise" true
    (List.exists (contains "job") (screen t));
  Task.succeed a;
  close t

(* Output without a line end is ended before the region is drawn again. *)
let test_with_output_partial () =
  let t = terminal ~theme ~width:30 () in
  let a = Task.start t.root "job" in
  Display.with_output t.display (fun ppf -> Fmt.pf ppf "partial");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check string) "its own line" "partial" (List.nth rows 0);
  Alcotest.(check bool) "region below" true (contains "job" (List.nth rows 1));
  Task.succeed a;
  close t

(* Bytes of the program's that move the cursor act above the region, and
   the region is drawn whole below them, on rows it erases first. The tty
   is raw, so that a line feed does not return the cursor either. *)
let test_with_output_moves () =
  List.iter
    (fun move ->
      let t = terminal ~theme ~raw:true ~width:30 () in
      let a = Task.start t.root "job" in
      Display.with_output t.display (fun ppf ->
          Fmt.pf ppf "ZZZZZZZZZZ%sZZZZZZZZZZ@." move);
      Display.redraw t.display;
      let row = List.find (contains "job") (screen t) in
      Alcotest.(check bool)
        (Fmt.str "%S: the row is its own: %S" move row)
        true
        (String.starts_with ~prefix:" |  job" row
        && not (String.contains row 'Z'));
      Task.succeed a;
      close t)
    [ "\027[A"; "\027[2A\r"; "\r"; "\027[K"; "\027[H" ]

let test_redraw_idle () =
  let t = terminal ~theme () in
  ignore (Task.start t.root "job");
  Display.redraw t.display;
  let before = List.length !(t.chunks) in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "nothing written" before (List.length !(t.chunks));
  close t

(* A terminal that narrows reflows the old frame onto more rows; the redraw
   moves up over all of them. *)
let test_narrowing () =
  let t = terminal ~theme ~width:40 ~height:8 ~label:"Header" () in
  Task.set_items (Task.start t.root "job") ~count:1 ~total:4;
  Display.redraw t.display;
  t.chunks := [];
  resize t ~width:20 ~height:8;
  Display.redraw t.display;
  (* Two lines of 40 cells take four rows at 20. *)
  Alcotest.(check bool)
    "moved up over the reflowed rows" true
    (contains "\r\027[3A\027[J" (written t));
  close t

let test_height_changes () =
  let t = terminal ~theme ~width:30 ~height:4 () in
  let tasks = List.init 5 (fun i -> Task.start t.root (Fmt.str "t%d" i)) in
  Display.redraw t.display;
  resize t ~width:30 ~height:10;
  Display.redraw t.display;
  resize t ~width:30 ~height:3;
  Display.redraw t.display;
  Alcotest.(check bool)
    "no line wider than the terminal" true
    (List.for_all (fun r -> View.width (View.text r) <= 30) (screen t));
  List.iter Task.succeed tasks;
  close t

let test_nested () =
  let t = terminal ~width:30 ~height:10 () in
  let p = Task.start t.root "parent" in
  let c1 = Task.start p "one" in
  let _c2 = Task.start p "two" in
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "branch" true
    (contains "\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80" (List.nth rows 1));
  Alcotest.(check bool)
    "last" true
    (contains "\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80" (List.nth rows 2));
  Task.succeed c1;
  Display.redraw t.display;
  Alcotest.(check bool)
    "an ended child stays under its parent" true
    (contains "\u{2713} one" (List.nth (screen t) 1));
  Task.fail p;
  let late = Task.start p "late" in
  Task.succeed late;
  close t;
  check_lines "the parent first, each child under it, in start order"
    [
      "\u{2717} parent";
      "  \u{251c}\u{2500}\u{2500} \u{2713} one";
      "  \u{2514}\u{2500}\u{2500} \u{25cb} two";
      "\u{25cb} late";
    ]
    (List.map untimed (lines t))

(* A task that its parent ends still says how it ended, on a pipe too. *)
let test_nested_plain () =
  let (), out =
    plain ~theme (fun d _ ->
        let p = Task.start d "parent" in
        let c1 = Task.start p "one" in
        ignore (Task.start p "two");
        Task.succeed c1;
        Task.fail p)
  in
  check_lines "a start line and an end line each"
    [
      "[-] parent";
      "[-] parent \u{25B8} one";
      "[-] parent \u{25B8} two";
      "[+] parent \u{25B8} one  0.0s";
      "[~] parent \u{25B8} two  0.0s  cancelled";
      "[x] parent  0.0s  failed";
      "";
    ]
    (String.split_on_char '\n' out)

let test_keys_and_transient () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start ~key:"k" d "first" in
        let b = Task.start ~key:"k" d "second" in
        Alcotest.(check bool) "same task" true (a == b);
        Task.succeed a;
        let c = Task.start ~key:"k" d "third" in
        Alcotest.(check bool) "a new task once it ended" false (a == c);
        Task.succeed c;
        Task.succeed (Task.start ~transient:true d "gone"))
  in
  check_lines "no line for a transient task"
    [ "[-] first"; "[+] first  0.0s"; "[-] third"; "[+] third  0.0s"; "" ]
    (String.split_on_char '\n' out)

(* A with_task that finds its key running enters that task and ends
   nothing: the task ends as the call that started it says. *)
let test_with_task_shared_key () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.run ~key:"k" d "first" (fun a ->
            Task.run ~key:"k" d "second" (fun b ->
                Alcotest.(check bool) "same task" true (a == b);
                Task.log b "inside");
            Task.log a "after the second";
            match Task.run ~key:"k" d "third" (fun _ -> raise Exit) with
            | () -> Alcotest.fail "the exception was swallowed"
            | exception Exit -> Task.log a "after the third"))
  in
  check_lines "one task, ended once by the first"
    [
      "[-] first";
      "  # first: inside";
      "  # first: after the second";
      "  # first: after the third";
      "[+] first  0.0s";
      "";
    ]
    (String.split_on_char '\n' out)

let test_counts () =
  let (), out =
    plain ~theme (fun d _ ->
        let t = Task.start d "count" in
        Task.set_items t ~count:5 ~total:10;
        Task.set_items t ~count:3 ~total:8;
        Task.set_items t ~count:12 ~total:10;
        Task.succeed t;
        let b = Task.start d "bytes" in
        Task.set_bytes b ~count:1048576L ~total:2097152L;
        check_invalid "Console.Task.set_items: the task counts bytes" (fun () ->
            Task.set_items b ~count:1 ~total:1);
        Task.succeed b)
  in
  check_lines "never back, total raised"
    [
      "[-] count";
      "[+] count  12/12  0.0s";
      "[-] bytes";
      "[+] bytes  1.00 MiB / 2.00 MiB  0.0s";
      "";
    ]
    (String.split_on_char '\n' out)

(* [label ~style] draws the label in [style], bold by default. *)
let test_label_style () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let task = Task.start root "job" in
  let drawn layout = View.to_string ~styled:true (Progress.row ~layout task) in
  Alcotest.(check string)
    "bold" "\027[1mjob\027[0m"
    (drawn Progress.Layout.(label ()));
  Alcotest.(check string)
    "faint" "\027[2mjob\027[0m"
    (drawn Progress.Layout.(label ~style:Style.faint ()))

(* [pp_bytes] formats a count of bytes as a row's count and rate do. *)
let test_pp_bytes () =
  List.iter
    (fun (n, s) ->
      Alcotest.(check string) s s (Fmt.str "%a" Progress.pp_bytes n))
    [
      (512L, "512 B");
      (1572864L, "1.50 MiB");
      (1023L, "1023 B");
      (1048570L, "1023.99 KiB");
      (1048575L, "1.00 MiB");
      (-1L, "0 B");
    ]

(* [choose f] draws each running task's row by the layout [f] gives it. *)
let test_layout_choose () =
  let layout =
    Progress.Layout.(
      choose (fun task ->
          if Task.label task = "plain" then label ()
          else list [ const (View.text "*"); label () ]))
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let a = Task.start t.root "plain" and b = Task.start t.root "starred" in
  Display.redraw t.display;
  check_lines "a layout each" [ "plain"; "*starred" ] (screen t);
  Task.succeed a;
  Task.succeed b;
  close t

(* The label column counts the labels the rows draw: a running row whose
   layout has no label does not widen it. *)
let test_label_column_drawn () =
  let layout =
    Progress.Layout.(
      choose (fun task ->
          if Task.label task = "unlabelled and long" then
            const (View.text "bytes")
          else list [ label (); const (View.text " |") ]))
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let a = Task.start t.root "short" in
  let b = Task.start t.root "unlabelled and long" in
  Display.redraw t.display;
  check_lines "as wide as the drawn labels" [ "short |"; "bytes" ] (screen t);
  Task.succeed a;
  Task.succeed b;
  close t

(* A transient task that succeeded leaves the tree and the children of its
   parent, its line and all. *)
let test_transient_child_leaves () =
  let t = terminal ~theme ~width:60 () in
  let parent = Task.start t.root "resolve" in
  let child = Task.start ~transient:true parent "download" in
  Task.set_bytes child ~count:10L ~total:10L;
  t.advance 1.;
  Task.succeed child;
  Display.redraw t.display;
  Alcotest.(check bool)
    "no line in the region" false
    (List.exists (contains "download") (screen t));
  Alcotest.(check bool)
    "no line in its parent's tree" false
    (contains "download" (View.to_string (Progress.tree parent)));
  Alcotest.(check int) "no child" 0 (List.length (Task.children parent));
  Task.succeed parent;
  close t

(* With [keep_ended], the tasks that ended stay on a pipe too, so that the
   last frame sees them. *)
let test_keep_ended_plain () =
  let children = ref [] in
  let view root =
    View.fit (fun _ ->
        children :=
          List.concat_map
            (fun t -> List.map Task.label (Task.children t))
            (Task.children root);
        View.empty)
  in
  let (), _ =
    plain ~keep_ended:true ~view (fun root _ ->
        Task.run root "step" (fun step ->
            Task.succeed (Task.start step "layer")))
  in
  check_lines "the step's tasks, at the end" [ "layer" ] !children

(* A count near [max_int] still shows its share. *)
let test_large_counts () =
  let layout =
    Progress.Layout.(list [ label (); const (View.text " "); percent ])
  in
  let t = terminal ~theme ~layout ~width:30 () in
  let task = Task.start t.root "big" in
  Task.set_items task ~count:(max_int / 2) ~total:max_int;
  Display.redraw t.display;
  Alcotest.(check bool)
    "half" true
    (contains " 49%" (List.nth (screen t) 0)
    || contains " 50%" (List.nth (screen t) 0));
  Task.succeed task;
  close t

let test_invalid () =
  let d, display =
    show ~now:(fun () -> 0.) (Format.formatter_of_buffer (Buffer.create 1))
  in
  check_invalid "Console.Task.start: empty key" (fun () ->
      ignore (Task.start ~key:"" d "x"));
  check_invalid "Console.Task.start: negative log_lines" (fun () ->
      ignore (Task.start ~log_lines:(-1) d "x"));
  let t = Task.start d "t" in
  check_invalid "Console.Task.set_items: negative progress" (fun () ->
      Task.set_items t ~count:(-1) ~total:1);
  check_invalid "Console.Progress.Layout.bar: negative width" (fun () ->
      ignore (Progress.Layout.bar ~width:(-1) ()));
  List.iter
    (fun n ->
      check_invalid "Console.Progress.Layout.give_way: stage below 1" (fun () ->
          ignore (Progress.Layout.give_way n Progress.Layout.rate)))
    [ 0; -3 ];
  check_invalid "Console.Progress.Layout: more than one label" (fun () ->
      ignore Progress.Layout.(label () ++ label ()));
  check_invalid "Console.Progress.Layout: more than one spacer" (fun () ->
      ignore Progress.Layout.(list [ spacer; label (); spacer ]));
  Display.close display

(* [size] has no documented bound, and a terminal reports what it is told
   ($COLUMNS under [`Tty] without a terminal): a display on a terminal of
   any width runs, and writes in proportion to what it draws, not to the
   width. *)
let test_any_width () =
  List.iter
    (fun w ->
      let buf = Buffer.create 256 in
      let ppf = Format.formatter_of_buffer buf in
      match
        let d, display =
          show ~size:(fun () -> (w, 24)) ~now:(fun () -> 0.) ppf
        in
        Task.run d "work" (fun t -> Task.set_items t ~count:1 ~total:2);
        Display.close display
      with
      | () ->
          Format.pp_print_flush ppf ();
          if Buffer.length buf > 100_000 then
            Alcotest.failf "width %d: %d bytes written" w (Buffer.length buf)
      | exception e -> Alcotest.failf "width %d: %s" w (Printexc.to_string e))
    [ 1_000_000; Sys.max_string_length; max_int ]

(* A fiber that outlives the display, or a library that holds on to it,
   is ignored once it closed: nothing raises and nothing is written. *)
let test_closed () =
  let t = terminal ~theme ~label:"Build" () in
  let d = t.display in
  let running = Task.start t.root "running" in
  Display.close d;
  let before = written t in
  Display.close d;
  let late = Task.start t.root "late" in
  List.iter
    (fun task ->
      Task.set_label task "renamed";
      Task.set_items task ~count:1 ~total:2;
      Task.log task "late line";
      Task.succeed task;
      Task.fail task;
      Task.cancel task)
    [ running; late ];
  Display.redraw d;
  Display.set_view d (View.text "view");
  Display.print d (View.text "printed");
  Task.succeed ~message:"late" t.root;
  Alcotest.(check string) "nothing written" before (written t);
  Alcotest.(check int)
    "with_output still calls its function" 42
    (Display.with_output d (fun _ -> 42))

let test_colours () =
  let t = terminal ~styled:true () in
  let a = Task.start t.root "job" in
  Task.fail a;
  close t;
  let out = written t in
  (* The default theme's red is the terminal's own. *)
  Alcotest.(check bool) "red failure" true (contains "\027[1;31m" out);
  Alcotest.(check bool) "styles closed" true (contains "\027[0m" out)

(* On a terminal wider than 100 columns, the rows and the lines of ended
   tasks keep to 100, so that a count and a timer stay near their label. *)
let test_wide () =
  let t = terminal ~width:160 ~height:8 () in
  let task = Task.start t.root ("compile " ^ String.make 62 'x') in
  Task.set_items task ~count:1 ~total:3;
  Task.log task "a log line";
  Display.redraw t.display;
  let widths () = List.map Width.string_width (lines t) in
  Alcotest.(check int) "the live row" 100 (List.fold_left max 0 (widths ()));
  Task.succeed task;
  close t;
  Alcotest.(check int)
    "the line it ends on" 100
    (List.fold_left max 0 (widths ()))

(* On a terminal, each write of the display is one synchronized update, so
   that a terminal that knows the mode shows a frame whole; a pipe gets no
   such escape. *)
let test_synchronized () =
  let begin_ = "\027[?2026h" and end_ = "\027[?2026l" in
  let t = terminal ~label:"Build" () in
  let task = Task.start t.root "a" in
  t.advance 0.2;
  Display.redraw t.display;
  Task.log task "x";
  Task.succeed task;
  close t;
  let occurrences sub s = List.length (Astring.String.cuts ~sep:sub s) - 1 in
  List.iter
    (fun c ->
      if
        not
          (String.starts_with ~prefix:begin_ c
          && String.ends_with ~suffix:end_ c
          && occurrences begin_ c = 1
          && occurrences end_ c = 1)
      then Alcotest.failf "not one synchronized update: %S" c)
    !(t.chunks);
  let (), out = plain (fun d _ -> Task.succeed (Task.start d "a")) in
  Alcotest.(check bool) "none on a pipe" false (contains "?2026" out)

let split out = String.split_on_char '\n' out

(* On a pipe there is no window: a task says when it starts, each of its lines
   comes at once after its label, and it says when it ends. *)
let test_pipe_lines () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.run d "job" (fun job ->
            Task.log job "one";
            Driver.report d `Info "note";
            Task.log job "two"))
  in
  check_lines "in turn, each after its task's label"
    [
      "[-] job";
      "  # job: one";
      "  job: note";
      "  # job: two";
      "[+] job  0.0s";
      "";
    ]
    (split out)

(* [plain] prints the program's own lines on a pipe, in place of those of
   the tasks; the output of no task and the last frame print as before, and
   a terminal ignores it. *)
let test_plain_own_lines () =
  let plain e =
    match (Task.Event.kind e, Task.Event.task e) with
    | `End, Some (_, path) ->
        Some (View.text ("done " ^ String.concat "/" path))
    | _ -> None
  in
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let root = Task.root ~label:"Build" ~now:(fun () -> 0.) () in
  let d = Progress.show ~progress:`Plain ~plain ppf root in
  Task.run root "job" (fun job ->
      Task.log job "one";
      Task.log root "above");
  Task.succeed root;
  Display.close d;
  Format.pp_print_flush ppf ();
  check_lines "the program's lines"
    [ "above"; "done job"; "[+] Build 0.0s (1/1)"; "" ]
    (split (Buffer.contents buffer));
  let t = terminal ~theme ~plain () in
  Task.succeed (Task.start t.root "job");
  close t;
  Alcotest.(check bool)
    "rows on a terminal" true
    (List.exists (contains "[+] job") (lines t))

(* With [delay], the region shows only once the display has run that long,
   while what is printed goes out at once; work done sooner leaves only
   its last frame. *)
let test_delay () =
  let t = terminal ~theme ~label:"Build" ~delay:0.5 () in
  let job = Task.start t.root "job" in
  Display.print t.display (View.text "printed");
  Display.redraw t.display;
  check_lines "nothing yet but the print" [ "printed" ] (screen t);
  t.advance 0.6;
  Display.redraw t.display;
  Alcotest.(check bool)
    "the rows once due" true
    (List.exists (contains "job") (screen t));
  Task.succeed job;
  close t;
  let t = terminal ~theme ~label:"Build" ~delay:0.5 () in
  Task.succeed (Task.start t.root "quick");
  Display.redraw t.display;
  close t;
  Alcotest.(check bool)
    "no region for a quick work" false
    (contains "\027[?25l" (written t));
  Alcotest.(check bool)
    "its last frame" true
    (List.exists (contains "Build") (lines t))

(* The start line is written when the task starts, so a task that hangs
   leaves a trace. *)
let test_pipe_start () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~now:(fun () -> 0.) ppf in
  let p = Task.start d "fetch" in
  ignore (Task.start p "index");
  Format.pp_print_flush ppf ();
  check_lines "the start lines, at once"
    [ "[-] fetch"; "[-] fetch \u{25B8} index"; "" ]
    (split (Buffer.contents buffer));
  Display.close display

(* A line the program printed goes on over the rows below as the terminal
   wraps it, mid-word, while a line after a mark goes on under its text,
   between words. *)
let test_root_line_wraps () =
  let t = terminal ~theme ~width:10 ~height:6 () in
  Task.log t.root "Get:1 [72.7 kB]";
  Driver.report t.root `Warning "aaaa bbbbbbb";
  close t;
  check_lines "as a terminal, then under the mark"
    [ "Get:1 [72."; "7 kB]"; "! aaaa"; "  bbbbbbb" ]
    (lines t)

(* A task that succeeds drops its output, on a terminal, even the
   lines that left its window. *)
let test_success_drops () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let job = Task.start ~log_lines:2 t.root "job" in
  List.iter (Task.log job) [ "one"; "two"; "three" ];
  Display.redraw t.display;
  Alcotest.(check bool)
    "not above while it runs" false
    (List.exists (contains "one") (screen t));
  Task.succeed job;
  close t;
  check_lines "only its line" [ "[+] job   0.0s" ] (lines t)

(* A task that fails prints all its output under its line, in one
   block, without its label and not cut: a line too wide goes on over the
   rows below. [with_task] fails its task when its function raises. *)
let test_failure_block () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let wide = String.make 45 'w' in
  (match
     Task.run ~log_lines:2 t.root "job" (fun job ->
         List.iter (Task.log job) [ "l1"; "l2"; "l3"; wide ];
         failwith "boom")
   with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  close t;
  let rows = lines t in
  Alcotest.(check string)
    "the failure first" "[x] job: Failure(\"boom\")"
    (untimed (List.hd rows));
  check_lines "then the block"
    [ "  # l1"; "  # l2"; "  # l3" ]
    (List.filteri (fun i _ -> i >= 1 && i <= 3) rows);
  let rest = List.filteri (fun i _ -> i > 3) rows in
  Alcotest.(check string)
    "the wide line whole" ("  # " ^ wide)
    (String.concat ""
       (List.mapi
          (fun i l -> if i = 0 then l else String.sub l 4 (String.length l - 4))
          rest));
  Alcotest.(check bool)
    "no label under the line" false
    (List.exists (contains "job:") (List.tl rows))

(* The block holds at most 1 MiB, the latest lines, and one line
   says how many were dropped. *)
let test_failure_cap () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let job = Task.start t.root "job" in
  let n = 1200 in
  let line i = Fmt.str "line%04d%s" i (String.make 992 'x') in
  for i = 0 to n - 1 do
    Task.log job (line i)
  done;
  t.chunks := [];
  Task.fail job;
  let out = written t in
  let tokens =
    Re.all (Re.Perl.compile_pat "line([0-9]{4})") out
    |> List.map (fun g -> int_of_string (Re.Group.get g 1))
  in
  let first = List.fold_left min n tokens in
  check_lines "one line per line kept"
    (List.init (n - first) (fun i -> Fmt.str "%d" (first + i)))
    (List.map string_of_int tokens);
  let size i j =
    List.fold_left ( + ) 0
      (List.init (j - i) (fun k -> String.length (line (i + k)) + 1))
  in
  Alcotest.(check bool) "at most 1 MiB" true (size first n <= 1048576);
  Alcotest.(check bool) "as much as fits" true (size (first - 1) n > 1048576);
  Alcotest.(check bool)
    "how many were dropped" true
    (contains (Fmt.str "%d lines dropped" first) out);
  close t

(* On a pipe nothing is kept: every line was printed when it came, and
   none is printed again under the failure. *)
let test_failure_plain () =
  let n = 2000 in
  let (), out =
    plain ~theme (fun d _ ->
        let job = Task.start ~log_lines:2 d "job" in
        for i = 0 to n - 1 do
          Task.log job (Fmt.str "line%04d %s" i (String.make 600 'x'))
        done;
        Task.fail job)
  in
  let lines = split out in
  Alcotest.(check int)
    "every line once, and the task's two lines"
    (n + 2 + 1)
    (List.length lines);
  let tail = List.filteri (fun i _ -> i >= n + 1) lines in
  check_lines "nothing again under the failure"
    [ "[x] job  0.0s  failed"; "" ]
    tail

(* A task that is cancelled, as Ctrl-C cancels them all, prints all its
   output under its line, as a failure does, the lines that left its window
   too. *)
let test_cancelled_output () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let job = Task.start ~log_lines:2 t.root "job" in
  let other = Task.start ~log_lines:1 t.root "other" in
  List.iter (Task.log job) [ "a"; "b"; "c" ];
  Task.log other "z";
  Task.cancel job;
  Display.close t.display;
  check_lines "all of it under each line"
    [ "[~] job"; "  # a"; "  # b"; "  # c"; "[~] other"; "  # z" ]
    (List.map untimed (lines t));
  let (), out =
    plain ~theme (fun d _ ->
        let job = Task.start ~log_lines:2 d "job" in
        Task.log job "a";
        Task.cancel job)
  in
  check_lines "nothing repeated on a pipe"
    [ "[-] job"; "  # job: a"; "[~] job  0.0s  cancelled"; "" ]
    (split out)

(* The root is the work's top task: within it, output goes above the rows
   at once and stays, while a task runs and when that task fails, and also
   once the work ended. Ending it ends the work, once. *)
let test_root () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let d = t.root in
  let root = d in
  (match
     Task.run ~log_lines:2 d "job" (fun job ->
         Task.log job "mine";
         Task.within root (fun () -> Driver.report d `Warning "above");
         Task.log root "also above";
         Display.redraw t.display;
         check_lines "above the rows at once"
           [ "! above"; "also above" ]
           (List.filteri (fun i _ -> i < 2) (screen t));
         Alcotest.(check bool)
           "back in the task" true
           (match Driver.current d with Some c -> c == job | None -> false);
         failwith "boom")
   with
  | () -> Alcotest.fail "no exception"
  | exception Failure _ -> ());
  Task.set_items root ~count:1 ~total:2;
  Task.set_bytes root ~count:1L ~total:2L;
  Alcotest.(check bool)
    "the root counts nothing" true
    (Task.progress root = `Spin);
  Task.succeed root;
  Task.fail root;
  Task.cancel root;
  Alcotest.(check bool)
    "the first ending counts" true
    (Task.state root = `Succeeded);
  Task.log root "still there";
  Display.close t.display;
  check_lines "they stay, the task's own under its failure"
    [
      "! above";
      "also above";
      "[x] job: Failure(\"boom\")";
      "  # mine";
      "still there";
    ]
    (List.map untimed (lines t));
  let (), out =
    plain ~theme (fun d _ ->
        Task.run d "job" (fun _ ->
            Task.within d (fun () ->
                Driver.report d `Info "above";
                Task.succeed (Task.start d "top"));
            Driver.report d `Info "inside"))
  in
  check_lines "on a pipe, unlabelled"
    [
      "[-] job";
      "above";
      "[-] top";
      "[+] top  0.0s";
      "  job: inside";
      "[+] job  0.0s";
      "";
    ]
    (split out)

(* [within task f] makes a task that runs current for [f], and ends nothing:
   what [f] prints and the tasks it starts are [task]'s, also when [f]
   raises. *)
let test_within () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start d "a" in
        Task.run d "b" (fun _ ->
            let v =
              Task.within a (fun () ->
                  Driver.report d `Info "for a";
                  ignore (Task.start a "under a");
                  7)
            in
            Alcotest.(check int) "its value" 7 v;
            Driver.report d `Info "for b");
        (match Task.within a (fun () -> failwith "inside") with
        | () -> Alcotest.fail "no exception"
        | exception Failure _ -> ());
        Task.log a "still runs";
        Task.succeed a)
  in
  check_lines "the lines of the task given"
    [
      "[-] a";
      "[-] b";
      "  a: for a";
      "[-] a \u{25B8} under a";
      "  b: for b";
      "[+] b  0.0s";
      "  # a: still runs";
      "[+] a \u{25B8} under a  0.0s";
      "[+] a  0.0s";
      "";
    ]
    (split out)

(* Two tasks that run at once: on a terminal each block is printed under
   its own line, never interleaved with the other's. *)
let test_two_tasks () =
  let t = terminal ~theme ~width:30 ~height:10 () in
  let a = Task.start t.root "a" and b = Task.start t.root "b" in
  List.iter
    (fun (task, s) -> Task.log task s)
    [ (a, "a1"); (b, "b1"); (a, "a2"); (b, "b2") ];
  Task.fail a;
  Task.fail b;
  close t;
  check_lines "one block each"
    [ "[x] a"; "  # a1"; "  # a2"; "[x] b"; "  # b1"; "  # b2" ]
    (List.map untimed (lines t))

(* [with_task]: the task is current for its function, so tasks and Logs
   messages inside it go under it; it succeeds when the function returns,
   unless the function ended it first. *)
let test_with_task () =
  let v, out =
    plain ~theme (fun d _ ->
        Task.run d "outer" (fun outer ->
            Task.run outer "inner" (fun _ -> Driver.report d `Info "deep");
            ignore (Task.start outer "child");
            Task.run outer "ended" (fun t ->
                Task.fail t;
                Driver.report d `Info "after its end";
                Task.log t "ignored");
            42))
  in
  Alcotest.(check int) "its value" 42 v;
  check_lines "nested under the task given"
    [
      "[-] outer";
      "[-] outer \u{25B8} inner";
      "  outer \u{25B8} inner: deep";
      "[+] outer \u{25B8} inner  0.0s";
      "[-] outer \u{25B8} child";
      "[-] outer \u{25B8} ended";
      "[x] outer \u{25B8} ended  0.0s  failed";
      "  outer \u{25B8} ended: after its end";
      "[+] outer \u{25B8} child  0.0s";
      "[+] outer  0.0s";
      "";
    ]
    (split out);
  let (), out =
    plain ~theme (fun d _ ->
        Task.run d "outer" (fun _ ->
            let top = Task.start d "top" in
            Driver.report d `Info "to outer";
            Task.succeed top))
  in
  check_lines "a task goes under the task given, output to the current one"
    [
      "[-] outer";
      "[-] top";
      "  outer: to outer";
      "[+] top  0.0s";
      "[+] outer  0.0s";
      "";
    ]
    (split out)

(* A task that [with_task] fails when its function raises says why, in its
   line and in its JSON end event. *)
let test_with_task_reason () =
  let run progress =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    let d, display = show ~progress ~now:(fun () -> 0.) ppf in
    (try Task.run d "fetch" (fun _ -> failwith "disk full")
     with Failure _ -> ());
    Display.close display;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Alcotest.(check bool)
    "in the line" true
    (contains "[x] fetch: Failure(\"disk full\")" (run `Plain));
  Alcotest.(check bool)
    "in the end event" true
    (contains "\"status\":\"failed\",\"elapsed\":0,\"message\":\"fetch: Failure"
       (run `Json))

(* [children] are the tasks under a task, the top tasks under the root,
   those that ended among them while their top task runs. *)
let test_children () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let d = t.root in
  let a = Task.start d "a" and b = Task.start d "b" in
  let a1 = Task.start a "a1" in
  let a2 = Task.start a "a2" in
  Task.succeed a1;
  Alcotest.(check int) "the top tasks" 2 (List.length (Task.children d));
  Alcotest.(check bool)
    "the tasks under a" true
    (match Task.children a with [ x; y ] -> x == a1 && y == a2 | _ -> false);
  Alcotest.(check int) "none under b" 0 (List.length (Task.children b));
  close t

(* [output] is the lines a task keeps, oldest first, and [dropped] counts
   the oldest it let go past 1 MiB; a task that succeeded keeps none, and
   the root has none. *)
let test_output () =
  let t = terminal ~theme ~width:40 ~height:10 () in
  let top = Task.start t.root "top" in
  let job = Task.start top "job" in
  Task.log job "one\ntwo";
  check_lines "its lines" [ "one"; "two" ] (Task.output job);
  Alcotest.(check int) "none dropped" 0 (Task.dropped job);
  let big = String.make 1023 'x' in
  for _ = 1 to 1100 do
    Task.log job big
  done;
  let kept = Task.output job in
  Alcotest.(check int)
    "the oldest dropped" 1102
    (List.length kept + Task.dropped job);
  Alcotest.(check bool) "the latest kept" true (List.hd kept = big);
  Task.fail job;
  Alcotest.(check int)
    "a failure keeps its lines" (List.length kept)
    (List.length (Task.output job));
  let ok = Task.start top "ok" in
  Task.log ok "done";
  Task.succeed ok;
  check_lines "a success keeps none" [] (Task.output ok);
  Task.log t.root "above";
  check_lines "the root has none" [] (Task.output t.root);
  Task.succeed top;
  close t

(* [rate] is the count per second that the rate column shows, once the
   task counted. *)
let test_rate () =
  let t = terminal ~theme ~width:100 () in
  let job = Task.start t.root "job" in
  Alcotest.(check (option (float 1e-9))) "none yet" None (Task.rate job);
  t.advance 2.;
  Task.set_bytes job ~count:2048L ~total:4096L;
  Alcotest.(check (option (float 1e-9)))
    "bytes per second" (Some 1024.) (Task.rate job);
  Display.redraw t.display;
  Alcotest.(check bool)
    "as the row shows it" true
    (List.exists (contains "1.00 KiB/s") (screen t));
  Task.succeed job;
  close t

(* A task whose count follows a list. *)
let test_iter () =
  let seen = ref [] in
  let (), out =
    plain ~theme (fun d _ ->
        let items = [ 1; 2; 3 ] in
        let total = List.length items in
        Task.run d "items" (fun t ->
            Task.set_items t ~count:0 ~total;
            List.iteri
              (fun i x ->
                seen := x :: !seen;
                Task.set_items t ~count:(i + 1) ~total)
              items))
  in
  Alcotest.(check (list int)) "in order" [ 1; 2; 3 ] (List.rev !seen);
  check_lines "counted" [ "[-] items"; "[+] items  3/3  0.0s"; "" ] (split out)

let steps =
  Table.view
    [
      Table.column (View.text "step");
      Table.column ~align:`Right (View.text "time");
    ]
    [
      [ View.text "fetch"; View.text "1.2s" ];
      [ View.text "compile"; View.text "3.4s" ];
    ]

(* [pp v] printed on a formatter in [theme], at [margin]. *)
let printed ?(theme = theme) ~margin pp v =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  Format.pp_set_margin ppf margin;
  pp ppf v;
  Format.pp_print_flush ppf ();
  String.split_on_char '\n' (Buffer.contents buf)

let table ~width = View.sized ~width:(min width 100) steps

(* A view of the display's room: [f root ~width ~height], the width and the
   height of the region. *)
let fit root f =
  View.fit (fun room ->
      let width = Option.value (Room.width room) ~default:80
      and height = Option.value (Room.height room) ~default:max_int in
      f root ~width ~height)

(* The default view in a room [width] wide and [height] tall. *)
let default root ~width ~height = View.sized ~width ~height (Progress.view root)

(* A view is a span: here a table above the default view, the tree of the
   tasks and then the summary, which stays the last line. The view is given
   the terminal's width and its height less one. *)
let test_view () =
  let t = terminal ~theme ~width:40 ~height:12 ~label:"Build" () in
  let heights = ref [] in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         heights := height :: !heights;
         above (table ~width) d));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let table = printed ~margin:40 View.pp steps in
  let rows = screen t in
  let n = List.length table in
  check_lines "the table first" table (List.filteri (fun i _ -> i < n) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows n));
  Alcotest.(check bool)
    "the summary last" true
    (contains "[-] Build" (List.nth rows (n + 1)));
  Alcotest.(check int) "the room" 11 (List.hd !heights)

(* A view line wider than the terminal is cut, and a view taller than the
   room loses its first lines, so that the summary at its bottom stays. *)
let test_view_fits () =
  let t = terminal ~theme ~width:20 ~height:6 ~label:"Build" () in
  let tall = View.text "a view line much wider than twenty\nb\nc\nd\ne\nf" in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat [ tall; default d ~width ~height:2 ]));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let rows = screen t in
  check_lines "the last five lines" [ "d"; "e"; "f" ]
    (List.filteri (fun i _ -> i < 3) rows);
  Alcotest.(check bool) "the row" true (contains "compile" (List.nth rows 3));
  Alcotest.(check bool)
    "the summary" true
    (contains "[-] Build" (List.nth rows 4));
  Alcotest.(check int) "within the screen less one" 5 (List.length rows);
  Display.set_view t.display (View.text "a view line much wider than twenty\nb");
  check_lines "cut to the width"
    [ "a view line much wi\u{2026}"; "b" ]
    (screen t);
  close t

(* On a terminal too short for the view, the view loses its first lines,
   and a table cut in its height goes whole, beside text in a hcat too: the
   region shows no fragment of its frame. *)
let test_view_short () =
  let t = terminal ~theme ~width:40 ~height:6 ~label:"Build" () in
  let table = View.sized ~width:30 steps in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat [ table; default d ~width ~height:2 ]));
  ignore (Task.start t.root "compile");
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check int) "the tree and the summary" 2 (List.length rows);
  Alcotest.(check bool)
    "nothing of the table" false
    (List.exists (fun r -> contains "+-" r || contains "s |" r) rows);
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ ->
         View.vcat
           [
             View.text "a\nb\nc";
             View.hcat [ table; View.text "x" ];
             View.text "d\ne";
             default d ~width ~height:2;
           ]));
  check_lines "past the table, the text" [ "d"; "e" ]
    (List.filteri (fun i _ -> i < 2) (screen t));
  close t

(* On a wide terminal the view is given its width, and the tree keeps to
   100 columns. *)
let test_view_width () =
  let t = terminal ~theme ~width:160 () in
  let given = ref 0 in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         given := width;
         default d ~width ~height));
  Task.set_items
    (Task.start t.root ("compile " ^ String.make 62 'x'))
    ~count:1 ~total:3;
  Display.redraw t.display;
  Alcotest.(check int) "the width" 160 !given;
  Alcotest.(check int) "the row" 100 (Width.string_width (List.hd (screen t)))

(* The default view and the summary take the width they are given, so a
   view puts them in a column narrower than the terminal. *)
let test_default_view_column () =
  let t = terminal ~theme ~width:120 ~height:12 ~label:"Building" () in
  let panel = View.sized ~width:30 (Panel.view (View.text "status")) in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height ->
         View.hcat [ default d ~width:(width - 32) ~height; panel ]));
  let step = Task.start t.root "compile" in
  Task.set_items step ~count:1 ~total:2;
  Display.redraw t.display;
  let view = default t.root ~width:30 ~height:10 in
  Alcotest.(check bool) "the view in 30 cells" true (View.width view <= 30);
  Alcotest.(check bool)
    "the summary in 12" true
    (View.width (View.sized ~width:12 (Progress.summary t.root)) <= 12);
  Alcotest.(check bool)
    "the panel on the screen" true
    (List.for_all (fun r -> Width.string_width r <= 120) (screen t)
    && List.exists (contains "status") (screen t));
  close t

(* Widgets side by side make a view, each at the width it is given. *)
let test_view_hcat () =
  let t = terminal ~theme ~width:40 ~height:12 ~label:"Build" () in
  let panel = Panel.view (View.text "status") in
  let side width =
    let half = (width - 2) / 2 in
    View.hcat [ View.sized ~width:half steps; View.sized ~width:half panel ]
  in
  Display.set_view t.display
    (fit t.root (fun d ~width ~height:_ -> above (side width) d));
  Display.redraw t.display;
  let want = printed ~margin:40 View.pp (side 40) in
  let rows = screen t in
  check_lines "side by side" want
    (List.filteri (fun i _ -> i < List.length want) rows);
  Alcotest.(check bool)
    "on one line" true
    (contains "step" (List.nth rows 1) && contains "status" (List.nth rows 1))

(* A view replaces the one before, and its last frame stays once the
   display closes. *)
let test_view_replaced () =
  let t = terminal ~theme ~width:30 () in
  Display.set_view t.display (View.text "first");
  Display.redraw t.display;
  Display.set_view t.display (View.text "second");
  check_lines "the second view, at once" [ "second" ] (screen t);
  close t;
  check_lines "its last frame stays" [ "second" ] (lines t)

(* Without a terminal a view is drawn once, when the display closes, after
   the tasks' lines. *)
let test_view_plain () =
  let (), out =
    plain ~theme ~label:"Build"
      ~view:(above (View.text "TOP"))
      (fun d _ -> Task.succeed (Task.start d "compile"))
  in
  check_lines "the last frame, at the end"
    [ "[-] compile"; "[+] compile  0.0s"; "TOP"; "[+] Build 0.0s (1/1)"; "" ]
    (split out)

let border_glyphs =
  List.concat_map
    (fun b ->
      let c = Border.chars b in
      [
        c.top_left;
        c.top;
        c.top_right;
        c.left;
        c.right;
        c.bottom_left;
        c.bottom;
        c.bottom_right;
        c.cross;
        c.top_cross;
        c.bottom_cross;
        c.left_cross;
        c.right_cross;
      ])
    Border.[ ascii; single; rounded; double; heavy ]

let clusters = Width.clusters

(* A table's frame moves in a live display drawn in a theme with motion:
   from one frame to the next, only its border cells change, each to a glyph
   as wide, so that every line keeps its width and its cells their place. *)
let test_view_moves () =
  let run theme ~styled =
    let t = terminal ~theme ~styled ~width:40 ~height:12 () in
    Display.set_view t.display
      (fit t.root (fun _ ~width ~height:_ -> table ~width));
    Display.redraw t.display;
    let frames =
      List.init 12 (fun _ ->
          let before = List.length !(t.chunks) in
          t.advance 0.25;
          Display.redraw t.display;
          (List.length !(t.chunks) > before, screen t))
    in
    close t;
    frames
  in
  let still = printed ~theme:Theme.matrix ~margin:40 View.pp steps in
  let frames = run Theme.matrix ~styled:true in
  Alcotest.(check bool)
    "frames are drawn" true
    (List.exists (fun (wrote, _) -> wrote) frames);
  List.iter
    (fun (_, rows) ->
      List.iteri
        (fun i (still, row) ->
          let a = clusters still and b = clusters row in
          if List.length a <> List.length b then
            Alcotest.failf "row %d: %S became %S" i still row;
          List.iter2
            (fun a b ->
              if
                a <> b
                && not
                     (List.mem a border_glyphs
                     && Width.string_width a = Width.string_width b)
              then Alcotest.failf "row %d: %S became %S" i still row)
            a b)
        (List.combine still
           (List.filteri (fun i _ -> i < List.length still) rows)))
    frames;
  let unmoved frames =
    List.for_all (fun (wrote, rows) -> (not wrote) && rows = still) frames
  in
  Alcotest.(check bool)
    "still without colour" true
    (unmoved (run Theme.matrix ~styled:false));
  Alcotest.(check bool)
    "still once stilled" true
    (unmoved (run (Theme.still Theme.matrix) ~styled:true))

(* A table in [Border.simple] has no frame to move: in a theme with motion,
   every frame of a view is the still one. *)
let test_simple_view_stands_still () =
  let simple =
    Table.view ~border:Border.simple
      [
        Table.column (View.text "step");
        Table.column ~align:`Right (View.text "time");
      ]
      [
        [ View.text "fetch"; View.text "1.2s" ];
        [ View.text "compile"; View.text "3.4s" ];
      ]
  in
  List.iter
    (fun theme ->
      let t = terminal ~theme ~styled:true ~width:40 ~height:12 () in
      Display.set_view t.display simple;
      Display.redraw t.display;
      let still = screen t in
      for _ = 1 to 12 do
        t.advance 0.25;
        Display.redraw t.display;
        Alcotest.(check (list string)) "still" still (screen t)
      done;
      close t)
    Theme.[ matrix; rainbow; tokyo_night ]

(* A broken view must not prevent teardown from restoring the terminal: it
   is dropped, and the default view drawn in its place. *)
let test_view_exception_close () =
  let t = terminal ~styled:true ~width:40 ~height:10 () in
  ignore (Task.start t.root "running");
  (match Display.set_view t.display (View.fit (fun _ -> raise Exit)) with
  | () -> Alcotest.fail "view exception was swallowed"
  | exception Exit -> ());
  Alcotest.(check bool)
    "the default view drawn" true
    (List.exists (contains "running") (screen t));
  let raised =
    try
      close t;
      false
    with Exit -> true
  in
  Alcotest.(check bool)
    "closing does not call the broken view again" false raised;
  Alcotest.(check bool)
    "cursor restored after the view exception" true
    (Console_vte.cursor_visible t.vte)

(* A view that raises in a frame that no call to the display asked for, the
   first one or the last one, is dropped for the default view, the display
   still closes, and the next redraw or the close raises its exception. *)
let test_view_exception_later () =
  let t =
    terminal ~width:40 ~height:10
      ~view:(fun _ -> View.fit (fun _ -> raise Exit))
      ()
  in
  ignore (Task.start t.root "running");
  Alcotest.check_raises "the redraw raises" Exit (fun () ->
      Display.redraw t.display);
  Alcotest.(check bool)
    "the default view drawn" true
    (List.exists (contains "running") (screen t));
  close t;
  let last root =
    View.fit (fun room ->
        match Room.height room with
        | None -> raise Exit
        | Some _ -> Progress.view root)
  in
  let t = terminal ~label:"work" ~width:40 ~height:10 ~view:last () in
  Display.redraw t.display;
  Alcotest.check_raises "the close raises" Exit (fun () -> close t);
  Alcotest.(check bool)
    "the default view printed" true
    (List.exists (contains "work") (lines t));
  Alcotest.(check bool)
    "the cursor shown" true
    (Console_vte.cursor_visible t.vte);
  Alcotest.check_raises "on a pipe" Exit (fun () ->
      ignore (plain ~view:last (fun _ _ -> ())))

(* On a narrow terminal the label is what tells two rows apart: it gives
   way last, after the bar, and the row still ends with its timer. *)
let test_label_before_bar () =
  let t = terminal ~theme ~width:40 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
  t.advance 1.5;
  Display.redraw t.display;
  let rows = screen t in
  Alcotest.(check bool)
    "the parent's label shows" true
    (List.exists (contains "RUN") rows);
  Alcotest.(check bool)
    "the child's label shows" true
    (List.exists (contains "sha") rows);
  Alcotest.(check bool)
    "the timers show" true
    (List.for_all
       (fun r -> (not (contains "\xe2\x80\xa6" r)) || contains "1.5s" r)
       rows);
  close t

(* The same in two trees side by side, as examples/columns lays them out on
   75 columns. *)
let test_label_before_bar_hcat () =
  let t = terminal ~theme ~width:75 ~height:12 () in
  let view d ~width ~height =
    let half = (min width 100 - 2) / 2 in
    match Task.children d with
    | [ a; b ] ->
        View.hcat
          [
            View.sized ~width:half ~height (Progress.tree a);
            View.sized ~width:half ~height (Progress.tree b);
          ]
    | _ -> default d ~width ~height
  in
  Display.set_view t.display (fit t.root view);
  let a = Task.start t.root "linux" and b = Task.start t.root "macos" in
  let ca = Task.start a "compile" in
  let cb = Task.start b "compile" in
  Task.set_items ca ~count:4 ~total:12;
  Task.set_items cb ~count:2 ~total:12;
  Display.redraw t.display;
  Alcotest.(check bool)
    "compile shows in a 36-cell tree" true
    (List.exists (contains "comp") (screen t));
  close t

(* When the display closes, the scrollback keeps each task's label readable:
   a label that does not fit next to its count goes on below, never one
   character a line. *)
let test_last_frame_label_readable () =
  let t = terminal ~theme ~width:40 ~height:30 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd..." in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:2147483648L ~total:2147483648L;
  Task.succeed layer;
  Task.succeed step;
  close t;
  let rows = lines t in
  Alcotest.(check bool)
    "no row holds a lone character" false
    (List.exists (fun r -> String.length (String.trim r) = 1) rows);
  Alcotest.(check bool)
    "each label on one row" true
    (List.exists (contains "RUN make") rows
    && List.exists (contains "sha256") rows)

(* On a pipe, a task's output and the tasks under it are told apart by
   their paths, not by an indentation: a child's lines name its parent, and
   a line of output names its own task, two columns in. *)
let test_plain_output_not_sibling () =
  let (), out =
    plain ~theme (fun d _ ->
        let p = Task.start d "parent" in
        let c = Task.start p "child" in
        Task.log p "parent says";
        Task.succeed c;
        Task.succeed p)
  in
  check_lines "each line names its task"
    [
      "[-] parent";
      "[-] parent \u{25B8} child";
      "  # parent: parent says";
      "[+] parent \u{25B8} child  0.0s";
      "[+] parent  0.0s";
      "";
    ]
    (split out)

(* On a pipe a failure's output is printed once: the lines streamed as they
   came are not printed again under the failure. *)
let test_plain_failure_output_once () =
  let (), out =
    plain ~theme (fun d _ ->
        let job = Task.start d "job" in
        Task.log job "checksum mismatch";
        Task.fail job)
  in
  Alcotest.(check int)
    "the line once" 1
    (List.length (List.filter (contains "checksum mismatch") (split out)))

(* A task's mark is what the program says of it: a task under it that
   failed does not fail it, and a program that fails it marks it so. *)
let test_parent_of_failure () =
  let (), out =
    plain ~theme (fun d _ ->
        Task.run d "parent" (fun p -> Task.fail (Task.start p "child"));
        Task.run d "step" (fun p ->
            Task.fail (Task.start p "layer");
            Task.fail p))
  in
  check_lines "each mark as the program said"
    [
      "[-] parent";
      "[-] parent \u{25B8} child";
      "[x] parent \u{25B8} child  0.0s  failed";
      "[+] parent  0.0s";
      "[-] step";
      "[-] step \u{25B8} layer";
      "[x] step \u{25B8} layer  0.0s  failed";
      "[x] step  0.0s  failed";
      "";
    ]
    (split out);
  let t = terminal ~theme ~width:40 ~height:8 () in
  Task.run t.root "parent" (fun p -> Task.fail (Task.start p "child"));
  close t;
  Alcotest.(check bool)
    "on a terminal too" true
    (List.exists (fun r -> untimed r = "[+] parent") (lines t))

(* With [`Quiet] a nested failure says where it is: its parent's label goes
   with it, since its indentation alone means nothing. *)
let test_quiet_failure_context () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let p = Task.start d "RUN make" in
  let c = Task.start p "sha256" in
  Task.log c "checksum mismatch";
  Task.fail c;
  Task.succeed p;
  Display.close display;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buffer in
  Alcotest.(check bool) "the failure shows" true (contains "sha256" out);
  Alcotest.(check bool) "with its parent" true (contains "RUN make" out)

(* On a pipe, the lines of two trees that run at once interleave: a child's
   line says whose child it is, as its output lines say their task. *)
let test_plain_interleaved_children () =
  let (), out =
    plain ~theme (fun d _ ->
        let a = Task.start d "linux" and b = Task.start d "macos" in
        let fa = Task.start a "fetch" in
        let fb = Task.start b "fetch" in
        Task.succeed fa;
        Task.succeed fb;
        Task.succeed a;
        Task.succeed b)
  in
  let fetches = List.filter (contains "fetch") (split out) in
  Alcotest.(check bool)
    "each fetch line names its build" true
    (List.for_all (fun l -> contains "linux" l || contains "macos" l) fetches)

(* With [`Quiet] only what fails shows, and a transient task that fails is
   no exception: its output is the run's only trace of the failure. *)
let test_quiet_transient_failure () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~progress:`Quiet ~now:(fun () -> 0.) ppf in
  let t = Task.start ~transient:true d "probe" in
  Task.log t "fatal: disk full";
  Task.fail t;
  Display.close display;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "the failure's output shows" true
    (contains "disk full" (Buffer.contents buffer))

(* The summary runs while the display is open. *)
let test_summary_between_tasks () =
  let t = terminal ~theme ~width:40 ~label:"Build" () in
  let a = Task.start t.root "fetch" in
  Task.succeed a;
  Display.redraw t.display;
  Alcotest.(check bool)
    "still running" true
    (List.exists (contains "[-] Build") (screen t));
  Alcotest.(check bool)
    "not a success" false
    (List.exists (contains "[+] Build") (screen t));
  close t

(* The 4 KiB cut keeps a flag, one grapheme cluster, whole. *)
let test_cap_keeps_a_flag () =
  let flag = "\u{1F1EB}\u{1F1F7}" in
  let label = String.make 4089 'a' ^ flag ^ "b" in
  let (), out =
    plain (fun d _ ->
        let t = Task.start d label in
        Task.succeed t)
  in
  let has affix = contains affix out in
  Alcotest.(check bool)
    "the flag whole or not at all" true
    (has flag || not (has "\u{1F1EB}"))

(* The mark of a line of output is not the separator of its path. *)
let test_pipe_output_mark_not_separator () =
  let (), out =
    plain (fun d _ ->
        let a = Task.start d "a" in
        let b = Task.start a "b" in
        Task.log b "x";
        Task.succeed b;
        Task.succeed a)
  in
  let line = List.find (contains ": x") (String.split_on_char '\n' out) in
  let count = List.length (Re.matches (Re.Perl.compile_pat "\u{203A}") line) in
  Alcotest.(check int) ("one U+203A in " ^ String.escaped line) 1 count

(* A label is not cut for a count column its row leaves empty. *)
let test_label_not_cut_for_an_empty_count () =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Fmt.set_utf_8 ppf true;
  let d, _display =
    show ~size:(fun () -> (40, 30)) ~progress:`Tty ~now:(fun () -> 0.) ppf
  in
  let rec nest p n =
    if n = 0 then p else nest (Task.start p (Fmt.str "level-%d-label" n)) (n - 1)
  in
  let leaf = nest d 8 in
  Task.set_items leaf ~count:1 ~total:4;
  let tree =
    View.to_string (View.sized ~width:40 ~height:30 (Progress.tree d))
  in
  Alcotest.(check bool)
    ("level-5-label whole in\n" ^ tree)
    true
    (contains "level-5-label" tree)

(* On a pipe a failed transient task prints its line and its reason. *)
let test_plain_transient_failure () =
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  Theme.set ppf theme;
  let d, display = show ~label:"S" ~progress:`Plain ~now:(fun () -> 0.) ppf in
  (try Task.run ~transient:true d "probe" (fun _ -> failwith "disk full")
   with Failure _ -> ());
  Display.close display;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buffer in
  Alcotest.(check bool)
    ("the failure shows in\n" ^ out)
    true (contains "probe" out)

let test_tty_transient_failure () =
  let t = terminal ~theme ~width:40 ~label:"S" () in
  (try
     Task.run ~transient:true t.root "probe" (fun r ->
         Task.log r "disk full";
         failwith "boom")
   with Failure _ -> ());
  close t;
  let all = lines t in
  Alcotest.(check bool)
    ("the failure shows in\n" ^ String.concat "\n" all)
    true
    (List.exists (contains "[x] probe") all
    && List.exists (contains "disk full") all)

let test_label_gives_way_last () =
  let label = "a-label-of-thirty-cells-long-x" in
  let given = Re.Perl.compile_pat "MiB|GiB|[0-9]s|\u{2588}|\u{2591}" in
  List.iter
    (fun width ->
      let t = terminal ~theme ~width ~height:4 () in
      let r = Task.start t.root label in
      t.advance 1.;
      Task.set_bytes r ~count:268435456L ~total:2147483648L;
      t.advance 4.;
      Display.redraw t.display;
      let row = List.find (contains "a-label") (screen t) in
      if not (contains label row) then
        Alcotest.(check bool)
          (Fmt.str "at %d nothing but the label is left in %S" width row)
          false (Re.execp given row);

      close t)
    [ 80; 70; 60; 55; 45; 42; 36; 33; 30; 20 ]

(* The cell of the last of [row]'s block or shade glyphs. *)
let bar_end row = Option.map snd (bar_span row)

let test_bars_line_up () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:2 ~total:5;
  t.advance 1.;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
  t.advance 1.;
  Task.set_bytes layer ~count:536870912L ~total:2147483648L;
  Display.redraw t.display;
  let rows = screen t in
  let ends = List.filter_map bar_end rows in
  Alcotest.(check int)
    ("two bars in\n" ^ String.concat "\n" rows)
    2 (List.length ends);
  Alcotest.(check bool)
    ("the bars end in one column in\n" ^ String.concat "\n" rows)
    true
    (List.for_all (( = ) (List.hd ends)) ends);
  close t

(* The cell width of [row], every glyph one cell. *)
let cells row = Array.length (glyphs row)
let span = Alcotest.(option (pair int int))

(* A row is packed from the left: the label, then the bar, the count and
   the time. The bars of all rows start one cell after the widest label,
   are as wide, 40 cells at most, and the rest of the line stays empty. *)
let test_rows_pack_left () =
  let t = terminal ~theme ~width:100 ~height:8 () in
  let labels = [ "parser"; "layout"; "a-longer-render-suite" ] in
  List.iteri
    (fun i l -> Task.set_items (Task.start t.root l) ~count:(10 + i) ~total:120)
    labels;
  Display.redraw t.display;
  let rows = screen t in
  let all = String.concat "\n" rows in
  let row l = List.find (contains l) rows in
  match List.map (fun l -> bar_span (row l)) labels with
  | Some (first, last) :: _ as spans ->
      List.iter
        (Alcotest.check span
           ("the bars in one column in\n" ^ all)
           (Some (first, last)))
        spans;
      Alcotest.(check int) ("bars of 40 cells in\n" ^ all) 40 (last - first + 1);
      let widest =
        List.fold_left
          (fun n l -> max n (cell_of l (row l) + String.length l))
          0 labels
      in
      Alcotest.(check int)
        ("one cell between the widest label and the bars in\n" ^ all)
        (widest + 1) first;
      List.iter
        (fun l ->
          Alcotest.(check bool)
            ("the end of the line empty in\n" ^ all)
            true
            (cells (row l) < 100))
        labels;
      close t
  | _ -> Alcotest.fail ("no bar in\n" ^ all)

(* While the terminal narrows, the bar loses a cell for each column it
   loses, from 40 cells down to 10, before the rate gives way. *)
let test_bar_shrinks_first () =
  let row_at width =
    let t = terminal ~theme ~width ~height:4 () in
    let r = Task.start t.root "layer" in
    t.advance 1.;
    Task.set_bytes r ~count:268435456L ~total:2147483648L;
    t.advance 1.;
    Task.set_bytes r ~count:536870912L ~total:2147483648L;
    Display.redraw t.display;
    let row = List.find (contains "layer") (screen t) in
    close t;
    ( Option.map (fun (first, last) -> last - first + 1) (bar_span row),
      contains "/s" row,
      row )
  in
  let rows = List.init 71 (fun i -> row_at (110 - i)) in
  (match rows with
  | (bar, _, row) :: _ ->
      Alcotest.(check (option int))
        ("a bar of 40 cells in\n" ^ row)
        (Some 40) bar
  | [] -> ());
  let rec walk = function
    | (bar, true, row) :: ((bar', rate', row') :: _ as rest) ->
        let msg = Fmt.str "from\n%s\nto\n%s" row row' in
        (match (bar, bar') with
        | Some b, Some b' when rate' ->
            Alcotest.(check bool)
              ("the bar loses at most a cell " ^ msg)
              true
              (b' = b - 1 || (b = 40 && b' = 40))
        | Some b, _ ->
            Alcotest.(check int) ("a bar of 10 cells, then no rate " ^ msg) 10 b
        | None, _ -> Alcotest.fail ("a rate without a bar " ^ msg));
        walk rest
    | (_, false, row) :: (_, true, row') :: _ ->
        Alcotest.failf "the rate comes back from\n%s\nto\n%s" row row'
    | _ :: rest -> walk rest
    | [] -> ()
  in
  walk rows;
  Alcotest.(check bool)
    "the rate gives way" true
    (List.exists (fun (_, rate, _) -> not rate) rows)

(* A count of bytes that gives way keeps to three digits: 1000 of a unit
   or more is shown in the next one. *)
let test_short_bytes () =
  let row_at width =
    let t = terminal ~theme ~width ~height:4 () in
    let r = Task.start t.root "layer" in
    Task.set_bytes r ~count:1048575L ~total:1048576000L;
    Display.redraw t.display;
    let row = List.find (contains "layer") (screen t) in
    close t;
    row
  in
  let rows = List.init 40 (fun i -> row_at (20 + i)) in
  Alcotest.(check bool)
    ("1.0M/1.0G in\n" ^ String.concat "\n" rows)
    true
    (List.exists (contains "1.0M/1.0G") rows);
  List.iter
    (fun row ->
      Alcotest.(check bool)
        ("no four digits in\n" ^ row)
        false
        (contains "1024K" row || contains "1000M" row))
    rows

(* [full row] is [true] iff the bar of [row] has no cell of its track. *)
let full row =
  match bar_span row with
  | None -> false
  | Some (i, j) ->
      Array.for_all
        (Uchar.equal (Uchar.of_int 0x2588))
        (Array.sub (glyphs row) i (j - i + 1))

(* A task that ends keeps its count and its time in the columns of the
   rows that run, and its bar, full, in the cells of theirs. *)
let test_ended_keeps_columns () =
  let t = terminal ~theme ~width:100 ~height:8 () in
  let build = Task.start t.root "build" in
  let a = Task.start build "running" and b = Task.start build "ends" in
  Task.set_items a ~count:3 ~total:12;
  Task.set_items b ~count:12 ~total:12;
  Display.redraw t.display;
  let before = screen t in
  Task.succeed b;
  Display.redraw t.display;
  let after = screen t in
  let row l rows = List.find (contains l) rows in
  let msg =
    "in\n" ^ String.concat "\n" before ^ "\nthen\n" ^ String.concat "\n" after
  in
  let count rows = cell_of "12/12" (row "ends" rows) + 5 in
  Alcotest.(check int)
    ("the count of the task that ended stays " ^ msg)
    (count before) (count after);
  Alcotest.(check int)
    ("in the column of the counts " ^ msg)
    (cell_of "3/12" (row "running" after) + 4)
    (count after);
  Alcotest.(check int)
    ("its time ends in its column " ^ msg)
    (cells (row "running" after))
    (cells (row "ends" after));
  Alcotest.check span
    ("its bar in the cells of theirs " ^ msg)
    (bar_span (row "running" after))
    (bar_span (row "ends" after));
  Alcotest.(check bool) ("its bar full " ^ msg) true (full (row "ends" after));
  close t

(* The line of a task that ended at its total runs from its label to its
   count with no gap: its bar is full. One that failed before its total
   draws its bar as far as it went. *)
let test_ended_bar () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let total = 60 in
  let render = Task.start t.root "render" and lint = Task.start t.root "lint" in
  for count = 1 to total do
    Task.set_items render ~count ~total;
    t.advance 0.02
  done;
  Task.set_items lint ~count:15 ~total;
  Task.succeed render;
  Task.fail lint;
  Display.redraw t.display;
  let rows = screen t in
  let msg = "in\n" ^ String.concat "\n" rows in
  let row l = List.find (contains l) rows in
  let render = row "render" and lint = row "lint" in
  let blanks row =
    let a = cell_of "render" row + 6 and b = cell_of "60/60" row in
    Array.fold_left
      (fun n u -> if Uchar.equal u (Uchar.of_int 0x20) then n + 1 else n)
      0
      (Array.sub (glyphs row) a (b - a))
  in
  Alcotest.(check bool) ("a full bar " ^ msg) true (full render);
  Alcotest.(check bool)
    ("no gap between the label and the count " ^ msg)
    true
    (blanks render < 5);
  Alcotest.check span
    ("the bar that failed in the same cells " ^ msg)
    (bar_span render) (bar_span lint);
  Alcotest.(check bool) ("a part of a bar " ^ msg) false (full lint);
  close t

(* At a fixed width nothing in the region moves sideways while counts grow,
   a row with a wider count starts and a table's cells change: two trees
   side by side, as examples/columns lays them out. *)
let region_stands_still ~width ~bars =
  let t = terminal ~theme ~width ~height:16 () in
  let status = Hashtbl.create 2 in
  let table () =
    Table.view
      [
        Table.column (View.text "Build");
        Table.column ~min_width:7 (View.text "Status");
      ]
      (List.map
         (fun b ->
           [
             View.text b;
             View.text
               (Option.value (Hashtbl.find_opt status b) ~default:"waiting");
           ])
         [ "linux"; "macos" ])
  in
  let view d ~width ~height =
    match Task.children d with
    | [ a; b ] ->
        View.vcat
          [
            View.sized ~width (table ());
            View.hcat
              [
                View.sized ~width:((width - 2) / 2) ~height (Progress.tree a);
                View.sized ~width:((width - 2) / 2) ~height (Progress.tree b);
              ];
          ]
    | _ -> default d ~width ~height
  in
  Display.set_view t.display (fit t.root view);
  let linux = Task.start t.root "linux" and macos = Task.start t.root "macos" in
  Hashtbl.replace status "linux" "running";
  let fetch = Task.start macos "fetch" in
  let lfetch = Task.start linux "fetch" in
  let frames = ref [] in
  let frame () =
    Display.redraw t.display;
    let rows = screen t in
    let border = cells (List.hd rows) in
    let bar =
      List.find_map
        (fun r -> if contains "fetch" r then bar_end r else None)
        (List.rev rows)
    in
    frames := (border, bar, String.concat "\n" rows) :: !frames
  in
  for cur = 1 to 4 do
    Task.set_items fetch ~count:cur ~total:4;
    if cur <= 3 then Task.set_items lfetch ~count:cur ~total:4;
    if cur = 3 then begin
      Task.succeed lfetch;
      let compile = Task.start linux "compile" in
      Task.set_items compile ~count:12 ~total:12;
      Hashtbl.replace status "linux" "done"
    end;
    frame ()
  done;
  match List.rev !frames with
  | [] -> ()
  | (border, bar, rows) :: rest ->
      Alcotest.(check bool) ("a bar in\n" ^ rows) bars (bar <> None);
      List.iter
        (fun (b, r, rows) ->
          Alcotest.(check int) ("the table's edge in\n" ^ rows) border b;
          Alcotest.(check (option int)) ("the bar in\n" ^ rows) bar r)
        rest

let test_region_stands_still () =
  region_stands_still ~width:100 ~bars:true;
  region_stands_still ~width:80 ~bars:false

(* examples/progress: once a task counting bytes has its total, its first
   rate moves no bar sideways. *)
let test_rate_keeps_bars_still () =
  let t = terminal ~theme ~width:80 ~height:8 () in
  let step = Task.start t.root "RUN make" in
  let layer = Task.start step "sha256:abcd" in
  Task.set_items step ~count:5 ~total:5;
  Task.set_bytes layer ~count:0L ~total:2147483648L;
  Display.redraw t.display;
  let before = screen t in
  t.advance 0.2;
  Task.set_bytes layer ~count:268435456L ~total:2147483648L;
  Display.redraw t.display;
  let after = screen t in
  let step_bar rows =
    List.find_map
      (fun r -> if contains "RUN make" r then bar_end r else None)
      rows
  in
  Alcotest.(check (option int))
    ("the bar of RUN make in\n" ^ String.concat "\n" before ^ "\nthen\n"
   ^ String.concat "\n" after)
    (step_bar before) (step_bar after);
  close t

(* The count and rate columns only widen, at most once for each new kind of
   progress, and never narrow. Rows of items are as dense as their counts
   need. The first task of bytes widens the count column to the widest
   count of bytes and opens the rate column, once: later tasks of bytes,
   larger totals and first rates move no bar. *)
let test_columns_widen_once () =
  let t = terminal ~theme ~width:80 ~height:10 () in
  let bar label rows =
    List.find_map (fun r -> if contains label r then bar_end r else None) rows
  in
  let step = Task.start t.root "RUN make" in
  Task.set_items step ~count:2 ~total:5;
  Display.redraw t.display;
  let items = screen t in
  let row = List.find (contains "RUN make") items in
  let count =
    let cells = Array.to_list (glyphs row) in
    let rec find i = function
      | [] -> None
      | u :: _
        when i > Option.get (bar_end row) && Uchar.equal u (Uchar.of_char '2')
        ->
          Some i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 cells
  in
  Alcotest.(check bool)
    ("the count close to the bar in\n" ^ row)
    true
    (Option.get count - Option.get (bar_end row) <= 8);
  let layer = Task.start step "sha256:abcd" in
  Task.set_bytes layer ~count:0L ~total:1073741824L;
  Display.redraw t.display;
  let first = screen t in
  t.advance 0.2;
  Task.set_bytes layer ~count:268435456L ~total:1073741824L;
  let other = Task.start step "sha256:ef01" in
  Task.set_bytes other ~count:0L ~total:549755813888L;
  t.advance 0.2;
  Task.set_bytes other ~count:274877906944L ~total:549755813888L;
  Task.set_bytes layer ~count:536870912L ~total:1073741824L;
  Display.redraw t.display;
  let later = screen t in
  Alcotest.(check (option int))
    ("the bar of RUN make in\n" ^ String.concat "\n" first ^ "\nthen\n"
   ^ String.concat "\n" later)
    (bar "RUN make" first) (bar "RUN make" later);
  Alcotest.(check (option int))
    ("the bars line up in\n" ^ String.concat "\n" later)
    (bar "RUN make" later) (bar "sha256:ef01" later);
  close t

let test_elapsed_rounded () =
  List.iter
    (fun (secs, shown) ->
      let now = ref 0. in
      let buffer = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buffer in
      Theme.set ppf theme;
      let d, display = show ~progress:`Plain ~now:(fun () -> !now) ppf in
      let t = Task.start d "t" in
      now := secs;
      Task.succeed t;
      Display.close display;
      Format.pp_print_flush ppf ();
      let out = Buffer.contents buffer in
      Alcotest.(check bool)
        (Fmt.str "%g s reads %s in %S" secs shown out)
        true
        (contains ("t  " ^ shown) out))
    [
      (0.96, "1.0s");
      (0.94, "0.9s");
      (59.97, "1m00s");
      (119.6, "2m00s");
      (3599.6, "1h00m");
    ]

(* The bytes per second a rate text says, such as "724.00 KiB/s". *)
let rate_of s =
  match String.split_on_char ' ' (String.trim s) with
  | [ n; unit ] ->
      let k =
        match unit with
        | "B/s" -> 1.
        | "KiB/s" -> 1024.
        | "MiB/s" -> 1048576.
        | u -> Alcotest.failf "unit %S" u
      in
      float_of_string n *. k
  | _ -> Alcotest.failf "rate %S" s

(* The rate weighs the past by the time since it was measured: a task that
   reports every hundredth of a second and one that reports every second
   show the same rate for the same bytes. *)
let test_rate_time_weighted () =
  let now = ref 0. in
  let root = Task.root ~now:(fun () -> !now) () in
  let often = Task.start root "often" and seldom = Task.start root "seldom" in
  (* 1 MiB/s for 3 s, then 256 KiB/s. *)
  let bytes t =
    if t <= 3. then t *. 1048576. else (3. *. 1048576.) +. ((t -. 3.) *. 262144.)
  in
  let total = 100_000_000L in
  for i = 1 to 400 do
    now := float i /. 100.;
    Task.set_bytes often ~count:(Int64.of_float (bytes !now)) ~total;
    if i mod 100 = 0 then
      Task.set_bytes seldom ~count:(Int64.of_float (bytes !now)) ~total
  done;
  let rate task =
    rate_of (View.to_string (Progress.row ~layout:Progress.Layout.rate task))
  in
  let a = rate often and b = rate seldom in
  if Float.abs (a -. b) > 0.1 *. Float.max a b then
    Alcotest.failf "%.0f B/s reported often, %.0f B/s seldom" a b;
  Alcotest.(check bool)
    "between the old rate and the new" true
    (a > 262144. && a < 1048576.)

(* The time left is what the count leaves at the task's rate. *)
let test_eta () =
  let now = ref 0. in
  let root = Task.root ~now:(fun () -> !now) () in
  let task = Task.start root "work" in
  Task.set_items task ~count:0 ~total:4;
  now := 1.;
  Task.set_items task ~count:1 ~total:4;
  Alcotest.(check string)
    "three items at one a second" "ETA 3.0s"
    (String.trim
       (View.to_string (Progress.row ~layout:Progress.Layout.eta task)));
  let fresh = Task.start root "fresh" in
  Alcotest.(check string)
    "none without a rate" ""
    (String.trim
       (View.to_string (Progress.row ~layout:Progress.Layout.eta fresh)))

(* A program sees each event of the work, in order, the work's end last. *)
let test_on_event () =
  let root = Task.root ~now:(fun () -> 0.) () in
  let seen = ref [] in
  Task.on_event root (fun e -> seen := e :: !seen);
  Task.run root "a" (fun a ->
      Task.set_items a ~count:1 ~total:2;
      Task.log a "line");
  Task.succeed ~message:"done" root;
  let kinds =
    List.rev_map
      (fun e ->
        match (Task.Event.kind e, Task.Event.task e) with
        | `Start, Some (_, [ "a" ]) -> "start a"
        | `Progress, _ -> "progress"
        | `Output, Some _ -> "output"
        | `End, Some _ -> "end a"
        | `End, None -> (
            match Task.Event.message e with
            | Some m -> "end of the work: " ^ m
            | None -> "end of the work")
        | _ -> "other")
      !seen
  in
  check_lines "the events in order"
    [ "start a"; "progress"; "output"; "end a"; "end of the work: done" ]
    kinds

(* What a top task leaves in the scrollback is what [commit] draws: here one
   line a task, or nothing. *)
let test_commit () =
  let run commit =
    let t = terminal ~theme ~width:30 ~height:8 ~commit () in
    let a = Task.start t.root "a" in
    Task.succeed (Task.start a "a1");
    Task.succeed a;
    Task.succeed (Task.start t.root "b");
    close t;
    lines t
  in
  check_lines "one line a top task" [ "done a"; "done b" ]
    (run (fun t -> Some (View.text ("done " ^ Task.label t))));
  check_lines "nothing" [] (run (fun _ -> None))

(* [commit] sees a top task as it ended, the output of a success too, which
   the task lets go of once it moved. *)
let test_commit_output () =
  let seen = ref [] in
  let commit job =
    seen := Task.output job;
    None
  in
  let t = terminal ~theme ~width:30 ~height:8 ~commit () in
  let job = Task.start t.root "job" in
  Task.log job "one\ntwo";
  Task.succeed job;
  check_lines "its output" [ "one"; "two" ] !seen;
  check_lines "then none" [] (Task.output job);
  close t

(* [print] writes at once: above the region on a terminal, with the region
   drawn again below it, and in turn on a pipe. *)
let test_print () =
  let t = terminal ~theme ~width:30 ~height:8 () in
  ignore (Task.start t.root "job");
  Display.redraw t.display;
  let before = List.length !(t.chunks) in
  Display.print t.display (View.text "printed");
  Alcotest.(check int) "one write" (before + 1) (List.length !(t.chunks));
  check_lines "above the region, at once" [ "printed"; " |  job" ]
    (List.map untimed (screen t));
  close t;
  let buffer = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buffer in
  let _, display = show ~progress:`Plain ~now:(fun () -> 0.) ppf in
  Display.print display (View.text "printed");
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "on a pipe, at once" "printed\n" (Buffer.contents buffer);
  Display.close display

(* [`Plain] on a terminal prints its last frame as wide as the terminal,
   and whole without one. *)
let test_plain_last_frame_width () =
  let rule = View.fit (fun _ -> Rule.view ()) in
  let last ?size () =
    let buffer = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buffer in
    let _, display =
      show ~progress:`Plain ?size ~view:(Fun.const rule) ~now:(fun () -> 0.) ppf
    in
    Display.close display;
    Format.pp_print_flush ppf ();
    let lines = String.split_on_char '\n' (Buffer.contents buffer) in
    List.fold_left (fun n l -> max n (View.width (View.text l))) 0 lines
  in
  Alcotest.(check int)
    "a terminal of 40 columns" 40
    (last ~size:(fun () -> (40, 24)) ());
  Alcotest.(check int) "whole on a pipe" 4 (last ())

(* A display that keeps its rows and trees from frame to frame shows what a
   display that keeps nothing lays out, as tasks start, move, log, are
   renamed and end, between frames and as the time moves the spinners and
   timers. A layout with a piece of its own keeps nothing: an empty one adds
   nothing to a row. *)
let test_kept_rows () =
  let time = ref 0. in
  let now () = !time in
  let root = Task.root ~label:"build" ~now () in
  let screen layout =
    let vte = Console_vte.v ~rows:16 ~cols:60 () in
    let buf = Buffer.create 256 in
    let ppf =
      Format.make_formatter (Buffer.add_substring buf) (fun () ->
          Console_vte.feed vte (Buffer.contents buf);
          Buffer.clear buf)
    in
    Fmt.set_utf_8 ppf true;
    ( vte,
      Display.v ~size:(fun () -> (60, 16)) ~now ppf (Progress.view ~layout root)
    )
  in
  let kept_vte, kept = screen Progress.Layout.default in
  let fresh_vte, fresh =
    screen Progress.Layout.(default ++ custom (fun _ -> View.empty))
  in
  let check step =
    Display.redraw kept;
    Display.redraw fresh;
    Alcotest.(check (list string))
      step
      (Console_vte.screen fresh_vte)
      (Console_vte.screen kept_vte)
  in
  let a = Task.start root "fetch" in
  let b = Task.start root "compile" in
  let c = Task.start b "src/a.c" in
  check "started";
  for i = 1 to 12 do
    if i mod 2 = 0 then time := !time +. 0.037;
    Task.set_items a ~count:i ~total:12;
    if i mod 3 = 0 then Task.log c (Fmt.str "line %d" i);
    check (Fmt.str "step %d" i)
  done;
  Task.set_label c "src/b.c";
  check "renamed";
  Task.succeed a;
  check "a top task ended";
  Task.fail c;
  check "a nested task failed";
  time := !time +. 0.25;
  check "the time moves";
  Task.succeed b;
  check "every task ended";
  time := !time +. 1.;
  check "the time moves on";
  Display.close kept;
  Display.close fresh

(* A display opened on its own is told of no change: at each redraw it draws
   again a view that reads the tasks or the program's state, the clock
   standing still, and lays out once a view that reads neither. *)
let test_redraw_reads_state () =
  let now () = 0. in
  let root = Task.root ~label:"build" ~now () in
  let vte = Console_vte.v ~rows:8 ~cols:40 () in
  let buf = Buffer.create 256 in
  let ppf =
    Format.make_formatter (Buffer.add_substring buf) (fun () ->
        Console_vte.feed vte (Buffer.contents buf);
        Buffer.clear buf)
  in
  let jobs = ref [] in
  let status = View.fit (fun _ -> View.text (String.concat "," !jobs)) in
  let still, calls = laid () in
  let d =
    Display.v
      ~size:(fun () -> (40, 8))
      ~now ppf
      (View.vcat [ still; status; Progress.view root ])
  in
  let shows step affix =
    Display.redraw d;
    Alcotest.(check bool)
      step true
      (List.exists (contains affix) (Console_vte.screen vte))
  in
  let a = Task.start root "fetch" in
  shows "a task started" "fetch";
  Task.set_items a ~count:3 ~total:4;
  shows "its count" "3/4";
  jobs := [ "linux"; "macos" ];
  shows "the program's state" "linux,macos";
  Alcotest.(check int) "the still view laid out once" 1 !calls;
  Display.close d

(* The display of a root's tasks is told of each change to them: a redraw
   with no change lays out no view of the tasks, while a view that reads the
   program's state is laid out at each. *)
let test_redraw_told () =
  let t = terminal () in
  let rows, calls = laid ~tasks:true () in
  Display.set_view t.display (View.vcat [ rows; Progress.view t.root ]);
  let a = Task.start t.root "a" in
  Display.redraw t.display;
  let laid_out = !calls in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "not laid out again" laid_out !calls;
  Task.set_items a ~count:1 ~total:2;
  Display.redraw t.display;
  Alcotest.(check int) "laid out once changed" (laid_out + 1) !calls;
  Alcotest.(check bool)
    "the count shows" true
    (List.exists (contains "1/2") (screen t));
  let state, calls = laid ~live:true () in
  Display.set_view t.display (View.vcat [ state; Progress.view t.root ]);
  let laid_out = !calls in
  Display.redraw t.display;
  Display.redraw t.display;
  Alcotest.(check int) "the state laid out at each" (laid_out + 2) !calls;
  close t

let suite =
  ( "display",
    [
      Alcotest.test_case "kept rows are the rows laid out afresh" `Quick
        test_kept_rows;
      Alcotest.test_case "a redraw draws what the view reads" `Quick
        test_redraw_reads_state;
      Alcotest.test_case "a display told of changes draws on change" `Quick
        test_redraw_told;
      Alcotest.test_case "the rate weighs time, not reports" `Quick
        test_rate_time_weighted;
      Alcotest.test_case "the time left" `Quick test_eta;
      Alcotest.test_case "a program sees the events" `Quick test_on_event;
      Alcotest.test_case "commit" `Quick test_commit;
      Alcotest.test_case "a printed line wraps as a terminal" `Quick
        test_root_line_wraps;
      Alcotest.test_case "commit sees the output" `Quick test_commit_output;
      Alcotest.test_case "print writes at once" `Quick test_print;
      Alcotest.test_case "a view exception still allows close" `Quick
        test_view_exception_close;
      Alcotest.test_case "a view that raises later" `Quick
        test_view_exception_later;
      Alcotest.test_case "a view side by side" `Quick test_view_hcat;
      Alcotest.test_case "a simple table stands still" `Quick
        test_simple_view_stands_still;
      Alcotest.test_case "plain lines" `Quick test_plain_lines;
      Alcotest.test_case "a delay before the rows" `Quick test_delay;
      Alcotest.test_case "plain lines of the program" `Quick
        test_plain_own_lines;
      Alcotest.test_case "plain attribution" `Quick test_plain_attribution;
      Alcotest.test_case "plain unfinished tasks" `Quick test_plain_unfinished;
      Alcotest.test_case "a plain closing line says how it ended" `Quick
        test_plain_outcome_word;
      Alcotest.test_case "plain lines start a line" `Quick test_plain_column;
      Alcotest.test_case "plain sanitizes" `Quick test_plain_sanitizes;
      Alcotest.test_case "plain result" `Quick test_plain_result;
      Alcotest.test_case "empty live header remains pending" `Quick
        test_empty_header_pending;
      Alcotest.test_case "region" `Quick test_region;
      Alcotest.test_case "a view taller than the room" `Quick test_tall_header;
      Alcotest.test_case "a raw tty" `Quick test_raw_tty;
      Alcotest.test_case "after a partial line" `Quick test_after_a_partial_line;
      Alcotest.test_case "the window" `Quick test_window;
      Alcotest.test_case "alignment" `Quick test_alignment;
      Alcotest.test_case "label truncated" `Quick test_label_truncated;
      Alcotest.test_case "capped" `Quick test_capped;
      Alcotest.test_case "the marker fits" `Quick test_marker_fits;
      Alcotest.test_case "the marker counts" `Quick test_marker_counts;
      Alcotest.test_case "with_output" `Quick test_with_output;
      Alcotest.test_case "with_output without a line end" `Quick
        test_with_output_partial;
      Alcotest.test_case "with_output moving the cursor" `Quick
        test_with_output_moves;
      Alcotest.test_case "idle redraw writes nothing" `Quick test_redraw_idle;
      Alcotest.test_case "narrowing" `Quick test_narrowing;
      Alcotest.test_case "wide" `Quick test_wide;
      Alcotest.test_case "synchronized" `Quick test_synchronized;
      Alcotest.test_case "height changes" `Quick test_height_changes;
      Alcotest.test_case "nested" `Quick test_nested;
      Alcotest.test_case "nested on a pipe" `Quick test_nested_plain;
      Alcotest.test_case "keys and transient" `Quick test_keys_and_transient;
      Alcotest.test_case "tab stops in a task's output" `Quick test_tabs;
      Alcotest.test_case "a key shared by two with_task" `Quick
        test_with_task_shared_key;
      Alcotest.test_case "counts" `Quick test_counts;
      Alcotest.test_case "large counts" `Quick test_large_counts;
      Alcotest.test_case "invalid" `Quick test_invalid;
      Alcotest.test_case "a terminal of any width" `Quick test_any_width;
      Alcotest.test_case "a closed display ignores calls" `Quick test_closed;
      Alcotest.test_case "colours" `Quick test_colours;
      Alcotest.test_case "pipe lines" `Quick test_pipe_lines;
      Alcotest.test_case "pipe start lines" `Quick test_pipe_start;
      Alcotest.test_case "a success drops its output" `Quick test_success_drops;
      Alcotest.test_case "a failure prints its output" `Quick test_failure_block;
      Alcotest.test_case "a failure keeps 1 MiB" `Quick test_failure_cap;
      Alcotest.test_case "a failure on a pipe" `Quick test_failure_plain;
      Alcotest.test_case "a cancelled task prints its output" `Quick
        test_cancelled_output;
      Alcotest.test_case "the root task" `Quick test_root;
      Alcotest.test_case "within a task" `Quick test_within;
      Alcotest.test_case "two tasks" `Quick test_two_tasks;
      Alcotest.test_case "with_task" `Quick test_with_task;
      Alcotest.test_case "children" `Quick test_children;
      Alcotest.test_case "output" `Quick test_output;
      Alcotest.test_case "a label's style" `Quick test_label_style;
      Alcotest.test_case "bytes as rows show them" `Quick test_pp_bytes;
      Alcotest.test_case "a layout for each task" `Quick test_layout_choose;
      Alcotest.test_case "a transient child leaves" `Quick
        test_transient_child_leaves;
      Alcotest.test_case "keep_ended on a pipe" `Quick test_keep_ended_plain;
      Alcotest.test_case "the label column of drawn labels" `Quick
        test_label_column_drawn;
      Alcotest.test_case "rate" `Quick test_rate;
      Alcotest.test_case "a failed with_task says why" `Quick
        test_with_task_reason;
      Alcotest.test_case "iter" `Quick test_iter;
      Alcotest.test_case "view" `Quick test_view;
      Alcotest.test_case "view fits" `Quick test_view_fits;
      Alcotest.test_case "view width" `Quick test_view_width;
      Alcotest.test_case "a view on a short terminal" `Quick test_view_short;
      Alcotest.test_case "the default view in a column" `Quick
        test_default_view_column;
      Alcotest.test_case "view replaced" `Quick test_view_replaced;
      Alcotest.test_case "view on a pipe" `Quick test_view_plain;
      Alcotest.test_case "view moves" `Quick test_view_moves;
      Alcotest.test_case "the label gives way after the bar" `Quick
        test_label_before_bar;
      Alcotest.test_case "the label gives way after the bar, side by side"
        `Quick test_label_before_bar_hcat;
      Alcotest.test_case "the last frame keeps labels readable" `Quick
        test_last_frame_label_readable;
      Alcotest.test_case "a pipe's output is not a sibling of the children"
        `Quick test_plain_output_not_sibling;
      Alcotest.test_case "a pipe prints a failure's output once" `Quick
        test_plain_failure_output_once;
      Alcotest.test_case "a parent's mark is what the program says" `Quick
        test_parent_of_failure;
      Alcotest.test_case "a quiet failure names its parent" `Quick
        test_quiet_failure_context;
      Alcotest.test_case "a pipe names the parent of an interleaved child"
        `Quick test_plain_interleaved_children;
      Alcotest.test_case "a quiet transient failure shows" `Quick
        test_quiet_transient_failure;
      Alcotest.test_case "the summary runs between two tasks" `Quick
        test_summary_between_tasks;
      Alcotest.test_case "the 4 KiB cut keeps a flag whole" `Quick
        test_cap_keeps_a_flag;
      Alcotest.test_case "a pipe's output mark is not the path separator" `Quick
        test_pipe_output_mark_not_separator;
      Alcotest.test_case "a label is not cut for an empty count" `Quick
        test_label_not_cut_for_an_empty_count;
      Alcotest.test_case "a pipe shows a transient failure" `Quick
        test_plain_transient_failure;
      Alcotest.test_case "a terminal shows a transient failure" `Quick
        test_tty_transient_failure;
      Alcotest.test_case "the label gives way last" `Quick
        test_label_gives_way_last;
      Alcotest.test_case "the bars line up" `Quick test_bars_line_up;
      Alcotest.test_case "rows pack from the left" `Quick test_rows_pack_left;
      Alcotest.test_case "the bar shrinks before anything gives way" `Quick
        test_bar_shrinks_first;
      Alcotest.test_case "a short count of bytes keeps to three digits" `Quick
        test_short_bytes;
      Alcotest.test_case "a task that ends keeps its columns" `Quick
        test_ended_keeps_columns;
      Alcotest.test_case "a task that ended draws its bar" `Quick test_ended_bar;
      Alcotest.test_case "plain's last frame is the terminal's width" `Quick
        test_plain_last_frame_width;
      Alcotest.test_case "the columns widen once for each kind of progress"
        `Quick test_columns_widen_once;
      Alcotest.test_case "a first rate moves no bar" `Quick
        test_rate_keeps_bars_still;
      Alcotest.test_case "elapsed times are rounded" `Quick test_elapsed_rounded;
      Alcotest.test_case "the region stands still" `Quick
        test_region_stands_still;
    ] )
