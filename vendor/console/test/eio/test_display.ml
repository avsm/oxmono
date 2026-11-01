(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

(* Console_eio driver, on a buffer: no terminal, so each task line, log line
   and the result is printed as plain text. *)

module D = Console.Task

let capture ?label f =
  Eio_main.run @@ fun _env ->
  let clock = Eio_mock.Clock.Mono.make () in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Console_eio.run ~clock ~ppf ?label f;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let mem ~msg needle haystack =
  Alcotest.(check bool) msg true (contains needle haystack)

(* A finished task commits its label; the work's success prints its message. *)
let test_run_banner () =
  let out =
    capture ~label:"Build" (fun t ->
        let step = D.start t "compile foo.ml" in
        D.succeed step;
        D.log t "3 files, 0 errors";
        D.succeed ~message:"build succeeded" t)
  in
  mem ~msg:"task label" "compile foo.ml" out;
  mem ~msg:"detail" "3 files, 0 errors" out;
  mem ~msg:"verdict marker" "✓" out;
  mem ~msg:"verdict message" "build succeeded" out

(* The work's failure prints its message. *)
let test_run_fail () =
  let out =
    capture (fun t ->
        let step = D.start t "link" in
        D.fail step;
        D.fail ~message:"link failed" t)
  in
  mem ~msg:"fail marker" "✗" out;
  mem ~msg:"fail message" "link failed" out

(* An unhandled exception clears the region and propagates. *)
let test_run_raises () =
  Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
      ignore
        (capture (fun t ->
             let _ = D.start t "doomed" in
             failwith "boom")))

(* Logs messages are routed into the same permanent semantic history while the
   driver owns the reporter. *)
let named = Logs.Src.create "console.test"

let test_reporter () =
  let out =
    capture (fun t ->
        Logs.set_level (Some Logs.Debug);
        let step = D.start t "step" in
        Logs.app (fun m -> m "application-line");
        Logs.app (fun m -> m "╭───╮\n│ x │\n╰───╯");
        Logs.warn (fun m -> m "warning-line");
        Logs.info ~src:named (fun m -> m "named-line");
        Logs.debug (fun m -> m "debug-line");
        D.succeed step)
  in
  mem ~msg:"a named source says its name" "\n[console.test] named-line\n" out;
  mem ~msg:"application log, no mark" "\napplication-line\n" out;
  mem ~msg:"debug log, its mark" "\n\u{00b7} debug-line\n" out;
  mem ~msg:"multiline log keeps physical rows" "╭───╮\n│ x │\n╰───╯\n" out;
  mem ~msg:"warning log" "\n! warning-line\n" out

let absent ~msg needle haystack =
  Alcotest.(check bool) msg false (contains needle haystack)

(* Logs messages inside [Task.run] go under its task, from the fibers its
   function forks too. *)
let test_logs_in_task () =
  let out =
    capture (fun d ->
        Logs.set_level (Some Logs.Debug);
        D.run d "build" (fun _ ->
            Logs.info (fun m -> m "outer");
            Eio.Fiber.both
              (fun () -> Logs.warn (fun m -> m "forked"))
              (fun () -> Logs.debug (fun m -> m "deep"))))
  in
  mem ~msg:"the start line" "\u{2022} build\n" out;
  mem ~msg:"in the task" "\n  build: outer\n" out;
  mem ~msg:"from a forked fiber" "\n  ! build: forked\n" out;
  mem ~msg:"debug, from a forked fiber" "\n  \u{00b7} build: deep\n" out;
  mem ~msg:"the end line" "\u{2713} build" out

(* Two tasks in two fibers: each Logs message goes to its own fiber's. *)
let test_two_fibers () =
  let out =
    capture (fun d ->
        Logs.set_level (Some Logs.Info);
        let work name =
          D.run d name (fun _ ->
              Logs.info (fun m -> m "%s1" name);
              Eio.Fiber.yield ();
              Logs.info (fun m -> m "%s2" name))
        in
        Eio.Fiber.both (fun () -> work "a") (fun () -> work "b"))
  in
  List.iter
    (fun l -> mem ~msg:l ("\n  " ^ l ^ "\n") out)
    [ "a: a1"; "a: a2"; "b: b1"; "b: b2" ];
  List.iter (fun l -> absent ~msg:l l out) [ "a: b1"; "b: a1" ]

(* A fiber cancelled inside [Task.run] cancels its task: it did not fail. *)
let test_cancelled_fiber () =
  let out =
    capture (fun d ->
        Eio.Fiber.first
          (fun () ->
            D.run d "slow" (fun t ->
                D.log t "partial";
                Eio.Fiber.await_cancel ()))
          (fun () -> ()))
  in
  mem ~msg:"cancelled" "\u{25cb} slow" out;
  absent ~msg:"not failed" "\u{2717} slow" out

(* [within] makes a task current from another fiber, and the fibers it forks
   with it; a fiber cancelled there does not end the task. Within the root,
   a message goes above the rows, unlabelled. *)
let test_within_fiber () =
  let out =
    capture (fun d ->
        Logs.set_level (Some Logs.Info);
        let a = D.start d "a" in
        Eio.Fiber.both
          (fun () ->
            D.run d "b" (fun _ ->
                D.within a (fun () ->
                    Logs.info (fun m -> m "for a");
                    Eio.Fiber.both
                      (fun () -> Logs.info (fun m -> m "forked for a"))
                      ignore);
                Logs.info (fun m -> m "for b")))
          (fun () -> D.within d (fun () -> Logs.info (fun m -> m "for root")));
        Eio.Fiber.first
          (fun () -> D.within a Eio.Fiber.await_cancel)
          (fun () -> ());
        D.log a "still running";
        D.succeed a)
  in
  List.iter
    (fun l -> mem ~msg:l ("\n" ^ l ^ "\n") out)
    [
      "  a: for a";
      "  a: forked for a";
      "  b: for b";
      "for root";
      "  \u{203a} a: still running";
    ];
  mem ~msg:"a succeeds" "\u{2713} a" out;
  absent ~msg:"not cancelled" "\u{25cb} a" out

(* An Eio output formatter can suspend between writes to its destination.
   Concurrent tasks must still emit complete JSON records. *)
let test_json_concurrent_writes () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf =
    Format.make_formatter
      (fun text offset length ->
        let first = length / 2 in
        Buffer.add_substring out text offset first;
        Eio.Fiber.yield ();
        Buffer.add_substring out text (offset + first) (length - first))
      (fun () -> ())
  in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Eio.Fiber.List.iter
        (fun label ->
          D.run d label (fun task ->
              D.log task "line";
              D.set_items task ~count:1 ~total:1))
        [ "1"; "2"; "3" ]);
  Format.pp_print_flush ppf ();
  let lines =
    String.split_on_char '\n' (Buffer.contents out)
    |> List.filter (fun line -> line <> "")
  in
  List.iter
    (fun line ->
      match Jsont_bytesrw.decode_string Console.Task.Event.jsont line with
      | Ok e ->
          Alcotest.(check string)
            "written back" line
            (Console.Task.Event.to_string e)
      | Error error ->
          Alcotest.failf "interleaved JSON record %S: %s" line error)
    lines;
  Alcotest.(check int)
    "four events per task, and the end of the work" 13 (List.length lines)

(* The events of the lines of [out], each written back as it was. *)
let events out =
  String.split_on_char '\n' out
  |> List.filter (( <> ) "")
  |> List.map (fun line ->
      match Jsont_bytesrw.decode_string Console.Task.Event.jsont line with
      | Ok e ->
          Alcotest.(check string)
            "written back" line
            (Console.Task.Event.to_string e);
          e
      | Error e -> Alcotest.failf "%S does not decode: %s" line e)

(* The task and the text of the output events of [out]. *)
let output_events out =
  let module Event = Console.Task.Event in
  List.filter_map
    (fun e ->
      match (Event.kind e, Event.text e) with
      | `Output, Some text -> Some (Event.task e, text)
      | _ -> None)
    (events out)

let outputs out = List.map snd (output_events out)

(* Capture the standard formatter itself: run only takes program output
   from Fmt.stdout and Fmt.stderr in quiet and JSON modes. *)
let capture_stdout progress f =
  Eio_mock.Backend.run @@ fun () ->
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    {
      saved with
      out_string = Buffer.add_substring out;
      out_flush = (fun () -> ());
    };
  Fun.protect
    ~finally:(fun () -> Format.pp_set_formatter_out_functions Fmt.stdout saved)
    (fun () ->
      Console_eio.run
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~ppf:Fmt.stdout ~progress f;
      Format.pp_print_flush Fmt.stdout ());
  Buffer.contents out

(* A successful task's last line is still its output without a newline. *)
let test_quiet_unended_success () =
  let out =
    capture_stdout `Quiet (fun d -> D.run d "ok" (fun _ -> Fmt.pr "secret%!"))
  in
  Alcotest.(check string) "a successful run is silent" "" out

(* A line begun in one task cannot become the next task's output. *)
let test_json_unended_attribution () =
  let out =
    capture_stdout `Json (fun d ->
        D.run d "ok" (fun _ -> Fmt.pr "secret%!");
        D.run d "fail" (fun task ->
            Fmt.pr "public@.";
            D.fail task))
  in
  let task_name = function
    | Some (id, labels) -> Fmt.str "%d %s" id (String.concat "/" labels)
    | None -> "-"
  in
  Alcotest.(check (list (pair string string)))
    "each task keeps its bytes"
    [ ("0 ok", "secret"); ("1 fail", "public") ]
    (List.map (fun (task, text) -> (task_name task, text)) (output_events out))

(* A complete long line is one output record, just as with Task.log. *)
let test_json_long_line () =
  let text = String.make 65537 'x' in
  let out =
    capture_stdout `Json (fun d ->
        D.run d "large" (fun _ -> Fmt.pr "%s@." text))
  in
  let output = outputs out in
  Alcotest.(check (list int))
    "one complete line, no invented empty line" [ 65537 ]
    (List.map String.length output);
  Alcotest.(check bool) "the whole line is preserved" true (output = [ text ])

(* A line longer than 64 KiB is cut between two characters, and outside an
   escape code, where the formatter's write leaves one under way. *)
let test_long_line_cut () =
  let x = String.make 65535 'x' in
  let cut first rest =
    outputs
      (capture_stdout `Json (fun d ->
           D.run d "large" (fun _ ->
               Fmt.pr "%s%s%!" x first;
               Fmt.pr "%s@." rest)))
  in
  Alcotest.(check (list string))
    "between two characters" [ x; "\u{2764} rest" ]
    (cut "\xe2\x9d" "\xa4 rest");
  Alcotest.(check (list string))
    "outside an escape code" [ x; "red" ] (cut "\027[3" "1mred")

(* A write error on the display's output ends that output and nothing
   else, whatever the formatter's output raises it with: on an Eio flow, as
   [Eio.Io]. *)
let test_flow_write_error () =
  Eio_main.run @@ fun _env ->
  let closed =
    Eio.Net.err
      (Eio.Net.Connection_reset (Eio_unix.Unix_error (Unix.EPIPE, "write", "")))
  in
  let ppf = Format.make_formatter (fun _ _ _ -> raise closed) ignore in
  List.iter
    (fun progress ->
      let v =
        Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~ppf ~progress
          (fun d ->
            D.run d "work" (fun task ->
                D.set_items task ~count:1 ~total:2;
                D.log task "a line");
            D.succeed ~message:"done" d;
            42)
      in
      Alcotest.(check int) "the run's value" 42 v)
    [ `Plain; `Tty; `Quiet; `Json ]

let suite =
  ( "display",
    [
      Alcotest.test_case "a write error on a flow ends the output" `Quick
        test_flow_write_error;
      Alcotest.test_case "a long line is cut between characters" `Quick
        test_long_line_cut;
      Alcotest.test_case "JSON keeps a 65537-byte line as one output event"
        `Quick test_json_long_line;
      Alcotest.test_case "JSON keeps an unended line with its task" `Quick
        test_json_unended_attribution;
      Alcotest.test_case "quiet hides an unended successful line" `Quick
        test_quiet_unended_success;
      Alcotest.test_case "concurrent JSON writes remain whole" `Quick
        test_json_concurrent_writes;
      ("run banner", `Quick, test_run_banner);
      ("run fail", `Quick, test_run_fail);
      ("run raises", `Quick, test_run_raises);
      ("reporter", `Quick, test_reporter);
      ("Logs inside a task", `Quick, test_logs_in_task);
      ("two fibers, two tasks", `Quick, test_two_fibers);
      Alcotest.test_case "within a task from another fiber" `Quick
        test_within_fiber;
      ("a cancelled fiber cancels its task", `Quick, test_cancelled_fiber);
    ] )

module Report_domains = struct
  type sink = {
    ppf : Format.formatter;
    buffer : Buffer.t;
    foreign : int Atomic.t;
  }

  let sink ?(on_write = fun () -> ()) () =
    let owner = Domain.self () in
    let foreign = Atomic.make 0 in
    let buffer = Buffer.create 128 in
    let ppf =
      Format.make_formatter
        (fun text offset length ->
          if not (Int.equal (Domain.self () :> int) (owner :> int)) then
            Atomic.incr foreign;
          on_write ();
          Buffer.add_substring buffer text offset length)
        (fun () -> ())
    in
    { ppf; buffer; foreign }

  let with_runtime f = Eio_main.run f

  let run _env sink f =
    Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~ppf:sink.ppf f

  let pool env sw =
    Eio.Executor_pool.create ~sw ~domain_count:1 (Eio.Stdenv.domain_mgr env)

  let source = Logs.Src.create "console.worker"

  let send ?(over = fun () -> ()) ?(continued = fun () -> ()) reporter text =
    reporter.Logs.report source Logs.Info ~over continued (fun m -> m "%s" text)

  let acknowledged sink over_count continued reporter text expected =
    send reporter text
      ~over:(fun () -> Atomic.incr over_count)
      ~continued:(fun () ->
        Alcotest.(check string)
          "continuation follows complete output" expected
          (Buffer.contents sink.buffer);
        Atomic.incr continued)

  (* A reporter that keeps the messages it is given, installed while [f]
     runs. *)
  let recording f =
    let kept = ref [] and lock = Mutex.create () in
    let report _ _ ~over k msgf =
      msgf (fun ?header:_ ?tags:_ fmt ->
          Fmt.kstr
            (fun s ->
              Mutex.protect lock (fun () -> kept := s :: !kept);
              over ();
              k ())
            fmt)
    in
    let before = Logs.reporter () in
    Logs.set_reporter { Logs.report };
    Fun.protect
      (fun () -> f (fun () -> Mutex.protect lock (fun () -> List.rev !kept)))
      ~finally:(fun () -> Logs.set_reporter before)

  let restored previous =
    Alcotest.(check bool)
      "teardown restores the previous reporter" true
      (Logs.reporter () == previous)

  let owner_domain () =
    with_runtime @@ fun env ->
    let sink = sink ~on_write:Eio.Fiber.yield () in
    let previous = Logs.reporter () in
    let over_count = Atomic.make 0 and continued = Atomic.make 0 in
    run env sink (fun _ ->
        Eio.Switch.run @@ fun sw ->
        Eio.Executor_pool.submit_exn (pool env sw) ~weight:1. (fun () ->
            let reporter = Logs.reporter () in
            acknowledged sink over_count continued reporter "worker-first"
              "[console.worker] worker-first\n";
            acknowledged sink over_count continued reporter "worker-second"
              "[console.worker] worker-first\n[console.worker] worker-second\n"));
    Format.pp_print_flush sink.ppf ();
    Alcotest.(check int)
      "zero foreign accesses to the owned formatter" 0 (Atomic.get sink.foreign);
    Alcotest.(check string)
      "both complete records"
      "[console.worker] worker-first\n[console.worker] worker-second\n"
      (Buffer.contents sink.buffer);
    Alcotest.(check int) "each log completed once" 2 (Atomic.get over_count);
    Alcotest.(check int)
      "each continuation acknowledged once" 2 (Atomic.get continued);
    restored previous

  (* A message logged in another domain, within a task, is that task's
     output, as it would be in the domain that runs the display. *)
  let worker_task () =
    with_runtime @@ fun env ->
    let sink = sink () in
    run env sink (fun d ->
        D.run d "work" (fun task ->
            Eio.Switch.run @@ fun sw ->
            Eio.Executor_pool.submit_exn (pool env sw) ~weight:1. (fun () ->
                D.within task (fun () ->
                    send (Logs.reporter ()) "from a worker"))));
    Format.pp_print_flush sink.ppf ();
    let out = Buffer.contents sink.buffer in
    if not (contains "work: [console.worker] from a worker" out) then
      Alcotest.failf "the message is not the task's:\n%s" out

  (* An output refusal on the domain that runs the display does not fail
     the worker's log call: the record goes to the prior reporter. *)
  let output_error () =
    with_runtime @@ fun env ->
    recording @@ fun kept ->
    let first = ref true in
    let sink =
      sink
        ~on_write:(fun () ->
          if !first then begin
            first := false;
            failwith "owner sink refused"
          end)
        ()
    in
    let previous = Logs.reporter () in
    let over_count = Atomic.make 0 and continued = Atomic.make 0 in
    run env sink (fun _ ->
        Eio.Switch.run @@ fun sw ->
        Eio.Executor_pool.submit_exn (pool env sw) ~weight:1. (fun () ->
            send (Logs.reporter ()) "refused"
              ~over:(fun () -> Atomic.incr over_count)
              ~continued:(fun () -> Atomic.incr continued)));
    Alcotest.(check (list string))
      "the previous reporter receives the refused record" [ "refused" ]
      (kept ());
    Alcotest.(check int) "completion runs once" 1 (Atomic.get over_count);
    Alcotest.(check int) "the worker continues once" 1 (Atomic.get continued);
    Alcotest.(check int)
      "zero foreign sink accesses" 0 (Atomic.get sink.foreign);
    restored previous

  (* A logging failure must not turn successful application work into an
     exception: an unavailable display hands the record to the prior reporter. *)
  let output_refusal_falls_back () =
    with_runtime @@ fun env ->
    recording @@ fun kept ->
    let first = ref true in
    let sink =
      sink
        ~on_write:(fun () ->
          if !first then begin
            first := false;
            failwith "display formatter refused"
          end)
        ()
    in
    let previous = Logs.reporter () in
    let over_count = ref 0 and continued = ref 0 in
    run env sink (fun _ ->
        send (Logs.reporter ()) "application is still running"
          ~over:(fun () -> incr over_count)
          ~continued:(fun () -> incr continued));
    Alcotest.(check (list string))
      "the previous reporter receives the refused record once"
      [ "application is still running" ]
      (kept ());
    Alcotest.(check int) "completion runs once" 1 !over_count;
    Alcotest.(check int) "the caller continues once" 1 !continued;
    restored previous

  let stale_reporter () =
    with_runtime @@ fun env ->
    recording @@ fun kept ->
    let sink = sink () in
    let previous = Logs.reporter () in
    let reporter = run env sink (fun _ -> Logs.reporter ()) in
    send reporter "late";
    Alcotest.(check (list string))
      "the previous reporter has the message" [ "late" ] (kept ());
    Alcotest.(check string)
      "closed reporter writes zero bytes" ""
      (Buffer.contents sink.buffer);
    restored previous

  let foreign_domain () =
    with_runtime @@ fun env ->
    recording @@ fun kept ->
    let sink = sink () in
    run env sink (fun _ ->
        let reporter = Logs.reporter () in
        Domain.join (Domain.spawn (fun () -> send reporter "raw")));
    Alcotest.(check (list string))
      "the previous reporter has the message" [ "raw" ] (kept ());
    Alcotest.(check string)
      "the display writes nothing" ""
      (Buffer.contents sink.buffer)

  let blocked_sink enter =
    let in_output = ref false in
    let output =
      sink
        ~on_write:(fun () ->
          in_output := true;
          Fun.protect
            (fun () ->
              ignore (Eio.Promise.try_resolve enter () : bool);
              Eio.Fiber.await_cancel ())
            ~finally:(fun () -> in_output := false))
        ()
    in
    (output, in_output)

  let close_waiting_worker ?(fail = true) () =
    with_runtime @@ fun env ->
    recording @@ fun kept ->
    Eio.Switch.run @@ fun outer ->
    let workers = pool env outer in
    let entered, enter = Eio.Promise.create () in
    let sink, in_output = blocked_sink enter in
    let previous = Logs.reporter () in
    let worker = ref None and continued = Atomic.make 0 in
    let run () =
      run env sink (fun _ ->
          worker :=
            Some
              (Eio.Executor_pool.submit_fork ~sw:outer workers ~weight:1.
                 (fun () ->
                   send (Logs.reporter ()) "waiting" ~continued:(fun () ->
                       Atomic.incr continued)));
          Eio.Promise.await entered;
          if fail then failwith "close display")
    in
    if fail then
      Alcotest.check_raises "original body failure survives teardown"
        (Failure "close display") run
    else run ();

    Eio.Promise.await_exn (Option.get !worker);
    Alcotest.(check bool)
      "consumer stopped before display teardown" false !in_output;
    Alcotest.(check (list string))
      "the previous reporter has the message" [ "waiting" ] (kept ());
    Alcotest.(check int) "the log continues once" 1 (Atomic.get continued);
    restored previous

  let suite =
    ( "report_domains",
      [
        Alcotest.test_case "worker logs retain formatter ownership" `Quick
          owner_domain;
        Alcotest.test_case "a worker's message within a task is the task's"
          `Quick worker_task;
        Alcotest.test_case "a refused Logs record reaches the prior reporter"
          `Quick output_refusal_falls_back;
        Alcotest.test_case
          "a worker's refused record reaches the prior reporter" `Quick
          output_error;
        Alcotest.test_case "a captured reporter hands over after teardown"
          `Quick stale_reporter;
        Alcotest.test_case "a domain Eio does not run hands over" `Quick
          foreign_domain;
        Alcotest.test_case "scope closure releases a waiting external worker"
          `Quick
          (close_waiting_worker ~fail:true);
        Alcotest.test_case "normal closure releases a waiting external worker"
          `Quick
          (close_waiting_worker ~fail:false);
      ] )
end

let report_domains_suite = Report_domains.suite
