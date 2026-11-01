(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness
module P = Console_eio.Prompt

let prompt ?interactive actions =
  let flow = Eio_mock.Flow.make "prompt" in
  let out = Buffer.create 64 in
  Eio_mock.Flow.on_read flow actions;
  (P.v ?interactive ~ppf:(Format.formatter_of_buffer out) flow, out)

let got expected = function
  | Ok s -> Alcotest.(check string) "answer" expected s
  | Error `End_of_input -> Alcotest.fail "end of input"
  | Error (`Msg m) -> Alcotest.fail m

let refused = function
  | Error (`Msg _) -> ()
  | Error `End_of_input -> Alcotest.fail "end of input"
  | Ok _ -> Alcotest.fail "expected refusal"

let ended = function
  | Error `End_of_input -> ()
  | Error (`Msg m) -> Alcotest.fail m
  | Ok _ -> Alcotest.fail "expected the end of the input"

let message = function `Msg s -> s | `End_of_input -> "end of input"

let byte_reads s =
  List.init (String.length s) (fun i -> `Return (String.make 1 s.[i]))

let test_bytewise () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true
      (byte_reads " alpha \r\nbeta\n" @ [ `Raise End_of_file ])
  in
  Alcotest.(check bool) "interactive override" true (P.interactive t);
  got " alpha " (P.line t "first: ");
  got "beta" (P.line t "second: ");
  ended (P.line t "last: ");
  Alcotest.(check string)
    "questions flushed" "first: second: last: " (Buffer.contents out)

let test_utf8_split () =
  Eio_mock.Backend.run @@ fun () ->
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x754c);
  let s = Buffer.contents b in
  let t, _ =
    prompt ~interactive:true
      [
        `Return (String.sub s 0 1);
        `Return (String.sub s 1 (String.length s - 1) ^ "\nnext\n");
        `Raise End_of_file;
      ]
  in
  got s (P.line t "utf8: ");
  got "next" (P.line t "next: ")

let test_partial_eof () =
  Eio_mock.Backend.run @@ fun () ->
  let t, _ =
    prompt ~interactive:true [ `Return "unfinished"; `Raise End_of_file ]
  in
  (* Buf_read.line permits an unterminated final line; the prompt documents
     an Error when input ends. A partial answer must not open a typed gate. *)
  ended (P.line t "line: ")

exception Read_failure

let test_read_failure () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true [ `Return "part"; `Raise Read_failure ]
  in
  Alcotest.check_raises "read exception propagates" Read_failure (fun () ->
      ignore (P.line t "question: "));
  Alcotest.(check string)
    "flushed before fault" "question: " (Buffer.contents out)

let test_questions () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true (byte_reads " YES \nno\n" @ [ `Raise End_of_file ])
  in
  Alcotest.(check (result bool string))
    "yes" (Ok true)
    (Result.map_error message (P.confirm t "go?"));
  Alcotest.(check (result bool string))
    "no" (Ok false)
    (Result.map_error message (P.confirm t "go?"));
  Alcotest.(check string)
    "question syntax" "go? [y/N] go? [y/N] " (Buffer.contents out);
  ended (P.confirm t "end?")

let test_nonterminal () =
  Eio_mock.Backend.run @@ fun () ->
  List.iter
    (fun interactive ->
      let t, out = prompt ?interactive [ `Raise Read_failure ] in
      Alcotest.(check bool) "mock has no terminal" false (P.interactive t);
      refused (P.line t "line");
      refused (P.confirm t "confirm");
      refused (P.secret t "secret");
      Alcotest.(check string) "no output" "" (Buffer.contents out))
    [ None; Some false ];
  let t, out = prompt ~interactive:true [ `Raise Read_failure ] in
  refused (P.secret t "secret");
  Alcotest.(check string) "no masking without tty" "" (Buffer.contents out)

let test_limit () =
  Eio_mock.Backend.run @@ fun () ->
  let t, _ = prompt ~interactive:true (byte_reads (String.make 4097 'a')) in
  refused (P.line t "too long: ")

(* An answer too long is refused, and the next question reads the line
   after it, not what is left of it. *)
let test_limit_skipped () =
  Eio_mock.Backend.run @@ fun () ->
  let t, _ =
    prompt ~interactive:true
      (byte_reads (String.make 5000 'a' ^ "\nnext\n") @ [ `Raise End_of_file ])
  in
  refused (P.line t "too long: ");
  got "next" (P.line t "next: ")

let test_run_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer out in
  let value =
    Console_eio.run ~clock ~ppf ~label:"build" (fun d ->
        let task = Console.Task.start d "compile" in
        Eio.Fiber.both
          (fun () ->
            Eio.Time.Mono.sleep clock 0.5;
            Console.Task.succeed task)
          (fun () ->
            Eio.Fiber.yield ();
            Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 500_000_000L));
        Console_eio.run ~clock ~ppf (fun nested ->
            Alcotest.(check bool) "same display" true (nested == d));
        42)
  in
  Alcotest.(check int) "result" 42 value;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "task output" true
    (contains "compile" (Buffer.contents out));
  Alcotest.(check bool)
    "no cursor codes" false
    (String.contains (Buffer.contents out) '\027')

exception Body_failure

let test_run_failure () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer out in
  Alcotest.check_raises "body exception" Body_failure (fun () ->
      Console_eio.run ~clock ~ppf (fun d ->
          ignore (Console.Task.start d "abandoned");
          raise Body_failure));
  Console_eio.run ~clock ~ppf (fun _ -> ());
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "cancelled task" true
    (contains "\xe2\x97\x8b abandoned" (Buffer.contents out))

let test_nested_fiber () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let ppf = Format.formatter_of_buffer (Buffer.create 64) in
  Console_eio.run ~clock ~ppf (fun d ->
      Eio.Fiber.both
        (fun () ->
          Console_eio.run ~clock ~ppf (fun nested ->
              Alcotest.(check bool) "inherited display" true (d == nested)))
        Eio.Fiber.yield)

(* Two runs in sibling fibers share one display, which closes once the
   last of them ends: the task the second starts after the first returned
   still prints its lines. A run that starts while the other's display
   closes opens its own once that one is gone. *)
let test_sibling_fibers () =
  let siblings ~overlap =
    Eio_mock.Backend.run @@ fun () ->
    let clock = Eio_mock.Clock.Mono.make () in
    let out = Buffer.create 128 in
    let ppf = Format.formatter_of_buffer out in
    let first = ref None and same = ref false in
    Eio.Fiber.both
      (fun () ->
        Console_eio.run ~clock ~ppf (fun d ->
            first := Some d;
            if overlap then Eio.Fiber.yield ()))
      (fun () ->
        Console_eio.run ~clock ~ppf (fun d ->
            same := Option.fold ~none:false ~some:(( == ) d) !first;
            Eio.Fiber.yield ();
            Console.Task.run d "late" ignore));
    Format.pp_print_flush ppf ();
    (!same, contains "late" (Buffer.contents out))
  in
  Alcotest.(check (pair bool bool))
    "overlapping runs share the display" (true, true) (siblings ~overlap:true);
  Alcotest.(check (pair bool bool))
    "a run after the other's display" (false, true) (siblings ~overlap:false)

(* A callback failure belongs to its run; it cannot close the display a
   sibling still uses, even when that sibling yields while producing output. *)
let test_failing_first_run_waits () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  let entered, enter = Eio.Promise.create () in
  let sibling_finished = ref false and returned_early = ref false in
  Eio.Fiber.both
    (fun () ->
      Alcotest.check_raises "the first callback's exception survives"
        (Failure "first callback failed") (fun () ->
          Console_eio.run ~clock ~ppf (fun _ ->
              Eio.Promise.await entered;
              failwith "first callback failed"));
      returned_early := not !sibling_finished)
    (fun () ->
      Console_eio.run ~clock ~ppf (fun d ->
          Eio.Promise.resolve enter ();
          Eio.Fiber.yield ();
          Console.Task.run d "sibling" (fun task ->
              for i = 0 to 2 do
                Console.Task.log task (Fmt.str "record-%d" i);
                Eio.Fiber.yield ()
              done);
          sibling_finished := true));
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "the first run waits for the sibling's callback" false !returned_early;
  for i = 0 to 2 do
    Alcotest.(check bool)
      "every sibling record survives" true
      (contains (Fmt.str "sibling: record-%d" i) (Buffer.contents out))
  done;
  Alcotest.(check bool)
    "a successful sibling is not cancelled" false
    (contains "\u{25cb} sibling" (Buffer.contents out))

(* A fiber may outlive the run that forked it by using an outer switch.
   Its inherited context must not resurrect the display that already closed. *)
let test_run_from_surviving_fiber () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Fmt.set_utf_8 ppf true;
  let original = ref None and reused = ref false in
  Eio.Switch.run (fun sw ->
      let ended, finish = Eio.Promise.create () in
      Console_eio.run ~clock ~ppf ~label:"first" (fun d ->
          original := Some d;
          Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.await ended;
              Console_eio.run ~clock ~ppf ~label:"second" (fun next ->
                  reused :=
                    Option.fold ~none:false ~some:(( == ) next) !original;
                  Console.Task.run next "second-task" (fun task ->
                      Console.Task.log task "hello"))));
      Eio.Promise.resolve finish ());
  Format.pp_print_flush ppf ();
  Alcotest.(check bool) "a closed display is not reused" false !reused;
  Alcotest.(check bool)
    "the later task's output survives" true
    (contains "second-task: hello" (Buffer.contents out));
  Alcotest.(check bool)
    "the later run has its own header" true
    (contains "second 0.0s" (Buffer.contents out))

let renderer_equal a b =
  match (a, b) with `Ansi_tty, `Ansi_tty | `None, `None -> true | _ -> false

let test_setup () =
  Eio_mock.Backend.run @@ fun () ->
  let a = Fmt.style_renderer Fmt.stdout and b = Fmt.style_renderer Fmt.stderr in
  Fun.protect
    ~finally:(fun () ->
      Fmt.set_style_renderer Fmt.stdout a;
      Fmt.set_style_renderer Fmt.stderr b)
    (fun () ->
      List.iter
        (fun renderer ->
          Console_tty.setup ~style_renderer:renderer ();
          Alcotest.(check bool)
            "stdout" true
            (renderer_equal (Fmt.style_renderer Fmt.stdout) renderer);
          Alcotest.(check bool)
            "stderr" true
            (renderer_equal (Fmt.style_renderer Fmt.stderr) renderer))
        [ `None; `Ansi_tty ];
      Console_tty.setup ())

(* Lines logged from nested runs in fibers and from other domains all reach
   the output, each source's in the order it logged them. *)
let source = Logs.Src.create "console.concurrent"

let domain_records reporter prefix count =
  for i = 0 to count - 1 do
    reporter.Logs.report source Logs.Info
      ~over:(fun () -> ())
      (fun () -> ())
      (fun m -> m "%s%04d" prefix i)
  done

let records output prefix =
  List.filter_map
    (fun line ->
      Option.map
        (fun i -> String.sub line i (String.length line - i))
        (Astring.String.find_sub ~sub:prefix line))
    (String.split_on_char '\n' output)

let test_concurrent_order () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pool =
    Eio.Executor_pool.create ~sw ~domain_count:2 (Eio.Stdenv.domain_mgr env)
  in
  let count = 20 in
  let output = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer output in
  let clock = Eio_mock.Clock.Mono.make () in
  Console_eio.run ~clock ~ppf (fun display ->
      let reporter = Logs.reporter () in
      let fibers =
        List.init 2 (fun worker () ->
            Console_eio.run ~clock (fun nested ->
                Alcotest.(check bool) "same display" true (nested == display);
                let task =
                  Console.Task.start nested ("fiber" ^ string_of_int worker)
                in
                for i = 0 to count - 1 do
                  Console.Task.log task (Fmt.str "event%04d" i);
                  Eio.Fiber.yield ()
                done;
                Console.Task.succeed task))
      in
      let domains =
        List.init 2 (fun worker () ->
            Eio.Executor_pool.submit_exn pool ~weight:1. (fun () ->
                domain_records reporter
                  ("domain" ^ string_of_int worker ^ "_event")
                  count))
      in
      Eio.Fiber.all (fibers @ domains));
  Format.pp_print_flush ppf ();
  let output = Buffer.contents output in
  Alcotest.(check bool)
    "plain UTF-8" true
    (String.is_valid_utf_8 output && not (String.contains output '\027'));
  for worker = 0 to 1 do
    List.iter
      (fun prefix ->
        Alcotest.(check (list string))
          prefix
          (List.init count (fun i -> Fmt.str "%s%04d" prefix i))
          (records output prefix))
      [
        "fiber" ^ string_of_int worker ^ ": event";
        "domain" ^ string_of_int worker ^ "_event";
      ]
  done

(* A signal handler the program installs while a run holds the terminal is
   still in place after the run. *)
let mine (_ : int) = ()

let test_handler_kept () =
  let before = Sys.signal Sys.sigterm Sys.Signal_default in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigterm before)
  @@ fun () ->
  Eio_main.run @@ fun _env ->
  let ppf = Format.formatter_of_buffer (Buffer.create 64) in
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~ppf (fun _ ->
      Sys.set_signal Sys.sigterm (Sys.Signal_handle mine));
  Alcotest.(check bool)
    "the program's handler" true
    (match Sys.signal Sys.sigterm Sys.Signal_default with
    | Sys.Signal_handle f -> f == mine
    | _ -> false)

let suite =
  ( "session",
    [
      Alcotest.test_case "bytewise CRLF and buffer reuse" `Quick test_bytewise;
      Alcotest.test_case "nested fibers and domains keep their order" `Quick
        test_concurrent_order;
      Alcotest.test_case "a handler set during a run is kept" `Quick
        test_handler_kept;
      Alcotest.test_case "split UTF-8 and buffered second line" `Quick
        test_utf8_split;
      Alcotest.test_case "partial line at EOF is refused" `Quick
        test_partial_eof;
      Alcotest.test_case "read fault propagates after flush" `Quick
        test_read_failure;
      Alcotest.test_case "confirm scripted answers" `Quick test_questions;
      Alcotest.test_case "nonterminal prompts never read" `Quick
        test_nonterminal;
      Alcotest.test_case "answer limit" `Quick test_limit;
      Alcotest.test_case "an answer too long is skipped" `Quick
        test_limit_skipped;
      Alcotest.test_case "run with mock clock" `Quick test_run_clock;
      Alcotest.test_case "run failure releases display" `Quick test_run_failure;
      Alcotest.test_case "a surviving fiber opens a fresh display" `Quick
        test_run_from_surviving_fiber;
      Alcotest.test_case "nested child fiber shares display" `Quick
        test_nested_fiber;
      Alcotest.test_case "runs in sibling fibers share a display" `Quick
        test_sibling_fibers;
      Alcotest.test_case "a failing first run waits for its sibling" `Quick
        test_failing_first_run_waits;
      Alcotest.test_case "setup under mock backend" `Quick test_setup;
    ] )
