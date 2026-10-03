let with_clock f = Eio_main.run @@ fun env -> f (Eio.Stdenv.clock env)

let test_run_returns_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      let result = Console_eio.run ~clock ~ppf (fun _display -> 42) in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

let test_run_exception_without_cursor_restore_on_non_tty () =
  with_clock (fun clock ->
      let buf = Buffer.create 64 in
      let ppf = Format.formatter_of_buffer buf in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun _display -> failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check string) "no cursor restore" "" (Buffer.contents buf))

let test_nested_run_reuses_the_existing_display () =
  with_clock (fun clock ->
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      Console_eio.run ~clock ~ppf ~mode:`History (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              let task = Console.Display.task inner "nested work" in
              Console.Display.succeed task));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses outer output" true
        (String.starts_with ~prefix:"=> nested work" (Buffer.contents buf)))

(* A caller that grades its own live region hands [run_on] the context
   instead of letting [run] build one. What proves the handed context was
   used, rather than rebuilt from stdout and the machine clock, is the elapsed
   time: no real clock reads 1.3s over work that takes none, and no default
   formatter puts the line in this buffer. *)
let test_run_on_takes_the_callers_context () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~wait:(fun _ -> Eio.Fiber.await_cancel ())
      ~dimensions:(fun () -> (24, 10))
      ~is_tty:false ()
  in
  Console_eio.run_on ~ctx ~mode:`History (fun display ->
      let task = Console.Display.task display "step" in
      now := 1.3;
      Console.Display.succeed task);
  Format.pp_print_flush ppf ();
  Alcotest.(check string)
    "the caller's formatter and the caller's clock" "=> step  1.3s\n"
    (Buffer.contents buf);
  Alcotest.(check (float 0.))
    "the context reads the same clock" 1.3
    (Console.Display.ctx_now ctx)

(* The context's clock is the display's only one: the refresh waits through
   the context, and what the display reads as now is that clock's reading. The
   clock here advances only when it is waited on, so a refresh paced by any
   other clock leaves it at zero and records no wait. *)
let test_run_on_waits_on_the_contexts_clock () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let waits = ref [] in
  let wait seconds =
    waits := seconds :: !waits;
    now := !now +. seconds;
    Eio.Fiber.yield ()
  in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~wait
      ~dimensions:(fun () -> (24, 10))
      ~is_tty:false ()
  in
  let read =
    Console_eio.run_on ~ctx ~mode:`History (fun display ->
        for _ = 1 to 3 do
          Eio.Fiber.yield ()
        done;
        Console.Display.now display)
  in
  Alcotest.(check (list (float 0.)))
    "the refresh waited on the context's clock, once before the body and once \
     on each of its yields"
    [ 0.12; 0.12; 0.12; 0.12 ] (List.rev !waits);
  Alcotest.(check (float 0.))
    "the display reads that clock"
    (0.12 +. 0.12 +. 0.12 +. 0.12)
    read

(* A context built without [~wait] can be drawn on but not driven, and a driver
   says so before it draws, rather than refreshing on a clock nobody named. *)
let test_run_on_refuses_a_context_it_cannot_wait_on () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> 0.)
      ~dimensions:(fun () -> (24, 10))
      ~is_tty:false ()
  in
  Alcotest.check_raises "refused"
    (Invalid_argument
       "Console.Display.wait: the context was built without ~wait, so its \
        clock cannot be waited on") (fun () ->
      Console_eio.run_on ~ctx ~mode:`History (fun _display ->
          Alcotest.fail "the body ran under a display with no clock to wait on"))

(* The terminal question, asked of the library rather than of Unix. Under
   [dune test] stdout is a pipe or a file, so the answer is [false]; a runner
   that attached a terminal leaves the case nothing to pin. *)
let test_is_tty_is_false_without_a_terminal () =
  if Unix.isatty Unix.stdout then Alcotest.skip ();
  Alcotest.(check bool) "stdout is not a terminal" false (Console_eio.is_tty ())

(* Dimensions always answer, whatever the environment: the ioctl, else
   $COLUMNS/$LINES, else the 80x24 default. No source of an answer can make
   either component smaller than a single cell. *)
let test_dimensions_are_at_least_one_cell () =
  let width, height = Console_eio.dimensions () in
  Alcotest.(check bool) "width is at least one cell" true (width >= 1);
  Alcotest.(check bool) "height is at least one cell" true (height >= 1)

(* An elapsed time comes from the machine, not from the program, so a test that
   captures a display's output cannot assert one. CONSOLE_FROZEN_CLOCK is the
   door that makes such output reproducible: the same work, slow enough that a
   real clock would show it, renders 0.0s with the variable set. Both halves are
   asserted, because a frozen clock that was already frozen proves nothing. *)
let capture_a_slow_task () =
  with_clock (fun clock ->
      let buf = Buffer.create 128 in
      let ppf = Format.formatter_of_buffer buf in
      Console_eio.run ~clock ~ppf ~mode:`History (fun display ->
          let task = Console.Display.task display "slow step" in
          Eio.Time.sleep clock 0.25;
          Console.Display.succeed task);
      Format.pp_print_flush ppf ();
      Buffer.contents buf)

let ends_with_frozen_elapsed line =
  String.length line > 5 && String.ends_with ~suffix:"  0.0s" line

let test_frozen_clock_pins_every_elapsed () =
  let variable = "CONSOLE_FROZEN_CLOCK" in
  let running = capture_a_slow_task () in
  Alcotest.(check bool)
    "a real clock times the work" false
    (ends_with_frozen_elapsed (String.trim running));
  Unix.putenv variable "1";
  let frozen =
    Fun.protect ~finally:(fun () -> Unix.putenv variable "") capture_a_slow_task
  in
  Alcotest.(check bool)
    "a frozen clock reads zero" true
    (ends_with_frozen_elapsed (String.trim frozen));
  Alcotest.(check bool)
    "the variable no longer freezes once cleared" false
    (ends_with_frozen_elapsed (String.trim (capture_a_slow_task ())))

(* Under a terminal [run] takes SIGINT, to give the cursor back before the
   process dies of it, and on return gives back the handler it displaced. A
   program whose Ctrl-C is its own affair installs its handler under [run]
   and keeps it past [run]'s return, as a run that must answer a Ctrl-C after
   its display has closed does: what [run] restores is the handler it found,
   and only where its own still stands. The terminal is a pseudo-terminal put
   on stdout for the span of the case. *)
let handler_installed_under_run (_ : int) = ()
let handler_installed_before_run (_ : int) = ()

let is handler = function
  | Sys.Signal_handle f -> f == handler
  | Sys.Signal_default | Sys.Signal_ignore -> false

let test_a_handler_installed_under_run_outlives_it () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  (* What the display draws is read and dropped, so no write to the terminal
     blocks on a full buffer. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let buf = Cstruct.create 4096 in
      let source = Eio_unix.Pty.source pty in
      (try
         while true do
           ignore (Eio.Flow.single_read source buf : int)
         done
       with End_of_file | Eio.Io _ -> ());
      `Stop_daemon);
  let stdout = Unix.dup Unix.stdout in
  let before = Sys.signal Sys.sigint Sys.Signal_default in
  Sys.set_signal Sys.sigint before;
  Fun.protect ~finally:(fun () ->
      Unix.dup2 stdout Unix.stdout;
      Unix.close stdout;
      Sys.set_signal Sys.sigint before)
  @@ fun () ->
  Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) (fun tty ->
      Unix.dup2 tty Unix.stdout);
  let run f = Console_eio.run ~clock:(Eio.Stdenv.clock env) ~mode:`History f in
  (* The control: [run] takes the terminal, installs a handler of its own and,
     where nothing replaced it, gives back the one it found. *)
  Sys.set_signal Sys.sigint (Sys.Signal_handle handler_installed_before_run);
  run (fun _display ->
      match Sys.signal Sys.sigint Sys.Signal_default with
      | Sys.Signal_handle h as own when h != handler_installed_before_run ->
          Sys.set_signal Sys.sigint own
      | Sys.Signal_handle _ | Sys.Signal_default | Sys.Signal_ignore ->
          Alcotest.fail "run installed no handler: stdout is not a terminal");
  Alcotest.(check bool)
    "the handler run found is back once run returns" true
    (is handler_installed_before_run (Sys.signal Sys.sigint Sys.Signal_default));
  run (fun _display ->
      Sys.set_signal Sys.sigint (Sys.Signal_handle handler_installed_under_run));
  Alcotest.(check bool)
    "the handler installed under run still stands after it" true
    (is handler_installed_under_run (Sys.signal Sys.sigint Sys.Signal_default))

(* A history display on a terminal hides no cursor, so it writes nothing to
   show one: what reaches the terminal is what the caller printed, the bytes a
   pipe would get apart from the terminal's own CR before each LF. *)
let test_history_on_a_terminal_writes_only_its_output () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  let stdout = Unix.dup Unix.stdout in
  ( Fun.protect ~finally:(fun () ->
        Unix.dup2 stdout Unix.stdout;
        Unix.close stdout)
  @@ fun () ->
    Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) (fun tty ->
        Unix.dup2 tty Unix.stdout);
    Console_eio.run ~clock:(Eio.Stdenv.clock env) ~mode:`History (fun display ->
        Console.Display.with_output display (fun ppf -> Fmt.pf ppf "plain@."))
  );
  let written = Buffer.create 64 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  (try
     while true do
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.2 (fun () ->
           let n = Eio.Flow.single_read source buf in
           Buffer.add_string written (Cstruct.to_string ~len:n buf))
     done
   with Eio.Time.Timeout | End_of_file | Eio.Io _ -> ());
  Alcotest.(check string)
    "only the output" "plain\r\n" (Buffer.contents written)

(* [setup] reads the environment once: NO_COLOR turns stdout's styling off
   whatever it is, and TERM sets the depth colours are written at. *)
let test_setup_reads_the_environment () =
  let saved name = (name, Sys.getenv_opt name) in
  let env = List.map saved [ "NO_COLOR"; "TERM"; "COLORTERM" ] in
  let restore () =
    List.iter
      (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
      env;
    Console.Color.set_depth `True_color;
    Fmt.set_style_renderer Fmt.stdout `None;
    Fmt.set_style_renderer Fmt.stderr `None
  in
  Fun.protect ~finally:restore (fun () ->
      Unix.putenv "NO_COLOR" "1";
      Unix.putenv "TERM" "xterm-256color";
      Unix.putenv "COLORTERM" "";
      Console_eio.setup ~style_renderer:`Ansi_tty ();
      Alcotest.(check bool)
        "--color wins over NO_COLOR" true
        (Fmt.style_renderer Fmt.stdout = `Ansi_tty);
      Alcotest.(check bool)
        "depth from TERM" true
        (Console.Color.depth () = `Ansi_256);
      Console_eio.setup ();
      Alcotest.(check bool)
        "NO_COLOR" true
        (Fmt.style_renderer Fmt.stdout = `None))

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "setup reads the environment" `Quick
        test_setup_reads_the_environment;
      Alcotest.test_case "run returns without cursor restore on non-tty" `Quick
        test_run_returns_without_cursor_restore_on_non_tty;
      Alcotest.test_case "run exception without cursor restore on non-tty"
        `Quick test_run_exception_without_cursor_restore_on_non_tty;
      Alcotest.test_case "nested run reuses the existing display" `Quick
        test_nested_run_reuses_the_existing_display;
      Alcotest.test_case "run_on takes the caller's context" `Quick
        test_run_on_takes_the_callers_context;
      Alcotest.test_case "run_on waits on the context's clock" `Quick
        test_run_on_waits_on_the_contexts_clock;
      Alcotest.test_case "run_on refuses a context it cannot wait on" `Quick
        test_run_on_refuses_a_context_it_cannot_wait_on;
      Alcotest.test_case "is_tty is false without a terminal" `Quick
        test_is_tty_is_false_without_a_terminal;
      Alcotest.test_case "dimensions are at least one cell" `Quick
        test_dimensions_are_at_least_one_cell;
      Alcotest.test_case "frozen clock pins every elapsed" `Quick
        test_frozen_clock_pins_every_elapsed;
      Alcotest.test_case "a handler installed under run outlives it" `Quick
        test_a_handler_installed_under_run_outlives_it;
      Alcotest.test_case "history on a terminal writes only its output" `Quick
        test_history_on_a_terminal_writes_only_its_output;
    ] )
