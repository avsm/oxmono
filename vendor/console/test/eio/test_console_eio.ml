(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

let with_clock f = Eio_main.run @@ fun _env -> f (Eio_mock.Clock.Mono.make ())

let buffer () =
  let buf = Buffer.create 64 in
  (buf, Format.formatter_of_buffer buf)

let test_run_returns () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      let result = Console_eio.run ~clock ~ppf (fun _display -> 42) in
      Format.pp_print_flush ppf ();
      Alcotest.(check int) "result" 42 result;
      Alcotest.(check string) "nothing on a pipe" "" (Buffer.contents buf))

let seconds s = Mtime.of_uint64_ns (Int64.of_float (s *. 1e9))

(* What a task that waits two seconds on a mock monotonic clock prints. *)
let nap () =
  Eio_main.run @@ fun _ ->
  let clock = Eio_mock.Clock.Mono.make () in
  let buf, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Task.run display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () -> Eio.Time.Mono.sleep clock 2.)
            (fun () -> Eio_mock.Clock.Mono.set_time clock (seconds 2.))));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let took s out = contains ("\u{2713} nap  " ^ s) out

(* A display reads the time on its monotonic clock. *)
let test_run_on_a_monotonic_clock () =
  Alcotest.(check bool) "the time it took" true (took "2.0s" (nap ()))

let with_env bindings f =
  let saved = List.map (fun (k, _) -> (k, Sys.getenv_opt k)) bindings in
  List.iter (fun (k, v) -> Unix.putenv k v) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value v ~default:"")) saved)

(* [mock] at its next timer, if any: a sleep that waits for its turn on
   [Console_eio.clock] has none. *)
let step mock =
  try Eio_mock.Clock.Mono.advance mock with Invalid_argument _ -> ()

(* What a task prints that sleeps 1.5 and 0.5 seconds on [Console_eio.clock]
   of a mock clock, which moves to the end of each sleep in turn. *)
let sleeps () =
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let buf, ppf = buffer () in
  let slept = ref false in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Task.run display "nap" (fun _ ->
          Eio.Fiber.both
            (fun () ->
              Eio.Time.Mono.sleep clock 1.5;
              Eio.Time.Mono.sleep clock 0.5;
              slept := true)
            (fun () ->
              while not !slept do
                Eio.Fiber.yield ();
                step mock
              done)));

  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* What a task prints whose two fibers sleep 1 and 2 seconds at once on
   [Console_eio.clock] of a mock clock. *)
let overlapping () =
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let buf, ppf = buffer () in
  let slept = ref 0 in
  Console_eio.run ~clock ~ppf (fun display ->
      Console.Task.run display "nap" (fun _ ->
          Eio.Fiber.all
            [
              (fun () ->
                Eio.Time.Mono.sleep clock 1.;
                incr slept);
              (fun () ->
                Eio.Time.Mono.sleep clock 2.;
                incr slept);
              (fun () ->
                while !slept < 2 do
                  Eio.Fiber.yield ();
                  step mock
                done);
            ]));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* CONSOLE_DETERMINISTIC_CLOCK, set and not empty, makes the display read
   the time of the sleeps on a clock of [Console_eio.clock], and zero on
   another clock; the waits still run on the clock behind it. *)
let test_deterministic_clock () =
  let deterministic f =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] (fun () -> f ())
  in
  Alcotest.(check bool) "a moving clock" true (took "2.0s" (nap ()));
  Alcotest.(check bool)
    "another clock stops" true
    (took "0.0s" (deterministic nap));
  let first = deterministic sleeps in
  Alcotest.(check bool) "one sleep after the other" true (took "2.0s" first);
  Alcotest.(check string) "the same twice" first (deterministic sleeps);
  Alcotest.(check bool)
    "sleeps at once overlap" true
    (took "2.0s" (deterministic overlapping));
  let empty =
    with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "") ] (fun () -> nap ())
  in
  Alcotest.(check bool) "an empty value" true (took "2.0s" empty)

(* The two builds of examples/columns, with sleeps on [Console_eio.clock] of
   a mock clock whose real time a seeded random schedule moves: each build
   works a random real time before each sleep, which wakes the sleeps then
   due at once, and a driver moves the clock by random steps after random
   numbers of yields. *)
let perturbed ?cast ~progress seed =
  let env = match cast with Some f -> [ ("CONSOLE_CAST", f) ] | None -> [] in
  with_env env @@ fun () ->
  let rand = Random.State.make [| seed |] in
  let mock = Eio_mock.Clock.Mono.make () in
  let real = ref 0 in
  let pass () =
    if seed > 0 then (
      real := !real + Random.State.int rand 300_000_000;
      Eio_mock.Clock.Mono.set_time mock
        (Mtime.of_uint64_ns (Int64.of_int !real)))
  in
  let clock = Console_eio.clock mock in
  let build root (name, pace, steps) =
    Console.Task.run root name (fun b ->
        List.iter
          (fun (step, total) ->
            Console.Task.run b step (fun task ->
                for count = 1 to total do
                  pass ();
                  Eio.Time.Mono.sleep clock pace;
                  Console.Task.set_items task ~count ~total
                done))
          steps)
  in
  let builds =
    [
      ("linux", 0.08, [ ("fetch", 4); ("compile", 12); ("test", 8) ]);
      ( "macos",
        0.11,
        [ ("fetch", 4); ("compile", 12); ("test", 8); ("sign", 2) ] );
    ]
  in
  let buf, ppf = buffer () in
  let finished = ref false in
  Eio_main.run (fun _ ->
      Eio.Fiber.both
        (fun () ->
          Console_eio.run ~clock ~ppf ~progress ~keep_ended:true (fun root ->
              Eio.Fiber.List.iter (build root) builds);
          finished := true)
        (fun () ->
          while not !finished do
            for _ = 0 to Random.State.int rand 4 do
              Eio.Fiber.yield ()
            done;
            if seed > 0 then pass () else step mock
          done));
  Format.pp_print_flush ppf ();
  match cast with
  | Some file -> In_channel.with_open_bin file In_channel.input_all
  | None -> Buffer.contents buf

(* Whatever the real schedule, the virtual clock gives the same bytes: the
   same lines on a pipe, and the same frames on a terminal. *)
let test_deterministic_schedule () =
  with_env [ ("CONSOLE_DETERMINISTIC_CLOCK", "1") ] @@ fun () ->
  List.iter
    (fun (progress, ended) ->
      let calm = perturbed ~progress 0 in
      Alcotest.(check bool)
        "the builds take the longer time" true (contains ended calm);
      for seed = 1 to 50 do
        Alcotest.(check string)
          (Fmt.str "schedule %d" seed)
          calm (perturbed ~progress seed)
      done)
    [ (`Plain, "macos  2.9s"); (`Tty, "macos" ^ String.make 60 ' ' ^ "2.9s") ]

(* A recording made of [f] under [CONSOLE_CAST], on a terminal of 30 columns
   by 6 rows, its lines. *)
let recording f =
  let file = Filename.temp_file "console" ".cast" in
  with_env
    [
      ("CONSOLE_DETERMINISTIC_CLOCK", "1");
      ("CONSOLE_CAST", file);
      ("COLUMNS", "30");
      ("LINES", "6");
      ("TERM", "xterm");
    ]
    f;
  let lines = In_channel.with_open_bin file In_channel.input_lines in
  Sys.remove file;
  lines

let decode t line =
  match Jsont_bytesrw.decode_string t line with
  | Ok v -> v
  | Error e -> Alcotest.fail e

(* A task that sleeps 1.5 then 0.5 seconds, recorded, on a mock clock that
   never moves: a sleep that waited on it would never end, and the work
   fails if it does not end first. *)
let recorded_nap () =
  recording @@ fun () ->
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let _, ppf = buffer () in
  Eio.Fiber.first
    (fun () ->
      Console_eio.run ~clock ~ppf (fun root ->
          Console.Task.run root "nap" (fun t ->
              Eio.Time.Mono.sleep clock 1.5;
              Console.Task.set_items t ~count:1 ~total:2;
              Eio.Time.Mono.sleep clock 0.5;
              Console.Task.set_items t ~count:2 ~total:2));
      Format.pp_print_flush ppf ())
    (fun () ->
      for _ = 1 to 100_000 do
        Eio.Fiber.yield ()
      done;
      Alcotest.fail "a sleep waited for the real time")

(* A header, then one array a write, which reads back as it was written:
   the rows drawn, and nothing on the output itself. *)
let test_cast_format () =
  match recorded_nap () with
  | [] -> Alcotest.fail "no header"
  | header :: events ->
      Alcotest.(check string)
        "the header"
        {|{"version":2,"width":30,"height":6,"env":{"TERM":"xterm"}}|} header;
      Alcotest.(check bool) "events" true (List.length events > 2);
      List.iter
        (fun line ->
          let e = decode Console__.Cast.event line in
          Alcotest.(check bool)
            "an output event" true
            (contains {|,"o","|} line && String.starts_with ~prefix:"[" line);
          Alcotest.(check string)
            "read back" (line ^ "\n")
            (Console__.Cast.to_line Console__.Cast.event e))
        events;
      let first = decode Console__.Cast.event (List.hd events) in
      Alcotest.(check bool)
        "the rows drawn" true
        (contains "\027[?25l" first.data)

(* Each write is at its time on the virtual clock: the redraws a tenth of a
   second apart, the count when the first sleep ends, the last write when
   the second does. *)
let test_cast_times () =
  let events =
    List.map (decode Console__.Cast.event) (List.tl (recorded_nap ()))
  in
  let times = List.map (fun (e : Console__.Cast.event) -> e.time) events in
  Alcotest.(check bool) "in order" true (List.sort Float.compare times = times);
  Alcotest.(check bool)
    "at tenths of a second" true
    (List.for_all
       (fun t -> Float.abs ((t *. 10.) -. Float.round (t *. 10.)) < 1e-6)
       times);
  Alcotest.(check bool)
    "the count as the first sleep ends" true
    (List.exists
       (fun (e : Console__.Cast.event) -> e.time = 1.5 && contains "1/2" e.data)
       events);
  Alcotest.(check (float 0.))
    "the end" 2.
    (List.nth times (List.length times - 1))

(* What a run writes on [ppf] and then [text], recorded, the bytes of its
   events. *)
let recorded_text text =
  let lines =
    recording @@ fun () ->
    Eio_main.run @@ fun _ ->
    let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
    let _, ppf = buffer () in
    Console_eio.run ~clock ~ppf (fun root ->
        Console.Task.run root "nap" (fun _ -> Eio.Time.Mono.sleep clock 0.1));
    Fmt.pf ppf "%s@?" text
  in
  String.concat ""
    (List.map (fun l -> (decode Console__.Cast.event l).data) (List.tl lines))

(* A line feed is recorded after a carriage return, as a terminal's driver
   writes it, and one that has its carriage return already is left alone. *)
let test_cast_line_feeds () =
  let data = recorded_text "a\nb\r\nc\n" in
  Alcotest.(check bool)
    "each line feed after a carriage return" true
    (contains "a\r\nb\r\nc\r\n" data);
  Alcotest.(check bool) "no carriage return twice" false (contains "\r\r" data)

(* Whatever the real schedule, a recording is the same bytes. *)
let test_cast_schedule () =
  with_env
    [ ("CONSOLE_DETERMINISTIC_CLOCK", "1"); ("COLUMNS", "80"); ("LINES", "24") ]
  @@ fun () ->
  let recorded seed =
    let file = Filename.temp_file "console" ".cast" in
    Fun.protect
      ~finally:(fun () -> Sys.remove file)
      (fun () -> perturbed ~cast:file ~progress:`Tty seed)
  in
  let calm = recorded 0 in
  Alcotest.(check bool)
    "the builds take the longer time" true (contains "\n[2.86," calm);
  for seed = 1 to 50 do
    Alcotest.(check string) (Fmt.str "schedule %d" seed) calm (recorded seed)
  done

let test_run_raises () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
          Console_eio.run ~clock ~ppf (fun display ->
              ignore (Console.Task.start display "doomed");
              failwith "boom"));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the running task is cancelled" true
        (contains "\xe2\x97\x8b doomed" (Buffer.contents buf)))

(* A view that raises is dropped for the default one, and the run raises
   its exception: a view that raises at every frame, and one that raises
   only in the last, on a pipe and on a terminal. *)
let test_run_view_raises () =
  let always _ = Console.View.fit (fun _ -> failwith "view") in
  let last _ =
    Console.View.fit (fun room ->
        match Console.Room.height room with
        | None -> failwith "view"
        | Some _ -> Console.View.text "rows")
  in
  List.iter
    (fun (progress, view) ->
      with_clock (fun clock ->
          let buf, ppf = buffer () in
          Alcotest.check_raises "raised" (Failure "view") (fun () ->
              Console_eio.run ~clock ~ppf ~progress ~label:"work" ~view
                (fun root -> Console.Task.run root "step" ignore));
          Format.pp_print_flush ppf ();
          Alcotest.(check bool)
            "the default view" true
            (contains "work" (Buffer.contents buf))))
    [ (`Plain, always); (`Plain, last); (`Tty, always); (`Tty, last) ]

(* [commit] is what a top task leaves in the scrollback, as with
   [Progress.show]. *)
let test_run_commit () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      let commit t =
        Some (Console.View.text ("left by " ^ Console.Task.label t))
      in
      Console_eio.run ~clock ~ppf ~progress:`Tty ~commit (fun root ->
          Console.Task.run root "step" ignore);
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "the commit" true
        (contains "left by step" (Buffer.contents buf)))

(* [print] writes a view above the rows, styled, at the terminal's width,
   on a pipe in turn with the lines, and as events an output event of no
   task. *)
let test_run_print () =
  let printed progress =
    with_clock (fun clock ->
        let buf, ppf = buffer () in
        Fmt.set_style_renderer ppf `Ansi_tty;
        let warning =
          Console.View.(
            styled (Console.Style.fg Console.Color.red) (text "warning:"))
        in
        Console_eio.run ~clock ~ppf ~progress (fun root ->
            Console.Task.run root "step" (fun _ ->
                Console_eio.print
                  Console.View.(hcat ~gap:1 [ warning; text "careful" ])));
        Format.pp_print_flush ppf ();
        Buffer.contents buf)
  in
  let tty = printed `Tty in
  Alcotest.(check bool) "above the rows" true (contains "careful" tty);
  Alcotest.(check bool) "styled" true (contains "\027[31m" tty);
  Alcotest.(check bool) "on a pipe" true (contains "careful" (printed `Plain));
  Alcotest.(check bool)
    "an event" true
    (contains {|"event":"output","text":"warning: careful"|} (printed `Json))

(* What [f] writes on the standard error, read back from a file. *)
let on_stderr f =
  let file = Filename.temp_file "console" ".err" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Fun.protect
    ~finally:(fun () ->
      Format.pp_print_flush Fmt.stderr ();
      Unix.dup2 saved Unix.stderr;
      List.iter Unix.close [ saved; fd ])
    (fun () ->
      Unix.dup2 fd Unix.stderr;
      f ());
  In_channel.with_open_bin file In_channel.input_all

let test_print_outside_eio () =
  Alcotest.(check bool)
    "with_output" true
    (Console_eio.with_output (fun ppf -> ppf == Fmt.stderr));
  let printed =
    on_stderr (fun () -> Console_eio.print (Console.View.text "outside"))
  in
  Alcotest.(check string) "print" "outside\n" printed

let test_nested_run_reuses_the_display () =
  with_clock (fun clock ->
      let buf, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun outer ->
          Console_eio.run ~clock (fun inner ->
              Alcotest.(check bool) "same display" true (outer == inner);
              Console.Task.succeed (Console.Task.start inner "nested work")));
      Format.pp_print_flush ppf ();
      Alcotest.(check bool)
        "nested task uses the outer output" true
        (String.starts_with
           ~prefix:"\xe2\x80\xa2 nested work\n\xe2\x9c\x93 nested work"
           (Buffer.contents buf)))

(* A display in another domain is refused, and the one that runs is not
   disturbed. *)
let test_second_display_refused () =
  Eio_main.run @@ fun env ->
  let clock = Eio_mock.Clock.Mono.make () in
  let _, ppf = buffer () in
  Console_eio.run ~clock ~ppf (fun _ ->
      (* Deliberately cross domains to test rejection of this nonportable API. *)
      let refused =
        Eio.Domain_manager.unsafe_run (Eio.Stdenv.domain_mgr env) (fun () ->
            let clock = Eio_mock.Clock.Mono.make () in
            let ppf = Format.make_formatter (fun _ _ _ -> ()) ignore in
            try
              Console_eio.run ~clock ~ppf ignore;
              false
            with Invalid_argument message ->
              message = "Console_eio.run: a display is already running")
      in
      Alcotest.(check bool) "refused" true refused);

  Console_eio.run ~clock ~ppf ignore

let test_dimensions_read_the_environment () =
  let check msg columns lines expected =
    with_env
      [ ("COLUMNS", columns); ("LINES", lines) ]
      (fun () ->
        Alcotest.(check (pair int int)) msg expected (Terminal.dimensions None))
  in
  check "the exported size" "123" "40" (123, 40);
  check "a small size" "10" "2" (10, 2);
  check "zero is no answer" "0" "0" (80, 24);
  check "nor is none" "" "" (80, 24)

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

(* What [teardown.exe how] writes on a terminal, how it exits, and whether
   the terminal echoes what is typed once it has. With [continue], the child
   is sent SIGCONT, as a shell's [fg] does, again and again until it exits,
   once it has shown the cursor to stop: a SIGCONT that comes before the
   child stops is lost, and none waits, but one that comes while its SIGTSTP
   is pending discards it. *)
let session ?(no_color = false) ?(lang = "en_US.UTF-8") ?(continue = false) how
    =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = Eio_unix.Pty.tty pty in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:
        [|
          "TERM=xterm-256color";
          ("NO_COLOR=" ^ if no_color then "1" else "");
          "LANG=" ^ lang;
        |]
      ~fds:[ (0, tty, `Blocking); (1, tty, `Blocking); (2, tty, `Blocking) ]
      ~executable:"./teardown.exe" [ "teardown.exe"; how ]
  in
  let status = ref None and echo = ref None in
  let exited = Atomic.make false and stopping = Atomic.make false in
  let on_read out =
    if not (Atomic.get stopping) then
      match Astring.String.find_sub ~sub:"[?25l" out with
      | Some i ->
          if Option.is_some (Astring.String.find_sub ~start:i ~sub:"[?25h" out)
          then Atomic.set stopping true
      | None -> ()
  in
  let fg () =
    let pid = Eio.Process.pid child in
    while not (Atomic.get stopping || Atomic.get exited) do
      Domain.cpu_relax ()
    done;
    while not (Atomic.get exited) do
      (try Unix.kill pid Sys.sigcont with Unix.Unix_error _ -> ());
      Domain.cpu_relax ()
    done
  in
  let await () =
    Fun.protect
      ~finally:(fun () -> Atomic.set exited true)
      (fun () -> status := Some (Eio.Process.await child))
  in
  let written =
    Pty_reader.read_all ~on_read pty (fun () ->
        if continue then
          Eio.Fiber.both await (fun () ->
              Eio_unix.run_in_systhread ~label:"fg" fg)
        else await ();
        echo := Some (Eio_unix.Fd.use_exn "tcgetattr" tty Unix.tcgetattr))
  in
  (Option.get !status, written, (Option.get !echo).Unix.c_echo)

let teardown how =
  let status, out, _ = session how in
  (status, out)

let hide = "\027[?25l"

let last_cursor s =
  let rec find i acc =
    match Astring.String.find_sub ~start:i ~sub:"\027[?25" s with
    | None -> acc
    | Some j when j + 5 < String.length s -> find (j + 5) (Some s.[j + 5])
    | Some _ -> acc
  in
  find 0 None

let test_teardown () =
  List.iter
    (fun (how, expected) ->
      let status, out = teardown how in
      Alcotest.(check bool) (how ^ ": hid the cursor") true (contains hide out);
      Alcotest.(check (option char))
        (how ^ ": the cursor is shown last")
        (Some 'h') (last_cursor out);
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (how ^ ": standard output above the region, unlabelled")
        true
        (List.mem "a line of output" (Console_vte.screen ~scrollback:true vte));
      Alcotest.(check int)
        (how ^ ": the cursor is at the start of a line")
        0
        (snd (Console_vte.cursor_pos vte));
      Alcotest.(check bool)
        (how ^ ": exit status") true
        (match (status, expected) with
        | `Exited a, `Exited b -> Int.equal a b
        | _ -> false))
    [
      ("ok", `Exited 0);
      ("exit", `Exited 3);
      ("raise", `Exited 2);
      ("cancel", `Exited 0);
    ]

(* The final screen of what a session wrote. *)
let final_of out =
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Console_vte.screen ~scrollback:true vte

(* The final screen of [teardown.exe how]. *)
let final how = final_of (snd (teardown how))

(* Ctrl-C cancels the task, which prints its output under its line; a task
   that succeeds drops it. *)
let test_sigint_output () =
  let rows = final "sigint" in
  let rec under = function
    | l :: next :: _ when contains "\u{25cb} pushing" l -> next
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no cancelled line"
  in
  Alcotest.(check string)
    "the output stays" "  \u{203a} pushed 3 objects" (under rows);
  Alcotest.(check bool)
    "a success drops it" false
    (List.exists (contains "pushed 3 objects") (final "ok"))

(* What a task's function prints on the display's terminal, on either
   standard output, is its output, as are the Logs messages of the fibers it
   forks: it goes when the task succeeds and is printed under its line when
   it fails. A line printed within the root task goes above the rows. *)
let test_task_output () =
  let output =
    [
      "  \u{203a} an output line";
      "  \u{203a} a result line";
      "  \u{203a} a note line";
      "  ! a forked note";
    ]
  in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l ->
        List.filteri (fun i _ -> i < List.length output) rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let failed = final "logs-failed" in
  Alcotest.(check (list string)) "under the failure" output (under failed);
  let rows = final "logs" in
  List.iter
    (fun rows ->
      Alcotest.(check bool)
        "within the root, above the rows" true
        (List.mem "a root line" rows))
    [ failed; rows ];
  Alcotest.(check bool)
    "the success line" true
    (List.exists (contains "\u{2713} work") rows);
  List.iter
    (fun text ->
      Alcotest.(check bool)
        ("folded away: " ^ text) false
        (List.exists (contains text) rows))
    [ "an output line"; "a result line"; "a note line"; "a forked note" ]

(* A line written outside a task is not the output of the task in which
   the next line is written, even when it waited for a flush until then. *)
let test_held_line () =
  let rows = final "held" in
  let rec under = function
    | l :: rest when contains "\u{2717} work" l -> rest
    | _ :: rest -> under rest
    | [] -> Alcotest.fail "no failed line"
  in
  let output = List.filter (contains "\u{203a}") (under rows) in
  Alcotest.(check (list string))
    "the task's line alone under it"
    [ "  \u{203a} a task line" ]
    output;
  Alcotest.(check bool)
    "the held line above the rows" true
    (List.exists (String.starts_with ~prefix:"a held line") rows)

let test_sigint_teardown () =
  let _, out = teardown "sigint" in
  Alcotest.(check bool)
    "SIGINT interrupted a live display" true (contains hide out);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "SIGINT restores the cursor" true
    (Console_vte.cursor_visible vte)

let signaled_by_sigint = function
  | `Signaled s -> Int.equal s Sys.sigint
  | _ -> false

(* SIGINT in the middle of a frame's write ends the program once that
   frame is written: the screen is the one SIGINT leaves between frames. *)
let test_sigint_mid_write () =
  let status, out = teardown "sigint-mid-write" in
  Alcotest.(check bool)
    "SIGINT ends the program" true
    (signaled_by_sigint status);
  let vte = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed vte out;
  Alcotest.(check bool)
    "the cursor is shown" true
    (Console_vte.cursor_visible vte);
  Alcotest.(check (list string))
    "a clean final frame" (final "sigint") (final_of out)

(* A write that never ends does not hold SIGINT back for ever. *)
let test_sigint_blocked () =
  let status, _ = teardown "sigint-blocked" in
  Alcotest.(check bool)
    "SIGINT ends the program" true
    (signaled_by_sigint status)

(* However a session ends, text written after it is drawn as on a fresh
   terminal: at the first column, in the terminal's own colours. *)
let test_usable_after () =
  List.iter
    (fun (how, no_color) ->
      let _, out, _ = session ~no_color how in
      let name = Fmt.str "%s%s" how (if no_color then " (NO_COLOR)" else "") in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte out;
      Alcotest.(check bool)
        (name ^ ": cursor shown") true
        (Console_vte.cursor_visible vte);
      let y, x = Console_vte.cursor_pos vte in
      Alcotest.(check int) (name ^ ": first column") 0 x;
      Console_vte.feed vte "AFTER";
      let plain = Console_vte.v ~rows:1 ~cols:80 () in
      Console_vte.feed plain "AFTER";
      let g = Console_vte.grid vte and reference = Console_vte.grid plain in
      let i = Grid.idx g ~x:0 ~y and j = Grid.idx reference ~x:0 ~y:0 in
      let colours g i =
        List.concat_map
          (fun c ->
            let r, g, b, a = Ansi.Color.to_rgba c in
            [ r; g; b; a ])
          [ Console_vte.fg g i; Console_vte.bg g i ]
      in
      Alcotest.(check (list int))
        (name ^ ": default colours")
        (colours reference j) (colours g i))
    (List.concat_map
       (fun how -> [ (how, false); (how, true) ])
       [ "ok"; "raise"; "cancel"; "exit"; "sigint" ])

(* A terminal whose locale is not UTF-8 shows other characters as garbage:
   every widget and the display are then drawn with ASCII alone. *)
let test_ascii_locale () =
  let _, out, _ = session ~lang:"C" "widgets" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool)
    "ASCII only" true
    (String.for_all (fun c -> Char.code c < 0x80) out)

(* A program that chose its colours before [run] keeps them. *)
let test_choice_kept () =
  let _, out, _ = session "colourless" in
  Alcotest.(check bool) "drew a region" true (contains hide out);
  Alcotest.(check bool) "no colour" false (contains "\027[0m" out)

(* A theme is not a colour choice: a program that only chose a theme still
   has colour decided for it, and in that theme. *)
let test_theme_not_a_choice () =
  let _, out, _ = session "themed" in
  let nord =
    let buf = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Console.Color.set_depth ppf `Ansi_256;
    Console.(
      View.pp ppf
        (View.styled
           Style.(bold + fg (Theme.succeeded Theme.nord))
           (View.text "x")));
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    String.sub s 0 (String.index s 'x')
  in
  Alcotest.(check bool) "colour, in the theme" true (contains nord out)

(* Ctrl-Z gives the terminal back before the program stops: the region is
   erased and the cursor shown. Once continued, the region is drawn again and
   the work goes on. *)
let test_suspend () =
  let status, out, _ = session ~continue:true "suspend" in
  let show = "\027[?25h" in
  let hidden = Option.get (Astring.String.find_sub ~sub:hide out) in
  match Astring.String.find_sub ~start:hidden ~sub:show out with
  | None -> Alcotest.fail "the cursor is not shown before the stop"
  | Some stop ->
      let before = String.sub out 0 (stop + String.length show) in
      let vte = Console_vte.v ~rows:24 ~cols:80 () in
      Console_vte.feed vte before;
      Alcotest.(check bool)
        "the cursor is shown" true
        (Console_vte.cursor_visible vte);
      Alcotest.(check bool)
        "the region is erased" false
        (List.exists (contains "pausing") (Console_vte.screen vte));
      Alcotest.(check bool)
        "the region is drawn again, the cursor hidden" true
        (Option.is_some (Astring.String.find_sub ~start:stop ~sub:hide out));
      Alcotest.(check bool)
        "the work goes on" true
        (List.exists (contains "\u{2713} pausing") (final_of out));
      Alcotest.(check bool)
        "exit status" true
        (match status with `Exited 0 -> true | _ -> false)

(* In a process group that no shell controls, a stop does not take effect:
   the terminal is given back and the region drawn again once, and the work
   goes on. A display that gave the terminal back again and again would
   show the cursor without end; the reader stops it then. *)
let test_suspend_without_stop () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let child =
    Eio_unix.Process.spawn_unix ~sw
      (Eio.Stdenv.process_mgr env)
      ~env:[| "TERM=xterm-256color"; "LANG=en_US.UTF-8" |]
      ~fds:
        (List.map (fun fd -> (fd, Eio_unix.Pty.tty pty, `Blocking)) [ 0; 1; 2 ])
      ~executable:"./teardown.exe"
      [ "teardown.exe"; "orphaned-suspend" ]
  in
  let out = Buffer.create 4096 and buf = Cstruct.create 4096 in
  let shown = ref 0 and spun = ref false in
  let rec read () =
    let n = Eio.Flow.single_read (Eio_unix.Pty.source pty) buf in
    let s = Cstruct.to_string ~len:n buf in
    Buffer.add_string out s;
    shown := !shown + List.length (Astring.String.cuts ~sep:"\027[?25h" s) - 1;
    if !shown > 50 && not !spun then (
      spun := true;
      Unix.kill (Eio.Process.pid child) Sys.sigkill);
    read ()
  in
  let status =
    Eio.Fiber.first
      (fun () -> Eio.Process.await child)
      (fun () ->
        try read () with End_of_file | Eio.Io _ -> Eio.Fiber.await_cancel ())
  in
  Alcotest.(check bool)
    (Fmt.str "the region is given back a bounded number of times (%d)" !shown)
    false !spun;
  Alcotest.(check bool)
    "the work goes on" true
    (match status with `Exited 0 -> true | _ -> false)

(* Output outside a task that the program never flushes is not held
   without bound: all but the last 64 KiB of it is on the terminal before
   the program goes on. *)
let test_unflushed_output_is_bounded () =
  let _, out, _ = session "unflushed" in
  match Astring.String.find_sub ~sub:"MARK" out with
  | None -> Alcotest.fail "no mark"
  | Some k ->
      let after = String.length out - k in
      let held =
        List.length (Astring.String.cuts ~sep:"line " (String.sub out k after))
        - 1
      in
      Alcotest.(check bool)
        (Fmt.str "at most 64 KiB held back (%d lines)" held)
        true
        (held * String.length "line 000000\n" <= 65536)

let test_sigint_secret () =
  let status, _, echo = session "secret" in
  Alcotest.(check bool)
    "SIGINT still ends the program" true
    (match status with `Signaled s -> Int.equal s Sys.sigint | _ -> false);
  Alcotest.(check bool) "SIGINT during a password restores echo" true echo

let test_quiet_terminal_capture () =
  let status, out = teardown "quiet-ok" in
  Alcotest.(check bool)
    "the program succeeds" true
    (match status with `Exited 0 -> true | _ -> false);
  Alcotest.(check (list string))
    "successful task output is hidden; root output stays" [ "root line" ]
    (final_of out)

let test_cast_without_deterministic () =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_env [ ("CONSOLE_CAST", file); ("CONSOLE_DETERMINISTIC_CLOCK", "") ]
  @@ fun () ->
  Eio_main.run @@ fun _ ->
  let mock = Eio_mock.Clock.Mono.make () in
  let clock = Console_eio.clock mock in
  let _, ppf = buffer () in
  let slept = ref false in
  Eio.Fiber.first
    (fun () ->
      Console_eio.run ~clock ~ppf (fun _ ->
          Eio.Time.Mono.sleep clock 1.;
          slept := true))
    (fun () ->
      for _ = 1 to 1000 do
        Eio.Fiber.yield ()
      done);
  Alcotest.(check bool)
    "recording sleeps without advancing the real clock" true !slept

(* The lines of a recording of [f]'s run. *)
let recorded f =
  let file = Filename.temp_file "console" ".cast" in
  Fun.protect ~finally:(fun () -> Sys.remove file) @@ fun () ->
  with_env [ ("CONSOLE_CAST", file); ("CONSOLE_DETERMINISTIC_CLOCK", "1") ]
  @@ fun () ->
  Eio_main.run (fun _ ->
      let clock = Console_eio.clock (Eio_mock.Clock.Mono.make ()) in
      let _, ppf = buffer () in
      Console_eio.run ~clock ~ppf (fun _ -> Console_eio.with_output f));
  In_channel.with_open_bin file In_channel.input_lines

let test_cast_utf_8 () =
  let lines = recorded (fun ppf -> Fmt.pf ppf "bad \xff byte\n%!") in
  List.iter
    (fun l ->
      Alcotest.(check bool)
        ("valid UTF-8: " ^ String.escaped l)
        true (String.is_valid_utf_8 l))
    lines

let test_repeated_signal_mid_write () =
  let input =
    Unix.open_process_args_in "./teardown.exe"
      [| "teardown.exe"; "repeated-signal"; "int"; "int"; "0"; "false" |]
  in
  let raw = In_channel.input_all input in
  let status = Unix.close_process_in input in
  Alcotest.(check bool)
    "the signal still ends the process" true
    (status = Unix.WSIGNALED Sys.sigint);
  let terminal = Console_vte.v ~rows:24 ~cols:80 () in
  Console_vte.feed terminal raw;
  Alcotest.(check bool)
    "a second SIGINT waits until the terminal is restored" true
    (Console_vte.cursor_visible terminal);
  Alcotest.(check bool)
    "the final synchronized update is complete" true
    (String.ends_with ~suffix:"\027[?2026l" raw)

let suite =
  ( "console_eio",
    [
      Alcotest.test_case "two SIGINTs during a write restore the terminal"
        `Quick test_repeated_signal_mid_write;
      Alcotest.test_case "recording alone makes sleeps virtual" `Quick
        test_cast_without_deterministic;
      Alcotest.test_case "a recording is valid UTF-8" `Quick test_cast_utf_8;
      Alcotest.test_case "quiet hides successful terminal output" `Quick
        test_quiet_terminal_capture;
      Alcotest.test_case "the terminal is usable after every ending" `Quick
        test_usable_after;
      Alcotest.test_case "the program's colour choice is kept" `Quick
        test_choice_kept;
      Alcotest.test_case "a theme is not a colour choice" `Quick
        test_theme_not_a_choice;
      Alcotest.test_case "ASCII on a terminal that is not UTF-8" `Quick
        test_ascii_locale;
      Alcotest.test_case "SIGINT during a password restores echo" `Quick
        test_sigint_secret;
      Alcotest.test_case "unflushed output is held back boundedly" `Quick
        test_unflushed_output_is_bounded;
      Alcotest.test_case "SIGINT restores the terminal cursor" `Quick
        test_sigint_teardown;
      Alcotest.test_case "SIGINT in the middle of a write" `Quick
        test_sigint_mid_write;
      Alcotest.test_case "SIGINT ends a write that never ends" `Quick
        test_sigint_blocked;
      Alcotest.test_case "a stop that does not take effect resumes once" `Quick
        test_suspend_without_stop;
      Alcotest.test_case "Ctrl-Z gives the terminal back, fg redraws" `Quick
        test_suspend;
      Alcotest.test_case "SIGINT prints a cancelled task's output" `Quick
        test_sigint_output;
      Alcotest.test_case "a task's output on a terminal" `Quick test_task_output;
      Alcotest.test_case "a held line stays outside the next task" `Quick
        test_held_line;
      Alcotest.test_case "run returns" `Quick test_run_returns;
      Alcotest.test_case "run on a monotonic clock" `Quick
        test_run_on_a_monotonic_clock;
      Alcotest.test_case "deterministic clock" `Quick test_deterministic_clock;
      Alcotest.test_case "a deterministic clock ignores the schedule" `Quick
        test_deterministic_schedule;
      Alcotest.test_case "a recording's header and events" `Quick
        test_cast_format;
      Alcotest.test_case "a recording's times are virtual" `Quick
        test_cast_times;
      Alcotest.test_case "a recording ignores the schedule" `Quick
        test_cast_schedule;
      Alcotest.test_case "a recording's line feeds" `Quick test_cast_line_feeds;
      Alcotest.test_case "run raises" `Quick test_run_raises;
      Alcotest.test_case "a view that raises" `Quick test_run_view_raises;
      Alcotest.test_case "a commit" `Quick test_run_commit;
      Alcotest.test_case "print" `Quick test_run_print;
      Alcotest.test_case "print outside Eio" `Quick test_print_outside_eio;
      Alcotest.test_case "nested run reuses the display" `Quick
        test_nested_run_reuses_the_display;
      Alcotest.test_case "a display in another domain is refused" `Quick
        test_second_display_refused;
      Alcotest.test_case "dimensions read the environment" `Quick
        test_dimensions_read_the_environment;
      Alcotest.test_case "teardown on a terminal" `Quick test_teardown;
    ] )
