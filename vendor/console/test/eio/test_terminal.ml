(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Display_harness

(* [`Auto] draws a live region where one can be drawn, on the descriptor the
   display writes to: [Console_eio.run ~ppf:Fmt.stderr] asks standard error,
   whatever standard output is. A terminal whose [TERM] is [dumb] has no cursor
   to move, so it gets the appending display, the one a pipe gets. Each case
   puts a pseudo-terminal or a pipe on the two descriptors for its span and
   reads what the display wrote: a live region hides the cursor first
   (ESC [?25l), and an appending display writes no escape at all. *)

let hide_cursor = "\027[?25l"

let open_pty ~sw =
  match Eio_unix.Pty.open_pty ~sw () with
  | pty -> pty
  | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
      Alcotest.skip ()

let tty pty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup

(* [f ()] with standard output on [stdout], standard error on [stderr],
   [TERM] set to [term] and the colour of both decided for them, in UTF-8,
   each given back as it was found. *)
let with_fds ~stdout ~stderr ~term f =
  let saved_out = Unix.dup Unix.stdout and saved_err = Unix.dup Unix.stderr in
  let saved_term = Sys.getenv_opt "TERM" in
  let stds = [ Fmt.stdout; Fmt.stderr ] in
  let looks = List.map (fun p -> (Fmt.style_renderer p, Fmt.utf_8 p)) stds in
  Fun.protect ~finally:(fun () ->
      flush Stdlib.stdout;
      flush Stdlib.stderr;
      Unix.dup2 saved_out Unix.stdout;
      Unix.dup2 saved_err Unix.stderr;
      Unix.close saved_out;
      Unix.close saved_err;
      Unix.putenv "TERM" (Option.value saved_term ~default:"");
      List.iter2
        (fun p (r, u) ->
          Fmt.set_style_renderer p r;
          Fmt.set_utf_8 p u)
        stds looks)
  @@ fun () ->
  Unix.dup2 stdout Unix.stdout;
  Unix.dup2 stderr Unix.stderr;
  Unix.putenv "TERM" term;
  Console_tty.setup ~utf_8:true ();
  f ()

(* One task, drawn on standard error by [Console_eio.run] in [`Auto], on a
   clock that never moves: the case asserts which display was chosen and
   nothing about the machine's pace. *)
let draw_a_task () =
  let clock = Eio_mock.Clock.Mono.make () in
  Console_eio.run ~clock ~ppf:Fmt.stderr (fun display ->
      let task = Console.Task.start display "step" in
      Console.Task.succeed task)

(* A mark no display writes, so a reader of the terminal knows it has read
   everything written before it. *)
let end_mark = "<end of the case's output>"

let mark fd =
  ignore (Unix.write_substring fd end_mark 0 (String.length end_mark) : int)

let read_to_mark pty =
  let written = Buffer.create 256 and buf = Cstruct.create 4096 in
  let source = Eio_unix.Pty.source pty in
  let rec loop () =
    if not (String.ends_with ~suffix:end_mark (Buffer.contents written)) then begin
      let n = Eio.Flow.single_read source buf in
      Buffer.add_string written (Cstruct.to_string ~len:n buf);
      loop ()
    end
  in
  loop ();
  Buffer.contents written

let with_null f =
  let null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close null) (fun () -> f null)

(* Standard error is a terminal and standard output is not: the region is
   drawn, since the display draws on standard error. An implementation that
   asks standard output draws history here. *)
let test_auto_asks_the_formatters_own_descriptor () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  with_null @@ fun null ->
  let err_tty, out_tty =
    with_fds ~stdout:null ~stderr:tty ~term:"xterm-256color" (fun () ->
        let asked = (Unix.isatty Unix.stderr, Unix.isatty Unix.stdout) in
        draw_a_task ();
        mark Unix.stderr;
        asked)
  in
  Alcotest.(check bool) "standard error is a terminal" true err_tty;
  Alcotest.(check bool) "standard output is not" false out_tty;
  let written = read_to_mark pty in
  Alcotest.(check bool)
    "a live region on standard error's terminal" true
    (contains hide_cursor written)

(* Standard error is a pipe and standard output a terminal: the display
   appends, and the pipe holds the task's lines and no escape. An
   implementation that asks standard output draws a region into the pipe. *)
let test_auto_appends_on_a_pipe () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  let r, w = Unix.pipe ~cloexec:true () in
  Fun.protect ~finally:(fun () -> Unix.close r) @@ fun () ->
  ( Fun.protect ~finally:(fun () -> Unix.close w) @@ fun () ->
    with_fds ~stdout:tty ~stderr:w ~term:"xterm-256color" draw_a_task );
  let written = Buffer.create 256 and buf = Bytes.create 4096 in
  let rec drain () =
    match Unix.read r buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes written buf 0 n;
        drain ()
  in
  drain ();
  let written = Buffer.contents written in
  Alcotest.(check bool) "no escape" false (contains "\027[" written);
  Alcotest.(check string)
    "the task's lines" "\xe2\x80\xa2 step\n\xe2\x9c\x93 step  0.0s\n" written

(* Both descriptors are the same terminal; under TERM=xterm-256color the
   region is drawn, under TERM=dumb the display appends. *)
let test_auto_appends_under_term_dumb () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  let draw term =
    with_fds ~stdout:tty ~stderr:tty ~term (fun () ->
        draw_a_task ();
        mark Unix.stderr);
    read_to_mark pty
  in
  Alcotest.(check bool)
    "a live region under xterm-256color" true
    (contains hide_cursor (draw "xterm-256color"));
  let dumb = draw "dumb" in
  Alcotest.(check bool) "no escape under dumb" false (contains "\027[" dumb);
  Alcotest.(check bool)
    "the task's line under dumb" true
    (contains "\xe2\x9c\x93 step  0.0s" dumb)

(* Standard output is a terminal, but not the display's: what the program
   prints there goes out as it is written, a line not yet ended included,
   since no region of the display is on it. *)
let test_other_terminal_passes_through () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let out = open_pty ~sw and err = open_pty ~sw in
  let out_tty = tty out and err_tty = tty err in
  Fun.protect ~finally:(fun () ->
      Unix.close out_tty;
      Unix.close err_tty)
  @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let written =
    with_fds ~stdout:out_tty ~stderr:err_tty ~term:"xterm-256color" (fun () ->
        Console_eio.run ~clock ~ppf:Fmt.stderr (fun _ ->
            Fmt.pr "partial%!";
            mark Unix.stdout;
            read_to_mark out))
  in
  Alcotest.(check string)
    "byte for byte, at once" ("partial" ^ end_mark) written

(* Whether [s] holds an SGR sequence: a CSI whose final byte is ['m']. The
   cursor controls a region is made of are CSIs too, with other final bytes. *)
let has_sgr s =
  let n = String.length s in
  let rec final j =
    if j >= n then j
    else
      let c = Char.code s.[j] in
      if c >= 0x40 && c <= 0x7e then j else final (j + 1)
  in
  let rec go i =
    if i + 1 >= n then false
    else if s.[i] = '\027' && s.[i + 1] = '[' then
      let stop = final (i + 2) in
      (stop < n && s.[stop] = 'm') || go (stop + 1)
    else go (i + 1)
  in
  go 0

let with_no_color value f =
  let saved = Sys.getenv_opt "NO_COLOR" in
  Fun.protect ~finally:(fun () ->
      Unix.putenv "NO_COLOR" (Option.value saved ~default:""))
  @@ fun () ->
  Unix.putenv "NO_COLOR" value;
  f ()

(* [run] on a terminal draws in colour as [setup] decides, and draws plain when
   [NO_COLOR] asks it to; the region is drawn in place either way. *)
let test_run_styles_a_terminal () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let pty = open_pty ~sw in
  let tty = tty pty in
  Fun.protect ~finally:(fun () -> Unix.close tty) @@ fun () ->
  with_null @@ fun null ->
  let draw no_color =
    let ppf = Fmt.stderr in
    with_no_color no_color (fun () ->
        with_fds ~stdout:null ~stderr:tty ~term:"xterm-256color" (fun () ->
            Console_tty.setup ();
            let clock = Eio_mock.Clock.Mono.make () in
            Console_eio.run ~clock ~ppf (fun display ->
                Console.Task.succeed (Console.Task.start display "step"));
            mark Unix.stderr));
    read_to_mark pty
  in
  let coloured = draw "" in
  Alcotest.(check bool) "a live region" true (contains hide_cursor coloured);
  Alcotest.(check bool) "in colour" true (has_sgr coloured);
  let plain = draw "1" in
  Alcotest.(check bool)
    "a live region under NO_COLOR" true
    (contains hide_cursor plain);
  Alcotest.(check bool) "no colour under NO_COLOR" false (has_sgr plain)

let test_small_dimensions () =
  let columns = Sys.getenv_opt "COLUMNS" and lines = Sys.getenv_opt "LINES" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "COLUMNS" (Option.value columns ~default:"");
      Unix.putenv "LINES" (Option.value lines ~default:""))
    (fun () ->
      Unix.putenv "COLUMNS" "10";
      Unix.putenv "LINES" "2";
      Alcotest.(check (pair int int))
        "valid small geometry" (10, 2) (Terminal.dimensions None))

(* No terminal is wider or taller than 4096 cells: a larger $COLUMNS or
   $LINES is ignored, as one that is not a number is. *)
let test_absurd_dimensions () =
  let columns = Sys.getenv_opt "COLUMNS" and lines = Sys.getenv_opt "LINES" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "COLUMNS" (Option.value columns ~default:"");
      Unix.putenv "LINES" (Option.value lines ~default:""))
    (fun () ->
      Unix.putenv "COLUMNS" "4096";
      Unix.putenv "LINES" "4096";
      Alcotest.(check (pair int int))
        "the largest" (4096, 4096) (Terminal.dimensions None);
      Unix.putenv "COLUMNS" "99999999999";
      Unix.putenv "LINES" "4097";
      Alcotest.(check (pair int int))
        "too large" (80, 24) (Terminal.dimensions None))

let suite =
  ( "terminal",
    [
      Alcotest.test_case "small positive terminal dimensions are preserved"
        `Quick test_small_dimensions;
      Alcotest.test_case "absurd terminal dimensions are ignored" `Quick
        test_absurd_dimensions;
      Alcotest.test_case "auto asks the formatter's own descriptor" `Quick
        test_auto_asks_the_formatters_own_descriptor;
      Alcotest.test_case "another terminal's output passes through" `Quick
        test_other_terminal_passes_through;
      Alcotest.test_case "auto appends on a pipe" `Quick
        test_auto_appends_on_a_pipe;
      Alcotest.test_case "auto appends under TERM=dumb" `Quick
        test_auto_appends_under_term_dumb;
      Alcotest.test_case "run styles a terminal" `Quick
        test_run_styles_a_terminal;
    ] )
