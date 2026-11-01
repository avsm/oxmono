(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Console_eio.Prompt

let run f = Eio_main.run (fun _env -> f ())

(* A prompt over a string source and a buffer: the same code path a terminal
   takes, with the answers scripted and the questions captured. *)
let prompt ?interactive input =
  let out = Buffer.create 64 in
  let t =
    Prompt.v ?interactive
      ~ppf:(Format.formatter_of_buffer out)
      (Eio.Flow.string_source input)
  in
  (t, out)

(* The wording of a refusal is the implementation's to choose, so an answer is
   compared after collapsing every error but the end of the input to
   [Refused]. The one message whose content is itself a specification -- the
   gate must not echo the word it is asking for -- gets its own assertion. *)
type 'a answer = Got of 'a | Ended | Refused

let seen = function
  | Ok v -> Got v
  | Error `End_of_input -> Ended
  | Error (`Msg _) -> Refused

let answer t =
  let equal a b =
    match (a, b) with
    | Got x, Got y -> Alcotest.equal t x y
    | Ended, Ended | Refused, Refused -> true
    | (Got _ | Ended | Refused), _ -> false
  in
  let pp ppf = function
    | Got v -> Fmt.pf ppf "Got %a" (Alcotest.pp t) v
    | Ended -> Fmt.string ppf "Ended"
    | Refused -> Fmt.string ppf "Refused"
  in
  Alcotest.testable pp equal

let string_answer = answer Alcotest.string
let bool_answer = answer Alcotest.bool

(* A string source is not a terminal, so the default is a prompt that refuses
   every question without writing a byte. *)
let test_a_string_source_is_not_a_terminal () =
  run @@ fun () ->
  let t, out = prompt "y\n" in
  Alcotest.(check bool) "not interactive" false (Prompt.interactive t);
  Alcotest.check string_answer "line" Refused (seen (Prompt.line t "name: "));
  Alcotest.check bool_answer "confirm" Refused
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "asked nothing" "" (Buffer.contents out)

(* One [t] owns the read buffer, so the second question gets the second line
   rather than whatever survived the first read. *)
let test_line_asks_verbatim_and_keeps_its_place () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "alpha\nbeta\n" in
  Alcotest.check string_answer "first answer" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.(check string) "first question" "name: " (Buffer.contents out);
  Alcotest.check string_answer "second answer" (Got "beta")
    (seen (Prompt.line t "again: "));
  Alcotest.(check string) "both questions" "name: again: " (Buffer.contents out)

(* The answer is what was typed, spaces and all, minus the terminator. *)
let test_line_keeps_the_spaces_it_was_given () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "  spaced out  \n" in
  Alcotest.check string_answer "answer" (Got "  spaced out  ")
    (seen (Prompt.line t "name: "))

let test_line_drops_a_crlf_terminator () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "alpha\r\nbeta\r\n" in
  Alcotest.check string_answer "first" (Got "alpha")
    (seen (Prompt.line t "name: "));
  Alcotest.check string_answer "second" (Got "beta")
    (seen (Prompt.line t "again: "))

(* The question goes out before the read, so it is on the terminal even when
   the answer never comes. *)
let test_line_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "" in
  Alcotest.check string_answer "answer" Ended (seen (Prompt.line t "name: "));
  Alcotest.(check string) "question" "name: " (Buffer.contents out);
  let t, _ = prompt ~interactive:true "cut" in
  Alcotest.check string_answer "a line cut short" Ended
    (seen (Prompt.line t "name: "))

let test_confirm_asks_with_a_default_of_no () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "y\n" in
  Alcotest.check bool_answer "answer" (Got true)
    (seen (Prompt.confirm t "delete?"));
  Alcotest.(check string) "question" "delete? [y/N] " (Buffer.contents out)

let check_confirm input expected =
  let t, _ = prompt ~interactive:true (input ^ "\n") in
  Alcotest.check bool_answer
    (Fmt.str "confirm %S" input)
    (Got expected)
    (seen (Prompt.confirm t "delete?"))

let test_confirm_takes_y_and_yes_in_any_case () =
  run @@ fun () ->
  List.iter
    (fun input -> check_confirm input true)
    [ "y"; "Y"; "yes"; "  YES  " ]

let test_confirm_takes_everything_else_as_no () =
  run @@ fun () ->
  List.iter (fun input -> check_confirm input false) [ "n"; ""; "maybe" ]

let test_confirm_at_end_of_input_is_refused () =
  run @@ fun () ->
  let t, _ = prompt ~interactive:true "" in
  Alcotest.check bool_answer "answer" Ended (seen (Prompt.confirm t "delete?"))

(* A question is text: an escape in it cannot clear the screen. *)
let test_question_is_text () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "y\n" in
  ignore (Prompt.confirm t "\027[2Jdelete?");
  Alcotest.(check bool)
    "no escape" false
    (String.contains (Buffer.contents out) '\027')

let test_secret_without_a_terminal_is_refused () =
  run @@ fun () ->
  let t, out = prompt ~interactive:true "abc\n" in
  Alcotest.check string_answer "answer" Refused
    (seen (Prompt.secret t "secret: "));
  Alcotest.(check string) "nothing asked" "" (Buffer.contents out)

(* [f ()] with standard input on [stdin], given back as it was found. *)
let with_stdin ~stdin f =
  let saved = Unix.dup Unix.stdin in
  Fun.protect ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      Unix.close saved)
  @@ fun () ->
  Unix.dup2 stdin Unix.stdin;
  f ()

(* A pseudo-terminal for [f tty master], each a descriptor of its own. *)
let with_pty f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pty =
    match Eio_unix.Pty.open_pty ~sw () with
    | pty -> pty
    | exception Unix.Unix_error (Unix.EPERM, "posix_openpt", _) ->
        Alcotest.skip ()
  in
  let tty = Eio_unix.Fd.use_exn "tty" (Eio_unix.Pty.tty pty) Unix.dup in
  let master = Eio_unix.Fd.use_exn "pty" (Eio_unix.Pty.pty pty) Unix.dup in
  Fun.protect ~finally:(fun () -> List.iter Unix.close [ tty; master ])
  @@ fun () -> f env tty master

let masked tty = not (Unix.tcgetattr tty).Unix.c_echo

(* Whether the terminal has output for its reader, without waiting. *)
let ready fd =
  match Unix.select [ fd ] [] [] 0. with [], _, _ -> false | _ -> true

(* What the terminal shows, read until it shows nothing more for now. *)
let rec drain master =
  if ready master then begin
    ignore (Unix.read master (Bytes.create 4096) 0 4096 : int);
    drain master
  end

let type_in master s =
  ignore (Unix.write_substring master s 0 (String.length s) : int)

(* Waits until the terminal shows [s]. On Linux what is typed reaches the
   line discipline later, and a flush meanwhile misses it: once echoed, it
   is there. *)
let await_echo master s =
  let shown = Buffer.create 64 in
  let buf = Bytes.create 4096 in
  while not (Display_harness.contains s (Buffer.contents shown)) do
    ignore (Unix.select [ master ] [] [] (-1.));
    let n = Unix.read master buf 0 (Bytes.length buf) in
    Buffer.add_subbytes shown buf 0 n
  done

(* A prompt on [tty] whose questions and masks go to a formatter that
   counts the masks, [*], in [echoed]. *)
let prompt_on env ~echoed =
  let ppf =
    Format.make_formatter
      (fun s off len ->
        for i = off to off + len - 1 do
          if s.[i] = '*' then Atomic.incr echoed
        done)
      ignore
  in
  Prompt.v ~ppf (Eio.Stdenv.stdin env)

(* [secret t] answered on a pseudo-terminal with [typed] by another fiber
   of the domain, which runs only if the read lets it: a read that parks
   the domain hangs, and the run's guard ends it. The fiber types once the
   question is masked, since what is typed before is discarded. *)
let secret_typed typed =
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      let answer, () =
        Eio.Fiber.pair
          (fun () -> Prompt.secret t "secret: ")
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            type_in master typed)
      in
      seen answer)

(* [secret] waits for the terminal the way a fiber waits, so the rest of the
   domain runs meanwhile: the answer is typed by another fiber of the same
   domain, which only runs if the read let it. *)
let test_secret_leaves_the_domain_running () =
  Alcotest.check string_answer "the typed line" (Got "abc")
    (secret_typed "abc\n")

(* A password is the bytes typed: a bidi mark, a C1 control and a byte
   that is no UTF-8 come back as they were typed. *)
let test_secret_keeps_bytes () =
  let password = "\u{202e}p\xc2\x85w\xff" in
  Alcotest.check string_answer "the bytes typed" (Got password)
    (secret_typed (password ^ "\n"))

(* A domain types the line in pieces once the question is masked, each
   piece once the prompt has masked all typed before it, so that the
   terminal's input queue never fills; past the cap the prompt masks no
   more, and the rest, less than a queue, is typed at once. *)
let test_secret_cap () =
  let line = String.make 5000 'a' ^ "\n" in
  let cap = 4097 in
  let result =
    with_pty @@ fun env tty master ->
    let echoed = Atomic.make 0 in
    let typist =
      Domain.spawn (fun () ->
          while not (masked tty) do
            Domain.cpu_relax ()
          done;
          let rec go i =
            if i < cap then begin
              let n = min 64 (cap - i) in
              ignore (Unix.write_substring master line i n : int);
              while Atomic.get echoed < i + n do
                Domain.cpu_relax ()
              done;
              go (i + n)
            end
            else
              ignore
                (Unix.write_substring master line i (String.length line - i)
                  : int)
          in
          go 0)
    in
    let result =
      with_stdin ~stdin:tty (fun () ->
          Prompt.secret (prompt_on env ~echoed) "secret: ")
    in
    Domain.join typist;
    seen result
  in
  match result with
  | Got _ -> Alcotest.fail "a line of 5000 bytes taken"
  | _ -> ()

(* What is typed before [secret] masks the terminal is echoed, as it is
   under getpass(3), but it is not the answer: [secret] discards it and
   reads what is typed once the question is masked. [typed] is typed first,
   and a {!Prompt.line} answered [before] is asked first unless [before] is
   empty. A domain plays the terminal: it reads what the terminal shows, as
   masking waits for that output to be read, and once the echo is off, the
   input discarded, it types the answer. *)
let secret_after ~before typed =
  let result =
    with_pty @@ fun env tty master ->
    type_in master typed;
    await_echo master "early";
    let typist =
      Domain.spawn (fun () ->
          while not (masked tty) do
            drain master;
            Domain.cpu_relax ()
          done;
          type_in master "late\n")
    in
    let result =
      with_stdin ~stdin:tty (fun () ->
          let t = prompt_on env ~echoed:(Atomic.make 0) in
          if before <> "" then
            Alcotest.check string_answer "the line" (Got before)
              (seen (Prompt.line t "line: "));
          Prompt.secret t "secret: ")
    in
    Domain.join typist;
    result
  in
  Alcotest.check string_answer "the line typed once masked" (Got "late")
    (seen result)

let test_secret_drops_typeahead () = secret_after ~before:"" "early\n"

(* A line typed ahead of a question that {!Prompt.line} answers is not
   kept for the [secret] after it: the terminal hands [line] its own line
   alone, and [secret] discards the rest, which the terminal echoed. *)
let test_secret_after_a_line () = secret_after ~before:"name" "name\nearly\n"

(* While one fiber waits for an answer, the output of another fiber is
   still its task's: only the prompting fiber writes straight out. Run takes
   the program's output from Fmt.stdout, which writes into a buffer here. *)
let test_prompt_leaves_other_fibers_attributed () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio_mock.Clock.Mono.make () in
  let source, sink = Eio_unix.pipe sw in
  let questions = Buffer.create 64 in
  let t =
    Prompt.v ~interactive:true
      ~ppf:(Format.formatter_of_buffer questions)
      source
  in
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    { saved with out_string = Buffer.add_substring out; out_flush = ignore };
  let answer =
    Fun.protect
      ~finally:(fun () ->
        Format.pp_set_formatter_out_functions Fmt.stdout saved)
      (fun () ->
        Console_eio.run ~clock ~ppf:Fmt.stdout ~progress:`Json (fun display ->
            Eio.Fiber.pair
              (fun () -> Prompt.line t "name: ")
              (fun () ->
                Console.Task.run display "work" (fun _ -> Fmt.pr "from work@.");
                Eio.Flow.copy_string "alice\n" sink)))
  in
  Alcotest.check string_answer "the answer" (Got "alice") (seen (fst answer));
  Alcotest.(check bool)
    "the line is the task's output" true
    (Display_harness.contains
       {|"event":"output","id":0,"task":["work"],"text":"from work"|}
       (Buffer.contents out))

(* [keys] reads each key as it is typed, unshown, and gives the terminal
   back as it found it; without a terminal it is refused. *)
let test_keys () =
  run (fun () ->
      let t, _ = prompt ~interactive:true "abc" in
      Alcotest.(check bool)
        "refused without a terminal" true
        (Result.is_error (Prompt.keys t (fun _ -> ()))));
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      let before = Unix.tcgetattr tty in
      let keys, () =
        Eio.Fiber.pair
          (fun () ->
            Prompt.keys t (fun input ->
                let a = Eio.Buf_read.any_char input in
                let b = Eio.Buf_read.any_char input in
                String.init 2 (function 0 -> a | _ -> b)))
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            Alcotest.(check bool)
              "key by key" false (Unix.tcgetattr tty).Unix.c_icanon;
            type_in master "q ")
      in
      (match keys with
      | Ok k -> Alcotest.(check string) "the keys" "q " k
      | Error _ -> Alcotest.fail "refused on a terminal");
      Alcotest.(check bool) "given back" true (Unix.tcgetattr tty = before))

let test_keys_at_end_of_input () =
  with_pty @@ fun env tty _ ->
  with_stdin ~stdin:tty (fun () ->
      let t = prompt_on env ~echoed:(Atomic.make 0) in
      Alcotest.check string_answer "the end of the input" Ended
        (seen (Prompt.keys t (fun _ -> raise End_of_file))))

let test_keys_split_sequences () =
  with_pty @@ fun env tty master ->
  with_stdin ~stdin:tty (fun () ->
      let prompt = prompt_on env ~echoed:(Atomic.make 0) in
      let before = Unix.tcgetattr tty in
      let typed = "界\027[A👩‍💻\027OD" in
      let received, () =
        Eio.Fiber.pair
          (fun () ->
            Prompt.keys prompt (fun input ->
                let out = Buffer.create (String.length typed) in
                for _ = 1 to String.length typed do
                  Buffer.add_char out (Eio.Buf_read.any_char input)
                done;
                Buffer.contents out))
          (fun () ->
            while not (masked tty) do
              Eio.Fiber.yield ()
            done;
            String.iter
              (fun byte ->
                type_in master (String.make 1 byte);
                Eio.Fiber.yield ())
              typed)
      in
      (match received with
      | Ok keys -> Alcotest.(check string) "every key byte, in order" typed keys
      | Error error -> Alcotest.failf "%a" Prompt.pp_error error);
      Alcotest.(check bool)
        "terminal attributes restored" true
        (Unix.tcgetattr tty = before))

let test_pp_error () =
  Alcotest.(check string)
    "end of input" "end of input"
    (Fmt.str "%a" Prompt.pp_error `End_of_input);
  Alcotest.(check string)
    "a message" "no terminal"
    (Fmt.str "%a" Prompt.pp_error (`Msg "no terminal"))

let test_line_at_limit () =
  run @@ fun () ->
  let line = String.make 4096 'x' in
  let t, _ = prompt ~interactive:true (line ^ "\nnext\n") in
  let first = seen (Prompt.line t "") in
  Alcotest.check string_answer "the next answer stays intact" (Got "next")
    (seen (Prompt.line t ""));
  Alcotest.check string_answer "4096 bytes is not longer than 4096" (Got line)
    first

let suite =
  ( "prompt",
    [
      Alcotest.test_case "line accepts exactly 4096 answer bytes" `Quick
        test_line_at_limit;
      Alcotest.test_case "a string source is not a terminal" `Quick
        test_a_string_source_is_not_a_terminal;
      Alcotest.test_case "line asks verbatim and keeps its place" `Quick
        test_line_asks_verbatim_and_keeps_its_place;
      Alcotest.test_case "line keeps the spaces it was given" `Quick
        test_line_keeps_the_spaces_it_was_given;
      Alcotest.test_case "line drops a crlf terminator" `Quick
        test_line_drops_a_crlf_terminator;
      Alcotest.test_case "line at end of input is refused" `Quick
        test_line_at_end_of_input_is_refused;
      Alcotest.test_case "confirm asks with a default of no" `Quick
        test_confirm_asks_with_a_default_of_no;
      Alcotest.test_case "confirm takes y and yes in any case" `Quick
        test_confirm_takes_y_and_yes_in_any_case;
      Alcotest.test_case "confirm takes everything else as no" `Quick
        test_confirm_takes_everything_else_as_no;
      Alcotest.test_case "confirm at end of input is refused" `Quick
        test_confirm_at_end_of_input_is_refused;
      Alcotest.test_case "a question is text" `Quick test_question_is_text;
      Alcotest.test_case "secret without a terminal is refused" `Quick
        test_secret_without_a_terminal_is_refused;
      Alcotest.test_case "secret leaves the domain running" `Quick
        test_secret_leaves_the_domain_running;
      Alcotest.test_case "secret drops what was typed before it masks" `Quick
        test_secret_drops_typeahead;
      Alcotest.test_case "secret drops what was typed after a line" `Quick
        test_secret_after_a_line;
      Alcotest.test_case "a password is the bytes typed" `Quick
        test_secret_keeps_bytes;
      Alcotest.test_case "secret refuses a line of more than 4096 bytes" `Quick
        test_secret_cap;
      Alcotest.test_case "a prompt leaves other fibers' output to their task"
        `Quick test_prompt_leaves_other_fibers_attributed;
      Alcotest.test_case "keys" `Quick test_keys;
      Alcotest.test_case "keys at the end of the input" `Quick
        test_keys_at_end_of_input;
      Alcotest.test_case "keys preserve split UTF-8 and escape sequences" `Quick
        test_keys_split_sequences;
      Alcotest.test_case "an error formats" `Quick test_pp_error;
    ] )
