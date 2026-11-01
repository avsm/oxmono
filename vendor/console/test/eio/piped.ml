(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A program under [Console_eio.run] on a pipe: its results on standard output,
    its display and Logs messages on standard error. Given a count, it does that
    many steps, enough to write far more than a pipe holds, so that a reader
    that stops early closes the pipe under it; with [bare], it prints the
    results alone, without console; with [exit], it calls [exit] inside a task,
    and an [at_exit] function it registered first logs and prints. With
    [quiet-long] or [quiet-prompt], quiet, it leaves a line unended on standard
    error, a line too long to hold or a question, before a task fails. With
    [json-stdout], as JSON events, a task prints a line on standard output and
    one on standard error. With [sleeps] and a number of steps, a task's two
    fibers sleep on {!Console_eio.clock}, one of them working that many steps
    before each sleep. With [size], it prints the size of the terminal of
    standard error. *)

let step display i =
  Fmt.pr "result %d\n" i;
  Console.Task.run display (Fmt.str "step %d" i) (fun _ ->
      Logs.info (fun m -> m "working on %d" i))

let () =
  Logs.set_level (Some Logs.Info);
  Eio_main.run @@ fun env ->
  (* A clock that does not move, so that every time printed is 0.0s. *)
  let clock =
    (Eio_mock.Clock.Mono.make () :> Eio.Time.Mono.ty Eio.Resource.t)
  in
  match Sys.argv with
  | [| _; "bare"; n |] ->
      for i = 1 to int_of_string n do
        Fmt.pr "result %d\n" i
      done
  | [| _; "notes"; n |] ->
      Console_eio.run ~clock (fun display ->
          for i = 1 to int_of_string n do
            step display i;
            Logs.warn (fun m -> m "after %d" i);
            Fmt.epr "note %d@." i
          done)
  | [| _; "exit" |] ->
      Logs.set_reporter (Logs_fmt.reporter ());
      at_exit (fun () ->
          Logs.warn (fun m -> m "logged at exit");
          Fmt.epr "printed at exit@.";
          Fmt.pr "result at exit@.");
      Console_eio.run ~clock (fun display ->
          Console.Task.run display "work" (fun _ ->
              Fmt.pr "result 1@.";
              exit 3))
  | [| _; "sleeps"; work |] ->
      let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
      let work () =
        for i = 1 to int_of_string work do
          ignore (Sys.opaque_identity (i * i))
        done
      in
      Console_eio.run ~clock ~progress:`Json (fun display ->
          Console.Task.run display "build" (fun _ ->
              Eio.Fiber.both
                (fun () ->
                  for _ = 1 to 4 do
                    work ();
                    Eio.Time.Mono.sleep clock 0.1
                  done)
                (fun () ->
                  for _ = 1 to 3 do
                    Eio.Time.Mono.sleep clock 0.13
                  done)))
  | [| _; "quiet-long" |] ->
      Console_eio.run ~clock ~progress:`Quiet (fun display ->
          Fmt.epr "%s%!" (String.make 70_000 'x');
          Console.Task.run display "work" Console.Task.fail)
  | [| _; "quiet-prompt" |] ->
      Console_eio.run ~clock ~progress:`Quiet (fun display ->
          let prompt =
            Console_eio.Prompt.v ~interactive:true (Eio.Stdenv.stdin env)
          in
          ignore (Console_eio.Prompt.line prompt "name? ");
          Console.Task.run display "work" Console.Task.fail)
  | [| _; "json-stdout" |] ->
      Console_eio.run ~clock ~progress:`Json (fun display ->
          Console.Task.run display "build" (fun _ ->
              Fmt.pr "from stdout@.";
              Fmt.epr "from stderr@."))
  | [| _; "size" |] -> (
      match Console_eio.size Fmt.stderr with
      | Some size ->
          let w, h = size () in
          Fmt.pr "%dx%d@." w h
      | None -> Fmt.pr "no terminal@.")
  | [| _; n |] ->
      Console_eio.run ~clock (fun display ->
          for i = 1 to int_of_string n do
            step display i
          done)
  | _ -> invalid_arg "piped [bare] count"
