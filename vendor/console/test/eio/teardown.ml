(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A command that ends inside a live display the way its argument says: it
   returns, calls [exit] (a CLI's error path), raises to the toplevel, or has
   its fiber cancelled; or, with [secret], it is interrupted while it reads
   a password; or, with [colourless], it chose no colour before the display;
   or, with [widgets], it prints every widget before it; or, with [themed],
   it gave standard error a theme and chose nothing else; or, with [logs] or [logs-failed], its
   task prints and logs, then succeeds or fails; or, with [suspend], it is stopped by
   SIGTSTP, as Ctrl-Z does, and goes on once continued, or, with
   [orphaned-suspend], in a session of its own where no stop takes effect; or, with [unflushed], it
   prints lines it never flushes; or, with [held], it prints a line outside a
   task that it does not flush, then one in a task that fails; or, with
   [sigint-mid-write] or [sigint-blocked], it is interrupted in the middle
   of a frame's write. It also prints a line on standard
   output, which goes above the region, and its task a line of output. *)

(* SIGINT comes once the password is masked. *)
let secret env =
  let prompt = Console_eio.Prompt.v (Eio.Stdenv.stdin env) in
  Eio.Fiber.both
    (fun () -> ignore (Console_eio.Prompt.secret prompt "password: "))
    (fun () ->
      while (Unix.tcgetattr Unix.stdin).Unix.c_echo do
        Eio.Fiber.yield ()
      done;
      Unix.kill (Unix.getpid ()) Sys.sigint)

(* The program chose no colour itself, as a [--color=never] does, before
   it runs a display. *)
let colourless _env =
  Fmt.set_style_renderer Fmt.stdout `None;
  Fmt.set_style_renderer Fmt.stderr `None;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.succeed (Console.Task.start root "plain"))

(* Every widget, on standard output once [setup] has looked at the locale,
   then a display with a bar. *)
let widgets _env =
  let open Console in
  Console_tty.setup ();
  let long = View.text (String.concat " " (List.init 20 (Fun.const "word"))) in
  let leaf = Tree.v long [] in
  Fmt.pr "%a@.%a@.%a@.%a@.%a@." View.pp
    (Table.view
       [
         Table.column (View.text "a");
         Table.column ~overflow:`Truncate (View.text "b");
       ]
       [ [ View.text "1"; long ] ])
    View.pp
    (Panel.view ~title:long long)
    View.pp
    (Tree.view (Tree.v long [ Tree.v long [ leaf ]; leaf ]))
    View.pp (Rule.view ~label:long ()) View.pp
    (Bar.horizontal ~brackets:true ~width:10 0.55);
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Task.start root "counting" in
      Task.set_items task ~count:1 ~total:3;
      Console_eio.with_output ignore;
      Task.succeed task)

(* The program chose a theme, and no colour, before the display. *)
let themed _env =
  Console.Theme.set Fmt.stderr Console.Theme.nord;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.succeed (Console.Task.start root "themed"))

(* A task's output, a line on each standard output, one within the root task
   and a Logs message from a fiber the task forked; with [fails], the task
   fails. *)
let logs ~fails _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Console.Task.run root "work" (fun task ->
          Console.Task.log task "an output line";
          Fmt.pr "a result line@.";
          Fmt.epr "a note line@.";
          Console.Task.within root (fun () -> Fmt.pr "a root line@.");
          Eio.Fiber.both
            (fun () -> Logs.warn (fun m -> m "a forked note"))
            Eio.Fiber.yield;
          if fails then Console.Task.fail task))

let suspend _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Console.Task.start root "pausing" in
      Console_eio.with_output ignore;
      Unix.kill (Unix.getpid ()) Sys.sigtstp;
      Console.Task.set_items task ~count:1 ~total:2;
      Console_eio.with_output ignore;
      Console.Task.succeed task)

(* SIGINT in the middle of a frame's write, half of it on the terminal; with
   [blocked], the write then never ends. *)
let mid_write ~blocked _env =
  let armed = ref false in
  let out = Format.pp_get_formatter_out_functions Fmt.stderr () in
  let out_string s o n =
    if !armed && n > 1 then begin
      armed := false;
      out.out_string s o (n / 2);
      out.out_flush ();
      Unix.kill (Unix.getpid ()) Sys.sigint;
      if blocked then begin
        let r, _ = Unix.pipe () in
        ignore (Unix.read r (Bytes.create 1) 0 1 : int)
      end;
      out.out_string s (o + (n / 2)) (n - (n / 2))
    end
    else out.out_string s o n
  in
  Format.pp_set_formatter_out_functions Fmt.stderr { out with out_string };
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      let task = Console.Task.start root "pushing" in
      Console.Task.log task "pushed 3 objects";
      Fmt.pr "a line of output@.";
      Console_eio.with_output (fun _ -> armed := true);
      Console.Task.succeed task)

let repeated_signal ~first ~second ~cut ~closing _env =
  let armed = ref false in
  let out_string s offset length =
    if !armed && length > 1 then begin
      armed := false;
      let cut = 1 + (cut mod (length - 1)) in
      output_substring stdout s offset cut;
      flush stdout;
      Unix.kill (Unix.getpid ()) first;
      Gc.minor ();
      Unix.kill (Unix.getpid ()) second;
      Gc.minor ();
      output_substring stdout s (offset + cut) (length - cut)
    end
    else output_substring stdout s offset length
  in
  let ppf = Format.make_formatter out_string (fun () -> flush stdout) in
  Fmt.set_utf_8 ppf true;
  Fmt.set_style_renderer ppf `Ansi_tty;
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) ~ppf ~progress:`Tty
    (fun root ->
      let task = Console.Task.start root "pushing" in
      Console_eio.with_output ignore;
      if closing then begin
        Console.Task.succeed task;
        armed := true
      end
      else begin
        armed := true;
        Console.Task.succeed task
      end)

(* Lines printed outside a task and never flushed, then a mark written
   straight to the terminal: what came out before the mark is what the
   display did not hold back. *)
let unflushed _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun _ ->
      for i = 1 to 100_000 do
        Fmt.pr "line %06d\n" i
      done;
      ignore (Unix.write_substring Unix.stdout "MARK\n" 0 5 : int))

(* A whole line outside a task, long enough that the formatter writes it,
   waits for a flush; the task's line that comes next must not take it
   along. *)
let held _env =
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ()) (fun root ->
      Fmt.pr "a held line %s\n" (String.make 100 '.');
      Console.Task.run root "work" (fun task ->
          Fmt.pr "a task line@.";
          Console.Task.fail task))

(* Quiet mode keeps terminal output only for a failed or cancelled task. *)
let quiet ending _env =
  Logs.set_level (Some Logs.Info);
  Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ())
    ~progress:`Quiet ~label:"quiet header" (fun root ->
      Console.Task.run root "quiet work" (fun task ->
          Console.Task.log task "display line";
          Fmt.pr "stdout line@.";
          Fmt.epr "stderr line@.";
          Logs.info (fun m -> m "logs line");
          match ending with
          | "fail" -> Console.Task.fail task
          | "cancel" -> Console.Task.cancel task
          | _ -> ());
      Console.Task.within root (fun () -> Fmt.pr "root line@."))

let display _env =
  let clock = Eio_mock.Clock.Mono.make () in
  let run () =
    Console_eio.run ~clock (fun root ->
        let task = Console.Task.start root "pushing" in
        Console.Task.log task "pushed 3 objects";
        Fmt.pr "a line of output@.";
        Console_eio.with_output ignore;
        match Sys.argv with
        | [| _; "sigint" |] -> Unix.kill (Unix.getpid ()) Sys.sigint
        | [| _; "exit" |] -> exit 3
        | [| _; "raise" |] -> failwith "push failed"
        | [| _; "cancel" |] -> Eio.Fiber.await_cancel ()
        | _ -> Console.Task.succeed task)
  in
  Eio.Fiber.first run Eio.Fiber.yield

let () =
  Eio_main.run @@ fun env ->
  match Sys.argv with
  | [| _; "repeated-signal"; first; second; cut; closing |] ->
      let signal = function "int" -> Sys.sigint | _ -> Sys.sigterm in
      repeated_signal ~first:(signal first) ~second:(signal second)
        ~cut:(int_of_string cut) ~closing:(bool_of_string closing) env
  | [| _; "quiet-ok" |] -> quiet "ok" env
  | [| _; "quiet-fail" |] -> quiet "fail" env
  | [| _; "quiet-cancel" |] -> quiet "cancel" env
  | [| _; "secret" |] -> secret env
  | [| _; "colourless" |] -> colourless env
  | [| _; "widgets" |] -> widgets env
  | [| _; "themed" |] -> themed env
  | [| _; "logs" |] -> logs ~fails:false env
  | [| _; "logs-failed" |] -> logs ~fails:true env
  | [| _; "suspend" |] -> suspend env
  | [| _; "orphaned-suspend" |] ->
      ignore (Unix.setsid () : int);
      suspend env
  | [| _; "unflushed" |] -> unflushed env
  | [| _; "held" |] -> held env
  | [| _; "sigint-mid-write" |] -> mid_write ~blocked:false env
  | [| _; "sigint-blocked" |] -> mid_write ~blocked:true env
  | _ -> display env
