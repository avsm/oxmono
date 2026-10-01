(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* okitd's loop. One fiber reads calls, runs them in the order they arrive and
   answers each with one result, so the trace lines written while a call runs
   can carry that call's id without anything having to be tracked.

   Nothing here reads a file of the workspace. The one path it names is the
   dune-project it tests for, which decides whether a session is attempted. *)

(* [clip n s] is [s] on one line and within [n] bytes, counting what it drops.
   A trace goes in a column of an interface, and a protocol fault carries a line
   that may be a whole source file. The count is part of what fits, so the
   answer is no longer than [n]. *)
let clip n s =
  let s =
    String.map (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c) s
  in
  if String.length s <= n then s
  else
    let dropped = Printf.sprintf "… (%d bytes)" (String.length s) in
    String.sub s 0 (max 0 (n - String.length dropped)) ^ dropped

let no_dune reason =
  Printf.sprintf
    "okit's dune session is not running: %s\n\
     The session is started once, when okitd starts, so this is not tried \
     again."
    reason

let no_merlin =
  "okit found no ocamlmerlin to answer for this workspace, so this tool has \
   nothing to ask. Put ocamlmerlin on the PATH and start again."

(* Merlin reads the uses of a name outside the file it was given from the index
   dune writes for the [@ocaml-index] alias, and where that index is missing it
   answers with the uses in that one file and says nothing. So the alias is
   built before the query, and a build that did not reach it is reported above
   the answer rather than left to make the answer look complete. *)
let stale_index =
  "The index of the workspace could not be rebuilt, so uses outside this file \
   may be missing or may name a place that has moved. Build the workspace and \
   ask again.\n"

(* Nothing okitd runs is given okitd's own standard input, which is the pipe the
   protocol arrives on. A command that reads it, [cat] or a git that prompts,
   would take the peer's next call for its own input, and the call would then
   never be answered. Every child is handed an empty one instead, so a program
   that reads sees the end of its input at once. *)
let no_input = Eio.Flow.string_source ""

let run ?(dir = ".") ~stdin ~stdout ~proc ~net ~clock ~fs () =
  let root = Eio.Path.(fs / dir) in
  Eio.Switch.run @@ fun sw ->
  Eio.Buf_write.with_flow stdout @@ fun w ->
  let reader = Eio.Buf_read.of_flow stdin ~max_size:Agentkit.Line.max_line in
  (* The call in flight, which every trace written while it runs belongs to.
     Startup and anything else outside a call has none. *)
  let current = ref None in
  (* Each message is flushed as it is written. A trace exists to say what a
     call that has not answered is waiting on, and one still in a buffer says
     nothing. *)
  let send msg =
    Proto.write_to_client w msg;
    Eio.Buf_write.flush w
  in
  let trace line = send (Proto.Trace { id = !current; line }) in
  (* A peer that gives up on okitd kills it, and a dune server this okitd
     started would then be left holding the workspace's build lock with nothing
     left to answer to. The signal is caught so that the session is stopped
     first. A flag on its own would not do, since the loop spends its life
     blocked in a read and would look at the flag only when the peer wrote a
     line it is never going to write, so the wait is a fiber racing the work. *)
  let signalled = Eio.Condition.create () in
  let asked_to_stop = ref false in
  let session = ref (Error "okitd's dune session was never started") in
  let stop () = match !session with Ok d -> Session.stop d | Error _ -> () in
  let serve () =
    let dune_project = Eio.Path.is_file Eio.Path.(root / "dune-project") in
    session :=
      if dune_project then
        Session.start ~trace ~client:"okit" ~sw ~proc ~net ~clock ~root ()
      else Error "this workspace has no dune-project";
    (* Merlin is looked for only where a session runs, as it is for the tools
       a humpty assembles itself: the merlin tools join the dune ones or
       neither is there. *)
    let merlin =
      match !session with
      | Ok _ -> Merlin.find ~trace ~proc ~root ()
      | Error _ -> None
    in
    let status =
      match (!session, dune_project) with
      | Ok _, _ -> Status.Active { merlin = Option.is_some merlin }
      | Error _, false -> Status.No_dune_project
      | Error e, true -> Status.Refused e
    in
    (* The names the workspace goes by, for writing merlin's absolute answers
       from its root. Merlin resolves the links in a path where Eio does not. *)
    let roots =
      match Eio.Path.native root with None -> [] | Some d -> Report.roots d
    in
    let with_dune f =
      match !session with Ok d -> f d | Error e -> no_dune e
    in
    let with_merlin f = match merlin with Some m -> f m | None -> no_merlin in
    (* A failure is reported as the text of a result, since the peer hands
       that text to a model, which can then correct itself. The wording is
       [Ds4.Tool]'s, so that a result reads the same whether the operation
       ran here or in the peer. Resource exhaustion is not a tool failure, and
       describing it as one would hide it while the process is already in
       trouble. *)
    let guard f =
      try f () with
      | (Out_of_memory | Stack_overflow | Eio.Cancel.Cancelled _) as e ->
          raise e
      | e -> "Error: " ^ Printexc.to_string e
    in
    let dispatch (op : Proto.op) =
      guard @@ fun () ->
      match op with
      | Proto.Build { targets } -> (
          with_dune @@ fun d ->
          let targets = Report.targets targets in
          Session.trace d ("build: " ^ String.concat " " targets);
          match Session.build d ~targets with
          | Error e -> e
          | Ok r -> Report.build ~what:"build" r)
      | Proto.Test -> (
          with_dune @@ fun d ->
          Session.trace d "test: running";
          match Session.runtest d with
          | Error e -> e
          | Ok r -> Report.build ~what:"tests" r)
      | Proto.Promote { path } -> (
          with_dune @@ fun d ->
          Session.trace d ("promote: " ^ path);
          match Session.promote d ~path with
          | Error e -> e
          | Ok () -> Printf.sprintf "promoted %s" path)
      | Proto.Project { module_ } -> (
          (* Described afresh on every call. The describe spawns dune, which
             this process is small enough to fork, so a workspace that has
             changed is reported as it is rather than as it was. *)
          with_dune
          @@ fun _ ->
          match Project.describe ~trace ~proc ~root () with
          | Error e -> e
          | Ok p ->
              if module_ = "" then Project.to_text p
              else Project.module_text p ~name:module_)
      | Proto.After_write { path; verb } -> (
          with_dune @@ fun d ->
          Session.trace d (verb ^ ": " ^ path);
          (* A file dune does not compile leaves a build with nothing to say
             about it, so nothing is what the write appends. *)
          if not (Report.built path) then ""
          else
            match Session.build d ~targets:[ "." ] with
            | Error e -> e
            | Ok r -> Report.after_write ~path r)
      | Proto.Outline { path; source } -> (
          with_merlin @@ fun m ->
          Merlin.trace m ("outline: " ^ path);
          match Merlin.outline m ~path ~source with
          | Error e -> e
          | Ok items -> Report.outline items)
      | Proto.Type_at { path; source; line; col } -> (
          with_merlin @@ fun m ->
          Merlin.trace m (Printf.sprintf "type_at: %s:%d:%d" path line col);
          match Merlin.type_at m ~path ~source { Merlin.line; col } with
          | Error e -> e
          | Ok typ -> typ)
      | Proto.Locate { path; source; line; col } -> (
          with_merlin @@ fun m ->
          Merlin.trace m (Printf.sprintf "locate: %s:%d:%d" path line col);
          match Merlin.locate m ~path ~source { Merlin.line; col } with
          | Error e -> e
          | Ok (`Not_found why) -> why
          | Ok (`Found (file, p)) ->
              Printf.sprintf "%s:%d:%d" (Report.under ~roots file) p.Merlin.line
                p.Merlin.col)
      | Proto.Errors { path; source } -> (
          with_merlin @@ fun m ->
          Merlin.trace m ("errors: " ^ path);
          match Merlin.errors m ~path ~source with
          | Error e -> e
          | Ok ps -> Report.problems ps)
      | Proto.Occurrences { path; source; line; col } -> (
          with_merlin @@ fun m ->
          with_dune @@ fun d ->
          Session.trace d "occurrences: indexing";
          let indexed =
            match Session.build d ~targets:[ "@ocaml-index" ] with
            | Ok r -> r.ok
            | Error _ -> false
          in
          Merlin.trace m (Printf.sprintf "occurrences: %s:%d:%d" path line col);
          match Merlin.occurrences m ~path ~source { Merlin.line; col } with
          | Error e -> e
          | Ok places ->
              (if indexed then "" else stale_index)
              ^ Report.occurrences ~roots places)
      | Proto.Search { path; source; query; limit } -> (
          with_merlin @@ fun m ->
          Merlin.trace m ("search: " ^ query);
          match Merlin.search m ~path ~source ~query ~limit with
          | Error e -> e
          | Ok hits -> Report.search ~roots hits)
      | Proto.Complete { path; source; line; col; prefix } -> (
          with_merlin @@ fun m ->
          Merlin.trace m
            (Printf.sprintf "complete: %s at %s:%d" prefix path line);
          match
            Merlin.complete m ~path ~source { Merlin.line; col } ~prefix
          with
          | Error e -> e
          | Ok cs -> Report.completions cs)
      | Proto.Bash { command } ->
          (* A shell beside a loaded model costs what any other fork does,
             which is why it runs here. It is granted or not by the peer,
             exactly as before, and it runs with this process's whole
             authority.

             The semantics are [Ds4.Toolbox.bash]'s: bash -c, the
             command's standard output captured, its standard error left to
             okitd's own. *)
          trace ("bash: " ^ clip 60 command);
          Eio.Process.parse_out proc Eio.Buf_read.take_all ~stdin:no_input
            [ "bash"; "-c"; command ]
    in
    send
      (Proto.Hello
         {
           status = Status.note status;
           dune = Result.is_ok !session;
           merlin = Option.is_some merlin;
         });
    let rec loop () =
      match Proto.read_to_server reader with
      | `Eof -> ()
      | `Msg Proto.Shutdown -> ()
      | `Bad line ->
          (* The peer is this same binary, so a line it cannot write is a
             fault rather than bad input, and one over the length limit leaves
             the reader part way through a line it will never finish. Either
             way nothing later on this connection is worth reading. *)
          stop ();
          failwith
            (Printf.sprintf "okitd read a line that is not a message: %s"
               (clip 200 line))
      | `Msg (Proto.Call { id; op }) ->
          current := Some id;
          let output = dispatch op in
          current := None;
          send (Proto.Result { id; output });
          loop ()
    in
    loop ()
  in
  let previous =
    Sys.signal Sys.sigterm
      (Sys.Signal_handle
         (fun _ ->
           asked_to_stop := true;
           Eio.Condition.broadcast signalled))
  in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigterm previous)
    (fun () ->
      (* The condition is looked at from inside the wait, so a signal arriving
         between the flag being read and the wait beginning is not lost. *)
      Eio.Fiber.first serve (fun () ->
          Eio.Condition.loop_no_mutex signalled (fun () ->
              if !asked_to_stop then Some () else None));
      (* Reached by the loop ending, by a shutdown, or by the signal, which
         cancels whatever the server was in the middle of. The session is
         stopped in each of them, and with it the dune server it started. *)
      stop ())
