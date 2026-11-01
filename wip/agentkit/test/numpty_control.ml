(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The control socket, against a real unix socket in a temporary store and no
   engine.

   The property the whole design of the status snapshot exists to hold is the
   first one here: the socket answers while a turn is blocked. [Agent.send]
   blocks for minutes at a time, so a control fiber that asked the agent
   anything would stop answering precisely when a person most wants to ask. The
   turn is stood in for by a fiber blocked on a promise nobody resolves, which
   is what a four minute prefill looks like from outside.

   The rest are the faults. A socket file a crashed run left is unlinked and
   replaced, since the store's lock is taken before this binds. A store path too
   long for a unix address gives a daemon that runs and warns rather than one
   that will not start. A client that goes away mid-answer is not an event. *)

module Control = Numpty_daemon.Control
module Journal = Agentkit.Journal
module Memory = Agentkit.Memory
module Status = Numpty_daemon.Status
module Store = Numpty_daemon.Store

let failures = ref 0

(* Flushed as it goes, since a socket that has stopped answering hangs this
   test and the last line printed is what says where. *)
let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  at 0

let job =
  {
    Status.task = "feeds";
    started = "2026-08-08T09:14:07Z";
    session = 2;
    ctx_used = 24000;
    ctx_size = 32768;
    turns = 12;
    tool_calls = 5;
    tool = Some "fetch";
    prefilled = 0;
    prefill_total = 0;
  }

let snapshot =
  {
    Status.run = 9;
    since = "2026-08-08T06:00:11Z";
    model = "/models/ds4.gguf";
    backend = "CPU";
    netd = "alive";
    version = 17;
    job = Some job;
    tasks =
      [
        { Status.task = "feeds"; next = None; waiting = true };
        {
          Status.task = "digest";
          next = Some "2026-08-09T07:00:00Z";
          waiting = false;
        };
      ];
  }

(* A store with something in it to read back, written with the writers a run
   uses. *)
let build ~clock root =
  Eio.Switch.run @@ fun sw ->
  let j = Journal.create ~sw ~clock ~run:9 ~seq:1 (Store.journal_dir root) in
  let m = Memory.create ~clock (Store.memory_dir root) in
  ignore (Journal.append j (Journal.Content "the tag moved"));
  ignore
    (Memory.write m ~seq:(Journal.next_seq j) ~cause:"recorded the moved tag"
       ~journal:(fun mw -> ignore (Journal.append j (Journal.Memory_write mw)))
       ~id:"ds4-upstream" ~kind:Memory.Fact ~title:"the upstream tag moved"
       ~body:"upstream is at v0.9" ~tags:[]);
  Journal.close j

(* A wait long enough that a slow machine does not fail the test, and short
   enough that a socket which has stopped answering fails it rather than hanging
   the suite. *)
let bound = 10.

let promptly ~clock what f =
  match Eio.Time.with_timeout clock bound (fun () -> Ok (f ())) with
  | Ok v -> Some v
  | Error `Timeout ->
      incr failures;
      Printf.printf "FAIL - %s did not answer within %gs\n" what bound;
      None

let run env =
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let fs = Eio.Stdenv.fs env in
  let tmp = Filename.temp_file "ds4-numpty-control" "" in
  Sys.remove tmp;
  let root = Eio.Path.(fs / tmp) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" tmp)))
  @@ fun () ->
  build ~clock root;
  (* What a crashed run leaves behind. The lock is taken before the socket is
     bound, so this file belongs to nobody. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) (Store.control_path root)
    "not really a socket";
  Eio.Switch.run @@ fun sw ->
  let live = ref snapshot in
  let impl = Control.make ~snapshot:(fun () -> !live) ~clock ~root in
  (match Control.serve ~sw ~net ~root impl with
  | `Serving _ -> check "a socket file a crashed run left is replaced" true
  | `Unbound why ->
      check ("a socket file a crashed run left is replaced (" ^ why ^ ")") false);
  (* One switch per question, so a connection does not outlive the answer. *)
  let ask request =
    Eio.Switch.run (fun sw -> Control.ask ~sw ~net ~root request)
  in
  (* The one worth writing first. A fiber blocked on a promise nobody resolves
     stands in for a turn inside the engine, and the socket has to answer
     anyway. *)
  let turn_ended = ref false in
  let never, _resolve = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () ->
      Eio.Promise.await never;
      turn_ended := true)
    (fun () ->
      (match
         promptly ~clock "status, while a turn is blocked" (fun () ->
             ask Control.Status)
       with
      | Some (Ok (Control.Running r)) ->
          check "status answers while a turn is blocked"
            (r.Status.r_run = 9 && r.Status.r_netd = "alive");
          check "and says which job is running" (r.Status.r_job = Some "feeds");
          check "and which memory version is in force" (r.Status.r_version = 17)
      | _ -> check "status answers while a turn is blocked" false);
      match
        promptly ~clock "jobs, while a turn is blocked" (fun () ->
            ask Control.Jobs)
      with
      | Some (Ok (Control.Jobs_are j)) ->
          check "jobs answers while a turn is blocked"
            (match j.Status.j_job with
            | Some job -> job.Status.session = 2 && job.Status.ctx_used = 24000
            | None -> false);
          check "and names the tool call in flight"
            (match j.Status.j_job with
            | Some job -> job.Status.tool = Some "fetch"
            | None -> false);
          check "and carries every task with when it fires next"
            (List.map (fun (d : Status.due) -> d.Status.task) j.Status.j_tasks
            = [ "feeds"; "digest" ])
      | _ -> check "jobs answers while a turn is blocked" false);
  check "the turn was not disturbed by any of it" (not !turn_ended);
  (* The live prefill counter is the caller's to fill in, and the snapshot is
     read afresh for every question, so a status taken later says something
     later. *)
  live :=
    {
      !live with
      Status.job =
        Some { job with Status.prefilled = 1200; prefill_total = 8000 };
    };
  (match ask Control.Jobs with
  | Ok (Control.Jobs_are j) ->
      check "a later question is answered from a later snapshot"
        (match j.Status.j_job with
        | Some job -> job.Status.prefilled = 1200
        | None -> false)
  | _ -> check "a later question is answered from a later snapshot" false);
  (* The two conveniences, served from the store so that one client reaches
     everything without knowing which side of the line a thing falls on. *)
  (match ask (Control.Memory { at = None }) with
  | Ok (Control.Memory_is m) ->
      check "memory is served from the store"
        (m.Status.m_version = 1 && contains m.Status.m_text "ds4-upstream")
  | _ -> check "memory is served from the store" false);
  (match ask (Control.Log { since = None; kinds = []; limit = 10 }) with
  | Ok (Control.Lines l) ->
      check "the log is served from the store"
        (List.exists (fun line -> contains line "the tag moved") l.Status.lines)
  | _ -> check "the log is served from the store" false);
  (match
     ask (Control.Log { since = None; kinds = [ "memory_write" ]; limit = 10 })
   with
  | Ok (Control.Lines l) ->
      check "and filtered by kind" (List.length l.Status.lines = 1)
  | _ -> check "and filtered by kind" false);
  (* A line that does not parse is a terminal fault for that connection: the
     reader is part way through a line it will never finish, so no later read is
     trustworthy. The daemon carries on for everybody else. *)
  let path = Eio.Path.native_exn (Store.control_path root) in
  Eio.Switch.run (fun sw ->
      let flow = Eio.Net.connect ~sw net (`Unix path) in
      Eio.Buf_write.with_flow flow (fun w ->
          Eio.Buf_write.string w "this is not a request\n";
          Eio.Buf_write.flush w);
      let r = Eio.Buf_read.of_flow flow ~max_size:0x10000 in
      let answered = Eio.Buf_read.line r in
      check "a line that does not parse is refused in words"
        (contains answered "error");
      check "and the connection ends there"
        (match Eio.Buf_read.line r with
        | _ -> false
        | exception End_of_file -> true));
  (* A client that goes away mid-answer ends its own fiber and nothing else. *)
  Eio.Switch.run (fun sw ->
      let flow = Eio.Net.connect ~sw net (`Unix path) in
      Eio.Buf_write.with_flow flow (fun w ->
          Eio.Buf_write.string w "{\"jobs\":{}}\n";
          Eio.Buf_write.flush w));
  (match
     promptly ~clock "status, after a client disconnected mid-answer" (fun () ->
         ask Control.Status)
   with
  | Some (Ok (Control.Running _)) ->
      check "a client that disconnects mid-answer is not an event" true
  | _ -> check "a client that disconnects mid-answer is not an event" false);
  (* follow is the one method whose shape differs between the two transports,
     being a stream of lines here. A record appended after the stream started
     must reach it. *)
  let followed = ref [] in
  Eio.Switch.run (fun sw ->
      Eio.Fiber.first
        (fun () ->
          ignore
            (Control.stream ~sw ~net ~root ~kinds:[ "content" ] (fun l ->
                 followed := l :: !followed)))
        (fun () ->
          Eio.Time.sleep clock 0.2;
          Eio.Switch.run (fun jsw ->
              let next =
                (Journal.recover (Store.journal_dir root)).Journal.next_seq
              in
              let j =
                Journal.create ~sw:jsw ~clock ~run:9 ~seq:next
                  (Store.journal_dir root)
              in
              ignore
                (Journal.append j (Journal.Content "appended while following"));
              Journal.close j);
          let rec wait n =
            if !followed <> [] || n > 40 then ()
            else begin
              Eio.Time.sleep clock 0.25;
              wait (n + 1)
            end
          in
          (* Returning ends the stream, which is what a client leaving does. *)
          wait 0));
  check "follow streams a record appended after it started"
    (List.exists (fun l -> contains l "appended while following") !followed);
  (* A store path too long for a unix address gives a daemon that runs and warns
     rather than one that will not start. *)
  let deep =
    List.fold_left
      (fun p _ -> Eio.Path.(p / "a-directory-with-a-name"))
      root [ 1; 2; 3; 4; 5 ]
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 deep;
  match Control.serve ~sw ~net ~root:deep impl with
  | `Serving _ ->
      check "a store path too long for a unix address is refused" false
  | `Unbound why ->
      check "a store path too long for a unix address is refused"
        (contains why "hundred");
      check "and the refusal says what to do about it" (contains why "--store")

let () =
  Eio_main.run run;
  if !failures > 0 then begin
    Printf.printf "\n%d failure(s)\n" !failures;
    exit 1
  end
