(* SPDX-License-Identifier: ISC *)
module Runner = Spindle__Runner
module Patch = Spindle__Patch
module Store = Spindle__Store
module J = Spindle__Json

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 30. @@ fun () ->
  let name = Filename.temp_file "spindle-runner-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  let directory = Eio.Path.(env#fs / name) in
  let git args =
    Eio.Process.run env#process_mgr ~cwd:directory
      ("git" :: "-c" :: "user.name=Test" :: "-c"
     :: "user.email=test@example.test" :: args)
  in
  git [ "init"; "-q" ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; "fixture" ];
  let commit =
    Eio.Process.parse_out env#process_mgr Eio.Buf_read.line ~cwd:directory
      [ "git"; "rev-parse"; "HEAD" ]
  in
  let runner : Runner.env =
    { system = env; directory; slots = Eio.Semaphore.make 1 }
  in
  let run ~cancel id =
    let ready = name ^ "/" ^ id ^ ".ready" in
    let leaked = name ^ "/" ^ id ^ ".leaked" in
    let script =
      {|(sleep 0.6; echo leaked > "$2") & echo ready > "$1"; |}
      ^ if cancel then "wait" else "exit 0"
    in
    let job =
      Spindle.Job.v "children"
        [ Command [ "sh"; "-c"; script; "child-test"; ready; leaked ] ]
    in
    let workflow = Runner.v job in
    let input : Runner.input =
      {
        id;
        repo = "did:web:repo.test";
        source = name;
        commit;
        metadata = J.obj [];
      }
    in
    Eio.Fiber.both
      (fun () ->
        Runner.execute runner input
          ~record:(fun _ -> 0L)
          ~persist:(fun () -> ())
          workflow)
      (fun () ->
        let rec wait () =
          if not (Sys.file_exists ready) then (
            Eio.Time.sleep env#clock 0.01;
            wait ())
        in
        wait ();
        if cancel then Runner.cancel ~persist:(fun () -> ()) workflow);
    assert (workflow.status = if cancel then "cancelled" else "success");
    Eio.Time.sleep env#clock 0.8;
    assert (not (Sys.file_exists leaked));
    assert (not (Sys.file_exists (name ^ "/" ^ id ^ ".children.work")))
  in
  run ~cancel:true "cancel";
  run ~cancel:false "finish";
  let log_job id script check =
    let workflow =
      Runner.v (Spindle.Job.v "output" [ Command [ "sh"; "-c"; script ] ])
    in
    let input : Runner.input =
      {
        id;
        repo = "did:web:repo.test";
        source = name;
        commit;
        metadata = J.obj [];
      }
    in
    Runner.execute runner input
      ~record:(fun _ -> 0L)
      ~persist:(fun () -> ())
      workflow;
    let output stream =
      List.rev workflow.events
      |> List.filter_map (fun event ->
          if
            J.get "type" event = "data"
            && J.number (J.required "step" event) = 1.
            && J.get "stream" event = stream
          then Some (J.get "content" event)
          else None)
      |> String.concat ""
    in
    check workflow output
  in
  log_job "long" "head -c 70000 /dev/zero | tr '\\000' x"
    (fun workflow output ->
      assert (workflow.status = "success");
      assert (output "stdout" = String.make 70000 'x'));
  log_job "whitespace" "printf 'one\\r\\n\\rprogress'; printf error >&2"
    (fun workflow output ->
      assert (workflow.status = "success");
      assert (output "stdout" = "one\r\n\rprogress");
      assert (output "stderr" = "error"));
  log_job "unicode"
    "printf '\\342'; sleep 0.05; printf '\\202\\254'; printf '\\377\\360\\237' \
     >&2" (fun workflow output ->
      assert (workflow.status = "success");
      assert (output "stdout" = "€");
      assert (output "stderr" = "��"));
  log_job "limit" "head -c 1100000 /dev/zero | tr '\\000' x" (fun workflow _ ->
      assert (workflow.status = "failed");
      assert (workflow.log_bytes <= 1024 * 1024));
  (* Hold the command open after writing an unterminated line. An independent
     reader must find it both in the live stream and in SQLite before exit. *)
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      let release = name ^ "/release-output" in
      let workflow =
        Runner.v
          (Spindle.Job.v "output"
             [
               Command
                 [
                   "sh";
                   "-c";
                   "printf durable-partial; while [ ! -e \"$1\" ]; do sleep \
                    0.05; done";
                   "partial-test";
                   release;
                 ];
             ])
      in
      let input : Runner.input =
        {
          id = "partial";
          repo = "did:web:repo.test";
          source = name;
          commit;
          metadata = J.obj [];
        }
      in
      let record event =
        Store.append_log store ~pipeline:input.id ~workflow:workflow.job.name
          event
      in
      let persist () =
        Store.batch
          ~logs:[ (input.id, workflow.job.name, workflow.log_seq) ]
          store ~deletes:[]
          ~puts:[ ("pipeline", input.id, J.encode (Runner.snapshot workflow)) ]
      in
      let contains events =
        List.exists
          (fun event ->
            J.field "content" event = Some (J.str "durable-partial"))
          events
      in
      Eio.Fiber.both
        (fun () -> Runner.execute runner input ~record ~persist workflow)
        (fun () ->
          let rec wait () =
            if not (contains workflow.events) then (
              Eio.Time.sleep env#clock 0.01;
              wait ())
          in
          wait ();
          assert (workflow.status = "running");
          Eio.Switch.run (fun reader_sw ->
              let reopened = Store.open_ ~sw:reader_sw directory in
              let events =
                Store.logs reopened ~pipeline:input.id
                  ~workflow:workflow.job.name
                |> List.map (fun (_, raw) -> J.decode raw)
              in
              assert (contains events);
              let snapshot =
                J.decode (Option.get (Store.get reopened "pipeline" input.id))
              in
              assert (not (contains (J.list (J.required "events" snapshot)))));
          Eio.Path.save ~create:(`Exclusive 0o600)
            Eio.Path.(env#fs / release)
            "");
      assert (workflow.status = "success");
      assert (
        Store.logs store ~pipeline:input.id ~workflow:workflow.job.name = []);
      let restored =
        Runner.restore
          (J.decode (Option.get (Store.get store "pipeline" input.id)))
      in
      assert (contains restored.events));
  (match Patch.revision env "not a gzip stream" with
  | _ -> failwith "invalid gzip was accepted"
  | exception J.Invalid _ -> ());
  let compressed =
    Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
      ~stdin:
        (Eio.Flow.string_source
           ("From " ^ commit ^ " Mon Sep 17 00:00:00 2001\n"))
      [ "gzip"; "-c" ]
  in
  assert (Patch.revision env compressed = commit);
  print_endline
    "runner: bounded streaming output, UTF-8, durable partial logs and child \
     cleanup passed"
