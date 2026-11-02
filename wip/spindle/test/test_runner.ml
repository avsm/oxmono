(* SPDX-License-Identifier: ISC *)
module Runner = Spindle__Runner
module Store = Spindle__Store
module Executor = Spindle.Executor
module Job = Spindle.Job
module J = Spindle__Json
module Mask = Spindle__Mask

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
  let runner ?(timeout = 60.) executor : Runner.env =
    { system = env; directory; slots = Eio.Semaphore.make 1; timeout; executor }
  in
  let input ?(secrets = []) id : Runner.input =
    {
      id;
      repo = "did:web:repo.test";
      source = "https://knot.test/repo";
      commit = String.make 40 'a';
      metadata = J.obj [ ("actor", J.str "did:web:actor.test") ];
      variables = [ ("CI", "true") ];
      secrets;
    }
  in
  (* Each step's end event, as (step, kind, outcome). *)
  let ends (workflow : Runner.t) =
    List.rev workflow.events
    |> List.filter_map (fun event ->
        if J.get "type" event = "control" && J.get "status" event = "end" then
          Some
            ( int_of_float (J.number (J.required "step" event)),
              J.get "kind" event,
              J.get "outcome" event )
        else None)
  in
  let output ?(step = 0) ?(stream = "stdout") (workflow : Runner.t) =
    List.rev workflow.events
    |> List.filter_map (fun event ->
        if
          J.get "type" event = "data"
          && J.number (J.required "step" event) = float_of_int step
          && J.get "stream" event = stream
        then Some (J.get "content" event)
        else None)
    |> String.concat ""
  in
  let run ?timeout ?secrets ?(job = Job.v "w" []) executor =
    let workflow = Runner.v job in
    Runner.execute (runner ?timeout executor)
      (input ?secrets "3jzfcijpj2z2a")
      ~record:(fun _ -> 0L)
      ~persist:(fun () -> ())
      workflow;
    workflow
  in
  (* Steps, their outcomes and both streams. *)
  let w =
    run (fun ctx _ ->
        ctx.step "one" (fun () -> ctx.log `Stdout "out");
        ctx.step "two" (fun () -> ctx.log `Stderr "err"))
  in
  assert (w.status = "success");
  assert (ends w = [ (0, "user", "success"); (1, "user", "success") ]);
  assert (output w = "out" && output ~step:1 ~stream:"stderr" w = "err");
  (* A step that raises fails the workflow and is marked failed. *)
  let w =
    run (fun ctx _ ->
        ctx.step "one" ignore;
        ctx.step "two" (fun () -> failwith "broken"))
  in
  assert (w.status = "failed");
  assert (ends w = [ (0, "user", "success"); (1, "user", "failed") ]);
  (* Logging outside a step is refused. *)
  let w = run (fun ctx _ -> ctx.log `Stdout "stray") in
  assert (w.status = "failed" && w.events = []);
  (* Malformed UTF-8 is replaced. *)
  let w = run (fun ctx _ -> ctx.step "s" (fun () -> ctx.log `Stdout "\xff€")) in
  assert (output w = "\xef\xbf\xbd€");
  (* The log is bounded. *)
  let w =
    run (fun ctx _ ->
        ctx.step "s" (fun () ->
            for _ = 1 to 20 do
              ctx.log `Stdout (String.make 65536 'x')
            done))
  in
  assert (w.status = "failed" && w.log_bytes <= 1024 * 1024);
  (* The time limit stops a workflow. *)
  let w =
    run ~timeout:0.05 (fun ctx _ ->
        ctx.step "s" (fun () -> Eio.Time.sleep env#clock 5.))
  in
  assert (w.status = "timeout");
  assert (ends w = [ (0, "user", "failed") ]);
  (* Cancellation stops a running executor. *)
  let started, started_r = Eio.Promise.create () in
  let workflow = Runner.v (Job.v "w" []) in
  Eio.Fiber.both
    (fun () ->
      Runner.execute
        (runner (fun ctx _ ->
             ctx.step "s" (fun () ->
                 Eio.Promise.resolve started_r ();
                 Eio.Time.sleep env#clock 5.)))
        (input "cancel")
        ~record:(fun _ -> 0L)
        ~persist:(fun () -> ())
        workflow)
    (fun () ->
      Eio.Promise.await started;
      Runner.cancel ~persist:(fun () -> ()) workflow);
  assert (workflow.status = "cancelled");
  assert (ends workflow = [ (0, "user", "failed") ]);
  (* Secret values and their base64 forms are hidden, even split across
     calls, and text held back at the end of a step is kept. *)
  let w =
    run
      ~secrets:[ ("TOKEN", "hunter2") ]
      (fun ctx _ ->
        ctx.step "s" (fun () ->
            List.iter (ctx.log `Stdout)
              [ "pass=hun"; "ter2 "; Mask.base64 "hunter2"; " hunt" ]))
  in
  assert (output w = "pass=*** *** hunt");
  (* The executor receives the workflow and its pipeline. *)
  let yaml = "engine: nixery\nsteps: []\n" in
  let job =
    Job.workflow
      ~env:[ ("LEVEL", "workflow") ]
      ~clone:false ~definition:(J.obj []) ~yaml "build.yml"
      [ Job.step ~env:[ ("LEVEL", "step") ] "build" "make" ]
  in
  let seen = ref None in
  let w =
    run ~job ~secrets:[ ("TOKEN", "hunter2") ] (fun _ r -> seen := Some r)
  in
  assert (w.status = "success");
  let r = Option.get !seen in
  assert (r.pipeline = "3jzfcijpj2z2a" && r.workflow = "build.yml");
  assert (r.source = "https://knot.test/repo" && not r.clone);
  assert (r.yaml = Some yaml && r.env = [ ("LEVEL", "workflow") ]);
  assert (r.variables = [ ("CI", "true") ]);
  assert (r.secrets = [ ("TOKEN", "hunter2") ]);
  assert (r.steps = job.steps);
  let restored = Runner.restore (Runner.snapshot w) in
  assert (restored.job.steps = job.steps && restored.job.env = job.env);
  assert (restored.job.yaml = Some yaml && not restored.job.clone);
  (* The default executor describes the request with placeholders for
     secrets and logs each step without running it. *)
  let w =
    run ~job
      ~secrets:[ ("TOKEN", "hunter2") ]
      (Executor.print ~delay:0. env#clock)
  in
  assert (w.status = "success");
  assert (ends w = [ (0, "user", "success"); (1, "user", "success") ]);
  let described = output w in
  let contains s =
    let n = String.length s in
    let rec at i =
      i + n <= String.length described
      && (String.sub described i n = s || at (i + 1))
    in
    at 0
  in
  assert (contains "TOKEN=<secret>" && not (contains "hunter2"));
  assert (contains "  engine: nixery" && contains "LEVEL=workflow");
  assert (output ~step:1 w = "Command\n  make\nEnvironment\n  LEVEL=step\n");
  (* An unterminated line is in the live stream and in SQLite before the
     workflow finishes, but not in the snapshot. *)
  Eio.Switch.run (fun sw ->
      let store = Store.open_ ~sw directory in
      let written, written_r = Eio.Promise.create () in
      let release, release_r = Eio.Promise.create () in
      let workflow = Runner.v (Job.v "output" []) in
      let input = input "partial" in
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
      let executor (ctx : Executor.context) _ =
        ctx.step "s" (fun () ->
            ctx.log `Stdout "durable-partial";
            Eio.Promise.resolve written_r ();
            Eio.Promise.await release)
      in
      Eio.Fiber.both
        (fun () ->
          Runner.execute (runner executor) input ~record ~persist workflow)
        (fun () ->
          Eio.Promise.await written;
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
          Eio.Promise.resolve release_r ());
      assert (workflow.status = "success");
      assert (
        Store.logs store ~pipeline:input.id ~workflow:workflow.job.name = []);
      let restored =
        Runner.restore
          (J.decode (Option.get (Store.get store "pipeline" input.id)))
      in
      assert (contains restored.events));
  (* Snapshots from before the executor still restore. *)
  let old =
    J.obj
      [
        ( "workflow",
          J.obj [ ("name", J.str "inspect"); ("status", J.str "success") ] );
        ("events", J.arr []);
        ( "steps",
          J.arr
            [
              J.obj [ ("metadata", J.bool true) ];
              J.obj [ ("argv", J.arr [ J.str "ls"; J.str "-la" ]) ];
            ] );
      ]
  in
  assert (
    List.map
      (fun (s : Job.step) -> (s.name, s.command))
      (Runner.restore old).job.steps
    = [ ("spindle request metadata", ""); ("ls -la", "ls -la") ]);
  print_endline
    "runner: executor steps, outcomes, cancellation, time limit, bounded and \
     durable logs, secret masking and the default executor passed"
