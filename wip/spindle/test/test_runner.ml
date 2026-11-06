(* SPDX-License-Identifier: ISC *)
module Runner = Spindle__Runner
module Store = Spindle__Store
module Executor = Spindle.Executor
module Job = Spindle.Job
module J = Spindle__Json
module Mask = Spindle__Mask

let () =
  Eio_main.run @@ fun env ->
  (* A gate gives up only what it holds: a wait whose taking back is
     cancelled leaves the resource given up, and the next wait does not give
     it up again. *)
  (let released = ref 0 and acquired = ref 0 and refuse = ref true in
   let gate =
     Spindle__Gate.v
       ~release:(fun () -> incr released)
       ~acquire:(fun () ->
         if !refuse then (
           refuse := false;
           raise (Eio.Cancel.Cancelled Exit))
         else incr acquired)
   in
   (match Spindle__Gate.run gate (fun () -> failwith "wait failed") with
   | () -> assert false
   | exception Failure _ -> ());
   assert (!released = 1 && !acquired = 0);
   Spindle__Gate.run gate (fun () -> ());
   assert (!released = 1 && !acquired = 1);
   Spindle__Gate.run gate (fun () -> ());
   assert (!released = 2 && !acquired = 2));
  Eio.Time.with_timeout_exn env#clock 30. @@ fun () ->
  Testkit.with_dir env "spindle-runner-" @@ fun name directory ->
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
      files =
        {
          list = (fun dir -> if dir = "" then [ "demo.opam" ] else []);
          read =
            (fun path ->
              if path = "demo.opam" then Some "opam-version: \"2.0\"" else None);
        };
      labels = (fun () -> []);
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
      ~spawn:(fun ~env:_ _ _ -> ())
      workflow;
    workflow
  in
  (* A workflow waiting in a builder's queue is pending, without a start
     time, and starts when it leaves the queue. *)
  let workflow = Runner.v (Job.v "w" []) in
  let seen = ref [] and queued = ref (-1.) in
  Runner.execute
    (runner (fun ctx _ ->
         ctx.queued (fun () -> ());
         ctx.step "build" (fun () -> ())))
    (input "3jzfcijpj2z2a")
    ~record:(fun _ -> 0L)
    ~persist:(fun () ->
      seen := (workflow.status, workflow.started <> None) :: !seen)
    ~spawn:(fun ~env:_ _ _ -> ())
    ~queued_for:(fun s -> queued := s)
    workflow;
  assert (workflow.status = `Success && !queued >= 0.);
  (match List.rev !seen with
  | (`Running, true) :: (`Pending, false) :: (`Running, true) :: _ -> ()
  | _ -> assert false);
  (* Steps, their outcomes and both streams. *)
  let w =
    run (fun ctx _ ->
        ctx.step "one" (fun () -> ctx.log `Stdout "out");
        ctx.step "two" (fun () -> ctx.log `Stderr "err"))
  in
  assert (w.status = `Success);
  assert (ends w = [ (0, "user", "success"); (1, "user", "success") ]);
  assert (output w = "out" && output ~step:1 ~stream:"stderr" w = "err");
  (* A step that raises fails the workflow and is marked failed. *)
  let w =
    run (fun ctx _ ->
        ctx.step "one" ignore;
        ctx.step "two" (fun () -> failwith "broken"))
  in
  assert (w.status = `Failed && w.error = Some "broken");
  assert (ends w = [ (0, "user", "success"); (1, "user", "failed") ]);
  (* The log has one line per frame. An unterminated line waits for its
     newline or the end of the step, and a long one is split between UTF-8
     characters. *)
  let frames ?(step = 0) (workflow : Runner.t) =
    List.rev workflow.events
    |> List.filter_map (fun event ->
        if
          J.get "type" event = "data"
          && J.number (J.required "step" event) = float_of_int step
        then Some (J.get "content" event)
        else None)
  in
  let w =
    run (fun ctx _ ->
        ctx.step "s" (fun () ->
            List.iter (ctx.log `Stdout) [ "a\nb"; "c\n\nd" ]))
  in
  assert (frames w = [ "a\n"; "bc\n"; "\n"; "d" ]);
  let long = String.make 16383 'x' ^ "€" in
  let w = run (fun ctx _ -> ctx.step "s" (fun () -> ctx.log `Stdout long)) in
  assert (frames w = [ String.make 16383 'x'; "€" ]);
  (* A step may carry its command and be marked as the executor's own. *)
  let starts (workflow : Runner.t) =
    List.rev workflow.events
    |> List.filter_map (fun event ->
        if J.get "type" event = "control" && J.get "status" event = "start" then
          Some
            ( J.get "content" event,
              Option.map J.string (J.field "command" event),
              J.get "kind" event )
        else None)
  in
  let w =
    run (fun ctx _ ->
        ctx.step ~kind:`System "setup" ignore;
        ctx.step ~command:"make" "build" ignore)
  in
  assert (
    starts w = [ ("setup", None, "system"); ("build", Some "make", "user") ]);
  (* Logging outside a step is refused. *)
  let w = run (fun ctx _ -> ctx.log `Stdout "stray") in
  assert (w.status = `Failed && w.events = []);
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
  assert (w.status = `Failed && w.log_bytes <= 1024 * 1024);
  (* The time limit stops a workflow. *)
  let w =
    run ~timeout:0.05 (fun ctx _ ->
        ctx.step "s" (fun () -> Eio.Time.sleep env#clock 5.))
  in
  assert (w.status = `Timeout && w.error = None);
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
        ~spawn:(fun ~env:_ _ _ -> ())
        workflow)
    (fun () ->
      Eio.Promise.await started;
      Runner.cancel ~persist:(fun () -> ()) workflow);
  assert (workflow.status = `Cancelled);
  assert (workflow.error = Some "User canceled the workflow");
  assert (ends workflow = [ (0, "user", "failed") ]);
  (* Secret values and their base64 forms are hidden, even split across
     calls, and text held back at the end of a step is kept. *)
  let w =
    run
      ~secrets:[ ("TOKEN", "hunter2") ]
      (fun ctx _ ->
        ctx.step "s" (fun () ->
            List.iter (ctx.log `Stdout)
              [ "pass=hun"; "ter2 "; Base64.encode_string "hunter2"; " hunt" ]))
  in
  assert (output w = "pass=*** *** hunt");
  (* Secrets are hidden in step names, commands and errors too. *)
  let w =
    run
      ~secrets:[ ("TOKEN", "hunter2") ]
      (fun ctx _ ->
        ctx.step ~command:"login hunter2" "use hunter2" (fun () -> ());
        failwith "bad token hunter2")
  in
  let log =
    J.encode (Runner.view w) ^ String.concat "" (List.map J.encode w.events)
  in
  let has = Testkit.has in
  assert (not (has log "hunter2"));
  assert (w.error = Some "bad token ***");
  (* A workflow stopped while it waits in a builder's queue was running. *)
  let w = Runner.v (Job.v "q" []) in
  w.status <- `Pending;
  w.queued <- true;
  let raw = Runner.snapshot w in
  assert (Runner.interrupted raw);
  assert ((Runner.restore raw).status = `Failed);
  assert ((Runner.restore ~resume:true raw).status = `Pending);
  w.queued <- false;
  assert (not (Runner.interrupted (Runner.snapshot w)));
  (* A line of exactly 16 KiB is one line. *)
  let line = String.make 16384 'x' ^ "\n" in
  let w = run (fun ctx _ -> ctx.step "s" (fun () -> ctx.log `Stdout line)) in
  assert (output w = line);
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
  assert (w.status = `Success);
  let r = Option.get !seen in
  assert (r.pipeline = "3jzfcijpj2z2a" && r.workflow = "build.yml");
  assert (r.source = "https://knot.test/repo" && not r.clone);
  assert (r.yaml = Some yaml && r.env = [ ("LEVEL", "workflow") ]);
  assert (r.variables = [ ("CI", "true") ]);
  assert (r.secrets = [ ("TOKEN", "hunter2") ]);
  assert (r.steps = job.steps);
  assert (r.files.list "" = [ "demo.opam" ] && r.files.read "missing" = None);
  let restored = Runner.restore (Runner.snapshot w) in
  assert (restored.job.steps = job.steps && restored.job.env = job.env);
  assert (restored.job.yaml = Some yaml && not restored.job.clone);
  (* The default executor describes the request with placeholders for
     secrets and logs each step without running it. *)
  let w =
    run ~job
      ~secrets:[ ("TOKEN", "hunter2") ]
      (Spindle.Demo.print ~delay:0. env#clock)
  in
  assert (w.status = `Success);
  assert (ends w = [ (0, "system", "success"); (1, "user", "success") ]);
  assert (
    starts w
    = [
        ("describe the request", None, "system"); ("build", Some "make", "user");
      ]);
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
  assert (output ~step:1 w = "Environment\n  LEVEL=step\n");
  (* The simulated planner solves, then spawns a build for each compiler,
     which reports made-up output for its steps. *)
  let spawned = ref [] in
  let workflow = Runner.v (Job.v "solve" []) in
  Runner.execute
    (runner
       (Spindle.Demo.solve ~compilers:[ "ocaml-4.14"; "ocaml-5.5" ] env#clock))
    (input "solve")
    ~record:(fun _ -> 0L)
    ~persist:(fun () -> ())
    ~spawn:(fun ~env name steps -> spawned := (name, env, steps) :: !spawned)
    workflow;
  assert (workflow.status = `Success);
  assert (List.map (fun (n, _, _) -> n) !spawned = [ "ocaml-5.5"; "ocaml-4.14" ]);
  let _, compiler_env, steps = List.hd !spawned in
  let build = Runner.v (Job.spawned ~env:compiler_env "ocaml-5.5" steps) in
  Runner.execute
    (runner (Spindle.Demo.solve env#clock))
    (input "build")
    ~record:(fun _ -> 0L)
    ~persist:(fun () -> ())
    ~spawn:(fun ~env:_ _ _ -> failwith "a build spawned")
    build;
  assert (build.status = `Success);
  assert (ends build = List.map (fun i -> (i, "user", "success")) [ 0; 1; 2 ]);
  let restored = Runner.restore (Runner.snapshot build) in
  assert (restored.job.spawned && restored.job.env = compiler_env);
  (* A line is in the live stream and in SQLite before the workflow finishes,
     but not in the snapshot. *)
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
        Store.batch store ~deletes:[]
          ~puts:[ ("pipeline", input.id, J.encode (Runner.snapshot workflow)) ]
      in
      let contains events =
        List.exists
          (fun event ->
            J.field "content" event = Some (J.str "durable-partial\n"))
          events
      in
      let executor (ctx : Executor.context) _ =
        ctx.step "s" (fun () ->
            ctx.log `Stdout "durable-partial\n";
            Eio.Promise.resolve written_r ();
            Eio.Promise.await release)
      in
      Eio.Fiber.both
        (fun () ->
          Runner.execute (runner executor) input ~record ~persist
            ~spawn:(fun ~env:_ _ _ -> ())
            workflow)
        (fun () ->
          Eio.Promise.await written;
          assert (workflow.status = `Running);
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
              assert (J.field "events" snapshot = None));
          Eio.Promise.resolve release_r ());
      assert (workflow.status = `Success);
      (* The log stays in the journal, out of the snapshot. *)
      assert (
        Store.logs store ~pipeline:input.id ~workflow:workflow.job.name
        |> List.map (fun (_, raw) -> J.decode raw)
        |> contains);
      let restored =
        Runner.restore
          (J.decode (Option.get (Store.get store "pipeline" input.id)))
      in
      assert (restored.events = []));
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
  (* Events a snapshot holds from before the journal kept every event stay
     in it, since the journal does not have them. *)
  let event = J.obj [ ("type", J.str "control") ] in
  let embedded =
    J.obj
      [
        ( "workflow",
          J.obj [ ("name", J.str "kept"); ("status", J.str "success") ] );
        ("events", J.arr [ event ]);
        ("steps", J.arr []);
      ]
  in
  let restored = Runner.restore embedded in
  assert (restored.events = [ event ]);
  assert (
    J.field "events" (Runner.snapshot restored) = Some (J.arr [ event ]));
  print_endline
    "runner: executor steps, outcomes, cancellation, time limit, bounded and \
     durable logs, secret masking and the default and solving executors passed"
