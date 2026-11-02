(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Engine = Spindle__Engine
module Job = Spindle.Job
open J

let () =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "spindle-events-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  let directory = Eio.Path.(env#fs / name) in
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store = Store.open_ ~sw directory in
  let settings =
    Spindle.Settings.v ~store
      ~now:(fun () -> 0.)
      ~defaults:{ repo = []; user = [] }
  in
  let network =
    Spindle__Network.v ~allow_http:true ~plc:"http://127.0.0.1:1" env
  in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let repo = "did:web:repo.test" in
  let catalog =
    Catalog.v ~store ~settings ~network ~owner ~hostname:"spindle.test"
      ~admission:Catalog.Closed ~ttl:60.
      ~now:(fun () -> Eio.Time.now env#clock)
      ~static:
        (Some
           {
             Catalog.did = repo;
             owner;
             name = "";
             knot = "";
             source = name ^ "/missing";
             created = "";
           })
      ()
  in
  let job = Job.v "push-only" [] ~accepts:(fun c -> c.kind = Job.Push) in
  let engine =
    Engine.v ~store ~catalog ~settings ~hostname:"spindle.test"
      ~jobs:[ Job.inspect; job ]
      ~executor:(fun _ _ -> ())
      ~system:env ~directory ~sw ()
  in
  Eio.Semaphore.acquire engine.runner.slots;
  Eio.Semaphore.acquire engine.runner.slots;
  let trigger =
    obj
      [
        ("$type", str "org.tangled.event.push");
        ("repo", str repo);
        ("newSha", str (String.make 64 'a'));
        ("oldSha", str (String.make 40 '0'));
        ("ref", str "refs/heads/main");
      ]
  in
  let request = obj [ ("repo", str repo); ("trigger", trigger) ] in
  let first =
    Engine.create engine ~dedup:"event-1" ~automatic:true ~actor:owner request
  in
  let second =
    Engine.create engine ~dedup:"event-1" ~automatic:true ~actor:owner request
  in
  assert (first = second && Option.is_some first);
  assert (Hashtbl.length engine.pipelines = 1);
  let pipeline = Option.get (Engine.find engine (Option.get first)) in
  assert (List.length pipeline.workflows = 2);
  assert (
    List.length (Engine.select_workflows pipeline [ "inspect"; "inspect" ]) = 1);
  Engine.cancel engine ~actor:owner ~repo ~id:pipeline.id ~names:[];
  assert (
    List.for_all (fun (r : Spindle__Runner.t) -> r.cancelled) pipeline.workflows);
  let trigger =
    obj
      [
        ("$type", str "org.tangled.ci.trigger.manual");
        ("commit", str (String.make 40 'a'));
      ]
  in
  let id =
    Engine.create engine ~automatic:false ~actor:owner
      (obj [ ("repo", str repo); ("trigger", trigger) ])
    |> Option.get
  in
  let pipeline = Engine.find engine id |> Option.get in
  assert (List.length pipeline.workflows = 1);
  let view = Engine.view engine pipeline in
  let run = List.hd (list (required "workflows" view)) in
  assert (get "id" (required "definition" run) = "inspect");
  assert (get "$type" (required "trigger" view) = Engine.manual);
  let retried =
    Engine.retry engine ~actor:owner ~repo ~id ~names:[] |> Option.get
  in
  assert (retried <> id);
  Engine.cancel engine ~actor:owner ~repo ~id:retried ~names:[];
  Engine.cancel engine ~actor:owner ~repo ~id ~names:[];
  (* A blocked pipeline waits for approval and survives as live work. *)
  let blocked =
    Engine.create engine ~blocked:true ~automatic:false ~actor:owner
      (obj [ ("repo", str repo); ("trigger", trigger) ])
    |> Option.get
  in
  let pipeline = Engine.find engine blocked |> Option.get in
  assert (
    List.for_all
      (fun (r : Spindle__Runner.t) -> r.status = "blocked")
      pipeline.workflows);
  assert (Hashtbl.mem engine.pipelines blocked);
  Engine.approve engine ~actor:owner ~repo ~id:blocked;
  assert (
    List.for_all
      (fun (r : Spindle__Runner.t) -> r.status = "pending")
      pipeline.workflows);
  (match Engine.approve engine ~actor:owner ~repo ~id:blocked with
  | () -> failwith "an unblocked pipeline was approved"
  | exception J.Invalid _ -> ());
  Engine.cancel engine ~actor:owner ~repo ~id:blocked ~names:[];
  Eio.Semaphore.release engine.runner.slots;
  Eio.Semaphore.release engine.runner.slots;
  Store.batch store ~puts:[]
    ~deletes:
      (List.map
         (fun (id, _) -> ("pipeline-view", id))
         (Store.list store "pipeline-view"));
  let id i =
    Atp.Tid.to_string
      (Atp.Tid.of_timestamp_us ~clockid:0 (Int64.of_int (1000000 + i)))
  in
  let sha i = Printf.sprintf "%040x" i in
  Store.batch store ~deletes:[]
    ~puts:
      (List.init 300 (fun i ->
           let trigger =
             if i mod 2 = 0 then Engine.push else Engine.manual
           in
           ( "pipeline-view",
             id i,
             encode
               (obj
                  [
                    ("id", str (id i));
                    ("repo", str repo);
                    ("commit", str (sha (i / 3)));
                    ( "trigger",
                      obj
                        [ ("$type", str trigger) ]
                    );
                  ]) )));
  let query ?cursor ?(commits = []) ?(triggers = []) () =
    Engine.query engine ~repo ~limit:17 ~cursor ~triggers ~commits
  in
  let page = query () in
  assert (number (required "total" page) = 300.);
  assert (get "cursor" page = id 283);
  assert (List.length (list (required "pipelines" page)) = 17);
  let page = query ~cursor:(id 283) () in
  assert (number (required "total" page) = 283.);
  assert (get "id" (List.hd (list (required "pipelines" page))) = id 282);
  let latest = query ~commits:[ sha 42 ] () in
  assert (get "id" (List.hd (list (required "pipelines" latest))) = id 128);
  let older = query ~commits:[ sha 42 ] ~cursor:(id 128) () in
  assert (list (required "pipelines" older) = []);
  let manual = query ~commits:[ sha 42 ] ~triggers:[ Engine.manual ] () in
  assert (get "id" (List.hd (list (required "pipelines" manual))) = id 127);
  Eio.Semaphore.acquire engine.runner.slots;
  Eio.Semaphore.acquire engine.runner.slots;
  let request =
    obj
      [
        ("repo", str repo);
        ( "trigger",
          obj
            [
              ("$type", str "org.tangled.ci.trigger.manual");
              ("commit", str (String.make 40 'a'));
            ] );
      ]
  in
  let create () =
    Option.get (Engine.create engine ~automatic:false ~actor:owner request)
  in
  let queued = List.init 32 (fun _ -> create ()) in
  (match create () with
  | _ -> failwith "pipeline capacity was not enforced"
  | exception Engine.Capacity -> ());
  let cancel id = Engine.cancel engine ~actor:owner ~repo ~id ~names:[] in
  cancel (List.hd queued);
  let replacement = create () in
  List.iter cancel (replacement :: List.tl queued);
  Eio.Semaphore.release engine.runner.slots;
  Eio.Semaphore.release engine.runner.slots;
  print_endline
    "events: durable dispatch deduplication, OCaml selection, approval, retry \
     and cancellation passed"
