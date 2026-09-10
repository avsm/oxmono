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
  let network =
    Spindle__Network.v ~allow_http:true ~plc:"http://127.0.0.1:1" env
  in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let repo = "did:web:repo.test" in
  let catalog =
    Catalog.v ~store ~network ~owner ~hostname:"spindle.test"
      ~static:
        (Some
           {
             Catalog.did = repo;
             owner;
             rkey = "";
             knot = "";
             source = name ^ "/missing";
           })
  in
  let job = Job.v "push-only" [] ~accepts:(fun c -> c.kind = Job.Push) in
  let engine =
    Engine.v ~store ~catalog ~hostname:"spindle.test" ~jobs:[ Job.inspect; job ]
      ~system:env ~directory ~sw
  in
  Eio.Semaphore.acquire engine.runner.slots;
  Eio.Semaphore.acquire engine.runner.slots;
  let trigger =
    obj
      [
        ("$type", str "sh.tangled.ci.trigger#push");
        ("newSha", str (String.make 40 'a'));
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
        ("$type", str "sh.tangled.ci.trigger#manual");
        ("sha", str (String.make 40 'a'));
      ]
  in
  let id =
    Engine.create engine ~automatic:false ~actor:owner
      (obj [ ("repo", str repo); ("trigger", trigger) ])
    |> Option.get
  in
  let pipeline = Engine.find engine id |> Option.get in
  assert (List.length pipeline.workflows = 1);
  Engine.cancel engine ~actor:owner ~repo ~id ~names:[];
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
           let trigger = if i mod 2 = 0 then "push" else "manual" in
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
                        [ ("$type", str ("sh.tangled.ci.trigger#" ^ trigger)) ]
                    );
                  ]) )));
  let query ?cursor ?(commits = []) ?(kinds = []) () =
    Engine.query engine ~repo ~limit:17 ~cursor ~kinds ~commits
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
  let manual = query ~commits:[ sha 42 ] ~kinds:[ "manual" ] () in
  assert (get "id" (List.hd (list (required "pipelines" manual))) = id 127);
  print_endline
    "events: durable dispatch deduplication, OCaml selection and cancellation \
     passed"
