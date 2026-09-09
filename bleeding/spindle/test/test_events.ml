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
  print_endline
    "events: durable dispatch deduplication, OCaml selection and cancellation \
     passed"
