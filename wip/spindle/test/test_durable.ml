(* SPDX-License-Identifier: ISC *)
(* A durable engine survives a shutdown: the interrupted workflow runs again
   with a fresh log once its lease runs out, and what it spawned before is
   not spawned twice. *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Engine = Spindle__Engine
module Runner = Spindle__Runner
module Job = Spindle.Job
open J

let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa"
let repo = "did:web:repo.test"

(* How many times each workflow's executor has started. *)
let runs = Hashtbl.create 4
let count name = Option.value ~default:0 (Hashtbl.find_opt runs name)

let () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 60. @@ fun () ->
  let name = Filename.temp_file "spindle-durable-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  let directory = Eio.Path.(env#fs / name) in
  Fun.protect ~finally:(fun () ->
      Eio.Process.run env#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  let started, set_started = Eio.Promise.create () in
  (* The first run spawns a child and waits forever. Later runs spawn it
     again and finish. *)
  let executor (ctx : Spindle.Executor.context) (r : Spindle.Executor.request) =
    Hashtbl.replace runs r.workflow (count r.workflow + 1);
    match r.workflow with
    | "inspect" ->
        ctx.step "work" (fun () ->
            ctx.log `Stdout (Printf.sprintf "run %d\n" (count "inspect")));
        ctx.spawn "child" [ Job.step "child step" "true" ];
        if count "inspect" = 1 then (
          Eio.Promise.resolve set_started ();
          Eio.Fiber.await_cancel ())
    | _ -> ()
  in
  let engine sw =
    let store = Store.open_ ~sw directory in
    let settings =
      Spindle.Settings.v ~store
        ~now:(fun () -> 0.)
        ~defaults:{ repo = []; user = [] }
    in
    let network =
      Spindle__Network.v ~allow_http:true ~plc:"http://127.0.0.1:1" env
    in
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
    let durable =
      Duraml_sqlite.open_ ~sw ~clock:env#clock ~random:env#secure_random
        Eio.Path.(directory / "jobs.sqlite")
    in
    let engine =
      Engine.v ~store ~catalog ~settings ~hostname:"spindle.test"
        ~jobs:[ Job.inspect ] ~durable ~executor ~concurrency:4 ~system:env
        ~directory ~sw ()
    in
    Engine.load engine;
    Engine.serve ~lease:1. engine;
    (engine, store)
  in
  (* The first spindle starts the workflow, then stops while it runs. *)
  let id = ref "" in
  (try
     Eio.Switch.run @@ fun sw ->
     let engine, _ = engine sw in
     let trigger =
       obj
         [
           ("$type", str "org.tangled.ci.trigger.manual");
           ("commit", str (String.make 40 'a'));
         ]
     in
     id :=
       Engine.create engine ~automatic:false ~actor:owner
         (obj [ ("repo", str repo); ("trigger", trigger) ])
       |> Option.get;
     Eio.Promise.await started;
     (* Let the child finish before stopping. *)
     let rec settle n =
       let p = Option.get (Engine.find engine !id) in
       match
         List.find_opt (fun (r : Runner.t) -> r.job.name = "child") p.workflows
       with
       | Some c when Runner.terminal c -> ()
       | _ when n = 0 -> failwith "the child never finished"
       | _ ->
           Eio.Time.sleep env#clock 0.05;
           settle (n - 1)
     in
     settle 200;
     Eio.Switch.fail sw Exit
   with Exit -> ());
  assert (count "inspect" = 1 && count "child" = 1);
  (* The second spindle runs it again from the start. *)
  try
    Eio.Switch.run @@ fun sw ->
    let engine, store = engine sw in
    let rec finished n =
      match Engine.find engine !id with
      | Some p
        when List.for_all Runner.terminal p.workflows
             && List.length p.workflows = 2 ->
          p
      | _ when n = 0 -> failwith "the workflow did not run again"
      | _ ->
          Eio.Time.sleep env#clock 0.1;
          finished (n - 1)
    in
    let p = finished 300 in
    let inspect =
      List.find (fun (r : Runner.t) -> r.job.name = "inspect") p.workflows
    in
    assert (inspect.status = "success");
    assert (count "inspect" = 2);
    (* The child was spawned once, and its earlier run stands. *)
    assert (count "child" = 1);
    (* The log is the second run's alone. *)
    ignore store;
    let log = (Runner.outcome inspect).log in
    let has sub =
      let n = String.length sub in
      let rec at i =
        i + n <= String.length log && (String.sub log i n = sub || at (i + 1))
      in
      at 0
    in
    assert (has "run 2" && not (has "run 1"));
    Eio.Switch.fail sw Exit
  with Exit -> ()

let () =
  print_endline
    "durable: an interrupted workflow runs again with a fresh log and spawns \
     nothing twice passed"
