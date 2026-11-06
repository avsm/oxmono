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
    Spindle__Settings.v ~store
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
  let awaited = ref None and watched = ref [] in
  let engine =
    Engine.v ~store ~catalog ~settings ~hostname:"spindle.test"
      ~jobs:[ Job.inspect; job ]
      ~executor:(fun ctx r ->
        (* A manual run asks for a spawned workflow with an input. *)
        match List.assoc_opt "TANGLED_INPUT_SPAWN" r.variables with
        | Some name when r.workflow = "inspect" ->
            ctx.spawn name [ Job.step "child step" "true" ];
            (* Progress is watched until every descendant has ended. *)
            let rec follow since =
              let now = ctx.spawned ?since () in
              watched := now :: !watched;
              if
                List.exists
                  (fun (p : Spindle.Executor.progress) ->
                    p.status = `Pending || p.status = `Running)
                  now
              then follow (Some now)
            in
            follow None;
            awaited := Some (ctx.await_spawned ())
        | Some _ when r.workflow = "child" ->
            ctx.spawn "grandchild" [ Job.step "grandchild step" "true" ]
        | Some _ when r.workflow = "grandchild" ->
            ctx.step "say" (fun () -> ctx.log `Stdout "hello\n")
        | _ -> ())
      ~system:env ~directory ~sw ()
  in
  Eio.Semaphore.acquire (Engine.slots engine);
  Eio.Semaphore.acquire (Engine.slots engine);
  let trigger =
    obj
      [
        ("$type", str "org.tangled.event.push");
        ("repo", str repo);
        ("newSha", str (String.make 64 'a'));
        ("oldSha", str (String.make 40 '0'));
        ("ref", str "refs/heads/main");
        ("pushOptions", arr [ str "deploy=now" ]);
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
  (* Refs are named as the reference spindle names them. *)
  let ref_ r = List.map snd (Engine.ref_variables r) in
  assert (ref_ "refs/heads/main" = [ "refs/heads/main"; "main"; "branch" ]);
  assert (ref_ "refs/tags/v1" = [ "refs/tags/v1"; "v1"; "tag" ]);
  assert (ref_ "refs/pull/1" = [ "refs/pull/1"; "refs/pull/1"; "branch" ]);
  (* A webhook receipt is recorded once. *)
  assert (Engine.first engine "push-hook" "r/refs/heads/main/a/b");
  assert (not (Engine.first engine "push-hook" "r/refs/heads/main/a/b"));
  assert (List.length (Engine.loaded engine) = 1);
  let pipeline = Option.get (Engine.find engine (Option.get first)) in
  assert (List.length pipeline.workflows = 2);
  (* A view shows the lexicon's trigger fields only. *)
  let shown = required "trigger" (Engine.view engine pipeline) in
  assert (field "pushOptions" shown = None && get "repo" shown = repo);
  assert (get "ref" shown = "refs/heads/main");
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
    Engine.retry engine ~principal:owner ~actor:owner ~repo ~id ~names:[]
    |> Option.get
  in
  assert (retried <> id);
  Engine.cancel engine ~actor:owner ~repo ~id:retried ~names:[];
  (* Access is checked for [principal], and [actor] is recorded, as for a
     repository's token naming a user. *)
  let user = "did:plc:uuuuuuuuuuuuuuuuuuuuuuuu" in
  let by_owner =
    Engine.retry engine ~principal:owner ~actor:user ~repo ~id ~names:[]
    |> Option.get
  in
  let actor id = get "actor" (Option.get (Engine.find engine id)).metadata in
  assert (actor by_owner = user);
  Engine.cancel engine ~actor:owner ~repo ~id:by_owner ~names:[];
  let manual =
    Engine.create engine ~automatic:false ~principal:owner ~actor:user
      (obj [ ("repo", str repo); ("trigger", trigger) ])
    |> Option.get
  in
  assert (actor manual = user);
  Engine.cancel engine ~actor:owner ~repo ~id:manual ~names:[];
  (* A static repository has no knot, so its own DID is no principal. *)
  List.iter
    (fun principal ->
      match
        Engine.create engine ~automatic:false ~principal ~actor:user
          (obj [ ("repo", str repo); ("trigger", trigger) ])
      with
      | _ -> failwith "an unauthorized principal started a pipeline"
      | exception Spindle__Xrpc_error.E (Httpz.Res.Forbidden, "AccessControl", _)
        ->
          ())
    [ user; repo ];
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
      (fun (r : Spindle__Runner.t) -> r.status = `Blocked)
      pipeline.workflows);
  assert (List.mem blocked (Engine.loaded engine));
  Engine.approve engine ~actor:owner ~repo ~id:blocked;
  assert (
    List.for_all
      (fun (r : Spindle__Runner.t) -> r.status = `Pending)
      pipeline.workflows);
  (match Engine.approve engine ~actor:owner ~repo ~id:blocked with
  | () -> failwith "an unblocked pipeline was approved"
  | exception J.Invalid _ -> ());
  Engine.cancel engine ~actor:owner ~repo ~id:blocked ~names:[];
  assert (Engine.find engine "not-a-tid" = None);
  Eio.Semaphore.release (Engine.slots engine);
  Eio.Semaphore.release (Engine.slots engine);
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
           let trigger = if i mod 2 = 0 then Engine.push else Engine.manual in
           ( "pipeline-view",
             id i,
             encode
               (obj
                  [
                    ("id", str (id i));
                    ("repo", str repo);
                    ("commit", str (sha (i / 3)));
                    ("trigger", obj [ ("$type", str trigger) ]);
                  ]) )));
  let query ?cursor ?(commits = []) ?(triggers = []) () =
    Engine.query engine ~repo ~limit:17 ~cursor ~triggers ~commits
  in
  let page = query () in
  assert (number (required "total" page) = 300.);
  assert (get "cursor" page = id 283);
  assert (List.length (list (required "pipelines" page)) = 17);
  (* Every page reports the whole count, as the reference spindle does. *)
  let page = query ~cursor:(id 283) () in
  assert (number (required "total" page) = 300.);
  assert (get "id" (List.hd (list (required "pipelines" page))) = id 282);
  (* Each of a commit's pipelines matches. *)
  let ids page = List.map (get "id") (list (required "pipelines" page)) in
  let latest = query ~commits:[ sha 42 ] () in
  assert (ids latest = [ id 128; id 127; id 126 ]);
  assert (number (required "total" latest) = 3.);
  let older = query ~commits:[ sha 42 ] ~cursor:(id 128) () in
  assert (ids older = [ id 127; id 126 ]);
  let manual = query ~commits:[ sha 42 ] ~triggers:[ Engine.manual ] () in
  assert (get "id" (List.hd (list (required "pipelines" manual))) = id 127);
  Eio.Semaphore.acquire (Engine.slots engine);
  Eio.Semaphore.acquire (Engine.slots engine);
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
  (* A repository's share of the capacity is 8 pipelines with work to do. *)
  let queued = List.init 8 (fun _ -> create ()) in
  (match create () with
  | _ -> failwith "pipeline capacity was not enforced"
  | exception Engine.Capacity -> ());
  let cancel id = Engine.cancel engine ~actor:owner ~repo ~id ~names:[] in
  cancel (List.hd queued);
  let replacement = create () in
  List.iter cancel (replacement :: List.tl queued);
  Eio.Semaphore.release (Engine.slots engine);
  Eio.Semaphore.release (Engine.slots engine);
  (* A workflow may add another to its running pipeline, which runs and is
     shown with it, and wait for it and what it spawns. Waiting gives up the
     only free slot. A retry reruns the workflow that spawned it. *)
  Eio.Semaphore.acquire (Engine.slots engine);
  let spawner =
    obj
      [
        ("$type", str Engine.manual);
        ("commit", str (String.make 40 'a'));
        ("inputs", arr [ obj [ ("key", str "spawn"); ("value", str "child") ] ]);
      ]
  in
  let id =
    Engine.create engine ~automatic:false ~actor:owner
      (obj [ ("repo", str repo); ("trigger", spawner) ])
    |> Option.get
  in
  let rec settle n =
    let p = Option.get (Engine.find engine id) in
    let names =
      List.map (fun (r : Spindle__Runner.t) -> r.job.name) p.workflows
    in
    if
      List.mem "grandchild" names
      && List.for_all
           (fun (r : Spindle__Runner.t) -> r.status = `Success)
           p.workflows
    then p
    else if n = 0 then
      failwith
        ("the spawned workflow did not finish: "
        ^ String.concat ", "
            (List.map
               (fun (r : Spindle__Runner.t) ->
                 r.job.name ^ "="
                 ^ Spindle__Runner.status_to_string r.status
                 ^ " "
                 ^ Option.value ~default:"" r.error)
               p.workflows))
    else (
      Eio.Time.sleep env#clock 0.05;
      settle (n - 1))
  in
  let p = settle 100 in
  Eio.Semaphore.release (Engine.slots engine);
  assert (
    List.map (fun (r : Spindle__Runner.t) -> r.job.name) p.workflows
    = [ "inspect"; "child"; "grandchild" ]);
  let shown =
    List.map (get "name") (list (required "workflows" (Engine.view engine p)))
  in
  assert (shown = [ "inspect"; "child"; "grandchild" ]);
  (match !awaited with
  | Some [ child; grandchild ] ->
      assert (child.workflow = "child" && child.status = `Success);
      assert (grandchild.workflow = "grandchild");
      assert (grandchild.log = "## say\nhello\n")
  | _ -> failwith "the spawned workflows were not awaited");
  (* Each observation after the first differs from the one before, and the
     last shows both descendants ended. *)
  let rec distinct = function
    | a :: (b :: _ as rest) -> a <> b && distinct rest
    | _ -> true
  in
  assert (distinct !watched);
  (match !watched with
  | last :: _ ->
      assert (
        List.map
          (fun (p : Spindle.Executor.progress) ->
            (p.workflow, p.status, p.step))
          last
        = [ ("child", `Success, None); ("grandchild", `Success, None) ])
  | [] -> failwith "no progress was watched");
  let again =
    Engine.retry engine ~principal:owner ~actor:owner ~repo ~id
      ~names:[ "child" ]
    |> Option.get
  in
  (match (Option.get (Engine.find engine again)).workflows with
  | (first : Spindle__Runner.t) :: _ -> assert (first.job.name = "inspect")
  | [] -> assert false);
  (* A workflow spawned by a spawned one is retried through the planned
     workflow it descends from, and only that one runs. *)
  let deeper =
    Engine.retry engine ~principal:owner ~actor:owner ~repo ~id
      ~names:[ "grandchild" ]
    |> Option.get
  in
  assert (
    List.map
      (fun (r : Spindle__Runner.t) -> r.job.name)
      (List.filter
         (fun (r : Spindle__Runner.t) -> not r.job.spawned)
         (Option.get (Engine.find engine deeper)).workflows)
    = [ "inspect" ]);
  print_endline
    "events: durable dispatch deduplication, OCaml selection, approval, retry, \
     cancellation, spawning and awaiting passed"
