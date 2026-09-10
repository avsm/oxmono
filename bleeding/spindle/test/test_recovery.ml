(* SPDX-License-Identifier: ISC *)
module J = Spindle__Json
module Store = Spindle__Store
module Catalog = Spindle__Catalog
module Engine = Spindle__Engine
module Recovery = Spindle__Recovery
module Observer = Spindle__Observer
open J

let () =
  Eio_main.run @@ fun system ->
  Eio.Time.with_timeout_exn system#clock 20. @@ fun () ->
  let name = Filename.temp_file "spindle-recovery-" "" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  Fun.protect ~finally:(fun () ->
      Eio.Process.run system#process_mgr [ "rm"; "-rf"; "--"; name ])
  @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let directory = Eio.Path.(system#fs / name) in
  let git args =
    Eio.Process.run system#process_mgr ~cwd:directory
      ("git" :: "-c" :: "user.name=Test" :: "-c"
     :: "user.email=test@example.test" :: args)
  in
  git [ "init"; "-q"; "--initial-branch=main" ];
  git [ "commit"; "-q"; "--allow-empty"; "-m"; "fixture" ];
  git [ "tag"; "-a"; "v1"; "-m"; "annotated tag" ];
  git [ "branch"; "live" ];
  git [ "notes"; "add"; "-m"; "non-CI ref" ];
  let object_ =
    Eio.Process.parse_out system#process_mgr Eio.Buf_read.line ~cwd:directory
      [ "git"; "rev-parse"; "v1" ]
  in
  let store = Store.open_ ~sw directory in
  let network =
    Spindle__Network.v ~allow_http:true ~plc:"http://127.0.0.1:1" system
  in
  let owner = "did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" in
  let repo = "did:web:repo.test" in
  let catalog =
    Catalog.v ~store ~network ~owner ~hostname:"spindle.test"
      ~static:
        (Some { Catalog.did = repo; owner; rkey = ""; knot = ""; source = name })
  in
  let engine =
    Engine.v ~store ~catalog ~hostname:"spindle.test"
      ~jobs:[ Spindle.Job.inspect ] ~system ~directory ~sw
  in
  Eio.Semaphore.acquire engine.runner.slots;
  Eio.Semaphore.acquire engine.runner.slots;
  let ref_ = "refs/tags/v1" in
  let request =
    obj
      [
        ("repo", str repo);
        ( "trigger",
          obj
            [
              ("$type", str "sh.tangled.ci.trigger#push");
              ("newSha", str object_);
              ("oldSha", str (String.make 40 '0'));
              ("ref", str ref_);
            ] );
      ]
  in
  let id =
    Option.get
      (Engine.create engine
         ~dedup:(Recovery.dedup ~repo ~ref_ ~sha:object_)
         ~automatic:true ~actor:owner request)
  in
  Store.checkpoint_ref store ~repo ~ref_ ~sha:object_ ~position:1L;
  (* Neither an unrelated PDS outage nor an indefinitely busy inbox can stop
     this repository's ref recovery. Keep both tasks pending throughout. *)
  Store.schedule store "reconcile" "unavailable-member/sh.tangled.repo";
  Store.enqueue store ~source:"unrelated-knot" ~cursor:"1" ~key:"busy"
    ~value:"{}";
  Store.schedule store "recover" repo;
  Eio.Fiber.both
    (fun () -> Observer.recover_once ~engine ~network)
    (fun () ->
      Eio.Time.sleep system#clock 0.5;
      let sha =
        Eio.Process.parse_out system#process_mgr Eio.Buf_read.line
          ~cwd:directory
          [ "git"; "rev-parse"; "main" ]
      in
      Store.checkpoint_ref store ~repo ~ref_:"refs/heads/live" ~sha
        ~position:(Int64.of_float (Eio.Time.now system#clock *. 1e9)));
  assert (Store.get store "recover" repo = None);
  assert (
    Store.get store "reconcile" "unavailable-member/sh.tangled.repo" <> None);
  assert (Store.get store "inbox" "busy" <> None);
  assert (Store.ref_state store ~repo ~ref_:"refs/heads/main" <> None);
  let tag_runs =
    Store.list store "pipeline-view"
    |> List.filter (fun (_, raw) ->
        get "ref" (required "trigger" (decode raw)) = ref_)
  in
  assert (List.map fst tag_runs = [ id ]);
  assert (
    List.for_all
      (fun (_, raw) ->
        get "ref" (required "trigger" (decode raw)) <> "refs/heads/live")
      (Store.list store "pipeline-view"));
  Store.schedule store "recover" repo;
  Observer.recover_once ~engine ~network;
  assert (List.length (Store.list store "pipeline-view") = 2);
  Store.list store "pipeline-view"
  |> List.iter (fun (id, _) ->
      Engine.cancel engine ~actor:owner ~repo ~id ~names:[]);
  Eio.Semaphore.release engine.runner.slots;
  Eio.Semaphore.release engine.runner.slots;
  print_endline "recovery: notes, annotated tags and independent queues passed"
