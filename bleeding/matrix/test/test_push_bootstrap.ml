module Base = Matrix_client.Base_client
module Client_core = Matrix_client.Client
module Id = Matrix_proto.Id
module Push = Matrix_proto.Push
module Store = Matrix_client.Store
module Service = Matrix_eio.Sync_service
module Eio_error = Matrix_eio.Error

let user = Id.User_id.of_string_exn "@alice:example.org"
let device = Id.Device_id.of_string_exn "BOOTSTRAP"
let homeserver = Uriz.of_string_exn "https://hs.example"

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'p')
  end

let session : Client_core.session =
  {
    user_id = user;
    access_token = "syt_token";
    device_id = device;
    refresh_token = None;
  }

let client ~sw ~env fetch =
  let client = Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () in
  Matrix_eio.Client.with_session client session

let rules_with_id id =
  {
    Push.Ruleset.empty with
    override =
      [ Push.Rule.v ~rule_id:(Push.Rule_id.override id) [ Push.Action.Notify ] ];
  }

let fetched_rules = rules_with_id "fetched"
let synced_rules = rules_with_id "synced"

let encoded ruleset =
  match Jsont_bytesrw.encode_string Push.Ruleset.global_jsont ruleset with
  | Ok body -> body
  | Error error -> Alcotest.failf "cannot encode push rules: %s" error

let state_with_rules ruleset =
  Base.with_push_rules (Base.create ~user_id:user ()) ruleset

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env sw

let with_temp_root f =
  let dir = Filename.temp_dir "matrix-push-bootstrap-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () -> f dir)

let test_no_request_for_state () =
  with_eio @@ fun env sw ->
  let requests = ref 0 in
  let fetch =
    Fetch_mock.client (fun _ ->
        incr requests;
        Alcotest.fail "push-rules bootstrap unexpectedly requested")
  in
  let service = Service.create (state_with_rules fetched_rules) in
  Service.bootstrap_push_rules (client ~sw ~env fetch) service;
  Alcotest.(check int) "state rules avoid request" 0 !requests

let test_fetch_install_and_restart () =
  with_eio @@ fun env sw ->
  let requests = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr requests;
        Fetch_mock.respond (encoded fetched_rules) request)
  in
  let store = Store.memory () in
  let service = Service.create ~store (Base.create ~user_id:user ()) in
  Service.bootstrap_push_rules (client ~sw ~env fetch) service;
  Alcotest.(check int) "one endpoint request" 1 !requests;
  Alcotest.(check bool)
    "fetched rules active" true
    (Push.Ruleset.equal (Base.ruleset (Service.state service)) fetched_rules);
  Alcotest.(check bool)
    "fetched event persisted" true
    (Option.is_some (Store.find_account_data store "m.push_rules"));
  let restarted = Service.of_store ~store ~user_id:user () in
  let no_request =
    Fetch_mock.client (fun _ ->
        Alcotest.fail "restart fetched push rules again")
  in
  Service.bootstrap_push_rules (client ~sw ~env no_request) restarted;
  Alcotest.(check bool)
    "restart restores fetched rules" true
    (Push.Ruleset.equal (Base.ruleset (Service.state restarted)) fetched_rules)

let test_fallbacks_preserve_defaults () =
  with_eio @@ fun env sw ->
  List.iter
    (fun (name, status, body) ->
      let fetch =
        Fetch_mock.client (fun request ->
            Fetch_mock.respond ~status body request)
      in
      let service = Service.create (Base.create ~user_id:user ()) in
      Service.bootstrap_push_rules (client ~sw ~env fetch) service;
      Alcotest.(check bool)
        name true
        (Push.Ruleset.equal
           (Base.ruleset (Service.state service))
           (Push.default_ruleset ~user_id:user)))
    [
      ( "404 preserves defaults",
        404,
        {|{"errcode":"M_NOT_FOUND","error":"missing"}|} );
      ("malformed preserves defaults", 200, "not-json");
      ( "unsupported preserves defaults",
        400,
        {|{"errcode":"M_UNRECOGNIZED","error":"unsupported"}|} );
    ]

let test_unknown_propagates () =
  with_eio @@ fun env sw ->
  let fetch =
    Fetch_mock.client (fun request ->
        Fetch_mock.respond ~status:500
          {|{"errcode":"M_UNKNOWN","error":"server failure"}|} request)
  in
  let service = Service.create (Base.create ~user_id:user ()) in
  let client = client ~sw ~env fetch in
  try
    Service.bootstrap_push_rules client service;
    Alcotest.fail "M_UNKNOWN was swallowed"
  with
  | Eio.Io (Eio_error.E (Eio_error.Matrix { errcode; _ }), _) ->
      Alcotest.(check bool)
        "M_UNKNOWN propagates" true
        (errcode = Matrix_client.Error.M_UNKNOWN)
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)

let test_persistence_failure_rolls_back () =
  with_temp_root @@ fun dir ->
  with_eio @@ fun env sw ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let store = Store.on_disk ~dir:root in
  let prior_rules = Jsont.Json.string "malformed-prior-rules" in
  Store.set_account_data store "m.push_rules" prior_rules;
  (* Opened before the competing writer, so [store]'s fingerprint is still
     [None] when bootstrap tries to flush. *)
  let competing = Store.on_disk ~dir:root in
  Store.set_account_data competing "m.direct" (Jsont.Json.object' []);
  (match Store.flush competing with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "competing flush failed: %s"
        (Matrix_client.Error.to_string error));
  let service = Service.create ~store (Base.create ~user_id:user ()) in
  let requests = ref 0 in
  let fetch =
    Fetch_mock.client (fun request ->
        incr requests;
        Fetch_mock.respond (encoded fetched_rules) request)
  in
  let before = Base.ruleset (Service.state service) in
  (try
     Service.bootstrap_push_rules (client ~sw ~env fetch) service;
     Alcotest.fail "bootstrap unexpectedly succeeded despite stale store"
   with Eio.Io _ -> ());
  Alcotest.(check bool)
    "failed persistence leaves active state unchanged" true
    (Push.Ruleset.equal (Base.ruleset (Service.state service)) before);
  Alcotest.(check bool)
    "failed persistence rolls back account data" true
    (Option.equal Jsont.Json.equal
       (Store.find_account_data store "m.push_rules")
       (Some prior_rules));
  Alcotest.(check bool)
    "failed store remains dirty for an explicit retry" true (Store.dirty store);
  (* A failed attempt did not publish the result; another attempt must go
     through persistence again rather than taking a ruleset from the store. *)
  (try
     Service.bootstrap_push_rules (client ~sw ~env fetch) service;
     Alcotest.fail "retry unexpectedly succeeded without a successful flush"
   with Eio.Io _ -> ());
  Alcotest.(check bool)
    "retry still leaves active state unchanged" true
    (Push.Ruleset.equal (Base.ruleset (Service.state service)) before);
  Alcotest.(check int)
    "each retry performs its own endpoint attempt" 2 !requests

let sync_response =
  match
    Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
      (Printf.sprintf
         {|{"next_batch":"sync-rules","account_data":{"events":[{"type":"m.push_rules","content":%s}]}}|}
         (encoded synced_rules))
  with
  | Ok response -> response
  | Error error -> Alcotest.failf "cannot decode sync fixture: %s" error

let test_sync_wins_race () =
  with_eio @@ fun env sw ->
  let entered, entered_r = Eio.Promise.create () in
  let release, release_r = Eio.Promise.create () in
  let finished, finished_r = Eio.Promise.create () in
  let fetch =
    Fetch_mock.client (fun request ->
        Eio.Promise.resolve entered_r ();
        Eio.Promise.await release;
        Fetch_mock.respond (encoded fetched_rules) request)
  in
  let service = Service.create (Base.create ~user_id:user ()) in
  let c = client ~sw ~env fetch in
  Eio.Fiber.fork ~sw (fun () ->
      Service.bootstrap_push_rules c service;
      Eio.Promise.resolve finished_r ());
  Eio.Promise.await entered;
  ignore (Service.apply c service sync_response);
  Eio.Promise.resolve release_r ();
  Eio.Promise.await finished;
  Alcotest.(check bool)
    "later sync rules win" true
    (Push.Ruleset.equal (Base.ruleset (Service.state service)) synced_rules)

let () =
  Alcotest.run "push bootstrap"
    [
      ( "bootstrap",
        [
          Alcotest.test_case "no request" `Quick test_no_request_for_state;
          Alcotest.test_case "fetch and restart" `Quick
            test_fetch_install_and_restart;
          Alcotest.test_case "fallbacks" `Quick test_fallbacks_preserve_defaults;
          Alcotest.test_case "unknown propagates" `Quick test_unknown_propagates;
          Alcotest.test_case "persistence rollback" `Quick
            test_persistence_failure_rolls_back;
          Alcotest.test_case "sync wins" `Quick test_sync_wins_race;
        ] );
    ]
