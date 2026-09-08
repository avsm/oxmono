module Id = Matrix_proto.Id
module Eio_client = Matrix_eio.Client
module Eio_encryption = Matrix_eio.Encryption
module D = Matrix_eio.Dehydrated_device
module Secrets = Matrix_eio.Secrets

let user = Id.User_id.of_string_exn "@alice:example.org"
let device = Id.Device_id.of_string_exn "PRIMARY"
let homeserver = Uriz.of_string_exn "https://hs.example"
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let random seed =
  let seed = if String.equal seed "" then "x" else seed in
  Matrix_client.Random.of_source
    (Eio.Flow.string_source
       (String.init 500_000 (fun index -> seed.[index mod String.length seed])))

type server = {
  mutable device_response : string option;
  mutable account_data : (string * string) list;
  mutable delete_ok : bool;
  mutable puts : int;
  mutable dehydrated_puts : int;
  mutable deletes : int;
  mutable gets : int;
  mutable posts : int;
}

let json_field name = function
  | Jsont.Object (members, _) ->
      Option.map snd
        (List.find_opt
           (fun ((member, _), _) -> String.equal member name)
           members)
  | _ -> None

let json_string json =
  match Jsont.Json.decode Matrix_proto.Json.Codec.string json with
  | Ok value -> value
  | Error error -> Alcotest.failf "expected JSON string: %s" error

let json_encode json =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json json with
  | Ok value -> value
  | Error error -> Alcotest.failf "expected encodable JSON: %s" error

let server () =
  {
    device_response = None;
    account_data = [];
    delete_ok = true;
    puts = 0;
    dehydrated_puts = 0;
    deletes = 0;
    gets = 0;
    posts = 0;
  }

let request_url request =
  Fetch.Middleware.Url.to_string request.Fetch.Middleware.url

let is_dehydrated url =
  String.ends_with
    ~suffix:"/_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device"
    url

let fetch server (request : Fetch.Middleware.request) =
  let meth = Http.Method.to_string request.meth in
  let url = request_url request in
  match meth with
  | "GET" -> (
      server.gets <- server.gets + 1;
      if is_dehydrated url then
        match server.device_response with
        | Some body -> Fetch_mock.respond body request
        | None ->
            Fetch_mock.respond ~status:404 {|{"errcode":"M_NOT_FOUND"}|} request
      else
        match List.assoc_opt url server.account_data with
        | Some body -> Fetch_mock.respond body request
        | None ->
            Fetch_mock.respond ~status:404 {|{"errcode":"M_NOT_FOUND"}|} request
      )
  | "POST" ->
      server.posts <- server.posts + 1;
      Fetch_mock.respond {|{"events":[]}|} request
  | "PUT" ->
      server.puts <- server.puts + 1;
      if is_dehydrated url then begin
        server.dehydrated_puts <- server.dehydrated_puts + 1;
        (* Preserve a real uploaded V1 device so the next GET can exercise the
           complete rehydration callback path. *)
        (match request.body with
        | Fetch.String body -> (
            match
              Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body
            with
            | Ok body -> (
                match
                  (json_field "device_id" body, json_field "device_data" body)
                with
                | Some device_id, Some device_data ->
                    server.device_response <-
                      Some
                        (Printf.sprintf {|{"device_id":%S,"device_data":%s}|}
                           (json_string device_id) (json_encode device_data))
                | _ -> ())
            | Error _ -> ())
        | _ -> ());
        Fetch_mock.respond {|{"device_id":"SERVER_DEHYDRATED"}|} request
      end
      else begin
        let body =
          match request.body with
          | Fetch.String body -> body
          | _ -> Alcotest.fail "account-data PUT did not use a string body"
        in
        server.account_data <-
          (url, body) :: List.remove_assoc url server.account_data;
        Fetch_mock.respond "{}" request
      end
  | "DELETE" ->
      server.deletes <- server.deletes + 1;
      if server.delete_ok then
        Fetch_mock.respond {|{"device_id":"deleted"}|} request
      else
        Fetch_mock.respond ~status:500
          {|{"errcode":"M_UNKNOWN","error":"delete failed"}|} request
  | _ -> Fetch_mock.respond ~status:500 "{}" request

let setup env sw server seed =
  let fetch = Fetch_mock.client (fetch server) in
  let client =
    Eio_client.create ~sw ~env ~homeserver
      ~well_known_policy:Matrix_client.Client.Do_not_query ~fetch ()
    |> fun client ->
    Eio_client.with_session client
      {
        Matrix_client.Client.user_id = user;
        device_id = device;
        access_token = "syt_manager";
        refresh_token = None;
      }
  in
  let encryption =
    Eio_encryption.create ~random:(random seed) ~user_id:user ~device_id:device
      ()
  in
  let identity =
    Matrix_client.Cross_signing.create_private_identity ~user_id:user
  in
  Matrix_client.Cross_signing.generate_private_keys
    ~random:(random (seed ^ "identity"))
    identity;
  let store =
    Secrets.create_secret_store client ~random:(random (seed ^ "store")) ()
  in
  (client, encryption, identity, store.store)

let sleep clock duration = Eio.Time.sleep clock duration

let events_as_strings events =
  List.map
    (function
      | D.Manager.Created _ -> "created"
      | D.Manager.Uploaded _ -> "uploaded"
      | D.Manager.Deleted -> "deleted"
      | D.Manager.Key_cached -> "key-cached"
      | D.Manager.Rehydration_started _ -> "rehydration-started"
      | D.Manager.Rehydration_progress _ -> "rehydration-progress"
      | D.Manager.Rehydration_completed _ -> "rehydration-completed"
      | D.Manager.Rehydration_error _ -> "rehydration-error"
      | D.Manager.Rotation_error _ -> "rotation-error")
    events

let make_manager client encryption identity =
  D.Manager.create ~encryption ~client ~private_identity:identity ()

let test_only_if_cached env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "cached" in
  let manager = make_manager client encryption identity in
  let before = (server.gets, server.puts) in
  D.Manager.start ~only_if_key_cached:true ~clock:(Eio.Stdenv.clock env) manager
    ~store ();
  check_bool "only-if-cached makes no network requests" true
    (before = (server.gets, server.puts));
  check_int "only-if-cached makes no POST" 0 server.posts;
  check_int "only-if-cached makes no DELETE" 0 server.deletes

let test_start_callbacks_and_key_creation env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "create" in
  let manager = make_manager client encryption identity in
  let events = ref [] in
  let bad =
    D.Manager.subscribe manager (fun _ -> failwith "callback failure")
  in
  ignore bad;
  ignore (D.Manager.subscribe manager (fun event -> events := event :: !events));
  D.Manager.start ~skip_rehydration:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  D.Manager.stop manager;
  let events = events_as_strings (List.rev !events) in
  check_string "startup lifecycle ordering" "key-cached,created,uploaded"
    (String.concat "," events);
  check_bool "key is locally cached" true
    (Option.is_some (D.cached_key encryption));
  check_int "one immediate dehydrated PUT" 1 server.dehydrated_puts

let test_self_unsubscribe env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "unsubscribe" in
  let manager = make_manager client encryption identity in
  let calls = ref 0 in
  let registration = ref None in
  registration :=
    Some
      (D.Manager.subscribe manager (fun _ ->
           incr calls;
           Option.iter (D.Manager.unsubscribe manager) !registration));
  D.Manager.start ~skip_rehydration:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  D.Manager.stop manager;
  check_int "self-unsubscribing callback runs once" 1 !calls

let test_failed_rehydrate_preserves_key env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "failed" in
  let old =
    D.reset_key_with_driver encryption store ~random:(random "old-key")
  in
  let old_string = D.Pickle_key.to_base64 old in
  server.device_response <-
    Some
      {|{"device_id":"SERVER_DEHYDRATED","device_data":{"algorithm":"bad","device_pickle":"bad"}}|};
  let manager = make_manager client encryption identity in
  let events = ref [] in
  ignore (D.Manager.subscribe manager (fun event -> events := event :: !events));
  D.Manager.start ~create_new_key:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  D.Manager.stop manager;
  check_string "failed rehydration preserves pickle key" old_string
    (D.Pickle_key.to_base64 (Option.get (D.cached_key encryption)));
  check_string "failed rehydration preserves SSSS pickle key" old_string
    (D.Pickle_key.to_base64 (Option.get (D.load_key store)));
  check_bool "rehydration error is observable" true
    (List.exists
       (function D.Manager.Rehydration_error _ -> true | _ -> false)
       !events)

let test_successful_rehydrate_rotates_key env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "rotate" in
  let old =
    D.reset_key_with_driver encryption store ~random:(random "rotate-old")
  in
  let old_string = D.Pickle_key.to_base64 old in
  (* Seed the homeserver with an actual V1 device. The stateful mock turns the
     following PUT body into the GET payload used by manager rehydration. *)
  ignore
    (D.create_and_upload encryption client ~private_identity:identity
       ~pickle_key:old ~random:(random "rotate-upload") ());
  let manager = make_manager client encryption identity in
  let events = ref [] in
  ignore (D.Manager.subscribe manager (fun event -> events := event :: !events));
  D.Manager.start ~create_new_key:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  D.Manager.stop manager;
  check_string "rehydration and rotation event order"
    "rehydration-started,rehydration-completed,deleted,key-cached,created,uploaded"
    (String.concat "," (events_as_strings (List.rev !events)));
  check_bool "rehydration emitted completion" true
    (List.exists
       (function D.Manager.Rehydration_completed _ -> true | _ -> false)
       !events);
  check_bool "rehydration deleted old device" true
    (List.exists (function D.Manager.Deleted -> true | _ -> false) !events);
  check_bool "successful start rotates pickle key" true
    (not
       (String.equal old_string
          (D.Pickle_key.to_base64 (Option.get (D.cached_key encryption)))));
  check_string "rotated cache and SSSS agree"
    (D.Pickle_key.to_base64 (Option.get (D.cached_key encryption)))
    (D.Pickle_key.to_base64 (Option.get (D.load_key store)))

let test_skip_rehydrate_new_key_does_not_load_old_key env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "skip-new" in
  ignore (D.reset_key_with_driver encryption store ~random:(random "skip-old"));
  (* A fresh encryption machine has no local cache, while [store] still has
     the old SSSS value. Rust's skip-rehydration path must reset directly
     rather than loading/caching that old value first. *)
  let fresh_encryption =
    Eio_encryption.create ~random:(random "skip-fresh") ~user_id:user
      ~device_id:device ()
  in
  let manager = make_manager client fresh_encryption identity in
  let events = ref [] in
  ignore (D.Manager.subscribe manager (fun event -> events := event :: !events));
  D.Manager.start ~skip_rehydration:true ~create_new_key:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  D.Manager.stop manager;
  let names = events_as_strings (List.rev !events) in
  check_string "skip/new-key lifecycle has one cache event"
    "key-cached,created,uploaded" (String.concat "," names)

let test_rotation_cached_key_retry_and_stop env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "ticks" in
  let manager = make_manager client encryption identity in
  let clock = Eio.Stdenv.clock env in
  let rotation_errors = ref 0 in
  ignore
    (D.Manager.subscribe manager (function
      | D.Manager.Rotation_error _ -> incr rotation_errors
      | _ -> ()));
  D.Manager.start ~skip_rehydration:true ~interval:0.01 ~clock manager ~store ();
  let before_ticks = server.gets in
  let uploads_before = server.dehydrated_puts in
  let valid = Option.get (D.cached_key encryption) in
  Matrix_client.Encryption.set_dehydrated_pickle_key
    (Eio_encryption.machine encryption)
    "invalid";
  sleep clock 0.03;
  Matrix_client.Encryption.set_dehydrated_pickle_key
    (Eio_encryption.machine encryption)
    (D.Pickle_key.to_base64 valid);
  sleep clock 0.03;
  D.Manager.stop manager;
  let uploads_after = server.dehydrated_puts in
  check_bool "rotation reads no server key source" true
    (server.gets = before_ticks);
  check_bool "rotation error is observable" true (!rotation_errors > 0);
  check_bool "rotation reports and survives a cached-key failure" true
    (uploads_after > uploads_before);
  sleep clock 0.03;
  check_int "stop cancels future uploads" uploads_after server.dehydrated_puts

let test_delete_emits_only_on_success env =
  Eio.Switch.run @@ fun sw ->
  let server = server () in
  let client, encryption, identity, store = setup env sw server "delete" in
  let manager = make_manager client encryption identity in
  let events = ref [] in
  ignore (D.Manager.subscribe manager (fun event -> events := event :: !events));
  D.Manager.start ~skip_rehydration:true ~interval:0.05
    ~clock:(Eio.Stdenv.clock env) manager ~store ();
  server.delete_ok <- false;
  (try D.Manager.delete manager with _ -> ());
  check_bool "failed delete has no Deleted event" false
    (List.exists (function D.Manager.Deleted -> true | _ -> false) !events);
  server.delete_ok <- true;
  D.Manager.delete manager;
  check_bool "successful delete emits Deleted" true
    (List.exists (function D.Manager.Deleted -> true | _ -> false) !events)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "dehydrated manager"
    [
      ( "manager",
        [
          Alcotest.test_case "only if cached" `Quick (fun () ->
              test_only_if_cached env);
          Alcotest.test_case "startup callbacks and key creation" `Quick
            (fun () -> test_start_callbacks_and_key_creation env);
          Alcotest.test_case "self unsubscribe" `Quick (fun () ->
              test_self_unsubscribe env);
          Alcotest.test_case "failed rehydrate preserves key" `Quick (fun () ->
              test_failed_rehydrate_preserves_key env);
          Alcotest.test_case "successful start rotates key" `Quick (fun () ->
              test_successful_rehydrate_rotates_key env);
          Alcotest.test_case "skip rehydrate new key" `Quick (fun () ->
              test_skip_rehydrate_new_key_does_not_load_old_key env);
          Alcotest.test_case "cached-key rotation retry and stop" `Quick
            (fun () -> test_rotation_cached_key_retry_and_stop env);
          Alcotest.test_case "delete emits only on success" `Quick (fun () ->
              test_delete_emits_only_on_success env);
        ] );
    ]
