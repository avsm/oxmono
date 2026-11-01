(** Hermetic tests for the adaptive native-sliding/classic-sync driver. *)

module Client = Matrix_client.Client
module Id = Matrix_proto.Id
module Service = Matrix_eio.Sync_service
module Adaptive = Matrix_eio.Adaptive_sync

let user = Id.User_id.of_string_exn "@alice:example.org"
let device = Id.Device_id.of_string_exn "ADAPTIVE"
let homeserver = Uriz.of_string_exn "https://hs.example"

let session : Client.session =
  {
    user_id = user;
    access_token = "adaptive-token";
    device_id = device;
    refresh_token = None;
  }

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

let contains ~sub value =
  let n = String.length sub and m = String.length value in
  let rec loop i = i + n <= m && (String.sub value i n = sub || loop (i + 1)) in
  loop 0

let mock handler =
  let requests = ref [] in
  let fetch =
    Fetch_mock.client (fun request ->
        requests := Fetch.Middleware.Url.to_string request.url :: !requests;
        handler request)
  in
  (requests, fetch)

let client ~env ~sw fetch =
  Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
  Matrix_eio.Client.with_session client session

let with_loop f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env sw

let versions ~sliding =
  if sliding then
    {|{"versions":["v1.17"],"unstable_features":{"org.matrix.simplified_msc3575":true}}|}
  else {|{"versions":["v1.17"],"unstable_features":{}}|}

let response_json = {|{"pos":"sliding-position"}|}
let classic_json = {|{"next_batch":"classic-position"}|}

let run_case handler
    ?(on_error =
      fun error ->
        Alcotest.failf "unexpected adaptive error: %a" Matrix_eio.Error.pp_err
          error) ?set_presence expected_modes expected_responses check_requests
    =
  with_loop @@ fun env sw ->
  let requests, fetch = mock handler in
  let service = Service.of_user ~user_id:user () in
  let modes = ref [] and responses = ref [] in
  let changes = ref 0 in
  let done_, done_r = Eio.Promise.create () in
  let on_response response =
    responses := response :: !responses;
    let action =
      match response with
      | Adaptive.Sliding _ | Adaptive.Classic _ -> Matrix_eio.Sync.Stop
    in
    Eio.Promise.resolve done_r ();
    action
  in
  Adaptive.run ~sw ~clock:(Eio.Stdenv.clock env) (client ~env ~sw fetch)
    ~service ?set_presence
    ~on_mode:(fun mode -> modes := mode :: !modes)
    ~on_response ~on_error
    ~on_change:(fun _ _ -> incr changes)
    ();
  Eio.Promise.await done_;
  check_bool "mode transitions" true (List.rev !modes = expected_modes);
  check_int "responses" expected_responses (List.length !responses);
  check_bool "shared on_change callback" true (!changes > 0);
  check_requests (List.rev !requests)

let test_unavailable_uses_classic () =
  run_case
    (fun request ->
      let url = Fetch.Middleware.Url.to_string request.url in
      if contains ~sub:"/_matrix/client/versions" url then
        Fetch_mock.respond (versions ~sliding:false) request
      else begin
        check_bool "unavailable does not poll sliding" false
          (contains ~sub:"simplified_msc3575" url);
        Fetch_mock.respond classic_json request
      end)
    ~set_presence:`Unavailable
    [ Adaptive.Discovering; Adaptive.Classic ]
    1
    (fun requests ->
      check_int "versions and classic requests" 2 (List.length requests);
      check_bool "classic request" true
        (List.exists (contains ~sub:"/_matrix/client/v3/sync") requests);
      check_bool "classic receives sliding presence override" true
        (List.exists
           (fun request -> contains ~sub:"set_presence=unavailable" request)
           requests))

let test_advertised_uses_sliding () =
  run_case
    (fun request ->
      let url = Fetch.Middleware.Url.to_string request.url in
      if contains ~sub:"/_matrix/client/versions" url then
        Fetch_mock.respond (versions ~sliding:true) request
      else begin
        check_bool "advertised native endpoint is used" true
          (contains ~sub:"simplified_msc3575" url);
        Fetch_mock.respond response_json request
      end)
    [ Adaptive.Discovering; Adaptive.Sliding ]
    1
    (fun requests ->
      check_int "versions and sliding requests" 2 (List.length requests))

let test_unsupported_switches_once () =
  let errors = ref 0 in
  run_case
    (fun request ->
      let url = Fetch.Middleware.Url.to_string request.url in
      if contains ~sub:"/_matrix/client/versions" url then
        Fetch_mock.respond (versions ~sliding:true) request
      else if contains ~sub:"simplified_msc3575" url then
        Fetch_mock.respond ~status:404 {|{"error":"not found"}|} request
      else Fetch_mock.respond classic_json request)
    ~on_error:(fun _ ->
      incr errors;
      Matrix_eio.Sync.Stop)
    [ Adaptive.Discovering; Adaptive.Sliding; Adaptive.Classic ]
    1
    (fun requests ->
      check_int "one native and one classic request" 3 (List.length requests);
      check_int "unsupported is not surfaced" 0 !errors;
      check_int "one classic request" 1
        (List.length
           (List.filter (contains ~sub:"/_matrix/client/v3/sync") requests)))

let test_user_stop_does_not_fallback () =
  with_loop @@ fun env sw ->
  let requests, fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains ~sub:"/_matrix/client/versions" url then
          Fetch_mock.respond (versions ~sliding:true) request
        else Fetch_mock.respond response_json request)
  in
  let service = Service.of_user ~user_id:user () in
  let modes = ref [] in
  let done_, done_r = Eio.Promise.create () in
  Adaptive.run ~sw ~clock:(Eio.Stdenv.clock env) (client ~env ~sw fetch)
    ~service
    ~on_mode:(fun mode -> modes := mode :: !modes)
    ~on_response:(fun response ->
      match response with
      | Adaptive.Sliding _ ->
          Eio.Promise.resolve done_r ();
          Matrix_eio.Sync.Stop
      | Adaptive.Classic _ -> Alcotest.fail "unexpected classic response")
    ~on_error:(fun error ->
      Alcotest.failf "unexpected error: %a" Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ -> ())
    ();
  Eio.Promise.await done_;
  check_bool "user stop does not fallback" true
    (List.rev !modes = [ Adaptive.Discovering; Adaptive.Sliding ]);
  check_int "no classic request" 0
    (List.length
       (List.filter
          (contains ~sub:"/_matrix/client/v3/sync")
          (List.rev !requests)))

let test_discovery_retry () =
  let discovery = ref 0 and errors = ref 0 in
  run_case
    (fun request ->
      let url = Fetch.Middleware.Url.to_string request.url in
      if contains ~sub:"/_matrix/client/versions" url then begin
        incr discovery;
        if !discovery = 1 then Fetch_mock.respond ~status:500 "{}" request
        else Fetch_mock.respond (versions ~sliding:false) request
      end
      else Fetch_mock.respond classic_json request)
    ~on_error:(fun _ ->
      incr errors;
      Matrix_eio.Sync.Continue)
    [ Adaptive.Discovering; Adaptive.Classic ]
    1
    (fun requests ->
      check_int "discovery retry error" 1 !errors;
      check_int "two discovery requests and classic" 3 (List.length requests))

let test_shared_classic_cursor () =
  with_loop @@ fun env sw ->
  let requests, fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains ~sub:"/_matrix/client/versions" url then
          Fetch_mock.respond (versions ~sliding:false) request
        else Fetch_mock.respond classic_json request)
  in
  let store = Matrix_client.Store.memory () in
  let service = Service.of_store ~store ~user_id:user () in
  let done_, done_r = Eio.Promise.create () in
  Adaptive.run ~sw ~clock:(Eio.Stdenv.clock env) (client ~env ~sw fetch)
    ~service
    ~on_response:(fun response ->
      match response with
      | Adaptive.Classic _ ->
          Eio.Promise.resolve done_r ();
          Matrix_eio.Sync.Stop
      | Adaptive.Sliding _ -> Alcotest.fail "unexpected sliding response")
    ~on_error:(fun error ->
      Alcotest.failf "unexpected error: %a" Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ -> ())
    ();
  Eio.Promise.await done_;
  check_bool "classic cursor persisted in shared service" true
    (Matrix_client.Base_client.next_batch (Service.state service)
    = Some "classic-position");
  check_int "versions and classic request" 2 (List.length !requests)

let test_parent_cancellation_has_no_orphan () =
  let cancelled = ref false in
  (try
     Eio_main.run @@ fun env ->
     Eio.Switch.run @@ fun sw ->
     let started, started_r = Eio.Promise.create () in
     let _, fetch =
       mock (fun request ->
           let url = Fetch.Middleware.Url.to_string request.url in
           if contains ~sub:"/_matrix/client/versions" url then
             Fetch_mock.respond (versions ~sliding:true) request
           else begin
             Eio.Promise.resolve started_r ();
             Eio.Time.sleep (Eio.Stdenv.clock env) 60.;
             Fetch_mock.respond response_json request
           end)
     in
     let service = Service.of_user ~user_id:user () in
     Adaptive.run ~sw ~clock:(Eio.Stdenv.clock env) (client ~env ~sw fetch)
       ~service
       ~on_change:(fun _ _ -> ())
       ();
     Eio.Promise.await started;
     Eio.Switch.fail sw Exit
   with Exit -> cancelled := true);
  check_bool "parent cancellation tears down adaptive phases" true !cancelled

let test_eio_error_context () =
  let check
      ?(is_expected =
        function Matrix_eio.Error.Network "offline" -> true | _ -> false)
      expected run =
    try
      run ();
      Alcotest.fail "operation unexpectedly succeeded"
    with
    | Eio.Io (Matrix_eio.Error.E error, _) as exn when is_expected error ->
        let rendered = Fmt.str "%a" Eio.Exn.pp exn in
        check_bool expected true (contains ~sub:expected rendered)
    | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
  in
  check "fetching account data" (fun () ->
      Matrix_eio.Error.unwrap ~context:"fetching account data"
        (Error (Matrix_client.Error.Network_error "offline")));
  check "saving profile state" (fun () ->
      Matrix_eio.Error.with_context "saving profile state" (fun () ->
          raise (Matrix_eio.Error.err (Matrix_eio.Error.Network "offline"))));
  with_loop @@ fun env sw ->
  let anonymous =
    Matrix_eio.Client.create ~sw ~env ~homeserver
      ~fetch:(Fetch_mock.client (fun _ -> assert false))
      ()
  in
  check
    ~is_expected:(function
      | Matrix_eio.Error.Not_logged_in -> true | _ -> false)
    "getting logged-in Matrix user ID"
    (fun () -> ignore (Matrix_eio.Client.user_id anonymous))

let test_eio_context_preserves_non_io_exceptions () =
  (match
     Matrix_eio.Error.with_context "must not catch cancellation" (fun () ->
         raise (Eio.Cancel.Cancelled Exit))
   with
  | exception Eio.Cancel.Cancelled Exit -> ()
  | exception exn ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
  | () -> Alcotest.fail "cancellation was swallowed");
  match
    Matrix_eio.Error.with_context "must not catch programming errors" (fun () ->
        failwith "marker")
  with
  | exception Failure _ -> ()
  | exception exn ->
      Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
  | () -> Alcotest.fail "programming exception was swallowed"

let () =
  Alcotest.run "adaptive_sync"
    [
      ( "selection",
        [
          Alcotest.test_case "unavailable uses classic" `Quick
            test_unavailable_uses_classic;
          Alcotest.test_case "advertised uses sliding" `Quick
            test_advertised_uses_sliding;
          Alcotest.test_case "unsupported switches once" `Quick
            test_unsupported_switches_once;
          Alcotest.test_case "user stop does not fallback" `Quick
            test_user_stop_does_not_fallback;
          Alcotest.test_case "discovery retry" `Quick test_discovery_retry;
          Alcotest.test_case "shared classic cursor" `Quick
            test_shared_classic_cursor;
          Alcotest.test_case "parent cancellation has no orphan" `Quick
            test_parent_cancellation_has_no_orphan;
          Alcotest.test_case "Eio operation context" `Quick
            test_eio_error_context;
          Alcotest.test_case "Eio context preserves non-I/O exceptions" `Quick
            test_eio_context_preserves_non_io_exceptions;
        ] );
    ]
