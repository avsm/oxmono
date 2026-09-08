module R = Matrix_client.Retention
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Base = Matrix_client.Base_client
module Sync_service = Matrix_eio.Sync_service

let room = Result.get_ok (Id.Room_id.of_string "!room:example.org")
let user = Result.get_ok (Id.User_id.of_string "@alice:example.org")

let policy ?min_lifetime ?max_lifetime () =
  R.policy ?min_lifetime ?max_lifetime ()

let limits ?min_lifetime ?max_lifetime () =
  (R.{ min_lifetime; max_lifetime } : R.lifetime_limits)

let config ?(min_limits = None) ?(max_limits = None) policies =
  R.
    {
      limits = { min_lifetime = min_limits; max_lifetime = max_limits };
      policies;
    }

let check_lifetime name expected actual =
  Alcotest.(check (option int64)) name expected actual

let check_policy name expected actual =
  match (expected, actual) with
  | None, None -> ()
  | None, Some _ | Some _, None -> Alcotest.fail name
  | Some expected, Some actual ->
      check_lifetime (name ^ " min")
        (R.policy_min_lifetime expected)
        (R.policy_min_lifetime actual);
      check_lifetime (name ^ " max")
        (R.policy_max_lifetime expected)
        (R.policy_max_lifetime actual)

let test_effective_table () =
  let room_policy = policy ~min_lifetime:100L ~max_lifetime:500L () in
  let default = policy ~max_lifetime:200L () in
  let override = policy ~min_lifetime:1L ~max_lifetime:2L () in
  let cases =
    [
      ("no policy", None, config []);
      ("room only", Some room_policy, config []);
      ("default", None, config [ ("*", default) ]);
      ( "override",
        Some room_policy,
        config [ (Id.Room_id.to_string room, override) ] );
      ( "limits and conflict",
        Some (policy ~min_lifetime:50L ~max_lifetime:1000L ()),
        config
          ~min_limits:(Some (limits ~min_lifetime:100L ~max_lifetime:200L ()))
          ~max_limits:(Some (limits ~min_lifetime:300L ~max_lifetime:500L ()))
          [] );
    ]
  in
  List.iter
    (fun (name, room_policy, config) ->
      let expected =
        match name with
        | "no policy" -> None
        | "room only" -> room_policy
        | "default" -> Some default
        | "override" -> Some override
        | "limits and conflict" ->
            Some (policy ~min_lifetime:100L ~max_lifetime:500L ())
        | _ -> assert false
      in
      check_policy name expected
        (R.effective_policy ~room_id:room ~room_policy config))
    cases

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type request = { meth : string; url : string; body : string option }

let body req =
  match req.Fetch.Middleware.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let client handler log =
  let fetch =
    Fetch_mock.client (fun req ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body = body req;
          }
          :: !log;
        handler req)
  in
  let config =
    Matrix_client.Client.config
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      ()
  in
  Matrix_client.Client.create ~config ~fetch
    ~random:(Matrix_client.Random.of_env mock_env)

let test_endpoint_paths_and_body () =
  let log = ref [] in
  let response =
    {|{"limits":{"min_lifetime":{"min":1000}},"policies":{"*":{"max_lifetime":86400000}}}|}
  in
  let t =
    client
      (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/retention/configuration" url then
          Fetch_mock.respond response req
        else Fetch_mock.respond {|{"event_id":"$retention:example.org"}|} req)
      log
  in
  let got =
    match R.get_configuration t with
    | Ok c -> c
    | Error e ->
        Alcotest.failf "configuration failed: %s"
          (Matrix_client.Error.to_string e)
  in
  check_lifetime "decoded minimum" (Some 1000L)
    (Option.get got.limits.min_lifetime).min_lifetime;
  let event_id =
    match R.set_room_policy t ~room_id:room (policy ~max_lifetime:123L ()) with
    | Ok id -> id
    | Error e ->
        Alcotest.failf "set failed: %s" (Matrix_client.Error.to_string e)
  in
  Alcotest.(check string)
    "set event id" "$retention:example.org"
    (Id.Event_id.to_string event_id);
  let requests = List.rev !log in
  Alcotest.(check int) "two requests" 2 (List.length requests);
  Alcotest.(check string)
    "configuration path"
    "https://hs.example/_matrix/client/unstable/org.matrix.msc1763/retention/configuration"
    (List.hd requests).url;
  let set_request = List.nth requests 1 in
  Alcotest.(check string) "set method" "PUT" set_request.meth;
  Alcotest.(check string)
    "state path"
    "https://hs.example/_matrix/client/v3/rooms/!room:example.org/state/m.room.retention/"
    set_request.url;
  let json =
    match set_request.body with
    | Some body -> body
    | None -> Alcotest.fail "set body missing"
  in
  let decoded =
    match Jsont_bytesrw.decode_string R.policy_jsont json with
    | Ok p -> p
    | Error e -> Alcotest.failf "bad set body: %s" e
  in
  check_lifetime "set max" (Some 123L) (R.policy_max_lifetime decoded)

let test_unsupported_configuration_returns_none () =
  let log = ref [] in
  let t =
    client
      (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/retention/configuration" url then
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"unknown endpoint"}|} req
        else
          Alcotest.fail
            "room state must not be fetched when configuration is unsupported")
      log
  in
  check_policy "unsupported configuration" None
    (match R.effective t ~room_id:room with
    | Ok p -> p
    | Error e ->
        Alcotest.failf "fallback failed: %s" (Matrix_client.Error.to_string e))

let test_validation () =
  Alcotest.check_raises "negative constructor"
    (Invalid_argument "Retention.policy: negative lifetime") (fun () ->
      ignore (policy ~min_lifetime:(-1L) ()));
  Alcotest.check_raises "conflicting constructor"
    (Invalid_argument "Retention.policy: minimum exceeds maximum") (fun () ->
      ignore (policy ~min_lifetime:2L ~max_lifetime:1L ()));
  let bad_policy json =
    match Jsont_bytesrw.decode_string R.policy_jsont json with
    | Ok _ -> Alcotest.fail "malformed policy was accepted"
    | Error _ -> ()
  in
  bad_policy {|{"min_lifetime":-1}|};
  bad_policy {|{"min_lifetime":2,"max_lifetime":1}|};
  let bad_limits =
    {|{"limits":{"min_lifetime":{"min":2,"max":1}},"policies":{}}|}
  in
  (match Jsont_bytesrw.decode_string R.configuration_jsont bad_limits with
  | Ok _ -> Alcotest.fail "conflicting server limits were accepted"
  | Error _ -> ());
  let bad_key =
    {|{"limits":{},"policies":{"not-a-room":{"max_lifetime":1}}}|}
  in
  match Jsont_bytesrw.decode_string R.configuration_jsont bad_key with
  | Ok _ -> Alcotest.fail "invalid server policy key was accepted"
  | Error _ -> ()

let test_state_survives_restart () =
  let response =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont
        {|{"next_batch":"s1","rooms":{"join":{"!room:example.org":{"state":{"events":[{"type":"m.room.retention","state_key":"","sender":"@alice:example.org","event_id":"$r:example.org","origin_server_ts":1,"content":{"min_lifetime":1000,"max_lifetime":5000}}]},"timeline":{"events":[]}}}}}|}
    with
    | Ok response -> response
    | Error e -> Alcotest.failf "bad sync fixture: %s" e
  in
  let state = Sync_service.create (Base.create ~user_id:user ()) in
  let state, _ =
    Base.apply ~coverage:Base.complete_state_coverage (Sync_service.state state)
      response
  in
  let store = Matrix_client.Store.memory () in
  let before = Option.get (Base.find_room state room) in
  let before_policy = Option.get (Base.retention before) in
  check_lifetime "initial minimum" (Some 1000L)
    (Event.Room_retention_content.min_lifetime before_policy);
  Base.persist store state;
  let reopened = Base.of_store store ~user_id:user () in
  let room_info = Option.get (Base.find_room reopened room) in
  let retention = Option.get (Base.retention room_info) in
  check_lifetime "reloaded minimum" (Some 1000L)
    (Event.Room_retention_content.min_lifetime retention);
  check_lifetime "reloaded maximum" (Some 5000L)
    (Event.Room_retention_content.max_lifetime retention)

let () =
  Alcotest.run "retention"
    [
      ( "policy",
        [
          Alcotest.test_case "effective table" `Quick test_effective_table;
          Alcotest.test_case "validation" `Quick test_validation;
        ] );
      ( "endpoint",
        [
          Alcotest.test_case "paths and body" `Quick (fun () ->
              Eio_mock.Backend.run test_endpoint_paths_and_body);
          Alcotest.test_case "unsupported configuration returns none" `Quick
            (fun () ->
              Eio_mock.Backend.run test_unsupported_configuration_returns_none);
        ] );
      ( "state",
        [ Alcotest.test_case "restart" `Quick test_state_survives_restart ] );
    ]
