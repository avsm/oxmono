(** Hermetic coverage for room previews and knock moderation. *)

module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Store = Matrix_client.Store
module Preview = Matrix_client.Room_preview
module Knocks = Matrix_client.Knock_requests
module Client = Matrix_client.Client

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let room_id = rid "!preview:example.org"
let requester = uid "@requester:example.org"

let event ?event_id ?timestamp ~event_type ~state_key content :
    Store.state_event =
  {
    event_type = Event.Event_type.of_string event_type;
    state_key;
    content;
    sender = Some requester;
    event_id;
    origin_server_ts = timestamp;
  }

let obj members =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       members)

let room ?(membership = Store.Invited) ?(state_events = []) () =
  let base = Store.empty_room_info ~room_id ~membership in
  { base with state_events }

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_opt_string = Alcotest.(check (option string))
let check_opt_int64 = Alcotest.(check (option int64))

let test_stripped_preview_reload () =
  Eio_main.run @@ fun env ->
  let dir_name = Filename.temp_dir "matrix-preview-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir_name ])))
    (fun () ->
      let dir = Eio.Path.(Eio.Stdenv.fs env / dir_name) in
      let state_events =
        [
          event ~event_type:"m.room.create" ~state_key:""
            (obj
               [
                 ("room_version", Jsont.Json.string "10");
                 ( "predecessor",
                   obj
                     [
                       ("room_id", Jsont.Json.string "!older:example.org");
                       ("event_id", Jsont.Json.string "$upgrade:example.org");
                     ] );
               ]);
          event ~event_type:"m.room.name" ~state_key:""
            (obj [ ("name", Jsont.Json.string "An invite") ]);
          event ~event_type:"m.room.tombstone" ~state_key:""
            (obj
               [
                 ("body", Jsont.Json.string "Moved");
                 ("replacement_room", Jsont.Json.string "!newer:example.org");
               ]);
          event ~event_type:"m.room.member_hints" ~state_key:""
            (obj
               [
                 ( "service_members",
                   Jsont.Json.array
                     [| Jsont.Json.string "@service:example.org" |] );
               ]);
          event ~event_type:"m.room.member"
            ~state_key:(Id.User_id.to_string requester)
            (obj [ ("membership", Jsont.Json.string "invite") ]);
        ]
      in
      let store = Store.on_disk ~dir in
      Store.set_room store
        {
          (room ~state_events ()) with
          name = Some "An invite";
          heroes =
            [
              {
                Store.user_id = requester;
                display_name = Some "Requester";
                avatar_url = None;
              };
            ];
        };
      Alcotest.(check bool) "flush succeeds" true (Store.flush store = Ok ());
      let reopened = Store.on_disk ~dir in
      let preview =
        Preview.of_room (Option.get (Store.find_room reopened room_id))
      in
      check_string "room id" "!preview:example.org"
        (Id.Room_id.to_string preview.room_id);
      check_opt_string "name" (Some "An invite") preview.name;
      Alcotest.(check bool)
        "invited membership" true
        (preview.membership = Some Store.Invited);
      Alcotest.(check int)
        "active count" 0
        (Option.get preview.num_active_members);
      let create = Option.get preview.create in
      check_opt_string "room version" (Some "10")
        (Event.Room_create_content.room_version create);
      let predecessor =
        Option.get (Event.Room_create_content.predecessor create)
      in
      check_string "predecessor" "!older:example.org"
        (Id.Room_id.to_string
           (Event.Room_create_content.Predecessor.room_id predecessor));
      let tombstone = Option.get preview.tombstone in
      check_string "successor" "!newer:example.org"
        (Id.Room_id.to_string
           (Event.Room_tombstone_content.replacement_room tombstone));
      Alcotest.(check (list string))
        "service members" [ "@service:example.org" ]
        (List.map Id.User_id.to_string
           (Option.value preview.service_members ~default:[]));
      let heroes = Option.value preview.heroes ~default:[] in
      Alcotest.(check int) "hero survives reload" 1 (List.length heroes))

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

let session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "secret";
    device_id = Result.get_ok (Id.Device_id.of_string "DEVICE");
    refresh_token = None;
  }

type recorded = { meth : string; url : string; body : string option }

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
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    session

let json_response body = Fetch_mock.respond body

let test_remote_summary_preview () =
  Eio_mock.Backend.run @@ fun () ->
  let log = ref [] in
  let fetch_response =
    json_response
      {|{"room_id":"!remote:example.org","name":"Remote","topic":"Topic","avatar_url":"mxc://hs.example/a","canonical_alias":"#remote:example.org","num_joined_members":7,"room_type":"m.space","join_rule":"public","world_readable":true}|}
  in
  let client = client (fun req -> fetch_response req) log in
  let preview =
    match
      Preview.get client ~store:(Store.memory ())
        ~room_id_or_alias:(`Room_id (rid "!remote:example.org"))
        ()
    with
    | Ok p -> p
    | Error e ->
        Alcotest.failf "summary failed: %s" (Matrix_client.Error.to_string e)
  in
  check_int "remote member count" 7 preview.num_joined_members;
  check_opt_string "remote name" (Some "Remote") preview.name;
  check_bool "remote world readable" true (Option.get preview.is_world_readable);
  let request = List.hd (List.rev !log) in
  check_string "summary path"
    "https://hs.example/_matrix/client/v1/room_summary/!remote:example.org"
    request.url

let test_remote_state_fallback () =
  Eio_mock.Backend.run @@ fun () ->
  let log = ref [] in
  let state =
    {|[{"type":"m.room.name","state_key":"","sender":"@alice:example.org","origin_server_ts":1,"content":{"name":"Fallback"}},{"type":"m.room.history_visibility","state_key":"","sender":"@alice:example.org","origin_server_ts":1,"content":{"history_visibility":"world_readable"}}]|}
  in
  let client =
    client
      (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/room_summary/!fallback:example.org" url
        then
          Fetch_mock.respond ~status:404
            {|{"errcode":"M_NOT_FOUND","error":"no summary"}|} req
        else if
          String.ends_with ~suffix:"/rooms/!fallback:example.org/state" url
        then json_response state req
        else if String.ends_with ~suffix:"/joined_members" url then
          json_response {|{"joined":{"@alice:example.org":{}}}|} req
        else Fetch_mock.respond ~status:500 "unexpected" req)
      log
  in
  let preview =
    match
      Preview.get client ~store:(Store.memory ())
        ~room_id_or_alias:(`Room_id (rid "!fallback:example.org"))
        ()
    with
    | Ok p -> p
    | Error e ->
        Alcotest.failf "fallback failed: %s" (Matrix_client.Error.to_string e)
  in
  check_opt_string "fallback name" (Some "Fallback") preview.name;
  check_int "fallback count" 1 preview.num_joined_members;
  check_bool "fallback visibility" true (Option.get preview.is_world_readable);
  check_int "fallback requests" 3 (List.length !log)

let test_knock_metadata_and_seen_reload () =
  Eio_main.run @@ fun env ->
  let dir_name = Filename.temp_dir "matrix-knock-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir_name ])))
    (fun () ->
      let dir = Eio.Path.(Eio.Stdenv.fs env / dir_name) in
      let store = Store.on_disk ~dir in
      Store.set_room store
        (room ~membership:Store.Joined
           ~state_events:
             [
               event ~event_id:(eid "$knock:example.org")
                 ~timestamp:(Event.Timestamp.of_ms 42L)
                 ~event_type:"m.room.member"
                 ~state_key:(Id.User_id.to_string requester)
                 (obj
                    [
                      ("membership", Jsont.Json.string "knock");
                      ("displayname", Jsont.Json.string "Knocker");
                      ("avatar_url", Jsont.Json.string "mxc://hs.example/avatar");
                      ("reason", Jsont.Json.string "Please let me in");
                    ]);
             ]
           ());
      Alcotest.(check bool) "room flush" true (Store.flush store = Ok ());
      let request =
        match Knocks.list store ~room_id with
        | [ r ] -> r
        | _ -> Alcotest.fail "one knock expected"
      in
      check_opt_string "display name" (Some "Knocker") request.display_name;
      check_opt_string "reason" (Some "Please let me in") request.reason;
      check_opt_int64 "timestamp" (Some 42L)
        (Option.map Event.Timestamp.to_ms request.timestamp);
      check_bool "unseen" false request.is_seen;
      Alcotest.(check bool)
        "mark seen flush" true
        (Knocks.mark_seen store request = Ok ());
      let reopened = Store.on_disk ~dir in
      let after = List.hd (Knocks.list reopened ~room_id) in
      check_bool "seen survives reload" true after.is_seen)

let test_exact_moderation_requests () =
  Eio_mock.Backend.run @@ fun () ->
  let run_action action expected_path expected_body =
    let log = ref [] in
    let client = client (fun req -> json_response "{}" req) log in
    let request : Knocks.t =
      {
        room_id;
        event_id = None;
        timestamp = None;
        user_id = requester;
        display_name = None;
        avatar_url = None;
        reason = None;
        is_seen = false;
      }
    in
    let result =
      match action with
      | `Accept -> Knocks.accept client request
      | `Decline -> Knocks.decline client request ~reason:"No entry" ()
      | `Ban -> Knocks.decline_and_ban client request ()
    in
    Alcotest.(check bool) "request succeeds" true (result = Ok ());
    match List.rev !log with
    | [ { meth; url; body } ] ->
        check_string "method" "POST" meth;
        check_string "path" expected_path url;
        Alcotest.(check (option string)) "body" (Some expected_body) body
    | _ -> Alcotest.fail "exactly one moderation request expected"
  in
  let prefix =
    "https://hs.example/_matrix/client/v3/rooms/!preview:example.org/"
  in
  run_action `Accept (prefix ^ "invite")
    {|{"user_id":"@requester:example.org"}|};
  run_action `Decline (prefix ^ "kick")
    {|{"user_id":"@requester:example.org","reason":"No entry"}|};
  run_action `Ban (prefix ^ "ban") {|{"user_id":"@requester:example.org"}|}

let () =
  Alcotest.run "room preview and knocks"
    [
      ( "preview",
        [
          Alcotest.test_case "stripped preview reload" `Quick
            test_stripped_preview_reload;
          Alcotest.test_case "remote summary" `Quick test_remote_summary_preview;
          Alcotest.test_case "state fallback" `Quick test_remote_state_fallback;
        ] );
      ( "knocks",
        [
          Alcotest.test_case "metadata and seen reload" `Quick
            test_knock_metadata_and_seen_reload;
          Alcotest.test_case "exact moderation requests" `Quick
            test_exact_moderation_requests;
        ] );
    ]
