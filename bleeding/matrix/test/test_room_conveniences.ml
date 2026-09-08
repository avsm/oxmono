module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Json = Matrix_proto.Json
module Client = Matrix_client.Client
module Base_client = Matrix_client.Base_client
module Store = Matrix_client.Store
module Room = Matrix_client.Room

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let own_user = uid "@me:own.example"
let room_id = rid "!room:example.org"

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\000')
  end

let obj members =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       members)

let strings values =
  Jsont.Json.array (Array.of_list (List.map Jsont.Json.string values))

let event ?sender ?event_id ~event_type ~state_key content : Store.state_event =
  {
    event_type = Event.Event_type.of_string event_type;
    state_key;
    content;
    sender;
    event_id;
    origin_server_ts = Some (Event.Timestamp.of_ms 1L);
  }

let member ?(membership = "join") user =
  event ~sender:user
    ~event_id:(eid ("$" ^ Id.User_id.localpart user))
    ~event_type:"m.room.member"
    ~state_key:(Id.User_id.to_string user)
    (obj
       [
         ("membership", Jsont.Json.string membership);
         ("displayname", Jsont.Json.string (Id.User_id.localpart user));
       ])

let state ?account_data ?(membership = Store.Joined) state_events =
  let store = Store.memory () in
  let room = Store.empty_room_info ~room_id ~membership in
  Store.set_room store { room with state_events };
  Option.iter (Store.set_account_data store "m.direct") account_data;
  Base_client.of_store store ~user_id:own_user ()

let client ?session handler =
  let client =
    Client.create
      ~config:
        (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
      ~fetch:(Fetch_mock.client handler)
      ~random:(Matrix_client.Random.of_env random_env)
  in
  match session with
  | None -> client
  | Some session -> Client.with_session client session

let offline_room ?account_data ?membership state_events =
  Room.create
    ~client:(client (fun _ -> Alcotest.fail "unexpected request"))
    ~state:(state ?account_data ?membership state_events)
    room_id

let check_strings = Alcotest.(check (list string))
let user_strings users = List.map Id.User_id.to_string users

let test_routing_and_permalinks () =
  let leader = uid "@leader:leader.example" in
  let popular_a = uid "@a:popular.example" in
  let popular_b = uid "@b:popular.example" in
  let popular_c = uid "@c:popular.example" in
  let second_a = uid "@d:second.example" in
  let second_b = uid "@e:second.example" in
  let third = uid "@f:third.example" in
  let blocked = uid "@bad:blocked.example" in
  let ipv4 = uid "@ip:127.0.0.1" in
  let ipv6 = uid "@ip6:[2001:db8::1]" in
  let invited = uid "@invite:invited.example" in
  let power_levels =
    event ~sender:own_user ~event_type:"m.room.power_levels" ~state_key:""
      (obj
         [
           ( "users",
             obj
               [
                 (Id.User_id.to_string leader, Jsont.Json.int 75);
                 (Id.User_id.to_string popular_a, Jsont.Json.int 10);
               ] );
           ("users_default", Jsont.Json.int 0);
         ])
  in
  let acl =
    event ~sender:own_user ~event_type:"m.room.server_acl" ~state_key:""
      (obj
         [
           ("allow", strings [ "*" ]);
           ("deny", strings [ "blocked.*" ]);
           (* Federation may allow these, but permalink routing never does. *)
           ("allow_ip_literals", Jsont.Json.bool true);
         ])
  in
  let members =
    [
      own_user;
      leader;
      popular_a;
      popular_b;
      popular_c;
      second_a;
      second_b;
      third;
      blocked;
      ipv4;
      ipv6;
    ]
    |> List.map member
  in
  let room =
    offline_room
      (member ~membership:"invite" invited :: power_levels :: acl :: members)
  in
  check_strings "power server then populations"
    [ "leader.example"; "popular.example"; "second.example" ]
    (Room.routing_candidates room);
  Alcotest.(check string)
    "room permalink"
    "https://matrix.to/#/!room:example.org?via=leader.example&via=popular.example&via=second.example"
    (Room.permalink room ());
  Alcotest.(check string)
    "event permalink"
    "https://matrix.to/#/!room:example.org/$event?via=leader.example&via=popular.example&via=second.example"
    (Room.event_permalink room (eid "$event"));
  let alias_event =
    event ~sender:own_user ~event_type:"m.room.canonical_alias" ~state_key:""
      (obj
         [
           ("alias", Jsont.Json.null ());
           ("alt_aliases", strings [ "#first:example.org"; "#last:example.org" ]);
         ])
  in
  let alias_room = offline_room [ alias_event; member own_user ] in
  Alcotest.(check string)
    "last alternate alias, with no via"
    "https://matrix.to/#/%23last:example.org"
    (Room.permalink alias_room ~via:[ "ignored.example" ] ())

let test_direct_targets_and_roles () =
  let alice = uid "@alice:example.org" in
  let bob = uid "@bob:example.org" in
  let creator = uid "@creator:example.org" in
  let additional = uid "@additional:example.org" in
  let admin = uid "@admin:example.org" in
  let moderator = uid "@moderator:example.org" in
  let direct =
    obj
      [
        (Id.User_id.to_string alice, strings [ Id.Room_id.to_string room_id ]);
        (Id.User_id.to_string bob, strings [ "!elsewhere:example.org" ]);
      ]
  in
  let create =
    event ~sender:creator ~event_type:"m.room.create" ~state_key:""
      (obj
         [
           ("room_version", Jsont.Json.string "12");
           ("additional_creators", strings [ Id.User_id.to_string additional ]);
         ])
  in
  let levels =
    event ~sender:creator ~event_type:"m.room.power_levels" ~state_key:""
      (obj
         [
           ( "users",
             obj
               [
                 (Id.User_id.to_string admin, Jsont.Json.int 100);
                 (Id.User_id.to_string moderator, Jsont.Json.int 50);
               ] );
         ])
  in
  let room = offline_room ~account_data:direct [ create; levels ] in
  check_strings "direct target" [ "@alice:example.org" ]
    (user_strings (Room.direct_targets room));
  Alcotest.(check (option string))
    "sole target" (Some "@alice:example.org")
    (Option.map Id.User_id.to_string (Room.dm_target room));
  let is role user = Room.suggested_role room user = role in
  Alcotest.(check bool) "creator" true (is Room.Creator creator);
  Alcotest.(check bool) "additional creator" true (is Room.Creator additional);
  Alcotest.(check bool) "administrator" true (is Room.Administrator admin);
  Alcotest.(check bool) "moderator" true (is Room.Moderator moderator);
  Alcotest.(check bool) "user" true (is Room.User alice)

let test_invite_details () =
  let inviter = uid "@inviter:example.org" in
  let invite =
    event ~sender:inviter ~event_id:(eid "$invite") ~event_type:"m.room.member"
      ~state_key:(Id.User_id.to_string own_user)
      (obj [ ("membership", Jsont.Json.string "invite") ])
  in
  let profile = member inviter in
  let room = offline_room ~membership:Store.Invited [ invite; profile ] in
  match Room.invite_details room with
  | Error error ->
      Alcotest.failf "invite details failed: %s"
        (Matrix_client.Error.to_string error)
  | Ok details ->
      Alcotest.(check string)
        "inviter" "@inviter:example.org"
        (Id.User_id.to_string details.inviter);
      Alcotest.(check (option string))
        "invite event" (Some "$invite")
        (Option.map Id.Event_id.to_string details.invite_event.event_id);
      Alcotest.(check (option string))
        "inviter profile" (Some "@inviter:example.org")
        (Option.map
           (fun event -> event.Store.state_key)
           details.inviter_profile)

let body_of_request (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.String body -> body
  | Fetch.Empty | Fetch.Stream _ -> Alcotest.fail "expected a string body"

let test_direct_mutation_is_batched_and_idempotent () =
  Eio_mock.Backend.run @@ fun () ->
  let bob = uid "@bob:example.org" in
  let invited = uid "@invited:example.org" in
  let other_room = "!other:example.org" in
  let current =
    ref
      (obj
         [
           (Id.User_id.to_string bob, strings [ Id.Room_id.to_string room_id ]);
           ("@other:example.org", strings [ other_room ]);
         ])
  in
  let gets = ref 0 in
  let puts = ref 0 in
  let handler request =
    match Http.Method.to_string request.Fetch.Middleware.meth with
    | "GET" ->
        incr gets;
        let body =
          Result.get_ok
            (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json !current)
        in
        Fetch_mock.respond body request
    | "PUT" ->
        incr puts;
        current :=
          Result.get_ok
            (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
               (body_of_request request));
        Fetch_mock.respond "{}" request
    | method_ -> Alcotest.failf "unexpected %s request" method_
  in
  let session : Client.session =
    {
      user_id = own_user;
      access_token = "token";
      device_id = Result.get_ok (Id.Device_id.of_string "DEVICE");
      refresh_token = None;
    }
  in
  let state_events =
    [ member own_user; member bob; member ~membership:"invite" invited ]
  in
  let room =
    Room.create ~client:(client ~session handler) ~state:(state state_events)
      room_id
  in
  Result.get_ok (Room.set_is_direct room true);
  Alcotest.(check int) "one read" 1 !gets;
  Alcotest.(check int) "one batched write" 1 !puts;
  let rooms_for user =
    Option.bind (Json.find_mem user !current) Json.as_array
    |> Option.value ~default:[]
    |> List.filter_map Json.as_string
  in
  check_strings "joined target has one association"
    [ Id.Room_id.to_string room_id ]
    (rooms_for "@bob:example.org");
  check_strings "invited target is active too"
    [ Id.Room_id.to_string room_id ]
    (rooms_for "@invited:example.org");
  Result.get_ok (Room.set_is_direct room true);
  Alcotest.(check int) "idempotent call still reads" 2 !gets;
  Alcotest.(check int) "idempotent call does not write" 1 !puts;
  Result.get_ok (Room.set_is_direct room false);
  Alcotest.(check int) "unset is one more write" 2 !puts;
  check_strings "unrelated association survives" [ other_room ]
    (rooms_for "@other:example.org");
  Alcotest.(check bool)
    "empty target dropped" true
    (Option.is_none (Json.find_mem "@bob:example.org" !current));
  Result.get_ok (Room.set_is_direct room false);
  Alcotest.(check int) "repeated unset does not write" 2 !puts

let () =
  Alcotest.run "room conveniences"
    [
      ( "room",
        [
          Alcotest.test_case "routing and permalinks" `Quick
            test_routing_and_permalinks;
          Alcotest.test_case "direct targets and roles" `Quick
            test_direct_targets_and_roles;
          Alcotest.test_case "invite details" `Quick test_invite_details;
          Alcotest.test_case "batched direct mutation" `Quick
            test_direct_mutation_is_batched_and_idempotent;
        ] );
    ]
