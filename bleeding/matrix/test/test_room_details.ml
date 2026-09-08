module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Store = Matrix_client.Store
module Base_client = Matrix_client.Base_client
module Client = Matrix_client.Client
module Details = Matrix_client.Room_details

let uid value = Result.get_ok (Id.User_id.of_string value)
let rid value = Result.get_ok (Id.Room_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)
let own_user = uid "@alice:example.org"
let room_id = rid "!room:example.org"

let json value =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json value)

let state_event ?sender event_type state_key content : Store.state_event =
  {
    event_type = Event.Event_type.of_string event_type;
    state_key;
    content = json content;
    sender;
    event_id = None;
    origin_server_ts = None;
  }

let member ?(membership = "join") user content =
  state_event "m.room.member" user
    (Printf.sprintf {|{"membership":%S%s}|} membership content)

let cached_state ~complete state_events =
  let store = Store.memory () in
  let room = Store.empty_room_info ~room_id ~membership:Store.Joined in
  Store.set_room store { room with state_events; members_complete = complete };
  Base_client.of_store store ~user_id:own_user ()

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 1024 'r')
  end

let client handler =
  let fetch = Fetch_mock.client handler in
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch
    ~random:(Matrix_client.Random.of_env random_env)
  |> fun client ->
  Client.with_session client
    {
      user_id = own_user;
      access_token = "token";
      device_id = did "DEVICE";
      refresh_token = None;
    }

let find_member details user =
  List.find
    (fun (member : Details.member) ->
      String.equal (Id.User_id.to_string member.user_id) user)
    (Details.members details)

let member_ids details =
  Details.members details
  |> List.map (fun (member : Details.member) ->
      Id.User_id.to_string member.user_id)

let run test () = Eio_mock.Backend.run test

let test_cached_projection_and_no_request () =
  let state =
    cached_state ~complete:true
      [
        member "@alice:example.org" {|,"displayname":"Sam"|};
        member "@bob:example.org" {|,"displayname":"Sam"|};
        member "@admin:example.org" {|,"displayname":"Admin"|};
        member "@creator:example.org" {|,"displayname":"Creator"|};
        member "@bot:example.org" "";
        member ~membership:"leave" "@gone:example.org" "";
        member "not-a-user" {|,"displayname":"Broken"|};
        member ~membership:"future" "@future:example.org" "";
        state_event "m.room.power_levels" ""
          {|{"users":{"@admin:example.org":100,"@bob:example.org":50}}|};
        state_event
          ~sender:(uid "@creator:example.org")
          "m.room.create" "" {|{"room_version":"12"}|};
        state_event "m.room.member_hints" ""
          {|{"service_members":["@bot:example.org"]}|};
      ]
  in
  let requests = ref 0 in
  let handler (request : Fetch.Middleware.request) =
    incr requests;
    Alcotest.failf "unexpected request to %s"
      (Fetch.Middleware.Url.to_string request.url)
  in
  let details = Details.create ~client:(client handler) ~state room_id in
  Alcotest.(check bool)
    "members are complete" true
    (Details.members_complete details);
  Alcotest.(check (list string))
    "active members sorted"
    [
      "@admin:example.org";
      "@alice:example.org";
      "@bob:example.org";
      "@bot:example.org";
      "@creator:example.org";
    ]
    (member_ids details);
  let alice = find_member details "@alice:example.org" in
  let bob = find_member details "@bob:example.org" in
  let bot = find_member details "@bot:example.org" in
  let admin = find_member details "@admin:example.org" in
  let creator = find_member details "@creator:example.org" in
  Alcotest.(check string)
    "duplicate label includes ID" "Sam (@alice:example.org)" alice.display_label;
  Alcotest.(check bool)
    "duplicate marked ambiguous" true bob.display_name_ambiguous;
  Alcotest.(check string)
    "missing name falls back to ID" "@bot:example.org" bot.display_label;
  Alcotest.(check bool)
    "only account user flagged" true
    (alice.is_account_user && not bob.is_account_user);
  Alcotest.(check bool) "service hint applied" true bot.is_service_member;
  Alcotest.(check int)
    "one service member" 1
    (Details.service_member_count details);
  Alcotest.(check int)
    "four human members" 4
    (Details.human_member_count details);
  Alcotest.(check bool)
    "moderator role" true
    (bob.role = Matrix_client.Room.Moderator);
  Alcotest.(check bool)
    "administrator role" true
    (admin.role = Matrix_client.Room.Administrator);
  Alcotest.(check bool)
    "creator role" true
    (creator.role = Matrix_client.Room.Creator);
  ignore (Result.get_ok (Details.ensure_members details));
  Alcotest.(check int) "complete cache makes no request" 0 !requests

let test_lazy_members_are_replaced () =
  let state =
    cached_state ~complete:false
      [ member "@stale:example.org" {|,"displayname":"Stale"|} ]
  in
  let requests = ref [] in
  let handler (request : Fetch.Middleware.request) =
    requests := Fetch.Middleware.Url.to_string request.url :: !requests;
    Fetch_mock.respond ~status:200
      {|{"chunk":[{"state_key":"@alice:example.org","content":{"membership":"join","displayname":"Alice","avatar_url":"mxc://example.org/avatar"}},{"state_key":"@new:example.org","content":{"membership":"invite","displayname":"New"}}]}|}
      request
  in
  let original = Details.create ~client:(client handler) ~state room_id in
  let refreshed = Result.get_ok (Details.ensure_members original) in
  Alcotest.(check bool)
    "original remains partial" false
    (Details.members_complete original);
  Alcotest.(check bool)
    "returned facade is complete" true
    (Details.members_complete refreshed);
  Alcotest.(check (list string))
    "authoritative response replaces stale cache"
    [ "@alice:example.org"; "@new:example.org" ]
    (member_ids refreshed);
  let alice = find_member refreshed "@alice:example.org" in
  Alcotest.(check (option string))
    "avatar retained" (Some "mxc://example.org/avatar")
    (Option.map Matrix_client.Media.Mxc.to_string alice.avatar_url);
  Alcotest.(check int) "one members request" 1 (List.length !requests);
  Alcotest.(check string)
    "members URL"
    "https://hs.example/_matrix/client/v3/rooms/!room:example.org/members"
    (List.hd !requests)

let test_failed_refresh_preserves_original () =
  let state = cached_state ~complete:false [ member "@alice:example.org" "" ] in
  let requests = ref 0 in
  let handler request =
    incr requests;
    Fetch_mock.respond ~status:500 "{}" request
  in
  let details = Details.create ~client:(client handler) ~state room_id in
  Alcotest.(check bool)
    "refresh fails" true
    (Result.is_error (Details.ensure_members details));
  Alcotest.(check bool)
    "original stays partial" false
    (Details.members_complete details);
  Alcotest.(check (list string))
    "original cache remains" [ "@alice:example.org" ] (member_ids details);
  Alcotest.(check int) "one failed request" 1 !requests

let test_unknown_room_fails_locally () =
  let state = Base_client.create ~user_id:own_user () in
  let requests = ref 0 in
  let handler request =
    incr requests;
    Fetch_mock.respond ~status:200 {|{"chunk":[]}|} request
  in
  let details = Details.create ~client:(client handler) ~state room_id in
  Alcotest.(check bool)
    "unknown room fails" true
    (Result.is_error (Details.ensure_members details));
  Alcotest.(check int) "no request" 0 !requests

let () =
  Alcotest.run "room details"
    [
      ( "members",
        [
          Alcotest.test_case "cached projection" `Quick
            (run test_cached_projection_and_no_request);
          Alcotest.test_case "lazy members refresh" `Quick
            (run test_lazy_members_are_replaced);
          Alcotest.test_case "failed refresh" `Quick
            (run test_failed_refresh_preserves_original);
          Alcotest.test_case "unknown room" `Quick
            (run test_unknown_room_fails_locally);
        ] );
    ]
