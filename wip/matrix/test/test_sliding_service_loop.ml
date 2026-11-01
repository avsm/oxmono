(** Focused loop tests for service-backed simplified sliding sync. *)

module Base = Matrix_client.Base_client
module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Store = Matrix_client.Store
module Service = Matrix_eio.Sync_service
module Sliding = Matrix_eio.Sliding_sync
module Request = Matrix_proto.Sliding_sync.Request
module Response = Matrix_proto.Sliding_sync.Response
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Sync = Matrix_proto.Sync

let user = Id.User_id.of_string_exn "@alice:example.org"
let bob = Id.User_id.of_string_exn "@bob:example.org"
let device = Id.Device_id.of_string_exn "SLIDING_LOOP"
let room_id = Id.Room_id.of_string_exn "!service:example.org"
let homeserver = Uriz.of_string_exn "https://hs.example"

let session : Client.session =
  {
    user_id = user;
    access_token = "syt_sliding_loop";
    device_id = device;
    refresh_token = None;
  }

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let check_string_option label expected actual =
  Alcotest.(check (option string)) label expected actual

let contains ~sub string =
  let sub_length = String.length sub and length = String.length string in
  let rec loop offset =
    offset + sub_length <= length
    && (String.sub string offset sub_length = sub || loop (offset + 1))
  in
  sub_length = 0 || loop 0

let decode_classic json =
  match Jsont_bytesrw.decode_string Sync.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad classic response: %s\n%s" error json

let decode_sliding json =
  match Jsont_bytesrw.decode_string Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad sliding response: %s\n%s" error json

let response ?(lists = "{}") ?(rooms = "{}") ?(extensions = "{}") pos =
  decode_sliding
    (Printf.sprintf {|{"pos":%S,"lists":%s,"rooms":%s,"extensions":%s}|} pos
       lists rooms extensions)

let state_event event_id event_type state_key content =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@alice:example.org","origin_server_ts":1,"type":%S,"state_key":%S,"content":%s}|}
    event_id event_type state_key content

let message event_id timestamp body =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":%d,"type":"m.room.message","content":{"msgtype":"m.text","body":%S}}|}
    event_id timestamp body

let room_exn state room_id =
  match Base.find_room state room_id with
  | Some room -> room
  | None -> Alcotest.failf "room %s is absent" (Id.Room_id.to_string room_id)

let latest_event_id (room : Store.room_info) =
  Option.bind room.latest_event (fun event -> event.Event.Raw_event.event_id)
  |> Option.map Id.Event_id.to_string

let profile_string state user_id field =
  match Base.find_profile_field state user_id field with
  | Some (Jsont.String (value, _)) -> Some value
  | Some _ | None -> None

let seed_common_store store =
  let state = Base.create ~user_id:user () in
  let state, _ =
    Base.apply state (decode_classic {|{"next_batch":"classic-stored"}|})
  in
  let name =
    state_event "$seed-name:example.org" "m.room.name" ""
      {|{"name":"Seed room"}|}
  in
  let rooms =
    Printf.sprintf
      {|{"!service:example.org":{"required_state":[%s],"bump_stamp":1}}|} name
  in
  let extensions =
    {|{
      "to_device":{"next_batch":"stored-to-device","events":[]},
      "org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Stored Bob"}}}}
    }|}
  in
  let state, _ =
    Base.apply_sliding state
      (response ~rooms ~lists:{|{"main":{"count":1}}|} ~extensions
         "stored-position")
  in
  Base.persist store state;
  Service.of_store ~store ~user_id:user ()

let legacy_slot =
  Store.Slot.v ~name:"sliding_sync_state" Matrix_proto.Json.Codec.json

let save_legacy store json =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json json with
  | Error error -> Alcotest.failf "bad legacy fixture: %s" error
  | Ok value -> (
      match Store.Slot.set store legacy_slot value with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "failed to save legacy state: %s"
            (Error.to_string error))

let legacy_state ?(pos = "legacy-position")
    ?(to_device_since = "legacy-to-device") () =
  let event = message "$legacy:example.org" 5 "from legacy state" in
  Printf.sprintf
    {|{"format_version":1,"pos":%S,"to_device_since":%S,"lists":{"legacy":1},"rooms":[{"room_id":"!legacy:example.org","is_invite":false,"highlight_count":0,"notification_count":0,"timeline":[%s],"required_state":[],"bump_stamp":7,"heroes":[]}],"profiles":{"@bob:example.org":{"displayname":"Legacy Bob"}}}|}
    pos to_device_since event

type recorded = { url : string; body : string option }

let body_of_request (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> None
  | Fetch.String body -> Some body
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun request ->
        log :=
          {
            url = Fetch.Middleware.Url.to_string request.url;
            body = body_of_request request;
          }
          :: !log;
        handler request)
  in
  (log, fetch)

let requests log = List.rev !log

let request_body request =
  match request.body with
  | Some body -> (
      match Client.Http.decode_response Request.jsont body with
      | Ok request -> request
      | Error error ->
          Alcotest.failf "bad recorded request: %s" (Error.to_string error))
  | None -> Alcotest.fail "recorded request has no body"

let eio_client ~env ~sw fetch =
  Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
  Matrix_eio.Client.with_session client session

let with_loop test =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> test ~env ~sw

let unexpected_error error =
  Alcotest.failf "unexpected loop error: %a" Matrix_eio.Error.pp_err error

let test_stored_resumption_commit_and_callback_order () =
  let accepted_event =
    message "$accepted:example.org" 10 "accepted by the common fold"
  in
  let rooms =
    Printf.sprintf {|{"!service:example.org":{"timeline":[%s]}}|} accepted_event
  in
  let extensions =
    {|{
      "to_device":{"next_batch":"accepted-to-device","events":[]},
      "org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Accepted Bob"}}}}
    }|}
  in
  let log, fetch =
    mock (fun request ->
        Fetch_mock.respond
          (Printf.sprintf
             {|{"pos":"accepted-position","rooms":%s,"extensions":%s}|} rooms
             extensions)
          request)
  in
  let store = Store.memory () in
  let service = seed_common_store store in
  let order = ref [] in
  let committed_at_change = ref false in
  let changes_seen = ref 0 in
  let raw_saw_change = ref false in
  with_loop @@ fun ~env ~sw ->
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~service ~initial_pos:"caller-position"
    ~on_change:(fun state changes ->
      order := !order @ [ "change" ];
      incr changes_seen;
      let active = Service.state service in
      let active_room = room_exn active room_id in
      let stored_room =
        match Store.find_room store room_id with
        | Some room -> room
        | None -> Alcotest.fail "committed room is absent from store"
      in
      committed_at_change :=
        state == active
        && Base.sliding_pos state = Some "accepted-position"
        && Store.sliding_pos store = Some "accepted-position"
        && latest_event_id active_room = Some "$accepted:example.org"
        && latest_event_id stored_room = Some "$accepted:example.org"
        && profile_string state bob "displayname" = Some "Accepted Bob"
        && List.length changes.room_changes = 1)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            raw_saw_change := !order = [ "change" ];
            order := !order @ [ "raw" ];
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    (Request.v () |> Request.enable_to_device ~since:"caller-to-device");
  Eio.Promise.await done_;
  check_int "one common-state change callback" 1 !changes_seen;
  check_bool "on_change sees state and store committed" true
    !committed_at_change;
  check_bool "raw callback follows on_change" true !raw_saw_change;
  Alcotest.(check (list string)) "callback order" [ "change"; "raw" ] !order;
  let state = Service.state service in
  check_string_option "accepted common cursor" (Some "accepted-position")
    (Base.sliding_pos state);
  check_string_option "classic token remains independent"
    (Some "classic-stored") (Base.next_batch state);
  check_string_option "accepted to-device cursor" (Some "accepted-to-device")
    (Base.sliding_to_device_since state);
  match requests log with
  | [ request ] ->
      check_bool "stored position overrides initial_pos" true
        (contains ~sub:"pos=stored-position" request.url);
      check_bool "caller position is not sent" false
        (contains ~sub:"caller-position" request.url);
      check_string_option "stored to-device token overrides request token"
        (Some "stored-to-device")
        (request_body request).extensions.to_device.since
  | seen -> Alcotest.failf "expected one request, got %d" (List.length seen)

let test_generation_stale_and_controller_cancel () =
  with_loop @@ fun ~env ~sw ->
  let first_started, first_started_resolver = Eio.Promise.create () in
  let release_first, release_first_resolver = Eio.Promise.create () in
  let second_started, second_started_resolver = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  let calls = ref 0 in
  let log, fetch =
    mock (fun request ->
        let call = !calls in
        incr calls;
        match call with
        | 0 ->
            Eio.Promise.resolve first_started_resolver ();
            Eio.Promise.await release_first;
            Fetch_mock.respond
              {|{"pos":"stale-generation","rooms":{"!service:example.org":{}},"extensions":{"io.element.msc4308.thread_subscriptions":{"subscribed":{"!service:example.org":{"$thread:example.org":{"automatic":false,"bump_stamp":12}}},"prev_batch":"older"}}}|}
              request
        | 1 ->
            Eio.Promise.resolve second_started_resolver ();
            Eio.Promise.await never;
            assert false
        | 2 -> Fetch_mock.respond {|{"pos":"accepted-after-restarts"}|} request
        | n -> Alcotest.failf "unexpected request %d" (n + 1))
  in
  let store = Store.memory () in
  let thread_store = Store.memory () in
  let service = seed_common_store store in
  let controller =
    Sliding.Controller.create (Request.v () |> Request.subscribe_room ~room_id)
  in
  let changes = ref 0 and raw = ref 0 and errors = ref 0 in
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~service ~thread_subscription_store:thread_store
    ~on_change:(fun _ _ -> incr changes)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr raw;
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            incr errors;
            Matrix_eio.Sync.Stop);
      }
    controller;
  Eio.Promise.await first_started;
  Service.forget_room service room_id;
  Eio.Promise.resolve release_first_resolver ();
  Eio.Promise.await second_started;
  Sliding.Controller.set_room_subscriptions ~room_ids:[] controller;
  Eio.Promise.await done_;
  check_int "generation-stale response and cancelled poll are refetched" 3
    !calls;
  check_int "only accepted response changes common state" 1 !changes;
  check_int "only accepted response reaches raw callback" 1 !raw;
  check_int "stale/cancel restarts are not errors" 0 !errors;
  check_string_option "stale/cancel responses cannot advance cursor"
    (Some "accepted-after-restarts")
    (Base.sliding_pos (Service.state service));
  check_bool "stale response cannot restore forgotten room" true
    (Base.find_room (Service.state service) room_id = None);
  check_bool "stale response cannot commit thread subscription" true
    (Matrix_client.Thread_subscriptions.find_stored thread_store ~room_id
       ~thread_root:(Id.Event_id.of_string_exn "$thread:example.org")
    = Ok None);
  check_bool "stale response cannot commit thread catch-up" true
    (Matrix_client.Thread_subscriptions.catchup_tokens thread_store = Ok []);
  let sent = requests log in
  check_int "three recorded requests" 3 (List.length sent);
  List.iter
    (fun request ->
      check_bool "every restart keeps last committed position" true
        (contains ~sub:"pos=stored-position" request.url))
    sent;
  check_int "controller cancellation sends empty subscriptions" 0
    (List.length (request_body (List.nth sent 2)).room_subscriptions)

let test_expired_position_resets_only_sliding_session () =
  with_loop @@ fun ~env ~sw ->
  let calls = ref 0 in
  let log, fetch =
    mock (fun request ->
        incr calls;
        match !calls with
        | 1 ->
            Fetch_mock.respond ~status:400
              {|{"errcode":"M_UNKNOWN_POS","error":"expired"}|} request
        | 2 -> Fetch_mock.respond {|{"pos":"fresh-position"}|} request
        | n -> Alcotest.failf "unexpected request %d" n)
  in
  let store = Store.memory () in
  let service = seed_common_store store in
  let controller =
    Sliding.Controller.create
      (Request.v ()
      |> Request.subscribe_room ~room_id
      |> Request.enable_to_device ~since:"caller-to-device")
  in
  let errors = ref 0 and changes = ref 0 and raw = ref 0 in
  let reset_observed = ref false in
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever_controlled ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~service
    ~on_change:(fun _ _ -> incr changes)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            incr raw;
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun error ->
            incr errors;
            let state = Service.state service in
            reset_observed :=
              Sliding.is_expired_pos error
              && Base.sliding_pos state = None
              && Base.sliding_to_device_since state = None
              && Base.sliding_lists state = []
              && Base.next_batch state = Some "classic-stored"
              && Base.find_room state room_id <> None
              && profile_string state bob "displayname" = Some "Stored Bob"
              && (Sliding.Controller.request controller).room_subscriptions = [];
            Matrix_eio.Sync.Continue);
      }
    controller;
  Eio.Promise.await done_;
  check_int "expiry is reported once" 1 !errors;
  check_bool "expiry resets only the sliding session" true !reset_observed;
  check_int "fresh response changes common state once" 1 !changes;
  check_int "only fresh response reaches raw callback" 1 !raw;
  let state = Service.state service in
  check_string_option "fresh position is accepted" (Some "fresh-position")
    (Base.sliding_pos state);
  check_string_option "classic token survives expiry" (Some "classic-stored")
    (Base.next_batch state);
  check_string "room survives expiry" "Seed room"
    (Option.value (room_exn state room_id).name ~default:"");
  check_string_option "profile survives expiry" (Some "Stored Bob")
    (profile_string state bob "displayname");
  let sent = requests log in
  check_int "expiry produces a fresh request" 2 (List.length sent);
  check_bool "first request resumes stored position" true
    (contains ~sub:"pos=stored-position" (List.nth sent 0).url);
  check_bool "fresh request omits expired position" false
    (contains ~sub:"pos=" (List.nth sent 1).url);
  let first_body = request_body (List.nth sent 0) in
  let fresh_body = request_body (List.nth sent 1) in
  check_int "first request carries subscription" 1
    (List.length first_body.room_subscriptions);
  check_int "fresh request clears subscriptions" 0
    (List.length fresh_body.room_subscriptions);
  check_string_option "first request resumes stored to-device token"
    (Some "stored-to-device") first_body.extensions.to_device.since;
  check_string_option "fresh request clears expired to-device token" None
    fresh_body.extensions.to_device.since

let test_startup_migrates_legacy_state () =
  let store = Store.memory () in
  save_legacy store (legacy_state ());
  let service = Service.of_store ~store ~user_id:user () in
  let log, fetch =
    mock (fun request ->
        Fetch_mock.respond {|{"pos":"after-legacy-migration"}|} request)
  in
  let changes = ref 0 in
  with_loop @@ fun ~env ~sw ->
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~service ~initial_pos:"caller-position"
    ~on_change:(fun _ _ -> incr changes)
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    (Request.v () |> Request.enable_to_device ~since:"caller-to-device");
  Eio.Promise.await done_;
  check_int "migration itself does not emit a response change" 1 !changes;
  let state = Service.state service in
  check_string_option "network response advances migrated cursor"
    (Some "after-legacy-migration") (Base.sliding_pos state);
  check_string_option "legacy to-device cursor remains current"
    (Some "legacy-to-device")
    (Base.sliding_to_device_since state);
  check_bool "legacy room is folded into the common state" true
    (Base.find_room state (Id.Room_id.of_string_exn "!legacy:example.org")
    <> None);
  check_string_option "legacy profile is folded into the common state"
    (Some "Legacy Bob")
    (profile_string state bob "displayname");
  check_bool "legacy list metadata is folded into common state" true
    (Base.sliding_lists state = [ ("legacy", 1) ]);
  check_bool "legacy private slot is consumed" true
    (Store.Slot.find store legacy_slot = Ok None);
  match requests log with
  | [ request ] ->
      check_bool "legacy cursor overrides caller initial_pos" true
        (contains ~sub:"pos=legacy-position" request.url);
      check_string_option "legacy to-device cursor overrides caller token"
        (Some "legacy-to-device")
        (request_body request).extensions.to_device.since
  | seen -> Alcotest.failf "expected one request, got %d" (List.length seen)

let test_startup_common_cursor_wins_legacy () =
  let store = Store.memory () in
  ignore (seed_common_store store);
  save_legacy store
    (legacy_state ~pos:"legacy-loser" ~to_device_since:"legacy-loser-device" ());
  let service = Service.of_store ~store ~user_id:user () in
  let log, fetch =
    mock (fun request -> Fetch_mock.respond {|{"pos":"after-common"}|} request)
  in
  with_loop @@ fun ~env ~sw ->
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~service ~initial_pos:"caller-position"
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    (Request.v () |> Request.enable_to_device ~since:"caller-to-device");
  Eio.Promise.await done_;
  let state = Service.state service in
  check_bool "legacy private slot is consumed when common cursor wins" true
    (Store.Slot.find store legacy_slot = Ok None);
  check_bool "losing legacy room is not merged" true
    (Base.find_room state (Id.Room_id.of_string_exn "!legacy:example.org")
    = None);
  check_string_option "common profile wins over legacy snapshot"
    (Some "Stored Bob")
    (profile_string state bob "displayname");
  match requests log with
  | [ request ] ->
      check_bool "existing common cursor wins over legacy and caller" true
        (contains ~sub:"pos=stored-position" request.url);
      check_bool "losing legacy cursor is not sent" false
        (contains ~sub:"legacy-loser" request.url);
      check_string_option "common to-device cursor wins"
        (Some "stored-to-device")
        (request_body request).extensions.to_device.since
  | seen -> Alcotest.failf "expected one request, got %d" (List.length seen)

let test_compatibility_state_store_uses_common_fold () =
  let store = Store.memory () in
  save_legacy store (legacy_state ());
  let log, fetch =
    mock (fun request -> Fetch_mock.respond {|{"pos":"compatibility"}|} request)
  in
  with_loop @@ fun ~env ~sw ->
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
    (eio_client ~env ~sw fetch)
    ~state_store:store
    ~callbacks:
      {
        on_response =
          (fun _ ->
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error = unexpected_error;
      }
    (Request.v ());
  Eio.Promise.await done_;
  let state = Base.of_store store ~user_id:user () in
  check_string_option "compatibility path persists common cursor"
    (Some "compatibility") (Base.sliding_pos state);
  check_bool "compatibility path consumes legacy slot" true
    (Store.Slot.find store legacy_slot = Ok None);
  check_bool "compatibility path migrates legacy room" true
    (Base.find_room state (Id.Room_id.of_string_exn "!legacy:example.org")
    <> None);
  check_int "compatibility path makes one request" 1
    (List.length (requests log))

let test_legacy_migration_restart_and_conflict_rollback () =
  Eio_main.run @@ fun env ->
  let make_dir prefix =
    let path = Filename.temp_file prefix ".d" in
    Sys.remove path;
    Unix.mkdir path 0o700;
    Eio.Path.(Eio.Stdenv.fs env / path)
  in
  let flush label store =
    match Store.flush store with
    | Ok () -> ()
    | Error error -> Alcotest.failf "%s: %s" label (Error.to_string error)
  in
  let dir = make_dir "matrix-sliding-legacy-migration-" in
  let initial = Store.on_disk ~dir in
  save_legacy initial (legacy_state ());
  flush "seed legacy snapshot" initial;
  let migrating = Store.on_disk ~dir in
  let candidate = Base.of_store migrating ~user_id:user () in
  (match Base.migrate_legacy_sliding_state migrating candidate with
  | Ok (candidate, true) ->
      check_string_option "migration publishes legacy position"
        (Some "legacy-position")
        (Base.sliding_pos candidate)
  | Ok (_, false) -> Alcotest.fail "legacy snapshot was not consumed"
  | Error error ->
      Alcotest.failf "legacy migration failed: %s" (Error.to_string error));
  let reopened = Store.on_disk ~dir in
  check_bool "legacy slot removal survives restart" true
    (Store.Slot.find reopened legacy_slot = Ok None);
  let restarted = Base.of_store reopened ~user_id:user () in
  check_string_option "common cursor survives restart" (Some "legacy-position")
    (Base.sliding_pos restarted);
  check_bool "migrated room survives restart" true
    (Base.find_room restarted (Id.Room_id.of_string_exn "!legacy:example.org")
    <> None);

  let conflict_dir = make_dir "matrix-sliding-legacy-conflict-" in
  let seed = Store.on_disk ~dir:conflict_dir in
  save_legacy seed (legacy_state ~pos:"must-remain" ());
  flush "seed conflicting legacy snapshot" seed;
  let stale = Store.on_disk ~dir:conflict_dir in
  let competing = Store.on_disk ~dir:conflict_dir in
  Store.set_next_batch competing "competing-write";
  flush "commit competing writer" competing;
  let stale_state = Base.of_store stale ~user_id:user () in
  (match Base.migrate_legacy_sliding_state stale stale_state with
  | Error (Error.Policy_denied _) -> ()
  | Error error ->
      Alcotest.failf "wrong migration conflict: %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "stale migration unexpectedly committed");
  check_bool "failed migration restores its in-memory legacy slot" true
    (match Store.Slot.find stale legacy_slot with
    | Ok (Some _) -> true
    | Ok None | Error _ -> false);
  check_bool "failed migration leaves the disk legacy slot intact" true
    (match Store.Slot.find (Store.on_disk ~dir:conflict_dir) legacy_slot with
    | Ok (Some _) -> true
    | Ok None | Error _ -> false)

let test_service_default_presence_wakes_poll () =
  with_loop @@ fun ~env ~sw ->
  let first_started, first_started_resolver = Eio.Promise.create () in
  let release_first, release_first_resolver = Eio.Promise.create () in
  let replacement_started, replacement_started_resolver =
    Eio.Promise.create ()
  in
  let calls = ref 0 in
  let log, fetch =
    mock (fun request ->
        let call = !calls in
        incr calls;
        if call = 0 then begin
          Eio.Promise.resolve first_started_resolver ();
          Eio.Promise.await release_first;
          Fetch_mock.respond {|{"pos":"uncancelled-first"}|} request
        end
        else begin
          Eio.Promise.resolve replacement_started_resolver ();
          Fetch_mock.respond {|{"pos":"accepted-after-presence"}|} request
        end)
  in
  let store = Store.memory () in
  let service = seed_common_store store in
  let client = eio_client ~env ~sw fetch in
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  let delivered = ref [] and changes = ref 0 and errors = ref 0 in
  let done_, done_resolver = Eio.Promise.create () in
  Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client ~service
    ~on_change:(fun _ _ -> incr changes)
    ~callbacks:
      {
        on_response =
          (fun response ->
            delivered := response.Response.pos :: !delivered;
            Eio.Promise.resolve done_resolver ();
            Matrix_eio.Sync.Stop);
        on_error =
          (fun _ ->
            incr errors;
            Matrix_eio.Sync.Stop);
      }
    (Request.v ());
  Eio.Promise.await first_started;
  Client.set_sync_presence (Matrix_eio.Client.base client) `Offline;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  check_int "no-op presence update leaves poll running" 1 !calls;
  Client.set_sync_presence (Matrix_eio.Client.base client) `Unavailable;
  for _ = 1 to 8 do
    Eio.Fiber.yield ()
  done;
  let woke = !calls = 2 in
  (* This also gives a missing-wakeup implementation a deterministic way out,
     so the failing test cannot leave the suite blocked forever. *)
  Eio.Promise.resolve release_first_resolver ();
  if woke then Eio.Promise.await replacement_started;
  Eio.Promise.await done_;
  check_bool "effective presence change starts a replacement request" true woke;
  check_int "presence restart is not an error" 0 !errors;
  check_int "only the replacement response changes common state" 1 !changes;
  Alcotest.(check (list string))
    "only replacement response is delivered"
    [ "accepted-after-presence" ]
    (List.rev !delivered);
  check_string_option "only replacement cursor commits"
    (Some "accepted-after-presence")
    (Base.sliding_pos (Service.state service));
  let sent = requests log in
  check_int "presence change creates exactly one replacement" 2
    (List.length sent);
  check_bool "first request uses old presence and stored position" true
    (contains ~sub:"set_presence=offline" (List.nth sent 0).url
    && contains ~sub:"pos=stored-position" (List.nth sent 0).url);
  check_bool "replacement uses latest presence and same position" true
    (contains ~sub:"set_presence=unavailable" (List.nth sent 1).url
    && contains ~sub:"pos=stored-position" (List.nth sent 1).url);
  Client.set_sync_presence (Matrix_eio.Client.base client) `Online;
  for _ = 1 to 4 do
    Eio.Fiber.yield ()
  done;
  check_int "terminated loop removes presence listener" 2 !calls

let test_service_rejects_legacy_state_options () =
  let run incompatible =
    let requested = ref 0 in
    let rejected =
      try
        with_loop @@ fun ~env ~sw ->
        let _, fetch =
          mock (fun request ->
              incr requested;
              Fetch_mock.respond {|{"pos":"unexpected"}|} request)
        in
        let service = Service.of_user ~user_id:user () in
        let callbacks : Response.t Matrix_eio.Sync.callbacks =
          {
            on_response = (fun _ -> Matrix_eio.Sync.Stop);
            on_error = (fun _ -> Matrix_eio.Sync.Stop);
          }
        in
        let client = eio_client ~env ~sw fetch in
        let encryption () =
          Matrix_eio.Encryption.create
            ~random:(Client.random (Matrix_eio.Client.base client))
            ~user_id:user ~device_id:device ()
        in
        (match incompatible with
        | `State_store ->
            Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
              ~service ~state_store:(Store.memory ()) ~callbacks (Request.v ())
        | `Profile_service ->
            Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
              ~service ~profile_service:service ~callbacks (Request.v ())
        | `Encryption ->
            Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
              ~encryption:(encryption ()) ~callbacks (Request.v ())
        | `Verification ->
            let encryption = encryption () in
            let verification =
              Matrix_eio.Verification_service.create ~client ~encryption ()
            in
            Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
              ~verification ~callbacks (Request.v ())
        | `On_change ->
            Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env) client
              ~on_change:(fun _ _ -> ())
              ~callbacks (Request.v ()));
        false
      with Invalid_argument _ -> true
    in
    (rejected, !requested)
  in
  let state_rejected, state_requests = run `State_store in
  let profile_rejected, profile_requests = run `Profile_service in
  let encryption_rejected, encryption_requests = run `Encryption in
  let verification_rejected, verification_requests = run `Verification in
  let on_change_rejected, on_change_requests = run `On_change in
  check_bool "service rejects legacy state_store" true state_rejected;
  check_bool "service rejects legacy profile_service" true profile_rejected;
  check_bool "compatibility mode accepts service-only encryption" false
    encryption_rejected;
  check_bool "compatibility mode accepts service-only verification" false
    verification_rejected;
  check_bool "compatibility mode accepts service-only on_change" false
    on_change_rejected;
  check_int "state_store rejection precedes network" 0 state_requests;
  check_int "profile_service rejection precedes network" 0 profile_requests;
  check_int "compatibility encryption reaches canonical loop" 2
    encryption_requests;
  check_int "compatibility verification reaches canonical loop" 1
    verification_requests;
  check_int "compatibility on_change reaches canonical loop" 1
    on_change_requests

exception Change_callback_failed

let test_change_callback_exception_propagates_after_commit () =
  let requested = ref 0 in
  let _, fetch =
    mock (fun request ->
        incr requested;
        Fetch_mock.respond {|{"pos":"committed-before-callback-failure"}|}
          request)
  in
  let store = Store.memory () in
  let service = Service.of_store ~store ~user_id:user () in
  let errors = ref 0 and raw = ref 0 in
  let raised =
    try
      with_loop @@ fun ~env ~sw ->
      Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~env ~sw fetch)
        ~service
        ~on_change:(fun _ _ -> raise Change_callback_failed)
        ~callbacks:
          {
            on_response =
              (fun _ ->
                incr raw;
                Matrix_eio.Sync.Stop);
            on_error =
              (fun _ ->
                incr errors;
                Matrix_eio.Sync.Stop);
          }
        (Request.v ());
      false
    with Change_callback_failed -> true
  in
  check_bool "on_change exception escapes the loop" true raised;
  check_int "on_change exception is not routed to on_error" 0 !errors;
  check_int "raw callback does not follow failed on_change" 0 !raw;
  check_int "response was fetched exactly once" 1 !requested;
  check_string_option "state commits before on_change"
    (Some "committed-before-callback-failure")
    (Base.sliding_pos (Service.state service));
  check_string_option "store commits before on_change"
    (Some "committed-before-callback-failure") (Store.sliding_pos store)

let make_temp_dir env prefix =
  let native_path = Filename.temp_file prefix ".d" in
  Unix.unlink native_path;
  Unix.mkdir native_path 0o700;
  (native_path, Eio.Path.(Eio.Stdenv.fs env / native_path))

let remove_temp_dir native_path =
  (try
     Sys.readdir native_path
     |> Array.iter (fun name ->
         try Unix.unlink (Filename.concat native_path name) with _ -> ())
   with _ -> ());
  try Unix.rmdir native_path with _ -> ()

let test_persistence_failure_retries_without_advancing () =
  Eio_main.run @@ fun env ->
  let native_path, dir = make_temp_dir env "matrix-sliding-loop-failure" in
  Fun.protect
    ~finally:(fun () -> remove_temp_dir native_path)
    (fun () ->
      let store = Store.on_disk ~dir in
      let service = Service.create ~store (Base.create ~user_id:user ()) in
      Unix.rmdir native_path;
      Eio.Switch.run @@ fun sw ->
      let calls = ref 0 in
      let log, fetch =
        mock (fun request ->
            incr calls;
            match !calls with
            | 1 ->
                Fetch_mock.respond
                  {|{"pos":"not-saved","rooms":{"!failed:example.org":{}}}|}
                  request
            | 2 ->
                Fetch_mock.respond
                  {|{"pos":"saved-after-retry","rooms":{"!service:example.org":{}}}|}
                  request
            | n -> Alcotest.failf "unexpected request %d" n)
      in
      let errors = ref 0 and changes = ref 0 and raw = ref 0 in
      let failure_was_unpublished = ref false in
      let done_, done_resolver = Eio.Promise.create () in
      Sliding.sync_forever ~sw ~clock:(Eio.Stdenv.clock env)
        (eio_client ~env ~sw fetch)
        ~service
        ~on_change:(fun _ _ -> incr changes)
        ~callbacks:
          {
            on_response =
              (fun _ ->
                incr raw;
                Eio.Promise.resolve done_resolver ();
                Matrix_eio.Sync.Stop);
            on_error =
              (fun _ ->
                incr errors;
                failure_was_unpublished :=
                  Base.sliding_pos (Service.state service) = None
                  && Base.find_room (Service.state service)
                       (Id.Room_id.of_string_exn "!failed:example.org")
                     = None;
                if not (Sys.file_exists native_path) then
                  Unix.mkdir native_path 0o700;
                Matrix_eio.Sync.Continue);
          }
        (Request.v ());
      Eio.Promise.await done_;
      check_int "one persistence error is retried" 1 !errors;
      check_bool "failed cursor and room are not published" true
        !failure_was_unpublished;
      check_int "only persisted response invokes on_change" 1 !changes;
      check_int "only persisted response invokes raw callback" 1 !raw;
      check_string_option "retry commits its cursor" (Some "saved-after-retry")
        (Base.sliding_pos (Service.state service));
      check_bool "failed response room stays absent" true
        (Base.find_room (Service.state service)
           (Id.Room_id.of_string_exn "!failed:example.org")
        = None);
      check_bool "retry room is committed" true
        (Base.find_room (Service.state service) room_id <> None);
      let sent = requests log in
      check_int "persistence retry refetches response" 2 (List.length sent);
      List.iter
        (fun request ->
          check_bool "failed persistence does not advance request cursor" false
            (contains ~sub:"pos=" request.url))
        sent)

let () =
  Alcotest.run "sliding_service_loop"
    [
      ( "commit",
        [
          Alcotest.test_case "stored resume and callback order" `Quick
            test_stored_resumption_commit_and_callback_order;
          Alcotest.test_case "persistence failure retry" `Quick
            test_persistence_failure_retries_without_advancing;
          Alcotest.test_case "on_change exception after commit" `Quick
            test_change_callback_exception_propagates_after_commit;
        ] );
      ( "control",
        [
          Alcotest.test_case "generation stale and controller cancel" `Quick
            test_generation_stale_and_controller_cancel;
          Alcotest.test_case "M_UNKNOWN_POS reset" `Quick
            test_expired_position_resets_only_sliding_session;
          Alcotest.test_case "incompatible option rejection" `Quick
            test_service_rejects_legacy_state_options;
          Alcotest.test_case "default presence wakes poll" `Quick
            test_service_default_presence_wakes_poll;
        ] );
      ( "migration",
        [
          Alcotest.test_case "legacy snapshot startup migration" `Quick
            test_startup_migrates_legacy_state;
          Alcotest.test_case "common cursor wins legacy snapshot" `Quick
            test_startup_common_cursor_wins_legacy;
          Alcotest.test_case "compatibility state store uses common fold" `Quick
            test_compatibility_state_store_uses_common_fold;
          Alcotest.test_case "disk restart and conflict rollback" `Quick
            test_legacy_migration_restart_and_conflict_rollback;
        ] );
    ]
