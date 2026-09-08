(** Focused transaction tests for {!Matrix_eio.Sync_service.apply_sliding}. *)

module Base = Matrix_client.Base_client
module Store = Matrix_client.Store
module Service = Matrix_eio.Sync_service
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Sync = Matrix_proto.Sync
module Sliding = Matrix_proto.Sliding_sync

let user = Id.User_id.of_string_exn "@alice:example.org"
let bob = Id.User_id.of_string_exn "@bob:example.org"
let device = Id.Device_id.of_string_exn "SLIDING"
let room_id = Id.Room_id.of_string_exn "!service:example.org"
let homeserver = Uriz.of_string_exn "https://hs.example"

let session : Matrix_client.Client.session =
  {
    user_id = user;
    access_token = "syt_sliding";
    device_id = device;
    refresh_token = None;
  }

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let check_string_option label expected actual =
  Alcotest.(check (option string)) label expected actual

let decode_classic json =
  match Jsont_bytesrw.decode_string Sync.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad classic response: %s\n%s" error json

let decode_sliding json =
  match Jsont_bytesrw.decode_string Sliding.Response.jsont json with
  | Ok response -> response
  | Error error -> Alcotest.failf "bad sliding response: %s\n%s" error json

let response ?(lists = "{}") ?(rooms = "{}") ?(extensions = "{}") pos =
  decode_sliding
    (Printf.sprintf {|{"pos":%S,"lists":%s,"rooms":%s,"extensions":%s}|} pos
       lists rooms extensions)

let profile_response pos display_name =
  let extensions =
    Printf.sprintf
      {|{"org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":%S}}}}}|}
      display_name
  in
  response ~extensions pos

let message event_id timestamp body =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":%d,"type":"m.room.message","content":{"msgtype":"m.text","body":%S}}|}
    event_id timestamp body

let state_event event_id event_type state_key content =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@alice:example.org","origin_server_ts":1,"type":%S,"state_key":%S,"content":%s}|}
    event_id event_type state_key content

let room_exn state =
  match Base.find_room state room_id with
  | Some room -> room
  | None -> Alcotest.fail "service room is absent"

let event_id_exn (event : Event.Raw_event.t) =
  match event.event_id with
  | Some event_id -> Id.Event_id.to_string event_id
  | None -> Alcotest.fail "hook event has no event id"

let profile_string state user_id field =
  match Base.find_profile_field state user_id field with
  | Some (Jsont.String (value, _)) -> value
  | Some _ -> Alcotest.failf "profile field %S is not a string" field
  | None -> Alcotest.failf "profile field %S is absent" field

let store_profile_string store user_id field =
  let profile =
    Store.profiles store
    |> List.find_opt (fun (candidate, _) -> Id.User_id.equal candidate user_id)
  in
  match profile with
  | None -> None
  | Some (_, fields) -> (
      match List.assoc_opt field fields with
      | Some (Jsont.String (value, _)) -> Some value
      | Some _ | None -> None)

let client ~env ~sw =
  let fetch =
    Fetch_mock.client (fun _ ->
        Alcotest.fail "direct response application made an HTTP request")
  in
  Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
  Matrix_eio.Client.with_session client session

let with_eio test =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> test ~env ~sw

let flush store =
  match Store.flush store with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "store flush failed: %s"
        (Matrix_client.Error.to_string error)

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

let test_commit_and_hook_order () =
  with_eio @@ fun ~env ~sw ->
  let client = client ~env ~sw in
  let store = Store.memory () in
  let service = Service.create ~store (Base.create ~user_id:user ()) in
  ignore
    (Service.apply client service
       (decode_classic {|{"next_batch":"classic-service"}|}));
  let hook_order = ref [] in
  let room_hook_committed = ref false in
  let response_hook_committed = ref false in
  let profile_observations = ref [] in
  Service.on_room_event service (fun _ event ->
      hook_order := !hook_order @ [ "room:" ^ event_id_exn event ];
      room_hook_committed :=
        Base.sliding_pos (Service.state service) = Some "sliding-service-1"
        && Store.sliding_pos store = Some "sliding-service-1");
  Service.on_sliding_response service (fun state _ _ ->
      hook_order := !hook_order @ [ "response" ];
      response_hook_committed :=
        state == Service.state service
        && Base.sliding_pos state = Some "sliding-service-1"
        && Store.sliding_pos store = Some "sliding-service-1");
  Service.on_profile_change service (fun state changes ->
      profile_observations :=
        ( List.length changes,
          Base.sliding_pos state,
          Store.sliding_pos store,
          store_profile_string store bob "displayname" )
        :: !profile_observations);
  let timeline =
    String.concat ","
      [
        message "$service-1:example.org" 1 "one";
        message "$service-2:example.org" 2 "two";
      ]
  in
  let rooms =
    Printf.sprintf
      {|{"!service:example.org":{"timeline":[%s],"bump_stamp":10}}|} timeline
  in
  let extensions =
    {|{"org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Service Bob"}}}}}|}
  in
  let changes =
    Service.apply_sliding client service
      (response ~rooms ~lists:{|{"main":{"count":1}}|} ~extensions
         "sliding-service-1")
  in
  let state = Service.state service in
  check_string_option "active sliding cursor committed"
    (Some "sliding-service-1") (Base.sliding_pos state);
  check_string_option "common store cursor committed" (Some "sliding-service-1")
    (Store.sliding_pos store);
  check_string_option "classic token remains independent"
    (Some "classic-service") (Base.next_batch state);
  check_string_option "stored classic token remains independent"
    (Some "classic-service") (Store.next_batch store);
  check_bool "room committed to active state" true
    (Base.find_room state room_id <> None);
  check_bool "room committed to common store" true
    (Store.find_room store room_id <> None);
  check_string "profile committed to state" "Service Bob"
    (profile_string state bob "displayname");
  check_string_option "profile committed to store" (Some "Service Bob")
    (store_profile_string store bob "displayname");
  check_int "profile appears once in response changes" 1
    (List.length changes.profile_changes);
  Alcotest.(check (list string))
    "room events precede sliding response hook"
    [ "room:$service-1:example.org"; "room:$service-2:example.org"; "response" ]
    !hook_order;
  check_bool "room hook runs after common commit" true !room_hook_committed;
  check_bool "response hook runs after common commit" true
    !response_hook_committed;
  (match List.rev !profile_observations with
  | [ (count, state_pos, store_pos, stored_name) ] ->
      check_int "profile hook carries one change" 1 count;
      check_string_option "profile hook sees active commit"
        (Some "sliding-service-1") state_pos;
      check_string_option "profile hook sees store commit"
        (Some "sliding-service-1") store_pos;
      check_string_option "profile hook sees persisted profile"
        (Some "Service Bob") stored_name
  | observations ->
      Alcotest.failf "expected one profile-hook call, got %d"
        (List.length observations));
  let replay_changes =
    Service.apply_sliding client service
      (response ~extensions "sliding-service-2")
  in
  check_int "semantic profile replay reports no change" 0
    (List.length replay_changes.profile_changes);
  check_int "semantic profile replay does not notify profile hook again" 1
    (List.length !profile_observations)

let test_profile_change_subscriptions () =
  with_eio @@ fun ~env ~sw ->
  let client = client ~env ~sw in
  let store = Store.memory () in
  let initial = Service.create ~store (Base.create ~user_id:user ()) in
  ignore
    (Service.apply_sliding client initial (profile_response "profile-1" "One"));
  (* Recreate the service from the common store to ensure subscriptions see
     changes made after a persisted profile has been resumed. *)
  let service = Service.of_store ~store ~user_id:user () in
  check_string "restored profile" "One"
    (profile_string (Service.state service) bob "displayname");
  let self_calls = ref 0 in
  let stable_calls = ref 0 in
  let raising_calls = ref 0 in
  let self_subscription = ref None in
  let self =
    Service.subscribe_profile_changes service (fun _state changes ->
        incr self_calls;
        check_int "self callback receives one effective change" 1
          (List.length changes);
        match !self_subscription with
        | None -> Alcotest.fail "self subscription was not installed"
        | Some subscription ->
            Service.unsubscribe_profile_changes service subscription;
            Service.unsubscribe_profile_changes service subscription)
  in
  self_subscription := Some self;
  let stable =
    Service.subscribe_profile_changes service (fun _state changes ->
        incr stable_calls;
        check_int "stable callback receives one effective change" 1
          (List.length changes))
  in
  ignore
    (Service.subscribe_profile_changes service (fun _state _changes ->
         incr raising_calls;
         failwith "profile subscriber failure"));
  ignore
    (Service.apply_sliding client service (profile_response "profile-2" "Two"));
  check_int "sliding profile update notifies self once" 1 !self_calls;
  check_int "sliding profile update notifies stable subscriber" 1 !stable_calls;
  check_int "raising subscriber is isolated" 1 !raising_calls;
  (* The position changes, but the semantic profile delta is a no-op. *)
  ignore
    (Service.apply_sliding client service (profile_response "profile-3" "Two"));
  check_int "profile replay does not notify self" 1 !self_calls;
  check_int "profile replay does not notify stable subscriber" 1 !stable_calls;
  check_int "profile replay does not notify raising subscriber" 1 !raising_calls;
  Service.unsubscribe_profile_changes service stable;
  Service.unsubscribe_profile_changes service stable;
  ignore
    (Service.apply_sliding client service (profile_response "profile-4" "Four"));
  check_int "self unsubscribe suppresses future callbacks" 1 !self_calls;
  check_int "explicit unsubscribe suppresses future callbacks" 1 !stable_calls;
  check_int "raising subscriber remains isolated after another unsubscribe" 2
    !raising_calls

let test_reset_and_stale_generation () =
  with_eio @@ fun ~env ~sw ->
  let client = client ~env ~sw in
  let store = Store.memory () in
  let service = Service.create ~store (Base.create ~user_id:user ()) in
  ignore
    (Service.apply client service
       (decode_classic {|{"next_batch":"classic-before-reset"}|}));
  let room_name =
    state_event "$service-name:example.org" "m.room.name" ""
      {|{"name":"Kept room"}|}
  in
  let rooms =
    Printf.sprintf {|{"!service:example.org":{"required_state":[%s]}}|}
      room_name
  in
  let extensions =
    {|{
      "to_device":{"next_batch":"to-device-before-reset","events":[]},
      "org.matrix.msc4262.profiles":{"users":{"@bob:example.org":{"updated":{"displayname":"Kept Bob"}}}}
    }|}
  in
  ignore
    (Service.apply_sliding client service
       (response ~rooms ~lists:{|{"main":{"count":1}}|} ~extensions
          "sliding-before-reset"));
  let stale_generation = Service.generation service in
  Service.reset_sliding_session service;
  check_int "reset advances lifecycle generation" (stale_generation + 1)
    (Service.generation service);
  let state = Service.state service in
  check_string_option "reset clears sliding cursor" None
    (Base.sliding_pos state);
  check_string_option "reset clears to-device cursor" None
    (Base.sliding_to_device_since state);
  Alcotest.(check (list (pair string int)))
    "reset clears list metadata" [] (Base.sliding_lists state);
  check_string_option "reset preserves classic token"
    (Some "classic-before-reset") (Base.next_batch state);
  check_string "reset preserves room" "Kept room"
    (Option.value (room_exn state).name ~default:"");
  check_string "reset preserves profile" "Kept Bob"
    (profile_string state bob "displayname");
  check_string_option "store reset clears sliding cursor" None
    (Store.sliding_pos store);
  check_string_option "store reset clears to-device cursor" None
    (Store.sliding_to_device_since store);
  check_string_option "store reset preserves classic token"
    (Some "classic-before-reset") (Store.next_batch store);
  check_bool "store reset preserves room" true
    (Store.find_room store room_id <> None);
  check_string_option "store reset preserves profile" (Some "Kept Bob")
    (store_profile_string store bob "displayname");
  let sliding_hooks = ref 0 in
  Service.on_sliding_response service (fun _ _ _ -> incr sliding_hooks);
  let stale =
    Service.apply_sliding_if_current stale_generation client service
      (response ~rooms:{|{"!stale:example.org":{}}|} "stale-position")
  in
  check_bool "stale generation rejects response" true (stale = None);
  check_string_option "stale response cannot restore sliding cursor" None
    (Base.sliding_pos (Service.state service));
  check_bool "stale response cannot invent a room" true
    (Base.find_room (Service.state service)
       (Id.Room_id.of_string_exn "!stale:example.org")
    = None);
  check_int "stale response runs no hooks" 0 !sliding_hooks;
  let restored = Base.of_store store ~user_id:user () in
  check_string_option "reset survives common-store reload" None
    (Base.sliding_pos restored);
  check_string "room survives common-store reload" "Kept room"
    (Option.value (room_exn restored).name ~default:"");
  check_string "profile survives common-store reload" "Kept Bob"
    (profile_string restored bob "displayname")

let test_to_device_disabled () =
  with_eio @@ fun ~env ~sw ->
  let client = client ~env ~sw in
  let store = Store.memory () in
  let service = Service.create ~store (Base.create ~user_id:user ()) in
  let disabled_extensions =
    {|{"to_device":{"next_batch":"disabled-token","events":[{"type":"m.test","content":{"n":1}}]}}|}
  in
  let changes =
    Service.apply_sliding ~to_device_enabled:false client service
      (response ~extensions:disabled_extensions "disabled-position")
  in
  check_string_option "disabled response advances sliding position"
    (Some "disabled-position")
    (Base.sliding_pos (Service.state service));
  check_string_option "disabled response does not advance to-device cursor" None
    (Base.sliding_to_device_since (Service.state service));
  check_string_option "disabled cursor is not persisted" None
    (Store.sliding_to_device_since store);
  check_int "disabled response still exposes to-device events" 1
    (List.length changes.to_device);
  let enabled_extensions =
    {|{"to_device":{"next_batch":"enabled-token","events":[]}}|}
  in
  ignore
    (Service.apply_sliding client service
       (response ~extensions:enabled_extensions "enabled-position"));
  check_string_option "enabled response advances to-device cursor"
    (Some "enabled-token")
    (Base.sliding_to_device_since (Service.state service));
  check_string_option "enabled cursor is persisted" (Some "enabled-token")
    (Store.sliding_to_device_since store)

let test_persistence_failure_does_not_publish () =
  with_eio @@ fun ~env ~sw ->
  let client = client ~env ~sw in
  let native_path, dir = make_temp_dir env "matrix-sliding-service-failure" in
  Fun.protect
    ~finally:(fun () -> remove_temp_dir native_path)
    (fun () ->
      let initial = Store.on_disk ~dir in
      Store.set_next_batch initial "classic-on-disk";
      Store.replace_sliding_session initial ~pos:(Some "sliding-on-disk")
        ~to_device_since:None ~lists:[];
      flush initial;
      let service_store = Store.on_disk ~dir in
      let competing = Store.on_disk ~dir in
      let service = Service.of_store ~store:service_store ~user_id:user () in
      let hooks = ref 0 in
      Service.on_room_event service (fun _ _ -> incr hooks);
      Service.on_sliding_response service (fun _ _ _ -> incr hooks);
      Store.replace_sliding_session competing ~pos:(Some "external-position")
        ~to_device_since:None ~lists:[];
      flush competing;
      let failed =
        try
          ignore
            (Service.apply_sliding client service
               (response
                  ~rooms:
                    (Printf.sprintf
                       {|{"!service:example.org":{"timeline":[%s]}}|}
                       (message "$not-committed:example.org" 3 "not committed"))
                  "candidate-position"));
          false
        with Eio.Io _ -> true
      in
      check_bool "stale common-store writer rejects commit" true failed;
      check_string_option "failed commit leaves active sliding cursor"
        (Some "sliding-on-disk")
        (Base.sliding_pos (Service.state service));
      check_string_option "failed commit leaves active classic token"
        (Some "classic-on-disk")
        (Base.next_batch (Service.state service));
      check_string_option "failed commit restores the shared in-memory store"
        (Some "sliding-on-disk")
        (Store.sliding_pos service_store);
      check_bool "failed commit does not publish room" true
        (Base.find_room (Service.state service) room_id = None);
      check_int "failed commit runs no hooks" 0 !hooks;
      check_int "persistence failure does not poison the lifecycle mutex" 0
        (Service.generation service);
      let disk = Store.on_disk ~dir in
      check_string_option "failed writer leaves competing disk cursor intact"
        (Some "external-position") (Store.sliding_pos disk))

let () =
  Alcotest.run "sliding_service"
    [
      ( "commit",
        [
          Alcotest.test_case "common commit and hook order" `Quick
            test_commit_and_hook_order;
          Alcotest.test_case "cancellable profile subscriptions" `Quick
            test_profile_change_subscriptions;
          Alcotest.test_case "persistence failure is not published" `Quick
            test_persistence_failure_does_not_publish;
        ] );
      ( "lifecycle",
        [
          Alcotest.test_case "reset and stale generation" `Quick
            test_reset_and_stale_generation;
          Alcotest.test_case "to-device disabled" `Quick test_to_device_disabled;
        ] );
    ]
