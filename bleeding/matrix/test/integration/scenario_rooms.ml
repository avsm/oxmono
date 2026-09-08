(** The first scenarios against a live homeserver.

    Registration, a room's whole life — create, name, topic, invite, join,
    members — messages and attachment graphs through the send queue and out of
    the other client's sync, media up and back down, one simplified sliding
    sync, a state read on a room id whose [!] and [:] {!Uriz} leaves unescaped,
    and the three room endpoints Synapse serves only under [/_matrix/client/v1].

    Every check here is a request. What the mock suites assert about request
    shape is not repeated; what is asserted is that a real Synapse accepts what
    this SDK sends and that the SDK reads back what it returns. *)

module Id = Matrix_proto.Id
module Base = Matrix_client.Base_client
module Auth = Matrix_client.Auth
module Messages = Matrix_client.Messages
module Relations = Matrix_client.Relations
module Directory = Matrix_client.Directory
module Rooms = Matrix_client.Rooms
module Spaces = Matrix_client.Spaces
module State = Matrix_client.State
module Media = Matrix_client.Media
module Retention = Matrix_client.Retention
module Server = Matrix_client.Server
module Sliding_sync = Matrix_client.Sliding_sync
module Adaptive_sync = Matrix_eio.Adaptive_sync
module Sync = Matrix_eio.Sync
module Ss_wire = Matrix_proto.Sliding_sync
module Store = Matrix_client.Store
module Preview = Matrix_client.Room_preview
module Peeking = Matrix_client.Peeking
module Knocks = Matrix_client.Knock_requests
module Event = Matrix_proto.Event
module Directory_search = Matrix_ui.Room_directory_search
module Event_search = Matrix_ui.Search_service
module Search = Matrix_client.Search
module Thread_info = Matrix_ui.Thread_info
module Thread_list = Matrix_ui.Thread_list
module Event_focused = Matrix_ui.Event_focused
module Back_pagination = Matrix_ui.Back_pagination
module Event_cache = Matrix_ui.Event_cache
module Pinned_events = Matrix_ui.Pinned_events
module Room_details = Matrix_client.Room_details
module Read_state = Matrix_client.Read_state
module Receipts = Matrix_eio.Receipts

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let rid = Id.Room_id.to_string
let uid = Id.User_id.to_string

let json_of_jsont label codec value =
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error message -> Alcotest.failf "%s: %s" label message

let raw_event_string_content_member name = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem "content" members with
      | Some (_, content) -> Harness.string_member name content
      | None -> None)
  | _ -> None

(* {1 A joined room}

   Most scenarios want two users in one room with both sync loops running.
   The invite and the join are themselves under test in
   {!test_room_lifecycle}; here they are only setup, so the waits are the
   same but the assertions are not repeated. *)

type stage = {
  alice : Harness.user;
  bob : Harness.user;
  alice_sync : Harness.sync;
  bob_sync : Harness.sync;
  room_id : Id.Room_id.t;
}

let joined_room h =
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let alice_sync = Harness.start_sync h alice in
  let bob_sync = Harness.start_sync h bob in
  let room_id =
    Harness.ok "create a room"
      (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat
         ~invite:[ bob.user_id ] ())
  in
  ignore
    (Harness.wait_for_room h ~label:"bob's invite" bob_sync room_id (fun info ->
         info.membership = Base.Invited));
  ignore
    (Harness.ok "bob joins"
       (Rooms.join (Harness.base bob) ~room_id_or_alias:(`Room_id room_id) ()));
  ignore
    (Harness.wait_for_room h ~label:"bob's join" bob_sync room_id (fun info ->
         info.membership = Base.Joined));
  Harness.wait_until h ~label:"alice to see both members" (fun () ->
      List.length
        (Matrix_eio.Sync_service.members (Harness.service alice_sync) room_id)
      >= 2);
  { alice; bob; alice_sync; bob_sync; room_id }

(* {1 Registration} *)

let test_register () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  check_bool "the two registrations are distinct users" false
    (String.equal (uid alice.user_id) (uid bob.user_id));
  check_string "alice got the localpart she asked for" alice.localpart
    (Id.User_id.localpart alice.user_id);
  check_string "and so did bob" bob.localpart (Id.User_id.localpart bob.user_id);
  let whoami (u : Harness.user) =
    uid (Harness.ok "whoami" (Auth.whoami (Harness.base u)))
  in
  check_string "alice's whoami matches her session" (uid alice.user_id)
    (whoami alice);
  check_string "bob's whoami matches his session" (uid bob.user_id) (whoami bob);
  (* The device the registration created is the one the session names. *)
  let devices =
    Harness.ok "list devices"
      (Matrix_client.Devices.get_devices (Harness.base alice))
  in
  check_bool "alice's device is listed" true
    (List.exists
       (fun (d : Matrix_client.Devices.device) ->
         Id.Device_id.equal d.device_id alice.device_id)
       devices);

  (* Exercise the high-level capability facade against a real response, while
     keeping assertions valid for another conforming homeserver configuration.
     The pinned Synapse omits the newer raw capabilities, which also covers
     their Ruma defaults. *)
  let client = Harness.base alice in
  let capabilities =
    Harness.ok "get capabilities" (Server.get_capabilities client)
  in
  check_bool "password policy resolves raw/default value"
    (Option.value ~default:true capabilities.change_password)
    (Harness.ok "resolve password capability"
       (Server.can_change_password client));
  check_bool "3PID policy resolves raw/default value"
    (Option.value ~default:true capabilities.thirdparty_id_changes)
    (Harness.ok "resolve 3PID capability"
       (Server.can_change_thirdparty_ids client));
  check_bool "login-token policy resolves raw/default value"
    (Option.value ~default:false capabilities.get_login_token)
    (Harness.ok "resolve login-token capability"
       (Server.can_get_login_token client));
  let room_versions =
    Harness.ok "resolve room-version capability" (Server.room_versions client)
  in
  check_bool "default room version is advertised" true
    (List.mem_assoc room_versions.default room_versions.available);
  let moderation =
    Harness.ok "resolve moderation capability"
      (Server.account_moderation client)
  in
  if
    Option.is_none
      (Server.find_capability capabilities ~name:"m.account_moderation")
  then begin
    check_bool "absent suspend capability defaults false" false
      moderation.suspend;
    check_bool "absent lock capability defaults false" false moderation.lock
  end;
  let forgets =
    Harness.ok "resolve forced-forget capability"
      (Server.forgets_room_when_leaving client)
  in
  if
    Option.is_none
      (Server.find_capability capabilities ~name:"m.forget_forced_upon_leave")
  then check_bool "absent forced-forget capability defaults false" false forgets;
  let profile_fields =
    Harness.ok "resolve extended-profile capability"
      (Server.extended_profile_fields client)
  in
  ignore profile_fields;
  ignore
    (Harness.ok "refresh capabilities" (Server.refresh_capabilities client))

(* {1 A room's life} *)

let test_room_lifecycle () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let alice_sync = Harness.start_sync h alice in
  let bob_sync = Harness.start_sync h bob in

  let room_id =
    Harness.ok "create a room"
      (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat ())
  in
  let name = "integration " ^ Harness.hex h 4 in
  let topic = "topic " ^ Harness.hex h 4 in
  ignore
    (Harness.ok "set the name"
       (State.set_name (Harness.base alice) ~room_id ~name));
  ignore
    (Harness.ok "set the topic"
       (State.set_topic (Harness.base alice) ~room_id ~topic));
  check_string "the name reads back" name
    (Option.get
       (Harness.ok "get the name"
          (State.get_name (Harness.base alice) ~room_id)));
  check_string "the topic reads back" topic
    (Option.get
       (Harness.ok "get the topic"
          (State.get_topic (Harness.base alice) ~room_id)));

  ignore
    (Harness.ok "invite bob"
       (Rooms.invite (Harness.base alice) ~room_id ~user_id:bob.user_id ()));

  (* Bob's sync carries the invite as an invited room, with the stripped
     state that names it. *)
  let invited =
    Harness.wait_for_room h ~label:"bob's invite" bob_sync room_id (fun info ->
        info.membership = Base.Invited)
  in
  check_string "the invite carries the room's name" name
    (Option.value invited.name ~default:"");
  check_bool "and it is one of bob's invited rooms" true
    (List.exists
       (fun (i : Base.room_info) -> String.equal (rid i.room_id) (rid room_id))
       (Base.rooms_with (Harness.state bob_sync) Base.Invited));

  let joined =
    Harness.ok "bob joins"
      (Rooms.join (Harness.base bob) ~room_id_or_alias:(`Room_id room_id) ())
  in
  check_string "the join answers with the same room id" (rid room_id)
    (rid joined);

  let bob_joined =
    Harness.wait_for_room h ~label:"bob's join" bob_sync room_id (fun info ->
        info.membership = Base.Joined)
  in
  check_string "bob's copy of the room has the name too" name
    (Option.value bob_joined.name ~default:"");
  check_string "and the topic" topic (Option.value bob_joined.topic ~default:"");

  (* Alice's sync state now knows both members. This is what
     Encryption.encrypt_room_event's ~members is fed from. *)
  let members =
    Harness.wait_for h ~label:"alice to see both members" (fun () ->
        let m =
          Matrix_eio.Sync_service.members (Harness.service alice_sync) room_id
        in
        if List.length m >= 2 then Some m else None)
  in
  let members = List.sort String.compare (List.map uid members) in
  Alcotest.(check (list string))
    "both members, from alice's sync state"
    (List.sort String.compare [ uid alice.user_id; uid bob.user_id ])
    members;

  (* And the server agrees. *)
  let joined_members =
    Harness.ok "joined members"
      (Rooms.get_joined_members (Harness.base alice) ~room_id)
  in
  check_int "the server reports two joined members" 2
    (List.length joined_members);
  let member_events =
    Harness.ok "the members endpoint"
      (Rooms.get_members (Harness.base alice) ~room_id
         ~membership:Matrix_proto.Event.Membership.Join ())
  in
  check_int "and the members endpoint agrees" 2 (List.length member_events);
  check_bool "the room is in alice's joined rooms" true
    (List.exists
       (fun r -> String.equal (rid r) (rid room_id))
       (Harness.ok "joined rooms" (Rooms.get_joined_rooms (Harness.base alice))))

(* {1 A message} *)

let test_send_and_receive () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let queue = Harness.start_send_queue h ~sync:s.alice_sync s.alice in
  let body = "hello bob " ^ Harness.hex h 4 in
  let request =
    Matrix_eio.Send_queue.send_text queue ~room_id:s.room_id ~body
  in
  let event_id = Harness.wait_sent h request in

  let event =
    Harness.wait_for_event h ~label:"bob to receive the message" s.bob_sync
      s.room_id (fun e ->
        Matrix_proto.Event.Event_type.to_string e.type_ = "m.room.message"
        && Harness.string_member "body" e.content = Some body)
  in
  check_string "the sender is alice" (uid s.alice.user_id) (uid event.sender);
  check_string "the event id is the one the send queue was given"
    (Id.Event_id.to_string event_id)
    (Id.Event_id.to_string (Option.get event.event_id));
  check_string "the msgtype survived" "m.text"
    (Option.value (Harness.string_member "msgtype" event.content) ~default:"");

  (* The room's latest event, as the base client tracks it. *)
  let info =
    Harness.wait_for_room h ~label:"bob's room to advance" s.bob_sync s.room_id
      (fun info ->
        match info.latest_event with
        | Some e -> Harness.string_member "body" e.content = Some body
        | None -> false)
  in
  check_string "and the room is named for both" (rid s.room_id)
    (rid info.room_id)

(* A public receipt carrying a thread id is broadcast through the same
   ephemeral stream as an ordinary receipt, but it advances only the sender's
   per-thread read position.  In particular it must not become the room's
   main-timeline receipt or clear a manual [m.marked_unread] flag. *)

let test_threaded_receipt () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let root_id =
    Harness.ok "send thread root"
      (Messages.send_text (Harness.base s.alice) ~room_id:s.room_id
         ~body:("thread root " ^ Harness.hex h 4)
         ())
  in
  ignore
    (Harness.wait_for_event h ~label:"bob to receive the thread root" s.bob_sync
       s.room_id (fun event ->
         Option.equal Id.Event_id.equal event.event_id (Some root_id)));
  let reply_id =
    Harness.ok "send thread reply"
      (Relations.send_in_thread (Harness.base s.alice) ~room_id:s.room_id
         ~thread_root_id:root_id
         ~body:("thread reply " ^ Harness.hex h 4)
         ())
  in
  ignore
    (Harness.wait_for_event h ~label:"bob to receive the thread reply"
       s.bob_sync s.room_id (fun event ->
         Option.equal Id.Event_id.equal event.event_id (Some reply_id)));

  (* Establish the room-level state before the threaded receipt arrives. *)
  Harness.ok "mark bob's room unread"
    (Matrix_client.Account_data.set_marked_unread (Harness.base s.bob)
       ~room_id:s.room_id ~unread:true);
  Harness.wait_until h ~label:"bob's marked-unread flag" (fun () ->
      match Base.find_room (Harness.state s.bob_sync) s.room_id with
      | Some info -> info.marked_unread
      | None -> false);
  let before = Base.receipts (Harness.state s.bob_sync) s.room_id in
  check_bool "no main receipt before threaded receipt" true
    (Read_state.public_read before = None);

  (* Bob is the other peer that received Alice's real root/reply.  The receipt
     is sent through the Eio wrapper and then observed after the server echoes
     it through Bob's sync stream. *)
  Receipts.send_receipt s.bob.client ~room_id:s.room_id ~event_id:reply_id
    ~thread_id:root_id ();
  let receipts =
    Harness.wait_for h ~label:"bob's threaded receipt to sync" (fun () ->
        let receipts = Base.receipts (Harness.state s.bob_sync) s.room_id in
        match Read_state.thread_public_read receipts ~thread_id:root_id with
        | Some receipt when Id.Event_id.equal receipt.event_id reply_id ->
            Some receipts
        | _ -> None)
  in
  check_bool "thread receipt advances the per-thread position" true
    (match Read_state.thread_public_read receipts ~thread_id:root_id with
    | Some receipt -> Id.Event_id.equal receipt.event_id reply_id
    | None -> false);
  check_bool "thread receipt does not become the main receipt" true
    (Read_state.public_read receipts = None);
  check_bool "thread receipt leaves marked-unread set" true
    (match Base.find_room (Harness.state s.bob_sync) s.room_id with
    | Some info -> info.marked_unread
    | None -> false)

(* {1 Media}

   A 1x1 PNG, so that the thumbnail request has something Synapse's
   thumbnailer can actually resize. *)

let png_1x1 =
  Base64.decode_exn
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="

let png_thumbnail_1x1 =
  Base64.decode_exn
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M/wHwAF/gL+6QAAAABJRU5ErkJggg=="

let test_attachment_send_and_receive () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let queue = Harness.start_send_queue h ~sync:s.alice_sync s.alice in
  (* Keep the visible event pending while its caption is changed. This makes
     the live case deterministic while still exercising the ordinary queue
     worker, both uploads and the final homeserver PUT after re-enabling it. *)
  Matrix_eio.Send_queue.set_enabled queue ~enabled:false;
  let body = "attachment " ^ Harness.hex h 4 in
  let edited_body = "edited " ^ body in
  let base_content =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "msgtype") (Jsont.Json.string "m.image");
        Jsont.Json.mem (Jsont.Json.name "body") (Jsont.Json.string body);
        (* These are local/stale values. The queue must replace them before
           the event reaches the homeserver. *)
        Jsont.Json.mem (Jsont.Json.name "url")
          (Jsont.Json.string "mxc://stale.example/base");
        Jsont.Json.mem (Jsont.Json.name "file")
          (Jsont.Json.object'
             [
               Jsont.Json.mem (Jsont.Json.name "url")
                 (Jsont.Json.string "mxc://stale.example/base-file");
             ]);
        Jsont.Json.mem (Jsont.Json.name "info")
          (Jsont.Json.object'
             [
               Jsont.Json.mem
                 (Jsont.Json.name "thumbnail_url")
                 (Jsont.Json.string "mxc://stale.example/base-thumbnail");
               Jsont.Json.mem
                 (Jsont.Json.name "thumbnail_file")
                 (Jsont.Json.object'
                    [
                      Jsont.Json.mem (Jsont.Json.name "url")
                        (Jsont.Json.string "mxc://stale.example/base-thumb-file");
                    ]);
               Jsont.Json.mem
                 (Jsont.Json.name "com.example.extra-info")
                 (Jsont.Json.bool true);
             ]);
      ]
  in
  let extra_content =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "url")
          (Jsont.Json.string "mxc://stale.example/extra");
        Jsont.Json.mem (Jsont.Json.name "file")
          (Jsont.Json.object'
             [
               Jsont.Json.mem (Jsont.Json.name "url")
                 (Jsont.Json.string "mxc://stale.example/extra-file");
             ]);
        Jsont.Json.mem (Jsont.Json.name "info")
          (Jsont.Json.object'
             [
               Jsont.Json.mem
                 (Jsont.Json.name "thumbnail_file")
                 (Jsont.Json.object'
                    [
                      Jsont.Json.mem (Jsont.Json.name "url")
                        (Jsont.Json.string
                           "mxc://stale.example/extra-thumb-file");
                    ]);
             ]);
        Jsont.Json.mem
          (Jsont.Json.name "com.example.extra")
          (Jsont.Json.bool true);
        (* Caption-owned fields in extras must not come back when the edit
           deliberately clears formatting and mentions. *)
        Jsont.Json.mem
          (Jsont.Json.name "filename")
          (Jsont.Json.string "stale-extra-name.png");
        Jsont.Json.mem (Jsont.Json.name "format")
          (Jsont.Json.string "org.matrix.custom.html");
        Jsont.Json.mem
          (Jsont.Json.name "formatted_body")
          (Jsont.Json.string "<b>stale caption</b>");
        Jsont.Json.mem
          (Jsont.Json.name "m.mentions")
          (Jsont.Json.object'
             [ Jsont.Json.mem (Jsont.Json.name "room") (Jsont.Json.bool true) ]);
      ]
  in
  let request =
    Matrix_eio.Send_queue.send_attachment queue ~room_id:s.room_id ~base_content
      ~extra_content
      ~original:
        (Matrix_eio.Send_queue.attachment_upload ~content_type:"image/png"
           ~data:png_1x1 ~filename:"original.png" ())
      ~thumbnail:
        (Matrix_eio.Send_queue.attachment_upload ~content_type:"image/png"
           ~data:png_thumbnail_1x1 ~filename:"thumbnail.png" ())
      ()
  in
  (match
     Matrix_eio.Send_queue.edit_attachment_caption queue request
       ~caption:(Some edited_body)
   with
  | Ok Matrix_eio.Send_queue.Updated -> ()
  | Ok _ -> Alcotest.fail "pending live attachment caption was not updated"
  | Error error ->
      Alcotest.failf "live attachment caption update failed: %s"
        (Matrix_client.Error.to_string error));
  Matrix_eio.Send_queue.set_enabled queue ~enabled:true;
  ignore (Harness.wait_sent h request);
  let event =
    Harness.wait_for_event h ~label:"bob to receive the attachment" s.bob_sync
      s.room_id (fun e ->
        Event.Event_type.to_string e.type_ = "m.room.message"
        && Harness.string_member "msgtype" e.content = Some "m.image"
        && Harness.string_member "body" e.content = Some edited_body)
  in
  let url =
    match Matrix_proto.Json.find_string "url" event.content with
    | Some url -> url
    | None -> Alcotest.fail "attachment event has no url"
  in
  let info =
    match Matrix_proto.Json.find_mem "info" event.content with
    | Some info -> info
    | None -> Alcotest.fail "attachment event has no info"
  in
  let thumbnail_url =
    match Matrix_proto.Json.find_string "thumbnail_url" info with
    | Some url -> url
    | None -> Alcotest.fail "attachment event has no thumbnail_url"
  in
  let original_mxc =
    match Media.Mxc.of_string url with
    | Ok mxc -> mxc
    | Error (`Msg error) -> Alcotest.failf "invalid original MXC: %s" error
  in
  let thumbnail_mxc =
    match Media.Mxc.of_string thumbnail_url with
    | Ok mxc -> mxc
    | Error (`Msg error) -> Alcotest.failf "invalid thumbnail MXC: %s" error
  in
  check_bool "original and thumbnail MXCs are distinct" true
    (not (String.equal url thumbnail_url));
  check_bool "stale original file does not leak" true
    (Matrix_proto.Json.find_mem "file" event.content = None);
  check_string "caption edit retains the logical filename" "original.png"
    (Option.value
       (Matrix_proto.Json.find_string "filename" event.content)
       ~default:"");
  check_bool "caption edit clears stale formatted and mention extras" true
    (Matrix_proto.Json.find_mem "format" event.content = None
    && Matrix_proto.Json.find_mem "formatted_body" event.content = None
    && Matrix_proto.Json.find_mem "m.mentions" event.content = None);
  check_bool "stale thumbnail file does not leak" true
    (Matrix_proto.Json.find_mem "thumbnail_file" info = None);
  check_bool "extra vendor field survives" true
    (Matrix_proto.Json.find_bool "com.example.extra" event.content = Some true);
  check_bool "extra info vendor field survives" true
    (Matrix_proto.Json.find_bool "com.example.extra-info" info = Some true);
  let download label mxc =
    let c = Harness.base s.alice in
    Harness.wait_for h ~label:(label ^ " to be downloadable") (fun () ->
        match
          Media.download c
            ~server_name:(Media.Mxc.server_name mxc)
            ~media_id:(Media.Mxc.media_id mxc)
        with
        | Ok content -> Some content
        | Error _ -> None)
  in
  check_string "original bytes survive upload" png_1x1
    (download "original" original_mxc).body;
  check_string "thumbnail bytes survive upload" png_thumbnail_1x1
    (download "thumbnail" thumbnail_mxc).body;
  let timeline = Harness.timeline s.bob_sync s.room_id in
  let attachment_events =
    List.filter
      (fun (e : Event.Raw_event.t) ->
        Harness.string_member "body" e.content = Some edited_body)
      timeline
  in
  check_int "exactly one visible attachment event" 1
    (List.length attachment_events);
  check_bool "upload nodes never become room events" true
    (not
       (List.exists
          (fun (e : Event.Raw_event.t) ->
            Event.Event_type.to_string e.type_ = "m.upload")
          timeline))

let test_media () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let c = Harness.base alice in
  let mxc =
    Harness.ok "upload"
      (Media.upload c ~content_type:"image/png" ~data:png_1x1
         ~filename:"pixel.png" ())
  in
  let server_name = Media.Mxc.server_name mxc
  and media_id = Media.Mxc.media_id mxc in

  (* Synapse serves media asynchronously enough that a download right after
     an upload can 404 on a fresh server; wait it out rather than flake. *)
  let downloaded =
    Harness.wait_for h ~label:"the upload to be downloadable" (fun () ->
        match Media.download c ~server_name ~media_id with
        | Ok r -> Some r
        | Error _ -> None)
  in
  check_string "the bytes come back unchanged" png_1x1 downloaded.body;
  check_string "and so does the content type" "image/png"
    (Option.value downloaded.content_type ~default:"");

  (* MSC2246 reserves the URI before the bytes exist, then fills that exact
     server/media-id pair with an authenticated PUT.  Exercise both the happy
     path and the server's overwrite guard against the real media repository. *)
  let reservation =
    Harness.ok "create preallocated media URI" (Media.create_content_uri c)
  in
  (match
     Media.upload_preallocated c reservation ~content_type:"image/png"
       ~data:png_1x1 ~filename:"reserved-pixel.png" ()
   with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "preallocated upload failed: %a"
        Media.pp_preallocated_upload_error e);
  (match
     Media.upload_preallocated c reservation ~content_type:"image/png"
       ~data:png_1x1 ()
   with
  | Error Media.Cannot_overwrite -> ()
  | Error e ->
      Alcotest.failf "preallocated overwrite returned the wrong error: %a"
        Media.pp_preallocated_upload_error e
  | Ok () -> Alcotest.fail "preallocated media was overwritten");
  let reserved_download =
    Harness.wait_for h ~label:"the preallocated upload to be downloadable"
      (fun () ->
        match
          Media.download c
            ~server_name:(Media.Mxc.server_name reservation.uri)
            ~media_id:(Media.Mxc.media_id reservation.uri)
        with
        | Ok r -> Some r
        | Error _ -> None)
  in
  check_string "the preallocated bytes come back unchanged" png_1x1
    reserved_download.body;
  check_string "and their content type is retained" "image/png"
    (Option.value reserved_download.content_type ~default:"");

  (* A thumbnail is either produced or refused with a Matrix error naming
     why; a network or JSON failure would be this SDK's problem. *)
  (match
     Media.thumbnail c ~server_name ~media_id ~width:32 ~height:32
       ~resize:`Scale ()
   with
  | Ok thumb ->
      check_bool "the thumbnail is not empty" true (String.length thumb.body > 0)
  | Error (Matrix_client.Error.Matrix_error e) ->
      Printf.printf "  note: thumbnail refused with %s: %s\n%!"
        (Matrix_client.Error.errcode_to_string e.errcode)
        e.error
  | Error e ->
      Alcotest.failf "thumbnail failed outside the protocol: %s"
        (Matrix_client.Error.to_string e));

  let config = Harness.ok "media config" (Media.get_config c) in
  check_bool "the server declares an upload limit" true
    (config.upload_size <> None)

(* {1 Sliding sync}

   One [sync_once] against
   [/_matrix/client/unstable/org.matrix.simplified_msc3575/sync], which
   Synapse serves when [experimental_features.msc3575_enabled] is on — the
   flag is named for MSC3575 even though the endpoint is MSC4186's. *)

let test_sliding_sync () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let request =
    Ss_wire.Request.v ~conn_id:"ocaml-matrix-integration" ()
    |> Ss_wire.Request.add_list ~name:"all"
         ~ranges:[ (0, 19) ]
         ~required_state:
           [
             Ss_wire.Required_state.v Matrix_proto.Event.Event_type.Room_name;
             Ss_wire.Required_state.v Matrix_proto.Event.Event_type.Room_topic;
           ]
         ~timeline_limit:5
  in
  let response =
    match Sliding_sync.sync_once (Harness.base s.alice) request with
    | Ok r -> r
    | Error e when Sliding_sync.is_unsupported e ->
        Alcotest.failf
          "the homeserver does not serve %s; is \
           experimental_features.msc3575_enabled set?"
          Sliding_sync.path
    | Error e ->
        Alcotest.failf "sliding sync: %s" (Matrix_client.Error.to_string e)
  in
  check_bool "the response carries a pos" true (String.length response.pos > 0);
  check_bool "the list we asked for answered" true
    (List.mem_assoc "all" response.lists);
  let room =
    match
      List.find_opt
        (fun (r, _) -> String.equal (rid r) (rid s.room_id))
        response.rooms
    with
    | Some (_, room) -> room
    | None ->
        Alcotest.failf "the joined room is not in the %d rooms listed"
          (List.length response.rooms)
  in
  check_bool "the room reports its joined count" true (room.joined_count <> None)

(* The high-level sliding loop must be able to drive the common base service,
   rather than maintaining a second room database.  Keep this deliberately to
   one response per loop: the response callback is the commit boundary, and
   the nested switches make sure no poll fiber survives the assertions. *)
let test_sliding_sync_service () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let room_name = "service-backed sliding room" in
  ignore
    (Harness.ok "name the service-backed room"
       (State.set_name (Harness.base s.alice) ~room_id:s.room_id ~name:room_name));
  Harness.wait_until h ~label:"the room name to reach classic sync" (fun () ->
      match Base.find_room (Harness.state s.alice_sync) s.room_id with
      | Some room -> room.name = Some room_name
      | None -> false);
  let store = Store.memory () in
  let service =
    Matrix_eio.Sync_service.of_store ~store ~user_id:s.alice.user_id ()
  in
  let request =
    Ss_wire.Request.v ~conn_id:"ocaml-svc-ss-1" ()
    |> Ss_wire.Request.add_list ~name:"joined"
         ~ranges:[ (0, 19) ]
         ~required_state:
           [
             Ss_wire.Required_state.v Matrix_proto.Event.Event_type.Room_name;
             Ss_wire.Required_state.v Matrix_proto.Event.Event_type.Room_member;
           ]
         ~timeline_limit:5
  in
  let run_once service =
    let completed, resolver = Eio.Promise.create () in
    let response = ref None in
    let failure = ref None in
    Eio.Switch.run @@ fun sw ->
    Matrix_eio.Sliding_sync.sync_forever ~sw ~clock:(Harness.clock h)
      s.alice.client ~timeout_ms:5_000 ~service
      ~on_change:(fun committed _changes ->
        (* [on_change] is not the raw callback: it is published only after
           the common service and its store have committed. *)
        check_bool "service state has the room at commit" true
          (Option.is_some (Base.find_room committed s.room_id)))
      ~callbacks:
        {
          Sync.on_response =
            (fun r ->
              response := Some r;
              check_bool "raw callback observes committed room" true
                (Option.is_some
                   (Base.find_room
                      (Matrix_eio.Sync_service.state service)
                      s.room_id));
              Eio.Promise.resolve resolver ();
              Sync.Stop);
          on_error =
            (fun e ->
              failure := Some e;
              Eio.Promise.resolve resolver ();
              Sync.Stop);
        }
      request;
    Eio.Promise.await completed;
    match (!response, !failure) with
    | Some r, None -> r
    | None, Some e ->
        Alcotest.failf "service-backed sliding sync: %a" Matrix_eio.Error.pp_err
          e
    | Some _, Some _ | None, None ->
        Alcotest.fail "service-backed sliding sync completed inconsistently"
  in
  let first = run_once service in
  check_bool "service response carries a position" true
    (String.length first.pos > 0);
  check_bool "service persisted the sliding position" true
    (Store.sliding_pos store = Some first.pos);
  check_bool "base state persisted the sliding position" true
    (Base.sliding_pos (Matrix_eio.Sync_service.state service) = Some first.pos);
  let room =
    match Base.find_room (Matrix_eio.Sync_service.state service) s.room_id with
    | Some room -> room
    | None -> Alcotest.fail "service-backed sliding sync lost the joined room"
  in
  check_string "service folded the joined room name" room_name
    (Option.value room.name ~default:"");
  check_int "service folded both joined members" 2 room.joined_member_count;
  check_bool "sliding service leaves classic cursor independent" true
    (Base.next_batch (Matrix_eio.Sync_service.state service) = None);
  let resumed =
    Matrix_eio.Sync_service.of_store ~store ~user_id:s.alice.user_id ()
  in
  let second = run_once resumed in
  check_bool "resumed service response carries a position" true
    (String.length second.pos > 0);
  check_bool "resumed service preserves the room" true
    (Option.is_some
       (Base.find_room (Matrix_eio.Sync_service.state resumed) s.room_id));
  check_bool "resumed service keeps classic cursor independent" true
    (Base.next_batch (Matrix_eio.Sync_service.state resumed) = None)

(* Exercise adaptive selection on the pinned Synapse deployment.  The harness
   deliberately enables MSC3575, so discovery must select the native endpoint
   and still fold its response into the caller-owned common service.  The
   unavailable and endpoint-rejection fallback branches stay deterministic in
   the hermetic adaptive-sync suite. *)
let test_adaptive_sync_native () =
  Harness.run @@ fun h ->
  let user = Harness.register_user h ~prefix:"adaptive" () in
  let service =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let modes = ref [] in
  let response = ref None in
  let done_, resolver = Eio.Promise.create () in
  Adaptive_sync.run ~sw:(Harness.switch h) ~clock:(Harness.clock h) user.client
    ~service
    ~on_mode:(fun mode -> modes := mode :: !modes)
    ~on_response:(fun current ->
      response := Some current;
      Eio.Promise.resolve resolver ();
      Sync.Stop)
    ~on_error:(fun error ->
      Alcotest.failf "adaptive sync: %a" Matrix_eio.Error.pp_err error)
    ~on_change:(fun _ _ -> ())
    ();
  Eio.Promise.await done_;
  check_bool "Synapse selects discovery then sliding" true
    (List.rev !modes = [ Adaptive_sync.Discovering; Adaptive_sync.Sliding ]);
  (match !response with
  | Some (Adaptive_sync.Sliding sliding) ->
      check_bool "sliding response has a position" true
        (String.length sliding.pos > 0)
  | Some (Adaptive_sync.Classic _) ->
      Alcotest.fail "Synapse unexpectedly returned a classic response"
  | None -> Alcotest.fail "adaptive sync returned no response");
  check_bool "shared sliding cursor advanced" true
    (Option.is_some (Base.sliding_pos (Matrix_eio.Sync_service.state service)))

(* {1 URL escaping}

   [TODO.md] records that this SDK leaves [!] and [:] unescaped in a path,
   which Uriz's [`Segment] encoding allows per RFC 3986, but which the
   spec's examples do not show and a strict router could reject. A state
   read on a joined room is the cheapest request that puts a room id in the
   path; it also exercises the [/state] and [/state/{type}] decoders against
   real events. *)

let test_room_id_in_path () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let path =
    Uriz.path
      (Uriz.with_path (Harness.homeserver h)
         ("/_matrix/client/v3/rooms/" ^ rid s.room_id ^ "/state"))
  in
  check_bool "the room id's ! reaches the path unescaped" true
    (String.contains path '!');
  check_bool "and so does its :" true (String.contains path ':');

  let events =
    Harness.ok "get the room state"
      (State.get_state (Harness.base s.alice) ~room_id:s.room_id)
  in
  check_bool "the state has an m.room.create" true
    (List.exists
       (fun (e : Matrix_proto.Event.Raw_event.t) ->
         Matrix_proto.Event.Event_type.to_string e.type_ = "m.room.create")
       events);
  check_bool "and two m.room.member events" true
    (List.length
       (List.filter
          (fun (e : Matrix_proto.Event.Raw_event.t) ->
            Matrix_proto.Event.Event_type.to_string e.type_ = "m.room.member")
          events)
    >= 2);
  let create =
    Harness.ok "get one state event"
      (State.get_state_event (Harness.base s.alice) ~room_id:s.room_id
         ~event_type:Matrix_proto.Event.Event_type.Room_create ~state_key:"" ())
  in
  check_bool "the create event has a room version" true
    (Harness.string_member "room_version" create <> None)

(* {1 Retention}

   Synapse accepts the stable room-state event but does not implement the
   MSC1763 server-configuration endpoint. This covers both halves of the API:
   state round-trips, while the high-level effective policy follows the Rust
   SDK and returns [None] when the server cannot enforce a configuration. *)

let test_retention () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let client = Harness.base alice in
  let room_id =
    Harness.ok "create a retention room"
      (Rooms.create client ~preset:Rooms.Trusted_private_chat ())
  in
  let policy =
    Retention.policy ~min_lifetime:60_000L ~max_lifetime:3_600_000L ()
  in
  ignore
    (Harness.ok "set room retention"
       (Retention.set_room_policy client ~room_id policy));
  let read_back =
    match
      Harness.ok "get room retention"
        (Retention.get_room_policy client ~room_id)
    with
    | Some policy -> policy
    | None -> Alcotest.fail "the room retention state disappeared"
  in
  Alcotest.(check (option int64))
    "minimum reads back" (Some 60_000L)
    (Retention.policy_min_lifetime read_back);
  Alcotest.(check (option int64))
    "maximum reads back" (Some 3_600_000L)
    (Retention.policy_max_lifetime read_back);
  match Retention.effective client ~room_id with
  | Ok None -> ()
  | Ok (Some _) ->
      Alcotest.fail
        "Synapse unexpectedly advertised an MSC1763 retention configuration"
  | Error e ->
      Alcotest.failf "effective retention: %s" (Matrix_client.Error.to_string e)

(* {1 The endpoints served only under /_matrix/client/v1}

   [Client.Http.get] prefixes [/_matrix/client/v3]. Relations, the space
   hierarchy and the room summary are registered under [v1] alone, so each
   builds an absolute path instead; Synapse answers 404 to the v3 spelling,
   which no mock catches. *)

let test_v1_room_endpoints () =
  Harness.run @@ fun h ->
  let s = joined_room h in
  let alice = Harness.base s.alice in
  let event_id =
    Harness.ok "send a message"
      (Messages.send_text alice ~room_id:s.room_id ~body:"react to me" ())
  in
  ignore
    (Harness.ok "react to it"
       (Relations.send_reaction alice ~room_id:s.room_id ~event_id ~key:"+1"));
  let relations =
    Harness.ok "read the relations"
      (Relations.get_reactions alice ~room_id:s.room_id ~event_id)
  in
  check_int "the reaction comes back" 1
    (List.length relations.Matrix_proto.Common.Page.chunk);

  let hierarchy =
    Harness.ok "read the hierarchy"
      (Spaces.get_hierarchy alice ~space:s.room_id ())
  in
  check_bool "a room with no children is its own hierarchy" true
    (List.exists
       (fun (r : Directory.room_summary) -> rid r.room_id = rid s.room_id)
       hierarchy.Matrix_proto.Common.Page.chunk);

  let summary =
    Harness.ok "read the room summary"
      (Directory.get_summary alice ~room_id_or_alias:(`Room_id s.room_id) ())
  in
  check_string "the summary names this room" (rid s.room_id)
    (rid summary.room_id);
  check_int "and counts both members" 2 summary.num_joined_members

(* {1 Room previews and knock moderation}

   Synapse exposes the MSC3266 room summary to a user who has only knocked.
   Keep the moderation cases independent: accepting one request changes its
   member event to an invite, while kicking and banning the others leave them
   out of the room.  The persisted store is deliberately made from the
   requester's live sync state, which is the restart boundary used by the
   client API. *)

let member_membership sync room_id user_id =
  match
    Base.find_state_event (Harness.state sync) room_id
      ~event_type:Event.Event_type.Room_member ~state_key:(uid user_id) ()
  with
  | None -> None
  | Some event ->
      Option.map Event.Room_member_content.membership
        (Result.to_option
           (Jsont.Json.decode Event.Room_member_content.jsont event.content))

let test_room_preview_and_knocks () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let alice_sync = Harness.start_sync h alice in
  let room_name = "knock room " ^ Harness.hex h 4 in
  let room_id =
    Harness.ok "create a named room"
      (Rooms.create (Harness.base alice) ~name:room_name
         ~preset:Rooms.Trusted_private_chat ())
  in
  ignore
    (Harness.ok "allow knocks"
       (State.set_state (Harness.base alice) ~room_id
          ~event_type:Event.Event_type.Room_join_rules
          ~content:
            (Jsont.Json.object'
               [
                 Jsont.Json.mem
                   (Jsont.Json.name "join_rule")
                   (Jsont.Json.string "knock");
               ])
          ()));

  let moderate ~prefix ~reason action =
    let requester = Harness.register_user h ~prefix () in
    let requester_sync = Harness.start_sync h requester in
    ignore
      (Harness.ok "requester knocks"
         (Rooms.knock (Harness.base requester)
            ~room_id_or_alias:(`Room_id room_id) ~reason ()));
    ignore
      (Harness.wait_for_room h ~label:(prefix ^ " to see its knock")
         requester_sync room_id (fun info -> info.membership = Base.Knocked));
    ignore
      (Harness.wait_for_room h ~label:(prefix ^ " knock to reach alice")
         alice_sync room_id (fun _ ->
           match member_membership alice_sync room_id requester.user_id with
           | Some Event.Membership.Knock -> true
           | _ -> false));
    (* Snapshot Alice's state at the moderation boundary into the same store
       shape a restarted client would use. *)
    let store = Store.memory () in
    Base.persist store (Harness.state alice_sync);
    let request =
      match Knocks.list store ~room_id with
      | [ request ] when Id.User_id.equal request.user_id requester.user_id ->
          request
      | requests ->
          Alcotest.failf "%s: expected one current knock, got %d" prefix
            (List.length requests)
    in
    check_string (prefix ^ " reason") reason
      (Option.value request.reason ~default:"");
    check_bool (prefix ^ " starts unseen") false request.is_seen;
    Harness.ok (prefix ^ " mark seen") (Knocks.mark_seen store request);
    check_bool
      (prefix ^ " is persisted as seen")
      true (List.hd (Knocks.list store ~room_id)).is_seen;
    Harness.ok (prefix ^ " moderation") (action requester request);
    (requester, requester_sync)
  in

  let accepted, accepted_sync =
    moderate ~prefix:"accepted" ~reason:"I have an invitation" (fun _ request ->
        Knocks.accept (Harness.base alice) request)
  in
  ignore
    (Harness.wait_for_room h ~label:"accepted requester to see invite"
       accepted_sync room_id (fun info -> info.membership = Base.Invited));
  ignore
    (Harness.wait_for_room h ~label:"alice to see accepted knock" alice_sync
       room_id (fun _ ->
         match member_membership alice_sync room_id accepted.user_id with
         | Some Event.Membership.Invite -> true
         | _ -> false));

  let declined, declined_sync =
    moderate ~prefix:"declined" ~reason:"Please let me in" (fun _ request ->
        Knocks.decline (Harness.base alice) request ~reason:"Not today" ())
  in
  ignore
    (Harness.wait_for_room h ~label:"declined requester to see leave"
       declined_sync room_id (fun info -> info.membership = Base.Left));
  ignore
    (Harness.wait_for_room h ~label:"alice to see declined knock" alice_sync
       room_id (fun _ ->
         match member_membership alice_sync room_id declined.user_id with
         | Some Event.Membership.Leave -> true
         | _ -> false));

  let banned, banned_sync =
    moderate ~prefix:"banned" ~reason:"I can help" (fun _ request ->
        Knocks.decline_and_ban (Harness.base alice) request ~reason:"Blocked" ())
  in
  ignore
    (Harness.wait_for_room h ~label:"banned requester to see leave" banned_sync
       room_id (fun info -> info.membership = Base.Left));
  ignore
    (Harness.wait_for_room h ~label:"alice to see banned knock" alice_sync
       room_id (fun _ ->
         match member_membership alice_sync room_id banned.user_id with
         | Some Event.Membership.Ban -> true
         | _ -> false));

  (* A separately persisted knocked user's state is enough for the preview
     service, which then exercises Synapse's live room-summary endpoint. *)
  let preview_user = Harness.register_user h ~prefix:"preview" () in
  let preview_sync = Harness.start_sync h preview_user in
  ignore
    (Harness.ok "preview requester knocks"
       (Rooms.knock
          (Harness.base preview_user)
          ~room_id_or_alias:(`Room_id room_id) ~reason:"Just looking" ()));
  ignore
    (Harness.wait_for_room h ~label:"preview requester to see knock"
       preview_sync room_id (fun info -> info.membership = Base.Knocked));
  let preview_store = Store.memory () in
  Base.persist preview_store (Harness.state preview_sync);
  let preview =
    Harness.ok "live room preview"
      (Preview.get
         (Harness.base preview_user)
         ~store:preview_store ~room_id_or_alias:(`Room_id room_id) ())
  in
  check_string "preview room id" (rid room_id) (rid preview.room_id);
  check_string "preview room name" room_name
    (Option.value preview.name ~default:"");
  check_bool "preview keeps knocked membership" true
    (preview.membership = Some Store.Knocked);
  check_bool "preview reports knock join rule" true
    (preview.join_rule = Some Event.Join_rule.Knock)

(* The high-level discovery facades use the same endpoints as the lower-level
   client modules, but add token/reset/cache state which mocks alone cannot
   prove a deployed server accepts. Keep them together so the public room and
   its search index are unique to one scenario. *)

let test_discovery_services () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"discovery" () in
  let alice_sync = Harness.start_sync h alice in
  let client = Harness.base alice in
  let tag = "discover-" ^ Harness.hex h 6 in
  let room_id =
    Harness.ok "create public discovery room"
      (Rooms.create client ~name:tag
         ~visibility:Matrix_proto.Common.Visibility.Public
         ~preset:Rooms.Public_chat ())
  in
  ignore
    (Harness.ok "publish discovery room"
       (Directory.set_visibility client ~room_id
          ~visibility:Matrix_proto.Common.Visibility.Public));

  let directory = Directory_search.create ~client () in
  Harness.wait_until h ~label:"public directory service to find its room"
    (fun () ->
      Harness.ok "search public directory"
        (Directory_search.search directory ~filter:tag ~batch_size:10 ());
      Matrix_ui.Observable.List.snapshot (Directory_search.results directory)
      |> Array.exists (fun (room : Directory.room_summary) ->
          Id.Room_id.equal room.room_id room_id));
  check_bool "directory search reaches its terminal page" true
    (Directory_search.is_at_last_page directory);

  let body = "indexed " ^ tag in
  let root_id =
    Harness.ok "send searchable thread root"
      (Messages.send_text client ~room_id ~body ())
  in
  let interleaved_id =
    Harness.ok "send interleaved main-room event"
      (Messages.send_text client ~room_id
         ~body:("main-room " ^ Harness.hex h 4)
         ())
  in
  let reply_body = "reply " ^ tag in
  let reply_id =
    Harness.ok "send thread reply"
      (Relations.send_in_thread client ~room_id ~thread_root_id:root_id
         ~body:reply_body ())
  in

  let search = Event_search.create ~client () in
  Harness.wait_until h ~label:"event search service to find its message"
    (fun () ->
      Harness.ok "search room events"
        (Event_search.search search ~criteria:(Search.v tag));
      Matrix_ui.Observable.List.snapshot (Event_search.results search)
      |> Array.exists (fun (hit : Search.hit) ->
          match hit.result with
          | Some event -> Harness.string_member "body" event.content = Some body
          | None -> false));

  let thread_info = Thread_info.create ~user_id:alice.user_id () in
  let thread_list = Thread_list.create ~client ~thread_info ~room_id () in
  let rich_thread =
    Harness.wait_for h ~label:"rich thread list to find its root" (fun () ->
        Thread_list.reset thread_list;
        Harness.ok "list rich thread roots"
          (Thread_list.next_page thread_list ());
        Array.find_opt
          (fun (info : Thread_info.info) ->
            Option.equal Id.Event_id.equal info.root.event_id (Some root_id))
          (Thread_list.snapshot thread_list))
  in
  check_bool "thread list is terminal" true
    (Thread_list.is_at_last_page thread_list);
  check_bool "thread list exposes a bundled reply count" true
    (match rich_thread.summary_status with
    | Thread_info.Known { reply_count; _ } -> reply_count >= 1
    | Thread_info.Unknown | Thread_info.Known_none -> false);
  check_bool "thread list exposes the bundled latest reply" true
    (match rich_thread.latest_reply with
    | Some event ->
        Option.equal Id.Event_id.equal event.event_id (Some reply_id)
        && Harness.string_member "body" event.content = Some reply_body
    | None -> false);
  Thread_list.close thread_list;

  let relation_page =
    Harness.ok "list recursive thread relations"
      (Relations.get_raw_relations client ~room_id ~event_id:root_id
         ~dir:Matrix_proto.Common.Direction.Backward ~recurse:true ())
  in
  check_bool "recursive relations contain the live reply" true
    (List.exists
       (fun (event : Event.Raw_event.t) ->
         Option.equal Id.Event_id.equal event.event_id (Some reply_id))
       relation_page.chunk);
  let focused =
    Event_focused.create ~client ~room_id ~event_id:reply_id ~limit:10 ()
  in
  let focused_start =
    match Event_focused.start focused () with
    | Ok result -> result
    | Error error ->
        Alcotest.failf "starting live thread-focused view: %a"
          Event_focused.pp_error error
  in
  let focused_ids =
    List.filter_map
      (fun (event : Event.Raw_event.t) -> event.event_id)
      focused_start.events
  in
  check_bool "automatic focused view contains the thread root" true
    (List.exists (Id.Event_id.equal root_id) focused_ids);
  check_bool "automatic focused view contains the target reply" true
    (List.exists (Id.Event_id.equal reply_id) focused_ids);
  check_bool "automatic focused view excludes an interleaved room event" false
    (List.exists (Id.Event_id.equal interleaved_id) focused_ids);
  Event_focused.close focused;

  let newest =
    Harness.ok "seed live back-pagination"
      (Messages.get_messages client ~room_id
         ~dir:Matrix_proto.Common.Direction.Backward ~limit:1 ())
  in
  let back_token =
    match newest.page.next_batch with
    | Some token -> token
    | None -> Alcotest.fail "live room returned no back-pagination token"
  in
  let back_cache = Event_cache.create () in
  Event_cache.prepend back_cache room_id
    ~events:(List.rev newest.page.chunk)
    ~prev_batch:(Some back_token);
  let back_queue =
    Back_pagination.create ~sw:(Harness.switch h) ~client
      ~event_cache:back_cache ()
  in
  let backfill =
    Back_pagination.enqueue back_queue
      {
        room_id;
        priority = Back_pagination.Normal;
        batch_size = 10;
        max_batches = Some 5;
        stop =
          (fun events ~reached_start:_ ->
            List.exists
              (fun (event : Event_cache.event) ->
                Option.equal Id.Event_id.equal event.event.event_id
                  (Some root_id))
              events);
      }
    |> Back_pagination.await
  in
  check_bool "live back-pagination stops on the requested event" true
    (backfill.reason = Back_pagination.Stop_condition);
  check_bool "live back-pagination inserted the requested event" true
    (Array.exists
       (fun (event : Event_cache.event) ->
         Option.equal Id.Event_id.equal event.event.event_id (Some root_id))
       (Event_cache.snapshot back_cache room_id));
  Back_pagination.close back_queue;

  ignore
    (Harness.ok "pin the thread root"
       (State.set_state client ~room_id
          ~event_type:Event.Event_type.Room_pinned_events
          ~content:
            (Jsont.Json.object'
               [
                 Jsont.Json.mem (Jsont.Json.name "pinned")
                   (Jsont.Json.list
                      [ Jsont.Json.string (Id.Event_id.to_string root_id) ]);
               ])
          ()));
  Harness.wait_until h ~label:"pinned state to reach the base client" (fun () ->
      Option.is_some
        (Base.find_state_event (Harness.state alice_sync) room_id
           ~event_type:Event.Event_type.Room_pinned_events ()));
  let pinned =
    Pinned_events.create ~client ~event_cache:(Event_cache.create ()) ~room_id
      ()
  in
  Harness.ok "load the live pinned-event projection"
    (Pinned_events.refresh pinned ~state:(Harness.state alice_sync));
  check_bool "pinned projection fetches the exact root" true
    (match Pinned_events.snapshot pinned with
    | [| event |] ->
        Option.equal Id.Event_id.equal event.event_id (Some root_id)
        && Harness.string_member "body" event.content = Some body
    | _ -> false);
  Pinned_events.close pinned;

  let store = Store.memory () in
  Store.set_room store (Store.empty_room_info ~room_id ~membership:Store.Joined);
  let partial = Base.of_store store ~user_id:alice.user_id () in
  let details = Room_details.create ~client ~state:partial room_id in
  let details =
    Harness.ok "refresh lazy room members" (Room_details.ensure_members details)
  in
  check_bool "live member snapshot is complete" true
    (Room_details.members_complete details);
  check_int "public room has its creator" 1
    (Room_details.human_member_count details)

(* The legacy room-preview stream has no modern replacement yet. Exercise it
   separately from [Room_preview], with a user who never joins the room. *)
let test_peeking () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"peek-owner" () in
  let lurker = Harness.register_user h ~prefix:"peek-reader" () in
  let owner = Harness.base alice in
  let room_id =
    Harness.ok "create world-readable room"
      (Rooms.create owner ~visibility:Matrix_proto.Common.Visibility.Public
         ~preset:Rooms.Public_chat ())
  in
  let history_visibility =
    Event.Room_history_visibility_content.make
      ~history_visibility:Event.History_visibility.World_readable
    |> json_of_jsont "encode world-readable history"
         Event.Room_history_visibility_content.jsont
  in
  ignore
    (Harness.ok "make room history world-readable"
       (State.set_state owner ~room_id
          ~event_type:Event.Event_type.Room_history_visibility
          ~content:history_visibility ()));
  ignore
    (Harness.ok "send initial preview event"
       (Messages.send_text owner ~room_id ~body:"before peeking" ()));
  let initial =
    Harness.ok "room initialSync"
      (Peeking.initial_sync (Harness.base lurker) ~room_id)
  in
  check_string "initialSync identifies the room" (rid room_id)
    (rid initial.room_id);
  Option.iter
    (fun membership ->
      check_bool "non-member peeks as leave" true (membership = Peeking.Leave))
    initial.membership;
  Option.iter
    (fun visibility ->
      check_bool "public room is reported public" true
        (visibility = Peeking.Public))
    initial.visibility;
  let initial_messages =
    match initial.messages with
    | Some messages -> messages
    | None -> Alcotest.fail "initialSync omitted its messages page"
  in
  let body = "live peek " ^ Harness.hex h 4 in
  ignore
    (Harness.ok "send live preview event"
       (Messages.send_text owner ~room_id ~body ()));
  let events =
    Harness.ok "peek event stream"
      (Peeking.peek_events (Harness.base lurker) ~room_id
         ~from:initial_messages.end_ ~timeout:5_000 ())
  in
  check_bool "peek stream returns the new event" true
    (List.exists
       (fun event -> raw_event_string_content_member "body" event = Some body)
       events.chunk)

let peeking_test =
  Alcotest.test_case "legacy world-readable peeking" `Quick test_peeking

let tests =
  [
    Alcotest.test_case "register and whoami" `Quick test_register;
    Alcotest.test_case "create, name, invite, join" `Quick test_room_lifecycle;
    Alcotest.test_case "send and receive" `Quick test_send_and_receive;
    Alcotest.test_case "threaded receipt stays thread-scoped" `Quick
      test_threaded_receipt;
    Alcotest.test_case "attachment send and receive" `Quick
      test_attachment_send_and_receive;
    Alcotest.test_case "media round trip" `Quick test_media;
    Alcotest.test_case "sliding sync once" `Quick test_sliding_sync;
    Alcotest.test_case "sliding sync through common service" `Quick
      test_sliding_sync_service;
    Alcotest.test_case "adaptive sync selects native" `Quick
      test_adaptive_sync_native;
    Alcotest.test_case "a room id in the path" `Quick test_room_id_in_path;
    Alcotest.test_case "room retention and unsupported server policy" `Quick
      test_retention;
    Alcotest.test_case "the v1 room endpoints" `Quick test_v1_room_endpoints;
    Alcotest.test_case "room previews and knock moderation" `Quick
      test_room_preview_and_knocks;
    Alcotest.test_case "directory, search, threads and room details" `Quick
      test_discovery_services;
    peeking_test;
  ]

let peeking_tests = [ peeking_test ]

(* Keep the second-homeserver smoke profile independent of positions in
   [tests]. Adding a Synapse-only scenario must not silently retarget one of
   Dendrite's selected case numbers. *)
let dendrite_core_tests =
  [
    Alcotest.test_case "register and whoami" `Quick test_register;
    Alcotest.test_case "create, name, invite, join" `Quick test_room_lifecycle;
    Alcotest.test_case "send and receive" `Quick test_send_and_receive;
    Alcotest.test_case "attachment send and receive" `Quick
      test_attachment_send_and_receive;
    Alcotest.test_case "a room id in the path" `Quick test_room_id_in_path;
  ]
