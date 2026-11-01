(** The room list against a live homeserver.

    {!Matrix_ui.Room_list} is matrix-rust-sdk's [room_list_service] projected
    onto what [/sync] reports, so every claim it makes about sections, unread
    counts, order and previews only holds if a real server reports what the
    hermetic tests in [test/test_room_list.ml] feed it by hand. These scenarios
    drive a {!Matrix_ui.Runtime} for Bob while Alice acts over HTTP, and assert
    on the room list Bob's runtime publishes:

    + {b sections}: a favourite, a low-priority room, a DM, an invite that
      becomes a join, and a room Bob leaves — each in its section, with the diff
      stream replaying to the final snapshot;
    + {b unread}: Synapse's own [notification_count] rises when Alice writes and
      falls after Bob's read receipt, and the locally evaluated count tracks it;
    + {b sort}: {!Matrix_ui.Room_list.Name} against
      {!Matrix_ui.Room_list.Activity} over three rooms;
    + {b search}: queries differing in case, diacritics and Unicode
      normalization, and a query on the room id;
    + {b previews}: a freshly joined room previews the own user's own join and
      then its last message, never a topic change; an edit becomes the preview
      of the message it replaces; and an encrypted room previews its plaintext;
    + {b marked unread}: the [m.marked_unread] room account data turns a silent
      room unread and a read receipt turns it back.

    Bob is the user under test throughout. *)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Rooms = Matrix_client.Rooms
module State = Matrix_client.State
module Tags = Matrix_client.Tags
module Account_data = Matrix_client.Account_data
module Messages = Matrix_client.Messages
module Receipts = Matrix_client.Receipts
module Queue = Matrix_eio.Send_queue
module Enc = Matrix_eio.Encryption
module Filter = Ui.Room_list.Filter

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_strings = Alcotest.(check (list string))
let rid = Id.Room_id.to_string
let did = Id.Device_id.to_string

(* A runtime forks fibers that never return, so a scenario that wants one to
   stop runs it under a switch of its own and raises out of the body. *)

exception Phase_done

let phase f =
  try
    Eio.Switch.run (fun sw ->
        f sw;
        raise Phase_done)
  with Phase_done -> ()

(* Synapse caches a [/sync] response for two minutes under a key that
   includes the timeout, so two runtimes for the same user must not ask for
   the same one. *)
let sync_timeout = ref 29_000

let start_runtime h ~sw ?encryption (user : Harness.user) =
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:user.client ~sync
      ?encryption ()
  in
  decr sync_timeout;
  Ui.Runtime.start
    ~params:{ Matrix_client.Sync.default_params with timeout = !sync_timeout }
    runtime;
  runtime

let create_room ?name ?(invite = []) ?is_direct ?encrypted (user : Harness.user)
    =
  let room_id =
    Harness.ok "create a room"
      (Rooms.create (Harness.base user) ~preset:Rooms.Trusted_private_chat
         ~invite ?is_direct ?encrypted ())
  in
  Option.iter
    (fun name ->
      ignore
        (Harness.ok "set the room name"
           (State.set_name (Harness.base user) ~room_id ~name)))
    name;
  room_id

let join (user : Harness.user) room_id =
  ignore
    (Harness.ok "join the room"
       (Rooms.join (Harness.base user) ~room_id_or_alias:(`Room_id room_id) ()))

let leave (user : Harness.user) room_id =
  Harness.ok "leave the room" (Rooms.leave (Harness.base user) ~room_id ())

let all_rooms list =
  Array.to_list (Ui.Observable.List.snapshot (Ui.Room_list.all_rooms list))

let visible list =
  Array.to_list (Ui.Observable.List.snapshot (Ui.Room_list.rooms list))

let ids rooms = List.map (fun (r : Ui.Room_list.room) -> rid r.id) rooms

let find rooms room_id =
  List.find_opt
    (fun (r : Ui.Room_list.room) -> String.equal (rid r.id) (rid room_id))
    rooms

let entry list room_id =
  match find (all_rooms list) room_id with
  | Some room -> room
  | None -> Alcotest.failf "the room list holds no %s" (rid room_id)

let is_visible list room_id = Option.is_some (find (visible list) room_id)

(* [Ui.Observable.List.next] blocks, so a scenario that asserts on the diff
   stream drains it in a fiber of its own. *)
let collect_diffs ~sw list =
  let initial, subscription = Ui.Observable.List.subscribe ~sw list in
  let received = ref [] in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        match Ui.Observable.List.next subscription with
        | None -> ()
        | Some batch ->
            received := !received @ batch;
            loop ()
      in
      loop ();
      `Stop_daemon);
  (initial, received)

let wait_section h list room_id section =
  Harness.wait_until h
    ~label:(Printf.sprintf "%s to reach its section" (rid room_id))
    (fun () ->
      match find (all_rooms list) room_id with
      | Some room -> room.section = section
      | None -> false)

(* Tags, [m.direct] and membership decide the section, and a room moves
   between them as the server reports the change. *)

let test_sections () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let plain =
    create_room ~name:("Plain-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  let fav = create_room ~name:("Fav-" ^ tag) ~invite:[ bob.user_id ] alice in
  let low = create_room ~name:("Low-" ^ tag) ~invite:[ bob.user_id ] alice in
  let dm =
    create_room ~name:("Dm-" ^ tag) ~invite:[ bob.user_id ] ~is_direct:true
      alice
  in
  let gone = create_room ~name:("Gone-" ^ tag) ~invite:[ bob.user_id ] alice in
  (* Left pending on purpose: an invite Bob has not answered. *)
  let pending =
    create_room ~name:("Pending-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  List.iter (join bob) [ plain; fav; low; dm; gone ];
  (* [is_direct] belongs to Bob's stripped invite and stops defining the room
     once he joins.  Joined rooms follow Bob's global [m.direct] account data,
     matching matrix-rust-sdk's MatrixSpec DM definition. *)
  Harness.ok "record the joined direct-room association"
    (Account_data.mark_as_dm (Harness.base bob) ~user_id:alice.user_id
       ~room_id:dm);
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  let initial, diffs = collect_diffs ~sw (Ui.Room_list.rooms list) in

  Harness.wait_until h ~label:"every room to reach the list" (fun () ->
      List.for_all
        (fun room_id -> Option.is_some (find (all_rooms list) room_id))
        [ plain; fav; low; dm; gone; pending ]);

  check_bool "an unanswered invite is an Invite" true
    ((entry list pending).section = Ui.Room_list.Invites);
  check_bool "a plain joined room is a Room" true
    ((entry list plain).section = Ui.Room_list.Rooms);
  check_bool "a joined room in m.direct is a DM" true (entry list dm).is_dm;
  wait_section h list dm Ui.Room_list.People;

  (* Tagging is Bob's own account data, so Bob sets it. *)
  Harness.ok "tag the room a favourite"
    (Tags.set_favourite (Harness.base bob) ~room_id:fav ~favourite:true ());
  Harness.ok "tag the room low priority"
    (Tags.set_low_priority (Harness.base bob) ~room_id:low ~low_priority:true ());
  wait_section h list fav Ui.Room_list.Favourites;
  wait_section h list low Ui.Room_list.Low_priority;
  check_bool "the tag is carried on the room" true
    (List.mem_assoc "m.favourite" (entry list fav).tags);

  (* An invite Bob accepts moves from Invites to Rooms. *)
  join bob pending;
  wait_section h list pending Ui.Room_list.Rooms;

  (* And a room Bob leaves moves to Historical, out of the default filter. *)
  leave bob gone;
  wait_section h list gone Ui.Room_list.Historical;
  Harness.wait_until h ~label:"the left room to leave the default filter"
    (fun () -> not (is_visible list gone));
  check_bool "but it is still in the unfiltered list" true
    (Option.is_some (find (all_rooms list) gone));

  (* The sections the default filter shows, in section order. *)
  let sections =
    List.map (fun (r : Ui.Room_list.room) -> r.section) (visible list)
  in
  check_bool "the visible list is ordered by section" true
    (List.sort_uniq compare sections
    = List.sort_uniq compare
        [
          Ui.Room_list.Favourites;
          Ui.Room_list.People;
          Ui.Room_list.Rooms;
          Ui.Room_list.Low_priority;
        ]);
  check_bool "and no section ever goes backwards" true
    (let rank = function
       | Ui.Room_list.Invites -> 0
       | Ui.Room_list.Favourites -> 1
       | Ui.Room_list.People -> 2
       | Ui.Room_list.Rooms -> 3
       | Ui.Room_list.Low_priority -> 4
       | Ui.Room_list.Historical -> 5
     in
     let ranks = List.map rank sections in
     List.sort Int.compare ranks = ranks);

  (* Composable filters over the same list. *)
  Ui.Room_list.set_filter list Filter.Favourite;
  check_strings "the favourite filter keeps exactly the tagged room"
    [ rid fav ]
    (ids (visible list));
  Ui.Room_list.set_filter list Filter.Dm;
  check_strings "the People category keeps the DM"
    [ rid dm ]
    (ids (visible list));
  Ui.Room_list.set_filter list
    (Filter.All
       [ Filter.Non_left; Filter.Not Filter.Dm; Filter.Not Filter.Favourite ]);
  check_bool "all [non_left; not People; not favourite] drops both" true
    ((not (is_visible list dm)) && not (is_visible list fav));
  Ui.Room_list.set_filter list (Filter.Room_ids [ plain ]);
  check_strings "the identifiers filter" [ rid plain ] (ids (visible list));
  Ui.Room_list.set_filter list Filter.Non_left;

  (* Every diff the subscription delivered, replayed over the snapshot it was
     created with, is the snapshot the list now holds. *)
  Harness.wait_until h ~label:"the diff stream to reproduce the snapshot"
    (fun () ->
      Ui.Observable.List.apply_all initial !diffs
      = Ui.Observable.List.snapshot (Ui.Room_list.rooms list));
  check_int "the replay has as many rooms as the snapshot"
    (List.length (visible list))
    (Array.length (Ui.Observable.List.apply_all initial !diffs))

(* Synapse's default push rules make a message in a room of two count as a
   notification, so [notification_count] in the sync's summary rises when
   alice writes; [Sync_service] evaluates the same rules locally and keeps
   its own count beside it. A read receipt clears both. *)

let test_unread () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let loud = create_room ~name:("Loud-" ^ tag) ~invite:[ bob.user_id ] alice in
  let quiet =
    create_room ~name:("Quiet-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  join bob loud;
  join bob quiet;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  let queue = Harness.start_send_queue h alice in
  let sync = Ui.Runtime.sync_service runtime in
  let info room_id =
    match
      Matrix_client.Base_client.find_room
        (Matrix_eio.Sync_service.state sync)
        room_id
    with
    | Some info -> info
    | None -> Alcotest.failf "no room_info for %s" (rid room_id)
  in
  Harness.wait_until h ~label:"both rooms to reach the list" (fun () ->
      Option.is_some (find (all_rooms list) loud)
      && Option.is_some (find (all_rooms list) quiet));

  let body = "shout " ^ tag in
  let event_id =
    Harness.wait_sent h (Queue.send_text queue ~room_id:loud ~body)
  in
  Harness.wait_until h ~label:"the notification count to rise" (fun () ->
      (entry list loud).notification_count > 0);
  let room = entry list loud in
  check_bool "the room reads as unread" true (Ui.Room_list.unread room);
  check_bool "the server counted the notification" true
    ((info loud).notification_count > 0);
  check_bool "and so did the local push evaluation" true
    ((info loud).local_notification_count > 0);
  check_bool "the local unread message count rose too" true
    (room.unread_messages > 0);
  check_bool "the other room stayed quiet" false
    (Ui.Room_list.unread (entry list quiet));

  Ui.Room_list.set_filter list
    (Filter.All
       [
         Filter.Non_left;
         Filter.Any
           [ Filter.Unread Filter.Notifications; Filter.Unread Filter.Messages ];
       ]);
  check_strings
    "the composed unread filter keeps only the room with the message"
    [ rid loud ]
    (ids (visible list));
  Ui.Room_list.set_filter list (Filter.Unread Filter.Notifications);
  check_strings "and so does rust-sdk's Notifications category"
    [ rid loud ]
    (ids (visible list));
  Ui.Room_list.set_filter list
    (Filter.All
       [
         Filter.Non_left;
         Filter.Any
           [ Filter.Unread Filter.Notifications; Filter.Unread Filter.Messages ];
       ]);

  Harness.ok "send a read receipt"
    (Receipts.send_receipt (Harness.base bob) ~room_id:loud ~event_id ());
  Harness.wait_until h ~label:"the notification count to drop" (fun () ->
      (entry list loud).notification_count = 0
      && (entry list loud).unread_messages = 0);
  check_bool "the server's count went with it" true
    ((info loud).notification_count = 0);
  check_bool "and the local one" true ((info loud).local_notification_count = 0);
  Harness.wait_until h ~label:"the room to fall out of the unread filter"
    (fun () -> not (is_visible list loud));
  Ui.Room_list.set_filter list Filter.Non_left;
  check_bool "clearing the filter brings it back" true (is_visible list loud)

(* A new message moves a room to the top of [Activity] and leaves [Name]
   alone. *)

let test_sort () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let names = [ "Alpha-" ^ tag; "Mike-" ^ tag; "Zulu-" ^ tag ] in
  let rooms =
    List.map (fun name -> create_room ~name ~invite:[ bob.user_id ] alice) names
  in
  List.iter (join bob) rooms;
  let alpha, mike, zulu =
    match rooms with
    | [ a; m; z ] -> (a, m, z)
    | _ -> Alcotest.fail "three rooms"
  in
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  let queue = Harness.start_send_queue h alice in
  let ours () =
    List.filter
      (fun (r : Ui.Room_list.room) ->
        List.exists (fun room_id -> String.equal (rid r.id) (rid room_id)) rooms)
      (visible list)
  in
  Harness.wait_until h ~label:"all three rooms to reach the list" (fun () ->
      List.length (ours ()) = 3);

  (* Oldest first: the rooms were created in this order, so alpha's own join
     is the oldest event of the three. Say what we mean by writing to them. *)
  List.iter
    (fun (room_id, body) ->
      ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body)))
    [ (zulu, "z " ^ tag); (mike, "m " ^ tag); (alpha, "a " ^ tag) ];
  Harness.wait_until h ~label:"all three messages to arrive" (fun () ->
      List.for_all
        (fun room_id -> Option.is_some (entry list room_id).latest_timestamp)
        rooms);

  Harness.wait_until h ~label:"Activity to put the newest room first" (fun () ->
      ids (ours ()) = [ rid alpha; rid mike; rid zulu ]);
  Ui.Room_list.set_sort list Ui.Room_list.Name;
  check_strings "Name sorts by display name"
    [ rid alpha; rid mike; rid zulu ]
    (ids (ours ()));

  (* A message in the last room by name moves it to the top under Activity
     and leaves it where it is under Name. *)
  ignore
    (Harness.wait_sent h
       (Queue.send_text queue ~room_id:zulu ~body:("later " ^ tag)));
  Harness.wait_until h ~label:"the new message to reach the list" (fun () ->
      (entry list zulu).latest = Some ("later " ^ tag));
  check_strings "Name is unmoved by a new message"
    [ rid alpha; rid mike; rid zulu ]
    (ids (ours ()));
  Ui.Room_list.set_sort list Ui.Room_list.Activity;
  check_strings "Activity moves it to the top"
    [ rid zulu; rid alpha; rid mike ]
    (ids (ours ()));
  check_bool "and its latest event is the newest" true
    (Option.compare Matrix_proto.Event.Timestamp.compare
       (entry list zulu).latest_timestamp (entry list alpha).latest_timestamp
    > 0)

(* [Matching.search_key] folds case, normalizes compatibly and drops combining
   marks, so a query matches a name it differs from in any of the three. *)

let test_search () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  (* An eszett, which folding maps onto "ss"; an e-acute written composed,
     which a query may write decomposed, bare or in capitals. *)
  let cafe = "Stra\u{00DF}e-Caf\u{00E9}-" ^ tag in
  let other = "Andere-" ^ tag in
  let cafe_room = create_room ~name:cafe ~invite:[ bob.user_id ] alice in
  let other_room = create_room ~name:other ~invite:[ bob.user_id ] alice in
  join bob cafe_room;
  join bob other_room;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  Harness.wait_until h ~label:"the named rooms to reach the list" (fun () ->
      match
        (find (all_rooms list) cafe_room, find (all_rooms list) other_room)
      with
      | Some room, Some _ -> String.equal room.name cafe
      | _ -> false);

  let matching query =
    Ui.Room_list.set_filter list
      (Filter.All [ Filter.Non_left; Filter.Search query ]);
    let result = ids (visible list) in
    Ui.Room_list.set_filter list Filter.Non_left;
    result
  in
  (* Every query below but the first is spelled differently from the name it
     has to match; the filter is what closes the gap. *)
  let only_cafe what query =
    check_bool
      (Printf.sprintf "the query %S differs from the name" what)
      false (String.equal query cafe);
    check_strings what [ rid cafe_room ] (matching query)
  in
  check_strings "an exact query matches" [ rid cafe_room ] (matching cafe);
  only_cafe "capitals with no eszett, which folding supplies"
    ("STRASSE-CAF\u{00C9}-" ^ String.uppercase_ascii tag);
  only_cafe "a decomposed e-acute matches a composed one" ("cafe\u{0301}-" ^ tag);
  only_cafe "a bare e matches an accented one" ("cafe-" ^ tag);
  only_cafe "and an accented query the same name" ("Caf\u{00E9}-" ^ tag);
  only_cafe "a fullwidth spelling normalizes onto the plain one"
    ("\u{FF43}\u{FF41}\u{FF46}\u{FF45}-" ^ tag);
  check_strings "a query on the room id matches that room"
    [ rid cafe_room ]
    (matching (rid cafe_room));
  check_strings "and a query matching neither name matches nothing" []
    (matching ("nothing-" ^ tag));
  check_int "clearing the query brings both back" 2
    (List.length
       (List.filter
          (fun (r : Ui.Room_list.room) ->
            String.equal (rid r.id) (rid cafe_room)
            || String.equal (rid r.id) (rid other_room))
          (visible list)))

(* The first sync of a freshly joined room puts the room's whole state in
   the timeline window, and only the own user's own join may preview it, so
   a room bob has just joined reads as "bob accepted the invitation" and
   never as a topic change or the literal ["m.room.member"]. *)

let test_preview () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let room_id =
    create_room ~name:("Preview-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  (* Bob joins, and alice then changes the topic: a state event newer than
     his join, and one the filter refuses. *)
  join bob room_id;
  ignore
    (Harness.ok "set a topic, a state event after the join"
       (State.set_topic (Harness.base alice) ~room_id ~topic:("topic " ^ tag)));
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  let queue = Harness.start_send_queue h alice in
  Harness.wait_until h ~label:"the room to reach the list" (fun () ->
      Option.is_some (find (all_rooms list) room_id));
  Harness.wait_until h ~label:"the topic to be applied" (fun () ->
      (entry list room_id).topic = Some ("topic " ^ tag));
  check_string "a room with no message previews the own user's own join"
    (Id.User_id.to_string bob.user_id ^ " accepted the invitation")
    (Option.value (entry list room_id).latest ~default:"<none>");
  check_bool "which the newer topic change did not displace" true
    (Option.map Id.User_id.to_string (entry list room_id).latest_sender
    = Some (Id.User_id.to_string bob.user_id));

  (* And a message displaces it. *)
  let body = "the only message " ^ tag in
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body));
  Harness.wait_until h ~label:"the message to become the preview" (fun () ->
      (entry list room_id).latest = Some body);
  let room = entry list room_id in
  check_string "a joined room previews its last message, not a state type" body
    (Option.value room.latest ~default:"<none>");
  check_bool "attributed to its sender" true
    (Option.map Id.User_id.to_string room.latest_sender
    = Some (Id.User_id.to_string alice.user_id));
  check_bool "with the message's timestamp" true
    (Option.is_some room.latest_timestamp);
  check_bool "and it is not an unsent echo" false room.latest_is_unsent;

  (* A reaction is not a preview either. *)
  let event_id =
    Harness.wait_for_event h (Harness.start_sync h alice) room_id (fun event ->
        match Matrix_proto.Event.Event_type.to_string event.type_ with
        | "m.room.message" -> true
        | _ -> false)
    |> fun event -> Option.get event.event_id
  in
  ignore
    (Harness.wait_sent h
       (Queue.send_reaction queue ~room_id ~relates_to:event_id ~key:"+1"));
  Harness.wait_until h ~label:"the reaction to reach bob" (fun () ->
      Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room_id
      |> Array.exists (fun (event : Ui.Event_cache.event) ->
          String.equal
            (Matrix_proto.Event.Event_type.to_string event.event.type_)
            "m.reaction"));
  check_string "a reaction does not become the preview" body
    (Option.value (entry list room_id).latest ~default:"<none>")

(* An encrypted room previews the plaintext, because the cache hands the
   projection the decryption rather than the ciphertext. *)

let test_encrypted_preview () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let alice_enc =
    Enc.of_env (Harness.env h) ~user_id:alice.user_id ~device_id:alice.device_id
      ()
  in
  let bob_enc =
    Enc.of_env (Harness.env h) ~user_id:bob.user_id ~device_id:bob.device_id ()
  in
  let tag = Harness.hex h 4 in
  let room_id =
    create_room ~name:("Secret-" ^ tag) ~invite:[ bob.user_id ] ~encrypted:true
      alice
  in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw ~encryption:bob_enc bob in
  let list = Ui.Runtime.room_list runtime in
  let alice_sync = Harness.start_sync h ~encryption:alice_enc alice in
  let queue =
    Harness.start_send_queue h ~encryption:alice_enc ~sync:alice_sync alice
  in
  Harness.wait_until h ~label:"the room to reach the list" (fun () ->
      Option.is_some (find (all_rooms list) room_id));
  Harness.wait_until h ~label:"the room to be known as encrypted" (fun () ->
      (entry list room_id).encrypted);

  let body = "ciphered " ^ tag in
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body));
  Harness.wait_until h ~label:"the plaintext to become the preview" (fun () ->
      (entry list room_id).latest = Some body);
  check_string "an encrypted room previews its plaintext" body
    (Option.value (entry list room_id).latest ~default:"<none>");
  check_bool "and the room is still flagged encrypted" true
    (entry list room_id).encrypted

(* The preview of an edited message is the edit. The edit is never a preview
   of its own: the fallback body Synapse stores, ["* corrected"], must not
   appear. *)

let send_edit (user : Harness.user) ~room_id ~target ~body =
  let mem name json = Jsont.Json.mem (Jsont.Json.name name) json in
  let content =
    Jsont.Json.object'
      [
        mem "msgtype" (Jsont.Json.string "m.text");
        mem "body" (Jsont.Json.string ("* " ^ body));
        mem "m.new_content"
          (Jsont.Json.object'
             [
               mem "msgtype" (Jsont.Json.string "m.text");
               mem "body" (Jsont.Json.string body);
             ]);
        mem "m.relates_to"
          (Jsont.Json.object'
             [
               mem "rel_type" (Jsont.Json.string "m.replace");
               mem "event_id" (Jsont.Json.string (Id.Event_id.to_string target));
             ]);
      ]
  in
  Harness.ok "send the edit"
    (Messages.send_event (Harness.base user) ~room_id
       ~event_type:Matrix_proto.Event.Event_type.Room_message ~content)

let test_edited_preview () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let room_id =
    create_room ~name:("Edited-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  let queue = Harness.start_send_queue h alice in
  Harness.wait_until h ~label:"the room to reach the list" (fun () ->
      Option.is_some (find (all_rooms list) room_id));

  let original = "original " ^ tag in
  let event_id =
    Harness.wait_sent h (Queue.send_text queue ~room_id ~body:original)
  in
  Harness.wait_until h ~label:"the message to become the preview" (fun () ->
      (entry list room_id).latest = Some original);

  let corrected = "corrected " ^ tag in
  ignore (send_edit alice ~room_id ~target:event_id ~body:corrected);
  Harness.wait_until h ~label:"the edit to become the preview" (fun () ->
      (entry list room_id).latest = Some corrected);
  check_string "an edited message previews as its edit" corrected
    (Option.value (entry list room_id).latest ~default:"<none>");
  check_bool "and not as the fallback body the edit carries" false
    (Option.value (entry list room_id).latest ~default:"" = "* " ^ corrected);

  (* A second edit wins over the first, and a later message over both. *)
  let again = "corrected again " ^ tag in
  ignore (send_edit alice ~room_id ~target:event_id ~body:again);
  Harness.wait_until h ~label:"the newest edit to become the preview" (fun () ->
      (entry list room_id).latest = Some again);
  let later = "a later message " ^ tag in
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:later));
  Harness.wait_until h ~label:"a later message to take over" (fun () ->
      (entry list room_id).latest = Some later)

(* Each unread count is ORed with the room's [m.marked_unread] flag, so a
   room with nothing in it reads as unread the moment a user marks it. It
   clears when the flag is written false, and when the own read receipt
   moves. *)

let set_marked_unread (user : Harness.user) ~room_id ~unread =
  Harness.ok "mark the room unread"
    (Account_data.set_room (Harness.base user) ~room_id
       ~event_type:Matrix_proto.Event.Event_type.Marked_unread
       ~content:
         (Jsont.Json.object'
            [
              Jsont.Json.mem (Jsont.Json.name "unread") (Jsont.Json.bool unread);
            ]))

let test_marked_unread () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  let room_id =
    create_room ~name:("Marked-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  let quiet_id =
    create_room ~name:("Quiet-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  join bob room_id;
  join bob quiet_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let list = Ui.Runtime.room_list runtime in
  Harness.wait_until h ~label:"both rooms to reach the list" (fun () ->
      Option.is_some (find (all_rooms list) room_id)
      && Option.is_some (find (all_rooms list) quiet_id));
  check_bool "neither room is unread to begin with" false
    (Ui.Room_list.unread (entry list room_id)
    || Ui.Room_list.unread (entry list quiet_id));

  (* Nothing has been said in the room; the flag alone makes it unread. *)
  set_marked_unread bob ~room_id ~unread:true;
  Harness.wait_until h ~label:"the flag to reach the room list" (fun () ->
      (entry list room_id).marked_unread);
  let room = entry list room_id in
  check_bool "a marked room reads as unread" true (Ui.Room_list.unread room);
  check_int "with no unread messages behind it" 0 room.unread_messages;
  check_int "and no notifications" 0 room.notification_count;
  check_bool "the other room is untouched" false
    (Ui.Room_list.unread (entry list quiet_id));

  Ui.Room_list.set_filter list (Filter.Unread Filter.Messages);
  check_strings "rust-sdk's Messages category ORs the flag"
    [ rid room_id ]
    (ids (visible list));
  Ui.Room_list.set_filter list (Filter.Unread Filter.Mentions);
  check_strings "and so does Mentions, which counts nothing here"
    [ rid room_id ]
    (ids (visible list));
  Ui.Room_list.set_filter list
    (Filter.All
       [
         Filter.Non_left;
         Filter.Any
           [ Filter.Unread Filter.Notifications; Filter.Unread Filter.Messages ];
       ]);
  check_strings "as does the composed unread filter"
    [ rid room_id ]
    (ids (visible list));
  Ui.Room_list.set_filter list Filter.Non_left;

  (* Writing it false clears it. *)
  set_marked_unread bob ~room_id ~unread:false;
  Harness.wait_until h ~label:"the flag to clear" (fun () ->
      not (entry list room_id).marked_unread);
  check_bool "and the room is no longer unread" false
    (Ui.Room_list.unread (entry list room_id));

  (* And so does our own read receipt: the flag goes up again, alice writes,
     and bob's receipt takes both the count and the flag down. *)
  set_marked_unread bob ~room_id ~unread:true;
  Harness.wait_until h ~label:"the flag to come back" (fun () ->
      (entry list room_id).marked_unread);
  let queue = Harness.start_send_queue h alice in
  let event_id =
    Harness.wait_sent h
      (Queue.send_text queue ~room_id ~body:("something " ^ tag))
  in
  Harness.wait_until h ~label:"the message to reach the list" (fun () ->
      (entry list room_id).notification_count > 0);
  Harness.ok "send a read receipt"
    (Receipts.send_receipt (Harness.base bob) ~room_id ~event_id ());
  Harness.wait_until h ~label:"the receipt to clear the flag and the count"
    (fun () ->
      let room = entry list room_id in
      (not room.marked_unread) && room.notification_count = 0);
  check_bool "our own read receipt clears the manual mark too" false
    (Ui.Room_list.unread (entry list room_id))

let tests =
  [
    Alcotest.test_case "sections, tags, DMs and membership moves" `Slow
      test_sections;
    Alcotest.test_case "unread counts and the read receipt" `Slow test_unread;
    Alcotest.test_case "name against activity order" `Slow test_sort;
    Alcotest.test_case "case, diacritics and normalization in search" `Slow
      test_search;
    Alcotest.test_case "previews skip state events and reactions" `Slow
      test_preview;
    Alcotest.test_case "an encrypted room previews its plaintext" `Slow
      test_encrypted_preview;
    Alcotest.test_case "an edit becomes the preview" `Slow test_edited_preview;
    Alcotest.test_case "m.marked_unread turns a silent room unread" `Slow
      test_marked_unread;
  ]
