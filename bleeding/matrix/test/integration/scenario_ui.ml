(** The reactive UI models against a live homeserver.

    [matrix-chat.ui] is a projection of what sync and the send queue report, so
    everything it claims is only true if a real server reports what the unit
    tests feed it by hand. These scenarios drive a {!Matrix_ui.Runtime} for one
    user while another user talks to the same room over HTTP, and assert on the
    {!Matrix_ui.Room_timeline} and {!Matrix_ui.Room_list} the runtime publishes:
    aggregation of reactions, edits and redactions, room ordering and filtering
    and accepting an invite, back-pagination across the gap a limited sync
    leaves, reload from SQLite, the life of a local echo, the state events a
    timeline shows and how they read, paginating to the beginning of a room, own
    reactions, formatted replies and redactions sent through the timeline, an
    encrypted room driven entirely by the runtime, a sync that cannot succeed,
    stopping a runtime, a hole left by a sync that skipped past what the store
    kept and the pagination that fills it, plaintext recovered from a
    ciphertext-only store after a restart, the read marker, and automatic
    receipt-target backfill.

    Bob is the user under test in most of them: he gets the runtime. Alice is
    driven directly through {!Matrix_client}'s endpoints and a send queue,
    because what is under test is the model, not the sending. The later
    scenarios give both users a runtime, because what they assert — whose
    reaction it is, what each side sees decrypted — is a difference between two
    views of the same room. *)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Rooms = Matrix_client.Rooms
module State = Matrix_client.State
module Queue = Matrix_eio.Send_queue

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_strings = Alcotest.(check (list string))
let rid = Id.Room_id.to_string
let uid = Id.User_id.to_string
let eid = Id.Event_id.to_string

(* Alice's raw send queue does not have a room-timeline facade, so an
   [m.replace] goes out as a plain [m.room.message] with the relation built
   here. *)

let jstr = Jsont.Json.string
let jmem name value = Jsont.Json.mem (Jsont.Json.name name) value
let jobj members = Jsont.Json.object' members

let text_content body =
  jobj [ jmem "msgtype" (jstr "m.text"); jmem "body" (jstr body) ]

let edit_content ~target ~body =
  jobj
    [
      jmem "msgtype" (jstr "m.text");
      jmem "body" (jstr ("* " ^ body));
      jmem "m.new_content" (text_content body);
      jmem "m.relates_to"
        (jobj
           [
             jmem "rel_type" (jstr "m.replace");
             jmem "event_id" (jstr (eid target));
           ]);
    ]

(* A scenario that wants a runtime to stop — because it is about to reopen
   the store the runtime writes to — runs it under a switch of its own.
   [Eio.Switch.run] waits for the fibers the runtime forked and those never
   return, so raising out of the body is what cancels them; the switch
   absorbs the resulting [Cancelled] and re-raises what was thrown. *)

exception Phase_done

let phase f =
  try
    Eio.Switch.run (fun sw ->
        f sw;
        raise Phase_done)
  with Phase_done -> ()

let create_room ?name ?(invite = []) ?is_direct (alice : Harness.user) =
  let room_id =
    Harness.ok "create a room"
      (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat
         ~invite ?is_direct ())
  in
  Option.iter
    (fun name ->
      ignore
        (Harness.ok "set the room name"
           (State.set_name (Harness.base alice) ~room_id ~name)))
    name;
  room_id

let join (user : Harness.user) room_id =
  ignore
    (Harness.ok "join the room"
       (Rooms.join (Harness.base user) ~room_id_or_alias:(`Room_id room_id) ()))

(* Synapse caches a [/sync] response for two minutes under a key that
   includes the user, the device, the [since] token and the timeout, so a
   second runtime for the same user replays the first one's responses
   instead of making the cold request it asked for. Giving each runtime its
   own timeout keeps the keys apart. *)
let sync_timeout = ref 30_000

let start_runtime h ~sw ?event_store ?encryption (user : Harness.user) =
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:user.client ~sync
      ?encryption ?event_store ()
  in
  decr sync_timeout;
  Ui.Runtime.start
    ~params:{ Matrix_client.Sync.default_params with timeout = !sync_timeout }
    runtime;
  runtime

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go index =
    index + n <= h && (String.sub haystack index n = needle || go (index + 1))
  in
  go 0

let items timeline = Array.to_list (Ui.Room_timeline.snapshot timeline)

let event_items timeline =
  List.filter_map
    (function Ui.Room_timeline.Event event -> Some event | _ -> None)
    (items timeline)

let is_message (item : Ui.Room_timeline.event_item) =
  match item.event.content with Ui.Presentation.Message _ -> true | _ -> false

let message_items timeline = List.filter is_message (event_items timeline)

let body_of (item : Ui.Room_timeline.event_item) =
  Option.value ~default:"?" (Ui.Presentation.preview item.event)

let message_bodies timeline = List.map body_of (message_items timeline)

let find_body timeline body =
  List.find_opt
    (fun item -> String.equal (body_of item) body)
    (message_items timeline)

let has_gap timeline =
  List.exists
    (function
      | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Gap _; _ } -> true
      | _ -> false)
    (items timeline)

(* The items a correct aggregation folds into another item rather than
   showing: a reaction, a redaction, and the [m.replace] that carries an
   edit. *)
let unaggregated timeline =
  List.filter
    (fun (item : Ui.Room_timeline.event_item) ->
      match (item.event.content, item.event.relation) with
      | Ui.Presentation.Reaction _, _ | Ui.Presentation.Redaction _, _ -> true
      | _, Some { kind = Ui.Presentation.Replacement; _ } -> true
      | _ -> false)
    (event_items timeline)

(* A scenario that asserts on the diff stream subscribes before the traffic
   it is about to cause, and drains the subscription in a fiber of its own,
   because [Observable.List.next] blocks. *)

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

(* Alice writes a message, reacts to it, edits it, writes a second message
   and redacts that. Bob's timeline should show two events: the first
   carrying the edit's body, [edited], and alice's reaction; the second
   flagged redacted. *)

let test_aggregation () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let queue = Harness.start_send_queue h alice in

  (* Subscribed before alice says anything, so every diff that builds the
     final snapshot is in the stream. *)
  let initial, diffs = collect_diffs ~sw (Ui.Room_timeline.items timeline) in

  let tag = Harness.hex h 4 in
  let first = "first " ^ tag in
  let edited = "edited " ^ tag in
  let second = "second " ^ tag in
  let first_id =
    Harness.wait_sent h (Queue.send_text queue ~room_id ~body:first)
  in
  Harness.wait_until h ~label:"bob to see the first message" (fun () ->
      find_body timeline first <> None);
  ignore
    (Harness.wait_sent h
       (Queue.send_reaction queue ~room_id ~relates_to:first_id ~key:"\u{1F44D}"));
  let edit_id =
    Harness.wait_sent h
      (Queue.send_message queue ~room_id ~event_type:"m.room.message"
         ~content:(edit_content ~target:first_id ~body:edited))
  in
  let second_id =
    Harness.wait_sent h (Queue.send_text queue ~room_id ~body:second)
  in
  (* The redaction only after bob has the message it redacts: otherwise the
     server may hand him the already-redacted stub and there is nothing to
     flag. *)
  Harness.wait_until h ~label:"bob to see the second message" (fun () ->
      find_body timeline second <> None);
  ignore
    (Harness.wait_sent h
       (Queue.send_redaction queue ~room_id ~event_id:second_id ()));

  Harness.wait_until h ~label:"bob's timeline to aggregate everything"
    (fun () ->
      match message_bodies timeline with
      | [ a; b ] when String.equal a edited && String.equal b second ->
          let item = Option.get (find_body timeline second) in
          item.redacted
      | _ -> false);

  check_strings "the timeline shows the edited first message and the second"
    [ edited; second ] (message_bodies timeline);
  check_int "no reaction, redaction or replacement is an item of its own" 0
    (List.length (unaggregated timeline));

  let first_item = Option.get (find_body timeline edited) in
  check_bool "the first message is marked edited" true first_item.edited;
  check_string "and it keeps the original's event id" (eid first_id)
    (eid (Option.get first_item.event.event_id));
  let revisions =
    Matrix_eio.Relations.get_edit_revisions bob.client ~room_id
      ~event_id:first_id ()
  in
  check_strings "the network edit history is original then replacement"
    [ eid first_id; eid edit_id ]
    (List.filter_map
       (fun (event : Matrix_proto.Event.Raw_event.t) ->
         Option.map eid event.event_id)
       revisions);
  (match first_item.reactions with
  | [ reaction ] ->
      check_string "the reaction key survived" "\u{1F44D}" reaction.key;
      check_int "counted once" 1 reaction.count;
      check_strings "by alice"
        [ uid alice.user_id ]
        (List.map uid reaction.senders);
      check_bool "and not by bob" false reaction.own
  | reactions ->
      Alcotest.failf "expected one reaction, got %d" (List.length reactions));

  let second_item = Option.get (find_body timeline second) in
  check_bool "the second message is flagged redacted" true second_item.redacted;

  (* Every diff the subscription delivered, replayed over the snapshot it
     was created with, is the snapshot the timeline now holds. *)
  Harness.wait_until h ~label:"the diff stream to reproduce the snapshot"
    (fun () ->
      Ui.Observable.List.apply_all initial !diffs
      = Ui.Room_timeline.snapshot timeline);
  check_int "the replay has as many items as the snapshot"
    (Array.length (Ui.Room_timeline.snapshot timeline))
    (Array.length (Ui.Observable.List.apply_all initial !diffs))

let test_room_list () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let tag = Harness.hex h 4 in
  (* An eszett, so that a query typed in capitals has something to fold:
     folding maps it onto "ss". *)
  let name_one = "Zimmer-Stra\u{00DF}e-" ^ tag in
  let name_two = "Andere-" ^ tag in
  let room_one = create_room ~name:name_one ~invite:[ bob.user_id ] alice in
  let room_two = create_room ~name:name_two ~invite:[ bob.user_id ] alice in
  join bob room_one;
  join bob room_two;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let room_list = Ui.Runtime.room_list runtime in
  let queue = Harness.start_send_queue h alice in

  let rooms () =
    Array.to_list (Ui.Observable.List.snapshot (Ui.Room_list.rooms room_list))
  in
  let find room_id =
    List.find_opt
      (fun (r : Ui.Room_list.room) -> String.equal (rid r.id) (rid room_id))
      (rooms ())
  in
  let top () =
    match rooms () with r :: _ -> Some r.Ui.Room_list.id | [] -> None
  in

  let first = "one " ^ tag in
  let second = "two " ^ tag in
  let third = "three " ^ tag in
  ignore
    (Harness.wait_sent h (Queue.send_text queue ~room_id:room_one ~body:first));
  Harness.wait_until h ~label:"the room list to carry the first room" (fun () ->
      match find room_one with
      | Some room -> room.latest = Some first
      | None -> false);
  let room = Option.get (find room_one) in
  check_string "the room list shows the name alice set" name_one room.name;
  check_string "and the latest body is the last visible message" first
    (Option.value room.latest ~default:"");
  check_string "attributed to alice" (uid alice.user_id)
    (uid (Option.get room.latest_sender));

  ignore
    (Harness.wait_sent h (Queue.send_text queue ~room_id:room_two ~body:second));
  Harness.wait_until h ~label:"the second room to reach the top" (fun () ->
      top () = Some room_two
      && (Option.get (find room_two)).latest = Some second);
  check_bool "the first room is no longer at the top" false
    (top () = Some room_one);

  ignore
    (Harness.wait_sent h (Queue.send_text queue ~room_id:room_one ~body:third));
  Harness.wait_until h ~label:"a new message to move the first room to the top"
    (fun () ->
      top () = Some room_one && (Option.get (find room_one)).latest = Some third);
  let one = Option.get (find room_one) and two = Option.get (find room_two) in
  check_bool "the newer timestamp is the one on top" true
    (Option.compare Matrix_proto.Event.Timestamp.compare one.latest_timestamp
       two.latest_timestamp
    > 0);

  (* [Matching.contains] folds case, and folding maps the eszett onto "ss", so a
     query typed in capitals with no eszett still matches the name. *)
  let query = "ZIMMER-STRASSE-" ^ String.uppercase_ascii tag in
  check_bool "the query is not the name" false (String.equal query name_one);
  Ui.Room_list.set_filter room_list
    (Ui.Room_list.Filter.All
       [ Ui.Room_list.Filter.Non_left; Ui.Room_list.Filter.Search query ]);
  check_strings "the filter matches the room whose name differs only by case"
    [ rid room_one ]
    (List.map (fun (r : Ui.Room_list.room) -> rid r.id) (rooms ()));
  Ui.Room_list.set_filter room_list Ui.Room_list.Filter.Non_left;
  check_int "clearing the filter brings both rooms back" 2
    (List.length (rooms ()));

  (* The invites section is the list a bot acts on, and [Runtime.join] is
     what acts on it: the invite arrives, the runtime accepts it by id, and
     the next sync moves the room out of the section. *)
  let room_three =
    create_room ~name:("Dritte-" ^ tag) ~invite:[ bob.user_id ] alice
  in
  Harness.wait_until h ~label:"the invite to reach the room list" (fun () ->
      match Ui.Room_list.find room_list room_three with
      | Some room -> room.section = Ui.Room_list.Invites
      | None -> false);
  (* [find] answers about the room, not about the filter in force. *)
  Ui.Room_list.set_filter room_list
    (Ui.Room_list.Filter.Membership Matrix_client.Base_client.Joined);
  check_bool "the invited room is filtered out of the published list" false
    (List.exists
       (fun (r : Ui.Room_list.room) -> rid r.id = rid room_three)
       (rooms ()));
  check_bool "but the lookup by id still finds it" true
    (Ui.Room_list.find room_list room_three <> None);
  (match Ui.Runtime.join runtime room_three with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "accepting the invite: %s"
        (Matrix_client.Error.to_string error));
  Harness.wait_until h ~label:"the accepted invite to become a joined room"
    (fun () ->
      match Ui.Room_list.find room_list room_three with
      | Some room ->
          room.membership = Matrix_client.Base_client.Joined
          && room.section <> Ui.Room_list.Invites
      | None -> false);
  check_bool "and it is in the published list now that bob has joined" true
    (List.exists
       (fun (r : Ui.Room_list.room) -> rid r.id = rid room_three)
       (rooms ()));
  match Ui.Runtime.leave runtime room_three with
  | Error error ->
      Alcotest.failf "leaving the room: %s"
        (Matrix_client.Error.to_string error)
  | Ok () ->
      Harness.wait_until h ~label:"the left room to leave the joined list"
        (fun () ->
          match Ui.Room_list.find room_list room_three with
          | Some room -> room.membership = Matrix_client.Base_client.Left
          | None -> false)

let history_size = 40

let test_back_pagination () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room alice in
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let body index = Printf.sprintf "history %s %02d" tag index in
  let expected = List.init history_size body in
  (* One FIFO per room, so these land in the order they were written. *)
  let requests =
    List.map (fun body -> Queue.send_text queue ~room_id ~body) expected
  in
  List.iter
    (fun request -> ignore (Harness.wait_sent h ~timeout:180. request))
    requests;

  (* Bob only now joins; the room's history visibility is the default
     "shared", so everything above is his to read. *)
  ignore
    (Harness.ok "invite bob"
       (Rooms.invite (Harness.base alice) ~room_id ~user_id:bob.user_id ()));
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in

  (* The initial sync is limited: the server sent a window, not the room. *)
  Harness.wait_until h ~label:"the initial sync to report a gap" (fun () ->
      Option.is_some
        (Ui.Observable.Value.get
           (Ui.Event_cache.prev_batch (Ui.Runtime.event_cache runtime) room_id))
      && Ui.Observable.Value.get
           (Ui.Event_cache.has_gap (Ui.Runtime.event_cache runtime) room_id));
  check_bool "the timeline shows a gap item" true (has_gap timeline);
  check_bool "and it does not already hold the whole history" true
    (List.length (message_bodies timeline) < history_size);

  let more () =
    Option.is_some
      (Ui.Observable.Value.get
         (Ui.Event_cache.prev_batch (Ui.Runtime.event_cache runtime) room_id))
  in
  let rec paginate rounds =
    if rounds > 0 && more () then
      let before =
        Array.length
          (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room_id)
      in
      let outcome = Ui.Room_timeline.paginate_back timeline ~limit:20 () in
      let after =
        Array.length
          (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room_id)
      in
      (* A page that added nothing and left the token in place would loop
         forever; stop and let the assertions below report what is missing. *)
      if after > before && outcome = Ok `More then paginate (rounds - 1)
  in
  paginate 30;
  check_bool "pagination reported no error" true
    (Ui.Observable.Value.get (Ui.Room_timeline.pagination_error timeline) = None);
  check_bool "and is no longer in flight" false
    (Ui.Observable.Value.get (Ui.Room_timeline.loading timeline));
  check_bool "the gap is closed once the token runs out" false
    (Ui.Observable.Value.get
       (Ui.Event_cache.has_gap (Ui.Runtime.event_cache runtime) room_id)
    && Option.is_some
         (Ui.Observable.Value.get
            (Ui.Event_cache.prev_batch (Ui.Runtime.event_cache runtime) room_id))
    );

  let seen =
    List.filter
      (fun body -> String.length body > 8 && String.sub body 0 8 = "history ")
      (message_bodies timeline)
  in
  check_int "every message is there exactly once" history_size
    (List.length seen);
  check_strings "in the order alice wrote them" expected seen

(* A process that comes back should find its history in SQLite before it
   has synced anything. The sync it then runs is limited, because the server
   answers a cold [/sync] with a window, and must merge into that history
   rather than duplicate or discard it. *)

let persisted_size = 15

let test_persistence () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let body index = Printf.sprintf "kept %s %02d" tag index in
  let expected = List.init persisted_size body in
  let path = Filename.temp_file "matrix-ui-integration-" ".sqlite3" in
  let open_store () =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "opening %s: %s" path
          (Ui.Event_store.Error.to_string error)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
    (fun () ->
      let store = open_store () in
      ( phase @@ fun sw ->
        let runtime = start_runtime h ~sw ~event_store:store bob in
        let timeline = Ui.Runtime.timeline runtime room_id in
        List.iter
          (fun body ->
            ignore
              (Harness.wait_sent h ~timeout:120.
                 (Queue.send_text queue ~room_id ~body)))
          expected;
        Harness.wait_until h ~label:"bob to see everything alice wrote"
          (fun () ->
            List.for_all (fun body -> find_body timeline body <> None) expected);
        Ui.Event_cache.flush_room (Ui.Runtime.event_cache runtime) room_id );
      Ui.Event_store.close store;

      (* A cache over the same file, with nothing driving it. *)
      let store = open_store () in
      let cold = Ui.Event_cache.create ~store () in
      let reloaded = Array.to_list (Ui.Event_cache.snapshot cold room_id) in
      let bodies events =
        List.filter_map
          (fun (event : Ui.Event_cache.event) ->
            let presented = Ui.Presentation.of_event event.event in
            match presented.content with
            | Ui.Presentation.Message _ -> Ui.Presentation.preview presented
            | _ -> None)
          events
      in
      check_strings "the cache reloads its history from SQLite" expected
        (bodies reloaded);

      (* And a runtime started over that store keeps it: the cold sync's
         window overlaps what is held, so there is no hole to protect. *)
      let after = "after " ^ tag in
      phase @@ fun sw ->
      let runtime = start_runtime h ~sw ~event_store:store bob in
      let timeline = Ui.Runtime.timeline runtime room_id in
      check_strings "the timeline starts from the reloaded history" expected
        (message_bodies timeline);
      (* The first response of a cold sync is limited. It must merge into
         the reloaded history rather than replace it. *)
      Harness.wait_until h ~label:"the cold sync's first response" (fun () ->
          match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
          | Ui.Runtime.Live _ -> true
          | _ -> false);
      check_strings "which the first sync response does not discard" expected
        (message_bodies timeline);
      ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:after));
      Harness.wait_until h
        ~label:"the new message to reach the reloaded timeline" (fun () ->
          find_body timeline after <> None);
      check_strings "which it appends to, without duplicating it"
        (expected @ [ after ]) (message_bodies timeline))

(* The queue is stopped for the room first, so that the echo is observed in
   its queued state rather than raced against a loopback round trip. *)

let test_local_echo () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let queue = Ui.Runtime.send_queue runtime in
  let body = "echo " ^ Harness.hex h 4 in

  let initial, diffs = collect_diffs ~sw (Ui.Room_timeline.items timeline) in
  Queue.set_room_enabled queue room_id ~enabled:false;
  let request = Ui.Room_timeline.send_text timeline ~body in
  let id = "txn:" ^ Queue.txn_id request in
  let find () =
    List.find_opt
      (fun (item : Ui.Room_timeline.event_item) -> String.equal item.id id)
      (event_items timeline)
  in
  Harness.wait_until h ~label:"the echo to appear as queued" (fun () ->
      match find () with
      | Some item ->
          item.delivery = Ui.Event_cache.Queued
          || item.delivery = Ui.Event_cache.Sending
      | None -> false);
  let echo = Option.get (find ()) in
  check_string "the echo carries the body that was sent" body (body_of echo);
  check_bool "and no event id yet" true (echo.event.event_id = None);
  let before = List.length (event_items timeline) in

  Queue.set_room_enabled queue room_id ~enabled:true;
  let event_id = Harness.wait_sent h request in
  Harness.wait_until h ~label:"the echo to be replaced by the server's copy"
    (fun () ->
      match find () with
      | Some item -> item.delivery = Ui.Event_cache.Synced
      | None -> false);
  let synced = Option.get (find ()) in
  check_string "the same item id survives the round trip" id synced.id;
  check_string "and now carries the server's event id" (eid event_id)
    (eid (Option.get synced.event.event_id));
  check_int "the item count did not double" before
    (List.length (event_items timeline));
  check_int "and the body appears exactly once" 1
    (List.length
       (List.filter
          (fun item -> String.equal (body_of item) body)
          (message_items timeline)));

  Harness.wait_until h ~label:"the diff stream to reproduce the snapshot"
    (fun () ->
      Ui.Observable.List.apply_all initial !diffs
      = Ui.Room_timeline.snapshot timeline)

(* The first sync of a two-member room puts the room's whole construction in
   the timeline window; Synapse's is the eight state events below. What a
   reader should get out of that is who came and went and what they called
   the room, not [m.room.create] and [m.room.power_levels] as messages. *)

let wire_type (item : Ui.Room_timeline.event_item) =
  Matrix_proto.Event.Event_type.to_string item.event.raw.type_

let cached_types cache room_id =
  Array.to_list (Ui.Event_cache.snapshot cache room_id)
  |> List.map (fun (event : Ui.Event_cache.event) ->
      Matrix_proto.Event.Event_type.to_string event.event.type_)

(* The suffix of [items] starting at the first one equal to [marker]. Bob's
   own join is the marker: everything before it depends on how much of the
   room's construction the server chose to put in his window, everything
   after it is what this scenario caused. *)
let rec suffix_from marker = function
  | [] -> []
  | first :: rest when String.equal first marker -> first :: rest
  | _ :: rest -> suffix_from marker rest

let test_state_events () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let carol = Harness.register_user h ~prefix:"carol" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let name = "Kitchen-" ^ tag and topic = "About " ^ tag in
  let last = "last word " ^ tag in

  (* Bob is watching while alice renames the room, gives it a topic, and
     invites carol, who joins and then leaves of her own accord. *)
  ignore
    (Harness.ok "set the room name"
       (State.set_name (Harness.base alice) ~room_id ~name));
  ignore
    (Harness.ok "set the room topic"
       (State.set_topic (Harness.base alice) ~room_id ~topic));
  ignore
    (Harness.ok "invite carol"
       (Rooms.invite (Harness.base alice) ~room_id ~user_id:carol.user_id ()));
  join carol room_id;
  ignore
    (Harness.ok "carol leaves" (Rooms.leave (Harness.base carol) ~room_id ()));
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:last));

  let expected =
    [
      uid bob.user_id ^ " accepted the invitation";
      uid alice.user_id ^ " changed the room name to " ^ name;
      uid alice.user_id ^ " changed the topic to " ^ topic;
      uid carol.user_id ^ " was invited";
      uid carol.user_id ^ " accepted the invitation";
      uid carol.user_id ^ " left";
      last;
    ]
  in
  let shown () = List.map body_of (event_items timeline) in
  Harness.wait_until h ~label:"bob's timeline to show every change" (fun () ->
      suffix_from (List.hd expected) (shown ()) = expected);
  check_strings "each state event reads as a sentence about who did what"
    expected
    (suffix_from (List.hd expected) (shown ()));

  (* The room's construction is in the cache and out of the timeline. *)
  let cached = cached_types (Ui.Runtime.event_cache runtime) room_id in
  let hidden =
    [
      "m.room.create";
      "m.room.power_levels";
      "m.room.join_rules";
      "m.room.history_visibility";
      "m.room.guest_access";
    ]
  in
  List.iter
    (fun type_ ->
      check_bool ("the cache holds " ^ type_) true (List.mem type_ cached))
    hidden;
  check_strings "and none of them is an item" []
    (List.filter
       (fun type_ -> List.mem type_ hidden)
       (List.map wire_type (event_items timeline)));

  (* The room list preview is the message, not a type string. *)
  let find_room () =
    List.find_opt
      (fun (r : Ui.Room_list.room) -> String.equal (rid r.id) (rid room_id))
      (Array.to_list
         (Ui.Observable.List.snapshot
            (Ui.Room_list.rooms (Ui.Runtime.room_list runtime))))
  in
  Harness.wait_until h ~label:"the room list to carry the room" (fun () ->
      find_room () <> None);
  let listed = Option.get (find_room ()) in
  check_string "the room list shows the name alice set" name listed.name;
  check_string "and its preview is the last message" last
    (Option.value listed.latest ~default:"");

  (* Now a state event is the newest thing in the room. A preview picked
     with [Presentation.is_preview_worthy] still names the message; one
     picked by "anything that is not a reaction or a redaction" names the
     topic change instead.

     [Room_list.latest_from_cache] does not call the predicate yet. When it
     does, the right-hand side of the check below becomes
     [(Option.get (find_room ())).latest] and this asserts the room list
     itself. *)
  let topic = topic ^ " (revised)" in
  ignore
    (Harness.ok "revise the topic"
       (State.set_topic (Harness.base alice) ~room_id ~topic));
  let revised = uid alice.user_id ^ " changed the topic to " ^ topic in
  Harness.wait_until h ~label:"bob to see the revised topic" (fun () ->
      List.exists (String.equal revised) (shown ()));
  let newest =
    let events =
      Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room_id
    in
    Ui.Presentation.of_event events.(Array.length events - 1).event
  in
  check_bool "the newest event in the room is now a state event" false
    (Ui.Presentation.is_preview_worthy newest);
  Harness.wait_until h ~label:"the room list to preview the last message"
    (fun () ->
      match find_room () with
      | Some (r : Ui.Room_list.room) -> r.latest = Some last
      | None -> false)

(* Synapse hands back an [end] token on the page that drains the history and
   only omits it on the next, empty page, so reaching [Timeline_start] takes
   one round trip more than there are events to fetch. *)

let has_start timeline =
  List.exists
    (function
      | Ui.Room_timeline.Virtual
          { content = Ui.Room_timeline.Timeline_start; _ } ->
          true
      | _ -> false)
    (items timeline)

let start_size = 5

let test_timeline_start () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let expected = List.init start_size (Printf.sprintf "start %s %d" tag) in
  List.iter
    (fun body ->
      ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body)))
    expected;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in

  Harness.wait_until h ~label:"the initial sync to leave a gap" (fun () ->
      has_gap timeline);
  check_bool "and no start marker while there is history to fetch" false
    (has_start timeline);
  check_bool "and the room's creation is behind the gap, not in the window"
    false
    (List.mem "m.room.create"
       (cached_types (Ui.Runtime.event_cache runtime) room_id));

  let more () =
    Option.is_some
      (Ui.Observable.Value.get
         (Ui.Event_cache.prev_batch (Ui.Runtime.event_cache runtime) room_id))
  in
  let rec drain rounds last =
    if rounds > 0 && more () then
      drain (rounds - 1) (Ui.Room_timeline.paginate_back timeline ~limit:100 ())
    else last
  in
  let outcome = drain 5 (Ok `Nothing_to_do) in
  check_bool "the last page reports the room's beginning" true
    (outcome = Ok `Reached_start);
  check_bool "pagination reported no error" true
    (Ui.Observable.Value.get (Ui.Room_timeline.pagination_error timeline) = None);
  check_bool "the token is spent" false (more ());
  (* No [wait_until]: [paginate_back] re-projects before it returns, so the
     page it fetched is in [items] on the line after the call rather than a
     scheduler turn later. *)
  check_bool "the start marker has already replaced the gap" true
    (has_start timeline);
  check_bool "and the gap is gone" false (has_gap timeline);
  check_strings "with alice's whole history behind it" expected
    (message_bodies timeline);
  check_bool "the room's creation is now in the cache" true
    (List.mem "m.room.create"
       (cached_types (Ui.Runtime.event_cache runtime) room_id));
  check_bool "and a further page has nothing to fetch" true
    (Ui.Room_timeline.paginate_back timeline ~limit:100 () = Ok `Nothing_to_do)

(* Alice answers bob's message with a formatted notice, reacts to it,
   unreacts by redacting the reaction, and bob redacts his own message. The
   [own] flag is the sender's own view of the reaction, so it must be true
   for alice and false for bob. *)

let reactions_of timeline body =
  match find_body timeline body with
  | None -> None
  | Some item -> Some item.reactions

let test_own_messages () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let alice_runtime = start_runtime h ~sw alice in
  let bob_runtime = start_runtime h ~sw bob in
  let alice_timeline = Ui.Runtime.timeline alice_runtime room_id in
  let bob_timeline = Ui.Runtime.timeline bob_runtime room_id in
  let tag = Harness.hex h 4 in
  let body = "reactable " ^ tag in

  let message_id =
    Harness.wait_sent h (Ui.Room_timeline.send_text bob_timeline ~body)
  in
  Harness.wait_until h ~label:"both timelines to hold the message" (fun () ->
      find_body alice_timeline body <> None
      && find_body bob_timeline body <> None);

  (* Alice answers with the three things a plain [send_text] cannot say: an
     [m.notice], HTML beside the plain body, and the relation naming what it
     answers. Bob reads all three back off his own timeline. *)
  let answer = "answer " ^ tag in
  ignore
    (Harness.wait_sent h
       (Ui.Room_timeline.send_message alice_timeline ~msgtype:`Notice
          ~formatted:
            (Printf.sprintf {|<b>%s</b><script>alert("no")</script>|} answer)
          ~reply_to:message_id ~body:answer ()));
  Harness.wait_until h ~label:"the reply to reach bob's timeline" (fun () ->
      find_body bob_timeline answer <> None);
  let reply = Option.get (find_body bob_timeline answer) in
  check_bool "bob sees it as a reply to the message it answers" true
    (Option.map eid reply.reply_to = Some (eid message_id));
  (match reply.event.content with
  | Ui.Presentation.Message message ->
      check_bool "sent as an m.notice" true
        (message.kind = Ui.Presentation.Notice);
      check_string "with the plain body beside it" answer message.body;
      let html =
        match message.formatted with
        | Some formatted -> formatted.html
        | None -> Alcotest.fail "expected a formatted body"
      in
      check_bool "the markup survives the round trip" true
        (contains ("<b>" ^ answer ^ "</b>") html);
      check_bool "and the script was cut out before it was sent" false
        (contains "script" html || contains "alert" html)
  | _ -> Alcotest.fail "expected a message");

  (* The static-location convenience uses the same queue and local-echo path as
     text. The other client sees the legacy fallback after the real server has
     accepted and synced it. *)
  let location_body = "location " ^ tag in
  let geo_uri = "geo:51.5007,-0.1246" in
  ignore
    (Harness.wait_sent h
       (Ui.Room_timeline.send_location alice_timeline
          ~description:"Elizabeth Tower" ~zoom_level:17
          ~asset:Ui.Room_timeline.Pin ~geo_uri ~body:location_body ()));
  Harness.wait_until h ~label:"the location to reach bob's timeline" (fun () ->
      find_body bob_timeline location_body <> None);
  let location = Option.get (find_body bob_timeline location_body) in
  (match location.event.content with
  | Ui.Presentation.Message message ->
      check_bool "the location keeps its message kind" true
        (message.kind = Ui.Presentation.Location);
      check_string "and its fallback body" location_body message.body
  | _ -> Alcotest.fail "expected a location message");
  check_string "the legacy geo URI survives the server round trip" geo_uri
    (Option.value ~default:""
       (Harness.string_member "geo_uri" location.event.raw.content));

  let reaction_id =
    Harness.wait_sent h
      (Ui.Room_timeline.send_reaction alice_timeline ~relates_to:message_id
         ~key:"\u{1F44D}")
  in
  let one_reaction timeline =
    match reactions_of timeline body with
    | Some [ reaction ] -> Some reaction
    | _ -> None
  in
  Harness.wait_until h ~label:"the reaction to reach both timelines" (fun () ->
      one_reaction alice_timeline <> None && one_reaction bob_timeline <> None);
  let mine = Option.get (one_reaction alice_timeline) in
  check_string "the key is what alice sent" "\u{1F44D}" mine.key;
  check_int "counted once" 1 mine.count;
  check_bool "and it is alice's own" true mine.own;
  check_bool "while bob sees it as someone else's" false
    (Option.get (one_reaction bob_timeline)).own;

  (* Redacting the reaction event takes it off the message it annotated. *)
  ignore
    (Harness.wait_sent h
       (Ui.Room_timeline.redact alice_timeline ~event_id:reaction_id ()));
  Harness.wait_until h ~label:"the reaction to disappear from both timelines"
    (fun () ->
      reactions_of alice_timeline body = Some []
      && reactions_of bob_timeline body = Some []);
  check_bool "no reaction item is left over on alice's side" true
    (unaggregated alice_timeline = []);

  (* And redacting his own message flags it, for him and for her. *)
  ignore
    (Harness.wait_sent h
       (Ui.Room_timeline.redact bob_timeline ~event_id:message_id ()));
  Harness.wait_until h ~label:"the message to be flagged redacted for both"
    (fun () ->
      match (find_body bob_timeline body, find_body alice_timeline body) with
      | Some his, Some hers -> his.redacted && hers.redacted
      | _ -> false);
  check_bool "the item stays, flagged rather than removed" true
    (find_body bob_timeline body <> None)

(* Handing the runtime an encryption machine is all a client has to do: the
   sync loop decrypts, the send queue encrypts, and the timeline shows the
   plaintext while the cache still holds the [m.room.encrypted] event that
   arrived. *)

let cached_for cache room_id ~event_id =
  Array.to_list (Ui.Event_cache.snapshot cache room_id)
  |> List.find_opt (fun (event : Ui.Event_cache.event) ->
      match event.event.event_id with
      | Some id -> String.equal (eid id) (eid event_id)
      | None -> false)

let test_encrypted_runtime () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let machine (user : Harness.user) =
    Matrix_eio.Encryption.of_env (Harness.env h) ~user_id:user.user_id
      ~device_id:user.device_id ()
  in
  let alice_enc = machine alice and bob_enc = machine bob in
  let path = Filename.temp_file "matrix-ui-encrypted-" ".sqlite3" in
  let open_store () =
    match Matrix_ui_sqlite.create ~plaintext_policy:Store_plaintext path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "opening %s: %s" path
          (Ui.Event_store.Error.to_string error)
  in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
  @@ fun () ->
  let store = open_store () in
  let body = "ciphered " ^ Harness.hex h 4 in
  let room_id = ref None in
  ( phase @@ fun sw ->
    let alice_runtime = start_runtime h ~sw ~encryption:alice_enc alice in
    let bob_runtime =
      start_runtime h ~sw ~encryption:bob_enc ~event_store:store bob
    in
    (* The first response is what uploads each machine's keys. *)
    List.iter
      (fun runtime ->
        Harness.wait_until h ~label:"the first sync response" (fun () ->
            match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
            | Ui.Runtime.Live _ -> true
            | _ -> false))
      [ alice_runtime; bob_runtime ];
    let room =
      Harness.ok "create an encrypted room"
        (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat
           ~invite:[ bob.user_id ] ~encrypted:true ())
    in
    room_id := Some room;
    join bob room;
    List.iter
      (fun (runtime, enc) ->
        Harness.wait_until h
          ~label:"the room to be known encrypted, with both members" (fun () ->
            Matrix_eio.Encryption.is_room_encrypted enc room
            && List.length
                 (Matrix_eio.Sync_service.members
                    (Ui.Runtime.sync_service runtime)
                    room)
               >= 2))
      [ (alice_runtime, alice_enc); (bob_runtime, bob_enc) ];

    let alice_timeline = Ui.Runtime.timeline alice_runtime room in
    let bob_timeline = Ui.Runtime.timeline bob_runtime room in
    let event_id =
      Harness.wait_sent h ~timeout:120.
        (Ui.Room_timeline.send_text alice_timeline ~body)
    in
    Harness.wait_until h ~timeout:120.
      ~label:"bob's timeline to show the plaintext" (fun () ->
        find_body bob_timeline body <> None);
    let item = Option.get (find_body bob_timeline body) in
    check_string "the item bob sees is a message" "m.room.message"
      (wire_type item);
    let cached =
      Option.get
        (cached_for (Ui.Runtime.event_cache bob_runtime) room ~event_id)
    in
    check_string "while the event that arrived is encrypted" "m.room.encrypted"
      (Matrix_proto.Event.Event_type.to_string cached.event.type_);
    check_bool "with the plaintext held beside it" true
      (Option.is_some cached.clear_event);
    Ui.Event_cache.flush_room (Ui.Runtime.event_cache bob_runtime) room );
  Ui.Event_store.close store;

  (* A cold cache over the same file, with nothing driving it and no
     encryption machine: the plaintext is what was written. *)
  let room = Option.get !room_id in
  let store = open_store () in
  let cold = Ui.Event_cache.create ~store () in
  let reloaded = Ui.Event_cache.snapshot cold room in
  Ui.Event_store.close store;
  let bodies =
    Array.to_list reloaded
    |> List.filter_map (fun (event : Ui.Event_cache.event) ->
        let presented =
          Ui.Presentation.of_event (Ui.Event_cache.effective event)
        in
        match presented.content with
        | Ui.Presentation.Message _ -> Ui.Presentation.preview presented
        | _ -> None)
  in
  check_bool "the reloaded cache still shows the body" true
    (List.mem body bodies);
  check_bool "and the encrypted event is still what it holds" true
    (Array.exists
       (fun (event : Ui.Event_cache.event) ->
         Matrix_proto.Event.Event_type.to_string event.event.type_
         = "m.room.encrypted")
       reloaded)

(* The token below is syntactically fine and unknown to the server, so the
   first [/sync] is a 401 that no amount of retrying will fix: the state goes
   [Failed] and then, because [on_error] says [Stop], [Stopped]. *)

let test_sync_failure () =
  Harness.run @@ fun h ->
  let bob = Harness.register_user h ~prefix:"bob" () in
  let session = Option.get (Matrix_eio.Client.session bob.client) in
  let wedged =
    Matrix_eio.Client.with_session bob.client
      {
        session with
        Matrix_client.Client.access_token = "syt_wedged_" ^ Harness.hex h 8;
      }
  in
  phase @@ fun sw ->
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:bob.user_id
      ~display_name:bob.localpart ()
  in
  let seen = ref [] in
  let failed_before_hook = ref false in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:wedged ~sync ()
  in
  decr sync_timeout;
  (* The first few failures are retried through [Offline]; then the hook gives
     up, which is what [Stopped] means. The hook observes [Failed] directly,
     before the retry decision can publish the next state. *)
  Ui.Runtime.start
    ~params:{ Matrix_client.Sync.default_params with timeout = !sync_timeout }
    ~on_error:(fun error ->
      (failed_before_hook :=
         !failed_before_hook
         ||
         match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
         | Ui.Runtime.Failed message -> contains "M_UNKNOWN_TOKEN" message
         | _ -> false);
      seen := Format.asprintf "%a" Matrix_eio.Error.pp_err error :: !seen;
      if List.length !seen < 3 then Matrix_eio.Sync_service.Retry_after 0.5
      else Matrix_eio.Sync_service.Stop)
    runtime;
  Harness.wait_until h ~timeout:60.
    ~label:"and then to give up rather than retry for ever" (fun () ->
      Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
      = Ui.Runtime.Stopped);
  check_bool "the failure is published before the error hook" true
    !failed_before_hook;
  check_bool "the error reached the caller's hook too" true (!seen <> [])

(* Nothing [Runtime.start] forked may outlive [Runtime.stop]: neither the
   sync loop, which would keep delivering alice's messages, nor the send
   queue, which would keep sending bob's. *)

let test_stop () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let before = "before " ^ tag in
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:before));
  Harness.wait_until h ~label:"bob's runtime to be live" (fun () ->
      find_body timeline before <> None);

  Ui.Runtime.stop runtime;
  check_bool "the state is Stopped as soon as stop returns" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Stopped);

  (* Bob's own send, enqueued after the stop, has nobody left to send it. *)
  let own = Ui.Room_timeline.send_text timeline ~body:("mine " ^ tag) in
  let after = "after " ^ tag in
  ignore (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:after));
  (* Long enough for a sync response to have arrived had the loop lived: the
     runtime's own timeout is tens of seconds, but a delivered event shows
     up in a fraction of one. *)
  Eio.Time.sleep (Harness.clock h) 5.;
  check_bool "the sync loop is gone: alice's next message never arrives" true
    (find_body timeline after = None);
  check_bool "and the send queue with it: bob's own is still pending" true
    (match Matrix_client.Send_queue.status own with
    | Matrix_client.Send_queue.Sent _ -> false
    | _ -> true);
  check_bool "the state stayed Stopped" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Stopped);
  check_bool "stopping twice is harmless" true
    (Ui.Runtime.stop runtime;
     Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
     = Ui.Runtime.Stopped);
  (* [stop] closes and forgets the timelines as well; an explicit close is
     consequently harmless, and the next lookup builds another. *)
  Ui.Runtime.close_timeline runtime room_id;
  check_bool "closing a timeline hands out a fresh one next time" false
    (Ui.Runtime.timeline runtime room_id == timeline)

(* Bob holds a message, then his runtime stops while alice writes more than
   a sync window holds. The window his next runtime is handed shares nothing
   with what the store kept, so the cache must keep that history behind a
   [Gap] rather than drop it, and [paginate_gap] must fill the hole exactly. *)

let hole_size = 20

let test_disjoint_gap () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let first = "first " ^ tag in
  let body index = Printf.sprintf "hole %s %02d" tag index in
  let missed = List.init hole_size body in
  let path = Filename.temp_file "matrix-ui-gap-" ".sqlite3" in
  let open_store () =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "opening %s: %s" path
          (Ui.Event_store.Error.to_string error)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
    (fun () ->
      let store = open_store () in
      ( phase @@ fun sw ->
        let runtime = start_runtime h ~sw ~event_store:store bob in
        let timeline = Ui.Runtime.timeline runtime room_id in
        ignore
          (Harness.wait_sent h (Queue.send_text queue ~room_id ~body:first));
        Harness.wait_until h ~label:"bob to see the first message" (fun () ->
            find_body timeline first <> None);
        Ui.Event_cache.flush_room (Ui.Runtime.event_cache runtime) room_id );
      Ui.Event_store.close store;

      (* Bob is away for all of these. *)
      List.iter
        (fun body ->
          ignore
            (Harness.wait_sent h ~timeout:180.
               (Queue.send_text queue ~room_id ~body)))
        missed;

      let store = open_store () in
      phase @@ fun sw ->
      let runtime = start_runtime h ~sw ~event_store:store bob in
      let timeline = Ui.Runtime.timeline runtime room_id in
      check_strings "the reloaded timeline starts from the store" [ first ]
        (message_bodies timeline);
      Harness.wait_until h
        ~label:"the cold sync's window, which cannot reach that far back"
        (fun () ->
          match message_bodies timeline with
          | seen -> List.length seen > 1 && List.mem first seen);
      check_bool "the history the store kept is still there" true
        (List.mem first (message_bodies timeline));
      check_bool "behind a hole" true
        (Ui.Observable.Value.get
           (Ui.Event_cache.has_gap (Ui.Runtime.event_cache runtime) room_id));
      let mid_gap () =
        List.find_opt
          (fun (gap : Ui.Event_cache.gap) -> gap.index > 0)
          (Ui.Observable.Value.get
             (Ui.Event_cache.gaps (Ui.Runtime.event_cache runtime) room_id))
      in
      (match mid_gap () with
      | None -> Alcotest.fail "expected a gap after the reloaded history"
      | Some gap ->
          check_bool "which the timeline shows in place" true
            (List.exists
               (function
                 | Ui.Room_timeline.Virtual
                     { id; content = Ui.Room_timeline.Gap _ } ->
                     id
                     = "virtual:gap:" ^ Ui.Event_cache.Gap_id.to_string gap.id
                 | _ -> false)
               (items timeline)));

      (* Fill it. Each page walks backwards from the gap's token until it
         meets the message the store kept, which closes the hole. *)
      let rec fill rounds =
        if rounds > 0 then
          match mid_gap () with
          | None -> ()
          | Some gap ->
              let before =
                Array.length
                  (Ui.Event_cache.snapshot
                     (Ui.Runtime.event_cache runtime)
                     room_id)
              in
              let outcome =
                Ui.Room_timeline.paginate_gap timeline ~limit:5 ~gap:gap.id ()
              in
              if
                Array.length
                  (Ui.Event_cache.snapshot
                     (Ui.Runtime.event_cache runtime)
                     room_id)
                > before
                && outcome = Ok `More
              then fill (rounds - 1)
      in
      fill 30;
      check_bool "pagination reported no error" true
        (Ui.Observable.Value.get (Ui.Room_timeline.pagination_error timeline)
        = None);
      check_bool "the hole is closed" true (mid_gap () = None);
      let seen =
        List.filter
          (fun body ->
            String.equal body first
            || (String.length body > 5 && String.sub body 0 5 = "hole "))
          (message_bodies timeline)
      in
      check_strings "and everything is there once, in alice's order"
        (first :: missed) seen)

(* Under the default [Ciphertext_only] policy SQLite holds the wire event, so
   a reloaded encrypted room would read as [Unable_to_decrypt] until sync
   delivered something new. The Megolm keys outlive the process, so the
   runtime decrypts what it reloaded without a single request. *)

let test_encrypted_reload () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let machine (user : Harness.user) =
    Matrix_eio.Encryption.of_env (Harness.env h) ~user_id:user.user_id
      ~device_id:user.device_id ()
  in
  let alice_enc = machine alice and bob_enc = machine bob in
  let path = Filename.temp_file "matrix-ui-reload-" ".sqlite3" in
  let open_store () =
    match Matrix_ui_sqlite.create path with
    | Ok store -> store
    | Error error ->
        Alcotest.failf "opening %s: %s" path
          (Ui.Event_store.Error.to_string error)
  in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
  @@ fun () ->
  let body = "restored " ^ Harness.hex h 4 in
  let room_id = ref None in
  let store = open_store () in
  ( phase @@ fun sw ->
    let alice_runtime = start_runtime h ~sw ~encryption:alice_enc alice in
    let bob_runtime =
      start_runtime h ~sw ~encryption:bob_enc ~event_store:store bob
    in
    List.iter
      (fun runtime ->
        Harness.wait_until h ~label:"the first sync response" (fun () ->
            match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
            | Ui.Runtime.Live _ -> true
            | _ -> false))
      [ alice_runtime; bob_runtime ];
    let room =
      Harness.ok "create an encrypted room"
        (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat
           ~invite:[ bob.user_id ] ~encrypted:true ())
    in
    room_id := Some room;
    join bob room;
    List.iter
      (fun (runtime, enc) ->
        Harness.wait_until h
          ~label:"the room to be known encrypted, with both members" (fun () ->
            Matrix_eio.Encryption.is_room_encrypted enc room
            && List.length
                 (Matrix_eio.Sync_service.members
                    (Ui.Runtime.sync_service runtime)
                    room)
               >= 2))
      [ (alice_runtime, alice_enc); (bob_runtime, bob_enc) ];
    let alice_timeline = Ui.Runtime.timeline alice_runtime room in
    let bob_timeline = Ui.Runtime.timeline bob_runtime room in
    ignore
      (Harness.wait_sent h ~timeout:120.
         (Ui.Room_timeline.send_text alice_timeline ~body));
    Harness.wait_until h ~timeout:120.
      ~label:"bob's timeline to show the plaintext" (fun () ->
        find_body bob_timeline body <> None);
    Ui.Event_cache.flush_room (Ui.Runtime.event_cache bob_runtime) room );
  Ui.Event_store.close store;
  let room = Option.get !room_id in

  (* What the store kept is ciphertext and nothing else. *)
  let store = open_store () in
  let cold = Ui.Event_cache.create ~store () in
  let reloaded = Ui.Event_cache.snapshot cold room in
  Ui.Event_store.close store;
  check_bool "the store kept the encrypted event" true
    (Array.exists
       (fun (event : Ui.Event_cache.event) ->
         Matrix_proto.Event.Event_type.to_string event.event.type_
         = "m.room.encrypted")
       reloaded);
  check_bool "and no plaintext beside it" true
    (Array.for_all
       (fun (event : Ui.Event_cache.event) -> Option.is_none event.clear_event)
       reloaded);

  (* A runtime with the same machine decrypts what it reloaded when the
     timeline is opened. It is deliberately never started: with no sync loop
     and no request of any kind, the plaintext below can only have come from
     the Megolm keys the machine still holds. *)
  let store = open_store () in
  phase @@ fun sw ->
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:bob.user_id
      ~display_name:bob.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:bob.client ~sync
      ~encryption:bob_enc ~event_store:store ()
  in
  let timeline = Ui.Runtime.timeline runtime room in
  check_bool "the runtime never synced" true
    (Ui.Observable.Value.get (Ui.Runtime.sync_state runtime)
    = Ui.Runtime.Not_started);
  check_bool "and the reloaded timeline still shows the plaintext" true
    (find_body timeline body <> None);
  let item = Option.get (find_body timeline body) in
  check_string "as a message" "m.room.message" (wire_type item);
  check_bool "over an event that is still encrypted on the wire" true
    (Array.exists
       (fun (event : Ui.Event_cache.event) ->
         Matrix_proto.Event.Event_type.to_string event.event.type_
         = "m.room.encrypted"
         && Option.is_some event.clear_event)
       (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room))

(* The marker goes after the last event the own user has read, past the run
   of their own that follows it, and is absent while nothing is read or the
   position is the timeline's end. *)

let test_read_marker () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let one = "one " ^ tag and two = "two " ^ tag and three = "three " ^ tag in
  let ids =
    List.map
      (fun body ->
        (body, Harness.wait_sent h (Queue.send_text queue ~room_id ~body)))
      [ one; two; three ]
  in
  Harness.wait_until h ~label:"bob to see all three" (fun () ->
      List.for_all (fun (body, _) -> find_body timeline body <> None) ids);
  let marker_after () =
    let rec walk previous = function
      | [] -> None
      | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Read_marker; _ }
        :: _ ->
          previous
      | Ui.Room_timeline.Event event :: rest -> walk (Some (body_of event)) rest
      | _ :: rest -> walk previous rest
    in
    walk None (items timeline)
  in
  check_bool "nothing read, no marker" true (marker_after () = None);
  ignore
    (Harness.ok "send a read marker"
       (Matrix_client.Receipts.set_read_marker (Harness.base bob) ~room_id
          ~fully_read:(List.assoc one ids) ()));
  Harness.wait_until h ~label:"the marker to reach bob's timeline" (fun () ->
      marker_after () = Some one);
  check_string "the marker sits after the message it marks" one
    (Option.get (marker_after ()));
  ignore
    (Harness.ok "move the read marker"
       (Matrix_client.Receipts.set_read_marker (Harness.base bob) ~room_id
          ~fully_read:(List.assoc two ids) ()));
  Harness.wait_until h ~label:"the marker to move" (fun () ->
      marker_after () = Some two);
  ignore
    (Harness.ok "read everything"
       (Matrix_client.Receipts.set_read_marker (Harness.base bob) ~room_id
          ~fully_read:(List.assoc three ids) ()));
  Harness.wait_until h ~label:"a marker at the end of the timeline to disappear"
    (fun () -> marker_after () = None)

(* A receipt can arrive in the cold sync while its event sits outside that
   sync's limited timeline. The Runtime-owned shared back-pagination queue
   should recover the target without a caller manually paginating, then ask
   the open timeline to resolve its read marker against the enlarged cache. *)

let test_receipt_target_backfill () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room alice in
  let queue = Harness.start_send_queue h alice in
  let tag = Harness.hex h 4 in
  let history =
    List.init 12 (fun index ->
        let body = Printf.sprintf "receipt history %s %02d" tag index in
        let event_id =
          Harness.wait_sent h ~timeout:180.
            (Queue.send_text queue ~room_id ~body)
        in
        (event_id, body))
  in
  ignore
    (Harness.ok "invite receipt reader"
       (Rooms.invite (Harness.base alice) ~room_id ~user_id:bob.user_id ()));
  join bob room_id;
  let target_id, target_body = List.nth history 4 in
  ignore
    (Harness.ok "send old public receipt"
       (Matrix_client.Receipts.send_receipt (Harness.base bob) ~room_id
          ~event_id:target_id ()));
  List.init 7 (fun index -> Printf.sprintf "receipt unread %s %02d" tag index)
  |> List.iter (fun body ->
      ignore
        (Harness.wait_sent h ~timeout:180.
           (Queue.send_text queue ~room_id ~body)));
  phase @@ fun sw ->
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:bob.user_id
      ~display_name:bob.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:bob.client ~sync ()
  in
  decr sync_timeout;
  Ui.Runtime.start
    ~params:
      {
        Matrix_client.Sync.default_params with
        filter = Some {|{"room":{"timeline":{"limit":5}}}|};
        timeout = !sync_timeout;
      }
    runtime;
  let timeline = Ui.Runtime.timeline runtime room_id in
  let cache = Ui.Runtime.event_cache runtime in
  let cached_target () =
    Array.exists
      (fun (event : Ui.Event_cache.event) ->
        Option.equal Id.Event_id.equal event.event.event_id (Some target_id))
      (Ui.Event_cache.snapshot cache room_id)
  in
  Harness.wait_until h ~label:"old receipt target to be backfilled"
    cached_target;
  check_bool "automatic backfill exposes the old message" true
    (find_body timeline target_body <> None);
  let marker_after () =
    let rec walk previous = function
      | [] -> None
      | Ui.Room_timeline.Virtual { content = Ui.Room_timeline.Read_marker; _ }
        :: _ ->
          previous
      | Ui.Room_timeline.Event event :: rest -> walk event.event.event_id rest
      | _ :: rest -> walk previous rest
    in
    walk None (items timeline)
  in
  Harness.wait_until h ~label:"backfilled receipt marker to resolve" (fun () ->
      Option.equal Id.Event_id.equal (marker_after ()) (Some target_id));
  check_bool "read marker follows the backfilled target" true
    (Option.equal Id.Event_id.equal (marker_after ()) (Some target_id));
  Harness.wait_until h ~label:"room unread count to reconcile" (fun () ->
      match Ui.Room_list.find (Ui.Runtime.room_list runtime) room_id with
      | Some room -> room.unread_messages = 7
      | None -> false);
  check_int "room unread count follows the backfilled receipt" 7
    (match Ui.Room_list.find (Ui.Runtime.room_list runtime) room_id with
    | Some room -> room.unread_messages
    | None -> -1)

(* Typing is ephemeral rather than a timeline event. Alice's notification must
   reach Bob's runtime, while Bob's own notification is deliberately omitted
   from the users shown to him. *)

let test_typing_live () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] alice in
  join bob room_id;
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let typing_users = Ui.Runtime.typing_users runtime room_id in
  let users () = Ui.Observable.Value.get typing_users in
  let has user_id = List.exists (Id.User_id.equal user_id) (users ()) in
  let timeout = 10_000 in
  Harness.ok "bob starts typing"
    (Matrix_client.Typing.set_typing (Harness.base bob) ~room_id ~typing:true
       ~timeout ());
  Harness.ok "alice starts typing"
    (Matrix_client.Typing.set_typing (Harness.base alice) ~room_id ~typing:true
       ~timeout ());
  Harness.wait_until h ~label:"bob to observe alice typing" (fun () ->
      has alice.user_id && not (has bob.user_id));
  Harness.ok "alice stops typing"
    (Matrix_client.Typing.set_typing (Harness.base alice) ~room_id ~typing:false
       ());
  Harness.wait_until h ~label:"alice typing to disappear" (fun () ->
      users () = [])

(* A live counterpart to the hermetic [Runtime.forget] tests: the room has a
   real timeline and state, a held local echo, and a remote [m.direct] entry.
   Forget must clear every projection synchronously and must not let a later
   response from the live sync loop bring the room back. *)

let test_forget_live () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let room_id = create_room ~invite:[ bob.user_id ] ~is_direct:true alice in
  join bob room_id;
  (* The invite's [is_direct] flag is authoritative only while Bob is invited.
     Once joined, Rust and the Matrix spec derive directness from Bob's global
     [m.direct] account data.  Establish that state before starting Runtime. *)
  Harness.ok "record the remote direct-room association"
    (Matrix_client.Account_data.mark_as_dm (Harness.base bob)
       ~user_id:alice.user_id ~room_id);
  phase @@ fun sw ->
  let runtime = start_runtime h ~sw bob in
  let timeline = Ui.Runtime.timeline runtime room_id in
  let room_list = Ui.Runtime.room_list runtime in
  let cache = Ui.Runtime.event_cache runtime in
  let queue = Ui.Runtime.send_queue runtime in
  let sync = Ui.Runtime.sync_service runtime in
  Harness.wait_until h ~label:"bob's runtime to observe the joined room"
    (fun () ->
      match Ui.Room_list.find room_list room_id with
      | Some room ->
          room.membership = Matrix_client.Base_client.Joined
          && room.is_dm
          && room.section = Ui.Room_list.People
      | None -> false);

  (* The real [m.direct] association gives [forget] remote account data to
     remove and was written before the sync loop, so this setup does not depend
     on Synapse waking a running classic sync for the account-data write. *)
  let alice_queue = Harness.start_send_queue h alice in
  let body = "forget-live " ^ Harness.hex h 4 in
  ignore (Harness.wait_sent h (Queue.send_text alice_queue ~room_id ~body));
  Harness.wait_until h ~label:"the runtime to observe the real message"
    (fun () -> find_body timeline body <> None);
  let remote_direct () =
    Harness.ok "read bob's direct rooms"
      (Matrix_client.Account_data.find_dm_rooms (Harness.base bob)
         ~user_id:alice.user_id)
  in
  check_bool "the remote m.direct entry contains the room" true
    (List.exists (Id.Room_id.equal room_id) (remote_direct ()));

  (* Keep one local send from leaving while the room is being left. *)
  Queue.set_room_enabled queue room_id ~enabled:false;
  let pending_body = "held-after-forget " ^ Harness.hex h 4 in
  let pending = Queue.send_text queue ~room_id ~body:pending_body in
  Harness.wait_until h ~label:"the held local send to remain pending" (fun () ->
      Queue.status pending = Queue.Pending
      && find_body timeline pending_body <> None);
  ignore (Harness.ok "leave the room" (Ui.Runtime.leave runtime room_id));
  Harness.wait_until h ~label:"the room to become left and historical"
    (fun () ->
      match Ui.Room_list.find room_list room_id with
      | Some room ->
          room.membership = Matrix_client.Base_client.Left
          && room.section = Ui.Room_list.Historical
      | None -> false);

  Harness.ok "forget the room through the runtime"
    (Ui.Runtime.forget runtime room_id);

  (* Runtime.forget is deliberately synchronous after the server accepts the
     request: all of these are immediate assertions, not waits. *)
  check_bool "the base room is gone immediately" true
    (Matrix_client.Base_client.find_room
       (Matrix_eio.Sync_service.state sync)
       room_id
    = None);
  check_bool "the room-list lookup is gone immediately" true
    (Ui.Room_list.find room_list room_id = None);
  check_int "the all-rooms projection is empty" 0
    (List.length
       (Array.to_list
          (Ui.Observable.List.snapshot (Ui.Room_list.all_rooms room_list))));
  check_int "the filtered room-list projection is empty" 0
    (List.length
       (Array.to_list
          (Ui.Observable.List.snapshot (Ui.Room_list.rooms room_list))));
  check_int "the event cache is empty immediately" 0
    (Array.length (Ui.Event_cache.snapshot cache room_id));
  check_bool "the event-cache previous token is gone" true
    (Ui.Observable.Value.get (Ui.Event_cache.prev_batch cache room_id) = None);
  check_bool "the event-cache gap flag is cleared" false
    (Ui.Observable.Value.get (Ui.Event_cache.has_gap cache room_id));
  check_int "the open timeline is empty immediately" 0
    (List.length (items timeline));
  check_int "the room queue is empty immediately" 0
    (List.length (Queue.room_requests queue room_id));
  check_bool "the queue no longer names the forgotten room" false
    (List.exists (Id.Room_id.equal room_id) (Queue.rooms queue));
  check_bool "the remote m.direct entry was removed" false
    (List.exists (Id.Room_id.equal room_id) (remote_direct ()));

  (* Create and join another room only after [forget] has returned. Seeing it
     in the state proves the absence checks below run after a later response
     was fetched and committed, rather than accidentally observing the
     response that reported the preceding leave. *)
  let probe_room = create_room ~invite:[ bob.user_id ] alice in
  join bob probe_room;
  Harness.wait_until h ~label:"a subsequent live sync after forgetting"
    (fun () ->
      match
        Matrix_client.Base_client.find_room
          (Matrix_eio.Sync_service.state sync)
          probe_room
      with
      | Some room -> room.membership = Matrix_client.Base_client.Joined
      | None -> false);
  check_bool "the forgotten room does not reappear after live sync" true
    (Ui.Room_list.find room_list room_id = None
    && Matrix_client.Base_client.find_room
         (Matrix_eio.Sync_service.state sync)
         room_id
       = None
    && Array.length (Ui.Event_cache.snapshot cache room_id) = 0
    && items timeline = [])

let tests =
  [
    Alcotest.test_case "reactions, edits and redactions" `Quick test_aggregation;
    Alcotest.test_case "room list ordering, filtering and joining" `Quick
      test_room_list;
    Alcotest.test_case "back-pagination across the gap" `Quick
      test_back_pagination;
    Alcotest.test_case "reload from SQLite" `Quick test_persistence;
    Alcotest.test_case "local echo" `Quick test_local_echo;
    Alcotest.test_case "state events as items" `Quick test_state_events;
    Alcotest.test_case "paginating to the timeline start" `Quick
      test_timeline_start;
    Alcotest.test_case "own reactions, replies and redactions" `Quick
      test_own_messages;
    Alcotest.test_case "an encrypted room through the runtime" `Quick
      test_encrypted_runtime;
    Alcotest.test_case "a sync that cannot succeed" `Quick test_sync_failure;
    Alcotest.test_case "stopping a runtime" `Quick test_stop;
    Alcotest.test_case "a disjoint sync leaves a fillable gap" `Quick
      test_disjoint_gap;
    Alcotest.test_case "plaintext after a restart" `Quick test_encrypted_reload;
    Alcotest.test_case "the read marker" `Quick test_read_marker;
    Alcotest.test_case "automatic receipt-target backfill" `Quick
      test_receipt_target_backfill;
    Alcotest.test_case "live typing users" `Quick test_typing_live;
    Alcotest.test_case "live runtime forget" `Quick test_forget_live;
  ]
