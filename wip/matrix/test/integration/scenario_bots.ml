(** The example plugins against a live homeserver.

    [examples/bots] is a library of {!Matrix_bot.Bot.plugin}s so that these
    scenarios can compose exactly what [examples/bots/bin/matrix_bot.ml]
    composes. Each one builds a spec, runs it with {!Matrix_bot.Bot.run} on a
    switch of its own, and then plays the human: it invites the bot, says
    things, and asserts on what the bot says back, read off the human's own
    {!Matrix_ui.Room_timeline}, which is the decrypted view in the encrypted
    scenarios.

    Nothing is stubbed. The bot's sync loop, send queue, encryption machine and
    event store are the real ones, so a scenario that passes is evidence that a
    bot written this way works, not that its logic is internally consistent.

    [Eio.Switch.run] around each bot returns only once every fiber it forked
    has, so a scenario that comes back is also a bot that left nothing behind.
*)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Bot = Matrix_bot.Bot
module Context = Matrix_bot.Context
module Plugin_store = Matrix_bot.Plugin_store
module Rooms = Matrix_client.Rooms
module Room_state = Matrix_client.State

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let rid = Id.Room_id.to_string
let uid = Id.User_id.to_string

(* Synapse caches a [/sync] response for two minutes under a key that
   includes the long-poll timeout, so a bot that restarts (a second runtime
   for one user) would replay the first one's responses. Varying it keeps
   them apart. *)
let sync_timeout = ref 28_000

let params () =
  decr sync_timeout;
  { Matrix_client.Sync.default_params with timeout = !sync_timeout }

(* The keys are published before any runtime exists, so a scenario never
   races the first [/sync] to get this device onto the server: an encrypted
   message sent to a bot whose device keys are not up yet is encrypted to
   nobody. *)
let machine h (user : Harness.user) =
  let enc =
    Matrix_eio.Encryption.of_env (Harness.env h) ~user_id:user.user_id
      ~device_id:user.device_id ()
  in
  Matrix_eio.Encryption.execute_requests enc user.client
    (Matrix_eio.Encryption.outgoing_requests enc);
  enc

let context ?encryption ?event_store ?plugin_store h (user : Harness.user) =
  Context.v ~env:(Harness.env h) ~sw:(Harness.switch h) ~client:user.client
    ~clock:(Harness.clock h) ?encryption ?event_store ?plugin_store ()

(* One run of a bot. [Bot.run] returns only when the bot is stopped, so it
   goes in a fiber and the body plays the human beside it. *)
let run_bot ctx spec body =
  Eio.Switch.run @@ fun sw ->
  let handle = ref None in
  Eio.Fiber.fork ~sw (fun () ->
      Bot.run ~params:(params ())
        ~on_start:(fun bot -> handle := Some bot)
        ctx spec);
  let rec wait () =
    match !handle with
    | Some bot -> bot
    | None ->
        Eio.Fiber.yield ();
        wait ()
  in
  let bot = wait () in
  Fun.protect ~finally:(fun () -> Bot.stop bot) (fun () -> body bot)

let create_room ?(encrypted = false) ?(preset = Rooms.Private_chat) ?name
    (user : Harness.user) ~invite =
  let room_id =
    Harness.ok "create a room"
      (Rooms.create (Harness.base user) ~preset ~invite ~encrypted ())
  in
  Option.iter
    (fun name ->
      ignore
        (Harness.ok "set the room name"
           (Room_state.set_name (Harness.base user) ~room_id ~name)))
    name;
  room_id

let empower (owner : Harness.user) room_id (user : Harness.user) level =
  Harness.ok "grant a power level"
    (Rooms.set_user_power_level (Harness.base owner) ~room_id
       ~user_id:user.user_id ~level)

let event_items timeline =
  Array.to_list (Ui.Room_timeline.snapshot timeline)
  |> List.filter_map (function
    | Ui.Room_timeline.Event event -> Some event
    | Ui.Room_timeline.Virtual _ -> None)

let message_items timeline =
  List.filter
    (fun (item : Ui.Room_timeline.event_item) ->
      match item.event.content with
      | Ui.Presentation.Message _ -> true
      | _ -> false)
    (event_items timeline)

let body_of (item : Ui.Room_timeline.event_item) =
  Option.value ~default:"" (Ui.Presentation.preview item.event)

let event_id (item : Ui.Room_timeline.event_item) = item.event.event_id

let room_of runtime room_id =
  Ui.Room_list.find (Ui.Runtime.room_list runtime) room_id

let is_encrypted runtime room_id =
  match room_of runtime room_id with
  | Some (room : Ui.Room_list.room) -> room.encrypted
  | None -> false

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go index =
    index + n <= h && (String.sub haystack index n = needle || go (index + 1))
  in
  n = 0 || go 0

let saying timeline needle =
  List.filter
    (fun item -> contains ~needle (body_of item))
    (message_items timeline)

let wait_saying h ?(timeout = 120.) timeline needle =
  Harness.wait_for h ~timeout
    ~label:(Printf.sprintf "a message containing %S" needle) (fun () ->
      match saying timeline needle with item :: _ -> Some item | [] -> None)

(* A timeline item is the plaintext: {!Ui.Presentation.t.raw} of a decrypted
   event is the clear event, not the [m.room.encrypted] that arrived. What
   arrived is in the event cache beside it, which is where a scenario has to
   look to see that the wire was ciphertext. *)
let wire_was_encrypted runtime room_id event_id =
  Array.exists
    (fun (event : Ui.Event_cache.event) ->
      match event.event.event_id with
      | Some id
        when String.equal (Id.Event_id.to_string id)
               (Id.Event_id.to_string event_id) ->
          String.equal
            (Matrix_proto.Event.Event_type.to_string event.event.type_)
            "m.room.encrypted"
          && Option.is_some event.clear_event
      | _ -> false)
    (Ui.Event_cache.snapshot (Ui.Runtime.event_cache runtime) room_id)

let members runtime room_id =
  List.map uid
    (Matrix_eio.Sync_service.members (Ui.Runtime.sync_service runtime) room_id)

let wait_members h runtime room_id predicate =
  Harness.wait_until h ~timeout:120. ~label:"the room's membership" (fun () ->
      predicate (members runtime room_id))

(* Alice can observe a bot's join before the bot's runtime has created the
   per-room collector.  Sending in that window is treated as backlog by the
   bot's default [`Skip] policy, so scenarios wait for the handle itself. *)
let wait_bot_room h running ?(encrypted = false) room_id =
  Harness.wait_until h ~timeout:120. ~label:"the bot's room collector"
    (fun () ->
      match Bot.find_room running room_id with
      | Some room -> (not encrypted) || Matrix_bot.Room.encrypted room
      | None -> false)

(* The other side of every scenario is a real user with a runtime of its
   own, so that the assertions are on a decrypted, aggregated timeline
   rather than on a sync response. *)

type human = { runtime : Ui.Runtime.t }

let start_human ?encryption h (user : Harness.user) =
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw:(Harness.switch h) ~clock:(Harness.clock h)
      ~client:user.client ~sync ?encryption ()
  in
  Ui.Runtime.start ~params:(params ()) runtime;
  Harness.wait_until h ~timeout:120. ~label:"the human's first sync response"
    (fun () ->
      match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
      | Ui.Runtime.Live _ -> true
      | _ -> false);
  { runtime }

let say h human room body =
  Harness.wait_sent h ~timeout:120.
    (Ui.Room_timeline.send_text (Ui.Runtime.timeline human.runtime room) ~body)

(** {1 Echo} *)

let echo_scenario ~encrypted () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"echo" () in
  let alice_enc = if encrypted then Some (machine h alice) else None in
  let bot_enc = if encrypted then Some (machine h bot) else None in
  let human = start_human h ?encryption:alice_enc alice in
  let ctx = context h bot ?encryption:bot_enc in
  let spec = Matrix_bots.Echo.plugin (Bot.v ~name:"echo" ()) in
  run_bot ctx spec @@ fun running ->
  let room = create_room ~encrypted alice ~invite:[ bot.user_id ] in
  (* The bot joins on its own: this is [auto_join]. *)
  wait_members h human.runtime room (fun joined ->
      List.mem (uid bot.user_id) joined);
  wait_bot_room h running ~encrypted room;
  let timeline = Ui.Runtime.timeline human.runtime room in
  let body = "hello " ^ Harness.hex h 4 in
  ignore (say h human room body);
  let echoed = wait_saying h timeline ("you said: " ^ body) in
  check_bool "the echo comes from the bot" true
    (String.equal (uid echoed.event.sender) (uid bot.user_id));
  if encrypted then
    check_bool "and the room really is encrypted" true
      (is_encrypted human.runtime room);

  (* The bot must not echo its own echo — it answers as an m.notice, and the
     spec drops notices — and must not answer twice. *)
  Harness.wait_until h ~timeout:15. ~label:"a moment for a wrong second echo"
    (fun () -> List.length (saying timeline "you said: ") >= 1);
  check_bool "exactly one echo of the message" true
    (List.length (saying timeline ("you said: " ^ body)) = 1);
  check_bool "the bot did not echo its own echo" true
    (saying timeline ("you said: you said: " ^ body) = [])

let test_echo_plain () = echo_scenario ~encrypted:false ()
let test_echo_encrypted () = echo_scenario ~encrypted:true ()

(** {1 Commands} *)

let test_commands () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bob = Harness.register_user h ~prefix:"bob" () in
  let bot = Harness.register_user h ~prefix:"cmd" () in
  let human = start_human h alice in
  let ctx = context h bot in
  let spec = Matrix_bots.Commands.plugin (Bot.v ~name:"commands" ()) in
  run_bot ctx spec @@ fun running ->
  let room = create_room alice ~invite:[ bot.user_id; bob.user_id ] in
  wait_members h human.runtime room (fun joined ->
      List.mem (uid bot.user_id) joined);
  wait_bot_room h running room;
  (* Setting a topic is a state event, so the bot needs 50 of its own
     whoever asks for it. *)
  empower alice room bot 50;
  let timeline = Ui.Runtime.timeline human.runtime room in

  (* [!ping] answers with a reply relation, which the timeline reports as
     [reply_to] on the answer rather than as a separate event. *)
  let ping_id = say h human room "!ping" in
  let pong = wait_saying h timeline "pong" in
  check_bool "the answer replies to the command" true
    (match pong.reply_to with
    | Some target ->
        String.equal
          (Id.Event_id.to_string target)
          (Id.Event_id.to_string ping_id)
    | None -> false);

  (* [!help] is generated from the registrations, so every command is in it
     with the usage and doc it was registered with. *)
  ignore (say h human room "!help");
  let help = body_of (wait_saying h timeline "!roll NdM") in
  check_bool "the generated list names every command" true
    (List.for_all
       (fun needle -> contains ~needle help)
       [ "!ping"; "!roll NdM"; "!react"; "!topic <text>"; "!help" ]);

  (* [!roll] is the ordinary case. *)
  ignore (say h human room "!roll 3d6");
  ignore (wait_saying h timeline "3d6:");

  (* [!react] is folded into the command's own item. *)
  let react_id = say h human room "!react" in
  Harness.wait_until h ~timeout:120. ~label:"the reaction to be aggregated"
    (fun () ->
      List.exists
        (fun (item : Ui.Room_timeline.event_item) ->
          match event_id item with
          | Some id
            when Id.Event_id.to_string id = Id.Event_id.to_string react_id ->
              List.exists
                (fun (reaction : Ui.Room_timeline.reaction) ->
                  reaction.key = "\xf0\x9f\x91\x8d")
                item.reactions
          | _ -> false)
        (event_items timeline));

  (* [!topic] is registered [~admin:true], so a sender under power level 50
     is refused before the handler runs and nothing reaches the server.
     Alice made the room and has 100; bob is the ordinary member. *)
  ignore
    (Harness.ok "bob joins"
       (Rooms.join (Harness.base bob) ~room_id_or_alias:(`Room_id room) ()));
  wait_members h human.runtime room (fun joined ->
      List.mem (uid bob.user_id) joined);
  let bob_queue = Harness.start_send_queue h bob in
  let bob_says body =
    Harness.wait_sent h ~timeout:120.
      (Matrix_eio.Send_queue.send_text bob_queue ~room_id:room ~body)
  in
  ignore (bob_says "!topic first try");
  ignore (wait_saying h timeline "you need to be a moderator");
  empower alice room bob 50;
  Harness.wait_until h ~timeout:120.
    ~label:"the bot to see bob's new power level" (fun () ->
      match Bot.find_room running room with
      | Some the_room -> Bot.is_admin running the_room bob.user_id
      | None -> false);
  let topic = "bots welcome " ^ Harness.hex h 3 in
  ignore (bob_says ("!topic " ^ topic));
  Harness.wait_until h ~timeout:120. ~label:"the topic change to arrive"
    (fun () ->
      List.exists
        (fun (item : Ui.Room_timeline.event_item) ->
          match item.event.content with
          | Ui.Presentation.State
              { state = Ui.Presentation.Room_topic (Some t); _ } ->
              String.equal t topic
          | _ -> false)
        (event_items timeline));
  check_bool "the room list picked the topic up" true
    (match room_of human.runtime room with
    | Some room -> room.topic = Some topic
    | None -> false);

  (* An unknown command is answered, so a typo is not silence. *)
  ignore (say h human room "!nonsense");
  ignore (wait_saying h timeline "I do not know !nonsense")

let test_commands_encrypted () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"cmd" () in
  let alice_enc = machine h alice in
  let bot_enc = machine h bot in
  let human = start_human h ~encryption:alice_enc alice in
  let ctx = context h bot ~encryption:bot_enc in
  let spec = Matrix_bots.Commands.plugin (Bot.v ~name:"commands" ()) in
  run_bot ctx spec @@ fun running ->
  let room = create_room ~encrypted:true alice ~invite:[ bot.user_id ] in
  wait_members h human.runtime room (fun joined ->
      List.mem (uid bot.user_id) joined);
  wait_bot_room h running ~encrypted:true room;
  Harness.wait_until h ~timeout:120. ~label:"the room to be known encrypted"
    (fun () -> Matrix_eio.Encryption.is_room_encrypted alice_enc room);
  let timeline = Ui.Runtime.timeline human.runtime room in
  ignore (say h human room "!ping");
  let pong = wait_saying h timeline "pong" in
  check_bool "the decrypted answer comes from the bot" true
    (String.equal (uid pong.event.sender) (uid bot.user_id));
  check_bool "the item the human reads is the plaintext" true
    (String.equal
       (Matrix_proto.Event.Event_type.to_string pong.event.raw.type_)
       "m.room.message");
  check_bool "what arrived on the wire was encrypted" true
    (match event_id pong with
    | Some id -> wire_was_encrypted human.runtime room id
    | None -> false);
  ignore (say h human room "!help");
  let help = wait_saying h timeline "!roll NdM" in
  check_bool "and the generated list came back decrypted whole" true
    (contains ~needle:"!ping" (body_of help))

(** {1 Welcome} *)

let test_welcome () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"welcome" () in
  let newcomer = Harness.register_user h ~prefix:"newcomer" () in
  let human = start_human h alice in
  let ctx = context h bot in
  let spec = Matrix_bots.Welcome.plugin (Bot.v ~name:"welcome" ()) in
  run_bot ctx spec @@ fun running ->
  let room = create_room alice ~invite:[ bot.user_id ] in
  wait_members h human.runtime room (fun joined ->
      List.mem (uid bot.user_id) joined);
  wait_bot_room h running room;
  let timeline = Ui.Runtime.timeline human.runtime room in

  (* The bot says nothing about its own arrival. *)
  check_bool "no welcome for the bot itself" true
    (saying timeline ("Welcome, " ^ uid bot.user_id) = []);

  Harness.ok "invite the newcomer"
    (Rooms.invite (Harness.base alice) ~room_id:room ~user_id:newcomer.user_id
       ());
  ignore
    (Harness.ok "the newcomer joins"
       (Rooms.join (Harness.base newcomer) ~room_id_or_alias:(`Room_id room) ()));
  ignore (wait_saying h timeline ("Welcome, " ^ uid newcomer.user_id));

  let name = "kitchen-" ^ Harness.hex h 3 in
  ignore
    (Harness.ok "rename the room"
       (Room_state.set_name (Harness.base alice) ~room_id:room ~name));
  ignore (wait_saying h timeline ("The room is now called " ^ name));

  let topic = "tea " ^ Harness.hex h 3 in
  ignore
    (Harness.ok "set the topic"
       (Room_state.set_topic (Harness.base alice) ~room_id:room ~topic));
  ignore (wait_saying h timeline ("The topic is now: " ^ topic));

  (* A display name change is an [m.room.member] event that changes no
     membership, and reaches the plugin as an Event.Profile. *)
  let called = "Newt " ^ Harness.hex h 3 in
  Harness.ok "the newcomer renames themselves"
    (Matrix_client.Profile.set_displayname (Harness.base newcomer)
       ~displayname:called);
  ignore (wait_saying h timeline ("is now known as " ^ called));

  (* An avatar change is a [profile_change] whose [displayname] field is
     unset, so it must be reported on its own rather than dropped alongside
     an absent name change. *)
  let avatar_url =
    Result.get_ok
      (Matrix_client.Media.Mxc.of_string
         ("mxc://"
         ^ Matrix_proto.Id.Server_name.to_string
             (Matrix_proto.Id.User_id.server_name newcomer.user_id)
         ^ "/" ^ Harness.hex h 8))
  in
  Harness.ok "the newcomer sets an avatar"
    (Matrix_client.Profile.set_avatar_url (Harness.base newcomer) ~avatar_url);
  ignore (wait_saying h timeline "changed their avatar");

  Harness.ok "the newcomer leaves"
    (Rooms.leave (Harness.base newcomer) ~room_id:room ());
  ignore (wait_saying h timeline ("Goodbye, " ^ uid newcomer.user_id))

(** {1 Moderator} *)

let test_moderator () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"mod" () in
  let troll = Harness.register_user h ~prefix:"troll" () in
  let human = start_human h alice in
  (* One store for both runs of the bot: what the plugin writes through
     Bot.plugin_store is what the restart reads. *)
  let store = Plugin_store.memory () in
  let spec =
    Matrix_bots.Moderator.plugin ~words:[ "badger" ] ~strikes:3
      (Bot.v ~name:"moderator" ())
  in
  let room = create_room alice ~invite:[ bot.user_id; troll.user_id ] in
  let timeline = Ui.Runtime.timeline human.runtime room in
  let troll_queue = Harness.start_send_queue h troll in
  let offend n =
    let body = Printf.sprintf "look at that badger %d %s" n (Harness.hex h 3) in
    ignore
      (Harness.wait_sent h ~timeout:120.
         (Matrix_eio.Send_queue.send_text troll_queue ~room_id:room ~body));
    body
  in

  ( run_bot (context h bot ~plugin_store:store) spec @@ fun running ->
    (* A moderator with no power level cannot redact anybody but itself, so
      the room's owner gives it one. Its own is above the troll's, which is
      what kicking needs. *)
    wait_members h human.runtime room (fun joined ->
        List.mem (uid bot.user_id) joined);
    wait_bot_room h running room;
    empower alice room bot 100;
    ignore
      (Harness.ok "the troll joins"
         (Rooms.join (Harness.base troll) ~room_id_or_alias:(`Room_id room) ()));
    wait_members h human.runtime room (fun joined ->
        List.mem (uid troll.user_id) joined);

    ignore (offend 1);
    ignore (wait_saying h timeline "Strike 1 of 3");
    Harness.wait_until h ~timeout:120. ~label:"the first message to be redacted"
      (fun () ->
        List.exists
          (fun (item : Ui.Room_timeline.event_item) -> item.redacted)
          (event_items timeline));
    check_bool "the redacted item is still on the timeline" true
      (List.exists
         (fun (item : Ui.Room_timeline.event_item) ->
           item.redacted
           && String.equal (uid item.event.sender) (uid troll.user_id))
         (event_items timeline));

    ignore (offend 2);
    ignore (wait_saying h timeline "Strike 2 of 3") );
  check_int "the strikes are in the plugin store, not in the process" 2
    (Option.value ~default:0
       (Option.join
          (Result.to_option
             (Plugin_store.find store ~room ~plugin:"moderator"
                ~key:(uid troll.user_id) Matrix_proto.Json.Codec.int))));

  (* The restart is a second bot over the same store, so the next offence is
     the third strike and not the first. *)
  run_bot (context h bot ~plugin_store:store) spec @@ fun running ->
  Harness.wait_until h ~timeout:120. ~label:"the restarted bot to be live"
    (fun () -> List.mem (uid bot.user_id) (members human.runtime room));
  wait_bot_room h running room;
  ignore (offend 3);
  ignore (wait_saying h timeline "has been removed after 3 strikes");
  wait_members h human.runtime room (fun joined ->
      not (List.mem (uid troll.user_id) joined))

(** {1 Notify} *)

let test_notify () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"notify" () in
  let alice_enc = machine h alice in
  let bot_enc = machine h bot in
  let human = start_human h ~encryption:alice_enc alice in
  let room = create_room ~encrypted:true alice ~invite:[ bot.user_id ] in
  let timeline = Ui.Runtime.timeline human.runtime room in
  let body = "the backup finished " ^ Harness.hex h 4 in
  let ctx = context h bot ~encryption:bot_enc in
  (* Notify.send returns of its own accord, unlike a plugin: it is the cron
     shape. It joins on the way in, so nothing invites the bot. *)
  let sent =
    match Matrix_bots.Notify.send ctx ~room:(`Room_id room) ~body () with
    | Ok event_id -> event_id
    | Error message -> Alcotest.failf "notify: %s" message
  in
  let seen = wait_saying h timeline body in
  check_bool "the id the notifier answered is the event alice sees" true
    (match event_id seen with
    | Some id ->
        String.equal (Id.Event_id.to_string id) (Id.Event_id.to_string sent)
    | None -> false);
  check_bool "and it went out encrypted" true
    (wire_was_encrypted human.runtime room sent)

(** {1 Logger} *)

let test_logger () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let bot = Harness.register_user h ~prefix:"logger" () in
  let human = start_human h alice in
  let room =
    create_room alice ~name:("log-" ^ Harness.hex h 3) ~invite:[ bot.user_id ]
  in
  (* Enough history that the bot's first sync window cannot hold all of it,
     so the backfill has a gap to resolve. *)
  let history =
    List.init 15 (fun n ->
        let body = Printf.sprintf "line %02d %s" n (Harness.hex h 3) in
        ignore (say h human room body);
        body)
  in
  let oldest = List.hd history in

  let path = Filename.temp_file "matrix-bots-logger-" ".sqlite3" in
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
  let buffer = Buffer.create 4096 in
  let out = Format.formatter_of_buffer buffer in
  let logged needle =
    Format.pp_print_flush out ();
    contains ~needle (Buffer.contents buffer)
  in
  let store = open_store () in
  let live = "live " ^ Harness.hex h 4 in
  (let spec =
     Matrix_bots.Logger.plugin ~out ~backfill:20 (Bot.v ~name:"logger" ())
   in
   run_bot (context h bot ~event_store:store) spec @@ fun _ ->
   (* The bot accepts the invite itself and then back-paginates; the pages
      reach the plugin as events like anything else. *)
   wait_members h human.runtime room (fun joined ->
       List.mem (uid bot.user_id) joined);
   Harness.wait_until h ~timeout:120.
     ~label:"the log to reach the oldest message" (fun () -> logged oldest);
   ignore (say h human room live);
   Harness.wait_until h ~timeout:120. ~label:"the log to show the new message"
     (fun () -> logged live));
  check_bool "the room list was printed" true (logged "room(s):");
  Ui.Event_store.close store;

  (* A cold cache over the same file decodes only its newest persisted chunk.
     Walk its local predecessors explicitly, with nothing driving sync or the
     network, to prove the whole backfill survived without relying on an eager
     restart load. *)
  let store = open_store () in
  let cold = Ui.Event_cache.create ~store () in
  let previews () =
    Array.to_list (Ui.Event_cache.snapshot cold room)
    |> List.filter_map (fun (event : Ui.Event_cache.event) ->
        Ui.Presentation.preview
          (Ui.Presentation.of_event (Ui.Event_cache.effective event)))
  in
  check_bool "the cold tail kept the live message" true
    (List.mem live (previews ()));
  let rec hydrate_local () =
    match Ui.Event_cache.hydrate_previous cold room with
    | Ui.Event_cache.Hydrated _ -> hydrate_local ()
    | Ui.Event_cache.No_persisted_history -> ()
    | Ui.Event_cache.Hydration_failed error ->
        Alcotest.failf "hydrating the logger store: %s"
          (Ui.Event_store.Error.to_string error)
  in
  hydrate_local ();
  let cached = previews () in
  Ui.Event_store.close store;
  check_bool "the store kept the backfilled history" true
    (List.mem oldest cached);
  check_bool "and the message that arrived live" true (List.mem live cached)

let tests =
  [
    Alcotest.test_case "the echo plugin answers in a room" `Quick
      test_echo_plain;
    Alcotest.test_case "the echo plugin answers in an encrypted room" `Quick
      test_echo_encrypted;
    Alcotest.test_case "the commands plugin answers its commands" `Quick
      test_commands;
    Alcotest.test_case "the commands plugin answers in an encrypted room" `Quick
      test_commands_encrypted;
    Alcotest.test_case "the welcome plugin narrates membership and state" `Quick
      test_welcome;
    Alcotest.test_case "the moderator plugin keeps its strikes across a restart"
      `Quick test_moderator;
    Alcotest.test_case "the notifier sends one encrypted message" `Quick
      test_notify;
    Alcotest.test_case "the logger plugin backfills and persists" `Quick
      test_logger;
  ]
