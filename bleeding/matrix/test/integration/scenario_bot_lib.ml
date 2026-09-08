(** [matrix-chat.bot] against a live homeserver.

    One bot, three runs of it. The first is the whole life of a bot: invited to
    an encrypted room by a human, it joins by itself, answers [!ping] with a
    reply and [!help] with the list it generates, and both answers reach the
    human decrypted. {!Matrix_bot.Bot.stop} then returns from
    {!Matrix_bot.Bot.run} with no fiber of its own left, which is what the
    scenario's [Eio.Switch.run] proves.

    The other two runs are restarts over the same {!Matrix_bot.Plugin_store.t}
    and the same SQLite event store, differing only in [backlog]: a message sent
    while the bot was down is passed over under [`Skip] and handled under
    [`Handle].

    Alice is the human; she is driven through a {!Matrix_ui.Runtime} of her own,
    because what the checks read is what her client shows after decryption. *)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Bot = Matrix_bot.Bot
module Context = Matrix_bot.Context
module Event = Matrix_bot.Event
module Plugin_store = Matrix_bot.Plugin_store
module Rooms = Matrix_client.Rooms

let check_bool = Alcotest.(check bool)
let check_strings = Alcotest.(check (list string))
let eid = Id.Event_id.to_string

let contains ~needle haystack =
  let width = String.length needle and length = String.length haystack in
  let rec scan index =
    index + width <= length
    && (String.equal (String.sub haystack index width) needle
       || scan (index + 1))
  in
  scan 0

(* Synapse caches a [/sync] response for two minutes under a key that
   includes the long-poll timeout, so two runtimes for one user asking for
   the same timeout replay each other's responses. Varying it keeps them
   apart, and a bot restarting is a second runtime for the same user. *)
let sync_timeout = ref 29_000

let params () =
  decr sync_timeout;
  { Matrix_client.Sync.default_params with timeout = !sync_timeout }

let start_runtime h ~sw ?encryption (user : Harness.user) =
  let sync =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let runtime =
    Ui.Runtime.create ~sw ~clock:(Harness.clock h) ~client:user.client ~sync
      ?encryption ()
  in
  Ui.Runtime.start ~params:(params ()) runtime;
  runtime

let wait_live h runtime =
  Harness.wait_until h ~label:"the first sync response" (fun () ->
      match Ui.Observable.Value.get (Ui.Runtime.sync_state runtime) with
      | Ui.Runtime.Live _ -> true
      | _ -> false)

let event_items timeline =
  Array.to_list (Ui.Room_timeline.snapshot timeline)
  |> List.filter_map (function
    | Ui.Room_timeline.Event item -> Some item
    | _ -> None)

let body_of (item : Ui.Room_timeline.event_item) =
  Option.value ~default:"?" (Ui.Presentation.preview item.event)

let find_where timeline predicate =
  List.find_opt (fun item -> predicate (body_of item)) (event_items timeline)

(* One run of the bot, on a switch of its own. [Eio.Switch.run] returns only
   once every fiber forked on it has, so a run that comes back is a bot that
   left nothing behind. *)
let run_bot ctx spec body =
  let returned = ref false in
  Eio.Switch.run (fun sw ->
      let handle = ref None in
      Eio.Fiber.fork ~sw (fun () ->
          Bot.run ~params:(params ())
            ~on_start:(fun bot -> handle := Some bot)
            ctx spec;
          returned := true);
      let rec wait () =
        match !handle with
        | Some bot -> bot
        | None ->
            Eio.Fiber.yield ();
            wait ()
      in
      let bot = wait () in
      Fun.protect ~finally:(fun () -> Bot.stop bot) (fun () -> body bot));
  !returned

let sqlite_store path =
  match Matrix_ui_sqlite.create ~plaintext_policy:Store_plaintext path with
  | Ok store -> store
  | Error error ->
      Alcotest.failf "opening %s: %s" path
        (Ui.Event_store.Error.to_string error)

(* A bot answering in an encrypted room it was invited to, and stopping. *)
let test_bot_in_an_encrypted_room () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let robot = Harness.register_user h ~prefix:"robot" () in
  let machine (user : Harness.user) =
    Matrix_eio.Encryption.of_env (Harness.env h) ~user_id:user.user_id
      ~device_id:user.device_id ()
  in
  let alice_enc = machine alice and robot_enc = machine robot in
  let path = Filename.temp_file "matrix-bot-events-" ".sqlite3" in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
  @@ fun () ->
  let store = Plugin_store.memory () in
  let alice_runtime =
    start_runtime h ~sw:(Harness.switch h) ~encryption:alice_enc alice
  in
  wait_live h alice_runtime;

  let events = sqlite_store path in
  let ctx =
    Context.v ~env:(Harness.env h) ~sw:(Harness.switch h) ~client:robot.client
      ~clock:(Harness.clock h) ~encryption:robot_enc ~event_store:events
      ~plugin_store:store ()
  in
  let spec =
    Bot.v ()
    |> Bot.command ~name:"ping" (fun _ c ->
        ignore (Event.reply c.message.envelope "pong"))
    |> Bot.help
  in
  let room_id = ref None in
  let returned =
    run_bot ctx spec @@ fun bot ->
    (* Alice makes the room and invites the bot; nothing tells the bot to
       join, [auto_join] does. *)
    let room =
      Harness.ok "create an encrypted room"
        (Rooms.create (Harness.base alice) ~preset:Rooms.Trusted_private_chat
           ~invite:[ robot.user_id ] ~encrypted:true ())
    in
    room_id := Some room;
    Harness.wait_until h ~timeout:120. ~label:"the bot to join on its own"
      (fun () -> Bot.find_room bot room <> None);
    List.iter
      (fun (runtime, enc) ->
        Harness.wait_until h ~timeout:120.
          ~label:"the room to be known encrypted, with both members" (fun () ->
            Matrix_eio.Encryption.is_room_encrypted enc room
            && List.length
                 (Matrix_eio.Sync_service.members
                    (Ui.Runtime.sync_service runtime)
                    room)
               >= 2))
      [ (alice_runtime, alice_enc); (Bot.runtime bot, robot_enc) ];
    check_bool "the bot sees the room as encrypted" true
      (Matrix_bot.Room.encrypted (Option.get (Bot.find_room bot room)));

    let alice_timeline = Ui.Runtime.timeline alice_runtime room in
    let ping =
      Harness.wait_sent h ~timeout:120.
        (Ui.Room_timeline.send_text alice_timeline ~body:"!ping")
    in
    Harness.wait_until h ~timeout:120. ~label:"the bot to answer !ping"
      (fun () -> find_where alice_timeline (String.equal "pong") <> None);
    let pong = Option.get (find_where alice_timeline (String.equal "pong")) in
    check_bool "the answer is a reply to the command" true
      (Option.map eid pong.reply_to = Some (eid ping));
    check_bool "and reaches alice as an m.notice, decrypted" true
      (match pong.event.content with
      | Ui.Presentation.Message { kind = Ui.Presentation.Notice; _ } -> true
      | _ -> false);

    ignore
      (Harness.wait_sent h ~timeout:120.
         (Ui.Room_timeline.send_text alice_timeline ~body:"!help"));
    let generated = contains ~needle:"list the commands" in
    Harness.wait_until h ~timeout:120. ~label:"the bot to answer !help"
      (fun () -> find_where alice_timeline generated <> None);
    let help = body_of (Option.get (find_where alice_timeline generated)) in
    check_bool "the generated list names every command" true
      (contains ~needle:"!ping" help && contains ~needle:"!help" help)
  in
  check_bool "stop returned from run, leaving no fiber behind" true returned;
  Ui.Event_store.close events;

  (* Two restarts over the same state and the same store. Alice says
     something while the bot is down each time; the first run is told to skip
     it, the second to handle it. *)
  let room = Option.get !room_id in
  let alice_timeline = Ui.Runtime.timeline alice_runtime room in
  let say body =
    ignore
      (Harness.wait_sent h ~timeout:120.
         (Ui.Room_timeline.send_text alice_timeline ~body))
  in
  let restart ~backlog =
    let seen = ref [] in
    let events = sqlite_store path in
    let ctx =
      Context.v ~env:(Harness.env h) ~sw:(Harness.switch h) ~client:robot.client
        ~clock:(Harness.clock h) ~encryption:robot_enc ~event_store:events
        ~plugin_store:store ()
    in
    let spec =
      Bot.v ~backlog ()
      |> Bot.on_message (fun _ (message : Event.message) ->
          seen := message.content.body :: !seen)
    in
    let returned =
      run_bot ctx spec @@ fun bot ->
      Harness.wait_until h ~timeout:120. ~label:"the bot to pick the room up"
        (fun () -> Bot.find_room bot room <> None);
      (* One more round of the loop, so that anything the backlog was going
         to deliver has been. *)
      Eio.Time.sleep (Harness.clock h) 2.
    in
    check_bool "the restart stopped cleanly" true returned;
    Ui.Event_store.close events;
    List.rev !seen
  in
  say "while stopped, skipped";
  check_strings "a `Skip restart passes over what arrived while it was down" []
    (restart ~backlog:`Skip);
  say "while stopped, handled";
  check_strings "and a `Handle restart delivers what came after the cursor"
    [ "while stopped, handled" ]
    (restart ~backlog:`Handle);
  check_bool "the cursor is kept per room under the bot's name" true
    (Plugin_store.find store ~room ~plugin:"matrix.bot/bot" ~key:"cursor"
       Matrix_proto.Json.Codec.string
    <> Ok None)

let tests =
  [
    Alcotest.test_case "a bot in an encrypted room, and its restarts" `Slow
      test_bot_in_an_encrypted_room;
  ]
