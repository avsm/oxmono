(** Tests for [matrix-chat.bot] with no homeserver: the plugin store, the command
    parser, spec construction, and the dispatcher driven over a mock [/sync].

    The bot's fibers are the point, so most checks run inside
    {!Matrix_bot.Bot.run}: [~on_start] hands out the running bot, the body feeds
    the runtime's event cache the way [test_matrix_ui.ml] does, waits for the
    handlers to be called and then stops the bot, which is what makes [run]
    return. The live counterpart is [test/integration/scenario_bot_lib.ml]. *)

module Ui = Matrix_ui
module Id = Matrix_proto.Id
module Bot = Matrix_bot.Bot
module Context = Matrix_bot.Context
module Args = Matrix_bot.Args
module Event = Matrix_bot.Event
module Room = Matrix_bot.Room
module Sent = Matrix_bot.Sent
module Plugin_store = Matrix_bot.Plugin_store
module Queue = Matrix_client.Send_queue
module Main = Matrix_bot.Main

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)
let check_strings = Alcotest.(check (list string))
let room_id = Id.Room_id.of_string_exn "!room:example.org"
let other_room = Id.Room_id.of_string_exn "!other:example.org"
let bot_user = Id.User_id.of_string_exn "@bot:example.org"
let alice = Id.User_id.of_string_exn "@alice:example.org"
let carol = Id.User_id.of_string_exn "@carol:example.org"

let contains ~needle haystack =
  let width = String.length needle and length = String.length haystack in
  let rec scan index =
    index + width <= length
    && (String.equal (String.sub haystack index width) needle
       || scan (index + 1))
  in
  scan 0

(** {1 The plugin store} *)

let ok result = Result.get_ok result

let stored t ?room ~plugin key =
  ok (Plugin_store.find t ?room ~plugin ~key Matrix_proto.Json.Codec.string)

let test_store_memory () =
  Eio_main.run @@ fun _ ->
  let t = Plugin_store.memory () in
  check_bool "an empty store knows nothing" true
    (stored t ~plugin:"p" "k" = None);
  ok
    (Plugin_store.set t ~plugin:"p" ~key:"k" Matrix_proto.Json.Codec.string
       "global");
  ok
    (Plugin_store.set t ~room:room_id ~plugin:"p" ~key:"k"
       Matrix_proto.Json.Codec.string "in the room");
  check_string "the global value" "global"
    (Option.get (stored t ~plugin:"p" "k"));
  check_string "and the room's own, under the same key" "in the room"
    (Option.get (stored t ~room:room_id ~plugin:"p" "k"));
  check_bool "another room has none" true
    (stored t ~room:other_room ~plugin:"p" "k" = None);
  check_bool "another plugin has none" true (stored t ~plugin:"q" "k" = None);
  let bump =
    Plugin_store.update t ~plugin:"p" ~key:"count" Matrix_proto.Json.Codec.int
      (function
      | None -> 1
      | Some n -> n + 1)
  in
  check_int "update returns what it stored" 1 (ok bump);
  check_int "and folds the value that was there" 2
    (ok
       (Plugin_store.update t ~plugin:"p" ~key:"count"
          Matrix_proto.Json.Codec.int (function
         | None -> 1
         | Some n -> n + 1)));
  check_strings "the keys of a plugin" [ "count"; "k" ]
    (Plugin_store.keys t ~plugin:"p" ());
  ok (Plugin_store.remove t ~plugin:"p" ~key:"k" ());
  check_bool "a removed key is gone" true (stored t ~plugin:"p" "k" = None);
  check_bool "and the room's copy is untouched" true
    (stored t ~room:room_id ~plugin:"p" "k" = Some "in the room");
  check_bool "a value that no longer decodes is an error" true
    (Result.is_error
       (Plugin_store.find t ~plugin:"p" ~key:"count"
          Matrix_proto.Json.Codec.string))

let with_temp_dir env f =
  let name =
    Printf.sprintf "matrix-bot-test-%d-%d" (Unix.getpid ()) (Random.bits ())
  in
  let dir =
    Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name () / name)
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun entry -> try Eio.Path.unlink Eio.Path.(dir / entry) with _ -> ())
        (Eio.Path.read_dir dir);
      try Eio.Path.rmdir dir with _ -> ())
    (fun () -> f dir)

let test_store_file () =
  Eio_main.run @@ fun env ->
  with_temp_dir env @@ fun dir ->
  let path = Eio.Path.(dir / "state.json") in
  let t = Plugin_store.open_file path in
  ok
    (Plugin_store.set t ~plugin:"p" ~key:"k" Matrix_proto.Json.Codec.string
       "value");
  ok
    (Plugin_store.set t ~room:room_id ~plugin:"p" ~key:"cursor"
       Matrix_proto.Json.Codec.string "$one");
  check_bool "the file is written on the first set" true (Eio.Path.is_file path);
  check_bool "and nothing is left half-written beside it" false
    (List.mem "state.json.tmp" (Eio.Path.read_dir dir));
  let reopened = Plugin_store.open_file path in
  check_string "a reopened store reads the global value back" "value"
    (Option.get (stored reopened ~plugin:"p" "k"));
  check_string "and the room's" "$one"
    (Option.get (stored reopened ~room:room_id ~plugin:"p" "cursor"));
  ok (Plugin_store.remove reopened ~plugin:"p" ~key:"k" ());
  check_bool "a removal survives the round trip" true
    (stored (Plugin_store.open_file path) ~plugin:"p" "k" = None);
  (* A file that does not parse is moved aside, not overwritten in place and
     not fatal: the bot starts with an empty store. *)
  Eio.Path.save ~create:(`Or_truncate 0o600) path "{ this is not json";
  let broken = Plugin_store.open_file path in
  check_bool "a broken file leaves an empty store" true
    (stored broken ~room:room_id ~plugin:"p" "cursor" = None);
  check_bool "and is renamed aside" true
    (List.exists
       (fun entry -> String.starts_with ~prefix:"state.json.broken-" entry)
       (Eio.Path.read_dir dir))

let test_store_file_atomic_writes () =
  Eio_main.run @@ fun env ->
  with_temp_dir env @@ fun dir ->
  let path = Eio.Path.(dir / "state.json") in
  let first = Plugin_store.open_file path in
  let second = Plugin_store.open_file path in
  let failures = ref [] in
  Eio.Switch.run @@ fun sw ->
  let first_done, first_done_r = Eio.Promise.create () in
  let second_done, second_done_r = Eio.Promise.create () in
  let write_many t prefix resolver =
    Eio.Fiber.fork ~sw (fun () ->
        for i = 1 to 25 do
          match
            Plugin_store.set t ~plugin:prefix ~key:(string_of_int i)
              Matrix_proto.Json.Codec.string prefix
          with
          | Ok () -> ()
          | Error error ->
              failures := Plugin_store.error_to_string error :: !failures
        done;
        Eio.Promise.resolve resolver ())
  in
  write_many first "first" first_done_r;
  write_many second "second" second_done_r;
  Eio.Promise.await first_done;
  Eio.Promise.await second_done;
  check_strings "concurrent handles report no write failures" [] !failures;
  ignore (Plugin_store.open_file path);
  check_bool "a concurrent write leaves a complete target" true
    (Eio.Path.is_file path);
  check_bool "unique temporary names are cleaned after success" false
    (List.exists
       (String.starts_with ~prefix:"state.json.tmp.")
       (Eio.Path.read_dir dir));

  (* A failed rename must clean the unique temporary file too. *)
  let directory_target = Eio.Path.(dir / "directory-target.json") in
  Eio.Path.mkdir ~perm:0o700 directory_target;
  let failed_open =
    try
      ignore (Plugin_store.open_file directory_target);
      false
    with Eio.Io _ -> true
  in
  check_bool "a directory is not mistaken for a missing store" true failed_open;
  let flush_target = Eio.Path.(dir / "flush-target.json") in
  let failed_store = Plugin_store.open_file flush_target in
  Eio.Path.mkdir ~perm:0o700 flush_target;
  let failed =
    Plugin_store.set failed_store ~plugin:"p" ~key:"k"
      Matrix_proto.Json.Codec.string "value"
  in
  check_bool "a replacement error is reported" true (Result.is_error failed);
  check_bool "failed replacement cleans its temporary file" false
    (List.exists
       (String.starts_with ~prefix:"directory-target.json.tmp.")
       (Eio.Path.read_dir dir));
  Eio.Path.rmdir directory_target;
  Eio.Path.rmdir flush_target

(** {1 Commands} *)

let test_command_parse () =
  let parse = Args.parse ~prefix:"!" in
  let show = function None -> "-" | Some (name, args) -> name ^ "/" ^ args in
  check_string "a bare command" "ping/" (show (parse "!ping"));
  check_string "leading and trailing space is trimmed" "ping/"
    (show (parse "  !ping  "));
  check_string "the name stops at the first space" "ping/one two"
    (show (parse "!ping one two"));
  check_string "the arguments are trimmed" "ping/one"
    (show (parse "!ping   one   "));
  check_string "a tab separates too" "ping/one" (show (parse "!ping\tone"));
  check_string "text without the prefix is not a command" "-"
    (show (parse "ping"));
  check_string "the prefix alone is not a command" "-" (show (parse "!"));
  check_string "nor is the prefix with only spaces" "-" (show (parse "!   "));
  check_string "a longer prefix" "roll/2d6"
    (show (Args.parse ~prefix:"bot: " "bot: roll 2d6"));
  check_strings "argv splits on any whitespace" [ "a"; "b"; "c" ]
    (Args.argv " a\tb\n c ");
  check_strings "and an empty argument string has no words" [] (Args.argv "  ")

(** {1 Spec construction} *)

let noop _ _ = ()

let test_spec_commands () =
  let spec =
    Bot.v ()
    |> Bot.command ~name:"ping" ~doc:"answer pong" noop
    |> Bot.command ~name:"roll" ~args:"<dice>" noop
    |> Bot.help
  in
  check_strings "the commands are listed in registration order, help last"
    [ "ping"; "roll"; "help" ]
    (List.map (fun (c : Bot.command_info) -> c.name) (Bot.commands spec));
  check_bool "with the arguments and doc they were given" true
    (Bot.commands spec
    = [
        { Bot.name = "ping"; args = None; doc = Some "answer pong" };
        { Bot.name = "roll"; args = Some "<dice>"; doc = None };
        { Bot.name = "help"; args = None; doc = Some "list the commands" };
      ]);
  check_strings "a renamed help command" [ "commands" ]
    (List.map
       (fun (c : Bot.command_info) -> c.name)
       (Bot.commands (Bot.help ~command:"commands" (Bot.v ()))))

(** {1 A bot over a mock homeserver}

    The mock answers one scripted [/sync] and then idles, which is enough to put
    the room in the room list and start its collector and dispatcher. Everything
    after that is fed straight into the runtime's event cache. *)

let json_event ?state_key ?(sender = alice) ?(ts = 1_700_000_000_000L) ~id
    ~type_ content =
  let state =
    match state_key with
    | None -> ""
    | Some key -> Printf.sprintf {|,"state_key":"%s"|} key
  in
  Printf.sprintf
    {|{"event_id":"%s","sender":"%s","origin_server_ts":%Ld,"type":"%s"%s,"content":%s}|}
    id
    (Id.User_id.to_string sender)
    ts type_ state content

let text ?sender ?(msgtype = "m.text") body =
  Printf.sprintf {|{"msgtype":"%s","body":"%s"}|} msgtype body |> fun content ->
  fun ~id -> json_event ?sender ~id ~type_:"m.room.message" content

let sync_body ~batch events =
  Printf.sprintf
    {|{"next_batch":"%s","rooms":{"join":{"!room:example.org":{"timeline":{"events":[%s],"limited":false,"prev_batch":"p"}}}}}|}
    batch (String.concat "," events)

let response json =
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> response
  | Error message -> Alcotest.fail message

(* The events a test wants delivered after the bot is running go through the
   base client, exactly as a sync response would, and out of it come the
   [room_change]s the event cache merges. *)
let feed cache state json =
  let state, changes = Matrix_client.Base_client.apply state (response json) in
  List.iter (Ui.Event_cache.apply_room_change cache) changes.room_changes;
  state

type server = { fetch : Fetch.plain; sent : string list ref }

let mock_server ~clock script =
  let pending = ref script in
  let sent = ref [] in
  let count = ref 0 in
  let fetch =
    Fetch_mock.client (fun (request : Fetch.Middleware.request) ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains ~needle:"/sync" url then (
          match !pending with
          | body :: rest ->
              pending := rest;
              Fetch_mock.respond body request
          | [] ->
              (* Without this the loop would spin: the mock answers at once
                 and the service asks again immediately. *)
              Eio.Time.sleep clock 0.05;
              incr count;
              Fetch_mock.respond
                (Printf.sprintf {|{"next_batch":"idle%d"}|} !count)
                request)
        else if contains ~needle:"/send/" url || contains ~needle:"/redact/" url
        then (
          (match request.body with
          | Fetch.String body -> sent := body :: !sent
          | _ -> ());
          incr count;
          Fetch_mock.respond
            (Printf.sprintf {|{"event_id":"$sent%d:example.org"}|} !count)
            request)
        else Fetch_mock.respond "{}" request)
  in
  { fetch; sent }

let client_of ~sw ~env server =
  let client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      ~fetch:server.fetch ()
  in
  Matrix_eio.Client.with_session client
    {
      user_id = bot_user;
      device_id = Id.Device_id.of_string_exn "BOTDEVICE";
      access_token = "token";
      refresh_token = None;
    }

let until ~clock ?(timeout = 10.) label predicate =
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if predicate () then ()
    else if Eio.Time.now clock > deadline then
      Alcotest.failf "timed out waiting for %s" label
    else (
      Eio.Time.sleep clock 0.005;
      loop ())
  in
  loop ()

let params = { Matrix_client.Sync.default_params with timeout = 100 }

(* One run of a bot against the mock. [body] is called with the running bot
   and must stop it, which is what makes [run] return. *)
let run_bot ~env ~store ?event_store ~script spec body =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let server = mock_server ~clock script in
  let client = client_of ~sw ~env server in
  let ctx = Context.v ~env ~sw ~client ~plugin_store:store ?event_store () in
  Bot.run ~params
    ~on_start:(fun bot ->
      Fun.protect ~finally:(fun () -> Bot.stop bot) (fun () -> body server bot))
    ctx spec

let joined ~clock bot =
  until ~clock "the room to be handled" (fun () ->
      Bot.find_room bot room_id <> None)

let names events =
  List.map
    (function
      | Event.Message m -> "message:" ^ m.content.body
      | Event.Command c -> "command:" ^ c.name ^ ":" ^ c.args
      | Event.Edit e -> "edit:" ^ e.message.content.body
      | Event.Sticker { body; _ } -> "sticker:" ^ body
      | Event.Poll { text; _ } -> "poll:" ^ text
      | Event.Reaction { key; _ } -> "reaction:" ^ key
      | Event.Redaction _ -> "redaction"
      | Event.Membership { user; _ } ->
          "membership:" ^ Id.User_id.to_string user
      | Event.Profile { user; _ } -> "profile:" ^ Id.User_id.to_string user
      | Event.Room_state _ -> "state"
      | Event.Custom { event_type; _ } -> "custom:" ^ event_type
      | Event.Invited _ -> "invited"
      | Event.Joined _ -> "joined"
      | Event.Left _ -> "left"
      | Event.Sync _ -> "sync")
    events

(* Ordering, the two default filters, and a handler that raises: the room's
   dispatcher runs the handlers in registration order, one event at a time,
   and the exception reaches neither the next handler nor the next event. *)
let test_dispatcher () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let seen = ref [] in
  let raised = ref 0 in
  let filtered = ref 0 in
  let elsewhere = ref 0 in
  let errors = ref [] in
  let senders = ref [] in
  let membership_reasons = ref [] in
  let custom_types = ref [] in
  let spec =
    Bot.v ~name:"dispatch" ()
    |> Bot.on (fun _ _ ->
        incr raised;
        failwith "handlers may raise")
    |> Bot.on (fun _ event -> seen := event :: !seen)
    |> Bot.in_rooms [ room_id ] (Bot.on_message (fun _ _ -> incr filtered))
    |> Bot.in_rooms [ other_room ] (Bot.on_message (fun _ _ -> incr elsewhere))
    |> Bot.from_users [ carol ]
         (Bot.on_message (fun _ message ->
              senders := message.content.body :: !senders))
    |> Bot.on_membership (fun _ membership ->
        membership_reasons := membership.reason :: !membership_reasons)
    |> Bot.on_custom (fun _ custom ->
        custom_types := custom.event_type :: !custom_types)
    |> Bot.on_error (fun _ _ exn -> errors := Printexc.to_string exn :: !errors)
  in
  run_bot ~env ~store
    ~script:[ sync_body ~batch:"s1" [] ]
    spec
    (fun _ bot ->
      joined ~clock bot;
      let cache = Ui.Runtime.event_cache (Bot.runtime bot) in
      let base = Matrix_client.Base_client.create ~user_id:bot_user () in
      let _ =
        feed cache base
          (sync_body ~batch:"s2"
             [
               text "hello" ~id:"$m1";
               text "!ping one two" ~id:"$c1";
               json_event ~id:"$r1" ~type_:"m.reaction"
                 {|{"m.relates_to":{"rel_type":"m.annotation","event_id":"$m1","key":"👍"}}|};
               json_event ~id:"$mem" ~type_:"m.room.member"
                 ~state_key:"@carol:example.org"
                 {|{"membership":"join","reason":"invited by the test"}|};
               json_event ~id:"$custom" ~type_:"org.example.widget"
                 {|{"answer":42}|};
               text ~sender:bot_user "mine" ~id:"$own";
               text ~msgtype:"m.notice" "quiet" ~id:"$notice";
             ])
      in
      until ~clock "every event to be handled" (fun () ->
          List.length !seen >= 7);
      Eio.Time.sleep clock 0.05);
  let events = List.rev !seen in
  (* A room's events and the sync state travel on separate streams, so only
     the order within the room is a promise. *)
  check_strings "the room's events, in order, without the bot's own or a notice"
    [
      "joined";
      "message:hello";
      "command:ping:one two";
      "reaction:\u{1F44D}";
      "membership:@carol:example.org";
      "custom:org.example.widget";
    ]
    (List.filter (fun name -> not (String.equal name "sync")) (names events));
  check_bool "and the sync state was announced" true
    (List.mem "sync" (names events));
  check_bool "the raising handler ran for every one of them" true (!raised >= 6);
  check_int "and the next handler still saw them" 7 (List.length events);
  check_int "a plugin scoped to the room saw its message" 1 !filtered;
  check_int "and one scoped elsewhere saw nothing" 0 !elsewhere;
  check_bool "every raise reached the error handler" true
    (List.length
       (List.filter (fun e -> contains ~needle:"handlers may raise" e) !errors)
    >= 7);
  check_strings "and a plugin scoped to another sender saw nothing" [] !senders;
  check_bool "membership handlers retain the moderation reason" true
    (!membership_reasons = [ Some "invited by the test" ]);
  check_strings "custom handlers retain the Matrix event type"
    [ "org.example.widget" ] !custom_types;
  check_string "the cursor is the last event handled" "$custom"
    (Option.get
       (stored store ~room:room_id ~plugin:"matrix.bot/dispatch" "cursor"))

(* [help] reads the command list off the running bot, so the text names
   every command however late [help] was registered. *)
let test_help_and_reply () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let spec =
    Bot.v ~name:"help" () |> Bot.help
    |> Bot.command ~name:"ping" ~doc:"answer pong" (fun _ c ->
        ignore (Event.reply c.message.envelope "pong"))
  in
  let bodies = ref [] in
  run_bot ~env ~store
    ~script:[ sync_body ~batch:"s1" [] ]
    spec
    (fun server bot ->
      joined ~clock bot;
      let cache = Ui.Runtime.event_cache (Bot.runtime bot) in
      let base = Matrix_client.Base_client.create ~user_id:bot_user () in
      let _ =
        feed cache base
          (sync_body ~batch:"s2"
             [ text "!help" ~id:"$h"; text "!ping" ~id:"$p" ])
      in
      until ~clock "both answers to be sent" (fun () ->
          List.length !(server.sent) >= 2);
      bodies := List.rev !(server.sent));
  match !bodies with
  | [ help; ping ] ->
      check_bool "the help text lists help" true (contains ~needle:"!help" help);
      check_bool "and ping with its doc" true
        (contains ~needle:"!ping — answer pong" help);
      check_bool "a bot answers as a notice" true
        (contains ~needle:"m.notice" help);
      check_bool "and in reply to the command" true
        (contains ~needle:"m.in_reply_to" help);
      check_bool "the command's own answer went out" true
        (contains ~needle:"pong" ping)
  | other -> Alcotest.failf "expected two sends, got %d" (List.length other)

(* A command nothing claims is answered once, by the default handler. *)
let test_unknown_command () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let unknown = ref [] in
  let spec =
    Bot.v ~name:"unknown" ()
    |> Bot.command ~name:"ping" (fun _ _ -> ())
    |> Bot.on_unknown_command (fun _ c -> unknown := c.name :: !unknown)
  in
  run_bot ~env ~store
    ~script:[ sync_body ~batch:"s1" [] ]
    spec
    (fun _ bot ->
      joined ~clock bot;
      let cache = Ui.Runtime.event_cache (Bot.runtime bot) in
      let base = Matrix_client.Base_client.create ~user_id:bot_user () in
      let _ =
        feed cache base
          (sync_body ~batch:"s2"
             [ text "!ping" ~id:"$a"; text "!nope x" ~id:"$b" ])
      in
      until ~clock "the unknown command" (fun () -> !unknown <> []));
  check_strings "only the command no handler claims" [ "nope" ] !unknown

(* A command registered under [in_rooms]/[from_users] is a real, known
   command, but its handler only runs inside its scope. Outside it, the
   handler is silently filtered out; [dispatch] must still recognise that as
   "no handler answered" and report it through [on_unknown_command], rather
   than treating the command's mere existence elsewhere as having handled
   it. *)
let test_scoped_command_out_of_scope_is_unknown () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let ran = ref 0 in
  let unknown = ref [] in
  let spec =
    Bot.v ~name:"scoped" ()
    |> Bot.in_rooms [ other_room ]
         (Bot.command ~name:"secret" (fun _ _ -> incr ran))
    |> Bot.on_unknown_command (fun _ c -> unknown := c.name :: !unknown)
  in
  run_bot ~env ~store
    ~script:[ sync_body ~batch:"s1" [] ]
    spec
    (fun _ bot ->
      joined ~clock bot;
      let cache = Ui.Runtime.event_cache (Bot.runtime bot) in
      let base = Matrix_client.Base_client.create ~user_id:bot_user () in
      let _ =
        feed cache base (sync_body ~batch:"s2" [ text "!secret" ~id:"$a" ])
      in
      until ~clock "the unknown command" (fun () -> !unknown <> []));
  check_int "the out-of-scope handler never ran" 0 !ran;
  check_strings "reported as unknown here, even though it exists elsewhere"
    [ "secret" ] !unknown

(* The cursor is what a restart reads: [`Skip] passes over what arrived while
   the bot was down and moves the cursor to it, [`Handle] replays it. Both
   runs share the state and a persistent event store, which is what lets the
   second one find the first one's cursor in the cache. *)
let test_backlog () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let events = Ui.Event_store.memory () in
  let first = sync_body ~batch:"s1" [ text "before" ~id:"$b1" ] in
  let second =
    sync_body ~batch:"s1" [ text "before" ~id:"$b1"; text "after" ~id:"$b2" ]
  in
  let run ~backlog script =
    let seen = ref [] in
    let spec =
      Bot.v ~name:"backlog" ~backlog ()
      |> Bot.on_message (fun _ message -> seen := message.content.body :: !seen)
    in
    run_bot ~env ~store ~event_store:events ~script spec (fun _ bot ->
        joined ~clock bot;
        Eio.Time.sleep clock 0.2);
    List.rev !seen
  in
  let cursor () =
    stored store ~room:room_id ~plugin:"matrix.bot/backlog" "cursor"
  in
  check_strings "the first run skips the history it starts with" []
    (run ~backlog:`Skip [ first ]);
  check_string "and the cursor is the newest event it passed over" "$b1"
    (Option.get (cursor ()));
  check_strings "a `Skip restart ignores what arrived while it was down" []
    (run ~backlog:`Skip [ second ]);
  check_string "moving the cursor on to it" "$b2" (Option.get (cursor ()));
  (* Rewound, so that the two restarts differ only in what [backlog] says
     to do with the same message. *)
  ok
    (Plugin_store.set store ~room:room_id ~plugin:"matrix.bot/backlog"
       ~key:"cursor" Matrix_proto.Json.Codec.string "$b1");
  check_strings "a `Handle restart replays what came after the cursor"
    [ "after" ]
    (run ~backlog:`Handle [ second ]);
  check_string "and moves the cursor past it" "$b2" (Option.get (cursor ()))

(** {1 Sends} *)

let queue_for user_id =
  Queue.create
    ~random:
      (Matrix_client.Random.of_source
         (Eio.Flow.string_source
            (String.init 4096 (fun index -> Char.chr (index * 37 mod 256)))))
    ~user_id ()

(* The promise resolves from the queue's own status change, and [await]
   answers [Timeout] rather than blocking for ever on a request the queue is
   still holding. *)
let test_sent () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let server = mock_server ~clock [] in
  let client = client_of ~sw ~env server in
  let queue = queue_for bot_user in
  let tracker = Sent.Internal.tracker ~sw ~clock queue in
  let pending =
    Sent.Internal.v tracker (Queue.send_text queue ~room_id ~body:"one")
  in
  check_bool "a queued send is queued" true (Sent.status pending = Sent.Queued);
  check_bool "and awaiting one the queue never sends times out" true
    (Sent.await ~timeout:0.05 pending = Sent.Timed_out);
  let request = Queue.send_text queue ~room_id ~body:"two" in
  let sent = Sent.Internal.v tracker request in
  let event_id = Id.Event_id.of_string_exn "$delivered:example.org" in
  let outcome =
    Queue.send_one queue
      ~send:(fun _ _ -> Ok event_id)
      (Matrix_eio.Client.base client)
      request
  in
  check_bool "the queue reports the send" true (outcome = Queue.Sent_ok event_id);
  check_bool "so the promise carries the event id" true
    (Sent.await ~timeout:1. sent = Sent.Sent event_id);
  check_bool "and the status agrees" true
    (Sent.status sent = Sent.Done (Sent.Sent event_id));
  (* A promise made after the fact still resolves: the status is read once
     the callback has been registered. *)
  let late = Sent.Internal.v tracker request in
  check_bool "a promise made after delivery resolves at once" true
    (Sent.await ~timeout:1. late = Sent.Sent event_id);
  let cancelled =
    Sent.Internal.v tracker (Queue.send_text queue ~room_id ~body:"three")
  in
  check_bool "cancelling an unsent request is accepted" true
    (Sent.cancel cancelled = `Cancelled);
  check_bool "and the promise says so" true
    (Sent.await ~timeout:1. cancelled = Sent.Cancelled)

let test_room_send_readiness () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let store = Plugin_store.memory () in
  let check script =
    let result = ref None in
    let spec =
      Bot.v ~name:"readiness" ()
      |> Bot.on_join (fun _ room ->
          result :=
            Some
              ( Room.ready_to_send room,
                Room.await_ready_to_send ~timeout:0. room ))
    in
    run_bot ~env ~store ~script:[ script ] spec (fun _ _ ->
        until ~clock "the readiness check" (fun () -> Option.is_some !result));
    Option.get !result
  in
  check_bool "a fully-synced plaintext room is ready immediately" true
    (check (sync_body ~batch:"ready-plain" []) = (true, true));
  let encryption =
    json_event ~state_key:"" ~id:"$encryption" ~type_:"m.room.encryption"
      {|{"algorithm":"m.megolm.v1.aes-sha2"}|}
  in
  let encrypted_sync =
    Printf.sprintf
      {|{"next_batch":"ready-encrypted","rooms":{"join":{"!room:example.org":{"state":{"events":[%s]},"timeline":{"events":[],"limited":false,"prev_batch":"p"}}}}}|}
      encryption
  in
  check_bool "an encrypted room without a machine is not ready" true
    (check encrypted_sync = (false, false))

(* The one-shot envelope returns the action's shell status and preserves its
    exception semantics; the context-owned helper is what keeps this test
    hermetic (the command-line entry point itself exits the process). *)
let test_main_one_shot_action () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = mock_server ~clock:(Eio.Stdenv.clock env) [] in
  let client = client_of ~sw ~env server in
  let ctx = Context.v ~env ~sw ~client () in
  check_int "the action's status is returned" 23
    (Main.run_once_with_context ctx (fun context ->
         check_bool "the action receives its context" true
           (Id.User_id.equal (Context.user_id context) bot_user);
         23));
  Alcotest.check_raises "an action exception is not swallowed"
    (Failure "one-shot failure") (fun () ->
      ignore
        (Main.run_once_with_context ctx (fun _ -> failwith "one-shot failure")))

let () =
  Alcotest.run "matrix.bot"
    [
      ( "plugin store",
        [
          Alcotest.test_case "in memory" `Quick test_store_memory;
          Alcotest.test_case "in a file" `Quick test_store_file;
          Alcotest.test_case "atomic file writes" `Quick
            test_store_file_atomic_writes;
        ] );
      ( "commands",
        [
          Alcotest.test_case "parsing" `Quick test_command_parse;
          Alcotest.test_case "spec construction" `Quick test_spec_commands;
        ] );
      ( "dispatch",
        [
          Alcotest.test_case "order, filters and failures" `Quick
            test_dispatcher;
          Alcotest.test_case "help and replies" `Quick test_help_and_reply;
          Alcotest.test_case "an unknown command" `Quick test_unknown_command;
          Alcotest.test_case "a scoped command out of scope is unknown" `Quick
            test_scoped_command_out_of_scope_is_unknown;
          Alcotest.test_case "the cursor across a restart" `Quick test_backlog;
        ] );
      ( "sends",
        [
          Alcotest.test_case "a promise for a send" `Quick test_sent;
          Alcotest.test_case "room readiness" `Quick test_room_send_readiness;
        ] );
      ( "main",
        [
          Alcotest.test_case "one-shot action" `Quick test_main_one_shot_action;
        ] );
    ]
