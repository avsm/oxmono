open Zulip_eio

let failf fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) @@ fun () ->
  really_input_string channel (in_channel_length channel)

let json path =
  match Jsont_bytesrw.decode_string Jsont.json (read_file path) with
  | Ok json -> json
  | Error error -> failf "cannot decode fixture %s: %s" path error

let members = function
  | Jsont.Object (members, _) -> members
  | _ -> Alcotest.fail "fixture value expected an object"

let field name json =
  match
    List.find_opt (fun ((key, _), _) -> String.equal key name) (members json)
  with
  | Some (_, value) -> value
  | None -> failf "fixture field %S is missing" name

let string name = function
  | Jsont.String (value, _) -> value
  | _ -> failf "fixture field %S expected a string" name

let int name = function
  | Jsont.Number (value, _) ->
      if Float.is_integer value then Int.of_float value
      else failf "fixture field %S expected an integer" name
  | _ -> failf "fixture field %S expected an integer" name

type principal = {
  id : Zulip.Id.User.t;
  email : string;
  api_key : string;
  full_name : string;
}

type channel = { id : Zulip.Id.Channel.t; name : string }

type fixture = {
  server : string;
  admin : principal;
  alice : principal;
  bob : principal;
  echo : principal;
  store : principal;
  public_channel : channel;
  private_channel : channel;
}

let principal json =
  {
    id = Zulip.Id.User.of_int (int "id" (field "id" json));
    email = string "delivery_email" (field "delivery_email" json);
    api_key = string "api_key" (field "api_key" json);
    full_name = string "full_name" (field "full_name" json);
  }

let channel json =
  {
    id = Zulip.Id.Channel.of_int (int "id" (field "id" json));
    name = string "name" (field "name" json);
  }

let fixture () =
  let path =
    match Sys.getenv_opt "ZULIP_TEST_FIXTURES" with
    | Some path when path <> "" -> path
    | _ ->
        Alcotest.fail
          "ZULIP_TEST_FIXTURES is required; run test/integration/zulip.sh up \
           first"
  in
  let json = json path in
  if int "schema" (field "schema" json) <> 1 then
    Alcotest.fail "unsupported integration fixture schema";
  let server =
    match Sys.getenv_opt "ZULIP_TEST_SERVER" with
    | Some server when server <> "" -> server
    | _ -> string "url" (field "url" (field "server" json))
  in
  let users = field "users" json in
  let bots = field "bots" users in
  let channels = field "channels" json in
  {
    server;
    admin = principal (field "admin" users);
    alice = principal (field "alice" users);
    bob = principal (field "bob" users);
    echo = principal (field "echo" bots);
    store = principal (field "store" bots);
    public_channel = channel (field "public" channels);
    private_channel = channel (field "private" channels);
  }

type clients = {
  fixture : fixture;
  admin : Client.t;
  alice : Client.t;
  bob : Client.t;
  echo : Client.t;
  store : Client.t;
  invalid : Client.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
}

let client env fixture principal =
  let transport = Transport.v env in
  let auth =
    Auth.create ~site:fixture.server ~email:principal.email
      ~api_key:principal.api_key
    |> ok
  in
  Client.create ~transport ~auth ~allow_insecure:true () |> ok

let with_clients fn =
  let fixture = fixture () in
  Eio_main.run @@ fun env ->
  let clients =
    {
      fixture;
      admin = client env fixture fixture.admin;
      alice = client env fixture fixture.alice;
      bob = client env fixture fixture.bob;
      echo = client env fixture fixture.echo;
      store = client env fixture fixture.store;
      invalid =
        client env fixture
          { fixture.admin with api_key = "invalid-fixture-key" };
      clock = env#clock;
    }
  in
  fn clients

let nonce () =
  Printf.sprintf "ocaml-zulip-integration-%d-%.0f" (Unix.getpid ())
    (Unix.gettimeofday () *. 1000.)

let check_user (expected : principal) user =
  Alcotest.(check int)
    "authenticated user id"
    (Zulip.Id.User.to_int expected.id)
    (Zulip.Id.User.to_int (Zulip.User.user_id user));
  Alcotest.(check string)
    "authenticated delivery email" expected.email
    (Option.value ~default:(Zulip.User.email user)
       (Zulip.User.delivery_email user))

let test_server_and_users () =
  with_clients @@ fun t ->
  let settings = Server.get_settings t.admin |> ok in
  Alcotest.(check bool)
    "server reports a feature level" true
    (settings.zulip_feature_level > 0);
  check_user t.fixture.admin (Users.me t.admin |> ok);
  check_user t.fixture.alice (Users.me t.alice |> ok);
  check_user t.fixture.bob (Users.me t.bob |> ok);
  Alcotest.(check int)
    "fixture public channel resolves"
    (Zulip.Id.Channel.to_int t.fixture.public_channel.id)
    (Zulip.Id.Channel.to_int
       (Channels.get_id t.admin ~name:t.fixture.public_channel.name |> ok));
  Alcotest.(check bool)
    "all fixture users listed" true
    (List.length (Users.list t.admin |> ok) >= 5)

let test_invalid_credentials () =
  with_clients @@ fun t ->
  match Users.me t.invalid with
  | Error (Error.Api { status = 401 | 403; _ }) -> ()
  | Error error ->
      failf "invalid key returned the wrong error: %s"
        (Error.error_to_string error)
  | Ok _ -> Alcotest.fail "an invalid API key was authenticated"

let test_channel_send_and_get () =
  with_clients @@ fun t ->
  let marker = nonce () in
  let content = "channel payload " ^ marker ^ " **markdown**" in
  let topic = "integration " ^ marker in
  let message_id =
    Messages.send_channel t.alice ~channel:t.fixture.public_channel.name ~topic
      ~content ()
    |> ok
  in
  Alcotest.(check string)
    "raw channel content" content
    (Messages.get_raw t.admin ~message_id () |> ok);
  let message = Messages.get t.admin ~message_id |> ok in
  Alcotest.(check int)
    "returned message id"
    (Zulip.Id.Message.to_int message_id)
    (Zulip.Id.Message.to_int (Zulip.Message.id message));
  (match Zulip.Message.destination message with
  | Channel { channel_id; channel_name; topic = returned_topic } ->
      Alcotest.(check int)
        "channel id"
        (Zulip.Id.Channel.to_int t.fixture.public_channel.id)
        (Zulip.Id.Channel.to_int channel_id);
      Alcotest.(check string)
        "channel name" t.fixture.public_channel.name channel_name;
      Alcotest.(check string) "topic" topic returned_topic
  | Direct _ -> Alcotest.fail "channel message was decoded as a direct message");
  let page =
    Messages.get_messages t.admin ~anchor:(Messages.Message_id message_id)
      ~num_before:0 ~num_after:0 ~include_anchor:true ()
    |> ok
  in
  Alcotest.(check bool) "anchored message returned" true page.found_anchor;
  Alcotest.(check bool)
    "anchored message has one result" true
    (List.exists
       (fun message ->
         Zulip.Id.Message.equal (Zulip.Message.id message) message_id)
       page.messages)

let test_group_direct_message () =
  with_clients @@ fun t ->
  let content = "group direct payload " ^ nonce () in
  let message_id =
    Messages.send_direct t.alice
      ~recipients:[ t.fixture.bob.id; t.fixture.echo.id ]
      ~content ()
    |> ok
  in
  Alcotest.(check string)
    "group direct raw content" content
    (Messages.get_raw t.bob ~message_id () |> ok);
  match Zulip.Message.destination (Messages.get t.bob ~message_id |> ok) with
  | Direct { participants; _ } ->
      List.iter
        (fun expected ->
          Alcotest.(check bool)
            "group direct participant" true
            (List.exists (Zulip.Id.User.equal expected) participants))
        [ t.fixture.alice.id; t.fixture.bob.id; t.fixture.echo.id ]
  | Channel _ ->
      Alcotest.fail "group direct message was decoded as a channel message"

let test_private_subscription_boundary () =
  with_clients @@ fun t ->
  let channel = t.fixture.private_channel in
  let topic = "private-boundary" in
  let message_id =
    Messages.send_channel t.admin ~channel:channel.name ~topic
      ~content:("private fixture " ^ nonce ())
      ()
    |> ok
  in
  (match Messages.get_raw t.bob ~message_id () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unsubscribed Bob read a private-channel message");
  (match
     Messages.send_channel t.bob ~channel:channel.name ~topic ~content:"denied"
       ()
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unsubscribed Bob sent to a private channel");
  let subscribed = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !subscribed then
        ignore
          (Channels.unsubscribe t.admin ~subscriptions:[ channel.name ]
             ~principals:(`User_ids [ t.fixture.bob.id ]) ()))
    (fun () ->
      Channels.subscribe t.admin
        ~subscriptions:
          [ { name = channel.name; color = None; description = None } ]
        ~principals:(`User_ids [ t.fixture.bob.id ]) ()
      |> ok |> ignore;
      subscribed := true;
      Alcotest.(check bool)
        "Bob subscribed to private fixture" true
        (Channels.get_subscription_status t.admin ~user_id:t.fixture.bob.id
           ~channel_id:channel.id
        |> ok);
      let visible_after_subscription =
        Messages.send_channel t.admin ~channel:channel.name ~topic
          ~content:("private after subscribe " ^ nonce ())
          ()
        |> ok
      in
      ignore
        (Messages.get_raw t.bob ~message_id:visible_after_subscription () |> ok);
      let posted =
        Messages.send_channel t.bob ~channel:channel.name ~topic
          ~content:("private post " ^ nonce ())
          ()
        |> ok
      in
      ignore (Messages.get_raw t.admin ~message_id:posted () |> ok);
      Channels.unsubscribe t.admin ~subscriptions:[ channel.name ]
        ~principals:(`User_ids [ t.fixture.bob.id ]) ()
      |> ok |> ignore;
      subscribed := false;
      Alcotest.(check bool)
        "Bob unsubscribed from private fixture" false
        (Channels.get_subscription_status t.admin ~user_id:t.fixture.bob.id
           ~channel_id:channel.id
        |> ok))

let test_render_flags_and_narrow () =
  with_clients @@ fun t ->
  let content = "narrow payload " ^ nonce () in
  let topic =
    Printf.sprintf "narrow-%d-%.0f" (Unix.getpid ())
      (Unix.gettimeofday () *. 1000.)
  in
  let rendered = Messages.render t.alice ~content:"**rendered**" |> ok in
  Alcotest.(check bool)
    "rendered HTML" true
    (String.starts_with ~prefix:"<p>" rendered);
  let message_id =
    Messages.send_channel t.alice ~channel:t.fixture.public_channel.name ~topic
      ~content ()
    |> ok
  in
  Messages.update_flags t.alice ~messages:[ message_id ] ~op:Add ~flag:`Starred
  |> ok;
  Alcotest.(check bool)
    "starred flag is returned" true
    (List.mem `Starred
       (Zulip.Message.flags (Messages.get t.alice ~message_id |> ok)));
  let matching =
    Messages.check_messages_match_narrow t.alice ~message_ids:[ message_id ]
      ~narrow:
        [
          Zulip.Narrow.stream t.fixture.public_channel.name;
          Zulip.Narrow.topic topic;
        ]
    |> ok
  in
  Alcotest.(check bool)
    "matches_narrow includes sent message" true
    (List.exists
       (fun (matched : Messages.narrow_match) ->
         Zulip.Id.Message.equal matched.message_id message_id)
       matching)

let one_pixel_gif =
  "GIF89a\001\000\001\000\128\000\000\255\255\255\000\000\000!\249\004\001\000\000\000\000,\000\000\000\000\001\000\001\000\000\002\002D\001\000;"

let test_custom_emoji () =
  with_clients @@ fun t ->
  let name = "ocaml" ^ string_of_int (Unix.getpid ()) in
  Server.upload_emoji t.admin ~name ~filename:"pixel.gif"
    ~content_type:"image/gif" one_pixel_gif
  |> ok;
  Fun.protect
    ~finally:(fun () -> ignore (Server.deactivate_emoji t.admin ~name))
    (fun () ->
      let emoji = Server.get_emoji t.admin |> ok in
      Alcotest.(check bool)
        "custom emoji is listed by name" true
        (List.exists
           (fun (emoji : Server.emoji) ->
             String.equal emoji.name name && not emoji.deactivated)
           emoji);
      Server.deactivate_emoji t.admin ~name |> ok;
      Alcotest.(check bool)
        "deactivated emoji remains listed" true
        (List.exists
           (fun (emoji : Server.emoji) ->
             String.equal emoji.name name && emoji.deactivated)
           (Server.get_emoji t.admin |> ok)))

let test_upload_and_download () =
  with_clients @@ fun t ->
  let payload = "upload payload " ^ nonce () ^ "\nwith a second line\n" in
  let uri =
    Attachments.upload_file t.alice ~filename:"integration.txt"
      ~content_type:"text/plain" payload
    |> ok
  in
  Alcotest.(check bool) "upload returned a URI" true (String.length uri > 0);
  let received = Buffer.create (String.length payload) in
  Client.download t.alice ~url:uri (Eio.Flow.buffer_sink received) |> ok;
  Alcotest.(check string)
    "downloaded upload bytes" payload (Buffer.contents received)

let test_bot_storage () =
  with_clients @@ fun t ->
  let key = "integration-" ^ nonce () in
  let value = "state " ^ nonce () in
  Bot_storage.set t.store [ (key, value) ] |> ok;
  Alcotest.(check (option string))
    "stored bot state" (Some value)
    (List.assoc_opt key (Bot_storage.get t.store ~keys:[ key ] () |> ok));
  Bot_storage.remove t.store ~keys:[ key ] () |> ok;
  Alcotest.(check (option string))
    "removed bot state" None
    (List.assoc_opt key (Bot_storage.get t.store () |> ok))

let rec poll_for_message t queue attempts =
  let events = Event_queue.get_events queue t.bob ~dont_block:true () |> ok in
  if
    List.exists
      (fun event -> Zulip.Event.type_ event = Zulip.Event_type.Message)
      (Event_queue.Batch.events events)
  then events
  else if attempts = 0 then
    Alcotest.fail "message event did not arrive before polling deadline"
  else (
    Eio.Time.sleep t.clock 0.1;
    poll_for_message t queue (attempts - 1))

let test_queue_and_invalidation () =
  with_clients @@ fun t ->
  let queue =
    Event_queue.register t.bob
      ~event_types:[ Zulip.Event_type.Message ]
      ~narrow:[ Event_queue.Narrow.channel t.fixture.public_channel.name ]
      ()
    |> ok
  in
  ignore
    (Messages.send_channel t.alice ~channel:t.fixture.public_channel.name
       ~topic:("events " ^ nonce ())
       ~content:("event payload " ^ nonce ())
       ()
    |> ok);
  let events = poll_for_message t queue 30 in
  Alcotest.(check bool)
    "received a message event" true
    (List.exists
       (fun event -> Zulip.Event.type_ event = Zulip.Event_type.Message)
       (Event_queue.Batch.events events));
  Event_queue.ack queue events |> ok;
  Event_queue.delete queue t.bob |> ok;
  match Event_queue.get_events queue t.bob ~dont_block:true () with
  | Error error when Error.is_bad_queue error -> ()
  | Error error ->
      failf "deleted queue returned the wrong error: %s"
        (Error.error_to_string error)
  | Ok _ -> Alcotest.fail "deleted queue was accepted by Zulip"

let within t seconds description action =
  try Eio.Time.with_timeout_exn t.clock seconds action
  with Eio.Time.Timeout ->
    failf "%s did not complete within %.0f seconds" description seconds

let with_echo_bot t ~all_messages action =
  Eio.Switch.run @@ fun sw ->
  let identity : Zulip_bot.Context.identity =
    {
      user_id = t.fixture.echo.id;
      email = t.fixture.echo.email;
      full_name = t.fixture.echo.full_name;
    }
  in
  let is_bot id =
    Zulip.Id.User.equal id t.fixture.echo.id
    || Zulip.Id.User.equal id t.fixture.store.id
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:t.echo ~identity ~clock:t.clock ~is_bot ()
  in
  let live, wake_live = Eio.Promise.create () in
  let reply, wake_reply = Eio.Promise.create () in
  let running = ref None in
  let replied = ref false in
  let spec =
    Zulip_bot.Bot.v ~all_messages ~workers:1 ()
    |> Zulip_bot.Bot.on_sync (fun _ -> function
      | Zulip_bot.Event.Live -> Eio.Promise.resolve wake_live ()
      | Connecting | Recovering _ | Stopped -> ())
    |> Zulip_bot.Bot.on_message (fun bot message ->
        if not !replied then (
          replied := true;
          let sent =
            Zulip_bot.Event.reply message.envelope ("echo: " ^ message.body)
          in
          Eio.Promise.resolve wake_reply sent;
          Zulip_bot.Bot.stop bot))
  in
  Eio.Fiber.fork ~sw (fun () ->
      Zulip_bot.Bot.run ~on_start:(fun bot -> running := Some bot) context spec);
  Fun.protect
    ~finally:(fun () -> Option.iter Zulip_bot.Bot.stop !running)
    (fun () ->
      within t 15. "bot queue registration" (fun () -> Eio.Promise.await live);
      action (fun () ->
          within t 15. "bot reply" (fun () -> Eio.Promise.await reply)))

let await_sent t sent =
  match Zulip_bot.Sent.await ~timeout:15. sent with
  | `Done (Sent _) -> ()
  | `Done (Failed error) ->
      failf "bot response failed: %s" (Error.error_to_string error)
  | `Done (Indeterminate (Some error)) ->
      failf "bot response was indeterminate: %s" (Error.error_to_string error)
  | `Done (Indeterminate None) ->
      Alcotest.fail "bot response was interrupted before completion"
  | `Done Cancelled -> Alcotest.fail "bot response was cancelled"
  | `Timed_out ->
      Alcotest.fail "bot response did not complete before its deadline"

let test_bot_one_to_one_echo () =
  with_clients @@ fun t ->
  let marker = nonce () in
  with_echo_bot t ~all_messages:false @@ fun await_reply ->
  ignore
    (Messages.send_direct t.alice ~recipients:[ t.fixture.echo.id ]
       ~content:marker ()
    |> ok);
  let sent = await_reply () in
  await_sent t sent;
  let reply_id =
    match Zulip_bot.Sent.await ~timeout:0. sent with
    | `Done (Sent id) -> id
    | _ -> assert false
  in
  Alcotest.(check string)
    "one-to-one echo content" ("echo: " ^ marker)
    (Messages.get_raw t.alice ~message_id:reply_id () |> ok)

let test_bot_group_direct_reply () =
  with_clients @@ fun t ->
  let marker = nonce () in
  with_echo_bot t ~all_messages:true @@ fun await_reply ->
  ignore
    (Messages.send_direct t.alice
       ~recipients:[ t.fixture.bob.id; t.fixture.echo.id ]
       ~content:marker ()
    |> ok);
  let sent = await_reply () in
  await_sent t sent;
  let reply_id =
    match Zulip_bot.Sent.await ~timeout:0. sent with
    | `Done (Sent id) -> id
    | _ -> assert false
  in
  Alcotest.(check string)
    "group echo content" ("echo: " ^ marker)
    (Messages.get_raw t.alice ~message_id:reply_id () |> ok);
  match
    Zulip.Message.destination (Messages.get t.alice ~message_id:reply_id |> ok)
  with
  | Direct { participants; _ } ->
      List.iter
        (fun expected ->
          Alcotest.(check bool)
            "group reply participant" true
            (List.exists (Zulip.Id.User.equal expected) participants))
        [ t.fixture.alice.id; t.fixture.bob.id; t.fixture.echo.id ]
  | Channel _ -> Alcotest.fail "bot group reply was not a direct message"

let test_bot_mentioned_channel_reply () =
  with_clients @@ fun t ->
  let marker = nonce () in
  let topic = "bot topic " ^ marker in
  let mention = "@**" ^ t.fixture.echo.full_name ^ "** " ^ marker in
  with_echo_bot t ~all_messages:false @@ fun await_reply ->
  ignore
    (Messages.send_channel t.alice ~channel:t.fixture.public_channel.name ~topic
       ~content:mention ()
    |> ok);
  let sent = await_reply () in
  await_sent t sent;
  let reply_id =
    match Zulip_bot.Sent.await ~timeout:0. sent with
    | `Done (Sent id) -> id
    | _ -> assert false
  in
  Alcotest.(check string)
    "mentioned channel echo content" ("echo: " ^ marker)
    (Messages.get_raw t.alice ~message_id:reply_id () |> ok);
  match
    Zulip.Message.destination (Messages.get t.alice ~message_id:reply_id |> ok)
  with
  | Channel { channel_id; topic = reply_topic; _ } ->
      Alcotest.(check int)
        "bot reply channel"
        (Zulip.Id.Channel.to_int t.fixture.public_channel.id)
        (Zulip.Id.Channel.to_int channel_id);
      Alcotest.(check string) "bot preserves topic" topic reply_topic
  | Direct _ -> Alcotest.fail "mentioned channel reply was direct"

let rec poll_for_event t client queue expected attempts =
  let events = Event_queue.get_events queue client ~dont_block:true () |> ok in
  Event_queue.ack queue events |> ok;
  match
    List.find_opt
      (fun event -> Zulip.Event.type_ event = expected)
      (Event_queue.Batch.events events)
  with
  | Some event -> event
  | None when attempts = 0 ->
      failf "event %s did not arrive before polling deadline"
        (Zulip.Event_type.to_string expected)
  | None ->
      Eio.Time.sleep t.clock 0.1;
      poll_for_event t client queue expected (attempts - 1)

let describe_bot_event event = Format.asprintf "%a" Zulip_bot.Event.pp event

let test_event_projection () =
  with_clients @@ fun t ->
  Eio.Switch.run @@ fun sw ->
  let identity : Zulip_bot.Context.identity =
    {
      user_id = t.fixture.echo.id;
      email = t.fixture.echo.email;
      full_name = t.fixture.echo.full_name;
    }
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:t.echo ~identity ~clock:t.clock ()
  in
  let queue =
    Event_queue.register t.admin
      ~event_types:
        [
          Zulip.Event_type.Message;
          Zulip.Event_type.Update_message;
          Zulip.Event_type.Reaction;
          Zulip.Event_type.Delete_message;
        ]
      ~narrow:[ Event_queue.Narrow.channel t.fixture.public_channel.name ]
      ()
    |> ok
  in
  (* Zulip limits topic names to 60 characters; retain a unique value below
     that bound so the assertion detects projection changes rather than server
     normalization. *)
  let topic =
    Printf.sprintf "event-%d-%.0f" (Unix.getpid ())
      (Unix.gettimeofday () *. 1000.)
  in
  let content = "event projection payload " ^ nonce () in
  let message_id =
    Messages.send_channel t.alice ~channel:t.fixture.public_channel.name ~topic
      ~content ()
    |> ok
  in
  let message = poll_for_event t t.admin queue Zulip.Event_type.Message 30 in
  (match Zulip_bot.Event.of_zulip context message with
  | Some (Zulip_bot.Event.Message event) ->
      Alcotest.(check int)
        "projected message sender"
        (Zulip.Id.User.to_int t.fixture.alice.id)
        (Zulip.Id.User.to_int event.envelope.sender);
      (match Zulip_bot.Room.id event.envelope.room with
      | Zulip_bot.Room.Channel channel_id ->
          Alcotest.(check int)
            "projected message channel"
            (Zulip.Id.Channel.to_int t.fixture.public_channel.id)
            (Zulip.Id.Channel.to_int channel_id)
      | Direct _ -> Alcotest.fail "stream event projected as direct room");
      Alcotest.(check string)
        "projected message topic" topic
        (Option.value ~default:"" (Zulip_bot.Room.topic event.envelope.room))
  | Some other ->
      failf "message event projected as %s" (describe_bot_event other)
  | None -> Alcotest.fail "message event was discarded");
  Messages.edit t.alice ~message_id ~content:(content ^ " edited") () |> ok;
  let edited =
    poll_for_event t t.admin queue Zulip.Event_type.Update_message 30
  in
  (match Zulip_bot.Event.of_zulip context edited with
  | Some (Zulip_bot.Event.Edit event) ->
      Alcotest.(check int)
        "edit actor"
        (Zulip.Id.User.to_int t.fixture.alice.id)
        (Zulip.Id.User.to_int event.envelope.sender);
      Alcotest.(check (option int))
        "edit message id"
        (Some (Zulip.Id.Message.to_int message_id))
        (Some (Zulip.Id.Message.to_int event.message_id))
  | Some other ->
      failf "update_message projected as %s" (describe_bot_event other)
  | None -> Alcotest.fail "update_message event was discarded");
  Messages.add_reaction t.admin ~message_id ~emoji_name:"thumbs_up" () |> ok;
  let reaction = poll_for_event t t.admin queue Zulip.Event_type.Reaction 30 in
  (match Zulip_bot.Event.of_zulip context reaction with
  | Some (Zulip_bot.Event.Reaction event) ->
      Alcotest.(check int)
        "reaction actor"
        (Zulip.Id.User.to_int t.fixture.admin.id)
        (Zulip.Id.User.to_int event.envelope.sender);
      Alcotest.(check (option int))
        "reaction message id"
        (Some (Zulip.Id.Message.to_int message_id))
        (Some (Zulip.Id.Message.to_int event.message_id))
  | Some other -> failf "reaction projected as %s" (describe_bot_event other)
  | None -> Alcotest.fail "reaction event was discarded");
  Messages.delete t.alice ~message_id |> ok;
  let deleted =
    poll_for_event t t.admin queue Zulip.Event_type.Delete_message 30
  in
  (match Zulip_bot.Event.of_zulip context deleted with
  | Some (Zulip_bot.Event.Delete event) ->
      Alcotest.(check (list int))
        "delete message IDs"
        [ Zulip.Id.Message.to_int message_id ]
        (List.map Zulip.Id.Message.to_int event.message_ids);
      Alcotest.(check (option bool))
        "actorless delete retains cached room" (Some true)
        (Option.map
           (fun room -> Zulip_bot.Room.is_direct room = false)
           event.room)
  | Some other ->
      failf "delete_message projected as %s" (describe_bot_event other)
  | None -> Alcotest.fail "delete_message event was discarded");
  Event_queue.delete queue t.admin |> ok

let test_message_parity () =
  with_clients (fun t ->
      Messages_scenarios.run ~client:t.admin
        ~channel:t.fixture.public_channel.id ~recipient:t.fixture.alice.id)

let test_channel_parity () =
  with_clients (fun t ->
      Channels_scenarios.run ~client:t.admin ~user:t.fixture.alice.id)

let test_account_parity () =
  with_clients (fun t ->
      Accounts_scenarios.run ~client:t.admin ~user:t.fixture.alice.id)

let test_restart () =
  with_clients (fun t ->
      Restart_scenarios.run ~client:t.admin ~sender:t.alice ~clock:t.clock
        ~channel:t.fixture.public_channel.id)

let test_saved_snippets () =
  with_clients (fun t -> Snippets_scenarios.run ~client:t.echo)

let () =
  Alcotest.run "zulip live integration"
    [
      ( "expanded APIs",
        [
          Alcotest.test_case "messages schedules drafts files" `Slow
            test_message_parity;
          Alcotest.test_case "channels folders groups permissions" `Slow
            test_channel_parity;
          Alcotest.test_case "profiles settings status presence" `Slow
            test_account_parity;
          Alcotest.test_case "bot saved snippets" `Slow test_saved_snippets;
        ] );
      ( "server and fixture users",
        [
          Alcotest.test_case "authenticate and discover" `Slow
            test_server_and_users;
        ] );
      ( "authentication",
        [
          Alcotest.test_case "invalid API key is rejected" `Slow
            test_invalid_credentials;
        ] );
      ( "messages",
        [
          Alcotest.test_case "channel send and get" `Slow
            test_channel_send_and_get;
          Alcotest.test_case "group direct message" `Slow
            test_group_direct_message;
          Alcotest.test_case "private subscription boundary" `Slow
            test_private_subscription_boundary;
          Alcotest.test_case "render flags and matches narrow" `Slow
            test_render_flags_and_narrow;
        ] );
      ( "uploads",
        [
          Alcotest.test_case "upload then download" `Slow
            test_upload_and_download;
        ] );
      ( "realm emoji",
        [ Alcotest.test_case "upload list deactivate" `Slow test_custom_emoji ]
      );
      ( "bot storage",
        [ Alcotest.test_case "set get remove" `Slow test_bot_storage ] );
      ( "event queue",
        [
          Alcotest.test_case "restart and queue recovery" `Slow test_restart;
          Alcotest.test_case "delivery and invalidation" `Slow
            test_queue_and_invalidation;
        ] );
      ( "event projection",
        [
          Alcotest.test_case "message edit reaction delete" `Slow
            test_event_projection;
        ] );
      ( "bot runtime",
        [
          Alcotest.test_case "one-to-one echo" `Slow test_bot_one_to_one_echo;
          Alcotest.test_case "group direct reply" `Slow
            test_bot_group_direct_reply;
          Alcotest.test_case "mentioned channel reply" `Slow
            test_bot_mentioned_channel_reply;
        ] );
    ]
