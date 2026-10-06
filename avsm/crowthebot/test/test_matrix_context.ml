open Crowthebot

module Id = Matrix_proto.Id
module Bot = Matrix_bot.Bot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let self = "@crow:example.org"
let room = "!room:example.org"
let encode ty value = Result.get_ok (Jsont_bytesrw.encode_string ty value)

let field name ty text =
  let codec =
    Jsont.Object.map Fun.id
    |> Jsont.Object.mem name ty ~enc:Fun.id
    |> Jsont.Object.finish
  in
  Result.get_ok (Jsont_bytesrw.decode_string codec text)

let message ?(sender = admin) id content =
  Printf.sprintf
    {|{"type":"m.room.message","event_id":%S,"sender":%S,"origin_server_ts":1,"content":%s}|}
    id sender content

let sync events =
  Printf.sprintf
    {|{"next_batch":"next","rooms":{"join":{"!room:example.org":{"timeline":{"events":[%s],"limited":false}}}}}|}
    (String.concat "," events)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  Store.add_room store room;
  let state = ref None in
  let sent = ref [] in
  let matrix =
    Matrix_rooms.create ~store ~self
      ~state:(fun () -> !state |> Option.map (fun read -> read ()))
      ~send:(fun () ->
        Some
          (fun id text ->
            sent := (Id.Room_id.to_string id, text) :: !sent;
            Ok "$sent"))
      ()
  in
  let query ?(actor = admin) name args =
    Matrix_rooms.invoke matrix ~actor ~room name args
  in
  check "tools require live state" (Result.is_error (query "matrix_rooms" "{}"));
  let responses = ref [] and targets = ref [] and accepted = ref 0 in
  let rounds = ref 0 in
  let complete _ tools =
    incr rounds;
    if !rounds mod 2 = 0 then (Some "Reply", [])
    else begin
      check "Matrix tools exposed to approved model requests"
        (List.length tools = 12);
      ( None,
        [
          Agentkit.Agent.
            { id = "room-info"; name = "matrix_room_info"; arguments = "{}" };
        ] )
    end
  in
  let engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
      ~store ~self ~plugins:[] ~complete:(Fake_model.v complete) ~now:(fun () -> 0.)
    |> fun engine -> Engine.with_matrix engine matrix
  in
  let input _ ({ message; original } : Matrix_input.t) =
    if message.content.kind = Matrix_ui.Presentation.Text then begin
      let e = message.envelope in
      Engine.handle engine
        ~mentioned:(Address.mentions ~self message.presentation.raw.content)
        ~on_accept:(fun () -> incr accepted)
        ~send:(fun text ->
          responses := text :: !responses;
          targets :=
            Option.value ~default:e.event_id original |> Id.Event_id.to_string
            |> fun id -> id :: !targets)
        Engine.
          {
            room;
            sender = Id.User_id.to_string e.sender;
            id = Id.Event_id.to_string e.event_id;
            body =
              Address.body ~reply:(message.reply_to <> None)
                message.content.body;
          }
    end
  in
  let rooms =
    List.init 7 (fun i ->
        Printf.sprintf
          {|"!r%d:example.org":{"state":{"events":[{"type":"m.room.name","state_key":"","content":{"name":"Room %d"}}]},"timeline":{"events":[],"limited":false}}|}
          i i)
  in
  let initial =
    Printf.sprintf
      {|{"next_batch":"initial","rooms":{"join":{%s,"!room:example.org":{"state":{"events":[{"type":"m.room.topic","state_key":"","content":{"topic":"Robot workshop"}},{"type":"m.room.member","state_key":"@admin:example.org","content":{"membership":"join"}},{"type":"m.room.member","state_key":"@crow:example.org","content":{"membership":"join"}}]},"timeline":{"events":[],"limited":false}},"!dm:example.org":{"state":{"events":[{"type":"m.room.member","state_key":"@admin:example.org","content":{"membership":"join"}},{"type":"m.room.member","state_key":"@crow:example.org","content":{"membership":"join"}}]},"timeline":{"events":[],"limited":false}}}}}|}
      (String.concat "," rooms)
  in
  let pending = ref [ initial ] in
  let uploads = ref [] and events = ref [] in
  let contains text part =
    let rec loop i =
      i + String.length part <= String.length text
      && (String.sub text i (String.length part) = part || loop (i + 1))
    in
    loop 0
  in
  let fetch =
    Fetch_mock.client (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        let body =
          match request.body with Fetch.String s -> s | _ -> ""
        in
        if String.ends_with ~suffix:"/versions" url then
          Fetch_mock.respond {|{"versions":["v1.11"]}|} request
        else if contains url "/upload" then begin
          uploads := body :: !uploads;
          Fetch_mock.respond {|{"content_uri":"mxc://example.org/voice"}|}
            request
        end
        else if contains url "/send/m.room.message/" then begin
          events := body :: !events;
          Fetch_mock.respond {|{"event_id":"$audio"}|} request
        end
        else
          match !pending with
          | next :: rest ->
              pending := rest;
              Fetch_mock.respond next request
          | [] ->
              Eio.Time.Mono.sleep (Eio.Stdenv.mono_clock env) 0.01;
              Fetch_mock.respond {|{"next_batch":"idle"}|} request)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn "https://matrix.example.org")
      ~fetch ()
    |> fun client ->
    Matrix_eio.Client.with_session client
      {
        user_id = Id.User_id.of_string_exn self;
        device_id = Id.Device_id.of_string_exn "BOT";
        access_token = "fixture";
        refresh_token = None;
      }
  in
  let ctx =
    Matrix_bot.Context.v ~env ~sw ~client
      ~plugin_store:(Matrix_bot.Plugin_store.memory ())
      ()
  in
  let clock = Eio.Stdenv.mono_clock env in
  let until condition =
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 10.) (fun () ->
        while not (condition ()) do
          Eio.Time.Mono.sleep clock 0.005
        done)
  in
  Bot.run ctx
    (Bot.v ~name:"crowthebot" ~auto_join:false () |> Matrix_input.register input)
    ~on_start:(fun bot ->
      Fun.protect
        ~finally:(fun () -> Bot.stop bot)
        (fun () ->
          state :=
            Some
              (fun () ->
                Matrix_eio.Sync_service.state
                  (Matrix_ui.Runtime.sync_service (Bot.runtime bot)));
          until (fun () -> List.length (Bot.rooms bot) = 9);
          let first = Result.get_ok (query "matrix_rooms" "{}") in
          check "room list paginated"
            (List.length (field "rooms" (Jsont.list Jsont.json) first) = 5);
          let cursor = field "next_after" Jsont.string first in
          let last =
            Result.get_ok
              (query "matrix_rooms"
                 ("{\"after\":" ^ encode Jsont.string cursor ^ "}"))
          in
          check "room list ends explicitly"
            (List.length (field "rooms" (Jsont.list Jsont.json) last) = 4
            && field "next_after" (Jsont.option Jsont.string) last = None);
          let info = Result.get_ok (query "matrix_room_info" "{}") in
          check "live topic and current room"
            (field "topic" Jsont.string info = "Robot workshop"
            && field "current" Jsont.bool info);
          check "membership metadata" (field "known_members" Jsont.int info = 2);
          check "nonjoined room refused"
            (Result.is_error
               (query "matrix_room_info" {|{"room":"!absent:example.org"}|}));
          check "stranger refused"
            (Result.is_error
               (query ~actor:"@guest:example.org" "matrix_rooms" "{}"));
          Store.set_person store ~actor:admin ~user:"@guest:example.org"
            ~role:Friend ~allowed:true;
          check "friend can query"
            (Result.is_ok
               (query ~actor:"@guest:example.org" "matrix_rooms" "{}"));
          Store.set_person store ~actor:admin ~user:"@guest:example.org"
            ~role:Friend ~allowed:false;
          check "revocation takes effect"
            (Result.is_error
               (query ~actor:"@guest:example.org" "matrix_rooms" "{}"));
          let post ?actor args = query ?actor "matrix_send" args in
          check "requester posts to a room they belong to"
            (Result.is_ok
               (post {|{"room":"!room:example.org","text":"Read **this**."}|})
            && (match !sent with
               | [ ("!room:example.org", text) ] ->
                   text = "Read **this**."
               | _ -> false));
          Store.set_person store ~actor:admin ~user:"@guest:example.org"
            ~role:Friend ~allowed:true;
          check "friends cannot post into rooms they are not in"
            (Result.is_error
               (post ~actor:"@guest:example.org"
                  {|{"room":"!room:example.org","text":"hi"}|}));
          check "unjoined rooms refused"
            (Result.is_error
               (post {|{"room":"!absent:example.org","text":"hi"}|}));
          Store.add_direct_room store ~room:"!dm:example.org" ~peer:admin;
          check "a friend can DM the admin through an existing DM"
            (Result.is_ok
               (post ~actor:"@guest:example.org"
                  {|{"user":"@admin:example.org","text":"Paper for you."}|})
            && fst (List.hd !sent) = "!dm:example.org");
          List.iter
            (fun (label, args) ->
              check label (Result.is_error (post args)))
            [
              ( "DMs only go to approved people",
                {|{"user":"@stranger:example.org","text":"hi"}|} );
              ( "DMs need an existing DM room",
                {|{"user":"@guest:example.org","text":"hi"}|} );
              ( "exactly one target",
                {|{"room":"!room:example.org","user":"@admin:example.org","text":"hi"}|}
              );
              ("blank text refused", {|{"room":"!room:example.org","text":" "}|});
            ];
          check "refused posts send nothing" (List.length !sent = 2);
          Store.set_person store ~actor:admin ~user:"@guest:example.org"
            ~role:Friend ~allowed:false;
          check "revoked friends cannot post"
            (Result.is_error
               (post ~actor:"@guest:example.org"
                  {|{"user":"@admin:example.org","text":"hi"}|}));
          check "voice notes are offered only when Crow can speak"
            (not
               (List.mem "matrix_voice_note"
                  (List.map Agentkit.Agent.Tool.name
                     (Matrix_rooms.tools matrix))));
          let spoken = ref [] in
          let speaking =
            Matrix_rooms.create ~store ~self
              ~state:(fun () -> !state |> Option.map (fun read -> read ()))
              ~speak:(fun () ->
                Some
                  (fun id text ->
                    spoken := (Id.Room_id.to_string id, text) :: !spoken;
                    Ok "$voice"))
              ()
          in
          let speak ?(actor = admin) args =
            Matrix_rooms.invoke speaking ~actor ~room "matrix_voice_note" args
          in
          check "voice notes are offered when Crow can speak"
            (List.mem "matrix_voice_note"
               (List.map Agentkit.Agent.Tool.name
                  (Matrix_rooms.tools speaking)));
          check "a voice note goes to the requesting room by default"
            (Result.is_ok (speak {|{"text":"Tea at nine."}|})
            && !spoken = [ (room, "Tea at nine.") ]);
          check "a voice note can go to an existing DM"
            (Result.is_ok
               (speak {|{"user":"@admin:example.org","text":"Hello."}|})
            && fst (List.hd !spoken) = "!dm:example.org");
          List.iter
            (fun (label, actor, args) ->
              check label (Result.is_error (speak ~actor args)))
            [
              ( "voice notes need a member of the room",
                "@guest:example.org",
                {|{"text":"hi"}|} );
              ("voice notes refuse long text", admin,
                Printf.sprintf {|{"text":"%s"}|} (String.make 2001 'a'));
              ("voice notes refuse blank text", admin, {|{"text":" "}|});
            ];
          check "refused voice notes are never spoken"
            (List.length !spoken = 2);
          let replies_here = ref 0 and requests = ref [] in
          let scripted script =
            let script = ref script in
            fun (r : Agentkit.Chat.request) ->
              requests := r :: !requests;
              match !script with
              | next :: rest ->
                  script := rest;
                  next
              | [] -> Agentkit.Chat.response (Some "Done.")
          in
          let engine_with complete =
            Engine.create
              ~config:
                (Config.default ~admin ~homeserver:"https://matrix.example.org")
              ~store ~self ~plugins:[] ~complete ~now:(fun () -> 0.)
            |> (fun e -> Engine.with_matrix e matrix)
            |> Engine.with_room_observation
          in
          let poster =
            engine_with
              (scripted
                 [
                   Agentkit.Chat.response
                     ~calls:
                       [
                         {
                           Agentkit.Agent.id = "post";
                           name = "matrix_send";
                           arguments =
                             {|{"room":"!room:example.org","text":"Paper link."}|};
                         };
                       ]
                     None;
                   Agentkit.Chat.response (Some "Posted it.");
                 ])
          in
          let posted_before = List.length !sent in
          Engine.handle poster
            ~send:(fun _ -> incr replies_here)
            Engine.
              {
                room;
                sender = admin;
                id = "$post-here";
                body = "!crow post the paper here";
              };
          check "a post to the requesting room is the reply"
            (!replies_here = 0 && List.length !sent = posted_before + 1);
          let fetched = ref 0 in
          let png = "\x89PNG\r\n\x1a\n" ^ String.make 16 'x' in
          let attachments () =
            incr fetched;
            [ Option.get (Agentkit.Chat.image_of_string png) ]
          in
          let viewer = engine_with (scripted []) in
          let show id body =
            Engine.handle viewer ~attachments
              ~send:(fun _ -> incr replies_here)
              Engine.{ room; sender = admin; id; body }
          in
          show "$image-chatter" "[image] lunch was great";
          check "unaddressed images are never fetched"
            (!fetched = 0 && !replies_here = 0);
          show "$image-question" "[image] crow, what is this?";
          check "addressed images reach the model"
            (!fetched = 1 && !replies_here = 1
            &&
            match List.rev (List.hd !requests).messages with
            | Agentkit.Chat.User_images { text; images = [ _ ] } :: _ ->
                text = "[image] crow, what is this?"
            | _ -> false);
          (* The library half: one upload, then an m.audio voice message
             that points at it. *)
          let target =
            Option.get (Bot.find_room bot (Id.Room_id.of_string_exn room))
          in
          (match
             Matrix_bot.Sent.await ~timeout:10.
               (Matrix_bot.Room.send_audio target ~voice:true ~duration:1500
                  ~waveform:[ 0; 512; 1024 ] ~content_type:"audio/ogg"
                  ~filename:"Voice message.ogg" "AUDIO-BYTES")
           with
          | Matrix_bot.Sent.Sent _ -> ()
          | outcome ->
              failwith
                ("send_audio did not complete: " ^ Diagnostics.sent outcome));
          check "the audio is uploaded as is in a clear room"
            (List.mem "AUDIO-BYTES" !uploads);
          check "the event is a voice message pointing at the upload"
            (match !events with
            | body :: _ ->
                contains body {|"msgtype":"m.audio"|}
                && contains body {|"url":"mxc://example.org/voice"|}
                && contains body "org.matrix.msc3245.voice"
                && contains body {|"duration":1500|}
                && contains body {|"waveform":[0,512,1024]|}
                && contains body {|"mimetype":"audio/ogg"|}
            | [] -> false);
          let edit id new_content =
            message id
              (Printf.sprintf
                 {|{"msgtype":"m.text","body":"* fallback","m.new_content":%s,"m.relates_to":{"rel_type":"m.replace","event_id":"$original"}}|}
                 new_content)
          in
          let first_edit =
            edit "$edit"
              {|{"msgtype":"m.text","body":"!crow corrected question"}|}
          in
          pending :=
            [
              sync
                [
                  message "$original"
                    {|{"msgtype":"m.text","body":"ambient question"}|};
                  first_edit;
                ];
            ];
          until (fun () -> List.length !responses = 1);
          check "edit executes revised text and replies to original"
            (!accepted = 1 && !targets = [ "$original" ]);
          check "edit id is the durable claim"
            ((not (Store.claim store ~room ~event:"$edit"))
            && Store.claim store ~room ~event:"$original");
          pending :=
            [
              sync
                [
                  first_edit;
                  edit "$mention-edit"
                    {|{"msgtype":"m.text","body":"new wording","m.mentions":{"user_ids":["@crow:example.org"]}}|};
                  edit "$notice-edit"
                    {|{"msgtype":"m.notice","body":"!crow ignore"}|};
                  edit "$quoted-edit"
                    {|{"msgtype":"m.text","body":"> <@crow:example.org> !crow quoted request\n\nA comment for somebody else","m.relates_to":{"m.in_reply_to":{"event_id":"$quoted"}}}|};
                  message "$malformed-edit"
                    {|{"msgtype":"m.text","body":"!crow fallback","m.relates_to":{"rel_type":"m.replace","event_id":"$original"}}|};
                  message ~sender:"@guest:example.org" "$denied-edit"
                    {|{"msgtype":"m.text","body":"* fallback","m.new_content":{"msgtype":"m.text","body":"!crow forbidden"},"m.relates_to":{"rel_type":"m.replace","event_id":"$original"}}|};
                  message "$sentinel"
                    {|{"msgtype":"m.text","body":"!crow final"}|};
                ];
            ];
          until (fun () -> List.length !responses = 3);
          Eio.Time.Mono.sleep clock 0.03;
          check
            "edits use new mentions and preserve duplicate and authority guards"
            (!accepted = 3 && List.length !responses = 3)));
  print_endline
    "crowthebot: Matrix edit delivery and live room inspection passed"
