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
  let matrix =
    Matrix_rooms.create ~store ~state:(fun () ->
        !state |> Option.map (fun read -> read ()))
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
        (List.length tools = 9);
      ( None,
        [
          Openrouter.Tool.
            { id = "room-info"; name = "matrix_room_info"; arguments = "{}" };
        ] )
    end
  in
  let engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://matrix.example.org")
      ~store ~self ~plugins:[] ~complete ~now:(fun () -> 0.)
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
      {|{"next_batch":"initial","rooms":{"join":{%s,"!room:example.org":{"state":{"events":[{"type":"m.room.topic","state_key":"","content":{"topic":"Robot workshop"}},{"type":"m.room.member","state_key":"@admin:example.org","content":{"membership":"join"}},{"type":"m.room.member","state_key":"@crow:example.org","content":{"membership":"join"}}]},"timeline":{"events":[],"limited":false}}}}}|}
      (String.concat "," rooms)
  in
  let pending = ref [ initial ] in
  let fetch =
    Fetch_mock.client (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if String.ends_with ~suffix:"/versions" url then
          Fetch_mock.respond {|{"versions":["v1.11"]}|} request
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
          until (fun () -> List.length (Bot.rooms bot) = 8);
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
            (List.length (field "rooms" (Jsont.list Jsont.json) last) = 3
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
