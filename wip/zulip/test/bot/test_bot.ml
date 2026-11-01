open Alcotest

let headers = Http.Header.of_list [ ("content-type", "application/json") ]
let success = {|{"result":"success","msg":""}|}

let read_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let contains text needle =
  let text_length = String.length text
  and needle_length = String.length needle in
  let rec search start =
    start + needle_length <= text_length
    &&
    if String.sub text start needle_length = needle then true
    else search (start + 1)
  in
  needle_length = 0 || search 0

let test_command_words () =
  check (list string) "normalises whitespace" [ "one"; "two"; "three" ]
    (Zulip_bot.Event.argv " one\t two\nthree ")

let test_plugin_store_update () =
  Eio_main.run @@ fun _env ->
  let store = Zulip_bot.Plugin_store.memory () in
  let codec = Jsont.int in
  let update current = Option.value ~default:0 current + 1 in
  check (result int string) "first update" (Ok 1)
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.update store ~plugin:"counter" ~key:"n" codec
          update));
  check (result int string) "second update" (Ok 2)
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.update store ~plugin:"counter" ~key:"n" codec
          update));
  let namespace = Zulip_bot.Plugin_store.memory () in
  Zulip_bot.Plugin_store.set namespace ~plugin:"p" ~key:"v" Jsont.int 1
  |> Result.get_ok;
  Zulip_bot.Plugin_store.set namespace ~plugin:"p" ~room:"*" ~key:"v" Jsont.int
    2
  |> Result.get_ok;
  check (option int) "global namespace is distinct from star room" (Some 1)
    (Zulip_bot.Plugin_store.find namespace ~plugin:"p" ~key:"v" Jsont.int
    |> Result.get_ok);
  check (option int) "star room is preserved" (Some 2)
    (Zulip_bot.Plugin_store.find namespace ~plugin:"p" ~room:"*" ~key:"v"
       Jsont.int
    |> Result.get_ok)

let test_plugin_store_typed_corruption () =
  Eio_main.run @@ fun _env ->
  let store = Zulip_bot.Plugin_store.memory () in
  check (result unit string) "write text" (Ok ())
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.set store ~plugin:"p" ~key:"v" Jsont.string
          "text"));
  check bool "update refuses incompatible existing value" true
    (match
       Zulip_bot.Plugin_store.update store ~plugin:"p" ~key:"v" Jsont.int
         (fun _ -> 1)
     with
    | Error (Zulip_bot.Plugin_store.Codec _) -> true
    | _ -> false);
  check
    (result (option string) string)
    "old value survives" (Ok (Some "text"))
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.find store ~plugin:"p" ~key:"v" Jsont.string))

let test_plugin_store_file () =
  Eio_main.run @@ fun env ->
  let dir =
    Eio.Path.(env#fs / (".zulip-bot-store-" ^ string_of_int (Unix.getpid ())))
  in
  Eio.Path.mkdirs ~perm:0o700 dir;
  let file = Eio.Path.(dir / "plugins.json") in
  Fun.protect ~finally:(fun () ->
      Eio.Path.unlink ~missing_ok:true file;
      Eio.Path.rmdir dir)
  @@ fun () ->
  let store = Zulip_bot.Plugin_store.open_file file |> Result.get_ok in
  check (result unit string) "persist" (Ok ())
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.set store ~plugin:"p" ~key:"v" Jsont.string
          "saved"));
  let reopened = Zulip_bot.Plugin_store.open_file file |> Result.get_ok in
  check
    (result (option string) string)
    "reload" (Ok (Some "saved"))
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.find reopened ~plugin:"p" ~key:"v" Jsont.string))

let test_failed_file_write_rolls_back () =
  Eio_main.run @@ fun env ->
  let file = Eio.Path.(env#fs / ".missing-zulip-store" / "plugins.json") in
  let store = Zulip_bot.Plugin_store.open_file file |> Result.get_ok in
  check bool "write fails" true
    (match
       Zulip_bot.Plugin_store.set store ~plugin:"p" ~key:"v" Jsont.string "nope"
     with
    | Error _ -> true
    | Ok () -> false);
  check
    (result (option string) string)
    "memory rolled back" (Ok None)
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.find store ~plugin:"p" ~key:"v" Jsont.string))

let test_oversized_file_write_preserves_value () =
  Eio_main.run @@ fun env ->
  let dir =
    Eio.Path.(
      env#fs / (".zulip-bot-store-size-" ^ string_of_int (Unix.getpid ())))
  in
  Eio.Path.mkdirs ~perm:0o700 dir;
  let file = Eio.Path.(dir / "plugins.json") in
  Fun.protect ~finally:(fun () ->
      Eio.Path.unlink ~missing_ok:true file;
      Eio.Path.rmdir dir)
  @@ fun () ->
  let store = Zulip_bot.Plugin_store.open_file file |> Result.get_ok in
  ignore
    (Zulip_bot.Plugin_store.set store ~plugin:"p" ~key:"v" Jsont.string "small");
  let oversized = String.make (1024 * 1024) 'x' in
  check bool "oversized update is rejected" true
    (match
       Zulip_bot.Plugin_store.set store ~plugin:"p" ~key:"v" Jsont.string
         oversized
     with
    | Error _ -> true
    | Ok () -> false);
  let reopened = Zulip_bot.Plugin_store.open_file file |> Result.get_ok in
  check
    (result (option string) string)
    "previous persisted value remains readable" (Ok (Some "small"))
    (Result.map_error Zulip_bot.Plugin_store.error_to_string
       (Zulip_bot.Plugin_store.find reopened ~plugin:"p" ~key:"v" Jsont.string))

let client env =
  let auth =
    Result.get_ok
      (Zulip_eio.Auth.create ~site:"https://example.test"
         ~email:"bot@example.test" ~api_key:"secret")
  in
  let fetch =
    Fetch_mock.client (fun request -> Fetch_mock.respond "{}" request)
  in
  let transport = Zulip_eio.Transport.of_fetch ~clock:env#clock fetch in
  Result.get_ok (Zulip_eio.Client.create ~transport ~auth ())

let identity : Zulip_bot.Context.identity =
  {
    user_id = Zulip.Id.User.of_int 1;
    email = "bot@example.test";
    full_name = "Bot";
  }

let destination =
  Zulip.Message.Channel
    {
      channel_id = Zulip.Id.Channel.of_int 4;
      channel_name = "general";
      topic = "test";
    }

let rate_limit =
  Zulip_eio.Error.Api
    {
      status = 429;
      code = "RATE_LIMIT_HIT";
      message = "slow down";
      extra = Jsont.Json.null ();
      retry_after = Some 0.;
    }

let transient =
  Zulip_eio.Error.Http
    { status = 503; message = "unavailable"; retry_after = None }

let test_cancelled_send () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let started, start = Eio.Promise.create () in
  let release, _ = Eio.Promise.create () in
  let send ~destination:_ ~content:_ =
    Eio.Promise.resolve start ();
    Eio.Promise.await release
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~send ()
  in
  let destination =
    Zulip.Message.Direct
      {
        recipient_id = Zulip.Id.Recipient.of_int 1;
        participants = [ Zulip.Id.User.of_int 2 ];
      }
  in
  ignore (Zulip_bot.Context.enqueue context ~destination ~content:"first");
  Eio.Promise.await started;
  let sent = Zulip_bot.Context.enqueue context ~destination ~content:"queued" in
  check bool "timeout is local to the wait" true
    (Zulip_bot.Sent.await ~timeout:0. sent = `Timed_out);
  check bool "timed-out send remains queued" true
    (Zulip_bot.Sent.status sent = Zulip_bot.Sent.Queued);
  check bool "cancels while queued" true
    (Zulip_bot.Sent.cancel sent = `Cancelled);
  check bool "settled cancellation" true
    (Zulip_bot.Sent.cancel sent = `Settled Zulip_bot.Sent.Cancelled);
  check bool "settled" true
    (Zulip_bot.Sent.await ~timeout:0. sent = `Done Zulip_bot.Sent.Cancelled)

let test_shutdown_unblocks_enqueue () =
  Eio_main.run @@ fun env ->
  Eio.Time.with_timeout_exn env#clock 2. @@ fun () ->
  let published, publish = Eio.Promise.create () in
  let closing, close = Eio.Promise.create () in
  let started, start = Eio.Promise.create () in
  let forever, _ = Eio.Promise.create () in
  let destination =
    Zulip.Message.Direct
      {
        recipient_id = Zulip.Id.Recipient.of_int 1;
        participants = [ Zulip.Id.User.of_int 2 ];
      }
  in
  Eio.Fiber.both
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let send ~destination:_ ~content:_ =
        Eio.Promise.resolve start ();
        Eio.Promise.await forever
      in
      let context =
        Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~send
          ~send_depth:1 ()
      in
      ignore (Zulip_bot.Context.enqueue context ~destination ~content:"sending");
      Eio.Promise.await started;
      ignore
        (Zulip_bot.Context.enqueue context ~destination ~content:"buffered");
      Eio.Promise.resolve publish context;
      Eio.Promise.await closing)
    (fun () ->
      let context = Eio.Promise.await published in
      let entering, enter = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          Eio.Promise.resolve enter ();
          let sent =
            Zulip_bot.Context.enqueue context ~destination ~content:"blocked"
          in
          check bool "blocked writer returns cancelled handle" true
            (Zulip_bot.Sent.await ~timeout:0. sent
            = `Done Zulip_bot.Sent.Cancelled))
        (fun () ->
          Eio.Promise.await entering;
          Eio.Fiber.yield ();
          Eio.Promise.resolve close ()))

let test_corrupt_store_open () =
  Eio_main.run @@ fun env ->
  let path =
    Eio.Path.(
      env#fs / (".zulip-corrupt-store-" ^ string_of_int (Unix.getpid ())))
  in
  Fun.protect ~finally:(fun () -> Eio.Path.unlink ~missing_ok:true path)
  @@ fun () ->
  Eio.Path.save ~create:(`Exclusive 0o600) path "{broken";
  check bool "corruption returns a backend error" true
    (match Zulip_bot.Plugin_store.open_file path with
    | Error (Backend _) -> true
    | _ -> false);
  Eio.Path.save ~create:(`Or_truncate 0o600) path "{}";
  Eio.Path.chmod ~follow:false ~perm:0o644 path;
  check bool "insecure file returns a backend error" true
    (match Zulip_bot.Plugin_store.open_file path with
    | Error (Backend _) -> true
    | _ -> false)

let test_rate_limit_retry () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attempts = ref 0 in
  let send ~destination:_ ~content:_ =
    incr attempts;
    if !attempts = 1 then Error rate_limit else Ok (Zulip.Id.Message.of_int 7)
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send ()
  in
  let sent = Zulip_bot.Context.enqueue context ~destination ~content:"hello" in
  check bool "rate limit is retried" true
    (match Zulip_bot.Sent.await ~timeout:1. sent with
    | `Done (Zulip_bot.Sent.Sent id) -> Zulip.Id.Message.to_int id = 7
    | _ -> false);
  check int "two attempts" 2 !attempts

let test_transient_send_not_replayed () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attempts = ref 0 in
  let send ~destination:_ ~content:_ =
    incr attempts;
    Error transient
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send ()
  in
  let sent = Zulip_bot.Context.enqueue context ~destination ~content:"hello" in
  check bool "5xx is indeterminate and not replayed" true
    (match Zulip_bot.Sent.await ~timeout:1. sent with
    | `Done (Zulip_bot.Sent.Indeterminate (Some _)) -> true
    | _ -> false);
  check int "one attempt" 1 !attempts

let test_send_callback_exception_is_indeterminate () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attempts = ref 0 in
  let send ~destination:_ ~content:_ =
    incr attempts;
    failwith "injected send failure"
  in
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send ()
  in
  let sent = Zulip_bot.Context.enqueue context ~destination ~content:"hello" in
  check bool "raised callback is an indeterminate transport error" true
    (match Zulip_bot.Sent.await ~timeout:1. sent with
    | `Done
        (Zulip_bot.Sent.Indeterminate
           (Some (Zulip_eio.Error.Transport (Fetch.Protocol_error message)))) ->
        contains message "injected send failure"
    | _ -> false);
  check int "raised callback is attempted once" 1 !attempts

let test_default_sender_self_dm () =
  Eio_mock.Backend.run_full @@ fun env ->
  let requests = ref [] in
  let backend request =
    requests := Httpz_media.Urlencoded.decode (read_body request) :: !requests;
    Fetch_mock.respond ~headers {|{"result":"success","msg":"","id":55}|}
      request
  in
  Eio.Switch.run @@ fun sw ->
  let auth =
    Result.get_ok
      (Zulip_eio.Auth.create ~site:"https://example.test"
         ~email:"bot@example.test" ~api_key:"secret")
  in
  let transport =
    Zulip_eio.Transport.of_fetch ~clock:env#clock (Fetch_mock.client backend)
  in
  let client = Result.get_ok (Zulip_eio.Client.create ~transport ~auth ()) in
  let context = Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock () in
  let destination =
    Zulip.Message.Direct
      {
        recipient_id = Zulip.Id.Recipient.of_int 1;
        participants = [ identity.user_id ];
      }
  in
  let sent = Zulip_bot.Context.enqueue context ~destination ~content:"self" in
  check bool "self-DM send completes" true
    (match Zulip_bot.Sent.await ~timeout:1. sent with
    | `Done (Zulip_bot.Sent.Sent id) -> Zulip.Id.Message.to_int id = 55
    | _ -> false);
  match !requests with
  | [ request ] ->
      check string "self-DM recipient is the bot user" "[1]"
        (List.assoc "to" request)
  | _ -> Alcotest.fail "self-DM should issue exactly one request"

let event json =
  Result.get_ok (Jsont_bytesrw.decode_string Zulip.Event.jsont json)

let message_event =
  event
    {|{"id":1,"type":"message","message":{"id":10,"sender_id":3,"sender_email":"a@example.test","sender_full_name":"A","timestamp":1,"content":"hello","content_type":"text/x-markdown","type":"stream","stream_id":4,"display_recipient":"general","subject":"topic","flags":[]},"flags":[]}|}

let reaction_event =
  event
    {|{"id":2,"type":"reaction","op":"add","message_id":10,"user_id":3,"emoji_name":"thumbs_up","emoji_code":"1f44d","reaction_type":"unicode_emoji"}|}

let delete_event = event {|{"id":3,"type":"delete_message","message_ids":[10]}|}
let unknown_delete = event {|{"id":3,"type":"delete_message","message_id":11}|}

let test_partial_events_use_location_cache () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send:(fun ~destination:_ ~content:_ -> assert false)
      ()
  in
  check bool "message projects" true
    (match Zulip_bot.Event.of_zulip context message_event with
    | Some (Zulip_bot.Event.Message _) -> true
    | _ -> false);
  check bool "partial reaction gets cached room" true
    (match Zulip_bot.Event.of_zulip context reaction_event with
    | Some (Zulip_bot.Event.Reaction reaction) ->
        Zulip.Id.Message.to_int reaction.message_id = 10
    | _ -> false);
  check bool "actorless plural delete gets cached room" true
    (match Zulip_bot.Event.of_zulip context delete_event with
    | Some (Zulip_bot.Event.Delete deleted) ->
        Option.is_some deleted.room
        && List.map Zulip.Id.Message.to_int deleted.message_ids = [ 10 ]
    | _ -> false);
  check bool "unlocatable delete is preserved raw" true
    (match Zulip_bot.Event.of_zulip context unknown_delete with
    | Some (Zulip_bot.Event.Custom custom) ->
        custom.event_type = "delete_message"
    | _ -> false)

let test_context_shutdown_settles_send () =
  Eio_main.run @@ fun env ->
  let started, mark_started = Eio.Promise.create () in
  let sent =
    Eio.Switch.run @@ fun sw ->
    let send ~destination:_ ~content:_ =
      Eio.Promise.resolve mark_started ();
      Eio.Promise.await (fst (Eio.Promise.create ()))
    in
    let context =
      Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
        ~send ()
    in
    let sent =
      Zulip_bot.Context.enqueue context ~destination ~content:"pending"
    in
    Eio.Promise.await started;
    sent
  in
  check bool "shutdown makes in-flight send indeterminate" true
    (match Zulip_bot.Sent.await ~timeout:0. sent with
    | `Done (Zulip_bot.Sent.Indeterminate None) -> true
    | _ -> false)

let bot_message ?(id = 10) ?(sender = 3) ?(flags = [])
    ?(destination = destination) body =
  Zulip.Message.create
    ~id:(Zulip.Id.Message.of_int id)
    ~sender_id:(Zulip.Id.User.of_int sender)
    ~sender_email:"sender@example.test" ~sender_full_name:"Sender" ~timestamp:1.
    ~content:body ~destination ~flags ()
  |> Result.get_ok

let test_local_message_has_no_event_id () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send:(fun ~destination:_ ~content:_ -> assert false)
      ()
  in
  check bool "webhook-style messages have no invented queue event ID" true
    (Option.is_none
       (Zulip_bot.Event.of_message context (bot_message "local")).envelope
         .event_id)

let test_partial_event_validation_and_moves () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let context =
    Zulip_bot.Context.v ~sw ~client:(client env) ~identity ~clock:env#clock
      ~send:(fun ~destination:_ ~content:_ -> assert false)
      ()
  in
  let old_destination =
    Zulip.Message.Channel
      {
        channel_id = Zulip.Id.Channel.of_int 4;
        channel_name = "old";
        topic = "old topic";
      }
  in
  List.iter
    (fun id ->
      ignore
        (Zulip_bot.Event.of_message context
           (bot_message ~id ~destination:old_destination "before move")))
    [ 10; 11 ];
  let moved =
    event
      {|{"id":4,"type":"update_message","message_id":10,"message_ids":[10,11],"user_id":3,"stream_id":4,"stream_name":"old","new_stream_id":5,"subject":"new topic"}|}
  in
  check bool "move projects to post-edit location" true
    (match Zulip_bot.Event.of_zulip context moved with
    | Some (Zulip_bot.Event.Edit edit) -> (
        match Zulip_bot.Room.destination edit.envelope.room with
        | Zulip.Message.Channel { channel_id; topic; _ } ->
            Zulip.Id.Channel.to_int channel_id = 5 && topic = "new topic"
        | Direct _ -> false)
    | _ -> false);
  let reaction =
    event
      {|{"id":5,"type":"reaction","op":"add","message_id":11,"user_id":3,"emoji_name":"thumbs_up","emoji_code":"1f44d","reaction_type":"unicode_emoji"}|}
  in
  check bool "second moved message retains post-edit location" true
    (match Zulip_bot.Event.of_zulip context reaction with
    | Some (Zulip_bot.Event.Reaction reaction) -> (
        match Zulip_bot.Room.destination reaction.envelope.room with
        | Zulip.Message.Channel { channel_id; topic; _ } ->
            Zulip.Id.Channel.to_int channel_id = 5 && topic = "new topic"
        | Direct _ -> false)
    | _ -> false);
  let malformed_flags =
    event
      {|{"id":6,"type":"message","message":{"id":12,"sender_id":3,"sender_email":"a@example.test","sender_full_name":"A","timestamp":1,"content":"hello","content_type":"text/x-markdown","type":"stream","stream_id":4,"display_recipient":"general","subject":"topic","flags":[]},"flags":[1]}|}
  in
  check bool "message with invalid event flags is malformed" true
    (match Zulip_bot.Event.of_zulip context malformed_flags with
    | Some (Zulip_bot.Event.Malformed _) -> true
    | _ -> false);
  let malformed_reaction =
    event {|{"id":7,"type":"reaction","message_id":9007199254740992}|}
  in
  check bool "reaction with invalid IDs is malformed" true
    (match Zulip_bot.Event.of_zulip context malformed_reaction with
    | Some (Zulip_bot.Event.Malformed _) -> true
    | _ -> false);
  List.iter
    (fun (field, raw) ->
      check bool
        ("reaction missing " ^ field ^ " is malformed")
        true
        (match Zulip_bot.Event.of_zulip context (event raw) with
        | Some (Zulip_bot.Event.Malformed _) -> true
        | _ -> false))
    [
      ( "op",
        {|{"id":8,"type":"reaction","message_id":11,"user_id":3,"emoji_name":"thumbs_up"}|}
      );
      ( "emoji_name",
        {|{"id":9,"type":"reaction","op":"add","message_id":11,"user_id":3}|} );
    ];
  let rendering_only =
    event
      {|{"id":10,"type":"update_message","message_id":10,"rendered_content":"<p>x</p>"}|}
  in
  check bool "actorless rendering update remains custom" true
    (match Zulip_bot.Event.of_zulip context rendering_only with
    | Some (Zulip_bot.Event.Custom _) -> true
    | _ -> false)

let direct_destination ?(recipient_id = 9) participants =
  Zulip.Message.Direct
    {
      recipient_id = Zulip.Id.Recipient.of_int recipient_id;
      participants = List.map Zulip.Id.User.of_int participants;
    }

let run_bot ?(is_bot = Fun.const false) spec on_start check_result =
  Eio_mock.Backend.run_full @@ fun env ->
  let deletes = ref 0 in
  let never, _ = Eio.Promise.create () in
  let backend (request : Fetch.Middleware.request) =
    let target = Fetch.Middleware.Url.path_and_query request.url in
    if String.starts_with ~prefix:"/api/v1/register" target then
      Fetch_mock.respond ~headers
        {|{"result":"success","queue_id":"test-queue","last_event_id":-1,"event_queue_longpoll_timeout_seconds":120}|}
        request
    else if request.meth = `DELETE then (
      incr deletes;
      Fetch_mock.respond ~headers success request)
    else if String.starts_with ~prefix:"/api/v1/events" target then
      Eio.Promise.await never
    else Fetch_mock.respond ~headers success request
  in
  Eio.Switch.run @@ fun sw ->
  let auth =
    Result.get_ok
      (Zulip_eio.Auth.create ~site:"https://example.test"
         ~email:"bot@example.test" ~api_key:"secret")
  in
  let transport =
    Zulip_eio.Transport.of_fetch ~clock:env#clock (Fetch_mock.client backend)
  in
  let client = Result.get_ok (Zulip_eio.Client.create ~transport ~auth ()) in
  let context =
    Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock ~is_bot
      ~send:(fun ~destination:_ ~content:_ -> Ok (Zulip.Id.Message.of_int 99))
      ()
  in
  let spec =
    spec
    |> Zulip_bot.Bot.on_sync (fun bot -> function
      | Zulip_bot.Event.Live -> on_start context bot
      | _ -> ())
  in
  Eio.Time.with_timeout_exn env#clock 5. (fun () ->
      Zulip_bot.Bot.run context spec);
  check_result !deletes

let test_runner_orders_rooms_and_stops_cleanly () =
  let release_first, wake_first = Eio.Promise.create () in
  let trace = ref [] in
  let stopped = ref 0 in
  let spec =
    Zulip_bot.Bot.v ~all_messages:true ~workers:2 ()
    |> Zulip_bot.Bot.on_sync (fun _ -> function
      | Zulip_bot.Event.Stopped -> incr stopped
      | Connecting | Live | Recovering _ -> ())
    |> Zulip_bot.Bot.on_message (fun bot message ->
        match message.body with
        | "a1" ->
            trace := !trace @ [ "a1-start" ];
            Eio.Promise.await release_first;
            trace := !trace @ [ "a1-end" ]
        | "b" ->
            check (list string) "first room has started" [ "a1-start" ] !trace;
            trace := !trace @ [ "b" ];
            Eio.Promise.resolve wake_first ();
            Eio.Fiber.yield ()
        | "a2" ->
            trace := !trace @ [ "a2" ];
            Zulip_bot.Bot.stop bot
        | _ -> assert false)
  in
  run_bot spec
    (fun context bot ->
      Zulip_bot.Bot.dispatch bot
        (Zulip_bot.Event.Message
           (Zulip_bot.Event.of_message context (bot_message ~id:1 "a1")));
      Zulip_bot.Bot.dispatch bot
        (Zulip_bot.Event.Message
           (Zulip_bot.Event.of_message context
              (bot_message ~id:2 ~destination:(direct_destination [ 1; 3 ]) "b")));
      Zulip_bot.Bot.dispatch bot
        (Zulip_bot.Event.Message
           (Zulip_bot.Event.of_message context (bot_message ~id:3 "a2"))))
    (fun deletes ->
      check (list string) "room FIFO and independent room progress"
        [ "a1-start"; "b"; "a1-end"; "a2" ]
        !trace;
      check int "queue is deleted once" 1 deletes;
      check int "stopped is reported once" 1 !stopped)

let test_only_replacing_plugin () =
  let accepted = ref [] in
  let replacement _ =
    Zulip_bot.Bot.v ~all_messages:true ()
    |> Zulip_bot.Bot.on_message (fun _ message ->
        accepted := message.body :: !accepted)
  in
  let spec =
    Zulip_bot.Bot.v ()
    |> Zulip_bot.Bot.on_message (fun _ _ ->
        Alcotest.fail "discarded handler ran")
    |> Zulip_bot.Bot.only
         (function Zulip_bot.Event.Message m -> m.body = "yes" | _ -> false)
         replacement
    |> Zulip_bot.Bot.on_message (fun bot m ->
        if m.body = "yes" then Zulip_bot.Bot.stop bot)
  in
  run_bot spec
    (fun context bot ->
      List.iteri
        (fun i body ->
          Zulip_bot.Bot.dispatch bot
            (Zulip_bot.Event.Message
               (Zulip_bot.Event.of_message context
                  (bot_message ~id:(i + 1) body))))
        [ "no"; "yes" ])
    (fun _ ->
      check (list string) "replacement handler remains scoped" [ "yes" ]
        !accepted)

let test_only_distinguishes_duplicate_handler_registrations () =
  let calls = ref [] in
  let shared_handler _ = function
    | Zulip_bot.Event.Message message -> calls := message.body :: !calls
    | _ -> ()
  in
  let scoped = Zulip_bot.Bot.on shared_handler in
  let spec =
    Zulip_bot.Bot.v ~all_messages:true ()
    |> Zulip_bot.Bot.on shared_handler
    |> Zulip_bot.Bot.only
         (function Zulip_bot.Event.Message m -> m.body = "yes" | _ -> false)
         scoped
    |> Zulip_bot.Bot.on_message (fun bot message ->
        if message.body = "no" then Zulip_bot.Bot.stop bot)
  in
  run_bot spec
    (fun context bot ->
      Zulip_bot.Bot.dispatch bot
        (Zulip_bot.Event.Message
           (Zulip_bot.Event.of_message context (bot_message "no"))))
    (fun _ ->
      check (list string) "only the preexisting registration runs" [ "no" ]
        !calls)

let test_runner_flood_drains () =
  let handled = ref [] in
  let spec =
    Zulip_bot.Bot.v ~all_messages:true ~workers:1 ~queue_depth:2
      ~total_queue_depth:3 ()
    |> Zulip_bot.Bot.on_message (fun bot message ->
        handled := !handled @ [ message.body ];
        if List.length !handled = 10 then Zulip_bot.Bot.stop bot)
  in
  run_bot spec
    (fun context bot ->
      List.iter
        (fun n ->
          Zulip_bot.Bot.dispatch bot
            (Zulip_bot.Event.Message
               (Zulip_bot.Event.of_message context
                  (bot_message ~id:n (string_of_int n)))))
        (List.init 10 (fun n -> n + 1)))
    (fun deletes ->
      check (list string) "flood drains in FIFO order"
        (List.init 10 (fun n -> string_of_int (n + 1)))
        !handled;
      check int "queue is deleted" 1 deletes)

let test_runner_handler_exception_continues () =
  let errors = ref 0 and handled = ref [] in
  let spec =
    Zulip_bot.Bot.v ~all_messages:true ~workers:1 ()
    |> Zulip_bot.Bot.on_error (fun _ _ _ -> incr errors)
    |> Zulip_bot.Bot.on_message (fun bot message ->
        if String.equal message.body "broken" then failwith "expected";
        handled := message.body :: !handled;
        Zulip_bot.Bot.stop bot)
  in
  run_bot spec
    (fun context bot ->
      List.iteri
        (fun n body ->
          Zulip_bot.Bot.dispatch bot
            (Zulip_bot.Event.Message
               (Zulip_bot.Event.of_message context
                  (bot_message ~id:(n + 1) body))))
        [ "broken"; "later" ])
    (fun _ ->
      check int "handler error reported" 1 !errors;
      check (list string) "later same-room event runs" [ "later" ] !handled)

let test_default_activation_and_commands () =
  let messages = ref 0 and commands = ref 0 in
  let completed = ref 0 in
  let finish bot =
    incr completed;
    if !completed = 6 then Zulip_bot.Bot.stop bot
  in
  let spec =
    Zulip_bot.Bot.v ~workers:3 ()
    |> Zulip_bot.Bot.on_message (fun bot _ ->
        incr messages;
        finish bot)
    |> Zulip_bot.Bot.command ~name:"ping" (fun bot _ ->
        incr commands;
        finish bot)
  in
  let is_bot id = Zulip.Id.User.to_int id = 2 in
  run_bot ~is_bot spec
    (fun context bot ->
      let dispatch ?flags ?destination ?sender ?id body =
        Zulip_bot.Bot.dispatch bot
          (Zulip_bot.Event.Message
             (Zulip_bot.Event.of_message context ?flags
                (bot_message ?destination ?sender ?id body)))
      in
      dispatch ~sender:1 ~destination:(direct_destination [ 1; 3 ]) "own";
      dispatch ~sender:2 ~destination:(direct_destination [ 1; 2 ]) "bot";
      dispatch "unmentioned channel";
      dispatch ~flags:[ `Mentioned ] "mentioned channel";
      dispatch ~destination:(direct_destination [ 1; 3; 4 ]) "group";
      dispatch ~destination:(direct_destination [ 1; 3 ]) "one to one";
      dispatch ~flags:[ `Mentioned ] "!ping";
      dispatch ~flags:[ `Mentioned ] "@**Bot** !ping";
      dispatch ~flags:[ `Mentioned ] "@**Bot|1** !ping";
      dispatch ~flags:[ `Mentioned ] "@**A previous name|1** !ping")
    (fun _ ->
      check int "only mentioned channel and one-to-one DM call message handler"
        2 !messages;
      check int
        "commands after display, stable and renamed mentions are dispatched" 4
        !commands)

let test_webhook_routes_mention_once () =
  let sends = ref [] in
  Eio_mock.Backend.run_full @@ fun env ->
  let never, _ = Eio.Promise.create () in
  let backend (request : Fetch.Middleware.request) =
    let target = Fetch.Middleware.Url.path_and_query request.url in
    if String.starts_with ~prefix:"/api/v1/register" target then
      Fetch_mock.respond ~headers
        {|{"result":"success","queue_id":"test-queue","last_event_id":-1,"event_queue_longpoll_timeout_seconds":120}|}
        request
    else if request.meth = `DELETE then
      Fetch_mock.respond ~headers success request
    else if String.starts_with ~prefix:"/api/v1/events" target then
      Eio.Promise.await never
    else Fetch_mock.respond ~headers success request
  in
  Eio.Switch.run @@ fun sw ->
  let auth =
    Result.get_ok
      (Zulip_eio.Auth.create ~site:"https://example.test"
         ~email:"bot@example.test" ~api_key:"secret")
  in
  let transport =
    Zulip_eio.Transport.of_fetch ~clock:env#clock (Fetch_mock.client backend)
  in
  let client = Result.get_ok (Zulip_eio.Client.create ~transport ~auth ()) in
  let context =
    Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock
      ~send:(fun ~destination ~content ->
        sends := (destination, content) :: !sends;
        Ok (Zulip.Id.Message.of_int 77))
      ()
  in
  let payload trigger token =
    Printf.sprintf
      {|{"token":%S,"trigger":%S,"message":{"id":12,"sender_id":3,"sender_email":"a@example.test","sender_full_name":"A","timestamp":1,"content":"hello","content_type":"text/x-markdown","type":"stream","stream_id":4,"display_recipient":"general","subject":"topic","flags":[]}}|}
      token trigger
  in
  let pp_webhook_result ppf = function
    | `Accepted -> Format.pp_print_string ppf "accepted"
    | `Stopped -> Format.pp_print_string ppf "stopped"
    | `Invalid_token -> Format.pp_print_string ppf "invalid token"
    | `Malformed message -> Format.fprintf ppf "malformed: %s" message
  in
  let spec =
    Zulip_bot.Bot.v ()
    |> Zulip_bot.Bot.on_sync (fun bot -> function
      | Zulip_bot.Event.Stopped ->
          check (of_pp pp_webhook_result) "stopped webhook is rejected" `Stopped
            (Zulip_bot.Webhook.handle ~token:"expected" bot
               ~payload:(payload "mention" "expected"))
      | _ -> ())
    |> Zulip_bot.Bot.on_message (fun bot message ->
        ignore
          (Zulip_bot.Sent.await ~timeout:1.
             (Zulip_bot.Event.reply message.envelope "reply"));
        Zulip_bot.Bot.stop bot)
  in
  Zulip_bot.Bot.run
    ~on_start:(fun bot ->
      check (of_pp pp_webhook_result) "empty configured token is rejected"
        `Invalid_token
        (Zulip_bot.Webhook.handle ~token:"" bot ~payload:(payload "mention" ""));
      check bool "deep payload rejected before recursive decoding" true
        (match
           Zulip_bot.Webhook.handle ~token:"expected" bot
             ~payload:(String.make 10000 '[' ^ "0" ^ String.make 10000 ']')
         with
        | `Malformed _ -> true
        | `Accepted | `Stopped | `Invalid_token -> false);
      check (of_pp pp_webhook_result) "bad token is rejected" `Invalid_token
        (Zulip_bot.Webhook.handle ~token:"expected" bot
           ~payload:(payload "mention" "wrong"));
      check (of_pp pp_webhook_result) "unknown trigger is rejected"
        (`Malformed "unsupported outgoing webhook trigger")
        (Zulip_bot.Webhook.handle ~token:"expected" bot
           ~payload:(payload "bad" "expected"));
      check (of_pp pp_webhook_result) "channel mention is accepted" `Accepted
        (Zulip_bot.Webhook.handle ~token:"expected" bot
           ~payload:(payload "mention" "expected")))
    context spec;
  match !sends with
  | [ (Zulip.Message.Channel { channel_id; topic; _ }, "reply") ] ->
      check int "reply uses incoming numeric channel" 4
        (Zulip.Id.Channel.to_int channel_id);
      check string "reply uses incoming topic" "topic" topic
  | _ -> Alcotest.fail "webhook should route exactly one reply"

let test_queue_recovers_without_repolling_expired_queue () =
  Eio_mock.Backend.run_full @@ fun env ->
  let registrations = ref 0 and q1_polls = ref 0 and q2_polls = ref 0 in
  let deletes = ref [] and live = ref 0 and recovering = ref 0 in
  let backend (request : Fetch.Middleware.request) =
    let target = Fetch.Middleware.Url.path_and_query request.url in
    if String.starts_with ~prefix:"/api/v1/register" target then (
      incr registrations;
      let response =
        if !registrations = 1 then
          {|{"result":"success","queue_id":"q1","last_event_id":-1,"event_queue_longpoll_timeout_seconds":120}|}
        else
          {|{"result":"success","queue_id":"q2","last_event_id":40,"event_queue_longpoll_timeout_seconds":120}|}
      in
      Fetch_mock.respond ~headers response request)
    else if request.meth = `DELETE then (
      deletes := read_body request :: !deletes;
      Fetch_mock.respond ~headers success request)
    else if String.starts_with ~prefix:"/api/v1/events" target then
      if contains target "queue_id=q1" then (
        incr q1_polls;
        Fetch_mock.respond ~status:400 ~headers
          {|{"result":"error","code":"BAD_EVENT_QUEUE_ID","msg":"expired"}|}
          request)
      else (
        incr q2_polls;
        check bool "fresh queue starts at its registration cursor" true
          (contains target "queue_id=q2" && contains target "last_event_id=40");
        Fetch_mock.respond ~headers
          {|{"result":"success","events":[{"id":41,"type":"future_event","x":1}]}|}
          request)
    else Fetch_mock.respond ~headers success request
  in
  Eio.Switch.run @@ fun sw ->
  let auth =
    Result.get_ok
      (Zulip_eio.Auth.create ~site:"https://example.test"
         ~email:"bot@example.test" ~api_key:"secret")
  in
  let transport =
    Zulip_eio.Transport.of_fetch ~clock:env#clock (Fetch_mock.client backend)
  in
  let client = Result.get_ok (Zulip_eio.Client.create ~transport ~auth ()) in
  let context =
    Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock
      ~send:(fun ~destination:_ ~content:_ -> assert false)
      ()
  in
  let spec =
    Zulip_bot.Bot.v ()
    |> Zulip_bot.Bot.on_sync (fun _ -> function
      | Zulip_bot.Event.Live -> incr live
      | Recovering _ -> incr recovering
      | Connecting | Stopped -> ())
    |> Zulip_bot.Bot.on_custom (fun bot custom ->
        if String.equal custom.event_type "future_event" then
          Zulip_bot.Bot.stop bot)
  in
  Eio.Time.with_timeout_exn env#clock 5. (fun () ->
      Zulip_bot.Bot.run context spec);
  check int "two registrations" 2 !registrations;
  check int "expired queue polled once" 1 !q1_polls;
  check int "replacement queue polled once" 1 !q2_polls;
  check int "live after each registration" 2 !live;
  check int "one recovery" 1 !recovering;
  check (list string) "only active replacement queue is deleted"
    [ "queue_id=q2" ] !deletes

let test_queue_terminal_poll_stops () =
  let run_case name ~status payload =
    Eio_mock.Backend.run_full @@ fun env ->
    let registrations = ref 0 and polls = ref 0 and deletes = ref 0 in
    let backend (request : Fetch.Middleware.request) =
      let target = Fetch.Middleware.Url.path_and_query request.url in
      if String.starts_with ~prefix:"/api/v1/register" target then (
        incr registrations;
        Fetch_mock.respond ~headers
          {|{"result":"success","queue_id":"q","last_event_id":-1,"event_queue_longpoll_timeout_seconds":120}|}
          request)
      else if request.meth = `DELETE then (
        incr deletes;
        Fetch_mock.respond ~headers success request)
      else if String.starts_with ~prefix:"/api/v1/events" target then (
        incr polls;
        Fetch_mock.respond ~status ~headers payload request)
      else Fetch_mock.respond ~headers success request
    in
    Eio.Switch.run @@ fun sw ->
    let auth =
      Result.get_ok
        (Zulip_eio.Auth.create ~site:"https://example.test"
           ~email:"bot@example.test" ~api_key:"secret")
    in
    let transport =
      Zulip_eio.Transport.of_fetch ~clock:env#clock (Fetch_mock.client backend)
    in
    let client = Result.get_ok (Zulip_eio.Client.create ~transport ~auth ()) in
    let context =
      Zulip_bot.Context.v ~sw ~client ~identity ~clock:env#clock
        ~send:(fun ~destination:_ ~content:_ -> assert false)
        ()
    in
    let recoveries = ref 0 in
    let spec =
      Zulip_bot.Bot.v ()
      |> Zulip_bot.Bot.on_sync (fun _ -> function
        | Recovering _ -> incr recoveries
        | Connecting | Live | Stopped -> ())
    in
    let result =
      Eio.Time.with_timeout_exn env#clock 5. (fun () ->
          Zulip_bot.Bot.run_result context spec)
    in
    check bool
      (name ^ " returns terminal failure")
      true (Result.is_error result);
    check int (name ^ " registers once") 1 !registrations;
    check int (name ^ " polls once") 1 !polls;
    check int (name ^ " reports one recovery") 1 !recoveries;
    check int (name ^ " deletes its queue") 1 !deletes
  in
  run_case "terminal auth" ~status:401
    {|{"result":"error","code":"INVALID_API_KEY","msg":"bad key"}|};
  run_case "malformed poll" ~status:200
    {|{"result":"success","events":[{"type":"message"}]}|}

let () =
  run "zulip bot"
    [
      ( "runtime",
        [
          test_case "command words" `Quick test_command_words;
          test_case "atomic update" `Quick test_plugin_store_update;
          test_case "typed corruption" `Quick test_plugin_store_typed_corruption;
          test_case "file persistence" `Quick test_plugin_store_file;
          test_case "failed write rollback" `Quick
            test_failed_file_write_rolls_back;
          test_case "oversized write rollback" `Quick
            test_oversized_file_write_preserves_value;
          test_case "send cancellation" `Quick test_cancelled_send;
          test_case "shutdown wakes blocked enqueue" `Quick
            test_shutdown_unblocks_enqueue;
          test_case "recoverable store open" `Quick test_corrupt_store_open;
          test_case "429 send retry" `Quick test_rate_limit_retry;
          test_case "no transient send retry" `Quick
            test_transient_send_not_replayed;
          test_case "send callback exception" `Quick
            test_send_callback_exception_is_indeterminate;
          test_case "default sender self-DM" `Quick test_default_sender_self_dm;
          test_case "partial event routing" `Quick
            test_partial_events_use_location_cache;
          test_case "local event has no queue ID" `Quick
            test_local_message_has_no_event_id;
          test_case "partial event validation and moves" `Quick
            test_partial_event_validation_and_moves;
          test_case "shutdown settles sends" `Quick
            test_context_shutdown_settles_send;
          test_case "runner room scheduling and cleanup" `Quick
            test_runner_orders_rooms_and_stops_cleanly;
          test_case "scope replacing plugin" `Quick test_only_replacing_plugin;
          test_case "scope duplicate handler registration" `Quick
            test_only_distinguishes_duplicate_handler_registrations;
          test_case "runner flood drains" `Quick test_runner_flood_drains;
          test_case "runner survives handler exception" `Quick
            test_runner_handler_exception_continues;
          test_case "default activation and commands" `Quick
            test_default_activation_and_commands;
          test_case "webhook mention dispatch" `Quick
            test_webhook_routes_mention_once;
          test_case "queue expiration recovery" `Quick
            test_queue_recovers_without_repolling_expired_queue;
          test_case "terminal queue poll" `Quick test_queue_terminal_poll_stops;
        ] );
    ]
