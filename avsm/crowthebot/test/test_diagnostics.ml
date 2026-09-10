open Crowthebot

let check name value = if not value then failwith name
let self = "@crow:example.org"
let admin = "@admin:example.org"
let alice = "@alice:example.org"
let room = "!room:example.org"
let secret = "SENSITIVE_SENTINEL"

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let raw ~id ~sender ~kind content =
  Result.get_ok
    (Jsont_bytesrw.decode_string Matrix_proto.Event.Raw_event.jsont
       (Printf.sprintf
          {|{"event_id":%S,"sender":%S,"origin_server_ts":1700000000000,"type":%S,"content":%s}|}
          id sender kind content))

let () =
  let buffer = Buffer.create 4096 in
  let formatter = Format.formatter_of_buffer buffer in
  Logs.set_reporter (Logs.format_reporter ~app:formatter ~dst:formatter ());
  let logs () =
    Format.pp_print_flush formatter ();
    Buffer.contents buffer
  in
  Diagnostics.configure ~verbose:false;
  Diagnostics.Log.info (fun m -> m "quiet startup");
  check "default is quiet" (logs () = "" && not (Diagnostics.enabled ()));
  Diagnostics.configure ~verbose:true;
  Logs.info (fun m -> m "%s" secret);
  let transport = Logs.Src.create "test.transport" in
  Logs.info ~src:transport (fun m -> m "%s" secret);
  check "verbose does not enable transport bodies" (logs () = "");
  check "verbose is enabled" (Diagnostics.enabled ());
  List.iter
    (fun exn ->
      let summary = Diagnostics.error exn in
      check "error summary is useful" (summary <> "");
      check "error summary redacted" (not (contains summary secret)))
    [
      Failure secret;
      Invalid_argument secret;
      Eio.Time.Timeout;
      Matrix_eio.Error.err (Http { status = 401; body = secret });
      Matrix_eio.Error.err
        (Matrix
           {
             errcode = M_UNKNOWN_CODE secret;
             error = secret;
             retry_after_ms = None;
           });
      Eio.Exn.create
        (Openrouter.E
           (Http_error { status = 429; code = Some secret; message = secret }));
    ];
  check "HTTP status retained"
    (Diagnostics.error
       (Matrix_eio.Error.err (Http { status = 401; body = secret }))
    = "Matrix HTTP 401");
  check "Matrix code retained"
    (Diagnostics.sent
       (Failed
          (Some
             (Matrix_error
                {
                  errcode = M_FORBIDDEN;
                  error = secret;
                  retry_after_ms = None;
                  soft_logout = None;
                })))
    = "Matrix M_FORBIDDEN");
  check "sync tokens hidden"
    (Diagnostics.sync (Live { batch = secret }) = "live");
  check "sync error body hidden" (Diagnostics.sync (Failed secret) = "failed");
  Eio_main.run @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let db = Sqlite3_eio.open_memory ~sw () in
  let store = Store.create db ~admin in
  Store.add_room store room;
  Diagnostics.configure ~verbose:false;
  Diagnostics.Log.info (fun m -> m "hidden-runtime-info");
  let access =
    Memory.for_request store ~actor:admin ~room ~event:"memory"
      ~source:"command"
  in
  List.iter
    (fun (tool, arguments) ->
      ignore
        (Audit.run store ~actor:admin ~room ~event:"memory" ~source:"command"
           ~call_id:"" ~tool ~arguments (fun () ->
             Memory.invoke access tool arguments)))
    [
      ("memory_store", Printf.sprintf {|{"fact":%S}|} secret);
      ("memory_search", Printf.sprintf {|{"query":%S}|} secret);
      ("memory_get", {|{"id":1}|});
      ("memory_erase", {|{"id":1}|});
    ];
  (try
     ignore
       (Audit.run store ~actor:admin ~room ~event:"scheduled"
          ~source:"scheduler" ~call_id:"1" ~tool:"caldav_sync" ~arguments:secret
          (fun () -> failwith secret))
   with Failure _ -> ());
  List.iter
    (fun text ->
      check "tool activity visible without verbose" (contains (logs ()) text))
    [
      "Tool started";
      "Tool finished";
      "status=ok";
      "status=error";
      "source=\"scheduler\"";
      "Memory stored fact_id=1";
      "Memory searched results=1";
      "Memory retrieved fact_id=1 found=true";
      "Memory erased fact_id=1 removed=true";
    ];
  check "default tool logging keeps contents and runtime chatter private"
    ((not (contains (logs ()) secret))
    && not (contains (logs ()) "hidden-runtime-info"));
  Buffer.clear buffer;
  Diagnostics.configure ~verbose:true;
  let clock = ref 0. and calls = ref 0 and replies = ref 0 in
  let engine =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://example.org")
      ~store ~self ~plugins:[]
      ~complete:(fun _ _ ->
        incr calls;
        (Some secret, []))
      ~now:(fun () -> !clock)
  in
  let send _ = incr replies in
  let event ?(sender = admin) ?(room = room) id body =
    Engine.{ sender; room; id; body }
  in
  let handle ?(direct = false) e = Engine.handle engine ~direct ~send e in
  handle (event "ambient" secret);
  handle (event ~room:"!disabled:example.org" "disabled" ("!crow " ^ secret));
  handle ~direct:true (event ~sender:alice "unknown" secret);
  handle ~direct:true (event "dm" secret);
  handle ~direct:true (event "follow-up" secret);
  clock := 20.;
  handle ~direct:true (event "dm" secret);
  check "logging preserves approval and replay without dropping follow-ups"
    (!calls = 2 && !replies = 2);
  List.iter
    (fun reason -> check reason (contains (logs ()) reason))
    [
      "not-addressed-or-empty";
      "room-not-enabled-and-not-a-verified-DM";
      "sender-not-approved";
      "Accepted";
      "direct=true";
      "event-already-claimed";
    ];
  List.iter
    (fun result ->
      ignore
        (Audit.run store ~actor:admin ~room ~event:"tool" ~source:"model"
           ~call_id:"call" ~tool:"example" ~arguments:secret (fun () -> result)))
    [ Ok secret; Error secret ];
  check "tool outcomes logged"
    (contains (logs ()) "Tool started"
    && contains (logs ()) "status=ok"
    && contains (logs ()) "status=rejected"
    && contains (logs ())
         (Printf.sprintf "argument_bytes=%d" (String.length secret))
    && contains (logs ())
         (Printf.sprintf "result_bytes=%d" (String.length secret)));
  let step = ref 0 in
  let sequence =
    [
      "location_sources";
      "location_list";
      "location_get";
      "location_resolve";
      "location_get";
      "location_get";
    ]
  in
  let failing =
    Engine.create
      ~config:(Config.default ~admin ~homeserver:"https://example.org")
      ~store ~self ~plugins:[]
      ~now:(fun () -> 0.)
      ~complete:(fun _ tools ->
        incr step;
        if !step <= 6 then begin
          check "tools remain available for all six calls" (tools <> []);
          ( None,
            [
              Openrouter.Tool.
                {
                  id = string_of_int !step;
                  name = List.nth sequence (!step - 1);
                  arguments =
                    "{\"latitude\":51.5074,\"secret\":\"" ^ secret ^ "\"}";
                };
            ] )
        end
        else begin
          check "seventh request synthesizes without tools" (tools = []);
          (None, [])
        end)
  in
  let fallback = ref "" in
  Engine.handle failing ~direct:true
    ~send:(fun text -> fallback := text)
    (event "synthesis-failure" "hi");
  check
    "empty terminal response retries once then delivers fallback with complete \
     tool sequence"
    (!step = 8
    && contains !fallback "tool activity is saved"
    && contains (logs ()) "Model returned no text"
    && contains (logs ()) "tools_remaining=0"
    && contains (logs ()) "tool_sequence=[1:location_sources("
    && contains (logs ()) "2:location_list("
    && contains (logs ()) "3:location_get("
    && contains (logs ()) "4:location_resolve("
    && contains (logs ()) "6:location_get("
    && not (contains (logs ()) "51.5074"));
  let metadata = ref None in
  ignore
    (Audit.run store ~actor:admin ~room ~event:"wifi-audit" ~source:"model"
       ~call_id:"wifi" ~tool:"location_get" ~arguments:{|{"person":"Alice"}|}
       (fun () ->
         Ok
           {|{"wifi_ssid":"PRIVATE_WIFI_SENTINEL","wifi_bssid":"02:11:22:33:44:55"}|}));
  check "Wi-Fi details stay out of terminal logs"
    ((not (contains (logs ()) "PRIVATE_WIFI_SENTINEL"))
    && not (contains (logs ()) "02:11:22:33:44:55"));
  ignore
    (Audit.run
       ~on_finish:(fun s -> metadata := Some s)
       store ~actor:admin ~room ~event:"location-audit" ~source:"model"
       ~call_id:"location" ~tool:"location_resolve" ~arguments:secret
       (fun () -> Ok (String.make 5000 'x' ^ secret)));
  check "metadata counts unredacted sizes without retaining contents"
    (match !metadata with
    | Some s ->
        s.argument_bytes = String.length secret
        && s.result_bytes = 5000 + String.length secret
        && s.status = "ok"
    | None -> false);
  let cache = Matrix_ui.Event_cache.create () in
  let room_id = Matrix_proto.Id.Room_id.of_string_exn room in
  let self_id = Matrix_proto.Id.User_id.of_string_exn self in
  let encrypted =
    raw ~id:"$encrypted" ~sender:admin ~kind:"m.room.encrypted" "{}"
  in
  let own = raw ~id:"$own" ~sender:self ~kind:"m.room.encrypted" "{}" in
  Matrix_ui.Event_cache.prepend cache room_id ~events:[ encrypted; own ]
    ~prev_batch:None;
  let stop = Diagnostics.watch_room ~sw ~self:self_id ~cache ~room:room_id in
  check "encrypted DM is visible before handler delivery"
    (contains (logs ()) "encrypted_pending=1");
  let plaintext =
    raw ~id:"$encrypted" ~sender:admin ~kind:"m.room.message"
      (Printf.sprintf {|{"msgtype":"m.text","body":%S}|} secret)
  in
  check "install room key result"
    (Matrix_ui.Event_cache.set_decrypted cache room_id ~encrypted ~plaintext);
  Eio.Fiber.yield ();
  check "decryption recovery is visible"
    (contains (logs ()) "encrypted_pending=0");
  check "decrypted body, tool payloads and errors omitted"
    (not (contains (logs ()) secret));
  stop ();
  Eio.Fiber.yield ();
  let before = logs () in
  Matrix_ui.Event_cache.prepend cache room_id
    ~events:[ raw ~id:"$later" ~sender:admin ~kind:"m.room.encrypted" "{}" ]
    ~prev_batch:None;
  Eio.Fiber.yield ();
  check "room watcher stops on leave" (logs () = before);
  Diagnostics.configure ~verbose:false;
  Diagnostics.Log.info (fun m -> m "quiet again");
  check "verbose can be disabled" (logs () = before);
  print_endline
    "crowthebot: diagnostic routing, decryption and redaction passed"
