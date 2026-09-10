open Crowthebot

let check name value = if not value then failwith name
let admin = "@admin:example.org"
let room = "!dm:example.org"

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let encode json = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json)

let member name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((k, _), v) -> if k = name then Some v else None)
        fields
  | _ -> None

let items json =
  match member "items" json with
  | Some (Jsont.Array (items, _)) -> items
  | _ -> assert false

let field name json =
  match member name json with
  | Some (Jsont.String (s, _)) -> s
  | _ -> assert false

let () =
  Eio_main.run @@ fun env ->
  let file = Filename.temp_file "crow-restart-" ".sqlite3" in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let p = file ^ suffix in
          if Sys.file_exists p then Sys.remove p)
        [ ""; "-journal"; "-wal"; "-shm" ])
  @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / file) in
  let clock = ref 0. in
  let with_store f =
    Eio.Switch.run @@ fun sw ->
    let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
    let store = Store.create ~now:(fun () -> !clock) db ~admin in
    f sw db store
  in
  let inspect section =
    Eio.Switch.run @@ fun sw ->
    let db =
      Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path
    in
    Inspect.read db ~section ~after:0 ~limit:100
  in
  let once, bounded, forever =
    with_store (fun _ _ store ->
        Store.add_direct_room store ~room ~peer:admin;
        let fact_id =
          Store.add_fact store ~actor:admin ~room ~event:"$source"
            ~source:"command" ~body:"Track my location for six hours."
        in
        let add cron until_at =
          Store.add_reminder store ~actor:admin ~room ~event:"$source" ~fact_id
            ~instruction:"Report my location here." ~cron ~until_at ~next_at:60.
        in
        ( add None None,
          add (Some "* * * * *") (Some 180.),
          add (Some "* * * * *") None ))
  in
  clock := 90.;
  let fired = ref [] in
  let fire (job : Store.reminder) ~run_id:_ =
    check "source and memory survive restart"
      (job.room = room && job.creator = admin && job.event = "$source"
      && job.target = Store.Memory 1);
    fired := job.reminder_id :: !fired;
    "delivered"
  in
  with_store (fun _ _ store ->
      check "DM delivery mapping survives restart"
        (Store.direct_peer store room = Some admin);
      Cron.run_due ~ready:(fun () -> false) store ~fire;
      check "startup cannot consume due reminders"
        (!fired = [] && items (inspect "runs") = []);
      Cron.run_due ~ready:(fun () -> true) store ~fire;
      check "overdue one-off and recurring jobs recover"
        (List.sort compare !fired = List.sort compare [ once; bounded; forever ]));
  with_store (fun _ _ store ->
      Cron.run_due store ~fire;
      check "restart does not replay delivered work" (List.length !fired = 3));
  clock := 300.;
  with_store (fun _ _ store ->
      Cron.run_due store ~fire;
      check "expired bounded work does not fire, missed intervals coalesce"
        (List.length !fired = 4 && List.hd !fired = forever);
      let job = Option.get (Store.get_reminder store forever) in
      check "recurrence advances beyond downtime" (job.next_at = 360.);
      clock := 360.;
      check "simulate crash after durable claim"
        (Store.claim_reminder store job ~next_at:(Some 420.) <> None));
  with_store (fun _ _ store ->
      check "crashed occurrence marked interrupted"
        (List.exists
           (fun row -> field "status" row = "interrupted")
           (items (inspect "runs")));
      Cron.run_due store ~fire;
      check "claimed action is not replayed" (List.length !fired = 4);
      clock := 420.;
      Cron.run_due store ~fire;
      check "recurrence continues after interrupted occurrence"
        (List.length !fired = 5));
  with_store (fun _ _ store ->
      let requests = ref [] in
      let raw_response =
        {|{"id":"completion-1","model":"test","created":1,"object":"chat.completion","extra":"preserved","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"**Hello**","reasoning":"private reasoning"}}]}|}
      in
      let client =
        Openrouter.of_fetch ~base_url:"https://model.example/v1"
          ~api_key:"AUTH_SECRET_SENTINEL"
          (Trace.wrap (Store.trace store)
             (Fetch_mock.client (fun req ->
                  requests :=
                    (match req.body with
                    | Fetch.String body -> body
                    | _ -> assert false)
                    :: !requests;
                  let page = inspect "traces" in
                  check "request is visible to external reader before response"
                    (List.exists
                       (fun row -> field "status" row = "running")
                       (items page));
                  Fetch_mock.respond
                    ~headers:
                      (Http.Header.of_list
                         [ ("content-type", "application/json") ])
                    raw_response req)))
      in
      let context event =
        Trace.
          {
            actor = admin;
            room;
            event;
            source_event = event;
            source = "message";
          }
      in
      let call event =
        Trace.with_context (context event) (fun () ->
            ignore
              (Openrouter.Chat.complete client
                 (Openrouter.Chat.request ~model:"test"
                    ~messages:
                      [
                        Openrouter.Message.user "trace this entire user message";
                      ]
                    ())))
      in
      Eio.Fiber.both (fun () -> call "$first") (fun () -> call "$second");
      let traces = items (inspect "traces") in
      check "concurrent exchanges retain their own provenance"
        (List.sort compare (List.map (field "event") traces)
        = [ "$first"; "$second" ]);
      check "raw request and response bodies retained exactly"
        (List.for_all
           (fun row ->
             field "request" row = List.hd !requests
             && field "response" row = raw_response
             && field "status" row = "ok")
           traces);
      check "HTTP credential omitted"
        (not (contains (encode (inspect "traces")) "AUTH_SECRET_SENTINEL"));
      let failing response =
        Openrouter.of_fetch ~base_url:"https://model.example/v1"
          (Trace.wrap (Store.trace store) (Fetch_mock.client response))
      in
      let expect_failure client =
        check "model failure propagates"
          (try
             ignore
               (Openrouter.Chat.complete client
                  (Openrouter.Chat.request ~model:"test"
                     ~messages:[ Openrouter.Message.user "hello" ]
                     ()));
             false
           with _ -> true)
      in
      expect_failure
        (failing (fun req ->
             Fetch_mock.respond ~status:503
               {|{"error":{"message":"server failure preserved"}}|} req));
      expect_failure
        (failing (fun _ -> failwith "transport secret must not appear"));
      expect_failure
        (failing (fun req ->
             Fetch_mock.respond (String.make ((1024 * 1024) + 1) 'x') req));
      let traces = items (inspect "traces") in
      check "HTTP failure and complete error body recorded"
        (List.exists
           (fun row ->
             field "status" row = "http-error"
             && contains (field "response" row) "server failure preserved")
           traces);
      check "oversize failure is explicit and bounded"
        (List.exists
           (fun row ->
             field "status" row = "response-too-large"
             && String.length (field "response" row) = 1024 * 1024)
           traces);
      check "exception text is excluded"
        (not
           (contains
              (encode (inspect "traces"))
              "transport secret must not appear")));
  with_store (fun _ _ _ ->
      check "traces survive close and reopen"
        (List.length (items (inspect "traces")) = 5));
  with_store (fun _ db store ->
      let first = ref true in
      let client =
        Openrouter.of_fetch ~base_url:"https://model.example/v1"
          (Trace.wrap (Store.trace store)
             (Fetch_mock.client (fun req ->
                  let body =
                    if !first then begin
                      first := false;
                      {|{"id":"tool-roundtrip","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,"tool_calls":[{"id":"remember","type":"function","function":{"name":"memory_store","arguments":"{\"fact\":\"provenance fact\"}"}}]}}]}|}
                    end
                    else
                      {|{"id":"tool-finished","model":"test","created":1,"object":"chat.completion","choices":[{"index":0,"finish_reason":"stop","message":{"role":"assistant","content":"Remembered."}}]}|}
                  in
                  Fetch_mock.respond
                    ~headers:
                      (Http.Header.of_list
                         [ ("content-type", "application/json") ])
                    body req)))
      in
      let config =
        Config.default ~admin ~homeserver:"https://matrix.example.org"
      in
      let engine =
        Engine.create ~config ~store ~self:"@crow:example.org" ~plugins:[]
          ~complete:(App.complete env config client) ~now:(fun () -> !clock)
      in
      let reply = ref "" in
      Engine.handle engine ~direct:true
        ~send:(fun text -> reply := text)
        { sender = admin; room; id = "$tools"; body = "Remember this." };
      let traces =
        items (inspect "traces")
        |> List.filter (fun row -> field "event" row = "$tools")
      in
      check "each model roundtrip is traced through the actual agent"
        (!reply = "Remembered."
        && List.length traces = 2
        && List.for_all
             (fun row ->
               field "source" row = "message"
               && field "source_event" row = "$tools")
             traces);
      check "tool results return in the next recorded request"
        (List.exists
           (fun row ->
             contains (field "request" row) "tool_call_id"
             && contains (field "request" row) "Stored fact")
           traces);
      List.iter
        (fun suffix ->
          let p = file ^ suffix in
          if Sys.file_exists p then
            check "trace database and WAL remain private"
              ((Unix.stat p).st_perm land 0o077 = 0))
        [ ""; "-wal"; "-shm" ];
      Sqlite3.Rc.check
        (Sqlite3_eio.exec db
           "INSERT INTO \
            model_traces(started_at,actor,room,event,source_event,source,request,status) \
            VALUES('1970-01-01T00:07:00Z','','local','$interrupted','','probe','{}','running')"));
  with_store (fun _ _ _ ->
      check "unfinished model exchange recovered after reopening"
        (List.exists
           (fun row ->
             field "event" row = "$interrupted"
             && field "status" row = "interrupted")
           (items (inspect "traces"))));
  let html =
    Rich_text.html
      "# Heading\n\n\
       **bold**, *italic*, `code`, [link](https://example.org).\n\n\
       - first\n\
       - second\n\n\
       ```ocaml\n\
       let x = 1 < 2\n\
       ```\n\n\
       | A | B |\n\
       |---|---|\n\
       | 1 | 2 |\n"
  in
  List.iter
    (fun fragment -> check fragment (contains html fragment))
    [
      "<strong>Heading</strong>";
      "<strong>bold</strong>";
      "<em>italic</em>";
      "<code>code</code>";
      "<ul>";
      "<pre>";
      "&lt;";
      "<table>";
      "https://example.org";
    ];
  let unsafe =
    Rich_text.html
      "[bad](javascript:alert%281%29)\n\n<script>alert('oops')</script>"
  in
  check "unsafe Markdown cannot inject active HTML"
    ((not (contains unsafe "javascript:")) && not (contains unsafe "<script"));
  print_endline
    "crowthebot: real restart recovery, live inspection, full traces and rich \
     replies passed"
