open Crowthebot

let check label value = if not value then failwith label
let admin = "@admin:example.org"
let friend = "@friend:example.org"
let self = "@crow:example.org"
let dm = "!dm:example.org"
let room = "!room:example.org"
let other = "!other:example.org"
let encode value = Result.get_ok (Jsont_bytesrw.encode_string Jsont.json value)
let quote value = Result.get_ok (Jsont_bytesrw.encode_string Jsont.string value)

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let member name = function
  | Jsont.Object (fields, _) ->
      List.find_map
        (fun ((k, _), v) -> if k = name then Some v else None)
        fields
  | _ -> None

let items value =
  match member "items" value with
  | Some (Jsont.Array (values, _)) -> values
  | _ -> failwith "missing items"

let field name value =
  match member name value with
  | Some (Jsont.String (value, _)) -> value
  | _ -> failwith ("missing " ^ name)

let () =
  let log_buffer = Buffer.create 4096 in
  let log_formatter = Format.formatter_of_buffer log_buffer in
  Logs.set_reporter
    (Logs.format_reporter ~app:log_formatter ~dst:log_formatter ());
  Diagnostics.configure ~verbose:true;
  Eio_main.run @@ fun env ->
  let file = Filename.temp_file "crow-compaction-" ".sqlite3" in
  Fun.protect ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let p = file ^ suffix in
          if Sys.file_exists p then Sys.remove p)
        [ ""; "-journal"; "-wal"; "-shm" ])
  @@ fun () ->
  let path = Eio.Path.(Eio.Stdenv.fs env / file) in
  let now = ref 1788912000. in
  let config = Config.default ~admin ~homeserver:"https://matrix.example.org" in
  let mode = ref "normal" and requests = ref [] and summaries = ref [] in
  let during_compaction = ref (fun () -> ()) in
  let replies = ref 0 in
  let with_store f =
    Eio.Switch.run @@ fun sw ->
    let db = Sqlite3_eio.open_path ~sw path in
    f db (Store.create ~now:(fun () -> !now) db ~admin)
  in
  let inspect ?(after = 0) section =
    Eio.Switch.run @@ fun sw ->
    let db = Sqlite3_eio.open_path ~sw ~mode:`READONLY path in
    Inspect.read db ~section ~after ~limit:100
  in
  let rec inspect_all section after =
    let page = inspect ~after section in
    items page
    @
    match member "next_after" page with
    | Some (Jsont.Number (next, _)) -> inspect_all section (int_of_float next)
    | _ -> []
  in
  let initialize ?(config = config) store =
    let fetch =
      Fetch_mock.client (fun req ->
          let body =
            match req.body with Fetch.String body -> body | _ -> assert false
          in
          requests := body :: !requests;
          let compacting = contains body "Compact this Matrix conversation" in
          let observing = contains body "Observe a Matrix room silently" in
          let message =
            if compacting then begin
              summaries := body :: !summaries;
              !during_compaction ();
              check "summarizer has no tools"
                ((not (contains body "memory_store"))
                && not (contains body "cron_create"));
              if !mode = "error" then failwith "fixture summary failure";
              if !mode = "tools" then
                {|{"role":"assistant","content":null,"tool_calls":[{"id":"bad","type":"function","function":{"name":"memory_store","arguments":"{}"}}]}|}
              else
                let summary =
                  if !mode = "truncated" then {|{"summary":"cut off|}
                  else if !mode = "malformed" then "not JSON"
                  else if !mode = "empty" then {|{"summary":""}|}
                  else if !mode = "oversized" then
                    "{\"summary\":" ^ quote (String.make 6001 'x') ^ "}"
                  else if contains body "$ambient-1" then
                    {|{"summary":"The room speaker plans a Friday picnic (event $ambient-1). Unconfirmed."}|}
                  else
                    {|{"summary":"The user confirmed the office network is TestNetwork (event $dm-1)."}|}
                in
                "{\"role\":\"assistant\",\"content\":" ^ quote summary ^ "}"
            end
            else if observing then
              "{\"role\":\"assistant\",\"content\":"
              ^ quote
                  {|{"observation":"The speaker is planning an outing.","addressed":false}|}
              ^ "}"
            else {|{"role":"assistant","content":"Delivered answer."}|}
          in
          let reason =
            if compacting && !mode = "truncated" then "length" else "stop"
          in
          if compacting then begin
            let json =
              Result.get_ok (Jsont_bytesrw.decode_string Jsont.json body)
            in
            check "compaction has a separate completion budget"
              (member "max_completion_tokens" json = Some (Jsont.Json.int 4096))
          end;
          Fetch_mock.respond
            ~headers:
              (Http.Header.of_list [ ("content-type", "application/json") ])
            ("{\"id\":\"test\",\"model\":\"test\",\"created\":1,\"object\":\"chat.completion\",\"choices\":[{\"index\":0,\"finish_reason\":\""
           ^ reason ^ "\",\"message\":" ^ message ^ "}]}")
            req)
      |> Trace.wrap (Store.trace store)
      |> Openrouter.of_fetch ~base_url:"https://model.example/v1"
    in
    Engine.create ~config ~store ~self ~plugins:[]
      ~complete:(App.complete env config fetch) ~now:(fun () -> !now)
    |> Engine.with_room_observation
  in
  let handle engine ?(sender = admin) ?(room = dm) ?(direct = true) id body =
    Engine.handle engine ~direct
      ~send:(fun _ -> incr replies)
      Engine.{ room; sender; id; body }
  in
  let summary store scope =
    Compaction.context (Store.compaction store) scope ~bytes:10000
  in
  with_store (fun _ store ->
      Store.add_room store room;
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:true;
      let engine = initialize store in
      for i = 1 to 8 do
        handle engine
          ("$dm-" ^ string_of_int i)
          (if i = 1 then "OFFICE_ORIGINAL: my office network is TestNetwork."
           else "next question")
      done;
      check "compaction starts near the count limit, with no extra reply"
        (List.length !summaries = 1 && !replies = 8);
      check "summary input keeps source events and dates"
        (contains (List.hd !summaries) "$dm-1"
        && contains (List.hd !summaries) "2026-09-09T00:00:00Z");
      let history = Store.history store ~room:dm ~user:admin in
      check "older complete exchanges removed, eight recent messages retained"
        (List.length history = 8
        && (List.hd history).role = "user"
        && not
             (List.exists
                (fun (m : Store.message) -> contains m.body "OFFICE_ORIGINAL")
                history));
      check "summary does not automatically create shared facts"
        (Store.search_facts store ~actor:admin ~query:"" = []);
      check "summary preserves conversation scope"
        (summary store (Thread { room = dm; user = admin }) <> None
        && summary store (Thread { room = other; user = admin }) = None
        && summary store (Thread { room = dm; user = friend }) = None
        && summary store (Room dm) = None));
  now := !now +. 60.;
  with_store (fun _ store ->
      let engine = initialize store in
      let before = List.length !summaries in
      handle engine "$restart" "Where is my office?";
      let request = List.hd !requests in
      check "summary and recent window survive a real SQLite reopen"
        (contains request "TestNetwork"
        && contains request "Conversation summary"
        && (not (contains request "OFFICE_ORIGINAL"))
        && List.length !summaries = before);
      handle engine ~room:other "$other" "unrelated conversation";
      check "DM summary stays out of other rooms"
        (not (contains (List.hd !requests) "TestNetwork"));
      handle engine ~sender:friend "$friend" "my separate conversation";
      check "sender summaries remain separate"
        (not (contains (List.hd !requests) "TestNetwork"));
      for i = 9 to 12 do
        handle engine ("$dm-" ^ string_of_int i) "follow-up"
      done;
      check "incremental compaction receives the previous summary"
        (List.length !summaries > before
        && contains (List.hd !summaries) "user confirmed the office network");
      for i = 1 to 15 do
        handle engine ~sender:friend ~room ~direct:false
          ("$ambient-" ^ string_of_int i)
          (if i = 1 then "PICNIC_ORIGINAL: picnic Friday." else "room chat")
      done;
      check "ambient room context compacted without promoting claims to memory"
        (summary store (Room room) <> None
        && Store.search_facts store ~actor:admin ~query:"" = []);
      check "room summary never includes the DM"
        (not (contains (Option.get (summary store (Room room))) "TestNetwork"));
      let before = List.length !requests in
      handle engine ~sender:friend ~room ~direct:false "$ambient-1"
        "PICNIC_ORIGINAL: picnic Friday.";
      check "compacted event replay stays suppressed"
        (List.length !requests = before);
      handle engine ~room ~direct:false "$room-question"
        "!crow What are our plans?";
      let request =
        List.find
          (fun body -> not (contains body "Compact this Matrix conversation"))
          !requests
      in
      check "room summary reaches replies as attributed untrusted context"
        (contains request "Friday picnic"
        && contains request "untrusted JSON"
        && not (contains request "TestNetwork"));
      let saved = summary store (Thread { room = dm; user = admin }) in
      List.iter
        (fun failure ->
          mode := failure;
          let before = List.length !summaries and sent = !replies in
          for i = 1 to 10 do
            handle engine ("$" ^ failure ^ string_of_int i) "continue"
          done;
          check
            (failure
           ^ " leaves the last good summary and bounded recent history")
            (List.length !summaries > before
            && !replies = sent + 10
            && summary store (Thread { room = dm; user = admin }) = saved
            && List.length (Store.history store ~room:dm ~user:admin) <= 20))
        [ "error"; "malformed"; "empty"; "oversized"; "tools"; "truncated" ];
      mode := "normal";
      check "compaction tool hallucinations never execute"
        (Store.search_facts store ~actor:admin ~query:"" = []);
      let state = Store.compaction store in
      let plan =
        Option.get
          (Compaction.prepare state
             (Thread { room = dm; user = admin })
             ~max_messages:20 ~max_bytes:40000 ~incoming_messages:2
             ~incoming_bytes:100)
      in
      Store.clear store ~room:dm ~user:admin;
      check "reset invalidates in-flight summaries"
        ((not (Compaction.commit state plan ~body:"stale result"))
        && summary store (Thread { room = dm; user = admin }) = None);
      Store.set_person store ~actor:admin ~user:friend ~role:Friend
        ~allowed:false;
      check
        "revocation clears shared summaries containing compacted speaker data"
        (summary store (Room room) = None));
  let traces = inspect "traces" |> items in
  check "compaction exchanges retain complete provenance"
    (List.exists
       (fun row ->
         field "source" row = "context-compaction"
         && field "room" row = dm
         && contains (field "request" row) "OFFICE_ORIGINAL"
         && contains (field "response" row) "TestNetwork")
       traces);
  with_store (fun _ store ->
      let state = Store.compaction store in
      let append event body =
        Store.append store ~room:other ~user:admin ~event ~max_messages:20
          ~max_bytes:40000
          Store.[ { role = "user"; body }; { role = "assistant"; body } ]
      in
      for i = 1 to 4 do
        append ("$bytes-" ^ string_of_int i) (String.make 4000 'b')
      done;
      let plan =
        Option.get
          (Compaction.prepare state
             (Thread { room = other; user = admin })
             ~max_messages:20 ~max_bytes:40000 ~incoming_messages:2
             ~incoming_bytes:100)
      in
      check
        "byte threshold compacts before message count, with bounded model input"
        (String.length (Compaction.input plan) <= 40000
        && Compaction.limit plan = 6000);
      append "$concurrent" "arrived during compaction";
      check "concurrent history changes reject a stale summary"
        (not (Compaction.commit state plan ~body:"outdated")));
  with_store (fun _ store ->
      let engine = initialize store in
      let seed room =
        for i = 1 to 7 do
          Store.append store ~room ~user:admin
            ~event:("$seed-" ^ string_of_int i)
            ~max_messages:20 ~max_bytes:40000
            Store.
              [
                { role = "user"; body = "earlier question" };
                { role = "assistant"; body = "earlier answer" };
              ]
        done
      in
      let race = "!reset-race:example.org" in
      seed race;
      (during_compaction :=
         fun () ->
           check "delivered exchange is durable before compaction starts"
             (List.exists
                (fun (m : Store.message) -> m.body = "just delivered")
                (Store.history store ~room:race ~user:admin));
           Store.clear store ~room:race ~user:admin);
      handle engine ~room:race "$reset-race" "just delivered";
      (during_compaction := fun () -> ());
      check "reset during the model call cannot resurrect delivered context"
        (Store.history store ~room:race ~user:admin = []
        && summary store (Thread { room = race; user = admin }) = None);
      let cancelled = "!cancel:example.org" in
      seed cancelled;
      let pending, _ = Eio.Promise.create () in
      (during_compaction := fun () -> Eio.Promise.await pending);
      let timed_out =
        try
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.02 (fun () ->
              handle engine ~room:cancelled "$cancel-summary"
                "keep delivered message");
          false
        with Eio.Time.Timeout -> true
      in
      (during_compaction := fun () -> ());
      check "cancelled compaction retains the delivered exchange within bounds"
        (timed_out
        && List.length (Store.history store ~room:cancelled ~user:admin) <= 20
        && List.exists
             (fun (m : Store.message) -> m.body = "keep delivered message")
             (Store.history store ~room:cancelled ~user:admin));
      let small = "!small:example.org" in
      let engine =
        initialize
          ~config:{ config with context_messages = 4; context_bytes = 1024 }
          store
      in
      for i = 1 to 4 do
        handle engine ~room:small ("$small-" ^ string_of_int i) "small question"
      done;
      let answer =
        List.find
          (fun body -> not (contains body "Compact this Matrix conversation"))
          !requests
      in
      check "summaries remain usable at the smallest profile byte limit"
        (contains answer "Conversation summary" && contains answer "TestNetwork");
      handle engine ~room:small "$long-small" (String.make 500 'q');
      let request =
        List.find
          (fun body -> not (contains body "Compact this Matrix conversation"))
          !requests
      in
      let request =
        Result.get_ok (Jsont_bytesrw.decode_string Jsont.json request)
      in
      let history_bytes =
        match member "messages" request with
        | Some (Jsont.Array (messages, _)) ->
            List.fold_left
              (fun n message ->
                if field "role" message = "system" then n
                else n + String.length (field "content" message))
              0 messages
        | _ -> assert false
      in
      check
        "summary, recent messages and new prompt share the configured budget"
        (history_bytes <= 1024);
      let scheduled = "!scheduled:example.org" in
      seed scheduled;
      let fact =
        Store.add_fact store ~actor:admin ~room:scheduled
          ~event:"$scheduled-source" ~source:"command"
          ~body:"Report the status."
      in
      let id =
        Store.add_reminder store ~actor:admin ~room:scheduled
          ~event:"$scheduled-source" ~fact_id:fact
          ~instruction:"Report the status." ~cron:None ~until_at:None
          ~next_at:!now
      in
      let job = Option.get (Store.get_reminder store id) in
      ignore (Engine.fire (initialize store) ~send:(fun _ -> ()) job ~run_id:42));
  check "scheduled compaction retains the originating Matrix event"
    (List.exists
       (fun row ->
         field "source" row = "context-compaction"
         && field "source_event" row = "$scheduled-source"
         && String.starts_with ~prefix:"$cron-" (field "event" row))
       (inspect_all "traces" 0));
  with_store (fun _ store ->
      let escaped = "!escaped:example.org" in
      for i = 1 to 10 do
        let body = String.make 1000 '\000' in
        Store.append store ~room:escaped ~user:admin
          ~event:("$escaped-" ^ string_of_int i)
          ~max_messages:20 ~max_bytes:40000
          Store.[ { role = "user"; body }; { role = "assistant"; body } ]
      done;
      let state = Store.compaction store in
      let plan =
        Option.get
          (Compaction.prepare state
             (Thread { room = escaped; user = admin })
             ~max_messages:20 ~max_bytes:40000 ~incoming_messages:2
             ~incoming_bytes:100)
      in
      let json =
        Result.get_ok
          (Jsont_bytesrw.decode_string Jsont.json (Compaction.input plan))
      in
      let covered =
        match member "messages" json with
        | Some (Jsont.Array (values, _)) -> List.length values
        | _ -> assert false
      in
      check "JSON expansion produces a bounded partial batch"
        (covered > 0 && covered < 12
        && String.length (Compaction.input plan) <= 40000);
      check "partial compaction removes exactly its covered prefix"
        (Compaction.commit state plan ~body:"The sender posted opaque data."
        && List.length (Store.history store ~room:escaped ~user:admin)
           = 20 - covered));
  Eio.Switch.run (fun sw ->
      let db = Sqlite3_eio.open_memory ~sw () in
      ignore (Store.create db ~admin);
      Sqlite3.Rc.check
        (Sqlite3_eio.exec db
           {|DROP TABLE history;
        CREATE TABLE history (id INTEGER PRIMARY KEY,room TEXT,user TEXT,role TEXT,body TEXT);
        INSERT INTO history VALUES (41,'!legacy:example.org','@admin:example.org','user','legacy question');
        INSERT INTO history VALUES (42,'!legacy:example.org','@admin:example.org','assistant','legacy answer');
        DROP TABLE conversation_summaries;
        DROP TABLE room_observation_events;
        PRAGMA user_version=7;|});
      let store = Store.create db ~admin in
      let legacy = "!legacy:example.org" in
      check "version-seven migration preserves earlier exchanges"
        (List.map
           (fun (m : Store.message) -> m.body)
           (Store.history store ~room:legacy ~user:admin)
        = [ "legacy question"; "legacy answer" ]);
      Store.append store ~room:legacy ~user:admin ~event:"$new-event"
        ~max_messages:4 ~max_bytes:40000
        Store.
          [
            { role = "user"; body = "new question" };
            { role = "assistant"; body = "new answer" };
          ];
      let state = Store.compaction store in
      let plan =
        Option.get
          (Compaction.prepare state
             (Thread { room = legacy; user = admin })
             ~max_messages:4 ~max_bytes:40000 ~incoming_messages:4
             ~incoming_bytes:100)
      in
      check "migration allocates new monotonic row IDs with event provenance"
        (contains (Compaction.input plan) "\"id\":43"
        && contains (Compaction.input plan) "$new-event");
      check "migrated conversations can be compacted"
        (Compaction.commit state plan ~body:"Legacy and new questions.");
      Store.append store ~room:legacy ~user:admin ~event:"$after-compaction"
        ~max_messages:4 ~max_bytes:40000
        Store.
          [
            { role = "user"; body = "one more" };
            { role = "assistant"; body = "answer" };
          ];
      let next =
        Option.get
          (Compaction.prepare state
             (Thread { room = legacy; user = admin })
             ~max_messages:4 ~max_bytes:40000 ~incoming_messages:4
             ~incoming_bytes:100)
      in
      check
        "compaction cursors are never reused after deleting an entire prefix"
        (contains (Compaction.input next) "\"id\":45");
      let store = Store.create db ~admin in
      check "summary migration is idempotent"
        (summary store (Thread { room = legacy; user = admin }) <> None));
  Format.pp_print_flush log_formatter ();
  check
    "terminal compaction logs contain metadata without conversation or summary \
     bodies"
    (contains (Buffer.contents log_buffer) "Context compaction finished"
    && (not (contains (Buffer.contents log_buffer) "OFFICE_ORIGINAL"))
    && not (contains (Buffer.contents log_buffer) "TestNetwork"));
  print_endline
    "crowthebot: conversation compaction, restart, isolation, failure recovery \
     and provenance passed"
