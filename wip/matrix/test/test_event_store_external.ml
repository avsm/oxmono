module Store = Matrix_ui.Event_store
module Model = Store.Internal
module Raw = Matrix_proto.Event.Raw_event

let room_id = Matrix_proto.Id.Room_id.of_string_exn "!external:example.org"

let raw id body =
  match
    Jsont_bytesrw.decode_string Raw.jsont
      (Printf.sprintf
         {|{"event_id":"$%s","sender":"@alice:example.org","origin_server_ts":1700000000000,"type":"m.room.message","content":{"msgtype":"m.text","body":"%s"}}|}
         id body)
  with
  | Ok event -> event
  | Error message -> Alcotest.fail message

let stored ?clear_event stable_id event =
  { Model.stable_id; event; clear_event; delivery = Model.Synced }

let room ?(external_events = []) () =
  {
    Model.chunks =
      [
        Model.Events
          {
            chunk_id = 0;
            prev_token = None;
            next_token = None;
            events = [ stored "event:$timeline" (raw "timeline" "timeline") ];
          };
      ];
    next_chunk_id = 1;
    external_events;
  }

let with_temp_store f =
  let path = Filename.temp_file "matrix-ui-external-" ".sqlite3" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          let file = path ^ suffix in
          if Sys.file_exists file then Sys.remove file)
        [ ""; "-wal"; "-shm" ])
    (fun () -> f path)

let ids events = List.map (fun (event : Model.event) -> event.stable_id) events

let test_memory_bound_and_order () =
  Eio_main.run @@ fun _ ->
  let events =
    List.init (Model.max_external_events + 3) (fun index ->
        let id = Printf.sprintf "external-%03d" index in
        stored ~clear_event:(raw id "clear") ("event:$" ^ id) (raw id id))
  in
  let store = Store.memory ~plaintext_policy:Store.Store_plaintext () in
  Alcotest.(check unit)
    "memory save" ()
    (Result.get_ok
       (Store.save_room store room_id (room ~external_events:events ())));
  let loaded = Result.get_ok (Store.load_room store room_id) |> Option.get in
  Alcotest.(check int)
    "external bound" Model.max_external_events
    (List.length loaded.external_events);
  Alcotest.(check string)
    "oldest retained external event" "event:$external-003"
    (List.hd loaded.external_events).stable_id;
  Alcotest.(check string)
    "newest retained external event" "event:$external-258"
    (List.hd (List.rev loaded.external_events)).stable_id;
  Alcotest.(check bool)
    "cleartext retained in memory" true
    (Option.is_some (List.hd loaded.external_events).clear_event)

let test_external_identity_and_delivery_are_normalized () =
  Eio_main.run @@ fun _ ->
  let identifiable =
    {
      (stored "event:$kept" (raw "kept" "kept")) with
      delivery = Model.Failed "not a detached-event state";
    }
  in
  let without_id =
    let event = { (raw "missing" "missing") with Raw.event_id = None } in
    stored "event:$missing" event
  in
  let store = Store.memory () in
  ignore
    (Result.get_ok
       (Store.save_room store room_id
          (room ~external_events:[ without_id; identifiable ] ())));
  let loaded = Result.get_ok (Store.load_room store room_id) |> Option.get in
  Alcotest.(check (list string))
    "only identifiable detached events survive" [ "event:$kept" ]
    (ids loaded.external_events);
  match (List.hd loaded.external_events).delivery with
  | Model.Synced -> ()
  | Model.Sending | Model.Queued | Model.Failed _ ->
      Alcotest.fail "detached delivery was not normalized to Synced"

let test_sqlite_reload_policy_remove () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let external_events =
    [
      stored ~clear_event:(raw "old" "clear-old") "event:$old" (raw "old" "old");
      stored ~clear_event:(raw "new" "clear-new") "event:$new" (raw "new" "new");
    ]
  in
  let store =
    Result.get_ok
      (Matrix_ui_sqlite.create ~plaintext_policy:Store.Store_plaintext path)
  in
  ignore (Store.save_room store room_id (room ~external_events ()));
  Store.close store;
  let restored =
    Result.get_ok
      (Matrix_ui_sqlite.create ~plaintext_policy:Store.Store_plaintext path)
  in
  let loaded = Result.get_ok (Store.load_room restored room_id) |> Option.get in
  Alcotest.(check (list string))
    "SQLite order"
    [ "event:$old"; "event:$new" ]
    (ids loaded.external_events);
  Alcotest.(check bool)
    "SQLite cleartext" true
    (List.for_all
       (fun (event : Model.event) -> Option.is_some event.clear_event)
       loaded.external_events);
  Store.close restored;
  let ciphertext = Result.get_ok (Matrix_ui_sqlite.create path) in
  let loaded =
    Result.get_ok (Store.load_room ciphertext room_id) |> Option.get
  in
  Alcotest.(check bool)
    "Ciphertext_only omits cleartext" true
    (List.for_all
       (fun (event : Model.event) -> Option.is_none event.clear_event)
       loaded.external_events);
  Alcotest.(check unit)
    "remove room" ()
    (Result.get_ok (Store.remove_room ciphertext room_id));
  Alcotest.(check bool)
    "removed after reload" false
    (Result.get_ok (Store.load_room ciphertext room_id) |> Option.is_some);
  Store.close ciphertext

let encode event = Result.get_ok (Jsont_bytesrw.encode_string Raw.jsont event)

let test_current_schema_migration () =
  Eio_main.run @@ fun _ ->
  with_temp_store @@ fun path ->
  let db = Sqlite3.db_open path in
  let timeline_json = encode (raw "timeline" "timeline") in
  let timeline_json =
    String.concat "''" (String.split_on_char '\'' timeline_json)
  in
  let migration_sql =
    "CREATE TABLE ui_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL); INSERT \
     INTO ui_meta VALUES('schema_version','2'); CREATE TABLE ui_rooms (room_id \
     TEXT PRIMARY KEY, next_chunk_id INTEGER NOT NULL); INSERT INTO ui_rooms \
     VALUES('!external:example.org',1); CREATE TABLE ui_chunks (room_id TEXT \
     NOT NULL, chunk_id INTEGER NOT NULL, position INTEGER NOT NULL, kind TEXT \
     NOT NULL, token TEXT, next_token TEXT, PRIMARY KEY(room_id,chunk_id)); \
     INSERT INTO ui_chunks \
     VALUES('!external:example.org',0,0,'events',NULL,NULL); CREATE TABLE \
     ui_events (room_id TEXT NOT NULL, chunk_id INTEGER NOT NULL, position \
     INTEGER NOT NULL, stable_id TEXT NOT NULL, event_json TEXT NOT NULL, \
     clear_json TEXT, delivery TEXT NOT NULL, error TEXT, PRIMARY \
     KEY(room_id,stable_id)); INSERT INTO ui_events \
     VALUES('!external:example.org',0,0,'event:$timeline','" ^ timeline_json
    ^ "',NULL,'synced',NULL); CREATE INDEX ui_chunks_order ON \
       ui_chunks(room_id,position); CREATE INDEX ui_events_order ON \
       ui_events(room_id,chunk_id,position)"
  in
  (match Sqlite3.exec db migration_sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Alcotest.failf "migration SQL: %s (%s)" (Sqlite3.Rc.to_string rc)
        (Sqlite3.errmsg db));
  ignore (Sqlite3.db_close db);
  let store = Result.get_ok (Matrix_ui_sqlite.create path) in
  let loaded = Result.get_ok (Store.load_room store room_id) |> Option.get in
  Alcotest.(check (list string))
    "migration preserves timeline" [ "event:$timeline" ]
    (ids (Model.room_events loaded));
  ignore
    (Store.save_room store room_id
       (room
          ~external_events:
            [ stored "event:$external" (raw "external" "external") ]
          ()));
  Store.close store;
  let restored = Result.get_ok (Matrix_ui_sqlite.create path) in
  let loaded = Result.get_ok (Store.load_room restored room_id) |> Option.get in
  Alcotest.(check (list string))
    "migrated external table works" [ "event:$external" ]
    (ids loaded.external_events);
  Store.close restored

let () =
  Alcotest.run "event store external"
    [
      ( "storage",
        [
          Alcotest.test_case "memory bound and order" `Quick
            test_memory_bound_and_order;
          Alcotest.test_case "identity and delivery normalization" `Quick
            test_external_identity_and_delivery_are_normalized;
          Alcotest.test_case "SQLite reload, policy and remove" `Quick
            test_sqlite_reload_policy_remove;
          Alcotest.test_case "current schema migration" `Quick
            test_current_schema_migration;
        ] );
    ]
