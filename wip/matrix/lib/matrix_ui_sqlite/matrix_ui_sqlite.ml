module Sqlite3 = struct
  include Sqlite3

  let column_is_null stmt index = column stmt index = Data.NULL
end

module Id = Matrix_proto.Id
module Raw = Matrix_proto.Event.Raw_event
module Store = Matrix_ui.Event_store
module Model = Matrix_ui.Event_store.Internal
module Media_store = Matrix_client.Media_store

let src = Logs.Src.create "matrix.ui.sqlite" ~doc:"Matrix SQLite stores"

module Log = (val Logs.src_log src : Logs.LOG)

exception Codec_error of string
exception Sqlite_backend_error of string
exception Backend_refused of string

let with_io_context label f =
  try f ()
  with Eio.Io _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Exn.reraise_with_context exn bt "%s" label

let run_systhread ~label f =
  with_io_context label (fun () -> Eio_unix.run_in_systhread ~label f)

let close_db_in_systhread_best_effort ~label db =
  try ignore (Sqlite3.db_close db) with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | Eio.Io _ as exn ->
      let contextual = Eio.Exn.add_context exn "%s" label in
      let message =
        Format.asprintf "%s failed during cleanup: %a" label Eio.Exn.pp
          contextual
      in
      Log.warn (fun m -> m "%s" message)
  | Sqlite3.Error message | Sqlite3.SqliteError message ->
      Log.warn (fun m -> m "%s failed during cleanup: sqlite: %s" label message)

let close_db_best_effort ~label db =
  try
    run_systhread ~label (fun () -> close_db_in_systhread_best_effort ~label db)
  with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | Eio.Io _ as exn ->
      let contextual = Eio.Exn.add_context exn "%s" label in
      let message =
        Format.asprintf "%s failed during cleanup: %a" label Eio.Exn.pp
          contextual
      in
      Log.warn (fun m -> m "%s" message)
  | Sqlite3.Error message | Sqlite3.SqliteError message ->
      Log.warn (fun m -> m "%s failed during cleanup: sqlite: %s" label message)

let schema_version = 3

let schema =
  {|
CREATE TABLE IF NOT EXISTS ui_rooms (
  room_id TEXT PRIMARY KEY,
  next_chunk_id INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS ui_chunks (
  room_id TEXT NOT NULL,
  chunk_id INTEGER NOT NULL,
  position INTEGER NOT NULL,
  kind TEXT NOT NULL,
  token TEXT,
  next_token TEXT,
  PRIMARY KEY (room_id, chunk_id)
);
CREATE INDEX IF NOT EXISTS ui_chunks_order ON ui_chunks(room_id, position);
CREATE TABLE IF NOT EXISTS ui_events (
  room_id TEXT NOT NULL,
  chunk_id INTEGER NOT NULL,
  position INTEGER NOT NULL,
  stable_id TEXT NOT NULL,
  event_json TEXT NOT NULL,
  clear_json TEXT,
  delivery TEXT NOT NULL,
  error TEXT,
  PRIMARY KEY (room_id, stable_id)
);
CREATE INDEX IF NOT EXISTS ui_events_order
  ON ui_events(room_id, chunk_id, position);
CREATE TABLE IF NOT EXISTS ui_external_events (
  room_id TEXT NOT NULL,
  event_id TEXT NOT NULL,
  stable_id TEXT NOT NULL,
  event_json TEXT NOT NULL,
  clear_json TEXT,
  ordinal INTEGER NOT NULL,
  PRIMARY KEY (room_id, event_id)
);
CREATE INDEX IF NOT EXISTS ui_external_events_order
  ON ui_external_events(room_id, ordinal);
|}

let drop_schema =
  {|
DROP TABLE IF EXISTS ui_events;
DROP TABLE IF EXISTS ui_external_events;
DROP TABLE IF EXISTS ui_chunks;
DROP TABLE IF EXISTS ui_rooms;
|}

(* Version 2 is the current timeline schema. Adding detached events is
   intentionally additive, so a restart does not throw away the timeline. *)
let external_schema =
  {|
CREATE TABLE IF NOT EXISTS ui_external_events (
  room_id TEXT NOT NULL,
  event_id TEXT NOT NULL,
  stable_id TEXT NOT NULL,
  event_json TEXT NOT NULL,
  clear_json TEXT,
  ordinal INTEGER NOT NULL,
  PRIMARY KEY (room_id, event_id)
);
CREATE INDEX IF NOT EXISTS ui_external_events_order
  ON ui_external_events(room_id, ordinal);
|}

let statement db sql f =
  let statement = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize statement))
    (fun () -> f statement)

let stored_version db =
  let version = ref None in
  statement db "SELECT value FROM ui_meta WHERE key='schema_version'"
    (fun stmt ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          version := int_of_string_opt (Sqlite3.column_text stmt 0)
      | Sqlite3.Rc.DONE -> ()
      | rc -> Sqlite3.Rc.check rc);
  !version

let ensure_schema db =
  Sqlite3.Rc.check (Sqlite3.exec db "PRAGMA journal_mode=WAL");
  Sqlite3.Rc.check
    (Sqlite3.exec db
       "CREATE TABLE IF NOT EXISTS ui_meta (key TEXT PRIMARY KEY, value TEXT \
        NOT NULL)");
  (match stored_version db with
  | Some version when version = schema_version -> ()
  | Some 2 -> Sqlite3.Rc.check (Sqlite3.exec db external_schema)
  | _ -> Sqlite3.Rc.check (Sqlite3.exec db drop_schema));
  Sqlite3.Rc.check (Sqlite3.exec db schema);
  statement db
    "INSERT OR REPLACE INTO ui_meta(key,value) VALUES('schema_version',?)"
    (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 (string_of_int schema_version));
      Sqlite3.Rc.check (Sqlite3.step stmt))

let encode event = Jsont_bytesrw.encode_string Raw.jsont event
let decode encoded = Jsont_bytesrw.decode_string Raw.jsont encoded

let delivery_to_columns = function
  | Model.Synced -> ("synced", None)
  | Model.Sending -> ("sending", None)
  | Model.Queued -> ("queued", None)
  | Model.Failed message -> ("failed", Some message)

let delivery_of_columns status error =
  match status with
  | "synced" -> Model.Synced
  | "sending" -> Model.Sending
  | "queued" -> Model.Queued
  | "failed" -> Model.Failed (Option.value error ~default:"send failed")
  | _ -> Model.Failed ("unknown persisted delivery state: " ^ status)

let encoded_event (event : Model.event) =
  let encode_one event =
    match encode event with
    | Ok value -> value
    | Error message -> raise (Codec_error message)
  in
  (encode_one event.event, Option.map encode_one event.clear_event)

let bound_external_events events =
  let excess = List.length events - Model.max_external_events in
  if excess <= 0 then events else List.drop excess events

let put_event_sql =
  "INSERT OR REPLACE INTO \
   ui_events(room_id,chunk_id,position,stable_id,event_json,clear_json,delivery,error) \
   VALUES(?,?,?,?,?,?,?,?)"

let bind_put stmt room_id ~chunk_id ~position (event : Model.event) =
  let encoded, clear = encoded_event event in
  let delivery, error = delivery_to_columns event.delivery in
  Sqlite3.Rc.check
    (Sqlite3.bind_values stmt
       [
         Sqlite3.Data.TEXT room_id;
         Sqlite3.Data.INT (Int64.of_int chunk_id);
         Sqlite3.Data.INT (Int64.of_int position);
         Sqlite3.Data.TEXT event.stable_id;
         Sqlite3.Data.TEXT encoded;
         Sqlite3.Data.opt_text clear;
         Sqlite3.Data.TEXT delivery;
         Sqlite3.Data.opt_text error;
       ]);
  Sqlite3.Rc.check (Sqlite3.step stmt);
  Sqlite3.Rc.check (Sqlite3.reset stmt);
  Sqlite3.Rc.check (Sqlite3.clear_bindings stmt)

let external_event_id (event : Model.event) =
  match event.event.Raw.event_id with
  | Some event_id -> Id.Event_id.to_string event_id
  | None -> raise (Codec_error "external event has no event id")

let bind_external stmt room_id ~ordinal (event : Model.event) =
  let encoded, clear = encoded_event event in
  Sqlite3.Rc.check
    (Sqlite3.bind_values stmt
       [
         Sqlite3.Data.TEXT room_id;
         Sqlite3.Data.TEXT (external_event_id event);
         Sqlite3.Data.TEXT event.stable_id;
         Sqlite3.Data.TEXT encoded;
         Sqlite3.Data.opt_text clear;
         Sqlite3.Data.INT (Int64.of_int ordinal);
       ]);
  Sqlite3.Rc.check (Sqlite3.step stmt);
  Sqlite3.Rc.check (Sqlite3.reset stmt);
  Sqlite3.Rc.check (Sqlite3.clear_bindings stmt)

let chunk_id_of = function
  | Model.Events chunk -> chunk.chunk_id
  | Model.Gap gap -> gap.gap_id

let write_layout db room_id (room : Model.room) =
  statement db
    "INSERT OR REPLACE INTO ui_rooms(room_id,next_chunk_id) VALUES(?,?)"
    (fun stmt ->
      Sqlite3.Rc.check
        (Sqlite3.bind_values stmt
           [
             Sqlite3.Data.TEXT room_id;
             Sqlite3.Data.INT (Int64.of_int room.next_chunk_id);
           ]);
      Sqlite3.Rc.check (Sqlite3.step stmt));
  statement db "DELETE FROM ui_chunks WHERE room_id=?" (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
      Sqlite3.Rc.check (Sqlite3.step stmt));
  statement db
    "INSERT INTO ui_chunks(room_id,chunk_id,position,kind,token,next_token) \
     VALUES(?,?,?,?,?,?)" (fun stmt ->
      List.iteri
        (fun position chunk ->
          let chunk_id, kind, token, next_token =
            match chunk with
            | Model.Events chunk ->
                (chunk.chunk_id, "events", chunk.prev_token, chunk.next_token)
            | Model.Gap gap -> (gap.gap_id, "gap", Some gap.token, None)
          in
          Sqlite3.Rc.check
            (Sqlite3.bind_values stmt
               [
                 Sqlite3.Data.TEXT room_id;
                 Sqlite3.Data.INT (Int64.of_int chunk_id);
                 Sqlite3.Data.INT (Int64.of_int position);
                 Sqlite3.Data.TEXT kind;
                 Sqlite3.Data.opt_text token;
                 Sqlite3.Data.opt_text next_token;
               ]);
          Sqlite3.Rc.check (Sqlite3.step stmt);
          Sqlite3.Rc.check (Sqlite3.reset stmt);
          Sqlite3.Rc.check (Sqlite3.clear_bindings stmt))
        room.chunks);
  statement db "DELETE FROM ui_external_events WHERE room_id=?" (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
      Sqlite3.Rc.check (Sqlite3.step stmt));
  statement db
    "INSERT OR REPLACE INTO \
     ui_external_events(room_id,event_id,stable_id,event_json,clear_json,ordinal) \
     VALUES(?,?,?,?,?,?)" (fun stmt ->
      let events = bound_external_events room.external_events in
      List.iteri
        (fun ordinal event -> bind_external stmt room_id ~ordinal event)
        events);
  (* Events whose chunk the layout no longer mentions go with it. *)
  let clause =
    match List.map chunk_id_of room.chunks with
    | [] -> ""
    | ids ->
        " AND chunk_id NOT IN ("
        ^ String.concat "," (List.map string_of_int ids)
        ^ ")"
  in
  statement db ("DELETE FROM ui_events WHERE room_id=?" ^ clause) (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
      Sqlite3.Rc.check (Sqlite3.step stmt))

let transaction db f =
  Sqlite3.Rc.check (Sqlite3.exec db "BEGIN IMMEDIATE");
  try
    let result = f () in
    Sqlite3.Rc.check (Sqlite3.exec db "COMMIT");
    result
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    (try Sqlite3.Rc.check (Sqlite3.exec db "ROLLBACK")
     with Sqlite3.Error message | Sqlite3.SqliteError message ->
       Log.warn (fun m -> m "SQLite transaction rollback failed: %s" message));
    Printexc.raise_with_backtrace exn bt

let load_event stmt =
  let decode_column column =
    match decode (Sqlite3.column_text stmt column) with
    | Ok event -> event
    | Error message -> raise (Codec_error message)
  in
  ( Sqlite3.column_int stmt 0,
    {
      Model.stable_id = Sqlite3.column_text stmt 1;
      event = decode_column 2;
      clear_event =
        (if Sqlite3.column_is_null stmt 3 then None else Some (decode_column 3));
      delivery =
        delivery_of_columns
          (Sqlite3.column_text stmt 4)
          (if Sqlite3.column_is_null stmt 5 then None
           else Some (Sqlite3.column_text stmt 5));
    } )

let load_external_event stmt =
  let decode_column column =
    match decode (Sqlite3.column_text stmt column) with
    | Ok event -> event
    | Error message -> raise (Codec_error message)
  in
  {
    Model.stable_id = Sqlite3.column_text stmt 0;
    event = decode_column 1;
    clear_event =
      (if Sqlite3.column_is_null stmt 2 then None else Some (decode_column 2));
    (* Detached [/event] records are server records, never local sends. *)
    delivery = Model.Synced;
  }

let rec rows stmt f =
  match Sqlite3.step stmt with
  | Sqlite3.Rc.ROW ->
      f ();
      rows stmt f
  | Sqlite3.Rc.DONE -> ()
  | rc -> Sqlite3.Rc.check rc

let load_events_chunk_rows db room_id chunk_id =
  let layout = ref None in
  statement db
    "SELECT kind,token,next_token FROM ui_chunks WHERE room_id=? AND chunk_id=?"
    (fun stmt ->
      Sqlite3.Rc.check
        (Sqlite3.bind_values stmt
           [
             Sqlite3.Data.TEXT room_id; Sqlite3.Data.INT (Int64.of_int chunk_id);
           ]);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          if String.equal (Sqlite3.column_text stmt 0) "events" then
            let column index =
              if Sqlite3.column_is_null stmt index then None
              else Some (Sqlite3.column_text stmt index)
            in
            layout := Some (column 1, column 2)
      | Sqlite3.Rc.DONE -> ()
      | rc -> Sqlite3.Rc.check rc);
  match !layout with
  | None -> None
  | Some (prev_token, next_token) ->
      let events = ref [] in
      statement db
        "SELECT chunk_id,stable_id,event_json,clear_json,delivery,error FROM \
         ui_events WHERE room_id=? AND chunk_id=? ORDER BY position"
        (fun stmt ->
          Sqlite3.Rc.check
            (Sqlite3.bind_values stmt
               [
                 Sqlite3.Data.TEXT room_id;
                 Sqlite3.Data.INT (Int64.of_int chunk_id);
               ]);
          rows stmt (fun () ->
              let _, event = load_event stmt in
              events := event :: !events));
      Some { Model.chunk_id; prev_token; next_token; events = List.rev !events }

let load_metadata db room_id =
  let next_chunk_id = ref None in
  statement db "SELECT next_chunk_id FROM ui_rooms WHERE room_id=?" (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> next_chunk_id := Some (Sqlite3.column_int stmt 0)
      | Sqlite3.Rc.DONE -> ()
      | rc -> Sqlite3.Rc.check rc);
  match !next_chunk_id with
  | None -> None
  | Some next_chunk_id ->
      let stable_ids = Hashtbl.create 16 in
      statement db
        "SELECT chunk_id,stable_id FROM ui_events WHERE room_id=? ORDER BY \
         chunk_id,position" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              let chunk_id = Sqlite3.column_int stmt 0 in
              let id = Sqlite3.column_text stmt 1 in
              let prior =
                Option.value (Hashtbl.find_opt stable_ids chunk_id) ~default:[]
              in
              Hashtbl.replace stable_ids chunk_id (id :: prior)));
      let external_events = ref [] in
      statement db
        "SELECT stable_id,event_json,clear_json FROM ui_external_events WHERE \
         room_id=? ORDER BY ordinal,event_id" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              external_events := load_external_event stmt :: !external_events));
      let chunks = ref [] in
      statement db
        "SELECT chunk_id,kind,token,next_token FROM ui_chunks WHERE room_id=? \
         ORDER BY position" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              let chunk_id = Sqlite3.column_int stmt 0 in
              let column index =
                if Sqlite3.column_is_null stmt index then None
                else Some (Sqlite3.column_text stmt index)
              in
              let token = column 2 in
              let chunk =
                match Sqlite3.column_text stmt 1 with
                | "gap" -> (
                    match token with
                    | Some token ->
                        Model.Gap_metadata { gap_id = chunk_id; token }
                    | None -> raise (Codec_error "gap chunk has no token"))
                | "events" ->
                    Model.Events_metadata
                      {
                        chunk_id;
                        prev_token = token;
                        next_token = column 3;
                        stable_ids =
                          List.rev
                            (Option.value
                               (Hashtbl.find_opt stable_ids chunk_id)
                               ~default:[]);
                      }
                | kind -> raise (Codec_error ("unknown chunk kind: " ^ kind))
              in
              chunks := chunk :: !chunks));
      let metadata : Model.room_metadata =
        {
          Model.chunks = List.rev !chunks;
          next_chunk_id;
          external_events = bound_external_events (List.rev !external_events);
        }
      in
      Some metadata

let initial db room_id =
  match load_metadata db room_id with
  | None -> None
  | Some metadata ->
      (match List.rev metadata.Model.chunks with
      | Model.Gap_metadata _ :: _ ->
          raise (Codec_error "persisted room layout ends with a gap")
      | _ -> ());
      let newest_id =
        List.fold_left
          (fun newest -> function
            | Model.Events_metadata { chunk_id; _ } -> Some chunk_id
            | Model.Gap_metadata _ -> newest)
          None metadata.Model.chunks
      in
      let newest = Option.bind newest_id (load_events_chunk_rows db room_id) in
      Some (Model.Tail { metadata; newest })

let load db room_id =
  let next_chunk_id = ref None in
  statement db "SELECT next_chunk_id FROM ui_rooms WHERE room_id=?" (fun stmt ->
      Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> next_chunk_id := Some (Sqlite3.column_int stmt 0)
      | Sqlite3.Rc.DONE -> ()
      | rc -> Sqlite3.Rc.check rc);
  match !next_chunk_id with
  | None -> None
  | Some next_chunk_id ->
      let by_chunk = Hashtbl.create 16 in
      statement db
        "SELECT chunk_id,stable_id,event_json,clear_json,delivery,error FROM \
         ui_events WHERE room_id=? ORDER BY chunk_id,position" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              let chunk_id, event = load_event stmt in
              Hashtbl.replace by_chunk chunk_id
                (event
                :: Option.value (Hashtbl.find_opt by_chunk chunk_id) ~default:[]
                )));
      let external_events = ref [] in
      statement db
        "SELECT stable_id,event_json,clear_json FROM ui_external_events WHERE \
         room_id=? ORDER BY ordinal,event_id" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              external_events := load_external_event stmt :: !external_events));
      let chunks = ref [] in
      statement db
        "SELECT chunk_id,kind,token,next_token FROM ui_chunks WHERE room_id=? \
         ORDER BY position" (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          rows stmt (fun () ->
              let chunk_id = Sqlite3.column_int stmt 0 in
              let column index =
                if Sqlite3.column_is_null stmt index then None
                else Some (Sqlite3.column_text stmt index)
              in
              let token = column 2 in
              let chunk =
                match Sqlite3.column_text stmt 1 with
                | "gap" -> (
                    match token with
                    | Some token -> Model.Gap { gap_id = chunk_id; token }
                    | None -> raise (Codec_error "gap chunk has no token"))
                | "events" ->
                    Model.Events
                      {
                        chunk_id;
                        prev_token = token;
                        next_token = column 3;
                        events =
                          List.rev
                            (Option.value
                               (Hashtbl.find_opt by_chunk chunk_id)
                               ~default:[]);
                      }
                | kind -> raise (Codec_error ("unknown chunk kind: " ^ kind))
              in
              chunks := chunk :: !chunks));
      Some
        {
          Model.chunks = List.rev !chunks;
          next_chunk_id;
          external_events = bound_external_events (List.rev !external_events);
        }

(* Every SQLite call runs on a systhread, so a slow disk parks that thread
   rather than the domain the sync fiber and every observable subscriber
   share. The [`FULL] handle plus the store's own lock keep the calls
   serialized, so only one thread is ever inside the library at a time. *)
let in_thread f =
  match run_systhread ~label:"accessing Matrix UI event-store SQLite" f with
  | value -> Ok value
  | exception (Sqlite3.Error message | Sqlite3.SqliteError message) ->
      Error (Store.Error.Backend message)
  | exception Backend_refused message -> Error (Store.Error.Backend message)
  | exception Codec_error message -> Error (Store.Error.Codec message)

module Backend = struct
  type t = Sqlite3.db

  let refuse_invalid = function
    | Ok () -> ()
    | Error message -> raise (Backend_refused message)

  let ensure_events_chunk db room_id chunk_id =
    statement db
      "SELECT 1 FROM ui_chunks WHERE room_id=? AND chunk_id=? AND kind='events'"
      (fun stmt ->
        Sqlite3.Rc.check
          (Sqlite3.bind_values stmt
             [
               Sqlite3.Data.TEXT room_id;
               Sqlite3.Data.INT (Int64.of_int chunk_id);
             ]);
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> ()
        | Sqlite3.Rc.DONE ->
            raise (Backend_refused "events chunk does not exist")
        | rc -> Sqlite3.Rc.check rc)

  let validate_event_rows db room_id =
    let reject_if_row sql message =
      statement db sql (fun stmt ->
          Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> raise (Backend_refused message)
          | Sqlite3.Rc.DONE -> ()
          | rc -> Sqlite3.Rc.check rc)
    in
    reject_if_row
      "SELECT 1 FROM ui_chunks AS c WHERE c.room_id=? AND c.kind='events' AND \
       NOT EXISTS (SELECT 1 FROM ui_events AS e WHERE e.room_id=c.room_id AND \
       e.chunk_id=c.chunk_id) LIMIT 1"
      "persisted events chunk is empty";
    reject_if_row
      "SELECT 1 FROM ui_events AS e LEFT JOIN ui_chunks AS c ON \
       c.room_id=e.room_id AND c.chunk_id=e.chunk_id WHERE e.room_id=? AND \
       (c.chunk_id IS NULL OR c.kind<>'events' OR e.stable_id='') LIMIT 1"
      "persisted event row has no events chunk or stable id"

  let load_room db room_id =
    in_thread (fun () -> load db (Id.Room_id.to_string room_id))

  let load_room_initial db room_id =
    in_thread (fun () -> initial db (Id.Room_id.to_string room_id))

  let load_events_chunk db room_id chunk_id =
    in_thread (fun () ->
        load_events_chunk_rows db (Id.Room_id.to_string room_id) chunk_id)

  let replace_events_chunk db room_id chunk_id events =
    let duplicate = Hashtbl.create (List.length events) in
    if events = [] then
      raise (Backend_refused "replacement events chunk is empty")
    else if
      List.exists
        (fun (event : Model.event) ->
          if event.stable_id = "" || Hashtbl.mem duplicate event.stable_id then
            true
          else (
            Hashtbl.add duplicate event.stable_id ();
            false))
        events
    then raise (Backend_refused "duplicate stable event id in chunk")
    else begin
      ensure_events_chunk db room_id chunk_id;
      List.iter
        (fun (event : Model.event) ->
          statement db
            "SELECT 1 FROM ui_events WHERE room_id=? AND stable_id=? AND \
             chunk_id<>?" (fun stmt ->
              Sqlite3.Rc.check
                (Sqlite3.bind_values stmt
                   [
                     Sqlite3.Data.TEXT room_id;
                     Sqlite3.Data.TEXT event.stable_id;
                     Sqlite3.Data.INT (Int64.of_int chunk_id);
                   ]);
              match Sqlite3.step stmt with
              | Sqlite3.Rc.ROW ->
                  raise
                    (Backend_refused
                       ("stable event id belongs to another chunk: "
                      ^ event.stable_id))
              | Sqlite3.Rc.DONE -> ()
              | rc -> Sqlite3.Rc.check rc))
        events;
      statement db "DELETE FROM ui_events WHERE room_id=? AND chunk_id=?"
        (fun stmt ->
          Sqlite3.Rc.check
            (Sqlite3.bind_values stmt
               [
                 Sqlite3.Data.TEXT room_id;
                 Sqlite3.Data.INT (Int64.of_int chunk_id);
               ]);
          Sqlite3.Rc.check (Sqlite3.step stmt));
      statement db put_event_sql (fun stmt ->
          List.iteri
            (fun position event ->
              bind_put stmt room_id ~chunk_id ~position event)
            events)
    end

  let save_room db room_id room =
    let room_id = Id.Room_id.to_string room_id in
    in_thread (fun () ->
        refuse_invalid (Model.validate_room room);
        transaction db (fun () ->
            write_layout db room_id room;
            (* [save_room] is a whole-room rewrite.  [write_layout] retains
               rows for chunk IDs which remain in the layout so that a cheap
               [Layout] delta can preserve unloaded chunks; clear them here
               before writing the caller's authoritative event rows. *)
            statement db "DELETE FROM ui_events WHERE room_id=?" (fun stmt ->
                Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
                Sqlite3.Rc.check (Sqlite3.step stmt));
            statement db put_event_sql (fun stmt ->
                List.iter
                  (function
                    | Model.Gap _ -> ()
                    | Model.Events chunk ->
                        List.iteri
                          (fun position event ->
                            bind_put stmt room_id ~chunk_id:chunk.chunk_id
                              ~position event)
                          chunk.events)
                  room.Model.chunks)))

  let apply db room_id changes =
    let room_id = Id.Room_id.to_string room_id in
    in_thread (fun () ->
        changes
        |> List.fold_left
             (fun latest -> function
               | Model.Layout room -> Some room
               | Model.Put_event _ | Model.Replace_events_chunk _
               | Model.Delete_event _ ->
                   latest)
             None
        |> Option.iter (fun room -> refuse_invalid (Model.validate_layout room));
        transaction db (fun () ->
            List.iter
              (function
                | Model.Layout room -> write_layout db room_id room
                | Model.Put_event { chunk_id; position; event } ->
                    if event.stable_id = "" then
                      raise (Backend_refused "stable event id is empty");
                    ensure_events_chunk db room_id chunk_id;
                    statement db put_event_sql (fun stmt ->
                        bind_put stmt room_id ~chunk_id ~position event)
                | Model.Replace_events_chunk { chunk_id; events } ->
                    replace_events_chunk db room_id chunk_id events
                | Model.Delete_event { stable_id } ->
                    statement db
                      "DELETE FROM ui_events WHERE room_id=? AND stable_id=?"
                      (fun stmt ->
                        Sqlite3.Rc.check
                          (Sqlite3.bind_values stmt
                             [
                               Sqlite3.Data.TEXT room_id;
                               Sqlite3.Data.TEXT stable_id;
                             ]);
                        Sqlite3.Rc.check (Sqlite3.step stmt)))
              changes;
            validate_event_rows db room_id))

  let remove_room db room_id =
    let room_id = Id.Room_id.to_string room_id in
    in_thread (fun () ->
        transaction db (fun () ->
            List.iter
              (fun table ->
                statement db
                  ("DELETE FROM " ^ table ^ " WHERE room_id=?")
                  (fun stmt ->
                    Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 room_id);
                    Sqlite3.Rc.check (Sqlite3.step stmt)))
              [ "ui_external_events"; "ui_events"; "ui_chunks"; "ui_rooms" ]))

  let close db =
    ignore
      (run_systhread ~label:"closing Matrix UI event-store SQLite" (fun () ->
           Sqlite3.db_close db))
end

let create ?plaintext_policy path =
  run_systhread ~label:"creating Matrix UI event-store SQLite" (fun () ->
      try
        let db = Sqlite3.db_open ~mutex:`FULL path in
        try
          (* Opening the handle and all schema work belong to the same system
             thread as the SQLite calls made by the backend.  In particular,
             do not leave a partially initialised handle behind if migration
             or construction fails. *)
          ensure_schema db;
          Ok (Store.v_lazy ?plaintext_policy (module Backend) db)
        with
        | Sqlite3.Error message | Sqlite3.SqliteError message ->
            close_db_in_systhread_best_effort
              ~label:"closing UI event-store SQLite database" db;
            Error (Store.Error.Backend message)
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            close_db_in_systhread_best_effort
              ~label:"closing UI event-store SQLite database" db;
            Printexc.raise_with_backtrace exn bt
      with
      | Sqlite3.Error message | Sqlite3.SqliteError message | Failure message ->
        Error
          (Store.Error.Backend
             ("opening UI event-store SQLite database: " ^ message)))

(* The media cache deliberately shares the database file with the UI event
   store, but has its own versioned namespace and backend handle.  Keeping a
   separate handle means closing one store cannot invalidate the other. *)
module Media_backend = struct
  module M = Matrix_client.Media_store
  module Media = Matrix_client.Media

  type t = Sqlite3.db

  let schema_version = 2

  let schema =
    {|
CREATE TABLE IF NOT EXISTS ui_media_meta (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS ui_media (
  uri TEXT NOT NULL,
  format TEXT NOT NULL,
  data BLOB NOT NULL,
  ignore_retention INTEGER NOT NULL,
  protected INTEGER NOT NULL,
  owner TEXT,
  last_access REAL NOT NULL,
  sequence INTEGER NOT NULL,
  PRIMARY KEY (uri, format)
);
CREATE INDEX IF NOT EXISTS ui_media_lru ON ui_media(last_access, sequence);
|}

  let default_policy =
    {
      M.max_file_size = Some (20 * 1024 * 1024);
      max_total_size = Some (400 * 1024 * 1024);
      expiry = Some (Ptime.Span.of_int_s (60 * 24 * 60 * 60));
      cleanup_frequency = Some (Ptime.Span.of_int_s (24 * 60 * 60));
    }

  let ensure db =
    Sqlite3.Rc.check (Sqlite3.exec db schema);
    let version = ref None in
    statement db "SELECT value FROM ui_media_meta WHERE key='schema_version'"
      (fun stmt ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            version := int_of_string_opt (Sqlite3.column_text stmt 0)
        | Sqlite3.Rc.DONE -> ()
        | rc -> Sqlite3.Rc.check rc);
    (match !version with
    | Some version when version <> schema_version ->
        (* Only this backend's namespace may be discarded on a media schema
           upgrade; UI timeline tables in the same database are untouched. *)
        Sqlite3.Rc.check
          (Sqlite3.exec db
             "DROP TABLE IF EXISTS ui_media; DELETE FROM ui_media_meta")
    | _ -> ());
    Sqlite3.Rc.check (Sqlite3.exec db schema);
    statement db
      "INSERT OR IGNORE INTO ui_media_meta(key,value) \
       VALUES('schema_version',?)" (fun stmt ->
        Sqlite3.Rc.check
          (Sqlite3.bind_text stmt 1 (string_of_int schema_version));
        Sqlite3.Rc.check (Sqlite3.step stmt))

  let meta db key =
    let value = ref None in
    statement db "SELECT value FROM ui_media_meta WHERE key=?" (fun stmt ->
        Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 key);
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> value := Some (Sqlite3.column_text stmt 0)
        | Sqlite3.Rc.DONE -> ()
        | rc -> Sqlite3.Rc.check rc);
    !value

  let set_meta db key value =
    match value with
    | None ->
        statement db "DELETE FROM ui_media_meta WHERE key=?" (fun stmt ->
            Sqlite3.Rc.check (Sqlite3.bind_text stmt 1 key);
            Sqlite3.Rc.check (Sqlite3.step stmt));
        ()
    | Some value ->
        statement db
          "INSERT OR REPLACE INTO ui_media_meta(key,value) VALUES(?,?)"
          (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_values stmt
                 [ Sqlite3.Data.TEXT key; Sqlite3.Data.TEXT value ]);
            Sqlite3.Rc.check (Sqlite3.step stmt))

  let opt_int db key default =
    match meta db key with
    | None -> default
    | Some "" -> None
    | Some value -> (
        match int_of_string_opt value with
        | Some value -> Some value
        | None -> default)

  let opt_span db key default =
    match meta db key with
    | None -> default
    | Some "" -> None
    | Some value -> (
        match Option.bind (float_of_string_opt value) Ptime.Span.of_float_s with
        | Some span -> Some span
        | None -> default)

  let policy db =
    {
      M.max_file_size = opt_int db "max_file_size" default_policy.max_file_size;
      max_total_size = opt_int db "max_total_size" default_policy.max_total_size;
      expiry = opt_span db "expiry" default_policy.expiry;
      cleanup_frequency =
        opt_span db "cleanup_frequency" default_policy.cleanup_frequency;
    }

  let set_policy db (policy : M.retention_policy) =
    (* An explicitly unlimited field must survive a reopen.  Keep an absent
       key distinct from the empty sentinel: absence means "use defaults",
       while the sentinel means an intentional [None]. *)
    let encode_option f = function
      | None -> Some ""
      | Some value -> Some (f value)
    in
    transaction db (fun () ->
        set_meta db "max_file_size"
          (encode_option string_of_int policy.max_file_size);
        set_meta db "max_total_size"
          (encode_option string_of_int policy.max_total_size);
        set_meta db "expiry"
          (encode_option
             (fun value -> string_of_float (Ptime.Span.to_float_s value))
             policy.expiry);
        set_meta db "cleanup_frequency"
          (encode_option
             (fun value -> string_of_float (Ptime.Span.to_float_s value))
             policy.cleanup_frequency))

  let ptime_float value = Ptime.to_float_s value
  let wall_now () = Ptime_clock.now ()

  let format = function
    | M.File -> "f"
    | M.Thumbnail { width; height; resize } ->
        let resize =
          match resize with
          | None -> "-"
          | Some `Crop -> "c"
          | Some `Scale -> "s"
        in
        Printf.sprintf "t:%d:%d:%s" width height resize

  let media_thread f =
    try run_systhread ~label:"accessing Matrix UI media-store SQLite" f
    with Sqlite3.Error message | Sqlite3.SqliteError message ->
      raise (Sqlite_backend_error ("media store sqlite: " ^ message))

  let result f =
    try Ok (media_thread f) with
    | Sqlite_backend_error message ->
        Error (Matrix_client.Error.Network_error message)
    | Invalid_argument message ->
        Error (Matrix_client.Error.Policy_denied message)

  let retention db = media_thread (fun () -> policy db)

  let set_retention db value =
    let non_negative name = function
      | Some value when value < 0 ->
          Error
            (Matrix_client.Error.Policy_denied ("Media_store: negative " ^ name))
      | _ -> Ok ()
    in
    match non_negative "max_file_size" value.M.max_file_size with
    | Error _ as error -> error
    | Ok () -> (
        match non_negative "max_total_size" value.M.max_total_size with
        | Error _ as error -> error
        | Ok () -> (
            match value.M.expiry with
            | Some expiry when Ptime.Span.compare expiry Ptime.Span.zero < 0 ->
                Error
                  (Matrix_client.Error.Policy_denied
                     "Media_store: negative expiry")
            | _ -> (
                match value.M.cleanup_frequency with
                | Some frequency
                  when Ptime.Span.compare frequency Ptime.Span.zero < 0 ->
                    Error
                      (Matrix_client.Error.Policy_denied
                         "Media_store: negative cleanup frequency")
                | _ -> result (fun () -> set_policy db value))))

  let last_cleanup db =
    media_thread (fun () ->
        Option.bind (meta db "last_cleanup") (fun value ->
            Option.bind (float_of_string_opt value) Ptime.of_float_s))

  let set_last_cleanup db value =
    result (fun () ->
        set_meta db "last_cleanup"
          (Option.map (fun value -> string_of_float (ptime_float value)) value))

  let next_sequence db =
    let sequence =
      Option.value (opt_int db "next_sequence" (Some 0)) ~default:0
    in
    set_meta db "next_sequence" (Some (string_of_int (sequence + 1)));
    sequence

  let insert db ~ignore_retention ~protected ?owner ~at key ~data =
    transaction db (fun () ->
        let sequence = next_sequence db in
        statement db
          "INSERT OR REPLACE INTO \
           ui_media(uri,format,data,ignore_retention,protected,owner,last_access,sequence) \
           VALUES(?,?,?,?,?,?,?,?)" (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_values stmt
                 [
                   Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                   Sqlite3.Data.TEXT (format key.M.format);
                   Sqlite3.Data.BLOB data;
                   Sqlite3.Data.INT
                     (Int64.of_int (Bool.to_int ignore_retention));
                   Sqlite3.Data.INT (Int64.of_int (Bool.to_int protected));
                   Sqlite3.Data.opt_text owner;
                   Sqlite3.Data.FLOAT (ptime_float at);
                   Sqlite3.Data.INT (Int64.of_int sequence);
                 ]);
            Sqlite3.Rc.check (Sqlite3.step stmt)))

  let add ?(ignore_retention = false) ?(protected = false) ?owner ?now db key
      ~data =
    result (fun () ->
        let policy = policy db in
        let max_file =
          match (policy.max_file_size, policy.max_total_size) with
          | None, None -> None
          | Some value, None | None, Some value -> Some value
          | Some left, Some right -> Some (min left right)
        in
        if
          (not ignore_retention)
          && Option.exists (fun limit -> String.length data > limit) max_file
        then ()
        else
          let at = Option.value now ~default:(wall_now ()) in
          insert db ~ignore_retention ~protected ?owner ~at key ~data)

  let get ~now db key =
    result (fun () ->
        let found = ref None in
        transaction db (fun () ->
            statement db
              "UPDATE ui_media SET last_access=? WHERE uri=? AND format=?"
              (fun stmt ->
                Sqlite3.Rc.check
                  (Sqlite3.bind_values stmt
                     [
                       Sqlite3.Data.FLOAT (ptime_float now);
                       Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                       Sqlite3.Data.TEXT (format key.M.format);
                     ]);
                Sqlite3.Rc.check (Sqlite3.step stmt));
            statement db "SELECT data FROM ui_media WHERE uri=? AND format=?"
              (fun stmt ->
                Sqlite3.Rc.check
                  (Sqlite3.bind_values stmt
                     [
                       Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                       Sqlite3.Data.TEXT (format key.M.format);
                     ]);
                match Sqlite3.step stmt with
                | Sqlite3.Rc.ROW -> found := Some (Sqlite3.column_blob stmt 0)
                | Sqlite3.Rc.DONE -> ()
                | rc -> Sqlite3.Rc.check rc));
        !found)

  let update_flag column value db key =
    result (fun () ->
        statement db
          ("UPDATE ui_media SET " ^ column ^ "=? WHERE uri=? AND format=?")
          (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_values stmt
                 [
                   Sqlite3.Data.INT (Int64.of_int (Bool.to_int value));
                   Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                   Sqlite3.Data.TEXT (format key.M.format);
                 ]);
            Sqlite3.Rc.check (Sqlite3.step stmt)))

  let protect db key = update_flag "protected" true db key
  let unprotect db key = update_flag "protected" false db key

  let set_ignore_retention db key value =
    update_flag "ignore_retention" value db key

  let is_protected db key =
    result (fun () ->
        let value = ref false in
        statement db "SELECT protected FROM ui_media WHERE uri=? AND format=?"
          (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_values stmt
                 [
                   Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                   Sqlite3.Data.TEXT (format key.M.format);
                 ]);
            match Sqlite3.step stmt with
            | Sqlite3.Rc.ROW -> value := Sqlite3.column_int stmt 0 <> 0
            | Sqlite3.Rc.DONE -> ()
            | rc -> Sqlite3.Rc.check rc);
        !value)

  let replace_key db ~from_ ~to_ =
    result (fun () ->
        let from_uri = Media.Mxc.to_string from_.M.uri
        and to_uri = Media.Mxc.to_string to_.M.uri
        and from_format = format from_.M.format
        and to_format = format to_.M.format in
        if from_uri <> to_uri || from_format <> to_format then
          transaction db (fun () ->
              let source_exists = ref false in
              statement db "SELECT 1 FROM ui_media WHERE uri=? AND format=?"
                (fun stmt ->
                  Sqlite3.Rc.check
                    (Sqlite3.bind_values stmt
                       [
                         Sqlite3.Data.TEXT from_uri;
                         Sqlite3.Data.TEXT from_format;
                       ]);
                  match Sqlite3.step stmt with
                  | Sqlite3.Rc.ROW -> source_exists := true
                  | Sqlite3.Rc.DONE -> ()
                  | rc -> Sqlite3.Rc.check rc);
              if !source_exists then begin
                statement db "DELETE FROM ui_media WHERE uri=? AND format=?"
                  (fun stmt ->
                    Sqlite3.Rc.check
                      (Sqlite3.bind_values stmt
                         [
                           Sqlite3.Data.TEXT to_uri; Sqlite3.Data.TEXT to_format;
                         ]);
                    Sqlite3.Rc.check (Sqlite3.step stmt));
                statement db
                  "UPDATE ui_media SET uri=?, format=? WHERE uri=? AND format=?"
                  (fun stmt ->
                    Sqlite3.Rc.check
                      (Sqlite3.bind_values stmt
                         [
                           Sqlite3.Data.TEXT to_uri;
                           Sqlite3.Data.TEXT to_format;
                           Sqlite3.Data.TEXT from_uri;
                           Sqlite3.Data.TEXT from_format;
                         ]);
                    Sqlite3.Rc.check (Sqlite3.step stmt))
              end))

  let remove db key =
    result (fun () ->
        statement db "DELETE FROM ui_media WHERE uri=? AND format=?"
          (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_values stmt
                 [
                   Sqlite3.Data.TEXT (Media.Mxc.to_string key.M.uri);
                   Sqlite3.Data.TEXT (format key.M.format);
                 ]);
            Sqlite3.Rc.check (Sqlite3.step stmt)))

  let remove_uri db uri =
    result (fun () ->
        statement db "DELETE FROM ui_media WHERE uri=?" (fun stmt ->
            Sqlite3.Rc.check
              (Sqlite3.bind_text stmt 1 (Media.Mxc.to_string uri));
            Sqlite3.Rc.check (Sqlite3.step stmt)))

  let prune_local ~owner ~keep ~older_than db =
    result (fun () ->
        let keep =
          List.map
            (fun key -> (Media.Mxc.to_string key.M.uri, format key.M.format))
            keep
        in
        transaction db (fun () ->
            let doomed = ref [] in
            statement db
              "SELECT uri,format FROM ui_media WHERE owner=? AND last_access<? \
               AND uri LIKE 'mxc://send-queue.localhost/%'" (fun stmt ->
                Sqlite3.Rc.check
                  (Sqlite3.bind_values stmt
                     [
                       Sqlite3.Data.TEXT owner;
                       Sqlite3.Data.FLOAT (ptime_float older_than);
                     ]);
                rows stmt (fun () ->
                    let uri = Sqlite3.column_text stmt 0 in
                    let format_ = Sqlite3.column_text stmt 1 in
                    if not (List.mem (uri, format_) keep) then
                      match Media.Mxc.of_string uri with
                      | Ok uri when M.is_local_uri uri ->
                          doomed :=
                            (Media.Mxc.to_string uri, format_) :: !doomed
                      | _ -> ()));
            List.iter
              (fun (uri, format_) ->
                statement db "DELETE FROM ui_media WHERE uri=? AND format=?"
                  (fun stmt ->
                    Sqlite3.Rc.check
                      (Sqlite3.bind_values stmt
                         [ Sqlite3.Data.TEXT uri; Sqlite3.Data.TEXT format_ ]);
                    Sqlite3.Rc.check (Sqlite3.step stmt)))
              !doomed))

  let clean ~now db =
    result (fun () ->
        let policy = policy db in
        transaction db (fun () ->
            Option.iter
              (fun limit ->
                statement db
                  "DELETE FROM ui_media WHERE ignore_retention=0 AND \
                   protected=0 AND length(data)>?" (fun stmt ->
                    Sqlite3.Rc.check (Sqlite3.bind_int stmt 1 limit);
                    Sqlite3.Rc.check (Sqlite3.step stmt)))
              (match (policy.max_file_size, policy.max_total_size) with
              | None, None -> None
              | Some value, None | None, Some value -> Some value
              | Some left, Some right -> Some (min left right));
            Option.iter
              (fun expiry ->
                statement db
                  "DELETE FROM ui_media WHERE ignore_retention=0 AND \
                   protected=0 AND (? - last_access)>=?" (fun stmt ->
                    Sqlite3.Rc.check
                      (Sqlite3.bind_values stmt
                         [
                           Sqlite3.Data.FLOAT (ptime_float now);
                           Sqlite3.Data.FLOAT (Ptime.Span.to_float_s expiry);
                         ]);
                    Sqlite3.Rc.check (Sqlite3.step stmt)))
              policy.expiry;
            (match policy.max_total_size with
            | None -> ()
            | Some limit ->
                let total = ref 0 in
                statement db
                  "SELECT coalesce(sum(length(data)),0) FROM ui_media WHERE \
                   ignore_retention=0" (fun stmt ->
                    match Sqlite3.step stmt with
                    | Sqlite3.Rc.ROW -> total := Sqlite3.column_int stmt 0
                    | Sqlite3.Rc.DONE -> ()
                    | rc -> Sqlite3.Rc.check rc);
                if !total > limit then begin
                  let doomed = ref [] in
                  statement db
                    "SELECT uri,format,length(data) FROM ui_media WHERE \
                     ignore_retention=0 AND protected=0 ORDER BY \
                     last_access,sequence" (fun stmt ->
                      rows stmt (fun () ->
                          let size = Sqlite3.column_int stmt 2 in
                          if !total > limit then begin
                            doomed :=
                              ( Sqlite3.column_text stmt 0,
                                Sqlite3.column_text stmt 1 )
                              :: !doomed;
                            total := !total - size
                          end));
                  List.iter
                    (fun (uri, format) ->
                      statement db
                        "DELETE FROM ui_media WHERE uri=? AND format=?"
                        (fun stmt ->
                          Sqlite3.Rc.check
                            (Sqlite3.bind_values stmt
                               [
                                 Sqlite3.Data.TEXT uri; Sqlite3.Data.TEXT format;
                               ]);
                          Sqlite3.Rc.check (Sqlite3.step stmt)))
                    !doomed
                end);
            (* Persist the cadence marker in the same transaction as every
               deletion pass, including expiry-only and file-only policies.
               The wrapper records it redundantly for generic backends, but
               this keeps SQLite crash-consistent on its own. *)
            if
              Option.is_some policy.max_file_size
              || Option.is_some policy.max_total_size
              || Option.is_some policy.expiry
            then
              set_meta db "last_cleanup"
                (Some (string_of_float (ptime_float now)))))

  let close db =
    ignore
      (run_systhread ~label:"closing Matrix UI media-store SQLite" (fun () ->
           Sqlite3.db_close db))
end

let create_media_store ?retention path =
  match
    run_systhread ~label:"creating Matrix UI media-store SQLite" (fun () ->
        try
          let db = Sqlite3.db_open ~mutex:`FULL path in
          try
            (* [ensure] is deliberately called directly: this callback is
               already the systhread, and [ensure] performs the pragmas and
               media-schema migration synchronously. *)
            Media_backend.ensure db;
            Ok db
          with
          | Sqlite3.Error message
          | Sqlite3.SqliteError message
          | Failure message ->
              close_db_in_systhread_best_effort
                ~label:"closing media-store SQLite database" db;
              Error
                (Matrix_client.Error.Network_error
                   ("media store sqlite: " ^ message))
          | exn ->
              let bt = Printexc.get_raw_backtrace () in
              close_db_in_systhread_best_effort
                ~label:"closing media-store SQLite database" db;
              Printexc.raise_with_backtrace exn bt
        with
        | Sqlite3.Error message | Sqlite3.SqliteError message | Failure message
        ->
          Error
            (Matrix_client.Error.Network_error
               ("opening media-store SQLite database: " ^ message)))
  with
  | Error _ as error -> error
  | Ok db -> (
      try
        let backend = Media_store.v (module Media_backend) db in
        match retention with
        | None -> Ok backend
        | Some policy -> (
            match Media_store.set_retention backend policy with
            | Ok () -> Ok backend
            | Error _ as error ->
                (try Media_store.close backend with
                | Eio.Cancel.Cancelled _ as exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    Printexc.raise_with_backtrace exn bt
                | Eio.Io _ as exn ->
                    Log.warn (fun m ->
                        m
                          "closing media-store backend after retention \
                           failure: %a"
                          Eio.Exn.pp exn)
                | Sqlite3.Error message | Sqlite3.SqliteError message ->
                    Log.warn (fun m ->
                        m
                          "closing media-store backend after retention \
                           failure: sqlite: %s"
                          message));
                error)
      with
      | Sqlite3.Error message | Sqlite3.SqliteError message | Failure message ->
          close_db_best_effort ~label:"closing media-store SQLite database" db;
          Error
            (Matrix_client.Error.Network_error ("media store sqlite: " ^ message))
      | exn ->
          let bt = Printexc.get_raw_backtrace () in
          close_db_best_effort ~label:"closing media-store SQLite database" db;
          Printexc.raise_with_backtrace exn bt)
