open Persistence

type t = { db : Sqlite3_eio.t; mutex : Eio.Mutex.t; now : unit -> string }

let init db =
  sql db
    {|
CREATE TABLE IF NOT EXISTS room_observations (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 room TEXT NOT NULL, sender TEXT NOT NULL, event TEXT NOT NULL UNIQUE,
 observed_at TEXT NOT NULL, body TEXT NOT NULL, note TEXT NOT NULL DEFAULT '');
CREATE INDEX IF NOT EXISTS room_observations_room ON room_observations(room,id);
CREATE TABLE IF NOT EXISTS room_observation_events (
 id INTEGER PRIMARY KEY AUTOINCREMENT, room TEXT NOT NULL, event TEXT NOT NULL UNIQUE);
INSERT OR IGNORE INTO room_observation_events(room,event)
 SELECT room,event FROM room_observations ORDER BY id;
|}

let create ~db ~mutex ~now = { db; mutex; now }

let seen t ~event =
  locked t.mutex @@ fun () ->
  rows t.db "SELECT 1 FROM room_observation_events WHERE event=?"
    [ text event ]
    (fun _ -> ())
  <> []

let excerpt ~bytes value =
  if String.length value <= bytes then value
  else if bytes < 12 then ""
  else Plugin.clip ~bytes:(bytes - 12) value

let trim t ~room ~max_messages ~max_bytes =
  let existing =
    rows t.db
      "SELECT id,length(CAST(body AS BLOB))+length(CAST(note AS BLOB)) FROM \
       room_observations WHERE room=? ORDER BY id DESC"
      [ text room ]
      (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))
  in
  let rec keep count bytes = function
    | [] -> ()
    | (id, size) :: rest ->
        if count >= max_messages || bytes + size > max_bytes then
          execute t.db "DELETE FROM room_observations WHERE room=? AND id<=?"
            [ text room; integer id ]
        else keep (count + 1) (bytes + size) rest
  in
  keep 0 0 existing

let record t ~room ~sender ~event ~body ~max_messages ~max_bytes =
  if max_messages < 1 || max_bytes < 2 then
    invalid_arg "Invalid room context bounds.";
  locked t.mutex @@ fun () ->
  transaction t.db @@ fun () ->
  execute t.db
    "INSERT OR IGNORE INTO room_observation_events(room,event) VALUES (?,?)"
    [ text room; text event ];
  if Sqlite3.changes (Sqlite3_eio.db t.db) = 0 then None
  else begin
    execute t.db
      "INSERT OR IGNORE INTO \
       room_observations(room,sender,event,observed_at,body) VALUES \
       (?,?,?,?,?)"
      [
        text room;
        text sender;
        text event;
        text (t.now ());
        text (excerpt ~bytes:(min 4096 (max_bytes / 2)) body);
      ];
    let id =
      if Sqlite3.changes (Sqlite3_eio.db t.db) = 1 then
        Some (Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db t.db)))
      else None
    in
    if id <> None then Compaction.touch t.db (Room room);
    trim t ~room ~max_messages ~max_bytes;
    execute t.db
      "DELETE FROM room_observation_events WHERE room=? AND id NOT IN (SELECT \
       id FROM room_observation_events WHERE room=? ORDER BY id DESC LIMIT \
       2048)"
      [ text room; text room ];
    id
  end

let finish t ~id ~note ~max_messages ~max_bytes =
  if max_messages < 1 || max_bytes < 2 then
    invalid_arg "Invalid room context bounds.";
  locked t.mutex @@ fun () ->
  transaction t.db @@ fun () ->
  let rooms =
    rows t.db "SELECT room FROM room_observations WHERE id=?"
      [ integer id ]
      (fun s -> Sqlite3.column_text s 0)
  in
  execute t.db "UPDATE room_observations SET note=? WHERE id=?"
    [ text (excerpt ~bytes:(min 1024 (max_bytes / 2)) note); integer id ];
  List.iter
    (fun room ->
      Compaction.touch t.db (Room room);
      trim t ~room ~max_messages ~max_bytes)
    rooms

let context t ~room ~bytes =
  if bytes < 2 then invalid_arg "Room context needs at least two bytes.";
  locked t.mutex @@ fun () ->
  let entries =
    rows t.db
      "SELECT sender,event,observed_at,body,note FROM room_observations WHERE \
       room=? ORDER BY id DESC"
      [ text room ]
      (fun s ->
        let keys =
          [ "sender"; "event"; "observed_at"; "message"; "observation" ]
        in
        Jsont.Json.object'
          (List.mapi
             (fun i key ->
               let value = Sqlite3.column_text s i in
               let value =
                 if i >= 3 then excerpt ~bytes:(min 2048 (bytes / 8)) value
                 else value
               in
               ((key, Jsont.Meta.none), Jsont.Json.string value))
             keys))
  in
  let encode entries =
    Result.get_ok
      (Jsont_bytesrw.encode_string Jsont.json (Jsont.Json.list entries))
  in
  let rec collect selected = function
    | [] -> encode selected
    | entry :: rest ->
        let candidate = entry :: selected in
        if String.length (encode candidate) > bytes then encode selected
        else collect candidate rest
  in
  collect [] entries
