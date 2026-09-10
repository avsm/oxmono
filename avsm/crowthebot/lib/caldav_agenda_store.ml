open Persistence
module A = Caldav_agenda

let encode = Jmap_eio.Codec.encode_exn Jsont.json
let decode = Jmap_eio.Codec.decode_exn Jsont.json

type t = {
  db : Sqlite3_eio.t;
  mutex : Eio.Mutex.t;
  admin : string;
  now : unit -> float;
  timestamp : float -> string;
}

let create ~db ~mutex ~admin ~now ~timestamp =
  { db; mutex; admin; now; timestamp }

let init db =
  sql db
    {|CREATE TABLE IF NOT EXISTS caldav_agendas(
id INTEGER PRIMARY KEY AUTOINCREMENT, mirror INTEGER NOT NULL, collection INTEGER NOT NULL,
start TEXT NOT NULL, finish TEXT NOT NULL, saved REAL NOT NULL, actor TEXT NOT NULL,
room TEXT NOT NULL, event TEXT NOT NULL, metadata TEXT NOT NULL, complete INTEGER NOT NULL, bytes INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS caldav_agenda_resources(
id INTEGER PRIMARY KEY AUTOINCREMENT, agenda INTEGER NOT NULL REFERENCES caldav_agendas(id) ON DELETE CASCADE,
collection INTEGER NOT NULL, href TEXT NOT NULL, etag TEXT, raw BLOB NOT NULL);
CREATE TABLE IF NOT EXISTS caldav_occurrences(
id INTEGER PRIMARY KEY AUTOINCREMENT, agenda INTEGER NOT NULL REFERENCES caldav_agendas(id) ON DELETE CASCADE,
resource INTEGER NOT NULL REFERENCES caldav_agenda_resources(id) ON DELETE CASCADE,
start TEXT NOT NULL, data TEXT NOT NULL);
CREATE INDEX IF NOT EXISTS caldav_occurrences_agenda ON caldav_occurrences(agenda,start,id);
INSERT OR IGNORE INTO tool_schemas VALUES('caldav-agenda',1);|};
  match
    rows db "SELECT version FROM tool_schemas WHERE name='caldav-agenda'" []
      (fun s -> Sqlite3.column_int s 0)
  with
  | [ 1 ] -> ()
  | _ -> invalid_arg "Unsupported agenda cache schema."

let access t actor f =
  locked t.mutex (fun () ->
      if
        actor <> t.admin
        && rows t.db
             "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
             [ text actor ]
             (fun _ -> ())
           = []
      then
        invalid_arg "Agenda operations require the admin or an allowed friend.";
      f ())

type snapshot = {
  id : int;
  mirror : int;
  start : string;
  finish : string;
  fetched_at : string;
  complete : bool;
  metadata : Jsont.json;
  count : int;
}

let snapshot t id =
  match
    rows t.db
      "SELECT mirror,start,finish,saved,complete,metadata,(SELECT count(*) \
       FROM caldav_occurrences WHERE agenda=a.id) FROM caldav_agendas a WHERE \
       id=?"
      [ integer id ]
      (fun s ->
        {
          id;
          mirror = Sqlite3.column_int s 0;
          start = Sqlite3.column_text s 1;
          finish = Sqlite3.column_text s 2;
          fetched_at = t.timestamp (Sqlite3.column_double s 3);
          complete = Sqlite3.column_int s 4 <> 0;
          metadata = decode (Sqlite3.column_text s 5);
          count = Sqlite3.column_int s 6;
        })
  with
  | [ s ] -> s
  | _ ->
      invalid_arg
        "Agenda snapshot expired or was evicted. Request the date range again."

let get t ~actor id = access t actor (fun () -> snapshot t id)

let fresh t ~actor ~mirror ~collection window =
  access t actor (fun () ->
      let start, finish = A.bounds window in
      match
        rows t.db
          "SELECT id FROM caldav_agendas WHERE mirror=? AND collection=? AND \
           start=? AND finish=? AND complete=1 AND saved>=? ORDER BY id DESC \
           LIMIT 1"
          [
            integer mirror;
            integer collection;
            text start;
            text finish;
            Sqlite3.Data.FLOAT (t.now () -. 300.);
          ]
          (fun s -> Sqlite3.column_int s 0)
      with
      | [ id ] -> Some (snapshot t id)
      | _ -> None)

type collection = {
  id : int;
  title : string;
  timezone : string option;
  resources : A.resource list;
}

let occurrence_data (c : collection) (e : A.occurrence) =
  let fields =
    match e.summary with
    | Jsont.Object (fs, _) -> List.map (fun ((k, _), v) -> (k, v)) fs
    | _ -> assert false
  in
  let fields =
    ("collection", Jsont.Json.int c.id)
    :: ("calendar", Jsont.Json.string (Plugin.clip ~bytes:160 c.title))
    :: ( "calendar_timezone",
         Option.fold ~none:(Jsont.Json.null ())
           ~some:(fun s -> Jsont.Json.string (Plugin.clip ~bytes:64 s))
           c.timezone )
    :: fields
  in
  encode
    (Jsont.Json.object'
       (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fields))

let save t ~actor ~room ~event ~mirror ~collection ~window ~metadata ~complete
    collections =
  let count = ref 0 in
  let bytes =
    List.fold_left
      (fun total (c : collection) ->
        List.fold_left
          (fun total (r : A.resource) ->
            count := !count + List.length r.occurrences;
            total + String.length r.raw
            + List.fold_left
                (fun n e -> n + String.length (occurrence_data c e))
                0 r.occurrences)
          total c.resources)
      (String.length (encode metadata))
      collections
  in
  if !count > 2000 then
    invalid_arg "Agenda exceeds 2000 occurrences. Ask for a shorter date range.";
  if bytes > 16777216 then
    invalid_arg
      "Agenda exceeds the 16 MiB cache limit. Ask for a shorter date range.";
  access t actor (fun () ->
      transaction t.db (fun () ->
          let start, finish = A.bounds window in
          execute t.db
            "INSERT INTO \
             caldav_agendas(mirror,collection,start,finish,saved,actor,room,event,metadata,complete,bytes) \
             VALUES(?,?,?,?,?,?,?,?,?,?,?)"
            [
              integer mirror;
              integer collection;
              text start;
              text finish;
              Sqlite3.Data.FLOAT (t.now ());
              text actor;
              text room;
              text event;
              text (encode metadata);
              integer (Bool.to_int complete);
              integer bytes;
            ];
          let id = last_id t.db in
          List.iter
            (fun (c : collection) ->
              List.iter
                (fun (r : A.resource) ->
                  execute t.db
                    "INSERT INTO \
                     caldav_agenda_resources(agenda,collection,href,etag,raw) \
                     VALUES(?,?,?,?,?)"
                    [
                      integer id;
                      integer c.id;
                      text r.href;
                      optional text r.etag;
                      Sqlite3.Data.BLOB r.raw;
                    ];
                  let resource = last_id t.db in
                  List.iter
                    (fun (e : A.occurrence) ->
                      let data = occurrence_data c e in
                      execute t.db
                        "INSERT INTO \
                         caldav_occurrences(agenda,resource,start,data) \
                         VALUES(?,?,?,?)"
                        [
                          integer id; integer resource; text e.starts; text data;
                        ])
                    r.occurrences)
                c.resources)
            collections;
          let total = ref 0 in
          rows t.db "SELECT id,bytes FROM caldav_agendas ORDER BY id DESC" []
            (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))
          |> List.iteri (fun i (old, size) ->
              total := !total + size;
              if i >= 20 || !total > 67108864 then
                execute t.db "DELETE FROM caldav_agendas WHERE id=?"
                  [ integer old ]);
          snapshot t id))

type entry = { id : int; resource : int; data : Jsont.json }

let page t ~actor ~snapshot:id ~offset =
  access t actor (fun () ->
      let s = snapshot t id in
      if offset < 0 || offset > s.count then
        invalid_arg "Invalid agenda offset.";
      rows t.db
        "SELECT id,resource,data FROM caldav_occurrences WHERE agenda=? ORDER \
         BY start,id LIMIT 10 OFFSET ?"
        [ integer id; integer offset ]
        (fun s ->
          {
            id = Sqlite3.column_int s 0;
            resource = Sqlite3.column_int s 1;
            data = decode (Sqlite3.column_text s 2);
          }))

let read t ~actor ~snapshot:id ~resource ~offset =
  access t actor (fun () ->
      ignore (snapshot t id);
      if offset < 0 || offset >= max_int then
        invalid_arg "Invalid agenda byte offset.";
      match
        rows t.db
          "SELECT substr(raw,?,4096),length(raw) FROM caldav_agenda_resources \
           WHERE agenda=? AND id=?"
          [ integer (offset + 1); integer id; integer resource ]
          (fun s -> (Sqlite3.column_blob s 0, Sqlite3.column_int s 1))
      with
      | [ (data, size) ] when offset <= size -> (data, size)
      | _ -> invalid_arg "Agenda resource or byte offset is unavailable.")
