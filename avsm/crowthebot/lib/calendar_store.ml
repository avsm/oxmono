open Persistence
module R = Jmap_eio.Calendars

type t = {
  db : Sqlite3_eio.t;
  mutex : Eio.Mutex.t;
  admin : string;
  now : unit -> float;
  timestamp : float -> string;
}

let create ~db ~mutex ~admin ~now ~timestamp =
  { db; mutex; admin; now; timestamp }

let now t = t.now ()
let stamp t = t.timestamp (t.now ())

let init db =
  sql db
    {|
CREATE TABLE IF NOT EXISTS calendar_mirrors(
 id INTEGER PRIMARY KEY AUTOINCREMENT, connection TEXT NOT NULL UNIQUE,
 identity TEXT NOT NULL, account TEXT NOT NULL, username TEXT NOT NULL,
 job_id INTEGER NOT NULL REFERENCES reminders(id), checked_at TEXT, error TEXT, pending INTEGER NOT NULL DEFAULT 1);
CREATE TABLE IF NOT EXISTS calendar_cursors(
 mirror INTEGER NOT NULL REFERENCES calendar_mirrors(id), kind TEXT NOT NULL,
 state TEXT, phase TEXT NOT NULL DEFAULT 'new', generation INTEGER NOT NULL DEFAULT 1,
 visible INTEGER NOT NULL DEFAULT 0, position INTEGER NOT NULL DEFAULT 0,
 query_state TEXT, revision INTEGER NOT NULL DEFAULT 0, synced_at TEXT,
 PRIMARY KEY(mirror,kind));
CREATE TABLE IF NOT EXISTS calendar_versions(
 id INTEGER PRIMARY KEY AUTOINCREMENT, mirror INTEGER NOT NULL REFERENCES calendar_mirrors(id),
 kind TEXT NOT NULL, remote_id TEXT NOT NULL, hash TEXT NOT NULL,
 raw TEXT NOT NULL, ical TEXT, search_text TEXT NOT NULL, observed_at TEXT NOT NULL,
 UNIQUE(mirror,kind,remote_id,hash));
CREATE TABLE IF NOT EXISTS calendar_objects(
 mirror INTEGER NOT NULL, kind TEXT NOT NULL, generation INTEGER NOT NULL,
 remote_id TEXT NOT NULL, version INTEGER NOT NULL REFERENCES calendar_versions(id),
 PRIMARY KEY(mirror,kind,generation,remote_id));
CREATE TABLE IF NOT EXISTS calendar_deletions(
 id INTEGER PRIMARY KEY AUTOINCREMENT, mirror INTEGER NOT NULL, kind TEXT NOT NULL,
 remote_id TEXT NOT NULL, observed_at TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS calendar_receipts(
 id INTEGER PRIMARY KEY AUTOINCREMENT, mirror INTEGER NOT NULL, kind TEXT NOT NULL,
 method TEXT NOT NULL, request TEXT NOT NULL, response TEXT NOT NULL, observed_at TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS calendar_blobs(
 mirror INTEGER NOT NULL, blob_id TEXT NOT NULL, body BLOB, error TEXT,
 retry_at REAL NOT NULL DEFAULT 0, observed_at TEXT, PRIMARY KEY(mirror,blob_id));
CREATE TABLE IF NOT EXISTS calendar_blob_refs(
 version INTEGER NOT NULL REFERENCES calendar_versions(id), blob_id TEXT NOT NULL,
 PRIMARY KEY(version,blob_id));
CREATE VIRTUAL TABLE IF NOT EXISTS calendar_fts USING fts5(search_text,
 content='calendar_versions',content_rowid='id');
CREATE TRIGGER IF NOT EXISTS calendar_fts_insert AFTER INSERT ON calendar_versions BEGIN
 INSERT INTO calendar_fts(rowid,search_text) VALUES(new.id,new.search_text);
END;
INSERT OR IGNORE INTO tool_schemas VALUES('calendar',1);
|};
  match
    rows db "SELECT version FROM tool_schemas WHERE name='calendar'" []
      (fun s -> Sqlite3.column_int s 0)
  with
  | [ 1 ] -> ()
  | _ -> invalid_arg "Unsupported calendar tool schema."

let require t actor =
  if
    actor <> t.admin
    && rows t.db
         "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
         [ text actor ]
         (fun _ -> ())
       = []
  then invalid_arg "Calendar operations require the admin or an allowed friend."

let access t actor f =
  locked t.mutex (fun () ->
      require t actor;
      f ())

let authorize t ~actor = access t actor (fun () -> ())

type mirror = {
  mirror_id : int;
  connection : string;
  identity : string;
  account : string;
  username : string;
  job_id : int;
  checked_at : string option;
  error : string option;
  pending : bool;
}

let mirror_row s =
  {
    mirror_id = Sqlite3.column_int s 0;
    connection = Sqlite3.column_text s 1;
    identity = Sqlite3.column_text s 2;
    account = Sqlite3.column_text s 3;
    username = Sqlite3.column_text s 4;
    job_id = Sqlite3.column_int s 5;
    checked_at = string_opt s 6;
    error = string_opt s 7;
    pending = Sqlite3.column_int s 8 <> 0;
  }

let mirror t id =
  match
    rows t.db "SELECT * FROM calendar_mirrors WHERE id=?"
      [ integer id ]
      mirror_row
  with
  | [ m ] -> m
  | _ -> invalid_arg "Unknown calendar mirror."

let get t ~actor id = access t actor (fun () -> mirror t id)

let list t ~actor ~after =
  access t actor (fun () ->
      rows t.db "SELECT * FROM calendar_mirrors WHERE id>? ORDER BY id LIMIT 20"
        [ integer after ]
        mirror_row)

let ensure t ~actor ~room ~event ~connection ~identity ~account ~username ~cron
    ~next_at =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  match
    rows t.db "SELECT * FROM calendar_mirrors WHERE connection=?"
      [ text connection ]
      mirror_row
  with
  | [ m ] ->
      if m.identity <> identity then
        invalid_arg
          "This connection now identifies a different calendar account. Use a \
           new connection name.";
      m
  | _ ->
      execute t.db
        {|
INSERT INTO reminders(creator,room,event,created_at,instruction,cron,next_at,state,tool_namespace,tool_key)
VALUES(?,?,?,?,?,?,?,'active','calendar',0)|}
        [
          text actor;
          text room;
          text event;
          text (stamp t);
          text "Mirror the configured calendar without sending a message.";
          text cron;
          Sqlite3.Data.FLOAT next_at;
        ];
      let job_id = last_id t.db in
      execute t.db
        {|
INSERT INTO calendar_mirrors(connection,identity,account,username,job_id) VALUES(?,?,?,?,?)|}
        [
          text connection;
          text identity;
          text account;
          text username;
          integer job_id;
        ];
      let id = last_id t.db in
      execute t.db "UPDATE reminders SET tool_key=? WHERE id=?"
        [ integer id; integer job_id ];
      List.iter
        (fun kind ->
          execute t.db "INSERT INTO calendar_cursors(mirror,kind) VALUES(?,?)"
            [ integer id; text (R.kind_name kind) ])
        R.kinds;
      mirror t id

type cursor = {
  mirror : int;
  kind : R.kind;
  state : string option;
  phase : string;
  generation : int;
  visible : int;
  position : int;
  query_state : string option;
  revision : int;
  synced_at : string option;
}

let cursor_row s =
  {
    mirror = Sqlite3.column_int s 0;
    kind = R.kind_of_string (Sqlite3.column_text s 1);
    state = string_opt s 2;
    phase = Sqlite3.column_text s 3;
    generation = Sqlite3.column_int s 4;
    visible = Sqlite3.column_int s 5;
    position = Sqlite3.column_int s 6;
    query_state = string_opt s 7;
    revision = Sqlite3.column_int s 8;
    synced_at = string_opt s 9;
  }

let cursors t ~actor id =
  access t actor (fun () ->
      ignore (mirror t id);
      rows t.db "SELECT * FROM calendar_cursors WHERE mirror=? ORDER BY kind"
        [ integer id ]
        cursor_row)

let args (c : cursor) = [ integer c.mirror; text (R.kind_name c.kind) ]

let active t id =
  rows t.db
    {|
SELECT 1 FROM calendar_mirrors m JOIN reminders r ON r.id=m.job_id
WHERE m.id=? AND r.state='active'|}
    [ integer id ]
    (fun _ -> ())
  <> []

let receipt t (c : cursor) (r : R.receipt) =
  execute t.db
    "INSERT INTO \
     calendar_receipts(mirror,kind,method,request,response,observed_at) \
     VALUES(?,?,?,?,?,?)"
    (args c
    @ [ text r.method_name; text r.request; text r.response; text (stamp t) ])

let add_item t (c : cursor) (i : R.item) =
  let json = Jmap_eio.Codec.decode_exn Jsont.json i.raw in
  let hash =
    Digestif.SHA256.(
      to_hex (digest_string (i.raw ^ "\000" ^ Option.value ~default:"" i.ical)))
  in
  let key = args c @ [ text i.remote_id; text hash ] in
  execute t.db
    {|
INSERT OR IGNORE INTO calendar_versions(mirror,kind,remote_id,hash,raw,ical,search_text,observed_at)
VALUES(?,?,?,?,?,?,?,?)|}
    (key
    @ [
        text i.raw;
        optional text i.ical;
        text (Calendar_index.text json);
        text (stamp t);
      ]);
  let version =
    List.hd
      (rows t.db
         "SELECT id FROM calendar_versions WHERE mirror=? AND kind=? AND \
          remote_id=? AND hash=?"
         key (fun s -> Sqlite3.column_int s 0))
  in
  execute t.db "INSERT OR REPLACE INTO calendar_objects VALUES(?,?,?,?,?)"
    (args c @ [ integer c.generation; text i.remote_id; integer version ]);
  List.iter
    (fun blob ->
      execute t.db "INSERT OR IGNORE INTO calendar_blob_refs VALUES(?,?)"
        [ integer version; text blob ];
      execute t.db
        "INSERT OR IGNORE INTO calendar_blobs(mirror,blob_id) VALUES(?,?)"
        [ integer c.mirror; text blob ])
    (Calendar_index.blobs json)

let destroy t c id =
  execute t.db
    "INSERT INTO calendar_deletions(mirror,kind,remote_id,observed_at) \
     VALUES(?,?,?,?)"
    (args c @ [ text id; text (stamp t) ]);
  execute t.db
    "DELETE FROM calendar_objects WHERE mirror=? AND kind=? AND generation=? \
     AND remote_id=?"
    (args c @ [ integer c.generation; text id ])

(* A cursor and all objects it describes commit together. An in-flight page
   cannot overwrite a cursor advanced or reset by another sync. *)
let commit t ~actor before after ~items ~destroyed ~receipts =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  if not (active t before.mirror) then invalid_arg "Calendar sync is cancelled.";
  let current =
    List.hd
      (rows t.db "SELECT * FROM calendar_cursors WHERE mirror=? AND kind=?"
         (args before) cursor_row)
  in
  if current.revision <> before.revision then
    invalid_arg "Calendar cursor changed. Retry sync.";
  if after.generation <> before.generation then
    execute t.db
      "DELETE FROM calendar_objects WHERE mirror=? AND kind=? AND generation<>?"
      (args before @ [ integer before.visible ]);
  List.iter (receipt t before) receipts;
  List.iter (add_item t after) items;
  List.iter (destroy t after) destroyed;
  let publish = after.phase = "live" && after.visible <> after.generation in
  let visible = if publish then after.generation else after.visible in
  if publish then begin
    execute t.db
      {|
INSERT INTO calendar_deletions(mirror,kind,remote_id,observed_at)
SELECT old.mirror,old.kind,old.remote_id,? FROM calendar_objects old
WHERE old.mirror=? AND old.kind=? AND old.generation=? AND NOT EXISTS(
 SELECT 1 FROM calendar_objects new WHERE new.mirror=old.mirror AND new.kind=old.kind
 AND new.remote_id=old.remote_id AND new.generation=?)|}
      ([ text (stamp t) ]
      @ args after
      @ [ integer after.visible; integer after.generation ]);
    execute t.db
      "DELETE FROM calendar_objects WHERE mirror=? AND kind=? AND generation<>?"
      (args after @ [ integer after.generation ])
  end;
  let synced_at =
    if after.phase = "live" then Some (stamp t) else after.synced_at
  in
  execute t.db
    {|
UPDATE calendar_cursors SET state=?,phase=?,generation=?,visible=?,position=?,query_state=?,
revision=revision+1,synced_at=? WHERE mirror=? AND kind=?|}
    ([
       optional text after.state;
       text after.phase;
       integer after.generation;
       integer visible;
       integer after.position;
       optional text after.query_state;
       optional text synced_at;
     ]
    @ args after)

let reset t ~actor c ~receipts =
  let after =
    {
      c with
      state = None;
      phase = "new";
      generation = c.generation + 1;
      position = 0;
      query_state = None;
    }
  in
  commit t ~actor c after ~items:[] ~destroyed:[] ~receipts

let finish t ~actor id ~error ~pending ~more =
  access t actor (fun () ->
      execute t.db
        "UPDATE calendar_mirrors SET checked_at=?,error=?,pending=? WHERE id=?"
        [
          text (stamp t);
          optional text error;
          integer (if pending then 1 else 0);
          integer id;
        ];
      if more then
        execute t.db
          {|
UPDATE reminders SET next_at=min(next_at,?) WHERE id=(SELECT job_id FROM calendar_mirrors WHERE id=?)
AND state='active'|}
          [ Sqlite3.Data.FLOAT (t.now () +. 60.); integer id ])

let current_join =
  {|
 FROM calendar_versions v JOIN calendar_objects o ON o.version=v.id
 JOIN calendar_cursors c ON c.mirror=o.mirror AND c.kind=o.kind AND c.visible=o.generation |}

type entry = {
  version : int;
  remote_id : string;
  kind : string;
  hash : string;
  observed_at : string;
  excerpt : string;
}

let search t ~actor id ~kind ~query ~after =
  access t actor (fun () ->
      ignore (mirror t id);
      let fts, condition, params =
        if query = "" then ("", "", [])
        else
          ( " JOIN calendar_fts ON calendar_fts.rowid=v.id ",
            " AND calendar_fts MATCH ?",
            [ text query ] )
      in
      rows t.db
        ("SELECT \
          v.id,v.remote_id,v.kind,v.hash,v.observed_at,substr(v.search_text,1,1200)"
       ^ current_join ^ fts ^ " WHERE v.mirror=? AND v.kind=? AND v.id>?"
       ^ condition ^ " ORDER BY v.id LIMIT 5")
        ([ integer id; text (R.kind_name kind); integer after ] @ params)
        (fun s ->
          {
            version = Sqlite3.column_int s 0;
            remote_id = Sqlite3.column_text s 1;
            kind = Sqlite3.column_text s 2;
            hash = Sqlite3.column_text s 3;
            observed_at = Sqlite3.column_text s 4;
            excerpt = Sqlite3.column_text s 5;
          }))

let read t ~actor id ~version ~ical ~offset =
  access t actor (fun () ->
      let column = if ical then "v.ical" else "v.raw" in
      match
        rows t.db
          ("SELECT substr(CAST(" ^ column ^ " AS BLOB),?,4097),length(CAST("
         ^ column ^ " AS BLOB))" ^ current_join ^ " WHERE v.mirror=? AND v.id=?"
          )
          [ integer (offset + 1); integer id; integer version ]
          (fun s -> (string_opt s 0, Sqlite3.column_int s 1))
      with
      | [ (Some data, size) ] -> (data, size)
      | _ ->
          invalid_arg
            "Calendar object is unavailable or has changed. Search again.")

let counts t ~actor id =
  access t actor (fun () ->
      rows t.db
        ("SELECT v.kind,count(*),sum(CASE WHEN v.ical IS NULL THEN 1 ELSE 0 \
          END)" ^ current_join ^ " WHERE v.mirror=? GROUP BY v.kind")
        [ integer id ]
        (fun s ->
          ( Sqlite3.column_text s 0,
            Sqlite3.column_int s 1,
            Sqlite3.column_int s 2 )))

let referenced =
  " EXISTS(SELECT 1" ^ current_join
  ^ " JOIN calendar_blob_refs r ON r.version=v.id WHERE v.mirror=b.mirror AND \
     r.blob_id=b.blob_id)"

let pending_blobs t ~actor id =
  access t actor (fun () ->
      rows t.db
        ("SELECT b.blob_id FROM calendar_blobs b WHERE b.mirror=? AND b.body \
          IS NULL AND b.retry_at<=? AND " ^ referenced
       ^ " ORDER BY b.retry_at,b.blob_id LIMIT 2")
        [ integer id; Sqlite3.Data.FLOAT (t.now ()) ]
        (fun s -> Sqlite3.column_text s 0))

let save_blob t ~actor id ~blob result =
  access t actor (fun () ->
      if not (active t id) then invalid_arg "Calendar sync is cancelled.";
      let body, error =
        match result with
        | Ok body -> (Some body, None)
        | Error error -> (None, Some error)
      in
      execute t.db
        ("UPDATE calendar_blobs AS b SET \
          body=?,error=?,retry_at=?,observed_at=?"
       ^ " WHERE b.mirror=? AND b.blob_id=? AND " ^ referenced)
        [
          optional (fun s -> Sqlite3.Data.BLOB s) body;
          optional text error;
          Sqlite3.Data.FLOAT (t.now () +. 3600.);
          text (stamp t);
          integer id;
          text blob;
        ])

let blob_counts t ~actor id =
  access t actor (fun () ->
      List.hd
        (rows t.db
           ("SELECT count(*),count(b.body) FROM calendar_blobs b WHERE \
             b.mirror=? AND " ^ referenced)
           [ integer id ]
           (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))))

let running t ~actor id = access t actor (fun () -> active t id)

let blob_failures t ~actor id =
  access t actor (fun () ->
      List.hd
        (rows t.db
           ("SELECT count(*) FROM calendar_blobs b WHERE b.mirror=? AND b.body \
             IS NULL AND b.error IS NOT NULL AND " ^ referenced)
           [ integer id ]
           (fun s -> Sqlite3.column_int s 0)))
