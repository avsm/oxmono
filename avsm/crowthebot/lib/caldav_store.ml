open Persistence
module R = Caldav_data

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
    {|CREATE TABLE IF NOT EXISTS caldav_mirrors(
 id INTEGER PRIMARY KEY AUTOINCREMENT, connection TEXT NOT NULL UNIQUE,
 identity TEXT NOT NULL, principal TEXT NOT NULL,
 job_id INTEGER NOT NULL REFERENCES reminders(id), checked_at TEXT, error TEXT, pending INTEGER NOT NULL DEFAULT 1);
CREATE TABLE IF NOT EXISTS caldav_collections(
 id INTEGER PRIMARY KEY AUTOINCREMENT, mirror INTEGER NOT NULL, href TEXT NOT NULL,
 title TEXT NOT NULL, properties TEXT NOT NULL, supports_sync INTEGER NOT NULL,
 active INTEGER NOT NULL DEFAULT 1, token TEXT, generation INTEGER NOT NULL DEFAULT 1,
 visible INTEGER NOT NULL DEFAULT 0, rebuilding INTEGER NOT NULL DEFAULT 1,
 staged INTEGER NOT NULL DEFAULT 0, next_token TEXT, more INTEGER NOT NULL DEFAULT 0,
 revision INTEGER NOT NULL DEFAULT 0, synced_at TEXT, checked REAL NOT NULL DEFAULT 0,
 UNIQUE(mirror,href));
CREATE TABLE IF NOT EXISTS caldav_tokens(collection INTEGER NOT NULL, token TEXT NOT NULL, PRIMARY KEY(collection,token));
CREATE TABLE IF NOT EXISTS caldav_pending(
 collection INTEGER NOT NULL, position INTEGER NOT NULL, href TEXT NOT NULL, etag TEXT,
 removed INTEGER NOT NULL, PRIMARY KEY(collection,position));
CREATE TABLE IF NOT EXISTS caldav_versions(
 id INTEGER PRIMARY KEY AUTOINCREMENT, collection INTEGER NOT NULL, href TEXT NOT NULL,
 hash TEXT NOT NULL, raw TEXT NOT NULL, search_text TEXT NOT NULL, parsed INTEGER NOT NULL,
 observed_at TEXT NOT NULL, UNIQUE(collection,href,hash));
CREATE TABLE IF NOT EXISTS caldav_objects(
 collection INTEGER NOT NULL, generation INTEGER NOT NULL, href TEXT NOT NULL,
 version INTEGER NOT NULL REFERENCES caldav_versions(id), etag TEXT,
 PRIMARY KEY(collection,generation,href));
CREATE INDEX IF NOT EXISTS caldav_objects_version ON caldav_objects(version);
CREATE TABLE IF NOT EXISTS caldav_deletions(
 id INTEGER PRIMARY KEY AUTOINCREMENT, collection INTEGER NOT NULL, href TEXT NOT NULL, observed_at TEXT NOT NULL);
CREATE VIRTUAL TABLE IF NOT EXISTS caldav_fts USING fts5(search_text,content='caldav_versions',content_rowid='id');
CREATE TRIGGER IF NOT EXISTS caldav_fts_insert AFTER INSERT ON caldav_versions BEGIN
 INSERT INTO caldav_fts(rowid,search_text) VALUES(new.id,new.search_text);
END;
INSERT OR IGNORE INTO tool_schemas VALUES('caldav',2);|};
  match
    rows db "SELECT version FROM tool_schemas WHERE name='caldav'" [] (fun s ->
        Sqlite3.column_int s 0)
  with
  | [ 1 ] ->
      Diagnostics.Tools.info (fun m -> m "CalDAV text index migration started");
      let rec reindex after count =
        let batch =
          rows db
            "SELECT id,raw FROM caldav_versions WHERE id>? ORDER BY id LIMIT 20"
            [ integer after ]
            (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_text s 1))
        in
        match batch with
        | [] -> count
        | _ ->
            List.iter
              (fun (id, raw) ->
                let text_value, parsed = Caldav_text.index raw in
                execute db
                  "UPDATE caldav_versions SET search_text=?,parsed=? WHERE id=?"
                  [ text text_value; integer (Bool.to_int parsed); integer id ])
              batch;
            reindex (fst (List.hd (List.rev batch))) (count + List.length batch)
      in
      let count = reindex 0 0 in
      sql db
        "INSERT INTO caldav_fts(caldav_fts) VALUES('rebuild'); UPDATE \
         tool_schemas SET version=2 WHERE name='caldav';";
      Diagnostics.Tools.info (fun m ->
          m "CalDAV text index migration finished resources=%d" count)
  | [ 2 ] -> ()
  | _ -> invalid_arg "Unsupported CalDAV tool schema."

let require t actor =
  if
    actor <> t.admin
    && rows t.db
         "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
         [ text actor ]
         (fun _ -> ())
       = []
  then invalid_arg "CalDAV operations require the admin or an allowed friend."

let access t actor f =
  locked t.mutex (fun () ->
      require t actor;
      f ())

let authorize t ~actor = access t actor (fun () -> ())

type mirror = {
  mirror_id : int;
  connection : string;
  identity : string;
  principal : string;
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
    principal = Sqlite3.column_text s 3;
    job_id = Sqlite3.column_int s 4;
    checked_at = string_opt s 5;
    error = string_opt s 6;
    pending = Sqlite3.column_int s 7 <> 0;
  }

let mirror t id =
  match
    rows t.db "SELECT * FROM caldav_mirrors WHERE id=?"
      [ integer id ]
      mirror_row
  with
  | [ m ] -> m
  | _ -> invalid_arg "Unknown CalDAV mirror."

let get t ~actor id = access t actor (fun () -> mirror t id)

let list t ~actor ~after =
  access t actor (fun () ->
      rows t.db "SELECT * FROM caldav_mirrors WHERE id>? ORDER BY id LIMIT 20"
        [ integer after ]
        mirror_row)

let ensure t ~actor ~room ~event ~connection ~identity ~principal ~cron ~next_at
    =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  match
    rows t.db "SELECT * FROM caldav_mirrors WHERE connection=?"
      [ text connection ]
      mirror_row
  with
  | [ m ] ->
      if m.identity <> identity then
        invalid_arg
          "This connection now identifies a different CalDAV account. Use a \
           new connection name.";
      m
  | _ ->
      execute t.db
        {|
INSERT INTO reminders(creator,room,event,created_at,instruction,cron,next_at,state,tool_namespace,tool_key)
VALUES(?,?,?,?,?,?,?,'active','caldav',0)|}
        [
          text actor;
          text room;
          text event;
          text (stamp t);
          text "Mirror the configured CalDAV without sending a message.";
          text cron;
          Sqlite3.Data.FLOAT next_at;
        ];
      let job_id = last_id t.db in
      execute t.db
        {|
INSERT INTO caldav_mirrors(connection,identity,principal,job_id) VALUES(?,?,?,?)|}
        [ text connection; text identity; text principal; integer job_id ];
      let id = last_id t.db in
      execute t.db "UPDATE reminders SET tool_key=? WHERE id=?"
        [ integer id; integer job_id ];
      mirror t id

type cursor = {
  id : int;
  mirror : int;
  collection : R.collection;
  token : string option;
  generation : int;
  visible : int;
  rebuilding : bool;
  staged : bool;
  next_token : string option;
  more : bool;
  revision : int;
  synced_at : string option;
}

let cursor_row s =
  {
    id = Sqlite3.column_int s 0;
    mirror = Sqlite3.column_int s 1;
    collection =
      {
        R.href = Sqlite3.column_text s 2;
        title = Sqlite3.column_text s 3;
        properties = Sqlite3.column_text s 4;
        sync = Sqlite3.column_int s 5 <> 0;
      };
    token = string_opt s 7;
    generation = Sqlite3.column_int s 8;
    visible = Sqlite3.column_int s 9;
    rebuilding = Sqlite3.column_int s 10 <> 0;
    staged = Sqlite3.column_int s 11 <> 0;
    next_token = string_opt s 12;
    more = Sqlite3.column_int s 13 <> 0;
    revision = Sqlite3.column_int s 14;
    synced_at = string_opt s 15;
  }

let running t ~actor id =
  access t actor (fun () ->
      rows t.db
        "SELECT 1 FROM caldav_mirrors m JOIN reminders r ON r.id=m.job_id \
         WHERE m.id=? AND r.state='active'"
        [ integer id ]
        (fun _ -> ())
      <> [])

let current t c =
  match
    rows t.db
      "SELECT c.* FROM caldav_collections c JOIN caldav_mirrors m ON \
       m.id=c.mirror JOIN reminders r ON r.id=m.job_id WHERE c.id=? AND \
       c.active=1 AND r.state='active'"
      [ integer c.id ]
      cursor_row
  with
  | [ now ] when now.revision = c.revision -> ()
  | _ ->
      invalid_arg "CalDAV cursor changed or polling was cancelled. Retry sync."

let discover t ~actor mirror collections =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  execute t.db "UPDATE caldav_collections SET active=0 WHERE mirror=?"
    [ integer mirror ];
  List.iter
    (fun (c : R.collection) ->
      execute t.db
        {|INSERT INTO caldav_collections(mirror,href,title,properties,supports_sync) VALUES(?,?,?,?,?)
ON CONFLICT(mirror,href) DO UPDATE SET title=excluded.title,properties=excluded.properties,
supports_sync=excluded.supports_sync,active=1|}
        [
          integer mirror;
          text c.href;
          text c.title;
          text c.properties;
          integer (Bool.to_int c.sync);
        ])
    collections

let cursors t ~actor mirror =
  access t actor (fun () ->
      rows t.db
        "SELECT * FROM caldav_collections WHERE mirror=? AND active=1 ORDER BY \
         CASE WHEN rebuilding=1 OR staged=1 OR more=1 OR checked<=? THEN 0 \
         ELSE 1 END,checked,id"
        [ integer mirror; Sqlite3.Data.FLOAT (t.now () -. 900.) ]
        cursor_row)

let stage t ~actor c (page : R.page) =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  current t c;
  if c.staged then invalid_arg "A CalDAV page is already staged.";
  let full = page.inventory || c.token = None in
  let rebuilding = c.rebuilding || full in
  let generation =
    if full && not c.rebuilding then c.generation + 1 else c.generation
  in
  Option.iter
    (fun token ->
      if
        rows t.db "SELECT 1 FROM caldav_tokens WHERE collection=? AND token=?"
          [ integer c.id; text token ]
          (fun _ -> ())
        <> []
      then invalid_arg "CalDAV pagination token repeated. Saved data retained.";
      if page.more then
        execute t.db "INSERT INTO caldav_tokens VALUES(?,?)"
          [ integer c.id; text token ])
    page.token;
  List.iteri
    (fun position (change : R.change) ->
      execute t.db "INSERT INTO caldav_pending VALUES(?,?,?,?,?)"
        [
          integer c.id;
          integer position;
          text change.href;
          optional text change.etag;
          integer (Bool.to_int change.removed);
        ])
    page.changes;
  execute t.db
    "UPDATE caldav_collections SET \
     staged=1,next_token=?,more=?,rebuilding=?,generation=?,revision=revision+1 \
     WHERE id=?"
    [
      optional text page.token;
      integer (Bool.to_int page.more);
      integer (Bool.to_int rebuilding);
      integer generation;
      integer c.id;
    ]

let pending t ~actor c =
  access t actor (fun () ->
      current t c;
      rows t.db
        "SELECT position,href,etag,removed FROM caldav_pending WHERE \
         collection=? ORDER BY position LIMIT 20"
        [ integer c.id ]
        (fun s ->
          ( Sqlite3.column_int s 0,
            R.
              {
                href = Sqlite3.column_text s 1;
                etag = string_opt s 2;
                removed = Sqlite3.column_int s 3 <> 0;
              } )))

type fetched = Gone | Held of int | Body of R.item

let held t ~actor c (change : R.change) =
  access t actor (fun () ->
      match change.etag with
      | None -> None
      | Some tag ->
          rows t.db
            "SELECT version FROM caldav_objects WHERE collection=? AND href=? \
             AND etag=? AND generation IN (?,?) ORDER BY generation DESC LIMIT \
             1"
            [
              integer c.id;
              text change.href;
              text tag;
              integer c.generation;
              integer c.visible;
            ]
            (fun s -> Sqlite3.column_int s 0)
          |> List.find_opt (fun _ -> true))

let commit t ~actor c fetched =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  current t c;
  if not c.staged then invalid_arg "No CalDAV page is staged.";
  List.iter
    (fun (position, (change : R.change), value) ->
      let key = [ integer c.id; integer c.generation; text change.href ] in
      (match value with
      | Gone ->
          execute t.db
            "DELETE FROM caldav_objects WHERE collection=? AND generation=? \
             AND href=?"
            key;
          execute t.db
            "INSERT INTO caldav_deletions(collection,href,observed_at) \
             VALUES(?,?,?)"
            [ integer c.id; text change.href; text (stamp t) ]
      | Held version ->
          execute t.db "INSERT OR REPLACE INTO caldav_objects VALUES(?,?,?,?,?)"
            (key @ [ integer version; optional text change.etag ])
      | Body item ->
          let hash = Digestif.SHA256.(to_hex (digest_string item.raw)) in
          let vkey = [ integer c.id; text change.href; text hash ] in
          execute t.db
            "INSERT OR IGNORE INTO \
             caldav_versions(collection,href,hash,raw,search_text,parsed,observed_at) \
             VALUES(?,?,?,?,?,?,?)"
            (vkey
            @ [
                text item.raw;
                text item.search;
                integer (Bool.to_int item.parsed);
                text (stamp t);
              ]);
          let version =
            List.hd
              (rows t.db
                 "SELECT id FROM caldav_versions WHERE collection=? AND href=? \
                  AND hash=?"
                 vkey (fun s -> Sqlite3.column_int s 0))
          in
          execute t.db "INSERT OR REPLACE INTO caldav_objects VALUES(?,?,?,?,?)"
            (key @ [ integer version; optional text item.etag ]));
      execute t.db
        "DELETE FROM caldav_pending WHERE collection=? AND position=?"
        [ integer c.id; integer position ])
    fetched;
  let remains =
    rows t.db "SELECT 1 FROM caldav_pending WHERE collection=? LIMIT 1"
      [ integer c.id ]
      (fun _ -> ())
    <> []
  in
  let publish = (not remains) && not c.more in
  if publish then
    execute t.db "DELETE FROM caldav_tokens WHERE collection=?" [ integer c.id ];
  if publish && c.rebuilding then begin
    execute t.db
      {|INSERT INTO caldav_deletions(collection,href,observed_at)
SELECT old.collection,old.href,? FROM caldav_objects old WHERE old.collection=? AND old.generation=?
AND NOT EXISTS(SELECT 1 FROM caldav_objects new WHERE new.collection=old.collection AND new.href=old.href AND new.generation=?)|}
      [ text (stamp t); integer c.id; integer c.visible; integer c.generation ];
    execute t.db
      "DELETE FROM caldav_objects WHERE collection=? AND generation<>?"
      [ integer c.id; integer c.generation ]
  end;
  if remains then
    execute t.db
      "UPDATE caldav_collections SET revision=revision+1,checked=? WHERE id=?"
      [ Sqlite3.Data.FLOAT (t.now ()); integer c.id ]
  else
    execute t.db
      {|UPDATE caldav_collections SET token=next_token,staged=0,revision=revision+1,
visible=?,rebuilding=?,synced_at=?,checked=? WHERE id=?|}
      [
        integer (if publish then c.generation else c.visible);
        integer (Bool.to_int (c.rebuilding && not publish));
        optional text (if publish then Some (stamp t) else c.synced_at);
        Sqlite3.Data.FLOAT (t.now ());
        integer c.id;
      ]

let reset t ~actor c =
  access t actor @@ fun () ->
  transaction t.db @@ fun () ->
  current t c;
  execute t.db "DELETE FROM caldav_pending WHERE collection=?" [ integer c.id ];
  execute t.db "DELETE FROM caldav_tokens WHERE collection=?" [ integer c.id ];
  execute t.db "DELETE FROM caldav_objects WHERE collection=? AND generation<>?"
    [ integer c.id; integer c.visible ];
  execute t.db
    "UPDATE caldav_collections SET \
     token=NULL,next_token=NULL,staged=0,rebuilding=1,generation=generation+1,revision=revision+1,checked=0 \
     WHERE id=?"
    [ integer c.id ]

let finish t ~actor id ~error =
  access t actor @@ fun () ->
  let more =
    rows t.db
      "SELECT 1 FROM caldav_collections WHERE mirror=? AND active=1 AND \
       (rebuilding=1 OR staged=1 OR more=1 OR checked<=?) LIMIT 1"
      [ integer id; Sqlite3.Data.FLOAT (t.now () -. 900.) ]
      (fun _ -> ())
    <> []
  in
  execute t.db
    "UPDATE caldav_mirrors SET checked_at=?,error=?,pending=? WHERE id=?"
    [
      text (stamp t);
      optional text error;
      integer (Bool.to_int more);
      integer id;
    ];
  if more && error = None then
    execute t.db
      "UPDATE reminders SET next_at=min(next_at,?) WHERE id=(SELECT job_id \
       FROM caldav_mirrors WHERE id=?) AND state='active'"
      [ Sqlite3.Data.FLOAT (t.now () +. 60.); integer id ]

let current_join =
  {| FROM caldav_versions v JOIN caldav_objects o ON o.version=v.id
 JOIN caldav_collections c ON c.id=o.collection AND c.visible=o.generation AND c.active=1 |}

type entry = {
  version : int;
  collection : int;
  href : string;
  hash : string;
  observed_at : string;
  excerpt : string;
  parsed : bool;
}

let search t ~actor mirror ~query ~after =
  access t actor @@ fun () ->
  let join, where, args =
    if query = "" then ("", "", [])
    else
      ( " JOIN caldav_fts ON caldav_fts.rowid=v.id ",
        " AND caldav_fts MATCH ?",
        [ text query ] )
  in
  rows t.db
    ("SELECT \
      v.id,c.id,v.href,v.hash,v.observed_at,substr(v.search_text,1,1200),v.parsed"
   ^ current_join ^ join ^ " WHERE c.mirror=? AND v.id>?" ^ where
   ^ " ORDER BY v.id LIMIT 5")
    ([ integer mirror; integer after ] @ args)
    (fun s ->
      {
        version = Sqlite3.column_int s 0;
        collection = Sqlite3.column_int s 1;
        href = Sqlite3.column_text s 2;
        hash = Sqlite3.column_text s 3;
        observed_at = Sqlite3.column_text s 4;
        excerpt = Sqlite3.column_text s 5;
        parsed = Sqlite3.column_int s 6 <> 0;
      })

let read t ~actor mirror ~version ~offset =
  access t actor (fun () ->
      match
        rows t.db
          ("SELECT substr(CAST(v.raw AS BLOB),?,4097),length(CAST(v.raw AS \
            BLOB))" ^ current_join ^ " WHERE c.mirror=? AND v.id=?")
          [ integer (offset + 1); integer mirror; integer version ]
          (fun s -> (Sqlite3.column_text s 0, Sqlite3.column_int s 1))
      with
      | [ v ] -> v
      | _ ->
          invalid_arg "CalDAV object is unavailable or changed. Search again.")

let counts t ~actor c =
  access t actor (fun () ->
      List.hd
        (rows t.db
           ("SELECT count(*),coalesce(sum(1-v.parsed),0)" ^ current_join
          ^ " WHERE c.id=?")
           [ integer c.id ]
           (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))))

let agendas t =
  Caldav_agenda_store.create ~db:t.db ~mutex:t.mutex ~admin:t.admin ~now:t.now
    ~timestamp:t.timestamp
