open Persistence

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
CREATE TABLE IF NOT EXISTS tool_schemas(name TEXT PRIMARY KEY, version INTEGER NOT NULL);
CREATE TABLE IF NOT EXISTS feeds_sources(
 id INTEGER PRIMARY KEY AUTOINCREMENT, url TEXT NOT NULL UNIQUE,
 kind TEXT NOT NULL DEFAULT 'unknown', title TEXT NOT NULL DEFAULT '',
 etag TEXT, last_modified TEXT, checked_at REAL, success_at TEXT,
 error TEXT, failures INTEGER NOT NULL DEFAULT 0, retry_at REAL NOT NULL DEFAULT 0);
CREATE TABLE IF NOT EXISTS feeds_subscriptions(
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id),
 creator TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 created_at TEXT NOT NULL, cron TEXT NOT NULL, until_at REAL,
 UNIQUE(source_id,room));
CREATE TABLE IF NOT EXISTS feeds_memberships(
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 subscription_id INTEGER NOT NULL REFERENCES feeds_subscriptions(id) ON DELETE CASCADE,
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id),
 job_id INTEGER REFERENCES reminders(id),
 initialized INTEGER NOT NULL DEFAULT 0, cursor INTEGER NOT NULL DEFAULT 0,
 UNIQUE(subscription_id,source_id));
CREATE TABLE IF NOT EXISTS feeds_seen(
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id) ON DELETE CASCADE,
 entry_key TEXT NOT NULL, PRIMARY KEY(source_id,entry_key));
CREATE TABLE IF NOT EXISTS feeds_outlines(
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id) ON DELETE CASCADE,
 url TEXT NOT NULL, PRIMARY KEY(source_id,url));
CREATE TABLE IF NOT EXISTS feeds_entries(
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id) ON DELETE CASCADE,
 entry_key TEXT NOT NULL, title TEXT NOT NULL, url TEXT, published TEXT,
 summary TEXT NOT NULL, observed_at TEXT NOT NULL,
 UNIQUE(source_id,entry_key));
CREATE INDEX IF NOT EXISTS feeds_entries_source ON feeds_entries(source_id,id);
CREATE TRIGGER IF NOT EXISTS feeds_member_delete AFTER DELETE ON feeds_memberships BEGIN
 UPDATE reminders SET state='cancelled' WHERE id=old.job_id;
END;
INSERT OR IGNORE INTO tool_schemas VALUES('feeds',1);
|};
  match
    rows db "SELECT version FROM tool_schemas WHERE name='feeds'" [] (fun s ->
        Sqlite3.column_int s 0)
  with
  | [ 2 ] -> ()
  | [ 1 ] ->
      sql db
        {|
ALTER TABLE feeds_entries ADD COLUMN content TEXT NOT NULL DEFAULT '';
UPDATE feeds_entries SET content=summary;
ALTER TABLE feeds_sources ADD COLUMN next_url TEXT;
UPDATE feeds_sources SET etag=NULL,last_modified=NULL,checked_at=NULL,retry_at=0;
UPDATE feeds_memberships SET initialized=0;
CREATE TABLE feeds_pages(
 source_id INTEGER NOT NULL REFERENCES feeds_sources(id) ON DELETE CASCADE,
 url TEXT NOT NULL, PRIMARY KEY(source_id,url));
CREATE VIRTUAL TABLE feeds_fts USING fts5(title,summary,content,
 content='feeds_entries',content_rowid='id');
CREATE TRIGGER feeds_fts_insert AFTER INSERT ON feeds_entries BEGIN
 INSERT INTO feeds_fts(rowid,title,summary,content) VALUES(new.id,new.title,new.summary,new.content);
END;
CREATE TRIGGER feeds_fts_delete AFTER DELETE ON feeds_entries BEGIN
 INSERT INTO feeds_fts(feeds_fts,rowid,title,summary,content) VALUES('delete',old.id,old.title,old.summary,old.content);
END;
CREATE TRIGGER feeds_fts_update AFTER UPDATE ON feeds_entries BEGIN
 INSERT INTO feeds_fts(feeds_fts,rowid,title,summary,content) VALUES('delete',old.id,old.title,old.summary,old.content);
 INSERT INTO feeds_fts(rowid,title,summary,content) VALUES(new.id,new.title,new.summary,new.content);
END;
INSERT INTO feeds_fts(feeds_fts) VALUES('rebuild');
UPDATE tool_schemas SET version=2 WHERE name='feeds';
|}
  | _ -> invalid_arg "unsupported feeds tool schema"

let require t actor =
  if
    actor <> t.admin
    && rows t.db
         "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
         [ text actor ]
         (fun _ -> ())
       = []
  then invalid_arg "Feed operations require the admin or an allowed friend."

let access t actor f =
  locked t.mutex (fun () ->
      require t actor;
      f ())

type source = {
  source_id : int;
  url : string;
  kind : string;
  title : string;
  etag : string option;
  last_modified : string option;
  checked_at : float option;
  success_at : string option;
  error : string option;
  failures : int;
  retry_at : float;
  next_url : string option;
}

let source_row s =
  {
    source_id = Sqlite3.column_int s 0;
    url = Sqlite3.column_text s 1;
    kind = Sqlite3.column_text s 2;
    title = Sqlite3.column_text s 3;
    etag = string_opt s 4;
    last_modified = string_opt s 5;
    checked_at =
      (match Sqlite3.column s 6 with
      | Sqlite3.Data.NULL -> None
      | _ -> Some (Sqlite3.column_double s 6));
    success_at = string_opt s 7;
    error = string_opt s 8;
    failures = Sqlite3.column_int s 9;
    retry_at = Sqlite3.column_double s 10;
    next_url = string_opt s 11;
  }

type subscription = {
  subscription_id : int;
  source_id : int;
  creator : string;
  room : string;
  event : string;
  created_at : string;
  cron : string;
  until_at : float option;
}

let subscription_row s =
  {
    subscription_id = Sqlite3.column_int s 0;
    source_id = Sqlite3.column_int s 1;
    creator = Sqlite3.column_text s 2;
    room = Sqlite3.column_text s 3;
    event = Sqlite3.column_text s 4;
    created_at = Sqlite3.column_text s 5;
    cron = Sqlite3.column_text s 6;
    until_at =
      (match Sqlite3.column s 7 with
      | Sqlite3.Data.NULL -> None
      | _ -> Some (Sqlite3.column_double s 7));
  }

type member = {
  member_id : int;
  subscription_id : int;
  source_id : int;
  job_id : int;
  initialized : bool;
  cursor : int;
}

let member_row s =
  {
    member_id = Sqlite3.column_int s 0;
    subscription_id = Sqlite3.column_int s 1;
    source_id = Sqlite3.column_int s 2;
    job_id = Sqlite3.column_int s 3;
    initialized = Sqlite3.column_int s 4 = 1;
    cursor = Sqlite3.column_int s 5;
  }

type entry = {
  entry_id : int;
  source_id : int;
  key : string;
  title : string;
  url : string option;
  published : string option;
  summary : string;
  observed_at : string;
  content : string;
}

let entry_row s =
  {
    entry_id = Sqlite3.column_int s 0;
    source_id = Sqlite3.column_int s 1;
    key = Sqlite3.column_text s 2;
    title = Sqlite3.column_text s 3;
    url = string_opt s 4;
    published = string_opt s 5;
    summary = Sqlite3.column_text s 6;
    observed_at = Sqlite3.column_text s 7;
    content = Sqlite3.column_text s 8;
  }

let one = function
  | [ x ] -> x
  | _ -> invalid_arg "Feed subscription no longer exists."

let source t id =
  rows t.db "SELECT * FROM feeds_sources WHERE id=?" [ integer id ] source_row
  |> one

let subscription t id =
  rows t.db "SELECT * FROM feeds_subscriptions WHERE id=?"
    [ integer id ]
    subscription_row
  |> one

let member t id =
  rows t.db "SELECT * FROM feeds_memberships WHERE id=?"
    [ integer id ]
    member_row
  |> one

let get t ~actor id =
  access t actor (fun () ->
      let sub = subscription t id in
      (sub, source t sub.source_id))

let list t ~actor ~after =
  access t actor (fun () ->
      rows t.db
        "SELECT * FROM feeds_subscriptions WHERE id>? ORDER BY id LIMIT 5"
        [ integer after ]
        subscription_row
      |> List.map (fun (s : subscription) -> (s, source t s.source_id)))

let members t ~actor id =
  access t actor (fun () ->
      ignore (subscription t id);
      rows t.db
        "SELECT * FROM feeds_memberships WHERE subscription_id=? ORDER BY id"
        [ integer id ]
        member_row)

let poll_context t ~actor id =
  access t actor (fun () ->
      let m = member t id in
      (m, subscription t m.subscription_id, source t m.source_id))

let cancel_member t ~actor ~member_id =
  access t actor (fun () ->
      let m = member t member_id in
      execute t.db
        "UPDATE reminders SET state='cancelled',instruction='Nested OPML is \
         not expanded.' WHERE id=?"
        [ integer m.job_id ])

let status t ~actor ~subscription_id ~after =
  access t actor (fun () ->
      ignore (subscription t subscription_id);
      rows t.db
        "SELECT * FROM feeds_memberships WHERE subscription_id=? AND id>? \
         ORDER BY id LIMIT 5"
        [ integer subscription_id; integer after ]
        member_row
      |> List.map (fun (m : member) ->
          let state =
            rows t.db "SELECT state FROM reminders WHERE id=?"
              [ integer m.job_id ]
              (fun s -> Sqlite3.column_text s 0)
            |> one
          in
          (m, source t m.source_id, state)))

let ensure_source t url =
  execute t.db "INSERT OR IGNORE INTO feeds_sources(url) VALUES(?)" [ text url ];
  rows t.db "SELECT id FROM feeds_sources WHERE url=?"
    [ text url ]
    (fun s -> Sqlite3.column_int s 0)
  |> one

let ensure_member t (sub : subscription) ~source_id ~next_at =
  execute t.db
    "INSERT OR IGNORE INTO feeds_memberships(subscription_id,source_id) \
     VALUES(?,?)"
    [ integer sub.subscription_id; integer source_id ];
  if Sqlite3.changes (Sqlite3_eio.db t.db) > 0 then begin
    let id = last_id t.db in
    let state =
      try
        require t sub.creator;
        "active"
      with Invalid_argument _ -> "cancelled"
    in
    execute t.db
      "UPDATE feeds_memberships SET initialized=coalesce((SELECT \
       max(m.initialized) FROM feeds_memberships m JOIN feeds_subscriptions s \
       ON s.id=m.subscription_id WHERE m.source_id=? AND s.room=? AND \
       m.id<>?),0),cursor=coalesce((SELECT max(m.cursor) FROM \
       feeds_memberships m JOIN feeds_subscriptions s ON \
       s.id=m.subscription_id WHERE m.source_id=? AND s.room=? AND m.id<>?),0) \
       WHERE id=?"
      [
        integer source_id;
        text sub.room;
        integer id;
        integer source_id;
        text sub.room;
        integer id;
        integer id;
      ];
    execute t.db
      "INSERT INTO \
       reminders(creator,room,event,created_at,instruction,cron,until_at,next_at,state,tool_namespace,tool_key) \
       VALUES(?,?,?,?,?,?,?,?,?,'feeds',?)"
      [
        text sub.creator;
        text sub.room;
        text sub.event;
        text (stamp t);
        text "Poll subscribed feed and report new entries.";
        text sub.cron;
        optional (fun x -> Sqlite3.Data.FLOAT x) sub.until_at;
        Sqlite3.Data.FLOAT next_at;
        text state;
        integer id;
      ];
    let job_id = last_id t.db in
    execute t.db "UPDATE feeds_memberships SET job_id=? WHERE id=?"
      [ integer job_id; integer id ]
  end

let add t ~actor ~room ~event ~url ~cron ~until_at ~next_at =
  access t actor (fun () ->
      transaction t.db (fun () ->
          let source_id = ensure_source t url in
          execute t.db
            "INSERT OR IGNORE INTO \
             feeds_subscriptions(source_id,creator,room,event,created_at,cron,until_at) \
             VALUES(?,?,?,?,?,?,?)"
            [
              integer source_id;
              text actor;
              text room;
              text event;
              text (stamp t);
              text cron;
              optional (fun x -> Sqlite3.Data.FLOAT x) until_at;
            ];
          let fresh = Sqlite3.changes (Sqlite3_eio.db t.db) > 0 in
          let sub =
            rows t.db
              "SELECT * FROM feeds_subscriptions WHERE source_id=? AND room=?"
              [ integer source_id; text room ]
              subscription_row
            |> one
          in
          ensure_member t sub ~source_id ~next_at;
          (sub, fresh)))

let prune_sources t =
  execute t.db
    "DELETE FROM feeds_sources WHERE id NOT IN (SELECT source_id FROM \
     feeds_memberships UNION SELECT source_id FROM feeds_subscriptions)"
    []

let remove t ~actor id =
  access t actor (fun () ->
      transaction t.db (fun () ->
          execute t.db "DELETE FROM feeds_subscriptions WHERE id=?"
            [ integer id ];
          let removed = Sqlite3.changes (Sqlite3_eio.db t.db) > 0 in
          prune_sources t;
          removed))

let sync_opml t ~actor ~member_id ~urls =
  access t actor (fun () ->
      transaction t.db (fun () ->
          let m = member t member_id in
          let sub = subscription t m.subscription_id in
          if sub.source_id <> m.source_id then
            invalid_arg "Nested OPML subscriptions are not followed.";
          execute t.db "DELETE FROM feeds_outlines WHERE source_id=?"
            [ integer m.source_id ];
          List.iter
            (fun url ->
              execute t.db "INSERT OR IGNORE INTO feeds_outlines VALUES(?,?)"
                [ integer m.source_id; text url ])
            urls;
          let ids =
            List.map (ensure_source t) urls |> List.sort_uniq Int.compare
          in
          List.iter
            (fun source_id ->
              ensure_member t sub ~source_id ~next_at:(t.now ()))
            ids;
          let old =
            rows t.db "SELECT * FROM feeds_memberships WHERE subscription_id=?"
              [ integer sub.subscription_id ]
              member_row
          in
          List.iter
            (fun (member : member) ->
              if
                member.source_id <> sub.source_id
                && not (List.mem member.source_id ids)
              then
                execute t.db "DELETE FROM feeds_memberships WHERE id=?"
                  [ integer member.member_id ])
            old;
          prune_sources t))

let opml_urls t ~actor ~member_id =
  access t actor (fun () ->
      let m = member t member_id in
      rows t.db "SELECT url FROM feeds_outlines WHERE source_id=? ORDER BY url"
        [ integer m.source_id ]
        (fun s -> Sqlite3.column_text s 0))

let entry_columns =
  "e.id,e.source_id,e.entry_key,e.title,e.url,e.published,e.summary,e.observed_at,''"

let entries t ~actor ~subscription_id ~after =
  access t actor (fun () ->
      ignore (subscription t subscription_id);
      rows t.db
        ("SELECT " ^ entry_columns
       ^ " FROM feeds_entries e JOIN feeds_memberships m ON \
          m.source_id=e.source_id WHERE m.subscription_id=? AND e.id>? ORDER \
          BY e.id LIMIT 5")
        [ integer subscription_id; integer after ]
        entry_row)

let complete_poll ?next_url ?page_url t ~actor ~member_id ~kind ~title ~etag
    ~last_modified ~entries =
  access t actor (fun () ->
      transaction t.db (fun () ->
          let m = member t member_id in
          let src = source t m.source_id in
          let page_url = Option.value ~default:src.url page_url in
          let first_page = src.next_url = None in
          if first_page then
            execute t.db "DELETE FROM feeds_pages WHERE source_id=?"
              [ integer m.source_id ];
          execute t.db "INSERT OR IGNORE INTO feeds_pages VALUES(?,?)"
            [ integer m.source_id; text page_url ];
          Option.iter
            (fun url ->
              if
                rows t.db
                  "SELECT 1 FROM feeds_pages WHERE source_id=? AND url=?"
                  [ integer m.source_id; text url ]
                  (fun _ -> ())
                <> []
              then invalid_arg "Feed pagination contains a cycle.";
              let pages =
                rows t.db "SELECT count(*) FROM feeds_pages WHERE source_id=?"
                  [ integer m.source_id ]
                  (fun s -> Sqlite3.column_int s 0)
              in
              if List.hd pages >= 1000 then
                invalid_arg "Feed import exceeds 1000 pages.")
            next_url;
          List.iter
            (fun (entry : entry) ->
              execute t.db
                "INSERT OR IGNORE INTO feeds_seen(source_id,entry_key) \
                 VALUES(?,?)"
                [ integer m.source_id; text entry.key ];
              execute t.db
                "INSERT INTO \
                 feeds_entries(source_id,entry_key,title,url,published,summary,observed_at,content) \
                 VALUES(?,?,?,?,?,?,?,?) ON CONFLICT(source_id,entry_key) DO \
                 UPDATE SET \
                 title=excluded.title,url=excluded.url,published=excluded.published,summary=excluded.summary,content=excluded.content"
                [
                  integer m.source_id;
                  text entry.key;
                  text entry.title;
                  optional text entry.url;
                  optional text entry.published;
                  text entry.summary;
                  text (stamp t);
                  text entry.content;
                ])
            entries;
          execute t.db
            "UPDATE feeds_sources SET \
             kind=?,title=?,etag=?,last_modified=?,checked_at=?,success_at=?,error=NULL,failures=0,retry_at=0,next_url=? \
             WHERE id=?"
            [
              text kind;
              text (if first_page then title else src.title);
              optional text (if first_page then etag else src.etag);
              optional text
                (if first_page then last_modified else src.last_modified);
              Sqlite3.Data.FLOAT (t.now ());
              optional text
                (if next_url = None then Some (stamp t) else src.success_at);
              optional text next_url;
              integer m.source_id;
            ];
          if next_url <> None then
            execute t.db
              "UPDATE reminders SET next_at=min(next_at,?) WHERE \
               state='active' AND id IN (SELECT job_id FROM feeds_memberships \
               WHERE source_id=?) AND (until_at IS NULL OR until_at>=?)"
              [
                Sqlite3.Data.FLOAT (t.now () +. 60.);
                integer m.source_id;
                Sqlite3.Data.FLOAT (t.now () +. 60.);
              ]))

let search t ~actor ~subscription_id ~after ~query =
  access t actor (fun () ->
      ignore (subscription t subscription_id);
      if query = "" || String.length query > 256 || String.contains query '\000'
      then invalid_arg "Use a full-text query of 1 to 256 bytes.";
      try
        rows t.db
          ("SELECT " ^ entry_columns
         ^ " FROM feeds_fts JOIN feeds_entries e ON e.id=feeds_fts.rowid JOIN \
            feeds_memberships m ON m.source_id=e.source_id WHERE \
            m.subscription_id=? AND e.id>? AND feeds_fts MATCH ? ORDER BY e.id \
            LIMIT 5")
          [ integer subscription_id; integer after; text query ]
          entry_row
      with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
        invalid_arg "Invalid feed search. Use words, quoted phrases or prefix*.")

let read_content t ~actor ~subscription_id ~entry_id ~offset =
  access t actor (fun () ->
      if offset < 0 || offset > Feed_http.max_bytes then
        invalid_arg "Invalid article byte offset.";
      rows t.db
        "SELECT substr(CAST(e.content AS BLOB),?,2049),length(CAST(e.content \
         AS BLOB)) FROM feeds_entries e JOIN feeds_memberships m ON \
         m.source_id=e.source_id WHERE m.subscription_id=? AND e.id=?"
        [ integer (offset + 1); integer subscription_id; integer entry_id ]
        (fun s -> (Sqlite3.column_blob s 0, Sqlite3.column_int s 1))
      |> function
      | [ page ] -> page
      | _ -> invalid_arg "Entry is not in this subscription's mirror.")

let not_modified t ~actor ~member_id =
  access t actor (fun () ->
      let m = member t member_id in
      execute t.db
        "UPDATE feeds_sources SET \
         checked_at=?,success_at=?,error=NULL,failures=0,retry_at=0 WHERE id=?"
        [ Sqlite3.Data.FLOAT (t.now ()); text (stamp t); integer m.source_id ])

let failed t ~actor ~member_id message =
  access t actor (fun () ->
      let m = member t member_id in
      let s = source t m.source_id in
      let delay =
        min 86400. (300. *. (2. ** float_of_int (min 9 s.failures)))
      in
      execute t.db
        "UPDATE feeds_sources SET \
         checked_at=?,error=?,failures=failures+1,retry_at=? WHERE id=?"
        [
          Sqlite3.Data.FLOAT (t.now ());
          text message;
          Sqlite3.Data.FLOAT (t.now () +. delay);
          integer s.source_id;
        ])

let pending t ~actor ~member_id =
  access t actor (fun () ->
      transaction t.db (fun () ->
          let m = member t member_id in
          let s = source t m.source_id in
          if s.success_at = None || s.next_url <> None || s.kind = "opml" then
            []
          else if not m.initialized then begin
            let sub = subscription t m.subscription_id in
            execute t.db
              "UPDATE feeds_memberships SET initialized=1,cursor=(SELECT \
               coalesce(max(id),0) FROM feeds_entries WHERE source_id=?) WHERE \
               source_id=? AND subscription_id IN (SELECT id FROM \
               feeds_subscriptions WHERE room=?)"
              [ integer m.source_id; integer m.source_id; text sub.room ];
            []
          end
          else
            rows t.db
              ("SELECT " ^ entry_columns
             ^ " FROM feeds_entries e WHERE source_id=? AND id>? ORDER BY id \
                LIMIT 10")
              [ integer m.source_id; integer m.cursor ]
              entry_row))

let acknowledge t ~actor ~member_id ~through =
  access t actor (fun () ->
      let m = member t member_id in
      let sub = subscription t m.subscription_id in
      execute t.db
        "UPDATE feeds_memberships SET cursor=max(cursor,?) WHERE source_id=? \
         AND subscription_id IN (SELECT id FROM feeds_subscriptions WHERE \
         room=?)"
        [ integer through; integer m.source_id; text sub.room ])

let request_poll t ~actor id =
  access t actor (fun () ->
      let sub = subscription t id in
      execute t.db
        "UPDATE reminders SET next_at=min(next_at,max(?,coalesce((SELECT \
         max(scheduled_at)+1 FROM reminder_runs WHERE \
         reminder_id=reminders.id),0))) WHERE state='active' AND id IN (SELECT \
         job_id FROM feeds_memberships WHERE subscription_id=? AND \
         source_id<>?)"
        [ Sqlite3.Data.FLOAT (t.now ()); integer id; integer sub.source_id ])
