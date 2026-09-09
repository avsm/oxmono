type role = Friend | Bot | Unknown

let role_string = function
  | Friend -> "friend"
  | Bot -> "bot"
  | Unknown -> "unknown"

let role_of_string = function
  | "friend" -> Friend
  | "bot" -> Bot
  | "unknown" -> Unknown
  | _ -> invalid_arg "role must be friend, bot or unknown"

type person = { user : string; role : role; allowed : bool }
type message = { role : string; body : string }

type t = {
  db : Sqlite3_eio.t;
  admin : string;
  mutex : Eio.Mutex.t;
  now : unit -> float;
}

let timestamp now =
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let today t = String.sub (timestamp (t.now ())) 0 10
let yesterday t = String.sub (timestamp (t.now () -. 86400.)) 0 10
let now t = t.now ()

open Persistence

let locked t f = Persistence.locked t.mutex f

let create ?(now = Unix.gettimeofday) db ~admin =
  ignore (Matrix_proto.Id.User_id.of_string_exn admin);
  let version =
    rows db "PRAGMA user_version" [] (fun s -> Sqlite3.column_int s 0)
  in
  if not (List.mem version [ [ 0 ]; [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ]) then
    invalid_arg "unsupported crowthebot database version";
  sql db "PRAGMA foreign_keys=ON; PRAGMA secure_delete=ON;";
  sql db "BEGIN IMMEDIATE";
  try
    sql db
      {|
CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS people (
 user TEXT PRIMARY KEY, role TEXT NOT NULL CHECK(role IN ('friend','bot','unknown')),
 allowed INTEGER NOT NULL CHECK(allowed IN (0,1)));
CREATE TABLE IF NOT EXISTS rooms (room TEXT PRIMARY KEY);
CREATE TABLE IF NOT EXISTS direct_rooms (room TEXT PRIMARY KEY, peer TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS history (
 id INTEGER PRIMARY KEY, room TEXT NOT NULL, user TEXT NOT NULL,
 role TEXT NOT NULL, body TEXT NOT NULL);
CREATE INDEX IF NOT EXISTS history_thread ON history(room,user,id);
CREATE TABLE IF NOT EXISTS events (
 id INTEGER PRIMARY KEY, room TEXT NOT NULL, event TEXT NOT NULL UNIQUE);
CREATE TABLE IF NOT EXISTS tool_uses (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 started_at TEXT NOT NULL, finished_at TEXT, day TEXT NOT NULL,
 actor TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 source TEXT NOT NULL, call_id TEXT NOT NULL, tool TEXT NOT NULL,
 arguments TEXT NOT NULL, result TEXT NOT NULL DEFAULT '',
 status TEXT NOT NULL CHECK(status IN
 ('running','ok','rejected','error','cancelled','interrupted')));
CREATE INDEX IF NOT EXISTS tool_uses_day ON tool_uses(day,id);
CREATE TABLE IF NOT EXISTS facts (
 id INTEGER PRIMARY KEY AUTOINCREMENT, created_at TEXT NOT NULL,
 author TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 source TEXT NOT NULL, body TEXT NOT NULL);
CREATE VIRTUAL TABLE IF NOT EXISTS facts_fts USING fts5(
 body, content='facts', content_rowid='id', tokenize='unicode61');
CREATE TRIGGER IF NOT EXISTS facts_insert AFTER INSERT ON facts BEGIN
 INSERT INTO facts_fts(rowid,body) VALUES(new.id,new.body);
END;
CREATE TRIGGER IF NOT EXISTS facts_delete AFTER DELETE ON facts BEGIN
 INSERT INTO facts_fts(facts_fts,rowid,body) VALUES('delete',old.id,old.body);
END;
INSERT INTO facts_fts(facts_fts,rank) VALUES('secure-delete',1);
CREATE TABLE IF NOT EXISTS daily_notes (
 day TEXT PRIMARY KEY, created_at TEXT NOT NULL, model TEXT NOT NULL,
 last_tool_id INTEGER NOT NULL, tool_count INTEGER NOT NULL, body TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS reminders (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 fact_id INTEGER NOT NULL REFERENCES facts(id) ON DELETE CASCADE,
 creator TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 created_at TEXT NOT NULL, instruction TEXT NOT NULL,
 cron TEXT, until_at REAL, next_at REAL NOT NULL,
 state TEXT NOT NULL CHECK(state IN ('active','completed','cancelled')));
CREATE INDEX IF NOT EXISTS reminders_due ON reminders(state,next_at);
CREATE TABLE IF NOT EXISTS reminder_runs (
 id INTEGER PRIMARY KEY AUTOINCREMENT, reminder_id INTEGER NOT NULL,
 scheduled_at REAL NOT NULL, started_at TEXT NOT NULL, finished_at TEXT,
 status TEXT NOT NULL, UNIQUE(reminder_id,scheduled_at));
|};
    if version <> [ 5 ] then
      sql db
        {|
ALTER TABLE reminders RENAME TO reminders_v4;
DROP INDEX reminders_due;
CREATE TABLE reminders (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 fact_id INTEGER REFERENCES facts(id) ON DELETE CASCADE,
 creator TEXT NOT NULL, room TEXT NOT NULL, event TEXT NOT NULL,
 created_at TEXT NOT NULL, instruction TEXT NOT NULL,
 cron TEXT, until_at REAL, next_at REAL NOT NULL,
 state TEXT NOT NULL CHECK(state IN ('active','completed','cancelled')),
 tool_namespace TEXT, tool_key INTEGER,
 CHECK((fact_id IS NOT NULL AND tool_namespace IS NULL AND tool_key IS NULL)
 OR (fact_id IS NULL AND tool_namespace IS NOT NULL AND tool_key IS NOT NULL)));
INSERT INTO reminders SELECT *,NULL,NULL FROM reminders_v4;
UPDATE sqlite_sequence SET seq=max(seq,coalesce((SELECT seq FROM sqlite_sequence WHERE name='reminders_v4'),0)) WHERE name='reminders';
DROP TABLE reminders_v4;
CREATE INDEX reminders_due ON reminders(state,next_at);
|};
    Feed_store.init db;
    Location_store.init db;
    sql db "PRAGMA user_version=5";
    execute db "INSERT OR IGNORE INTO settings VALUES ('admin',?)"
      [ text admin ];
    let stored =
      rows db "SELECT value FROM settings WHERE key='admin'" [] (fun s ->
          Sqlite3.column_text s 0)
    in
    if stored <> [ admin ] then
      invalid_arg "configured admin differs from database authority";
    let started_at = timestamp (now ()) in
    execute db "INSERT OR IGNORE INTO settings VALUES ('daily_since',?)"
      [ text (String.sub started_at 0 10) ];
    execute db
      "UPDATE tool_uses SET status='interrupted',finished_at=?,result='Process \
       stopped before the result was recorded.' WHERE status='running'"
      [ text started_at ];
    execute db
      "UPDATE reminder_runs SET status='interrupted',finished_at=? WHERE \
       status='running'"
      [ text started_at ];
    sql db "COMMIT";
    { db; admin; mutex = Eio.Mutex.create (); now }
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () -> sql db "ROLLBACK");
    Printexc.raise_with_backtrace exn bt

let admin t = t.admin

let person_unlocked t user =
  if user = t.admin then { user; role = Friend; allowed = true }
  else
    match
      rows t.db "SELECT role,allowed FROM people WHERE user=?"
        [ text user ]
        (fun s ->
          {
            user;
            role = role_of_string (Sqlite3.column_text s 0);
            allowed = Sqlite3.column_int s 1 = 1;
          })
    with
    | [ p ] -> p
    | _ -> { user; role = Unknown; allowed = false }

let person t user = locked t (fun () -> person_unlocked t user)

let observe t user =
  locked t @@ fun () ->
  if user <> t.admin then
    execute t.db
      {|
INSERT OR IGNORE INTO people SELECT ?, 'unknown', 0
WHERE (SELECT count(*) FROM people WHERE role='unknown') < 1000
|}
      [ text user ]

let set_person t ~actor ~user ~role ~allowed =
  if actor <> t.admin then
    invalid_arg "only the primary admin may change access";
  ignore (Matrix_proto.Id.User_id.of_string_exn user);
  if user = t.admin then invalid_arg "the primary admin cannot be changed";
  if role = Unknown && allowed then
    invalid_arg "unknown people cannot be allowed";
  locked t @@ fun () ->
  sql t.db "BEGIN IMMEDIATE";
  try
    execute t.db
      {|
INSERT INTO people VALUES (?,?,?) ON CONFLICT(user) DO UPDATE SET
role=excluded.role, allowed=excluded.allowed
|}
      [ text user; text (role_string role); integer (if allowed then 1 else 0) ];
    (* Revocation removes context immediately, including earlier assistant replies. *)
    if not allowed then
      execute t.db "DELETE FROM history WHERE user=?" [ text user ];
    if (not allowed) || role <> Friend then
      execute t.db "UPDATE reminders SET state='cancelled' WHERE creator=?"
        [ text user ];
    sql t.db "COMMIT"
  with exn ->
    sql t.db "ROLLBACK";
    raise exn

let people t =
  locked t @@ fun () ->
  { user = t.admin; role = Friend; allowed = true }
  :: rows t.db "SELECT user,role,allowed FROM people ORDER BY user" [] (fun s ->
      {
        user = Sqlite3.column_text s 0;
        role = role_of_string (Sqlite3.column_text s 1);
        allowed = Sqlite3.column_int s 2 = 1;
      })

let add_room t room =
  ignore (Matrix_proto.Id.Room_id.of_string_exn room);
  locked t (fun () ->
      execute t.db "INSERT OR IGNORE INTO rooms VALUES (?)" [ text room ])

let rooms t =
  locked t (fun () ->
      rows t.db "SELECT room FROM rooms ORDER BY room" [] (fun s ->
          Sqlite3.column_text s 0))

let add_direct_room t ~room ~peer =
  ignore (Matrix_proto.Id.Room_id.of_string_exn room);
  ignore (Matrix_proto.Id.User_id.of_string_exn peer);
  locked t (fun () ->
      execute t.db
        "INSERT INTO direct_rooms VALUES (?,?) ON CONFLICT(room) DO UPDATE SET \
         peer=excluded.peer"
        [ text room; text peer ])

let direct_peer t room =
  locked t (fun () ->
      match
        rows t.db "SELECT peer FROM direct_rooms WHERE room=?"
          [ text room ]
          (fun s -> Sqlite3.column_text s 0)
      with
      | [ peer ] -> Some peer
      | _ -> None)

let clear t ~room ~user =
  locked t (fun () ->
      execute t.db "DELETE FROM history WHERE room=? AND user=?"
        [ text room; text user ])

let history t ~room ~user =
  locked t (fun () ->
      rows t.db
        "SELECT role,body FROM history WHERE room=? AND user=? ORDER BY id"
        [ text room; text user ]
        (fun s ->
          { role = Sqlite3.column_text s 0; body = Sqlite3.column_text s 1 }))

let claim t ~room ~event =
  locked t @@ fun () ->
  execute t.db "INSERT OR IGNORE INTO events(room,event) VALUES (?,?)"
    [ text room; text event ];
  let fresh = Sqlite3.changes (Sqlite3_eio.db t.db) = 1 in
  execute t.db
    {|
DELETE FROM events WHERE room=? AND id NOT IN
(SELECT id FROM events WHERE room=? ORDER BY id DESC LIMIT 2048)
|}
    [ text room; text room ];
  fresh

let append t ~room ~user ~max_messages ~max_bytes messages =
  if max_messages < 0 || max_bytes < 0 then invalid_arg "negative context limit";
  locked t @@ fun () ->
  sql t.db "BEGIN IMMEDIATE";
  try
    List.iter
      (fun (m : message) ->
        execute t.db "INSERT INTO history(room,user,role,body) VALUES (?,?,?,?)"
          [ text room; text user; text m.role; text m.body ])
      messages;
    let existing =
      rows t.db
        "SELECT id,length(CAST(body AS BLOB)) FROM history WHERE room=? AND \
         user=? ORDER BY id DESC"
        [ text room; text user ]
        (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))
    in
    let rec trim n bytes = function
      | [] -> ()
      | (id, size) :: rest ->
          if n >= max_messages || bytes + size > max_bytes then
            execute t.db "DELETE FROM history WHERE room=? AND user=? AND id<=?"
              [ text room; text user; integer id ]
          else trim (n + 1) (bytes + size) rest
    in
    trim 0 0 existing;
    sql t.db "COMMIT"
  with exn ->
    sql t.db "ROLLBACK";
    raise exn

let require_friend t actor =
  let person = person_unlocked t actor in
  if (not person.allowed) || person.role <> Friend then
    invalid_arg "Memory is available to the primary admin and allowed friends."

type fact = {
  fact_id : int;
  created_at : string;
  author : string;
  room : string;
  event : string;
  source : string;
  body : string;
}

let fact_row s =
  {
    fact_id = Sqlite3.column_int s 0;
    created_at = Sqlite3.column_text s 1;
    author = Sqlite3.column_text s 2;
    room = Sqlite3.column_text s 3;
    event = Sqlite3.column_text s 4;
    source = Sqlite3.column_text s 5;
    body = Sqlite3.column_text s 6;
  }

let add_fact t ~actor ~room ~event ~source ~body =
  locked t @@ fun () ->
  require_friend t actor;
  let body = String.trim body in
  if body = "" || String.length body > 2048 || String.contains body '\000' then
    invalid_arg "A fact must contain 1 to 2048 bytes of text.";
  if source <> "command" && source <> "observation" then
    invalid_arg "invalid fact source";
  execute t.db
    "INSERT INTO facts(created_at,author,room,event,source,body) VALUES \
     (?,?,?,?,?,?)"
    [
      text (timestamp (t.now ()));
      text actor;
      text room;
      text event;
      text source;
      text body;
    ];
  Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db t.db))

let get_fact t ~actor id =
  locked t @@ fun () ->
  require_friend t actor;
  match rows t.db "SELECT * FROM facts WHERE id=?" [ integer id ] fact_row with
  | [ fact ] -> Some fact
  | _ -> None

let search_facts t ~actor ~query =
  locked t @@ fun () ->
  require_friend t actor;
  let query = String.trim query in
  if String.length query > 256 || String.contains query '\000' then
    invalid_arg "Search query must be at most 256 bytes.";
  if query = "" then
    rows t.db "SELECT * FROM facts ORDER BY id DESC LIMIT 20" [] fact_row
  else
    try
      rows t.db
        "SELECT f.* FROM facts_fts JOIN facts f ON f.id=facts_fts.rowid WHERE \
         facts_fts MATCH ? ORDER BY rank,f.id DESC LIMIT 20"
        [ text query ]
        fact_row
    with Sqlite3.Error _ | Sqlite3.SqliteError _ ->
      invalid_arg
        "Invalid full-text query. Use words, quoted phrases or prefix*."

let erase_fact t ~actor id =
  locked t @@ fun () ->
  require_friend t actor;
  execute t.db "DELETE FROM facts WHERE id=?" [ integer id ];
  Sqlite3.changes (Sqlite3_eio.db t.db) > 0

type tool_use = {
  log_id : int;
  started_at : string;
  finished_at : string option;
  actor : string;
  room : string;
  event : string;
  source : string;
  call_id : string;
  tool : string;
  arguments : string;
  result : string;
  status : string;
}

let start_tool t ~actor ~room ~event ~source ~call_id ~tool ~arguments =
  locked t @@ fun () ->
  let started_at = timestamp (t.now ()) in
  execute t.db
    "INSERT INTO \
     tool_uses(started_at,day,actor,room,event,source,call_id,tool,arguments,status) \
     VALUES (?,?,?,?,?,?,?,?,?,'running')"
    [
      text started_at;
      text (String.sub started_at 0 10);
      text actor;
      text room;
      text event;
      text source;
      text call_id;
      text tool;
      text arguments;
    ];
  Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db t.db))

let finish_tool t id ~status ~result =
  locked t (fun () ->
      execute t.db
        "UPDATE tool_uses SET finished_at=?,status=?,result=? WHERE id=? AND \
         status='running'"
        [ text (timestamp (t.now ())); text status; text result; integer id ])

let validate_day day =
  let invalid () = invalid_arg "Date must be a valid UTC day, YYYY-MM-DD." in
  if String.length day <> 10 || day.[4] <> '-' || day.[7] <> '-' then invalid ();
  let number start len =
    let value = String.sub day start len in
    if not (String.for_all (function '0' .. '9' -> true | _ -> false) value)
    then invalid ();
    int_of_string value
  in
  let year = number 0 4 and month = number 5 2 and date = number 8 2 in
  let leap = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in
  let days =
    match month with
    | 2 -> if leap then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | _ -> 0
  in
  if year = 0 || date < 1 || date > days then invalid ()

let tool_row s =
  {
    log_id = Sqlite3.column_int s 0;
    started_at = Sqlite3.column_text s 1;
    finished_at =
      (match Sqlite3.column s 2 with
      | Sqlite3.Data.NULL -> None
      | _ -> Some (Sqlite3.column_text s 2));
    actor = Sqlite3.column_text s 3;
    room = Sqlite3.column_text s 4;
    event = Sqlite3.column_text s 5;
    source = Sqlite3.column_text s 6;
    call_id = Sqlite3.column_text s 7;
    tool = Sqlite3.column_text s 8;
    arguments = Sqlite3.column_text s 9;
    result = Sqlite3.column_text s 10;
    status = Sqlite3.column_text s 11;
  }

let tool_uses t ~day ~after ~through ~limit =
  validate_day day;
  if limit < 1 || limit > 100 || after < 0 then invalid_arg "invalid log page";
  locked t (fun () ->
      rows t.db
        "SELECT \
         id,started_at,finished_at,actor,room,event,source,call_id,tool,arguments,result,status \
         FROM tool_uses WHERE day=? AND id>? AND id<=? ORDER BY id LIMIT ?"
        [ text day; integer after; integer through; integer limit ]
        tool_row)

let tool_snapshot t ~day =
  validate_day day;
  locked t (fun () ->
      match
        rows t.db
          "SELECT \
           coalesce(max(id),0),count(*),coalesce(sum(status='running'),0) FROM \
           tool_uses WHERE day=?"
          [ text day ]
          (fun s ->
            ( Sqlite3.column_int s 0,
              Sqlite3.column_int s 1,
              Sqlite3.column_int s 2 > 0 ))
      with
      | [ snapshot ] -> snapshot
      | _ -> assert false)

type daily_note = {
  day : string;
  generated_at : string;
  model : string;
  last_tool_id : int;
  tool_count : int;
  body : string;
}

let get_note t day =
  validate_day day;
  locked t (fun () ->
      match
        rows t.db "SELECT * FROM daily_notes WHERE day=?"
          [ text day ]
          (fun s ->
            {
              day = Sqlite3.column_text s 0;
              generated_at = Sqlite3.column_text s 1;
              model = Sqlite3.column_text s 2;
              last_tool_id = Sqlite3.column_int s 3;
              tool_count = Sqlite3.column_int s 4;
              body = Sqlite3.column_text s 5;
            })
      with
      | [ note ] -> Some note
      | _ -> None)

let save_note t ~day ~model ~last_tool_id ~tool_count ~body =
  validate_day day;
  locked t (fun () ->
      execute t.db
        "INSERT INTO daily_notes VALUES (?,?,?,?,?,?) ON CONFLICT(day) DO \
         UPDATE SET \
         created_at=excluded.created_at,model=excluded.model,last_tool_id=excluded.last_tool_id,tool_count=excluded.tool_count,body=excluded.body"
        [
          text day;
          text (timestamp (t.now ()));
          text model;
          integer last_tool_id;
          integer tool_count;
          text body;
        ])

let pending_note_days t =
  locked t (fun () ->
      rows t.db
        {|
WITH RECURSIVE days(day) AS (
 SELECT value FROM settings WHERE key='daily_since'
 UNION ALL SELECT date(day,'+1 day') FROM days WHERE day < date(?,'-1 day')
), candidates(day) AS (
 SELECT day FROM days UNION SELECT day FROM tool_uses
)
SELECT c.day FROM candidates c LEFT JOIN daily_notes n ON n.day=c.day
WHERE c.day < ?
 AND NOT EXISTS (SELECT 1 FROM tool_uses u WHERE u.day=c.day AND status='running')
 AND (n.day IS NULL OR n.last_tool_id <
      (SELECT coalesce(max(id),0) FROM tool_uses u WHERE u.day=c.day))
ORDER BY c.day LIMIT 7
|}
        [ text (today t); text (today t) ]
        (fun s -> Sqlite3.column_text s 0))

type reminder_target =
  | Memory of int
  | Tool of { namespace : string; key : int }

type reminder = {
  reminder_id : int;
  target : reminder_target;
  creator : string;
  room : string;
  event : string;
  created_at : string;
  instruction : string;
  cron : string option;
  until_at : float option;
  next_at : float;
  state : string;
}

let reminder_row s =
  {
    reminder_id = Sqlite3.column_int s 0;
    target =
      (match Sqlite3.column s 1 with
      | Sqlite3.Data.NULL ->
          Tool
            {
              namespace = Sqlite3.column_text s 11;
              key = Sqlite3.column_int s 12;
            }
      | _ -> Memory (Sqlite3.column_int s 1));
    creator = Sqlite3.column_text s 2;
    room = Sqlite3.column_text s 3;
    event = Sqlite3.column_text s 4;
    created_at = Sqlite3.column_text s 5;
    instruction = Sqlite3.column_text s 6;
    cron =
      (match Sqlite3.column s 7 with
      | Sqlite3.Data.NULL -> None
      | _ -> Some (Sqlite3.column_text s 7));
    until_at =
      (match Sqlite3.column s 8 with
      | Sqlite3.Data.NULL -> None
      | _ -> Some (Sqlite3.column_double s 8));
    next_at = Sqlite3.column_double s 9;
    state = Sqlite3.column_text s 10;
  }

let add_reminder t ~actor ~room ~event ~fact_id ~instruction ~cron ~until_at
    ~next_at =
  locked t @@ fun () ->
  require_friend t actor;
  let instruction = String.trim instruction in
  if instruction = "" || String.length instruction > 2048 then
    invalid_arg "Reminder instruction must contain 1 to 2048 bytes.";
  if
    rows t.db "SELECT id FROM facts WHERE id=?"
      [ integer fact_id ]
      (fun s -> Sqlite3.column_int s 0)
    = []
  then invalid_arg "Link the reminder to an existing memory fact.";
  execute t.db
    "INSERT INTO \
     reminders(fact_id,creator,room,event,created_at,instruction,cron,until_at,next_at,state) \
     VALUES (?,?,?,?,?,?,?,?,?,'active')"
    [
      integer fact_id;
      text actor;
      text room;
      text event;
      text (timestamp (t.now ()));
      text instruction;
      Option.fold ~none:Sqlite3.Data.NULL ~some:text cron;
      Option.fold ~none:Sqlite3.Data.NULL
        ~some:(fun x -> Sqlite3.Data.FLOAT x)
        until_at;
      Sqlite3.Data.FLOAT next_at;
    ];
  Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db t.db))

let reminders t ~actor =
  locked t @@ fun () ->
  require_friend t actor;
  rows t.db
    "SELECT * FROM reminders WHERE state='active' ORDER BY next_at LIMIT 50" []
    reminder_row

let get_reminder t id =
  locked t (fun () ->
      match
        rows t.db "SELECT * FROM reminders WHERE id=?"
          [ integer id ]
          reminder_row
      with
      | [ reminder ] -> Some reminder
      | _ -> None)

let cancel_reminder t ~actor id =
  locked t @@ fun () ->
  require_friend t actor;
  execute t.db
    "UPDATE reminders SET state='cancelled' WHERE id=? AND state<>'cancelled'"
    [ integer id ];
  Sqlite3.changes (Sqlite3_eio.db t.db) > 0

let due_reminders t =
  locked t (fun () ->
      rows t.db
        "SELECT * FROM reminders WHERE state='active' AND next_at<=? ORDER BY \
         next_at,id LIMIT 20"
        [ Sqlite3.Data.FLOAT (t.now ()) ]
        reminder_row)

let claim_reminder t (job : reminder) ~next_at =
  locked t @@ fun () ->
  sql t.db "BEGIN IMMEDIATE";
  try
    execute t.db
      "INSERT OR IGNORE INTO \
       reminder_runs(reminder_id,scheduled_at,started_at,status) SELECT \
       id,next_at,?,'running' FROM reminders WHERE id=? AND state='active' AND \
       next_at=? AND next_at<=?"
      [
        text (timestamp (t.now ()));
        integer job.reminder_id;
        Sqlite3.Data.FLOAT job.next_at;
        Sqlite3.Data.FLOAT (t.now ());
      ];
    let run_id =
      if Sqlite3.changes (Sqlite3_eio.db t.db) = 0 then None
      else Some (Int64.to_int (Sqlite3.last_insert_rowid (Sqlite3_eio.db t.db)))
    in
    (match run_id with
    | None -> ()
    | Some _ ->
        execute t.db "UPDATE reminders SET next_at=?,state=? WHERE id=?"
          [
            Sqlite3.Data.FLOAT (Option.value ~default:job.next_at next_at);
            text (if next_at = None then "completed" else "active");
            integer job.reminder_id;
          ]);
    sql t.db "COMMIT";
    run_id
  with exn ->
    sql t.db "ROLLBACK";
    raise exn

let finish_reminder t id ~status =
  locked t (fun () ->
      execute t.db "UPDATE reminder_runs SET status=?,finished_at=? WHERE id=?"
        [ text status; text (timestamp (t.now ())); integer id ])

let feeds t =
  Feed_store.create ~db:t.db ~mutex:t.mutex ~admin:t.admin ~now:t.now ~timestamp

let locations t =
  Location_store.create ~db:t.db ~mutex:t.mutex ~admin:t.admin ~now:t.now
    ~timestamp
