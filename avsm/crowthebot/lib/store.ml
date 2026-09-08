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
type t = { db : Sqlite3_eio.t; admin : string; mutex : Eio.Mutex.t }

let sql db source = Sqlite3.Rc.check (Sqlite3_eio.exec db source)

let statement db source args f =
  let stmt = Sqlite3_eio.prepare db source in
  Fun.protect ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> ignore (Sqlite3_eio.finalize db stmt)))
  @@ fun () ->
  List.iteri (fun i v -> Sqlite3.Rc.check (Sqlite3.bind stmt (i + 1) v)) args;
  f stmt

let execute db source args =
  statement db source args (fun stmt ->
      Sqlite3.Rc.check (Sqlite3_eio.step db stmt))

let rows db source args f =
  statement db source args (fun stmt ->
      let rec loop acc =
        match Sqlite3_eio.step db stmt with
        | Sqlite3.Rc.ROW -> loop (f stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            Sqlite3.Rc.check rc;
            assert false
      in
      loop [])

let text s = Sqlite3.Data.TEXT s
let integer i = Sqlite3.Data.INT (Int64.of_int i)
let locked t f = Eio.Mutex.use_rw ~protect:true t.mutex f

let create db ~admin =
  ignore (Matrix_proto.Id.User_id.of_string_exn admin);
  let version =
    rows db "PRAGMA user_version" [] (fun s -> Sqlite3.column_int s 0)
  in
  if version <> [ 0 ] && version <> [ 1 ] then
    invalid_arg "unsupported crowthebot database version";
  sql db "PRAGMA foreign_keys=ON; PRAGMA secure_delete=ON;";
  sql db
    {|
PRAGMA user_version=1;
CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS people (
 user TEXT PRIMARY KEY, role TEXT NOT NULL CHECK(role IN ('friend','bot','unknown')),
 allowed INTEGER NOT NULL CHECK(allowed IN (0,1)));
CREATE TABLE IF NOT EXISTS rooms (room TEXT PRIMARY KEY);
CREATE TABLE IF NOT EXISTS history (
 id INTEGER PRIMARY KEY, room TEXT NOT NULL, user TEXT NOT NULL,
 role TEXT NOT NULL, body TEXT NOT NULL);
CREATE INDEX IF NOT EXISTS history_thread ON history(room,user,id);
CREATE TABLE IF NOT EXISTS events (
 id INTEGER PRIMARY KEY, room TEXT NOT NULL, event TEXT NOT NULL UNIQUE);
|};
  execute db "INSERT OR IGNORE INTO settings VALUES ('admin',?)" [ text admin ];
  let stored =
    rows db "SELECT value FROM settings WHERE key='admin'" [] (fun s ->
        Sqlite3.column_text s 0)
  in
  if stored <> [ admin ] then
    invalid_arg "configured admin differs from database authority";
  { db; admin; mutex = Eio.Mutex.create () }

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
