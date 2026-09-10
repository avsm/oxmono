open Persistence

type t = {
  db : Sqlite3_eio.t;
  mutex : Eio.Mutex.t;
  admin : string;
  now : unit -> float;
}

let create ~db ~mutex ~admin ~now = { db; mutex; admin; now }

let init db =
  sql db
    {|CREATE TABLE IF NOT EXISTS email_results(
id INTEGER PRIMARY KEY AUTOINCREMENT, connection TEXT NOT NULL,
mode TEXT NOT NULL, operation TEXT NOT NULL, actor TEXT NOT NULL,
room TEXT NOT NULL, event TEXT NOT NULL, observed REAL NOT NULL,
data BLOB NOT NULL);
INSERT OR IGNORE INTO tool_schemas VALUES('email',1);|};
  match
    rows db "SELECT version FROM tool_schemas WHERE name='email'" [] (fun s ->
        Sqlite3.column_int s 0)
  with
  | [ 1 ] -> ()
  | _ -> invalid_arg "Unsupported email tool schema."

let require t actor =
  if
    actor <> t.admin
    && rows t.db
         "SELECT 1 FROM people WHERE user=? AND role='friend' AND allowed=1"
         [ text actor ]
         (fun _ -> ())
       = []
  then invalid_arg "Email tools require the admin or an allowed friend."

let access t actor f =
  locked t.mutex (fun () ->
      require t actor;
      f ())

let authorize t ~actor = access t actor (fun () -> ())
let expired t = Sqlite3.Data.FLOAT (t.now () -. 86400.)

let save t ~actor ~room ~event ~connection ~writable ~operation data =
  if String.length data > 33554432 then
    invalid_arg "Email result exceeds the 32 MiB cache limit.";
  access t actor (fun () ->
      transaction t.db (fun () ->
          execute t.db "DELETE FROM email_results WHERE observed < ?"
            [ expired t ];
          execute t.db
            "INSERT INTO \
             email_results(connection,mode,operation,actor,room,event,observed,data) \
             VALUES(?,?,?,?,?,?,?,?)"
            [
              text connection;
              text (if writable then "rw" else "ro");
              text operation;
              text actor;
              text room;
              text event;
              Sqlite3.Data.FLOAT (t.now ());
              Sqlite3.Data.BLOB data;
            ];
          let id = last_id t.db in
          (* Retain at most 50 immutable snapshots and 64 MiB of JSON. *)
          execute t.db "DELETE FROM email_results WHERE id <= ?"
            [ integer (id - 50) ];
          let sizes () =
            rows t.db
              "SELECT id,length(data) FROM email_results ORDER BY id DESC" []
              (fun s -> (Sqlite3.column_int s 0, Sqlite3.column_int s 1))
          in
          let total = ref 0 in
          List.iter
            (fun (key, size) ->
              total := !total + size;
              if !total > 67108864 then
                execute t.db "DELETE FROM email_results WHERE id=?"
                  [ integer key ])
            (sizes ());
          id))

type page = {
  data : string;
  total : int;
  observed : float;
  connection : string;
  mode : string;
}

let read t ~actor ~id ~offset =
  if id < 1 || offset < 0 || offset >= max_int then
    invalid_arg "Invalid email result ID or offset.";
  access t actor (fun () ->
      match
        rows t.db
          "SELECT substr(data,?,4096),length(data),observed,connection,mode \
           FROM email_results WHERE id=? AND observed >= ?"
          [ integer (offset + 1); integer id; expired t ]
          (fun s ->
            {
              data = Sqlite3.column_blob s 0;
              total = Sqlite3.column_int s 1;
              observed = Sqlite3.column_double s 2;
              connection = Sqlite3.column_text s 3;
              mode = Sqlite3.column_text s 4;
            })
      with
      | [ p ] when offset <= p.total -> p
      | [ _ ] -> invalid_arg "Email page offset exceeds the result size."
      | _ ->
          invalid_arg
            "Email result expired or was evicted. Repeat the original read or \
             query.")
