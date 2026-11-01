module S = Retrieval.Sql

let check message condition = if not condition then failwith message

let () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      S.execute db "PRAGMA foreign_keys=ON" [];
      S.execute db "CREATE TABLE parent (id INTEGER PRIMARY KEY)" [];
      S.execute db
        "CREATE TABLE child (parent_id INTEGER REFERENCES parent(id) \
         DEFERRABLE INITIALLY DEFERRED)"
        [];
      let inserted = ref false in
      check "deferred constraint must fail at commit"
        (try
           S.transaction db (fun () ->
               S.execute db "INSERT INTO child VALUES (1)" [];
               inserted := true);
           false
         with Sqlite3.SqliteError _ -> !inserted);
      check "failed commit rolls back its writes"
        (S.rows db "SELECT parent_id FROM child" [] = []);
      S.transaction db (fun () ->
          S.execute db "INSERT INTO parent VALUES (1)" [];
          S.execute db "INSERT INTO child VALUES (1)" []);
      check "connection remains usable after a failed commit"
        (S.rows db "SELECT parent_id FROM child" []
        = [ [| Sqlite3.Data.INT 1L |] ]))
