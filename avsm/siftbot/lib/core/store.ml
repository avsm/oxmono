module M = Siftbot_model
module D = Sqlite3.Data

let check = Sqlite3.Rc.check

exception Conflict

type t = { sql : Sqlite3_eio.t; lock : Eio.Mutex.t }

let statement db sql args f =
  let s = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> check (Sqlite3.finalize s))
    (fun () ->
      List.iteri (fun i d -> check (Sqlite3.bind s (i + 1) d)) args;
      f s)

let execute db sql args =
  statement db sql args (fun s -> check (Sqlite3.step s))

let rows db sql args =
  statement db sql args (fun s ->
      let rec loop acc =
        match Sqlite3.step s with
        | ROW -> loop (Sqlite3.row_data s :: acc)
        | DONE -> List.rev acc
        | rc ->
            check rc;
            assert false
      in
      loop [])

let str = function D.TEXT s -> s | _ -> failwith "Invalid database text"
let num = function D.INT n -> Int64.to_int n | _ -> failwith "Invalid integer"
let txt s = D.TEXT s
let int n = D.INT (Int64.of_int n)

let run t f =
  let result =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        try Ok (Sqlite3_eio.run t.sql f)
        with e -> Error (e, Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok x -> x
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt

let transaction db f =
  check (Sqlite3.exec db "BEGIN IMMEDIATE");
  match f () with
  | x ->
      check (Sqlite3.exec db "COMMIT");
      x
  | exception e ->
      ignore (Sqlite3.exec db "ROLLBACK");
      raise e

let revision_db db =
  match rows db "SELECT value FROM meta WHERE key='revision'" [] with
  | [ r ] -> num r.(0)
  | _ -> failwith "Missing revision"

let bump db = execute db "UPDATE meta SET value=value+1 WHERE key='revision'" []

let init sql =
  let t = { sql; lock = Eio.Mutex.create () } in
  run t (fun db ->
      check (Sqlite3.exec db "PRAGMA journal_mode=WAL; PRAGMA foreign_keys=ON;");
      transaction db (fun () ->
          let version =
            match rows db "PRAGMA user_version" [] with
            | [ r ] -> num r.(0)
            | _ -> assert false
          in
          if version > 6 then
            failwith "Siftbot database is newer than this binary";
          if version = 0 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE meta(key TEXT PRIMARY KEY, value INTEGER NOT NULL);
INSERT INTO meta VALUES('revision',0);
CREATE TABLE messages(key TEXT PRIMARY KEY, service TEXT NOT NULL,
 account TEXT NOT NULL, id TEXT NOT NULL, thread TEXT NOT NULL,
 date TEXT NOT NULL, revision TEXT NOT NULL, payload TEXT NOT NULL,
 UNIQUE(service,account,id));
CREATE VIRTUAL TABLE search USING fts5(key UNINDEXED, subject, sender, body);
CREATE TABLE checkpoints(job TEXT PRIMARY KEY, cursor TEXT NOT NULL);
CREATE TABLE areas(id TEXT PRIMARY KEY, name TEXT NOT NULL,
 definition TEXT NOT NULL, exclusions TEXT NOT NULL, parent TEXT,
 status TEXT NOT NULL, revision INTEGER NOT NULL);
CREATE TABLE assignments(target TEXT NOT NULL, conversation INTEGER NOT NULL,
 dimension TEXT NOT NULL, value TEXT NOT NULL, origin TEXT NOT NULL,
 version TEXT NOT NULL, source_revision TEXT NOT NULL,
 explanation TEXT NOT NULL, evidence TEXT NOT NULL, score REAL,
 PRIMARY KEY(target,conversation,dimension,value,origin));
CREATE TABLE derived(source TEXT NOT NULL, revision TEXT NOT NULL,
 kind TEXT NOT NULL, value TEXT NOT NULL, PRIMARY KEY(source,kind));
CREATE TABLE decisions(id INTEGER PRIMARY KEY, explanation TEXT NOT NULL,
 before_state TEXT NOT NULL, undone INTEGER NOT NULL DEFAULT 0);
CREATE TABLE history(revision INTEGER PRIMARY KEY, explanation TEXT NOT NULL);
PRAGMA user_version=1;
|});
          if version < 2 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE audit(id INTEGER PRIMARY KEY, model TEXT NOT NULL,
 prompt_version TEXT NOT NULL, corpus_revision INTEGER NOT NULL,
 question TEXT NOT NULL, answer TEXT NOT NULL, outcome TEXT NOT NULL,
 tools TEXT NOT NULL);
PRAGMA user_version=2;
|});
          if version < 3 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE vector_indexes(name TEXT PRIMARY KEY, model TEXT NOT NULL,
 dimensions INTEGER NOT NULL, normalization TEXT NOT NULL);
PRAGMA user_version=3;
|});
          if version < 4 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE evaluations(id TEXT PRIMARY KEY, corpus_revision INTEGER NOT NULL,
 payload TEXT NOT NULL);
PRAGMA user_version=4;
|});
          if version < 5 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE task_receipts(key TEXT PRIMARY KEY, source TEXT NOT NULL,
 title TEXT NOT NULL, status TEXT NOT NULL, receipt TEXT NOT NULL);
CREATE TABLE snoozes(target TEXT PRIMARY KEY, until_date TEXT NOT NULL,
 source_revision TEXT NOT NULL, explanation TEXT NOT NULL);
PRAGMA user_version=5;
|});
          if version < 6 then
            check
              (Sqlite3.exec db
                 {|
CREATE TABLE hydration(source TEXT PRIMARY KEY, service TEXT NOT NULL,
 account TEXT NOT NULL, position INTEGER NOT NULL);
PRAGMA user_version=6;
|})));
  t

let open_db ~sw path =
  Sqlite_vec.register ();
  init (Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path)

let memory ~sw () =
  Sqlite_vec.register ();
  init (Sqlite3_eio.open_memory ~sw ())

let revision t = run t revision_db
let encode = Jmap_eio.Codec.encode_exn M.message_jsont
let decode = Jmap_eio.Codec.decode_exn M.message_jsont

let get_db db source =
  match
    rows db "SELECT payload FROM messages WHERE key=?" [ txt (M.key source) ]
  with
  | [ r ] -> Some (decode (str r.(0)))
  | _ -> None

let get t s = run t (fun db -> get_db db s)

let messages_db db =
  rows db "SELECT payload FROM messages ORDER BY date,key" []
  |> List.map (fun r -> decode (str r.(0)))

let messages t = run t messages_db

let checkpoint_db db job =
  match rows db "SELECT cursor FROM checkpoints WHERE job=?" [ txt job ] with
  | [ r ] -> Some (str r.(0))
  | _ -> None

let checkpoint t job = run t (fun db -> checkpoint_db db job)

let vector_name name =
  if
    (not (String.starts_with ~prefix:"vec_" name))
    || not
         (String.for_all
            (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
            name)
  then failwith "Invalid vector index name";
  name

let invalidate db key =
  List.iter
    (fun row ->
      let table = vector_name (str row.(0)) in
      execute db ("DELETE FROM " ^ table ^ " WHERE source=?") [ txt key ])
    (rows db "SELECT name FROM vector_indexes" []);
  execute db "DELETE FROM derived WHERE source=?" [ txt key ];
  execute db
    "DELETE FROM assignments WHERE origin<>'admin' AND (target=? OR EXISTS \
     (SELECT 1 FROM json_each(evidence) WHERE value=?))"
    [ txt key; txt key ]

let invalidate_thread db (m : M.message) =
  let source = { m.source with id = m.thread } in
  invalidate db (M.key source)

let put db (m : M.message) =
  let key = M.key m.source in
  (match get_db db m.source with
  | Some old when old.revision <> m.revision ->
      invalidate db key;
      invalidate_thread db old;
      invalidate_thread db m
  | None -> invalidate_thread db m
  | _ -> ());
  execute db "INSERT OR REPLACE INTO messages VALUES(?,?,?,?,?,?,?,?)"
    [
      txt key;
      txt m.source.service;
      txt m.source.account;
      txt m.source.id;
      txt m.thread;
      txt m.date;
      txt m.revision;
      txt (encode m);
    ];
  execute db "DELETE FROM search WHERE key=?" [ txt key ];
  execute db "INSERT INTO search VALUES(?,?,?,?)"
    [ txt key; txt m.subject; txt m.sender; txt m.text ]

let delete db source =
  let key = M.key source in
  Option.iter (invalidate_thread db) (get_db db source);
  invalidate db key;
  execute db "DELETE FROM messages WHERE key=?" [ txt key ];
  execute db "DELETE FROM search WHERE key=?" [ txt key ]

let commit_batch ?(hydrate = []) ?hydrated t ~job ~expected ~cursor ms deleted =
  run t (fun db ->
      transaction db (fun () ->
          if checkpoint_db db job <> expected then raise Conflict;
          List.iter (put db) ms;
          List.iter (delete db) deleted;
          List.iter
            (fun (s : M.source) ->
              execute db "INSERT OR IGNORE INTO hydration VALUES(?,?,?,0)"
                [ txt (M.key s); txt s.service; txt s.account ])
            hydrate;
          (match hydrated with
          | None -> ()
          | Some (s, None) ->
              execute db "DELETE FROM hydration WHERE source=?"
                [ txt (M.key s) ]
          | Some (s, Some position) ->
              execute db "UPDATE hydration SET position=? WHERE source=?"
                [ int position; txt (M.key s) ]);
          execute db "INSERT OR REPLACE INTO checkpoints VALUES(?,?)"
            [ txt job; txt cursor ];
          bump db))

let search_db db query =
  if query = "" then
    rows db "SELECT payload FROM messages ORDER BY date DESC,key LIMIT 100" []
  else
    rows db
      "SELECT m.payload FROM search JOIN messages m ON search.key=m.key WHERE \
       search MATCH ? ORDER BY rank LIMIT 100"
      [ txt query ]

let search t query =
  run t (fun db -> List.map (fun r -> decode (str r.(0))) (search_db db query))

let area_of_row r : M.area =
  {
    id = str r.(0);
    name = str r.(1);
    definition = str r.(2);
    exclusions = str r.(3);
    parent = (if r.(4) = D.NULL then None else Some (str r.(4)));
    status = str r.(5);
    revision = num r.(6);
  }

let areas_db db =
  List.map area_of_row (rows db "SELECT * FROM areas ORDER BY id" [])

let source_of_key s = M.source_of_sexp (Sexplib.Sexp.of_string s)

let dim = function
  | "area" -> M.Area
  | "triage" -> Triage
  | "delivery" -> Delivery
  | "attention" -> Attention
  | _ -> failwith "dimension"

let origin = function
  | "admin" -> M.Admin
  | "rule" -> Rule
  | "model" -> Model
  | "server" -> Server
  | _ -> failwith "origin"

let origin_name = function
  | M.Admin -> "admin"
  | Rule -> "rule"
  | Model -> "model"
  | Server -> "server"

let string_list = Jsont.list Jsont.string

let assignments_db db =
  rows db "SELECT * FROM assignments ORDER BY target,dimension,value,origin" []
  |> List.map (fun r ->
         M.
           {
             target = source_of_key (str r.(0));
             conversation = num r.(1) = 1;
             dimension = dim (str r.(2));
             value = str r.(3);
             origin = origin (str r.(4));
             version = str r.(5);
             source_revision = str r.(6);
             explanation = str r.(7);
             evidence =
               Jmap_eio.Codec.decode_exn string_list (str r.(8))
               |> List.map source_of_key;
             score = (match r.(9) with D.FLOAT f -> Some f | _ -> None);
           })

let visible db assignments =
  let live (a : M.assignment) =
    if a.conversation then
      rows db
        "SELECT 1 FROM messages WHERE service=? AND account=? AND thread=? \
         LIMIT 1"
        [ txt a.target.service; txt a.target.account; txt a.target.id ]
      <> []
    else get_db db a.target <> None
  in
  List.filter
    (fun (a : M.assignment) ->
      live a
      && (a.origin = Admin
         || not
              (List.exists
                 (fun (b : M.assignment) ->
                   b.origin = Admin && b.target = a.target
                   && b.conversation = a.conversation
                   && b.dimension = a.dimension)
                 assignments)))
    assignments

let snapshot t ~query =
  run t (fun db ->
      {
        M.revision = revision_db db;
        messages = List.map (fun r -> decode (str r.(0))) (search_db db query);
        areas = areas_db db;
        assignments = visible db (assignments_db db);
        discussion =
          (match
             rows db
               "SELECT answer FROM audit WHERE prompt_version LIKE \
                'siftbot/discussion/%' ORDER BY id DESC LIMIT 1"
               []
           with
          | [ r ] -> str r.(0)
          | _ -> "");
        jobs =
          rows db "SELECT job,cursor FROM checkpoints ORDER BY job" []
          |> List.map (fun r -> str r.(0) ^ ": " ^ str r.(1));
      })

let save_area db (a : M.area) =
  execute db "INSERT OR REPLACE INTO areas VALUES(?,?,?,?,?,?,?)"
    [
      txt a.id;
      txt a.name;
      txt a.definition;
      txt a.exclusions;
      Option.fold ~none:D.NULL ~some:txt a.parent;
      txt a.status;
      int a.revision;
    ]

let save_assignment db (a : M.assignment) =
  if not (M.valid_value a.dimension a.value) then invalid_arg "Invalid label";
  (match a.score with
  | Some f when not (Float.is_finite f) -> invalid_arg "Non-finite score"
  | _ -> ());
  execute db "INSERT OR REPLACE INTO assignments VALUES(?,?,?,?,?,?,?,?,?,?)"
    [
      txt (M.key a.target);
      int (if a.conversation then 1 else 0);
      txt (M.dimension a.dimension);
      txt a.value;
      txt (origin_name a.origin);
      txt a.version;
      txt a.source_revision;
      txt a.explanation;
      txt (Jmap_eio.Codec.encode_exn string_list (List.map M.key a.evidence));
      Option.fold ~none:D.NULL ~some:(fun f -> D.FLOAT f) a.score;
    ]

let propose t areas assignments =
  run t (fun db ->
      transaction db (fun () ->
          List.iter
            (fun (a : M.area) ->
              if a.status <> "proposed" then
                invalid_arg "Proposal must be unapproved";
              if
                not
                  (List.exists (fun (b : M.area) -> b.id = a.id) (areas_db db))
              then save_area db a)
            areas;
          let groups =
            List.map
              (fun (a : M.assignment) ->
                ( M.key a.target,
                  a.conversation,
                  M.dimension a.dimension,
                  origin_name a.origin ))
              assignments
            |> List.sort_uniq compare
          in
          List.iter
            (fun (target, conversation, dimension, origin) ->
              if origin = "admin" then
                invalid_arg "Predictions cannot be human decisions";
              execute db
                "DELETE FROM assignments WHERE target=? AND conversation=? AND \
                 dimension=? AND origin=?"
                [
                  txt target;
                  int (if conversation then 1 else 0);
                  txt dimension;
                  txt origin;
                ])
            groups;
          List.iter
            (fun (a : M.assignment) ->
              if a.origin = Admin then
                invalid_arg "Predictions cannot be human decisions";
              if
                a.evidence = []
                || List.exists (fun s -> get_db db s = None) a.evidence
              then invalid_arg "Prediction evidence is missing or deleted";
              (if not a.conversation then
                 match get_db db a.target with
                 | Some m when m.revision = a.source_revision -> ()
                 | _ -> raise Conflict);
              if
                a.dimension = Area
                && not
                     (List.exists
                        (fun (b : M.area) ->
                          b.id = a.value && b.status = "approved")
                        (areas_db db))
              then invalid_arg "Area is not approved";
              save_assignment db a)
            assignments;
          bump db))

(* History is a JSON array of typed SQL rows. It is private to schema v1. *)
let row_json row =
  Jsont.Json.list
    (Array.to_list row
    |> List.map (function
         | D.TEXT s -> Jsont.Json.string s
         | D.INT n -> Jsont.Json.int (Int64.to_int n)
         | D.FLOAT f -> Jsont.Json.number f
         | D.NULL -> Jsont.Json.null ()
         | _ -> failwith "Unsupported history value"))

let before_state db =
  Jsont.Json.list
    [
      Jsont.Json.list (List.map row_json (rows db "SELECT * FROM areas" []));
      Jsont.Json.list
        (List.map row_json
           (rows db "SELECT * FROM assignments WHERE origin='admin'" []));
    ]
  |> Jmap_eio.Codec.encode_exn Jsont.json

let restore db state =
  let array = function Jsont.Array (xs, _) -> xs | _ -> failwith "history" in
  let data = function
    | Jsont.String (s, _) -> txt s
    | Jsont.Number (f, _) ->
        if Float.is_integer f then int (int_of_float f) else D.FLOAT f
    | Jsont.Null _ -> D.NULL
    | _ -> failwith "history value"
  in
  match array (Jmap_eio.Codec.decode_exn Jsont.json state) with
  | [ areas; assignments ] ->
      execute db "DELETE FROM areas" [];
      execute db "DELETE FROM assignments WHERE origin='admin'" [];
      List.iter
        (fun r ->
          execute db "INSERT INTO areas VALUES(?,?,?,?,?,?,?)"
            (List.map data (array r)))
        (array areas);
      List.iter
        (fun r ->
          execute db "INSERT INTO assignments VALUES(?,?,?,?,?,?,?,?,?,?)"
            (List.map data (array r)))
        (array assignments)
  | _ -> failwith "history state"

let edit t ~revision edit ~explanation =
  run t (fun db ->
      transaction db (fun () ->
          if revision_db db <> revision then raise Conflict;
          if String.trim explanation = "" then
            invalid_arg "Explain the decision";
          let before = before_state db in
          let area id f =
            match
              List.find_opt (fun (a : M.area) -> a.id = id) (areas_db db)
            with
            | None -> invalid_arg "Unknown area"
            | Some a -> save_area db (f { a with revision = a.revision + 1 })
          in
          (match edit with
          | M.Approve id -> area id (fun a -> { a with status = "approved" })
          | Archive id -> area id (fun a -> { a with status = "archived" })
          | Merge (from_id, into_id) ->
              if from_id = into_id then invalid_arg "Choose two different areas";
              if
                not
                  (List.exists
                     (fun (a : M.area) ->
                       a.id = into_id && a.status = "approved")
                     (areas_db db))
              then invalid_arg "Merge target must be approved";
              area from_id (fun a -> { a with status = "archived" });
              List.iter
                (fun (a : M.assignment) ->
                  if a.dimension = Area && a.value = from_id && a.origin = Admin
                  then
                    save_assignment db { a with value = into_id; explanation })
                (assignments_db db);
              execute db
                "DELETE FROM assignments WHERE dimension='area' AND value=?"
                [ txt from_id ]
          | Rename (id, name) ->
              if String.trim name = "" then invalid_arg "Empty area name";
              area id (fun a -> { a with name })
          | Define (id, definition, exclusions) ->
              area id (fun a -> { a with definition; exclusions })
          | Assign a ->
              if
                a.dimension = Area
                && not
                     (List.exists
                        (fun (b : M.area) ->
                          b.id = a.value && b.status = "approved")
                        (areas_db db))
              then invalid_arg "Area is not approved";
              (if a.conversation then (
                 if
                   rows db
                     "SELECT 1 FROM messages WHERE service=? AND account=? AND \
                      thread=?"
                     [
                       txt a.target.service;
                       txt a.target.account;
                       txt a.target.id;
                     ]
                   = []
                 then invalid_arg "Unknown conversation")
               else
                 match get_db db a.target with
                 | Some m when m.revision = a.source_revision -> ()
                 | _ -> raise Conflict);
              if a.dimension <> Area then
                execute db
                  "DELETE FROM assignments WHERE target=? AND conversation=? \
                   AND dimension=? AND origin='admin'"
                  [
                    txt (M.key a.target);
                    int (if a.conversation then 1 else 0);
                    txt (M.dimension a.dimension);
                  ];
              save_assignment db
                { a with origin = Admin; score = None; explanation }
          | Undo -> (
              match
                rows db
                  "SELECT id,before_state FROM decisions WHERE undone=0 ORDER \
                   BY id DESC LIMIT 1"
                  []
              with
              | [ r ] ->
                  restore db (str r.(1));
                  execute db "UPDATE decisions SET undone=1 WHERE id=?"
                    [ r.(0) ]
              | _ -> invalid_arg "Nothing to undo"));
          if edit <> M.Undo then
            execute db
              "INSERT INTO decisions(id,explanation,before_state) VALUES(?,?,?)"
              [ int (revision + 1); txt explanation; txt before ];
          execute db "DELETE FROM derived" [];
          execute db "DELETE FROM assignments WHERE origin<>'admin'" [];
          bump db;
          execute db "INSERT INTO history VALUES(?,?)"
            [ int (revision + 1); txt explanation ]))

let cache t ~source ~revision ~kind ~value =
  run t (fun db ->
      transaction db (fun () ->
          match get_db db source with
          | Some m when m.revision = revision ->
              execute db "INSERT OR REPLACE INTO derived VALUES(?,?,?,?)"
                [ txt (M.key source); txt revision; txt kind; txt value ]
          | _ -> raise Conflict))

let cached t ~source ~kind =
  run t (fun db ->
      match
        rows db
          "SELECT d.value FROM derived d JOIN messages m ON d.source=m.key AND \
           d.revision=m.revision WHERE d.source=? AND d.kind=?"
          [ txt (M.key source); txt kind ]
      with
      | [ r ] -> Some (str r.(0))
      | _ -> None)

let audit t ~model ~prompt_version ~corpus_revision ~question ~answer ~outcome
    ~tools =
  run t (fun db ->
      transaction db (fun () ->
          execute db
            "INSERT INTO \
             audit(model,prompt_version,corpus_revision,question,answer,outcome,tools) \
             VALUES(?,?,?,?,?,?,?)"
            [
              txt model;
              txt prompt_version;
              int corpus_revision;
              txt question;
              txt answer;
              txt outcome;
              txt (Jmap_eio.Codec.encode_exn string_list tools);
            ];
          bump db))

let vector_index t ~model ~dimensions ~normalization =
  if dimensions < 1 || dimensions > 4096 || model = "" || normalization = ""
  then invalid_arg "Invalid embedding identity or dimensions";
  let identity =
    Jmap_eio.Codec.encode_exn string_list
      [ model; string_of_int dimensions; normalization ]
  in
  let name = "vec_" ^ Digestif.SHA256.(to_hex (digest_string identity)) in
  run t (fun db ->
      transaction db (fun () ->
          execute db "INSERT OR IGNORE INTO vector_indexes VALUES(?,?,?,?)"
            [ txt name; txt model; int dimensions; txt normalization ];
          execute db
            (Printf.sprintf
               "CREATE VIRTUAL TABLE IF NOT EXISTS %s USING vec0(source TEXT \
                PRIMARY KEY, embedding float[%d], +revision TEXT, +input_hash \
                TEXT)"
               name dimensions)
            []));
  name

let vector_current t ~index ~source ~revision ~input_hash =
  run t (fun db ->
      rows db
        ("SELECT 1 FROM " ^ vector_name index
       ^ " WHERE source=? AND revision=? AND input_hash=?")
        [ txt (M.key source); txt revision; txt input_hash ]
      <> [])

let vector_json vector =
  if List.exists (fun f -> not (Float.is_finite f)) vector then
    invalid_arg "Embedding contains non-finite values";
  Jmap_eio.Codec.encode_exn (Jsont.list Jsont.number) vector

let vector_put t ~index ~source ~revision ~input_hash vector =
  let value = vector_json vector in
  run t (fun db ->
      transaction db (fun () ->
          match get_db db source with
          | Some m when m.revision = revision ->
              execute db
                ("DELETE FROM " ^ vector_name index ^ " WHERE source=?")
                [ txt (M.key source) ];
              execute db
                ("INSERT INTO " ^ vector_name index
               ^ "(source,embedding,revision,input_hash) VALUES(?,?,?,?)")
                [ txt (M.key source); txt value; txt revision; txt input_hash ]
          | _ -> raise Conflict))

let vector_search t ~index ~limit vector =
  if limit < 1 || limit > 100 then invalid_arg "Vector result limit is 1 to 100";
  let vector = vector_json vector in
  run t (fun db ->
      rows db
        ("WITH nearest AS MATERIALIZED (SELECT source,revision,distance FROM "
       ^ vector_name index
       ^ " WHERE embedding MATCH ? AND k=?) SELECT m.payload,v.distance FROM \
          nearest v JOIN messages m ON m.key=v.source AND \
          m.revision=v.revision ORDER BY v.distance,m.key")
        [ txt vector; int limit ]
      |> List.map (fun r ->
             ( decode (str r.(0)),
               match r.(1) with
               | D.FLOAT f -> f
               | _ -> failwith "Invalid vector distance" )))

let freeze t ~id ~revision ~payload =
  run t (fun db ->
      transaction db (fun () ->
          if revision_db db <> revision then raise Conflict;
          match
            rows db "SELECT payload FROM evaluations WHERE id=?" [ txt id ]
          with
          | [ row ] when str row.(0) = payload -> ()
          | [] ->
              execute db "INSERT INTO evaluations VALUES(?,?,?)"
                [ txt id; int revision; txt payload ]
          | _ -> raise Conflict))

let evaluations t =
  run t (fun db ->
      rows db "SELECT id,payload FROM evaluations ORDER BY id" []
      |> List.map (fun r -> (str r.(0), str r.(1))))

let reserve_task t ~key ~revision ~source ~source_revision ~title =
  run t (fun db ->
      transaction db (fun () ->
          if revision_db db <> revision then raise Conflict;
          (match get_db db source with
          | Some m when m.revision = source_revision -> ()
          | _ -> raise Conflict);
          match
            rows db "SELECT source,title FROM task_receipts WHERE key=?"
              [ txt key ]
          with
          | [ row ] -> (source_of_key (str row.(0)), str row.(1))
          | [] ->
              execute db "INSERT INTO task_receipts VALUES(?,?,?,'pending','')"
                [ txt key; txt (M.key source); txt title ];
              bump db;
              (source, title)
          | _ -> assert false))

let task_receipt t ~key ~status ~receipt =
  run t (fun db ->
      transaction db (fun () ->
          execute db "UPDATE task_receipts SET status=?,receipt=? WHERE key=?"
            [ txt status; txt receipt; txt key ];
          bump db))

let snooze t ~revision ~target ~until_date ~explanation =
  let until_date =
    match Ptime.of_rfc3339 until_date with
    | Ok (t, _, _) -> Ptime.to_rfc3339 ~tz_offset_s:0 t
    | Error _ -> invalid_arg "Use an RFC3339 snooze deadline"
  in
  run t (fun db ->
      transaction db (fun () ->
          if revision_db db <> revision then raise Conflict;
          let m =
            match get_db db target with Some m -> m | None -> raise Conflict
          in
          execute db "INSERT OR REPLACE INTO snoozes VALUES(?,?,?,?)"
            [
              txt (M.key target);
              txt until_date;
              txt m.revision;
              txt explanation;
            ];
          bump db))

let snoozed t ~now target =
  run t (fun db ->
      match
        rows db
          "SELECT 1 FROM snoozes s JOIN messages m ON m.key=s.target AND \
           m.revision=s.source_revision WHERE s.target=? AND s.until_date>?"
          [ txt (M.key target); txt now ]
      with
      | [] -> false
      | _ -> true)

let hydration t ~service ~account =
  run t (fun db ->
      match
        rows db
          "SELECT source,position FROM hydration WHERE service=? AND account=? \
           ORDER BY source LIMIT 1"
          [ txt service; txt account ]
      with
      | [ row ] -> Some (source_of_key (str row.(0)), num row.(1))
      | _ -> None)

let checkpoints t =
  run t (fun db ->
      rows db "SELECT job,cursor FROM checkpoints ORDER BY job" []
      |> List.map (fun r -> (str r.(0), str r.(1))))
