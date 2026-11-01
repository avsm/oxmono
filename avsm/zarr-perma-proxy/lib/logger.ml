module D = Database
module S = Sqlite3.Data

type transfer = {
  key : string;
  path : string;
  bytes : int;
  total : int option;
  age : float;
}

type t = { db : D.t; now : unit -> float; mutable writes : int }

let schema =
  {|
CREATE TABLE requests (
 id INTEGER PRIMARY KEY AUTOINCREMENT, time REAL NOT NULL,
 method TEXT NOT NULL, path TEXT NOT NULL, object_key TEXT,
 range_request INTEGER NOT NULL, status INTEGER NOT NULL, cache TEXT NOT NULL,
 served_bytes INTEGER NOT NULL, duration_us INTEGER NOT NULL);
CREATE TABLE downloads (
 id INTEGER PRIMARY KEY AUTOINCREMENT, time REAL NOT NULL, object_key TEXT NOT NULL,
 status INTEGER, fetched_bytes INTEGER NOT NULL, stored_bytes INTEGER NOT NULL,
 duration_us INTEGER NOT NULL, outcome TEXT NOT NULL);
CREATE TABLE request_hours (
 hour INTEGER NOT NULL, method TEXT NOT NULL, range_request INTEGER NOT NULL,
 status INTEGER NOT NULL, cache TEXT NOT NULL, requests INTEGER NOT NULL,
 served_bytes INTEGER NOT NULL, duration_us INTEGER NOT NULL,
 PRIMARY KEY(hour,method,range_request,status,cache));
CREATE TABLE download_hours (
 hour INTEGER NOT NULL, outcome TEXT NOT NULL, downloads INTEGER NOT NULL,
 fetched_bytes INTEGER NOT NULL, stored_bytes INTEGER NOT NULL,
 duration_us INTEGER NOT NULL, PRIMARY KEY(hour,outcome));
CREATE INDEX request_time ON requests(time);
CREATE INDEX download_time ON downloads(time);
|}

let live_schema =
  {|
CREATE TABLE IF NOT EXISTS runtime (
 id INTEGER PRIMARY KEY CHECK(id=1), session TEXT NOT NULL, heartbeat REAL NOT NULL,
 running INTEGER NOT NULL, connections INTEGER NOT NULL, queued INTEGER NOT NULL,
 transfers TEXT NOT NULL);
|}

let prune db now =
  List.iter
    (fun table ->
      D.execute db
        ("DELETE FROM " ^ table ^ " WHERE time < ?")
        [ S.FLOAT (now -. (30. *. 86400.)) ];
      D.exec db
        ("DELETE FROM " ^ table ^ " WHERE id <= COALESCE((SELECT max(id) FROM "
       ^ table ^ "),0)-1000000"))
    [ "requests"; "downloads" ]

let open_ ~sw ~clock dir =
  let db = D.open_ ~sw Eio.Path.(dir / "requests.sqlite3") schema in
  let now () = Eio.Time.now clock in
  let initial_time = now () in
  D.run db (fun db ->
      D.exec db live_schema;
      prune db initial_time);
  { db; now; writes = 0 }

let nullable f = function None -> S.NULL | Some x -> f x
let int n = S.INT (Int64.of_int n)
let hour now = S.INT (Int64.of_float (floor (now /. 3600.) *. 3600.))
let clipped s = if String.length s <= 4096 then s else String.sub s 0 4096

let record t f =
  let now = t.now () in
  t.writes <- t.writes + 1;
  let cleanup = t.writes mod 1000 = 0 in
  D.run t.db (fun db ->
      D.transaction db (fun () ->
          f db now;
          if cleanup then prune db now))

let request t ~meth ~path ~object_key ~range ~status ~cache ~bytes ~duration_us
    =
  let path =
    match String.index_opt path '?' with
    | None -> path
    | Some i -> String.sub path 0 i
  in
  let cache = Option.value cache ~default:"BYPASS" in
  let meth =
    match meth with "GET" | "HEAD" | "OPTIONS" -> meth | _ -> "OTHER"
  in
  record t (fun db now ->
      let dimensions =
        [ S.TEXT meth; int (if range then 1 else 0); int status; S.TEXT cache ]
      in
      D.execute db
        {|
INSERT INTO requests(time,method,path,object_key,range_request,status,cache,served_bytes,duration_us)
VALUES(?,?,?,?,?,?,?,?,?)|}
        ([
           S.FLOAT now;
           S.TEXT meth;
           S.TEXT (clipped path);
           nullable (fun x -> S.TEXT x) object_key;
         ]
        @ [
            int (if range then 1 else 0);
            int status;
            S.TEXT cache;
            int bytes;
            int duration_us;
          ]);
      D.execute db
        {|
INSERT INTO request_hours VALUES(?,?,?,?,?,1,?,?)
ON CONFLICT(hour,method,range_request,status,cache) DO UPDATE SET
requests=requests+1,served_bytes=served_bytes+excluded.served_bytes,
duration_us=duration_us+excluded.duration_us|}
        ([ hour now ] @ dimensions @ [ int bytes; int duration_us ]))

let download t ~object_key ~status ~bytes ~stored_bytes ~duration_us ~outcome =
  record t (fun db now ->
      D.execute db
        {|
INSERT INTO downloads(time,object_key,status,fetched_bytes,stored_bytes,duration_us,outcome)
VALUES(?,?,?,?,?,?,?)|}
        [
          S.FLOAT now;
          S.TEXT object_key;
          nullable int status;
          int bytes;
          int stored_bytes;
          int duration_us;
          S.TEXT outcome;
        ];
      D.execute db
        {|
INSERT INTO download_hours VALUES(?,?,1,?,?,?)
ON CONFLICT(hour,outcome) DO UPDATE SET downloads=downloads+1,
fetched_bytes=fetched_bytes+excluded.fetched_bytes,
stored_bytes=stored_bytes+excluded.stored_bytes,
duration_us=duration_us+excluded.duration_us|}
        [
          hour now; S.TEXT outcome; int bytes; int stored_bytes; int duration_us;
        ])

let value = function
  | S.NULL | S.NONE -> Jsont.Json.null ()
  | S.INT n -> Jsont.Json.int64 n
  | S.FLOAT f -> Jsont.Json.number f
  | S.TEXT s -> Jsont.Json.string s
  | S.BLOB _ -> invalid_arg "Telemetry contains a blob"

let json_rows db sql params =
  D.statement db sql params (fun stmt ->
      let names =
        Array.init (Sqlite3.column_count stmt) (Sqlite3.column_name stmt)
      in
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let row = Sqlite3.row_data stmt in
            let fields =
              Array.to_list
                (Array.mapi
                   (fun i v -> (Jsont.Json.name names.(i), value v))
                   row)
            in
            loop (Jsont.Json.object' fields :: acc)
        | Sqlite3.Rc.DONE -> Jsont.Json.list (List.rev acc)
        | rc ->
            Sqlite3.Rc.check rc;
            assert false
      in
      loop [])

let encode json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok s -> s
  | Error e -> failwith e

let readonly ~sw dir f =
  let db =
    D.open_ ~sw ~readonly:true Eio.Path.(dir / "requests.sqlite3") schema
  in
  D.run db f

let summary ~sw dir =
  readonly ~sw dir (fun db ->
      Jsont.Json.object'
        [
          (Jsont.Json.name "schema_version", Jsont.Json.int 1);
          ( Jsont.Json.name "totals",
            json_rows db
              {|
SELECT r.*, d.*,
 CASE WHEN r.served_bytes > 0 THEN 1.0*d.fetched_bytes/r.served_bytes END AS fetch_to_serve_ratio
FROM (
 SELECT coalesce(sum(requests),0) AS requests,
 coalesce(sum(CASE WHEN cache IN ('HIT','MISS','SHARED') THEN requests ELSE 0 END),0) AS cache_requests,
 coalesce(sum(CASE WHEN cache='HIT' THEN requests ELSE 0 END),0) AS hits,
 coalesce(sum(CASE WHEN cache='MISS' THEN requests ELSE 0 END),0) AS misses,
 coalesce(sum(CASE WHEN cache='SHARED' THEN requests ELSE 0 END),0) AS shared,
 1.0*sum(CASE WHEN cache='HIT' THEN requests ELSE 0 END) /
 nullif(sum(CASE WHEN cache IN ('HIT','MISS','SHARED') THEN requests ELSE 0 END),0) AS hit_rate,
 1.0*sum(CASE WHEN cache IN ('HIT','SHARED') THEN requests ELSE 0 END) /
 nullif(sum(CASE WHEN cache IN ('HIT','MISS','SHARED') THEN requests ELSE 0 END),0) AS avoided_fetch_rate,
 coalesce(sum(served_bytes),0) AS served_bytes FROM request_hours
) r CROSS JOIN (
 SELECT coalesce(sum(downloads),0) AS downloads,
 coalesce(sum(fetched_bytes),0) AS fetched_bytes,
 coalesce(sum(stored_bytes),0) AS stored_bytes FROM download_hours
) d|}
              [] );
          ( Jsont.Json.name "request_hours",
            json_rows db "SELECT * FROM request_hours ORDER BY hour" [] );
          ( Jsont.Json.name "download_hours",
            json_rows db "SELECT * FROM download_hours ORDER BY hour" [] );
        ]
      |> encode)

let export ~sw dir ~table ~after ~limit =
  if limit < 1 || limit > 10000 || after < 0L then
    invalid_arg "Invalid telemetry page";
  let table =
    match table with `Requests -> "requests" | `Downloads -> "downloads"
  in
  readonly ~sw dir (fun db ->
      json_rows db
        ("SELECT * FROM " ^ table ^ " WHERE id > ? ORDER BY id LIMIT ?")
        [ S.INT after; int limit ]
      |> encode)

let heartbeat t ~session ~running ~connections ~queued ~transfers =
  let transfers =
    List.map
      (fun (p : transfer) ->
        let path =
          match String.index_opt p.path '?' with
          | None -> p.path
          | Some n -> String.sub p.path 0 n
        in
        Jsont.Json.object'
          [
            (Jsont.Json.name "key", Jsont.Json.string p.key);
            (Jsont.Json.name "path", Jsont.Json.string (clipped path));
            (Jsont.Json.name "bytes", Jsont.Json.int p.bytes);
            ( Jsont.Json.name "total",
              Option.fold ~none:(Jsont.Json.null ()) ~some:Jsont.Json.int
                p.total );
            (Jsont.Json.name "age", Jsont.Json.number p.age);
          ])
      transfers
    |> Jsont.Json.list |> encode
  in
  let now = t.now () in
  D.run t.db (fun db ->
      D.execute db
        {|
INSERT INTO runtime VALUES(1,?,?,?,?,?,?)
ON CONFLICT(id) DO UPDATE SET session=excluded.session,heartbeat=excluded.heartbeat,
running=excluded.running,connections=excluded.connections,queued=excluded.queued,
transfers=excluded.transfers|}
        [
          S.TEXT session;
          S.FLOAT now;
          int (if running then 1 else 0);
          int connections;
          int queued;
          S.TEXT transfers;
        ])

let snapshot ~sw ~clock ~mono_clock dir =
  let monotonic =
    Eio.Time.Mono.now mono_clock |> Mtime.to_uint64_ns |> Int64.to_float
  in
  let now = Eio.Time.now clock in
  readonly ~sw dir (fun db ->
      D.exec db "BEGIN";
      Fun.protect
        ~finally:(fun () -> D.exec db "ROLLBACK")
        (fun () ->
          let runtime =
            if
              D.integer db
                "SELECT count(*) FROM sqlite_master WHERE type='table' AND \
                 name='runtime'"
                []
              = 0L
            then Jsont.Json.list []
            else json_rows db "SELECT * FROM runtime" []
          in
          Jsont.Json.object'
            [
              (Jsont.Json.name "time", Jsont.Json.number now);
              (Jsont.Json.name "monotonic", Jsont.Json.number (monotonic /. 1e9));
              (Jsont.Json.name "runtime", runtime);
              ( Jsont.Json.name "totals",
                json_rows db
                  {|
SELECT coalesce(sum(requests),0) AS requests,
 coalesce(sum(CASE WHEN cache='HIT' THEN requests ELSE 0 END),0) AS hits,
 coalesce(sum(CASE WHEN cache='MISS' THEN requests ELSE 0 END),0) AS misses,
 coalesce(sum(CASE WHEN cache='SHARED' THEN requests ELSE 0 END),0) AS shared,
 coalesce(sum(served_bytes),0) AS served_bytes,
 coalesce(sum(CASE WHEN status>=400 THEN requests ELSE 0 END),0) AS errors,
 coalesce(sum(duration_us),0) AS duration_us
FROM request_hours|}
                  [] );
              ( Jsont.Json.name "traffic",
                json_rows db
                  {|
SELECT coalesce(sum(fetched_bytes),0) AS fetched_bytes,
coalesce(sum(stored_bytes),0) AS stored_bytes FROM download_hours|}
                  [] );
              ( Jsont.Json.name "requests",
                json_rows db "SELECT * FROM requests ORDER BY id DESC LIMIT 200"
                  [] );
              ( Jsont.Json.name "downloads",
                json_rows db
                  "SELECT * FROM downloads ORDER BY id DESC LIMIT 200" [] );
            ]
          |> encode))
