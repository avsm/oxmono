module D = Duraml
module Q = Sqlite3
let get = function Ok x -> x | Error _ ->
  raise (D.err (D.Storage "Invalid stored queue record"))
let encode codec x = get (Jsont_bytesrw.encode_string codec x)
let decode codec s = get (Jsont_bytesrw.decode_string codec s)
let exec db sql = Q.Rc.check (Q.exec db sql)
let stmt db sql args f =
  let s=Q.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Q.finalize s)) (fun () ->
    Q.Rc.check (Q.bind_values s args); f s)
let command db sql args = stmt db sql args (fun s -> Q.Rc.check (Q.step s))
let scalar db sql args = stmt db sql args (fun s ->
  match Q.step s with Q.Rc.ROW -> Q.column_text s 0
  | rc -> Q.Rc.check rc; raise (D.err (D.Storage "Missing store metadata")))
let one db codec sql args = stmt db sql args (fun s -> match Q.step s with
  | Q.Rc.DONE -> None
  | Q.Rc.ROW -> Some (decode codec (Q.column_text s 0))
  | rc -> Q.Rc.check rc; assert false)
let many db codec sql args = stmt db sql args (fun s ->
  let rec loop acc = match Q.step s with
    | Q.Rc.DONE -> List.rev acc
    | Q.Rc.ROW -> loop (decode codec (Q.column_text s 0)::acc)
    | rc -> Q.Rc.check rc; assert false in loop [])
let id i = Q.Data.INT (Int64.of_string (D.Id.to_string i))
let integer n = Q.Data.INT (Int64.of_int n)
let optional f = function None -> Q.Data.NULL | Some x -> f x
(* Dispatch reads lanes in service order and probes each lane's oldest
   eligible job through the eligible index, so a claim costs one probe per
   lane plus any delayed retries older than the lane's first eligible job.
   The pending index serves the next-deadline aggregate. *)
let dispatch_schema = {|
CREATE INDEX eligible ON jobs(queue,lane,created,id)
 WHERE state IN ('queued','running');
CREATE INDEX pending ON jobs(queue,available)
 WHERE state IN ('queued','running');
CREATE TABLE lanes(queue TEXT NOT NULL,lane TEXT NOT NULL,
 served INTEGER NOT NULL,PRIMARY KEY(queue,lane));
CREATE INDEX lane_service ON lanes(queue,served,lane);
|}
let schema = {|
CREATE TABLE meta(key TEXT PRIMARY KEY,value TEXT NOT NULL);
CREATE TABLE jobs(id INTEGER PRIMARY KEY,queue TEXT NOT NULL,key TEXT,
 state TEXT NOT NULL,available TEXT NOT NULL,data TEXT NOT NULL,
 lane TEXT NOT NULL DEFAULT '',created TEXT NOT NULL DEFAULT '');
CREATE UNIQUE INDEX job_keys ON jobs(queue,key) WHERE key IS NOT NULL;
CREATE INDEX browsing ON jobs(queue,state,id);
CREATE TABLE attempts(job INTEGER NOT NULL REFERENCES jobs(id),
 generation INTEGER NOT NULL,data TEXT NOT NULL,PRIMARY KEY(job,generation));
CREATE TABLE events(sequence INTEGER PRIMARY KEY,data TEXT NOT NULL);
PRAGMA application_id=1146442305;
|} ^ dispatch_schema ^ "PRAGMA user_version=3;"
(* Schema 2 ordered dispatch by deadline alone. Version 3 adds the lane
   and creation columns, the lanes table and the dispatch indexes, filling
   them from the stored records. Every lane starts unserved. *)
let upgrade_2_to_3 = {|
ALTER TABLE jobs ADD COLUMN lane TEXT NOT NULL DEFAULT '';
ALTER TABLE jobs ADD COLUMN created TEXT NOT NULL DEFAULT '';
UPDATE jobs SET created=json_extract(data,'$.created_at'),
 lane=coalesce(json_extract(data,'$.spec.lane'),'');
DROP INDEX eligible;
|} ^ dispatch_schema ^ {|
INSERT INTO lanes SELECT DISTINCT queue,lane,0 FROM jobs;
PRAGMA user_version=3;
|}
let supported = "3"
(* The browsing index covers queue, state and ID, so both listing and
   counting stay indexed for every filter combination. *)
let filters q =
  let clauses=ref ["id>?"] in
  let args=ref [Option.fold ~none:(Q.Data.INT 0L) ~some:id (D.Query.after q)]
  in
  Option.iter (fun queue -> clauses:= !clauses@["queue=?"];
    args:= !args@[Q.Data.TEXT queue]) (D.Query.queue q);
  Option.iter (fun state -> clauses:= !clauses@["state=?"];
    args:= !args@[Q.Data.TEXT (D.state_name state)]) (D.Query.state q);
  (* Parents are rare lookups, so they read the record instead of a column. *)
  Option.iter (fun parent ->
    clauses:= !clauses@["json_extract(data,'$.parent')=?"];
    args:= !args@[Q.Data.TEXT (D.Id.to_string parent)]) (D.Query.parent q);
  String.concat " AND " !clauses, !args
let storage db identity =
  let find i=one db Codec.job_codec "SELECT data FROM jobs WHERE id=?" [id i] in
  let revision ()=Int64.of_string
    (scalar db "SELECT coalesce(max(sequence),0) FROM events" []) in
  {D.Pi.identity;
   allocate=(fun () ->
     let n=Int64.of_string (scalar db "SELECT value FROM meta WHERE key='next'"
       []) in
     if n=Int64.max_int then raise (D.err (D.Storage "Job IDs exhausted"));
     command db "UPDATE meta SET value=? WHERE key='next'"
       [TEXT (Int64.to_string (Int64.succ n))];
     Result.get_ok (D.Id.of_string (Int64.to_string n)));
   find;
   find_key=(fun ~queue ~key -> one db Codec.job_codec
     "SELECT data FROM jobs WHERE queue=? AND key=?" [TEXT queue;TEXT key]);
   put=(fun (j:D.job) ->
     let queue=D.Spec.queue (D.Job.spec j) and lane=D.Spec.lane (D.Job.spec j)
     in
     command db {|INSERT INTO jobs(id,queue,key,state,available,data,lane,
 created) VALUES(?,?,?,?,?,?,?,?)
ON CONFLICT(id) DO UPDATE SET state=excluded.state,
 available=excluded.available,data=excluded.data|}
     [id (D.Job.id j);TEXT queue;optional (fun s ->
       Q.Data.TEXT s) (D.Spec.key (D.Job.spec j));
      TEXT (D.state_name (D.Job.state j));TEXT (Codec.time_string
        (D.Job.available_at j));
      TEXT (encode Codec.job_codec j);TEXT lane;
      TEXT (Codec.time_string (D.Job.created_at j))];
     command db "INSERT OR IGNORE INTO lanes VALUES(?,?,0)"
       [TEXT queue;TEXT lane]);
   eligible=(fun ~queue ~now ->
     let lanes=stmt db
       "SELECT lane FROM lanes WHERE queue=? ORDER BY served,lane"
       [TEXT queue] (fun s ->
         let rec loop acc = match Q.step s with
           | Q.Rc.DONE -> List.rev acc
           | Q.Rc.ROW -> loop (Q.column_text s 0::acc)
           | rc -> Q.Rc.check rc; assert false in loop []) in
     (* The planner must walk the eligible index in creation order and
        stop at the first eligible row rather than sort the lane. *)
     List.find_map (fun lane -> one db Codec.job_codec
       "SELECT data FROM jobs INDEXED BY eligible WHERE queue=? AND lane=? \
        AND state IN ('queued','running') AND available<=? \
        ORDER BY created,id LIMIT 1"
       [TEXT queue;TEXT lane;TEXT (Codec.time_string now)]) lanes);
   served=(fun ~queue ~lane -> command db
     "UPDATE lanes SET served=(SELECT coalesce(max(served),0)+1 FROM lanes \
      WHERE queue=?) WHERE queue=? AND lane=?"
     [TEXT queue;TEXT queue;TEXT lane]);
   next_at=(fun ~queue -> stmt db
     "SELECT min(available) FROM jobs WHERE queue=? \
      AND state IN ('queued','running')"
     [TEXT queue] (fun s ->
       if Q.step s<>Q.Rc.ROW then failwith "Missing aggregate";
       match Q.column s 0 with NULL -> None | _ -> Some (Codec.time_of_string
         (Q.column_text s 0))));
   query=(fun q ->
     let where,args=filters q in
     many db Codec.job_codec ("SELECT data FROM jobs WHERE " ^ where ^
       " ORDER BY id LIMIT ?") (args@[integer (D.Query.limit q)]));
   count=(fun q ->
     let where,args=filters q in
     int_of_string (scalar db ("SELECT count(*) FROM jobs WHERE " ^ where)
       args));
   attempt=(fun i n -> one db Codec.attempt_codec
     "SELECT data FROM attempts WHERE job=? AND generation=?" [id i;integer n]);
   put_attempt=(fun a ->
     let c=D.Pi.attempt_claim a in
     command db
       "INSERT INTO attempts VALUES(?,?,?) ON CONFLICT(job,generation) \
       DO UPDATE SET data=excluded.data"
       [id (D.Claim.id c);integer (D.Claim.attempt c);
        TEXT (encode Codec.attempt_codec a)]);
   emit=(fun ~time ~kind ~job ~attempt ~detail ->
     let sequence=Int64.succ (revision ()) in
     let e=D.Pi.restore_event ~sequence ~time ~kind ~job ~attempt ~detail in
     command db "INSERT INTO events VALUES(?,?)"
       [INT sequence;TEXT (encode Codec.event_codec e)]);
   revision;
   events=(fun ~after ~limit -> many db Codec.event_codec
     "SELECT data FROM events WHERE sequence>? ORDER BY sequence LIMIT ?"
     [INT after;integer limit]);
   tail=(fun ~before ~limit -> many db Codec.event_codec
     "SELECT data FROM events WHERE sequence<? ORDER BY sequence DESC LIMIT ?"
     [INT (Option.value ~default:Int64.max_int before);integer limit])}
type t = {
  db : Sqlite3_eio.t; identity : string; mutex : Eio.Mutex.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  random : Eio.Flow.source_ty Eio.Resource.t;
  poll_interval : float; read_only : bool; mutable closed : bool;
}
let check t = if t.closed then raise (D.err D.Closed)
let protect_sql f = try f () with
  | Q.Error _ | Q.SqliteError _ ->
      raise (D.err (D.Storage "SQLite operation failed"))
let with_lock t f =
  let result=Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
    try check t; Ok (protect_sql f)
    with ex -> Error (ex,Printexc.get_raw_backtrace ())) in
  match result with Ok x -> x
  | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let call : type a. t -> a D.Pi.request -> a = fun t request ->
  check t;
  let token=match request with D.Pi.Try_take _ -> D.Pi.random_id t.random
    | _ -> "" in
  with_lock t (fun () ->
    match request with
    | D.Pi.Identity | D.Pi.Find _ | D.Pi.Query _ | D.Pi.Count _
    | D.Pi.Events _ | D.Pi.Tail _ ->
        let now=D.Pi.time_of_float_s (Eio.Time.now t.clock) in
        Sqlite3_eio.run t.db (fun db ->
          D.Pi.execute ~now ~token (storage db t.identity) request)
    | _ when t.read_only ->
        raise (D.err (D.Storage "Store was opened read-only"))
    | _ ->
        (* Sample time after acquiring the database write lock. Otherwise a
           busy writer could make a stale timestamp renew an expired claim. *)
        let transaction=ref true in
        Fun.protect ~finally:(fun () -> if !transaction then
          Eio.Cancel.protect (fun () -> Sqlite3_eio.run t.db (fun db ->
            let rc=Q.exec db "ROLLBACK" in
            if rc<>Q.Rc.OK &&
              Q.errmsg db<>"cannot rollback - no transaction is active" then
              (t.closed <- true; Q.Rc.check rc))))
          (fun () ->
            Sqlite3_eio.run t.db (fun db -> exec db "BEGIN IMMEDIATE");
            let now=D.Pi.time_of_float_s (Eio.Time.now t.clock) in
            let result=Sqlite3_eio.run t.db (fun db ->
              let result=D.Pi.execute ~now ~token (storage db t.identity)
                request in
              exec db "COMMIT"; result) in
            transaction:=false; result))
let await t (w:D.waiting) =
  let revision=with_lock t (fun () -> Sqlite3_eio.run t.db (fun db ->
    (storage db t.identity).revision ())) in
  if revision=(D.Waiting.revision w) then begin
    let now=Eio.Time.now t.clock in
    let poll=max (Float.next_after now infinity) (now+.t.poll_interval) in
    let at=match D.Waiting.next_at w with None -> poll
      | Some at -> min poll (D.Pi.clock_deadline at) in
    Eio.Time.sleep_until t.clock at;
    check t
  end
module Backend = struct
  type nonrec t = t
  let call = call
  let await = await
end
let open_ ~sw ~clock ~random ?(poll_interval=Ptime.Span.v (0,100_000_000_000L))
  ?(read_only=false) path =
  if Ptime.Span.compare poll_interval Ptime.Span.zero<=0 then
    invalid_arg "Invalid polling interval";
  let poll_interval=Ptime.Span.to_float_s poll_interval in
  let identity=D.Pi.random_id random in
  if not read_only then
    Eio.Path.with_open_out ~follow:false ~create:(`If_missing 0o600) path
      (fun _ -> ());
  let db=if read_only then
    Sqlite3_eio.open_path ~sw ~busy_timeout:5000 ~mode:`READONLY path
    else Sqlite3_eio.open_path ~sw ~busy_timeout:5000 path in
  let unsupported ()=raise (D.err (D.Storage "Unsupported Duraml database")) in
  let identity=protect_sql (fun () -> Sqlite3_eio.run db (fun db ->
    exec db "PRAGMA foreign_keys=ON";
    if read_only then begin
      let version=scalar db "PRAGMA user_version" [] in
      let app=scalar db "PRAGMA application_id" [] in
      if version<>supported || app<>"1146442305" then unsupported ();
      scalar db "SELECT value FROM meta WHERE key='identity'" []
    end else begin
    exec db "BEGIN IMMEDIATE";
    let identity=try
      let version=scalar db "PRAGMA user_version" [] in
      let app=scalar db "PRAGMA application_id" [] in
      if version="0" && app="0" then begin
        if scalar db "SELECT count(*) FROM sqlite_master \
          WHERE name NOT LIKE 'sqlite_%'" [] <> "0" then
          raise (D.err (D.Storage "Refusing unrelated database"));
        exec db schema;
        command db "INSERT INTO meta VALUES('identity',?),('next','1')"
          [TEXT identity]
      end else if app<>"1146442305" then unsupported ()
      else if version="2" then exec db upgrade_2_to_3
      else if version<>supported then unsupported ();
      let identity=scalar db "SELECT value FROM meta WHERE key='identity'" [] in
      exec db "COMMIT"; identity
    with ex -> exec db "ROLLBACK"; raise ex in
    exec db "PRAGMA journal_mode=WAL";
    exec db "PRAGMA synchronous=FULL";
    identity end)) in
  let t={db;identity;mutex=Eio.Mutex.create ();closed=false;poll_interval;
    read_only;
    clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);
    random=(random :> Eio.Flow.source_ty Eio.Resource.t)} in
  Eio.Switch.on_release sw (fun () -> t.closed <- true);
  Eio.Resource.T (t,D.Pi.handler (module Backend))
