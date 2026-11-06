module D = Duraml
module Q = Sqlite3
let damaged () = D.err (D.Storage "Invalid stored queue record")
let get = function Ok x -> x | Error _ -> raise (damaged ())
let encode codec x = get (Jsont_bytesrw.encode_string codec x)
(* Record constructors validate as they decode, and raise on a damaged
   record rather than return an error. *)
let decode codec s =
  try get (Jsont_bytesrw.decode_string codec s)
  with Invalid_argument _ | Failure _ -> raise (damaged ())
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
let time_of_string s =
  try Codec.time_of_string s with Invalid_argument _ -> raise (damaged ())
let id i = Q.Data.INT (Int64.of_string (D.Id.to_string i))
let integer n = Q.Data.INT (Int64.of_int n)
let optional f = function None -> Q.Data.NULL | Some x -> f x
let application_id = "1146442305"
let supported = "4"
(* The lanes table holds the lanes with queued or running jobs, and the
   rotations table each queue's latest service rank. Dispatch
   walks them in service order and seeks the first with an eligible job
   through the ready index, then walks that lane's eligible index in
   creation order, reading deadlines from the index, to its oldest eligible
   job. The pending index serves the next-deadline aggregate. *)
let dispatch_indexes = {|
CREATE INDEX eligible ON jobs(queue,lane,created,id,available)
 WHERE state IN ('queued','running');
CREATE INDEX ready ON jobs(queue,lane,available)
 WHERE state IN ('queued','running');
|}
let dispatch_schema = dispatch_indexes ^ {|
CREATE INDEX pending ON jobs(queue,available)
 WHERE state IN ('queued','running');
CREATE TABLE lanes(queue TEXT NOT NULL,lane TEXT NOT NULL,
 served INTEGER NOT NULL,PRIMARY KEY(queue,lane));
CREATE INDEX lane_service ON lanes(queue,served,lane);
CREATE TABLE rotations(queue TEXT PRIMARY KEY,latest INTEGER NOT NULL);
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
PRAGMA application_id=|} ^ application_id ^ ";" ^ dispatch_schema
  ^ "PRAGMA user_version=" ^ supported ^ ";"
(* Version 3 adds the lane and creation columns, the lanes table and the
   dispatch indexes, filling them from the stored records. *)
let upgrade_2_to_3 = {|
ALTER TABLE jobs ADD COLUMN lane TEXT NOT NULL DEFAULT '';
ALTER TABLE jobs ADD COLUMN created TEXT NOT NULL DEFAULT '';
UPDATE jobs SET created=json_extract(data,'$.created_at'),
 lane=coalesce(json_extract(data,'$.spec.lane'),'');
DROP INDEX eligible;
CREATE INDEX eligible ON jobs(queue,lane,created,id)
 WHERE state IN ('queued','running');
CREATE INDEX pending ON jobs(queue,available)
 WHERE state IN ('queued','running');
CREATE TABLE lanes(queue TEXT NOT NULL,lane TEXT NOT NULL,
 served INTEGER NOT NULL,PRIMARY KEY(queue,lane));
CREATE INDEX lane_service ON lanes(queue,served,lane);
INSERT INTO lanes SELECT DISTINCT queue,lane,0 FROM jobs;
PRAGMA user_version=3;
|}
(* Version 4 keeps only lanes with work, ranks a lane that gains work as
   served just now, and indexes deadlines for dispatch. *)
let upgrade_3_to_4 = {|
DROP INDEX eligible;
|} ^ dispatch_indexes ^ {|
CREATE TABLE rotations(queue TEXT PRIMARY KEY,latest INTEGER NOT NULL);
INSERT INTO rotations SELECT queue,max(served) FROM lanes GROUP BY queue;
DELETE FROM lanes WHERE NOT EXISTS (SELECT 1 FROM jobs
 WHERE jobs.queue=lanes.queue AND jobs.lane=lanes.lane
 AND state IN ('queued','running'));
UPDATE lanes SET served=(SELECT latest FROM rotations
 WHERE rotations.queue=lanes.queue) WHERE served=0;
PRAGMA user_version=4;
|}
(* The browsing index serves queue, state and ID filters when a queue is
   given. A parent filter reads every candidate record. *)
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
     match D.Job.state j with
     | D.Queued | D.Running -> command db
         "INSERT OR IGNORE INTO lanes VALUES(?1,?2,coalesce((SELECT latest \
          FROM rotations WHERE queue=?1),0))" [TEXT queue;TEXT lane]
     | D.Done | D.Failed_job | D.Cancelled -> command db
         "DELETE FROM lanes WHERE queue=?1 AND lane=?2 AND NOT EXISTS \
          (SELECT 1 FROM jobs INDEXED BY ready WHERE queue=?1 AND lane=?2 \
          AND state IN ('queued','running'))" [TEXT queue;TEXT lane]);
   eligible=(fun ~queue ~now ->
     (* The planner must walk the eligible index in creation order and
        stop at the first eligible row rather than sort the lane. *)
     one db Codec.job_codec
       "SELECT data FROM jobs INDEXED BY eligible WHERE queue=?1 \
        AND state IN ('queued','running') AND available<=?2 AND lane=\
        (SELECT lane FROM lanes INDEXED BY lane_service WHERE queue=?1 \
        AND EXISTS (SELECT 1 FROM jobs INDEXED BY ready WHERE \
        jobs.queue=?1 AND jobs.lane=lanes.lane \
        AND state IN ('queued','running') AND available<=?2) \
        ORDER BY served,lane LIMIT 1) \
        ORDER BY created,id LIMIT 1"
       [TEXT queue;TEXT (Codec.time_string now)]);
   served=(fun ~queue ~lane ->
     command db "INSERT INTO rotations VALUES(?,1) ON CONFLICT(queue) \
       DO UPDATE SET latest=latest+1" [TEXT queue];
     command db "UPDATE lanes SET served=(SELECT latest FROM rotations \
       WHERE queue=?1) WHERE queue=?1 AND lane=?2" [TEXT queue;TEXT lane]);
   next_at=(fun ~queue -> stmt db
     "SELECT min(available) FROM jobs WHERE queue=? \
      AND state IN ('queued','running')"
     [TEXT queue] (fun s ->
       if Q.step s<>Q.Rc.ROW then raise (damaged ());
       match Q.column s 0 with NULL -> None | _ -> Some (time_of_string
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
  | Q.Error m | Q.SqliteError m ->
      raise (D.err (D.Storage ("SQLite operation failed: " ^ m)))
let with_lock t f =
  let result=Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
    try check t; Ok (protect_sql f)
    with ex -> Error (ex,Printexc.get_raw_backtrace ())) in
  match result with Ok x -> x
  | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
(* [rollback db] ends any open transaction, and is false if one may have
   stayed open. It is not cancellable, so cancellation cannot leave the
   write lock held. *)
let rollback db =
  Eio.Cancel.protect (fun () -> Sqlite3_eio.run db (fun db ->
    Q.exec db "ROLLBACK"=Q.Rc.OK ||
    Q.errmsg db="cannot rollback - no transaction is active"))
(* [transaction t ~mode f] runs [f] in a transaction that [f] commits. A
   failure that might leave it open closes the handle instead of holding
   the lock. *)
let transaction t ~mode f =
  Sqlite3_eio.run t.db (fun db -> exec db ("BEGIN " ^ mode));
  match f () with
  | result -> result
  | exception ex ->
      let bt=Printexc.get_raw_backtrace () in
      if not (rollback t.db) then t.closed <- true;
      Printexc.raise_with_backtrace ex bt
let call : type a. t -> a D.Pi.request -> a = fun t request ->
  check t;
  (* An idle poll reads without the write lock or a fresh token. *)
  let idle=match request with
    | D.Pi.Try_take (queue,lease) when not t.read_only ->
      with_lock t (fun () ->
        transaction t ~mode:"DEFERRED" (fun () ->
          let now=D.Pi.time_of_float_s (Eio.Time.now t.clock) in
          Sqlite3_eio.run t.db (fun db ->
            let idle=D.Pi.idle ~now ~lease (storage db t.identity) ~queue in
            exec db "COMMIT"; idle)))
    | _ -> None in
  match idle,request with
  | Some waiting,D.Pi.Try_take _ -> D.Await waiting
  | _ ->
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
        transaction t ~mode:"IMMEDIATE" (fun () ->
          let now=D.Pi.time_of_float_s (Eio.Time.now t.clock) in
          Sqlite3_eio.run t.db (fun db ->
            let result=D.Pi.execute ~now ~token (storage db t.identity)
              request in
            exec db "COMMIT"; result)))
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
  let opening={db;identity;mutex=Eio.Mutex.create ();closed=false;
    poll_interval;read_only;
    clock=(clock :> float Eio.Time.clock_ty Eio.Resource.t);
    random=(random :> Eio.Flow.source_ty Eio.Resource.t)} in
  let run f=Sqlite3_eio.run db f in
  let identity=try protect_sql (fun () ->
    run (fun db -> exec db "PRAGMA foreign_keys=ON");
    if read_only then run (fun db ->
      let version=scalar db "PRAGMA user_version" [] in
      let app=scalar db "PRAGMA application_id" [] in
      if version<>supported || app<>application_id then unsupported ();
      scalar db "SELECT value FROM meta WHERE key='identity'" [])
    else begin
      let identity=transaction opening ~mode:"IMMEDIATE" (fun () ->
        run (fun db ->
          let version=scalar db "PRAGMA user_version" [] in
          let app=scalar db "PRAGMA application_id" [] in
          if version="0" && app="0" then begin
            if scalar db "SELECT count(*) FROM sqlite_master \
              WHERE name NOT LIKE 'sqlite_%'" [] <> "0" then
              raise (D.err (D.Storage "Refusing unrelated database"));
            exec db schema;
            command db "INSERT INTO meta VALUES('identity',?),('next','1')"
              [TEXT identity]
          end else if app<>application_id then unsupported ()
          else begin
            if version="2" then exec db upgrade_2_to_3;
            if version="2" || version="3" then exec db upgrade_3_to_4
            else if version<>supported then unsupported ()
          end;
          let identity=scalar db "SELECT value FROM meta WHERE key='identity'"
            [] in
          exec db "COMMIT"; identity)) in
      run (fun db ->
        if String.lowercase_ascii (scalar db "PRAGMA journal_mode=WAL" [])
           <> "wal" then
          raise (D.err (D.Storage "SQLite WAL journal unavailable"));
        exec db "PRAGMA synchronous=FULL");
      identity
    end)
  with ex ->
    let bt=Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () -> Eio.Resource.close db);
    Printexc.raise_with_backtrace ex bt in
  let t={opening with identity} in
  Eio.Switch.on_release sw (fun () -> t.closed <- true);
  Eio.Resource.T (t,D.Pi.handler (module Backend))
