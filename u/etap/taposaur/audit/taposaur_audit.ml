module Q = Sqlite3
type read = Read
type write = Write
type 'a t = {db : Sqlite3_eio.t; lock : Eio.Mutex.t}
type session = {store : write t; actor : string; component : string;
  run : string; parent : string option}
type phase = Started | Succeeded | Failed | Cancelled | Notice
let phase_name = function Started -> "started" | Succeeded -> "succeeded"
  | Failed -> "failed" | Cancelled -> "cancelled" | Notice -> "notice"
let phase_of_string = function "started" -> Started | "succeeded" -> Succeeded
  | "failed" -> Failed | "cancelled" -> Cancelled | "notice" -> Notice
  | _ -> invalid_arg "Unknown audit phase"
exception Completion_unrecorded of string
let check = Q.Rc.check
let exec db sql = check (Q.exec db sql)
let stmt db sql f = let s = Q.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Q.finalize s)) (fun () -> f s)
let run t f =
  let result = Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
    try Ok (Sqlite3_eio.run t.db f)
    with ex -> Error (ex, Printexc.get_raw_backtrace ())) in
  match result with Ok v -> v | Error (ex,bt) -> Printexc.raise_with_backtrace ex bt
let transaction db f =
  exec db "BEGIN IMMEDIATE";
  match f () with
  | v -> (try exec db "COMMIT"; v with ex -> exec db "ROLLBACK"; raise ex)
  | exception ex -> let bt = Printexc.get_raw_backtrace () in
      (try exec db "ROLLBACK" with _ -> ()); Printexc.raise_with_backtrace ex bt
let schema = {|
CREATE TABLE events (
 id INTEGER PRIMARY KEY AUTOINCREMENT,
 v INTEGER NOT NULL DEFAULT 1,
 time REAL NOT NULL,
 run TEXT NOT NULL, operation TEXT NOT NULL, parent TEXT,
 actor TEXT NOT NULL, component TEXT NOT NULL,
 action TEXT NOT NULL, subject TEXT NOT NULL,
 phase TEXT NOT NULL CHECK(phase IN ('started','succeeded','failed','cancelled','notice')),
 details TEXT NOT NULL CHECK(json_valid(details) AND json_type(details)='object'),
 UNIQUE(operation,phase));
CREATE UNIQUE INDEX terminal ON events(operation)
 WHERE phase IN ('succeeded','failed','cancelled');
CREATE INDEX subject_events ON events(subject,id);
CREATE INDEX actor_events ON events(actor,id);
CREATE INDEX action_events ON events(action,id);
CREATE INDEX run_events ON events(run,id);
CREATE INDEX phase_events ON events(phase,id);
CREATE TRIGGER no_update BEFORE UPDATE ON events
 BEGIN SELECT RAISE(ABORT,'Audit events are append-only'); END;
CREATE TRIGGER no_delete BEFORE DELETE ON events
 BEGIN SELECT RAISE(ABORT,'Audit events are append-only'); END;
PRAGMA application_id=1413562709;
PRAGMA user_version=1;
|}
let open_db ~sw ~readonly path =
  if not readonly then Eio.Path.with_open_out ~follow:false
    ~create:(`If_missing 0o600) path (fun _ -> ());
  let db = Sqlite3_eio.open_path ~sw ~busy_timeout:5000
    ?mode:(if readonly then Some `READONLY else None) path in
  let t = {db; lock=Eio.Mutex.create ()} in
  run t (fun db ->
    let scalar sql = stmt db sql (fun s ->
      if Q.step s <> Q.Rc.ROW then failwith "Missing audit schema state";
      Q.column_int s 0) in
    let validate () =
      let app = scalar "PRAGMA application_id" in
      let version = scalar "PRAGMA user_version" in
      if app = 0 && version = 0 && not readonly then begin
        if scalar "SELECT count(*) FROM sqlite_master WHERE name NOT LIKE 'sqlite_%'" <> 0
        then failwith "Refusing nonempty audit database";
        exec db schema
      end else if app <> 1413562709 || version <> 1 then
        failwith "Unsupported audit database" in
    if readonly then (validate (); exec db "PRAGMA query_only=ON")
    else begin
      transaction db validate;
      exec db "PRAGMA journal_mode=WAL";
      exec db "PRAGMA synchronous=FULL"
    end);
  t
let open_read ~sw path : read t = open_db ~sw ~readonly:true path
let open_write ~sw path : write t = open_db ~sw ~readonly:false path
let bounded name limit value =
  if value = "" || String.length value > limit || String.contains value '\000'
  then invalid_arg ("Invalid audit " ^ name)
let uuid () =
  let ch = open_in_bin "/dev/urandom" in
  let bytes = Fun.protect ~finally:(fun () -> close_in ch)
    (fun () -> really_input_string ch 16) in
  String.concat "" (List.init 16 (fun i -> Printf.sprintf "%02x" (Char.code bytes.[i])))
let session ?(actor="system") ~component store =
  bounded "actor" 256 actor; bounded "component" 128 component;
  {store; actor; component; run=uuid (); parent=None}
let store (s : session) = s.store
let run_id (s : session) = s.run
let actor (s : session) = s.actor
let quote s = match Jsont_bytesrw.encode_string Jsont.string s with
  | Ok s -> s | Error e -> failwith e
let details_json fields =
  if List.length fields > 32 then invalid_arg "Too many audit fields";
  let seen = Hashtbl.create 8 in
  let body = "{" ^ String.concat "," (List.map (fun (key,value) ->
    bounded "field name" 64 key;
    if Hashtbl.mem seen key then invalid_arg "Duplicate audit field";
    Hashtbl.add seen key ();
    let forbidden = ["password";"secret";"token";"authorization";"api_key"] in
    let lower = String.lowercase_ascii key in
    if List.exists (fun word ->
      let rec contains i = i + String.length word <= String.length lower &&
        (String.sub lower i (String.length word) = word || contains (i+1)) in
      contains 0) forbidden then invalid_arg "Sensitive audit field refused";
    quote key ^ ":" ^ quote value) fields) ^ "}" in
  if String.length body > 16384 then invalid_arg "Audit details exceed 16 KiB";
  body
let append (s : session) ~operation ~phase ~action ~subject details =
  bounded "action" 128 action; bounded "subject" 4096 subject;
  let body = details_json details in
  Eio.Cancel.protect (fun () -> run s.store (fun db ->
    stmt db "INSERT INTO events(time,run,operation,parent,actor,component,action,subject,phase,details) VALUES(?,?,?,?,?,?,?,?,?,?)"
      (fun q -> check (Q.bind_values q [Q.Data.FLOAT (Unix.gettimeofday ());
        TEXT s.run; TEXT operation;
        (match s.parent with None -> NULL | Some p -> TEXT p);
        TEXT s.actor; TEXT s.component; TEXT action; TEXT subject;
        TEXT (phase_name phase); TEXT body]);
        check (Q.step q))))
let record s ~action ~subject ?(details=[]) () =
  append s ~operation:(uuid ()) ~phase:Notice ~action ~subject details
let error_kind = function
  | Eio.Cancel.Cancelled _ -> "cancelled"
  | Eio.Io _ -> "io"
  | Unix.Unix_error _ -> "unix"
  | Q.Error _ | Q.SqliteError _ -> "sqlite"
  | Invalid_argument _ -> "invalid_argument"
  | Failure _ -> "failure"
  | _ -> "exception"
let with_operation ?session ?actor ?(details=[]) ?(success=fun _ -> true)
    ?(result_details=fun _ -> []) ~action ~subject f =
  match session with None -> f None | Some s ->
    let s = match actor with None -> s | Some actor ->
      bounded "actor" 256 actor; {s with actor} in
    let operation = uuid () in
    append s ~operation ~phase:Started ~action ~subject details;
    match f (Some {s with parent=Some operation}) with
    | result ->
        (try append s ~operation
           ~phase:(if success result then Succeeded else Failed) ~action ~subject
           (result_details result)
         with _ -> raise (Completion_unrecorded operation)); result
    | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        let kind = error_kind ex in
        (try append s ~operation
           ~phase:(if kind = "cancelled" then Cancelled else Failed)
           ~action ~subject ["error_kind",kind]
         with _ -> Printf.eprintf
           "Audit terminal event unavailable for operation %s; outcome requires reconciliation.\n%!" operation);
        Printexc.raise_with_backtrace ex bt

type event = {version:int;id:int64; time:float; run:string; operation:string;
  parent:string option; actor:string; component:string; action:string;
  subject:string; phase:phase; details:string}
let query ?actor ?action ?subject ?run:run_id ?operation ?phase ?(incomplete=false)
    ?before ?(limit=100) t =
  if limit < 1 || limit > 500 then invalid_arg "Audit limit must be 1..500";
  let filters = List.filter_map Fun.id [
    Option.map (fun s -> "e.actor=?",Q.Data.TEXT s) actor;
    Option.map (fun s -> "e.action=?",Q.Data.TEXT s) action;
    Option.map (fun s -> "e.subject=?",Q.Data.TEXT s) subject;
    Option.map (fun s -> "e.run=?",Q.Data.TEXT s) run_id;
    Option.map (fun s -> "e.operation=?",Q.Data.TEXT s) operation;
    Option.map (fun p -> "e.phase=?",Q.Data.TEXT (phase_name p)) phase;
    Option.map (fun n -> "e.id<?",Q.Data.INT n) before] in
  let conditions = List.map fst filters @ if incomplete then
    ["e.phase='started' AND NOT EXISTS (SELECT 1 FROM events terminal WHERE terminal.operation=e.operation AND terminal.phase IN ('succeeded','failed','cancelled'))"] else [] in
  let index = match operation,subject,run_id,action,actor,phase with
    | Some _,_,_,_,_,_ -> ""
    | _,Some _,_,_,_,_ -> " INDEXED BY subject_events"
    | _,_,Some _,_,_,_ -> " INDEXED BY run_events"
    | _,_,_,Some _,_,_ -> " INDEXED BY action_events"
    | _,_,_,_,Some _,_ -> " INDEXED BY actor_events"
    | _,_,_,_,_,Some _ -> " INDEXED BY phase_events"
    | _ -> "" in
  let sql = "SELECT e.id,e.time,e.run,e.operation,e.parent,e.actor,e.component,e.action,e.subject,e.phase,e.details,e.v FROM events e" ^ index ^
    (if conditions=[] then "" else " WHERE " ^ String.concat " AND " conditions) ^
    " ORDER BY e.id DESC LIMIT ?" in
  run t (fun db -> stmt db sql (fun s ->
    check (Q.bind_values s (List.map snd filters @ [Q.Data.INT (Int64.of_int limit)]));
    let rec loop acc = match Q.step s with
      | Q.Rc.ROW ->
        let e = {version=Q.column_int s 11;id=Q.column_int64 s 0;time=Q.column_double s 1;
          run=Q.column_text s 2;operation=Q.column_text s 3;
          parent=(if Q.column s 4 = NULL then None else Some (Q.column_text s 4));
          actor=Q.column_text s 5;component=Q.column_text s 6;
          action=Q.column_text s 7;subject=Q.column_text s 8;
          phase=phase_of_string (Q.column_text s 9);details=Q.column_text s 10} in
        loop (e::acc)
      | Q.Rc.DONE -> List.rev acc | rc -> check rc; assert false in loop []))
let to_json (e : event) = Printf.sprintf
  "{\"v\":%d,\"id\":%Ld,\"time\":%.6f,\"run\":%s,\"operation\":%s,\"parent\":%s,\"actor\":%s,\"component\":%s,\"action\":%s,\"subject\":%s,\"phase\":%s,\"details\":%s}"
  e.version e.id e.time (quote e.run) (quote e.operation)
  (Option.fold ~none:"null" ~some:quote e.parent) (quote e.actor)
  (quote e.component) (quote e.action) (quote e.subject)
  (quote (phase_name e.phase)) e.details
