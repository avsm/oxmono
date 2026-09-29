module Sql = Sqlite3
module Db = Sqlite3_eio

type t = {
  db : Db.t;
  count : int64;
  owner : Maildir.t;
  appended_ids : (string,unit) Hashtbl.t;
  mutable live : bool;
}

type page = {
  occurrences : Maildir.occurrence list;
  next_after : string option;
}

let fail message = failwith ("Local_inventory: " ^ message)

let check_live view =
  if not view.live then invalid_arg "Local_inventory: expired inventory"

let check ?inventory maildir =
  Option.iter (fun view ->
    check_live view;
    if view.owner != maildir then
      invalid_arg "Local_inventory: inventory belongs to another handle")
    inventory

let count view = check_live view; view.count

let db_check = Sql.Rc.check
let db_exec db sql = db_check (Db.exec db sql)
let with_stmt db sql f =
  let stmt=Eio.Cancel.protect (fun () -> Db.prepare db sql) in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () -> db_check (Db.finalize db stmt)))
    (fun () -> f stmt)
let bind stmt values = db_check (Sql.bind_values stmt values)
let step db stmt =
  match Db.step db stmt with
  | Sql.Rc.ROW -> true
  | Sql.Rc.DONE -> false
  | rc ->
      db_check rc;
      fail ("unexpected SQLite step result " ^ Sql.Rc.to_string rc)

(* [Maildir.occurrence] is private, so a row keeps the observation that
   this process marshalled while staging. The file never outlives the
   process that wrote it. *)
let staged stmt : Maildir.occurrence =
  match Sql.column stmt 0 with
  | Sql.Data.BLOB raw -> Marshal.from_string raw 0
  | _ -> fail "invalid staged occurrence"

let find view ~id =
  check_live view;
  with_stmt view.db "SELECT occurrence FROM occurrences WHERE id=?"
    (fun stmt ->
    bind stmt [Sql.Data.TEXT id];
    if step view.db stmt then Some (staged stmt) else None)

let page view ?after ~limit () =
  check_live view;
  if limit<=0 then invalid_arg "Local_inventory.page: limit must be positive";
  if limit=max_int then invalid_arg "Local_inventory.page: limit too large";
  let sql="SELECT occurrence FROM occurrences WHERE id > ? \
           ORDER BY id LIMIT ?" in
  let boundary=Option.value ~default:"" after in
  with_stmt view.db sql (fun stmt ->
    bind stmt [Sql.Data.TEXT boundary;Sql.Data.INT (Int64.of_int (limit+1))];
    let rec collect n acc =
      if not (step view.db stmt) then
        {occurrences=List.rev acc;next_after=None}
      else if n=limit then
        {occurrences=List.rev acc;
         next_after=Option.map (fun (o:Maildir.occurrence) -> o.id)
           (List.nth_opt acc 0)}
      else collect (n+1) (staged stmt::acc) in
    collect 0 [])

let prefix="local-inventory-"
let suffix=".sqlite3"

let discard path =
  Eio.Cancel.protect (fun () ->
    try Eio.Path.unlink path with Eio.Io _ -> ())

exception Duplicate of string

let with_pages ~spool_dir maildir f =
  let path=Eio.Path.(spool_dir / (prefix ^ Maildir.reserve_id () ^ suffix)) in
  let owned=ref false in
  Fun.protect ~finally:(fun () -> if !owned then discard path) (fun () ->
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) path (fun _ ->
      owned:=true);
    Eio.Switch.run (fun sw ->
      let db=Db.open_path ~sw path in
      db_exec db "PRAGMA journal_mode=OFF";
      db_exec db "PRAGMA synchronous=OFF";
      db_exec db "PRAGMA cache_size=-2048";
      db_exec db "CREATE TABLE occurrences \
        (id TEXT PRIMARY KEY,occurrence BLOB NOT NULL)";
      db_exec db "BEGIN";
      let staged=with_stmt db
        "INSERT INTO occurrences(id,occurrence) VALUES (?,?)" (fun stmt ->
        match
          Maildir.fold maildir ~init:0L ~f:(fun count (o:Maildir.occurrence) ->
            bind stmt [Sql.Data.TEXT o.id;
              Sql.Data.BLOB (Marshal.to_string o [])];
            (match Db.step db stmt with
             | Sql.Rc.DONE -> ()
             | Sql.Rc.CONSTRAINT ->
                 ignore (Db.reset db stmt : Sql.Rc.t);
                 raise (Duplicate o.id)
             | rc -> db_check rc);
            db_check (Db.reset db stmt);
            Int64.succ count)
        with
        | staged -> staged
        | exception Duplicate id ->
            Error (Maildir.Duplicate_identity id)) in
      match staged with
      | Error _ as error -> error
      | Ok count ->
          db_exec db "COMMIT";
          let view={db;count;owner=maildir;live=true;
            appended_ids=Hashtbl.create 32} in
          Fun.protect ~finally:(fun () -> view.live<-false) (fun () ->
            Ok (f view))))

let with_unchanged_occurrence ?inventory maildir o f =
  check ?inventory maildir;
  Maildir.with_unchanged_occurrence maildir o f

let sha256 ?inventory maildir o =
  check ?inventory maildir;
  Maildir.sha256 maildir o

let open_message ?inventory maildir ~sw o =
  check ?inventory maildir;
  Maildir.open_message maildir ~sw o

let append ?inventory writer ?id ~source ~length ~flags ?mtime () =
  check ?inventory (Maildir.of_writer writer);
  match inventory,id with
  | Some view,Some id when
      Hashtbl.mem view.appended_ids id || find view ~id<>None ->
      Error (Maildir.Duplicate_identity id)
  | _ ->
      let appended=Maildir.append writer ?id ~source ~length ~flags ?mtime
        () in
      (match inventory,appended with
       | Some view,Ok o -> Hashtbl.replace view.appended_ids o.id ()
       | _ -> ());
      appended

let recover spool_dir =
  let staging name =
    let n=String.length prefix in
    String.length name=n+35+String.length suffix &&
    String.starts_with ~prefix name && String.ends_with ~suffix name &&
    String.sub name n 3="im-" &&
    String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false)
      (String.sub name (n+3) 32) in
  let names=Eio.Path.read_dir spool_dir |> List.filter staging in
  List.iter (fun name -> Eio.Path.unlink Eio.Path.(spool_dir / name)) names;
  names
