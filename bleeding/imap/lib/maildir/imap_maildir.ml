type location = New | Cur
type occurrence = {
  id : string;
  filename : string;
  location : location;
  length : int64;
  mtime : float;
  inode : int64;
  ctime : float;
  flags : Mail_flag.Imap_flag.t list;
  internal_date : Imap.Internal_date.t option;
}
type directory = Dir : _ Eio.Path.t -> directory
type t = { root : directory; tmp : directory; new_dir : directory;
           cur : directory }
type recovery = { removed_temporary : string list }
exception Writer_lock_busy = Dotlock.Busy
exception Stale_occurrence
module Sql = Sqlite3
module Db = Sqlite3_eio

(* POSIX record locks are process-scoped: a second lockf in this process would
   succeed, and closing that second descriptor could release the first lock.
   Reserve the canonical directory before opening any lock-file descriptor. *)
let writer_locks = Hashtbl.create 17
let writer_locks_mutex = Mutex.create ()
let reserve_writer key path =
  Mutex.lock writer_locks_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock writer_locks_mutex) (fun () ->
    if Hashtbl.mem writer_locks key then raise (Writer_lock_busy path);
    Hashtbl.add writer_locks key ())
let release_writer key =
  Mutex.lock writer_locks_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock writer_locks_mutex) (fun () ->
    Hashtbl.remove writer_locks key)

let fail = Keywords.fail
let child (Dir p) name = Dir Eio.Path.(p / name)
let kind (Dir p) = Eio.Path.kind ~follow:false p
let is_directory (Dir p) = Eio.Path.is_directory p
let read_dir (Dir p) = Eio.Path.read_dir p
let unlink ?(missing_ok=false) (Dir p) = Eio.Path.unlink ~missing_ok p
let rename (Dir a) (Dir b) = Eio.Path.rename a b
let with_open_out d f = match d with Dir p ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) p f
let load (Dir p) = Eio.Path.load p
let size (Dir p) = Optint.Int63.to_int64 (Eio.Path.stat ~follow:false p).size
let open_in ~sw (Dir p) = Eio.Path.open_in ~sw p
let sync_directory (Dir p) =
  let native = Eio.Path.native_exn p in
  Eio_unix.run_in_systhread ~label:"imap-maildir-fsync" (fun () ->
    let fd = Unix.openfile native [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then
        fail "expected directory";
      Unix.fsync fd))

let ensure_directory (Dir p as d) =
  if kind d = `Not_found then (
    Eio.Path.mkdir ~perm:0o700 p;
    sync_directory (Dir Eio.Path.(p / "..")))
  else if not (is_directory d) then fail "non-directory path"

let open_dir root =
  ignore (Eio.Path.native_exn root : string);
  ensure_directory (Dir root);
  let tmp = Dir Eio.Path.(root / "tmp") in
  let new_dir = Dir Eio.Path.(root / "new") in
  let cur = Dir Eio.Path.(root / "cur") in
  List.iter (fun name ->
    let old=Dir Eio.Path.(root / name) in
    match kind old with
    | `Not_found -> ()
    | `Directory when read_dir old=[] -> ()
    | _ -> fail ("legacy " ^ name ^ " metadata requires offline migration"))
    [".imap-flags";".imap-dates"];
  List.iter ensure_directory [tmp; new_dir; cur];
  {root=Dir root;tmp;new_dir;cur}

let with_writer_lock t f =
  let native = match t.root with Dir p -> Eio.Path.native_exn p in
  let root = Unix.realpath native in
  let root_stat = Unix.stat root in
  let key = root_stat.Unix.st_dev, root_stat.Unix.st_ino in
  reserve_writer key root;
  Fun.protect ~finally:(fun () -> release_writer key) (fun () ->
    let lock_path = Filename.concat root ".imap-writer.lock" in
    let fd = Unix.openfile lock_path
      [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o600 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let stat = Unix.fstat fd in
      let entry = Unix.lstat lock_path in
      if stat.Unix.st_kind <> Unix.S_REG || stat.Unix.st_nlink <> 1 ||
         entry.Unix.st_kind <> Unix.S_REG ||
         stat.Unix.st_dev <> entry.Unix.st_dev ||
         stat.Unix.st_ino <> entry.Unix.st_ino then
        fail "writer lock must be a singly linked regular file";
      (try Unix.lockf fd Unix.F_TLOCK 0 with
       | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
         raise (Writer_lock_busy root));
      f ()))

let random_id () =
  Eio_unix.run_in_systhread ~label:"imap-maildir-random" (fun () ->
    let fd=Unix.openfile "/dev/urandom" [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let b=Bytes.create 16 in
      let rec read offset =
        if offset < 16 then match Unix.read fd b offset (16-offset) with
        | 0 -> fail "entropy source ended"
        | n -> read (offset+n) in
      read 0;
      let x=Buffer.create 32 in
      Bytes.iter (fun c -> Buffer.add_string x (Printf.sprintf "%02x" (Char.code c))) b;
      Buffer.contents x))

let valid_hex s =
  String.length s=32 && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) s
let random_occurrence_id () = "im-" ^ random_id ()
let reserve_id () = random_occurrence_id ()
let valid_supplied_id s =
  String.length s=35 && String.sub s 0 3="im-" &&
  valid_hex (String.sub s 3 32)
let index_sub s needle =
  let rec go i =
    if i+String.length needle>String.length s then None
    else if String.sub s i (String.length needle)=needle then Some i
    else go (i+1) in go 0
let id_of_filename name =
  let base=match index_sub name ":2," with
    | None -> name | Some i -> String.sub name 0 i in
  let base=match index_sub base ".r" with
    | Some i when i=35 && valid_supplied_id (String.sub base 0 i) &&
        String.length base=i+34 &&
        valid_hex (String.sub base (i+2) 32) -> String.sub base 0 i
    | _ -> base in
  if base="" || String.contains base '/' || String.contains base ':' ||
     String.contains base '\000' then None else Some base
let split_filename name =
  match id_of_filename name with
  | None -> None
  | Some id ->
    let letters=match index_sub name ":2," with
      | None -> ""
      | Some i ->
          let rest=String.sub name (i+3) (String.length name-i-3) in
          match String.index_opt rest ',' with
          | None -> rest | Some stop -> String.sub rest 0 stop in
    Some (id,letters)

let with_metadata_lock t f =
  let Dir path=child t.root "dovecot-uidlist.lock" in
  Dotlock.with_lock (Eio.Path.native_exn path) f

let read_keywords t =
  let path=child t.root "dovecot-keywords" in
  let raw=match kind path with
    | `Not_found -> ""
    | `Regular_file when size path<=Int64.of_int Keywords.max_size ->
        load path
    | _ -> fail "invalid or oversized dovecot-keywords" in
  Keywords.parse raw

let ensure_keywords t flags =
  let before=read_keywords t in
  let after=Keywords.add before flags in
  if not (Keywords.equal before after) then (
    let temporary=child t.tmp (".tmp-" ^ random_id ()) in
    let owned=ref false in
    Fun.protect ~finally:(fun () -> if !owned then Eio.Cancel.protect (fun () ->
      unlink ~missing_ok:true temporary)) (fun () ->
      with_open_out temporary (fun output ->
        owned:=true;
        Eio.Flow.copy_string (Keywords.encode after) output;
        Eio.File.sync output);
      rename temporary (child t.root "dovecot-keywords");
      sync_directory t.root;
      sync_directory t.tmp));
  after

let filename ?passed mapping base flags =
  base ^ ":2," ^ Keywords.letters ?passed mapping flags

let date_of_mtime mtime =
  let seconds=Float.floor mtime in
  if not (Float.is_finite seconds) || seconds < -1e12 || seconds > 1e12 then
    fail "Maildir modification time is outside the supported range";
  match Imap.Internal_date.of_unix_seconds (Int64.of_float seconds) with
  | Ok date -> date | Error message -> fail message

let dir t = function New -> t.new_dir | Cur -> t.cur
let message_path t occurrence = child (dir t occurrence.location) occurrence.filename
let one_with_keywords t mapping location name =
  match split_filename name with
  | None -> None
  | Some (id,letters) ->
    let flags=Keywords.flags mapping ~file:name letters in
    let p=child (dir t location) name in
    if kind p = `Not_found then None
    else if kind p <> `Regular_file then fail "non-regular Maildir entry"
    else
    let stat=match p with Dir path -> Eio.Path.stat ~follow:false path in
    let length=Optint.Int63.to_int64 stat.size in
    Some {id;filename=name;location;length;mtime=stat.mtime;inode=stat.ino;ctime=stat.ctime;
          flags;
          internal_date=Some (date_of_mtime stat.mtime)}
let one t = one_with_keywords t (read_keywords t)
let scan t = with_metadata_lock t (fun refresh ->
  let visits=ref 0 in
  let touch () = incr visits; if !visits mod 256=0 then refresh () in
  let mapping=read_keywords t in
  let gather location = read_dir (dir t location) |>
    List.filter_map (fun name ->
      touch ();
      match one_with_keywords t mapping location name with
      | Some x -> Some x
      | None ->
        if kind (child (dir t location) name)=`Regular_file then
          fail "unrecognized Maildir filename";
        None) in
  let all=gather New @ gather Cur in
  let ids=Hashtbl.create (List.length all) in
  List.iter (fun o ->
    if Hashtbl.mem ids o.id then fail "duplicate occurrence identity";
    Hashtbl.add ids o.id ()) all;
  List.sort (fun a b -> String.compare a.id b.id) all)

let find t ~id = List.find_opt (fun o -> o.id=id) (scan t)

type inventory = { occurrences : occurrence list; complete : bool }
let inventory t = {occurrences=scan t;complete=true}

type paged_inventory = {
  db : Db.t;
  count : int64;
  owner : t;
  appended_ids : (string,unit) Hashtbl.t;
  mutable live : bool;
}

let check_live view = if not view.live then invalid_arg "expired Maildir inventory"
let check_inventory t view =
  check_live view;
  if view.owner != t then
    invalid_arg "Imap_maildir: inventory belongs to another handle"
type inventory_page = { occurrences : occurrence list; next_after : string option }
let inventory_count x = check_live x; x.count

let db_check = Sql.Rc.check
let db_exec db sql = db_check (Db.exec db sql)
let with_stmt db sql f =
  let stmt=Eio.Cancel.protect (fun () -> Db.prepare db sql) in
  Fun.protect ~finally:(fun () -> Eio.Cancel.protect (fun () -> db_check (Db.finalize db stmt)))
    (fun () -> f stmt)
let bind stmt values = db_check (Sql.bind_values stmt values)
let data_text = function Sql.Data.TEXT s -> s | _ -> fail "invalid inventory text"
let data_text_opt = function Sql.Data.TEXT s -> Some s
  | Sql.Data.NULL -> None | _ -> fail "invalid inventory date"
let data_int = function Sql.Data.INT n -> n | _ -> fail "invalid inventory integer"
let data_float = function Sql.Data.FLOAT n -> n | _ -> fail "invalid inventory mtime"
let row_text stmt n = data_text (Sql.column stmt n)
let row_int stmt n = data_int (Sql.column stmt n)
let row_float stmt n = data_float (Sql.column stmt n)

let iter_directory_batched directory f =
  let Dir path=directory in
  let native=Eio.Path.native_exn path in
  let handle=Eio.Cancel.protect (fun () ->
    Eio_unix.run_in_systhread ~label:"imap-maildir-opendir"
      (fun () -> Unix.opendir native)) in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () ->
      Eio_unix.run_in_systhread ~label:"imap-maildir-closedir"
        (fun () -> Unix.closedir handle))) (fun () ->
    let rec loop () =
      let names,finished=Eio_unix.run_in_systhread
        ~label:"imap-maildir-readdir" (fun () ->
        let rec take n acc =
          if n=0 then List.rev acc,false else
          match Unix.readdir handle with
          | "." | ".." -> take n acc
          | name -> take (n-1) (name::acc)
          | exception End_of_file -> List.rev acc,true in
        take 256 []) in
      List.iter f names;
      if not finished then loop () in
    loop ())

let staged_flags view id = with_stmt view.db
  "SELECT wire FROM flags WHERE id=? ORDER BY ord" (fun stmt ->
    bind stmt [Sql.Data.TEXT id];
    let rec collect acc=match Db.step view.db stmt with
      | Sql.Rc.ROW ->
        let wire=row_text stmt 0 in
        let flag=match Mail_flag.Imap_flag.of_wire wire with
          | Ok flag -> flag | Error _ -> fail "invalid staged flag" in
        collect (flag::acc)
      | Sql.Rc.DONE -> List.rev acc
      | rc -> db_check rc; assert false in
    collect [])

let staged_occurrence view (id,filename,location,length,mtime,date,inode,ctime) =
  let location=match location with 0L -> New | 1L -> Cur
    | _ -> fail "invalid staged location" in
  let internal_date=Option.map (fun raw ->
    match Imap.Internal_date.of_string raw with
    | Ok date -> date | Error _ -> fail "invalid staged date") date in
  {id;filename;location;length;mtime;inode;ctime;
   flags=staged_flags view id;internal_date}

let inventory_find view ~id =
  check_live view;
  with_stmt view.db
    "SELECT id,filename,location,length,mtime,internal_date,inode,ctime FROM occurrences WHERE id=?"
    (fun stmt ->
      bind stmt [Sql.Data.TEXT id];
      match Db.step view.db stmt with
      | Sql.Rc.ROW -> Some (staged_occurrence view
          (row_text stmt 0,row_text stmt 1,row_int stmt 2,row_int stmt 3,
           row_float stmt 4,data_text_opt (Sql.column stmt 5),row_int stmt 6,row_float stmt 7))
      | Sql.Rc.DONE -> None
      | rc -> db_check rc; assert false)

let inventory_page view ?after ~limit () =
  check_live view;
  if limit<=0 then invalid_arg "Imap_maildir.inventory_page: limit must be positive";
  if limit=max_int then invalid_arg "Imap_maildir.inventory_page: limit too large";
  let sql="SELECT id,filename,location,length,mtime,internal_date,inode,ctime FROM occurrences WHERE id > ? \
    ORDER BY id LIMIT ?" in
  let boundary=Option.value ~default:"" after in
  let raw=with_stmt view.db sql (fun stmt ->
    bind stmt [Sql.Data.TEXT boundary;Sql.Data.INT (Int64.of_int (limit+1))];
    let rec collect acc = match Db.step view.db stmt with
      | Sql.Rc.ROW ->
        let row=(row_text stmt 0,row_text stmt 1,row_int stmt 2,row_int stmt 3,
          row_float stmt 4,data_text_opt (Sql.column stmt 5),row_int stmt 6,row_float stmt 7) in
        collect (row::acc)
      | Sql.Rc.DONE -> List.rev acc
      | rc -> db_check rc; assert false in
    collect []) in
  let has_more=List.length raw>limit in
  let raw=if has_more then List.filteri (fun i _ -> i<limit) raw else raw in
  let occurrences=List.map (staged_occurrence view) raw in
  let next_after=if has_more then
    Option.map (fun x -> x.id) (List.nth_opt occurrences (List.length occurrences-1))
    else None in
  {occurrences;next_after}

let with_inventory_pages t f =
  let name=".inventory-" ^ random_id () ^ ".sqlite3" in
  let path=child t.tmp name in
  let Dir db_path=path in
  let owned=ref false in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () ->
      if !owned then (
        unlink ~missing_ok:true path;
        sync_directory t.tmp))) (fun () ->
    with_open_out path (fun _ -> owned:=true);
    Eio.Switch.run (fun sw ->
      let db=Db.open_path ~sw db_path in
      db_exec db "PRAGMA journal_mode=OFF";
      db_exec db "PRAGMA synchronous=OFF";
      db_exec db "PRAGMA cache_size=-2048";
      db_exec db "CREATE TABLE occurrences \
        (id TEXT PRIMARY KEY,filename TEXT NOT NULL,location INTEGER NOT NULL,\
         length INTEGER NOT NULL,mtime REAL NOT NULL,internal_date TEXT,inode INTEGER NOT NULL,ctime REAL NOT NULL)";
      db_exec db "CREATE TABLE flags \
        (id TEXT NOT NULL,ord INTEGER NOT NULL,wire TEXT NOT NULL,\
         PRIMARY KEY(id,ord))";
      let inserted=ref 0L in
      with_metadata_lock t (fun refresh ->
        let visits=ref 0 in
        let touch () = incr visits; if !visits mod 256=0 then refresh () in
        let mapping=read_keywords t in
        db_exec db "BEGIN";
        with_stmt db "INSERT INTO occurrences(id,filename,location,length,mtime,internal_date,inode,ctime) \
          VALUES (?,?,?,?,?,?,?,?)" (fun row_stmt ->
          with_stmt db "INSERT INTO flags(id,ord,wire) VALUES (?,?,?)"
            (fun flag_stmt ->
            let stage location =
              iter_directory_batched (dir t location) (fun name ->
                touch ();
                match one_with_keywords t mapping location name with
                | None ->
                  if kind (child (dir t location) name)=`Regular_file then
                    fail "unrecognized Maildir filename"
                | Some o ->
                  bind row_stmt [Sql.Data.TEXT o.id;
                    Sql.Data.TEXT o.filename;
                    Sql.Data.INT (if location=New then 0L else 1L);
                    Sql.Data.INT o.length;
                    Sql.Data.FLOAT o.mtime;
                    (match o.internal_date with
                     | Some date -> Sql.Data.TEXT
                         (Imap.Internal_date.to_string date)
                     | None -> Sql.Data.NULL);
                    Sql.Data.INT o.inode; Sql.Data.FLOAT o.ctime];
                  (match Db.step db row_stmt with
                  | Sql.Rc.DONE -> ()
                  | Sql.Rc.CONSTRAINT ->
                    ignore (Db.reset db row_stmt : Sql.Rc.t);
                    fail "duplicate occurrence identity"
                  | rc -> db_check rc);
                  db_check (Db.reset db row_stmt);
                  List.iteri (fun ord flag ->
                    bind flag_stmt [Sql.Data.TEXT o.id;
                      Sql.Data.INT (Int64.of_int ord);
                      Sql.Data.TEXT (Mail_flag.Imap_flag.to_wire flag)];
                    db_check (Db.step db flag_stmt);
                    db_check (Db.reset db flag_stmt)) o.flags;
                  inserted:=Int64.succ !inserted) in
            stage New;
            stage Cur));
        db_exec db "COMMIT");
      let view={db;count= !inserted;owner=t;live=true;
                appended_ids=Hashtbl.create 32} in
      Fun.protect ~finally:(fun () -> view.live<-false) (fun () -> f view)))

let one_current ?inventory t o =
  match inventory with
  | None -> one t o.location o.filename
  | Some view ->
      check_inventory t view;
      one t o.location o.filename

let same_occurrence a b =
  let same_date = match a.internal_date,b.internal_date with
    | None,None -> true
    | Some a,Some b -> Imap.Internal_date.equal_instant a b
    | _ -> false in
  a.id=b.id && a.filename=b.filename && a.location=b.location &&
  a.length=b.length && a.mtime=b.mtime && a.inode=b.inode && a.ctime=b.ctime && same_date &&
  Mail_flag.Imap_flag.equal_durable a.flags b.flags

let require_current ?inventory t o =
  match one_current ?inventory t o with
  | Some actual when same_occurrence actual o -> ()
  | _ -> raise Stale_occurrence

let with_unchanged_occurrence ?inventory t occurrence f =
  let current () =
    match one_current ?inventory t occurrence with
    | Some actual -> same_occurrence actual occurrence
    | None -> false in
  if not (current ()) then Error `Changed
  else
    let value =
      try Ok (f ()) with
      | (Stale_occurrence | End_of_file |
          Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _)) as exn ->
          if current () then raise exn else Error `Changed in
    match value with
    | Error _ as error -> error
    | Ok value -> if current () then Ok value else Error `Changed

let upload_internal_date occurrence =
  match occurrence.internal_date with
  | Some date -> Ok date
  | None ->
      let seconds=Float.floor occurrence.mtime in
      if not (Float.is_finite seconds) ||
         seconds < -1e12 || seconds > 1e12 then
        Error "Maildir modification time is outside the supported range"
      else Imap.Internal_date.of_unix_seconds (Int64.of_float seconds)

let append ?inventory t ?id ~source ~length ~flags ?internal_date () =
  if length<0L then invalid_arg "Imap_maildir.append: negative length";
  Keywords.validate_flags flags;
  let id=match id with
    | None -> reserve_id ()
    | Some id when valid_supplied_id id -> id
    | Some _ -> invalid_arg "Imap_maildir.append: invalid occurrence ID" in
  let duplicate=match inventory with
    | None -> find t ~id <> None
    | Some view ->
        check_inventory t view;
        Hashtbl.mem view.appended_ids id ||
        inventory_find view ~id <> None in
  if duplicate then fail "occurrence ID already published";
  let tmpname=".tmp-" ^ random_id () in
  let temporary=child t.tmp tmpname in
  let timestamp=Option.map (fun date ->
    match Imap.Internal_date.to_unix_seconds date with
    | Ok seconds -> Int64.to_float seconds | Error message -> fail message) internal_date in
  with_metadata_lock t (fun _ -> ignore (ensure_keywords t flags));
  let created=ref false in
  let moved=ref false in
  Fun.protect ~finally:(fun () ->
    if !created && not !moved then Eio.Cancel.protect (fun () ->
      unlink ~missing_ok:true temporary))
    (fun () ->
      with_open_out temporary (fun output ->
        created:=true;
        let buffer=Cstruct.create 65536 in
        let rec copy remaining = if remaining>0L then (
          let n=Int64.to_int (Int64.min remaining 65536L) in
          let chunk=Cstruct.sub buffer 0 n in
          Eio.Flow.read_exact source chunk;
          Eio.Flow.write output [chunk];
          copy (Int64.sub remaining (Int64.of_int n))) in
        copy length;
        Option.iter (fun timestamp ->
          let Dir temp_path=temporary in
          Eio_unix.run_in_systhread (fun () ->
            let native=Eio.Path.native_exn temp_path in
            Unix.utimes native timestamp timestamp;
            if (Unix.stat native).Unix.st_mtime<>timestamp then
              fail "filesystem cannot represent INTERNALDATE")) timestamp;
        Eio.File.sync output);
      sync_directory t.tmp;
      with_metadata_lock t (fun touch ->
        let mapping=ensure_keywords t flags in
        let name=if flags=[] then id else filename mapping id flags in
        let location=if flags=[] then New else Cur in
        let target=child (dir t location) name in
        if kind target <> `Not_found then fail "message target already exists";
        touch ();
        rename temporary target;
        moved:=true;
        sync_directory (dir t location);
        sync_directory t.tmp;
        Option.iter (fun view -> Hashtbl.replace view.appended_ids id ()) inventory;
        match one_with_keywords t mapping location name with
        | Some occurrence -> occurrence
        | None -> fail "published Maildir message vanished"))

let open_message ?inventory t ~sw occurrence =
  require_current ?inventory t occurrence;
  open_in ~sw (message_path t occurrence)

let sha256 ?inventory t occurrence =
  require_current ?inventory t occurrence;
  let Dir p=message_path t occurrence in
  Eio.Path.with_open_in p (fun input ->
    let buffer=Cstruct.create 65536 in
    let rec loop remaining hash =
      if remaining=0L then hash else
      let n=Int64.to_int (Int64.min remaining 65536L) in
      let chunk=Cstruct.sub buffer 0 n in
      Eio.Flow.read_exact input chunk;
      loop (Int64.sub remaining (Int64.of_int n))
        (Digestif.SHA256.feed_string hash (Cstruct.to_string chunk)) in
    let digest=loop occurrence.length Digestif.SHA256.empty in
    (try ignore (Eio.Flow.single_read input (Cstruct.create 1));
         raise Stale_occurrence
     with End_of_file -> ());
    Digestif.SHA256.to_hex (Digestif.SHA256.get digest))

let set_flags t occurrence flags =
  Keywords.validate_flags flags;
  with_metadata_lock t (fun touch ->
    require_current t occurrence;
    let mapping=ensure_keywords t flags in
    let base,suffix=match index_sub occurrence.filename ":2," with
      | None -> occurrence.filename,""
      | Some i ->
          let rest=String.sub occurrence.filename (i+3)
            (String.length occurrence.filename-i-3) in
          let suffix=match String.index_opt rest ',' with
            | None -> "" | Some j -> String.sub rest j (String.length rest-j) in
          String.sub occurrence.filename 0 i,suffix in
    let _,old_letters=Option.get (split_filename occurrence.filename) in
    let passed=String.contains old_letters 'P' in
    let name=filename ~passed mapping base flags ^ suffix in
    if occurrence.location=Cur && occurrence.filename=name then occurrence else (
      let target=child t.cur name in
      if kind target<>`Not_found then fail "flag target already exists";
      touch ();
      rename (message_path t occurrence) target;
      sync_directory t.cur;
      if occurrence.location=New then sync_directory t.new_dir;
      match one_with_keywords t mapping Cur name with
      | Some current -> current | None -> fail "renamed message vanished"))

let remove t occurrence = with_metadata_lock t (fun touch ->
  require_current t occurrence;
  touch ();
  unlink (message_path t occurrence);
  sync_directory (dir t occurrence.location))

let recover t =
  let is_inventory_file s =
    String.length s=51 && String.sub s 0 11=".inventory-" &&
    valid_hex (String.sub s 11 32) && String.sub s 43 8=".sqlite3" in
  let temporaries=read_dir t.tmp |>
    List.filter (fun s ->
      (String.length s=37 && String.sub s 0 5=".tmp-"
       && valid_hex (String.sub s 5 32)) || is_inventory_file s) in
  List.iter (fun name -> unlink (child t.tmp name)) temporaries;
  if temporaries<>[] then sync_directory t.tmp;
  {removed_temporary=temporaries}
