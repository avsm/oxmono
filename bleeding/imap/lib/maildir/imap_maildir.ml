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
type keywords_key = int64 * Optint.Int63.t * float * float
type t = { root : directory; tmp : directory; new_dir : directory;
           cur : directory;
           mutable keywords : (keywords_key * Keywords.t) option }
type recovery = { removed_temporary : string list }
exception Writer_lock_busy = Dotlock.Busy
exception Metadata_lock_lost = Dotlock.Lost
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
let native (Dir p) = Eio.Path.native_exn p
let unlink ?(missing_ok=false) (Dir p) = Eio.Path.unlink ~missing_ok p
let rename (Dir a) (Dir b) = Eio.Path.rename a b
let with_open_out d f = match d with Dir p ->
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) p f
let open_in ~sw (Dir p) = Eio.Path.open_in ~sw p

(* Cleanup must not replace the exception that triggered it. [recover]
   removes whatever is left behind. *)
let discard d =
  Eio.Cancel.protect (fun () ->
    try unlink ~missing_ok:true d with Eio.Io _ -> ())

let in_systhread ~label context f =
  match Eio_unix.run_in_systhread ~label (fun () ->
    try f () with Unix.Unix_error (code,name,arg) ->
      raise (Eio_unix.Err.v code name arg)) with
  | value -> value
  | exception (Eio.Io _ as exn) ->
      Eio.Exn.reraise_with_context exn (Printexc.get_raw_backtrace ())
        "%s" context

let sync_directory d =
  let native = native d in
  (* Eio has no directory fsync. *)
  in_systhread ~label:"imap-maildir-fsync" ("syncing " ^ native) (fun () ->
    let fd = Unix.openfile native [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then
        fail ("expected directory " ^ native);
      Unix.fsync fd))

(* Eio has no hard link. Unlike rename, link refuses to replace an existing
   target, so a concurrent writer that ignores the lock is never clobbered. *)
let link source target =
  let source = native source and target = native target in
  in_systhread ~label:"imap-maildir-link"
    (Printf.sprintf "linking %s to %s" source target)
    (fun () -> Unix.link source target)

let ensure_directory (Dir p as d) =
  if kind d = `Not_found then (
    Eio.Path.mkdir ~perm:0o700 p;
    sync_directory (Dir Eio.Path.(p / "..")))
  else if not (is_directory d) then fail ("non-directory path " ^ native d)

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
  {root=Dir root;tmp;new_dir;cur;keywords=None}

let with_writer_lock t f =
  let Dir root=t.root in
  let name = Eio.Path.native_exn root in
  let root_stat = Eio.Path.stat ~follow:true root in
  let key = root_stat.dev, root_stat.ino in
  reserve_writer key name;
  Fun.protect ~finally:(fun () -> release_writer key) (fun () ->
    let path = Eio.Path.(root / ".imap-writer.lock") in
    Eio.Switch.run ~name:"imap-maildir-writer" (fun sw ->
      let file = Eio.Path.open_out ~sw ~create:(`If_missing 0o600) path in
      let stat = Eio.File.stat file in
      let entry = Eio.Path.stat ~follow:false path in
      if stat.kind <> `Regular_file || stat.nlink <> 1L ||
         entry.kind <> `Regular_file ||
         stat.dev <> entry.dev || stat.ino <> entry.ino then
        fail ("writer lock in " ^ name ^
              " must be a singly linked regular file");
      let fd = match Eio_unix.Resource.fd_opt file with
        | Some fd -> fd
        | None -> fail ("writer lock in " ^ name ^ " has no descriptor") in
      (* Eio has no record locks. *)
      (try Eio_unix.Fd.use_exn "lockf" fd (fun fd ->
         Unix.lockf fd Unix.F_TLOCK 0) with
       | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
         raise (Writer_lock_busy name));
      f ()))

let random_id () =
  (* Eio reaches an entropy source only through the environment, which
     [reserve_id] does not take. *)
  in_systhread ~label:"imap-maildir-random" "reading /dev/urandom" (fun () ->
    let fd=Unix.openfile "/dev/urandom" [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let b=Bytes.create 16 in
      let rec read offset =
        if offset < 16 then match Unix.read fd b offset (16-offset) with
        | 0 -> fail "entropy source ended"
        | n -> read (offset+n) in
      read 0;
      let x=Buffer.create 32 in
      Bytes.iter (fun c ->
        Buffer.add_string x (Printf.sprintf "%02x" (Char.code c))) b;
      Buffer.contents x))

let valid_hex s =
  String.length s=32 && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) s
let reserve_id () = "im-" ^ random_id ()
let valid_supplied_id s =
  String.length s=35 && String.starts_with ~prefix:"im-" s &&
  valid_hex (String.sub s 3 32)

type name = {
  ident : string; base : string; letters : string; suffix : string }

let parse_name filename =
  let length=String.length filename in
  let base,letters,suffix=match String.index_opt filename ':' with
    | Some i when i+3<=length && filename.[i+1]='2' && filename.[i+2]=',' ->
        let stop=Option.value ~default:length
          (String.index_from_opt filename (i+3) ',') in
        String.sub filename 0 i,
        String.sub filename (i+3) (stop-i-3),
        String.sub filename stop (length-stop)
    | _ -> filename,"","" in
  (* Step 4 removes the [.r<32hex>] alias, which nothing in this tree
     writes. *)
  let id=
    if String.length base=69 && String.sub base 35 2=".r" &&
       valid_supplied_id (String.sub base 0 35) &&
       valid_hex (String.sub base 37 32)
    then String.sub base 0 35 else base in
  if id="" || String.contains id '/' || String.contains id ':' ||
     String.contains id '\000' then None
  else Some {ident=id;base;letters;suffix}

let visible name = name<>"" && name.[0]<>'.'

let with_metadata_lock t f =
  let Dir path=child t.root "dovecot-uidlist.lock" in
  Dotlock.with_lock path f

let read_keywords t =
  let Dir p=child t.root "dovecot-keywords" in
  match Eio.Path.stat ~follow:false p with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Keywords.empty
  | stat when stat.kind=`Regular_file &&
              Optint.Int63.to_int stat.size<=Keywords.max_size ->
      let key=stat.ino,stat.size,stat.mtime,stat.ctime in
      (match t.keywords with
       | Some (cached,mapping) when cached=key -> mapping
       | _ ->
           let mapping=Keywords.parse (Eio.Path.load p) in
           t.keywords<-Some (key,mapping);
           mapping)
  | stat ->
      fail (Format.asprintf "dovecot-keywords is a %a of %s bytes"
        Eio.File.Stat.pp_kind stat.kind (Optint.Int63.to_string stat.size))

let ensure_keywords t flags =
  let before=read_keywords t in
  let after=Keywords.add before flags in
  if not (Keywords.equal before after) then (
    let temporary=child t.tmp (".tmp-" ^ random_id ()) in
    let owned=ref false in
    Fun.protect ~finally:(fun () -> if !owned then discard temporary)
      (fun () ->
      with_open_out temporary (fun output ->
        owned:=true;
        Eio.Flow.copy_string (Keywords.encode after) output;
        Eio.File.sync output);
      rename temporary (child t.root "dovecot-keywords");
      sync_directory t.root));
  after

let has_keywords = List.exists (function
  | Mail_flag.Imap_flag.Keyword _ -> true | _ -> false)

let filename ?passed mapping base flags =
  base ^ ":2," ^ Keywords.letters ?passed mapping flags

let internal_date_of_mtime mtime =
  let seconds=Float.floor mtime in
  if not (Float.is_finite seconds) || seconds < -1e12 || seconds > 1e12 then
    Error (Printf.sprintf
      "Maildir modification time %g is outside the supported range" mtime)
  else Imap.Internal_date.of_unix_seconds (Int64.of_float seconds)

let date_of_mtime mtime =
  match internal_date_of_mtime mtime with
  | Ok date -> date | Error message -> fail message

let dir t = function New -> t.new_dir | Cur -> t.cur
let message_path t occurrence =
  child (dir t occurrence.location) occurrence.filename

let observe t mapping location filename =
  let Dir p=child (dir t location) filename in
  match Eio.Path.stat ~follow:false p with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | stat when stat.kind<>`Regular_file -> None
  | stat ->
      match parse_name filename with
      | None -> fail ("unrecognized Maildir filename " ^ filename)
      | Some name ->
          Some {id=name.ident;filename;location;
                length=Optint.Int63.to_int64 stat.size;
                mtime=stat.mtime;inode=stat.ino;ctime=stat.ctime;
                flags=Keywords.flags (Lazy.force mapping) ~file:filename
                  name.letters;
                internal_date=Some (date_of_mtime stat.mtime)}

let iter_entries t refresh f =
  let visits=ref 0 in
  let mapping=Lazy.from_val (read_keywords t) in
  List.iter (fun location ->
    let Dir p=dir t location in
    Eio.Path.with_dir_entries p (Seq.iter (fun (kind,name) ->
      incr visits;
      if !visits mod 256=0 then refresh ();
      match kind with
      | (`Regular_file | `Unknown) when visible name ->
          Option.iter f (observe t mapping location name)
      | _ -> ()))) [New;Cur]

let duplicate id = fail ("duplicate occurrence identity " ^ id)

let scan t = with_metadata_lock t (fun refresh ->
  let all=ref [] in
  iter_entries t refresh (fun o -> all:=o:: !all);
  let sorted=List.sort (fun a b -> String.compare a.id b.id) !all in
  let rec check = function
    | a::(b::_ as rest) -> if a.id=b.id then duplicate a.id; check rest
    | _ -> () in
  check sorted;
  sorted)

let locate t refresh id =
  match parse_name id with
  | Some name when name.ident=id && visible id ->
      let mapping=lazy (read_keywords t) in
      let found=ref (Option.to_list (observe t mapping New id)) in
      let visits=ref 0 in
      let Dir cur=t.cur in
      Eio.Path.with_dir_entries cur (Seq.iter (fun (kind,name) ->
        incr visits;
        if !visits mod 256=0 then refresh ();
        match kind with
        | (`Regular_file | `Unknown) when String.starts_with ~prefix:id name
            && Option.map (fun n -> n.ident) (parse_name name)=Some id ->
            Option.iter (fun o -> found:=o:: !found)
              (observe t mapping Cur name)
        | _ -> ()));
      !found
  | _ -> []

let find t ~id = with_metadata_lock t (fun refresh ->
  match locate t refresh id with
  | [] -> None
  | [occurrence] -> Some occurrence
  | _ -> duplicate id)

type paged_inventory = {
  db : Db.t;
  count : int64;
  owner : t;
  appended_ids : (string,unit) Hashtbl.t;
  mutable live : bool;
}

let check_live view =
  if not view.live then invalid_arg "Imap_maildir: expired inventory"
let check_inventory t view =
  check_live view;
  if view.owner != t then
    invalid_arg "Imap_maildir: inventory belongs to another handle"
type inventory_page = {
  occurrences : occurrence list;
  next_after : string option;
}
let inventory_count x = check_live x; x.count

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

let columns =
  "id,filename,location,length,mtime,internal_date,inode,ctime,flags"

let encode_flags flags =
  String.concat " " (List.map Mail_flag.Imap_flag.to_wire flags)
let decode_flags raw =
  String.split_on_char ' ' raw |> List.filter_map (function
    | "" -> None
    | wire ->
        match Mail_flag.Imap_flag.of_wire wire with
        | Ok flag -> Some flag
        | Error _ -> fail ("invalid staged flag " ^ wire))

let staged_occurrence stmt =
  let column n = Sql.column stmt n in
  let text n = match column n with
    | Sql.Data.TEXT s -> s
    | _ -> fail (Printf.sprintf "invalid inventory text in column %d" n) in
  let int n = match column n with
    | Sql.Data.INT i -> i
    | _ -> fail (Printf.sprintf "invalid inventory integer in column %d" n) in
  let float n = match column n with
    | Sql.Data.FLOAT f -> f
    | _ -> fail (Printf.sprintf "invalid inventory real in column %d" n) in
  let location=match int 2 with
    | 0L -> New | 1L -> Cur
    | n -> fail (Printf.sprintf "invalid staged location %Ld" n) in
  (* Staged dates are never NULL until step 4 removes [internal_date]. *)
  let internal_date=match column 5 with
    | Sql.Data.NULL -> None
    | Sql.Data.TEXT raw ->
        (match Imap.Internal_date.of_string raw with
         | Ok date -> Some date
         | Error _ -> fail ("invalid staged date " ^ raw))
    | _ -> fail "invalid inventory date" in
  {id=text 0;filename=text 1;location;length=int 3;mtime=float 4;
   internal_date;inode=int 6;ctime=float 7;flags=decode_flags (text 8)}

let inventory_find view ~id =
  check_live view;
  with_stmt view.db
    ("SELECT " ^ columns ^ " FROM occurrences WHERE id=?") (fun stmt ->
    bind stmt [Sql.Data.TEXT id];
    if step view.db stmt then Some (staged_occurrence stmt) else None)

let inventory_page view ?after ~limit () =
  check_live view;
  if limit<=0 then
    invalid_arg "Imap_maildir.inventory_page: limit must be positive";
  if limit=max_int then
    invalid_arg "Imap_maildir.inventory_page: limit too large";
  let sql="SELECT " ^ columns ^
    " FROM occurrences WHERE id > ? ORDER BY id LIMIT ?" in
  let boundary=Option.value ~default:"" after in
  with_stmt view.db sql (fun stmt ->
    bind stmt [Sql.Data.TEXT boundary;Sql.Data.INT (Int64.of_int (limit+1))];
    let rec collect n acc =
      if not (step view.db stmt) then
        {occurrences=List.rev acc;next_after=None}
      else if n=limit then
        {occurrences=List.rev acc;
         next_after=Option.map (fun o -> o.id) (List.nth_opt acc 0)}
      else collect (n+1) (staged_occurrence stmt::acc) in
    collect 0 [])

let with_inventory_pages t f =
  let path=child t.tmp (".inventory-" ^ random_id () ^ ".sqlite3") in
  let Dir db_path=path in
  let owned=ref false in
  Fun.protect ~finally:(fun () -> if !owned then discard path) (fun () ->
    with_open_out path (fun _ -> owned:=true);
    Eio.Switch.run (fun sw ->
      let db=Db.open_path ~sw db_path in
      db_exec db "PRAGMA journal_mode=OFF";
      db_exec db "PRAGMA synchronous=OFF";
      db_exec db "PRAGMA cache_size=-2048";
      db_exec db "CREATE TABLE occurrences \
        (id TEXT PRIMARY KEY,filename TEXT NOT NULL,\
         location INTEGER NOT NULL,length INTEGER NOT NULL,\
         mtime REAL NOT NULL,internal_date TEXT,inode INTEGER NOT NULL,\
         ctime REAL NOT NULL,flags TEXT NOT NULL)";
      let count=with_metadata_lock t (fun refresh ->
        db_exec db "BEGIN";
        let count=ref 0L in
        with_stmt db ("INSERT INTO occurrences(" ^ columns ^
          ") VALUES (?,?,?,?,?,?,?,?,?)") (fun stmt ->
          iter_entries t refresh (fun o ->
            bind stmt [Sql.Data.TEXT o.id;
              Sql.Data.TEXT o.filename;
              Sql.Data.INT (if o.location=New then 0L else 1L);
              Sql.Data.INT o.length;
              Sql.Data.FLOAT o.mtime;
              (match o.internal_date with
               | Some date ->
                   Sql.Data.TEXT (Imap.Internal_date.to_string date)
               | None -> Sql.Data.NULL);
              Sql.Data.INT o.inode;
              Sql.Data.FLOAT o.ctime;
              Sql.Data.TEXT (encode_flags o.flags)];
            (match Db.step db stmt with
             | Sql.Rc.DONE -> ()
             | Sql.Rc.CONSTRAINT ->
                 ignore (Db.reset db stmt : Sql.Rc.t);
                 duplicate o.id
             | rc -> db_check rc);
            db_check (Db.reset db stmt);
            count:=Int64.succ !count));
        db_exec db "COMMIT";
        !count) in
      let view={db;count;owner=t;live=true;appended_ids=Hashtbl.create 32} in
      Fun.protect ~finally:(fun () -> view.live<-false) (fun () -> f view)))

let current ?inventory t o =
  Option.iter (check_inventory t) inventory;
  observe t (lazy (read_keywords t)) o.location o.filename

let same_occurrence a b =
  (* Observations always carry a date until step 4 removes the field. *)
  let same_date = match a.internal_date,b.internal_date with
    | None,None -> true
    | Some a,Some b -> Imap.Internal_date.equal_instant a b
    | _ -> false in
  a.id=b.id && a.filename=b.filename && a.location=b.location &&
  a.length=b.length && a.mtime=b.mtime && a.inode=b.inode &&
  a.ctime=b.ctime && same_date &&
  Mail_flag.Imap_flag.equal_durable a.flags b.flags

let require_current ?inventory t o =
  match current ?inventory t o with
  | Some actual when same_occurrence actual o -> ()
  | _ -> raise Stale_occurrence

let with_unchanged_occurrence ?inventory t occurrence f =
  let unchanged () =
    match current ?inventory t occurrence with
    | Some actual -> same_occurrence actual occurrence
    | None -> false in
  if not (unchanged ()) then Error `Changed
  else
    let value =
      try Ok (f ()) with
      | (Stale_occurrence | End_of_file |
          Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _)) as exn ->
          if unchanged () then raise exn else Error `Changed in
    match value with
    | Error _ as error -> error
    | Ok value -> if unchanged () then Ok value else Error `Changed

let upload_internal_date occurrence =
  match occurrence.internal_date with
  | Some date -> Ok date
  (* Unreachable until step 4 removes [internal_date]. *)
  | None -> internal_date_of_mtime occurrence.mtime

let set_mtime (Dir p) output timestamp =
  let native=Eio.Path.native_exn p in
  (* Eio has no utimes. Unix.utimes reads two zero times as the current
     time, so the access time must stay nonzero for an epoch date. *)
  let atime=if timestamp=0. then 1. else timestamp in
  in_systhread ~label:"imap-maildir-utimes" ("setting times of " ^ native)
    (fun () -> Unix.utimes native atime timestamp);
  if (Eio.File.stat output).mtime<>timestamp then
    fail (Printf.sprintf "filesystem cannot represent INTERNALDATE %.0f"
      timestamp)

let published ?inventory t refresh id =
  (match inventory with
   | Some view ->
       Hashtbl.mem view.appended_ids id || inventory_find view ~id<>None
   | None -> false)
  || locate t refresh id<>[]

let append ?inventory t ?id ~source ~length ~flags ?internal_date () =
  if length<0L then invalid_arg "Imap_maildir.append: negative length";
  Keywords.validate_flags flags;
  Option.iter (check_inventory t) inventory;
  let id,supplied=match id with
    | None -> reserve_id (),false
    | Some id when valid_supplied_id id -> id,true
    | Some id ->
        invalid_arg ("Imap_maildir.append: invalid occurrence ID " ^ id) in
  let timestamp=Option.map (fun date ->
    match Imap.Internal_date.to_unix_seconds date with
    | Ok seconds -> Int64.to_float seconds
    | Error message -> fail message) internal_date in
  let keywords=has_keywords flags in
  if keywords then
    with_metadata_lock t (fun _ -> ignore (ensure_keywords t flags));
  let temporary=child t.tmp (".tmp-" ^ random_id ()) in
  let created=ref false in
  match
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
      Option.iter (set_mtime temporary output) timestamp;
      Eio.File.sync output);
    with_metadata_lock t (fun refresh ->
      if supplied && published ?inventory t refresh id then
        fail ("occurrence ID " ^ id ^ " already published");
      let mapping=if keywords then read_keywords t else Keywords.empty in
      let location,name=
        if flags=[] then New,id else Cur,filename mapping id flags in
      refresh ();
      link temporary (child (dir t location) name);
      sync_directory (dir t location);
      (* Unlinking changes the ctime of the published inode, so it precedes
         the observation. *)
      discard temporary;
      Option.iter (fun view -> Hashtbl.replace view.appended_ids id ())
        inventory;
      match observe t (Lazy.from_val mapping) location name with
      | Some occurrence -> occurrence
      | None -> fail ("published Maildir message " ^ name ^ " vanished"))
  with
  | occurrence -> occurrence
  | exception exn ->
      let bt=Printexc.get_raw_backtrace () in
      if !created then discard temporary;
      Printexc.raise_with_backtrace exn bt

let open_message ?inventory t ~sw occurrence =
  require_current ?inventory t occurrence;
  open_in ~sw (message_path t occurrence)

let sha256 ?inventory t occurrence =
  require_current ?inventory t occurrence;
  let Dir p=message_path t occurrence in
  Eio.Path.with_open_in p (fun input ->
    let bytes=Bigarray.(Array1.create char c_layout 65536) in
    let rec loop remaining hash =
      if remaining=0L then hash else
      let n=Int64.to_int (Int64.min remaining 65536L) in
      Eio.Flow.read_exact input (Cstruct.of_bigarray ~len:n bytes);
      loop (Int64.sub remaining (Int64.of_int n))
        (Digestif.SHA256.feed_bigstring hash ~len:n bytes) in
    let digest=loop occurrence.length Digestif.SHA256.empty in
    let probe=Cstruct.of_bigarray ~len:1 bytes in
    (try ignore (Eio.Flow.single_read input probe); raise Stale_occurrence
     with End_of_file -> ());
    Digestif.SHA256.to_hex (Digestif.SHA256.get digest))

let set_flags t occurrence flags =
  Keywords.validate_flags flags;
  with_metadata_lock t (fun refresh ->
    require_current t occurrence;
    let mapping=ensure_keywords t flags in
    let old=Option.get (parse_name occurrence.filename) in
    let passed=String.contains old.letters 'P' in
    let name=filename ~passed mapping old.base flags ^ old.suffix in
    if occurrence.location=Cur && occurrence.filename=name then occurrence
    else (
      let source=message_path t occurrence in
      refresh ();
      link source (child t.cur name);
      sync_directory t.cur;
      unlink source;
      sync_directory (dir t occurrence.location);
      match observe t (Lazy.from_val mapping) Cur name with
      | Some current -> current
      | None -> fail ("renamed Maildir message " ^ name ^ " vanished")))

let remove t occurrence = with_metadata_lock t (fun refresh ->
  require_current t occurrence;
  refresh ();
  unlink (message_path t occurrence);
  sync_directory (dir t occurrence.location))

let recover t =
  let is_inventory_file s =
    String.length s=51 && String.starts_with ~prefix:".inventory-" s &&
    valid_hex (String.sub s 11 32) && String.ends_with ~suffix:".sqlite3" s in
  let is_temporary s =
    String.length s=37 && String.starts_with ~prefix:".tmp-" s &&
    valid_hex (String.sub s 5 32) in
  let temporaries=read_dir t.tmp |>
    List.filter (fun s -> is_temporary s || is_inventory_file s) in
  List.iter (fun name -> unlink (child t.tmp name)) temporaries;
  {removed_temporary=temporaries}
