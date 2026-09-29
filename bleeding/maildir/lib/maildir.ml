module Dotlock = Dotlock
module Keywords = Keywords

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
}
type directory = Dir : _ Eio.Path.t -> directory
type keywords_key = int64 * Optint.Int63.t * float * float
type t = { root : directory; tmp : directory; new_dir : directory;
           cur : directory;
           mutable keywords : (keywords_key * Keywords.t) option }
type writer = { maildir : t; mutable live : bool }
type recovery = { removed_temporary : string list }
type error = Maildir_error.t =
  | Legacy_metadata of string
  | Not_a_directory of string
  | Non_native_path of string
  | Malformed_filename of string
  | Duplicate_identity of string
  | Unknown_letter of { file : string; letter : char }
  | Keyword_map of string
  | Unsupported_flag of Mail_flag.Imap_flag.t
  | Too_many_keywords of Mail_flag.Imap_flag.t
  | Unrepresentable_date of float
  | Target_exists of string
  | Vanished of string

let pp_error = Maildir_error.pp

exception Writer_lock_busy of string
exception Writer_expired
exception Metadata_lock_busy = Dotlock.Busy
exception Metadata_lock_lost = Dotlock.Lost
exception Stale_occurrence

type Eio.Exn.err += Unusable_file of string

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Unusable_file message ->
        Format.fprintf ppf "Maildir: %s" message; true
    | _ -> false)

let unusable message = raise (Eio.Exn.create (Unusable_file message))

exception Invalid of error

let invalid e = raise (Invalid e)
let get = function Ok value -> value | Error e -> invalid e
let catch f = try Ok (f ()) with Invalid e -> Error e

(* POSIX record locks are process-scoped: a second lockf in this process would
   succeed, and closing that second descriptor could release the first lock.
   Reserve the canonical directory before opening any lock-file descriptor. *)
module Inode = struct
  type t = int64 * int64
  let compare (dev, ino) (dev', ino') =
    match Int64.compare dev dev' with 0 -> Int64.compare ino ino' | c -> c
  include (val Base.Comparator.make__portable ~compare
      ~sexp_of_t:(fun (dev, ino) -> Base.Sexp.List
        [ Atom (Int64.to_string dev); Atom (Int64.to_string ino) ]))
end
let writer_locks = Atomic.make (Base.Set.empty (module Inode))
let rec reserve_writer key path =
  let held = Atomic.get writer_locks in
  if Base.Set.mem held key then raise (Writer_lock_busy path);
  if not (Atomic.compare_and_set writer_locks held (Base.Set.add held key))
  then reserve_writer key path
let rec release_writer key =
  let held = Atomic.get writer_locks in
  if not (Atomic.compare_and_set writer_locks held (Base.Set.remove held key))
  then release_writer key

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
  in_systhread ~label:"maildir-fsync" ("syncing " ^ native) (fun () ->
    let fd = Unix.openfile native [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then
        unusable ("expected directory " ^ native);
      Unix.fsync fd))

let ensure_directory (Dir p as d) =
  if kind d = `Not_found then (
    Eio.Path.mkdir ~perm:0o700 p;
    sync_directory (Dir Eio.Path.(p / "..")))
  else if not (is_directory d) then invalid (Not_a_directory (native d))

let open_dir root = catch (fun () ->
  if Eio.Path.native root = None then
    invalid (Non_native_path (Format.asprintf "%a" Eio.Path.pp root));
  ensure_directory (Dir root);
  let tmp = Dir Eio.Path.(root / "tmp") in
  let new_dir = Dir Eio.Path.(root / "new") in
  let cur = Dir Eio.Path.(root / "cur") in
  List.iter (fun name ->
    let old=Dir Eio.Path.(root / name) in
    match kind old with
    | `Not_found -> ()
    | `Directory when read_dir old=[] -> ()
    | _ -> invalid (Legacy_metadata name))
    [".imap-flags";".imap-dates"];
  List.iter ensure_directory [tmp; new_dir; cur];
  {root=Dir root;tmp;new_dir;cur;keywords=None})

let with_writer t f =
  let Dir root=t.root in
  let name = Eio.Path.native_exn root in
  let root_stat = Eio.Path.stat ~follow:true root in
  let key = root_stat.dev, root_stat.ino in
  reserve_writer key name;
  Fun.protect ~finally:(fun () -> release_writer key) (fun () ->
    let path = Eio.Path.(root / ".imap-writer.lock") in
    Eio.Switch.run ~name:"maildir-writer" (fun sw ->
      let file = Eio.Path.open_out ~sw ~create:(`If_missing 0o600) path in
      let stat = Eio.File.stat file in
      let entry = Eio.Path.stat ~follow:false path in
      if stat.kind <> `Regular_file || stat.nlink <> 1L ||
         entry.kind <> `Regular_file ||
         stat.dev <> entry.dev || stat.ino <> entry.ino then
        unusable ("writer lock in " ^ name ^
                  " must be a singly linked regular file");
      let fd = match Eio_unix.Resource.fd_opt file with
        | Some fd -> fd
        | None -> unusable ("writer lock in " ^ name ^ " has no descriptor") in
      (* Eio has no record locks. *)
      (try Eio_unix.Fd.use_exn "lockf" fd (fun fd ->
         Unix.lockf fd Unix.F_TLOCK 0) with
       | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
         raise (Writer_lock_busy name));
      let writer={maildir=t;live=true} in
      Fun.protect ~finally:(fun () -> writer.live<-false) (fun () ->
        f writer)))

let of_writer writer =
  if not writer.live then raise Writer_expired;
  writer.maildir

let random_id () =
  (* Eio reaches an entropy source only through the environment, which
     [reserve_id] does not take. *)
  in_systhread ~label:"maildir-random" "reading /dev/urandom" (fun () ->
    let fd=Unix.openfile "/dev/urandom" [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      let b=Bytes.create 16 in
      let rec read offset =
        if offset < 16 then match Unix.read fd b offset (16-offset) with
        | 0 -> unusable "/dev/urandom ended"
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

type name = { base : string; letters : string; suffix : string }

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
  if base="" || String.contains base '/' || String.contains base ':' ||
     String.contains base '\000' then None
  else Some {base;letters;suffix}

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
           let mapping=get (Keywords.parse (Eio.Path.load p)) in
           t.keywords<-Some (key,mapping);
           mapping)
  | stat ->
      invalid (Keyword_map (Format.asprintf "dovecot-keywords is a %a of %s \
        bytes" Eio.File.Stat.pp_kind stat.kind
        (Optint.Int63.to_string stat.size)))

let ensure_keywords t flags =
  let before=read_keywords t in
  let after=get (Keywords.add before flags) in
  if not (Keywords.equal before after) then (
    let contents=get (Keywords.encode after) in
    let temporary=child t.tmp (".tmp-" ^ random_id ()) in
    let owned=ref false in
    Fun.protect ~finally:(fun () -> if !owned then discard temporary)
      (fun () ->
      with_open_out temporary (fun output ->
        owned:=true;
        Eio.Flow.copy_string contents output;
        Eio.File.sync output);
      rename temporary (child t.root "dovecot-keywords");
      sync_directory t.root));
  after

let has_keywords = List.exists (function
  | Mail_flag.Imap_flag.Keyword _ -> true | _ -> false)

let filename ?passed mapping base flags =
  base ^ ":2," ^ Keywords.letters ?passed mapping flags

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
      | None -> invalid (Malformed_filename filename)
      | Some name ->
          Some {id=name.base;filename;location;
                length=Optint.Int63.to_int64 stat.size;
                mtime=stat.mtime;inode=stat.ino;ctime=stat.ctime;
                flags=get (Keywords.flags (Lazy.force mapping) ~file:filename
                  name.letters)}

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

let fold t ~init ~f = with_metadata_lock t (fun refresh ->
  catch (fun () ->
    let acc=ref init in
    iter_entries t refresh (fun o -> acc:=f !acc o);
    !acc))

let scan t =
  Result.bind (fold t ~init:[] ~f:(fun all o -> o::all)) (fun all ->
    let sorted=List.sort (fun a b -> String.compare a.id b.id) all in
    let rec check = function
      | a::(b::_ as rest) ->
          if a.id=b.id then Error (Duplicate_identity a.id) else check rest
      | _ -> Ok sorted in
    check sorted)

let locate t refresh id =
  match parse_name id with
  | Some name when name.base=id && visible id ->
      let mapping=lazy (read_keywords t) in
      let found=ref (Option.to_list (observe t mapping New id)) in
      let visits=ref 0 in
      let Dir cur=t.cur in
      Eio.Path.with_dir_entries cur (Seq.iter (fun (kind,name) ->
        incr visits;
        if !visits mod 256=0 then refresh ();
        match kind with
        | (`Regular_file | `Unknown) when String.starts_with ~prefix:id name
            && Option.map (fun n -> n.base) (parse_name name)=Some id ->
            Option.iter (fun o -> found:=o:: !found)
              (observe t mapping Cur name)
        | _ -> ()));
      !found
  | _ -> []

let find t ~id = with_metadata_lock t (fun refresh ->
  catch (fun () ->
    match locate t refresh id with
    | [] -> None
    | [occurrence] -> Some occurrence
    | _ -> invalid (Duplicate_identity id)))

(* An observation whose flags can no longer be read cannot be shown to
   match, so it counts as changed. *)
let current t o =
  try observe t (lazy (read_keywords t)) o.location o.filename
  with Invalid _ -> None

let same_occurrence a b =
  a.id=b.id && a.filename=b.filename && a.location=b.location &&
  a.length=b.length && a.mtime=b.mtime && a.inode=b.inode &&
  a.ctime=b.ctime && Mail_flag.Imap_flag.equal_durable a.flags b.flags

let require_current t o =
  match current t o with
  | Some actual when same_occurrence actual o -> ()
  | _ -> raise Stale_occurrence

let with_unchanged_occurrence t occurrence f =
  let unchanged () =
    match current t occurrence with
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

let set_mtime (Dir p) output timestamp =
  let native=Eio.Path.native_exn p in
  (* Eio has no utimes. Unix.utimes reads two zero times as the current
     time, so the access time must stay nonzero for an epoch date. *)
  let atime=if timestamp=0. then 1. else timestamp in
  in_systhread ~label:"maildir-utimes" ("setting times of " ^ native)
    (fun () -> Unix.utimes native atime timestamp);
  if (Eio.File.stat output).mtime<>timestamp then
    invalid (Unrepresentable_date timestamp)

let check_mtime mtime =
  if not (Float.is_finite mtime) then invalid (Unrepresentable_date mtime)

let check t ~flags ?mtime () =
  get (Keywords.validate_flags flags);
  Option.iter check_mtime mtime;
  if has_keywords flags then
    with_metadata_lock t (fun _ ->
      ignore (get (Keywords.add (read_keywords t) flags) : Keywords.t))

let check_append writer ~flags ?mtime () =
  let t=of_writer writer in
  catch (check t ~flags ?mtime)

let append writer ?id ~source ~length ~flags ?mtime () =
  let t=of_writer writer in
  if length<0L then invalid_arg "Maildir.append: negative length";
  catch @@ fun () ->
  get (Keywords.validate_flags flags);
  let id,supplied=match id with
    | None -> reserve_id (),false
    | Some id when valid_supplied_id id -> id,true
    | Some id ->
        invalid_arg ("Maildir.append: invalid occurrence ID " ^ id) in
  Option.iter check_mtime mtime;
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
      Option.iter (set_mtime temporary output) mtime;
      Eio.File.sync output);
    with_metadata_lock t (fun refresh ->
      if supplied && locate t refresh id<>[] then
        invalid (Duplicate_identity id);
      let mapping=if keywords then ensure_keywords t flags
        else Keywords.empty in
      let location,name=
        if flags=[] then New,id else Cur,filename mapping id flags in
      let target=child (dir t location) name in
      if kind target<>`Not_found then invalid (Target_exists name);
      refresh ();
      rename temporary target;
      sync_directory (dir t location);
      match observe t (Lazy.from_val mapping) location name with
      | Some occurrence -> occurrence
      | None -> invalid (Vanished name))
  with
  | occurrence -> occurrence
  | exception exn ->
      let bt=Printexc.get_raw_backtrace () in
      if !created then discard temporary;
      Printexc.raise_with_backtrace exn bt

let open_message t ~sw occurrence =
  require_current t occurrence;
  open_in ~sw (message_path t occurrence)

let sha256 t occurrence =
  require_current t occurrence;
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

let set_flags writer occurrence flags =
  let t=of_writer writer in
  Result.bind (Keywords.validate_flags flags) @@ fun () ->
  with_metadata_lock t @@ fun refresh ->
  require_current t occurrence;
  catch @@ fun () ->
  let mapping=ensure_keywords t flags in
  let old=Option.get (parse_name occurrence.filename) in
  let passed=String.contains old.letters 'P' in
  let name=filename ~passed mapping old.base flags ^ old.suffix in
  if occurrence.location=Cur && occurrence.filename=name then occurrence
  else (
    let target=child t.cur name in
    if kind target<>`Not_found then invalid (Target_exists name);
    refresh ();
    rename (message_path t occurrence) target;
    sync_directory t.cur;
    if occurrence.location=New then sync_directory t.new_dir;
    match observe t (Lazy.from_val mapping) Cur name with
    | Some current -> current
    | None -> invalid (Vanished name))

let remove writer occurrence =
  let t=of_writer writer in
  with_metadata_lock t (fun refresh ->
    require_current t occurrence;
    refresh ();
    unlink (message_path t occurrence);
    sync_directory (dir t occurrence.location))

let recover writer =
  let t=of_writer writer in
  let is_temporary s =
    String.length s=37 && String.starts_with ~prefix:".tmp-" s &&
    valid_hex (String.sub s 5 32) in
  let temporaries=read_dir t.tmp |> List.filter is_temporary in
  List.iter (fun name -> unlink (child t.tmp name)) temporaries;
  {removed_temporary=temporaries}
