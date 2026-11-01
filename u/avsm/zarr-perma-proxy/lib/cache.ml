module D = Database
module S = Sqlite3.Data
module Hash = Digestif.SHA256

type selection = Whole | From of int * int option | Suffix of int
type range = { start : int; stop : int }
type outcome = Hit | Miss | Shared

exception Unsatisfiable of int
exception Upstream of string
exception Limit of string

let error fmt = Printf.ksprintf (fun s -> raise (Upstream s)) fmt

type limits = {
  max_object_bytes : int;
  reserve_bytes : int64;
  downloads : int;
  timeout : Duration.t;
  max_entries : int;
}

let default_limits =
  {
    max_object_bytes = 8 * 1024 * 1024 * 1024;
    reserve_bytes = 5_368_709_120L;
    downloads = 4;
    timeout = Duration.of_sec 1800;
    max_entries = 1_000_000;
  }

type entry = {
  hash : string;
  size : int;
  content_type : string;
  etag : string option;
  modified : string option;
  path : Eio.Fs.dir_ty Eio.Path.t;
  live : bool ref;
}

type result = Found of entry | Missing of int

type flight =
  (result * outcome, exn * Printexc.raw_backtrace) Stdlib.result Eio.Promise.t

type progress = Logger.transfer = {
  key : string;
  path : string;
  bytes : int;
  total : int option;
  age : float;
}

type transfer = { received : int ref; total : int option ref; since : Mtime.t }

type t = {
  dir : Eio.Fs.dir_ty Eio.Path.t;
  blobs : Eio.Fs.dir_ty Eio.Path.t;
  tmp : Eio.Fs.dir_ty Eio.Path.t;
  db : D.t;
  log : Logger.t;
  client : Fetch.plain option;
  random : Eio.Flow.source_ty Eio.Resource.t;
  clock : Eio.Time.Mono.ty Eio.Resource.t;
  limits : limits;
  slots : Eio.Semaphore.t;
  flights : (string, flight) Hashtbl.t;
  publish : Eio.Mutex.t;
  space : Eio.Mutex.t;
  mutable reserved : int64;
  active : (string, transfer) Hashtbl.t;
  mutable queued : int;
  mutable started : bool;
  mutable maintaining : bool;
  live : bool ref;
}

let schema =
  {|
CREATE TABLE blobs(hash TEXT PRIMARY KEY CHECK(length(hash)=64),
 size INTEGER NOT NULL CHECK(size>=0));
CREATE TABLE objects(url TEXT PRIMARY KEY, object_key TEXT NOT NULL,
 status INTEGER NOT NULL CHECK(status IN (200,404,410)),
 hash TEXT REFERENCES blobs(hash), content_type TEXT NOT NULL,
 etag TEXT, modified TEXT,
 CHECK((status=200 AND hash IS NOT NULL) OR (status!=200 AND hash IS NULL)));
CREATE INDEX objects_hash ON objects(hash);
CREATE INDEX objects_key ON objects(object_key);
|}

let key url = Hash.(to_hex (digest_string url))

let hex s n =
  String.length s = n
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let exists path = Eio.Path.kind ~follow:false path <> `Not_found

let blob_path t hash =
  if not (hex hash 64) then error "Invalid blob hash in cache index";
  Eio.Path.(t.blobs / String.sub hash 0 2 / hash)

let check_live t = if not !(t.live) then invalid_arg "Cache owner has closed"
let nullable f = function None -> S.NULL | Some x -> f x

let opt_text = function
  | S.NULL -> None
  | S.TEXT s -> Some s
  | _ -> error "Invalid cache field"

let int n = S.INT (Int64.of_int n)

let size64 n =
  if n < 0L || n > Int64.of_int max_int then error "Object size out of range";
  Int64.to_int n

let stat_size path =
  let stat = Eio.Path.stat ~follow:false path in
  if stat.kind <> `Regular_file then error "Cache blob is not a regular file";
  Optint.Int63.to_int64 stat.size

let lookup t url =
  let rows =
    D.run t.db (fun db ->
        D.rows db
          {|
SELECT o.status,o.hash,b.size,o.content_type,o.etag,o.modified
FROM objects o LEFT JOIN blobs b ON b.hash=o.hash WHERE o.url=?|}
          [ S.TEXT url ])
  in
  match rows with
  | [] -> None
  | [ [| S.INT status; S.NULL; S.NULL; _; _; _ |] ]
    when status = 404L || status = 410L ->
      Some (Missing (Int64.to_int status))
  | [
   [|
     S.INT 200L; S.TEXT hash; S.INT size; S.TEXT content_type; etag; modified;
   |];
  ] ->
      let path = blob_path t hash in
      if (not (exists path)) || stat_size path <> size then
        error
          "Missing or truncated cache blob %s; forget the affected URLs and \
           run GC"
          hash;
      Some
        (Found
           {
             hash;
             size = size64 size;
             content_type;
             etag = opt_text etag;
             modified = opt_text modified;
             path;
             live = t.live;
           })
  | _ -> error "Invalid cache index row"

let insert t ~url ~status ~hash ~size ~content_type ~etag ~modified =
  D.run t.db (fun db ->
      D.transaction db (fun () ->
          let count = D.integer db "SELECT count(*) FROM objects" [] in
          if count >= Int64.of_int t.limits.max_entries then
            raise (Limit "URL index is full");
          Option.iter
            (fun hash ->
              D.execute db
                "INSERT INTO blobs(hash,size) VALUES(?,?) ON CONFLICT(hash) DO \
                 NOTHING"
                [ S.TEXT hash; int size ];
              if
                D.integer db "SELECT size FROM blobs WHERE hash=?"
                  [ S.TEXT hash ]
                <> Int64.of_int size
              then error "Blob length disagrees with its index")
            hash;
          D.execute db
            {|
INSERT INTO objects(url,object_key,status,hash,content_type,etag,modified)
VALUES(?,?,?,?,?,?,?)|}
            [
              S.TEXT url;
              S.TEXT (key url);
              int status;
              nullable (fun x -> S.TEXT x) hash;
              S.TEXT content_type;
              nullable (fun x -> S.TEXT x) etag;
              nullable (fun x -> S.TEXT x) modified;
            ]))

let with_space t amount f =
  let amount = Int64.of_int amount in
  Eio.Mutex.use_ro t.space (fun () ->
      let free = Disk.free t.dir in
      if
        free < t.limits.reserve_bytes
        || Int64.sub free t.limits.reserve_bytes < t.reserved
        || Int64.sub (Int64.sub free t.limits.reserve_bytes) t.reserved < amount
      then raise (Limit "Insufficient space above the cache disk reserve");
      t.reserved <- Int64.add t.reserved amount);
  let left = ref amount in
  Fun.protect
    ~finally:(fun () -> t.reserved <- Int64.sub t.reserved !left)
    (fun () ->
      f (fun n ->
          let n = Int64.of_int n in
          left := Int64.sub !left n;
          t.reserved <- Int64.sub t.reserved n))

let random_name t =
  let b = Cstruct.create 16 in
  Eio.Flow.read_exact t.random b;
  Digest.to_hex (Cstruct.to_string b) ^ ".part"

let receive t ~url resp ~received ~stored ~total =
  let declared =
    Option.map size64 (Fetch.header Fetch.Header.content_length resp)
  in
  total := declared;
  let bound = Option.value declared ~default:t.limits.max_object_bytes in
  if bound > t.limits.max_object_bytes then
    raise (Limit "Object exceeds configured size limit");
  with_space t bound (fun written ->
      let tmp = Eio.Path.(t.tmp / random_name t) in
      let owned = ref false in
      Fun.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
              if !owned && exists tmp then Eio.Path.unlink tmp))
        (fun () ->
          let digest = ref (Hash.init ()) in
          Eio.Path.with_open_out ~follow:false ~create:(`Exclusive 0o600) tmp
            (fun file ->
              owned := true;
              let buffer = Cstruct.create 65536 in
              (try
                 while true do
                   let n = Eio.Flow.single_read (Fetch.body resp) buffer in
                   if n > bound - !received then (
                     received := min (max_int - n) !received + n;
                     match declared with
                     | Some _ ->
                         error "Upstream body exceeds its declared length"
                     | None ->
                         raise (Limit "Object exceeds configured size limit"));
                   received := !received + n;
                   let bytes = Cstruct.to_string ~len:n buffer in
                   digest := Hash.feed_string !digest bytes;
                   Eio.Flow.copy_string bytes file;
                   written n
                 done
               with End_of_file -> ());
              Option.iter
                (fun expected ->
                  if !received <> expected then
                    error "Truncated upstream body: %d of %d bytes" !received
                      expected)
                declared;
              Eio.File.sync file);
          let hash = Hash.(to_hex (get !digest)) in
          let content_type =
            Option.value ~default:"application/octet-stream"
              (Http.Header.get (Fetch.headers resp) "content-type")
          in
          let etag = Http.Header.get (Fetch.headers resp) "etag"
          and modified = Http.Header.get (Fetch.headers resp) "last-modified" in
          Eio.Mutex.use_ro t.publish (fun () ->
              let parent = Eio.Path.(t.blobs / String.sub hash 0 2) in
              Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
              Eioext.Path.require_private_dir parent;
              Disk.sync t.blobs;
              let path = blob_path t hash in
              (* GC cannot run while this owner is live. A crash before the SQL
                 commit leaves only an orphan, never published partial coverage. *)
              Eio.Cancel.protect (fun () ->
                  if exists path then (
                    if stat_size path <> Int64.of_int !received then
                      error "Existing blob has wrong length";
                    Eio.Path.unlink tmp)
                  else (
                    Eio.Path.rename tmp path;
                    stored := !received);
                  owned := false;
                  Disk.sync parent;
                  Disk.sync t.tmp;
                  insert t ~url ~status:200 ~hash:(Some hash) ~size:!received
                    ~content_type ~etag ~modified);
              Found
                {
                  hash;
                  size = !received;
                  content_type;
                  etag;
                  modified;
                  path;
                  live = t.live;
                })))

let warn_logger exn =
  Printf.eprintf "Proxy telemetry write failed: %s\n%!" (Printexc.to_string exn)

let download_inner t url ~received ~total =
  let started = Eio.Time.Mono.now t.clock in
  let stored = ref 0 and status = ref None in
  let log outcome =
    let elapsed = Mtime.span started (Eio.Time.Mono.now t.clock) in
    let duration_us = int_of_float (Mtime.Span.to_float_ns elapsed /. 1000.) in
    Eio.Cancel.protect (fun () ->
        try
          Logger.download t.log ~object_key:(key url) ~status:!status
            ~bytes:!received ~stored_bytes:!stored ~duration_us ~outcome
        with exn -> warn_logger exn)
  in
  match
    Eio.Time.Timeout.run_exn
      (Eio.Time.Timeout.seconds t.clock (Duration.to_f t.limits.timeout))
      (fun () ->
        if
          D.run t.db (fun db -> D.integer db "SELECT count(*) FROM objects" [])
          >= Int64.of_int t.limits.max_entries
        then raise (Limit "URL index is full");
        let client =
          match t.client with
          | Some c -> c
          | None -> invalid_arg "No upstream client configured"
        in
        Fetch.with_response
          ~headers:Fetch.Header.[ (accept_encoding, [ pref "identity" ]) ]
          ~redirects:5 client `GET url
          (fun resp ->
            status := Some (Fetch.status resp);
            match Fetch.status resp with
            | (404 | 410) as status ->
                insert t ~url ~status ~hash:None ~size:0
                  ~content_type:"application/octet-stream" ~etag:None
                  ~modified:None;
                Missing status
            | 200 ->
                (match
                   Http.Header.get (Fetch.headers resp) "content-encoding"
                 with
                | None | Some "identity" -> ()
                | Some _ -> error "Upstream content coding changes object bytes");
                if Http.Header.mem (Fetch.headers resp) "content-range" then
                  error "Unexpected Content-Range on a whole-object GET";
                receive t ~url resp ~received ~stored ~total
            | status -> error "Whole-object GET returned HTTP %d" status))
  with
  | value ->
      log (match value with Found _ -> "success" | Missing _ -> "missing");
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      log
        (match exn with
        | Limit _ -> "limit"
        | Eio.Cancel.Cancelled _ -> "cancelled"
        | _ -> "error");
      Printexc.raise_with_backtrace exn bt

let download t url =
  let transfer =
    { received = ref 0; total = ref None; since = Eio.Time.Mono.now t.clock }
  in
  Hashtbl.add t.active url transfer;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.active url)
    (fun () ->
      download_inner t url ~received:transfer.received ~total:transfer.total)

let activity t =
  check_live t;
  let now = Eio.Time.Mono.now t.clock in
  let active =
    Hashtbl.fold
      (fun url (transfer : transfer) acc ->
        let path =
          match Fetch.Middleware.Url.of_string url with
          | Error _ -> ""
          | Ok url -> (
              let path = Fetch.Middleware.Url.path_and_query url in
              match String.index_opt path '?' with
              | None -> path
              | Some n -> String.sub path 0 n)
        in
        {
          key = key url;
          path;
          bytes = !(transfer.received);
          total = !(transfer.total);
          age = Mtime.Span.to_float_ns (Mtime.span transfer.since now) /. 1e9;
        }
        :: acc)
      t.active []
  in
  (t.queued, List.sort (fun a b -> compare b.age a.age) active)

let rec get t ~url =
  check_live t;
  if t.maintaining then invalid_arg "Cache maintenance is in progress";
  t.started <- true;
  match Hashtbl.find_opt t.flights url with
  | Some flight -> (
      match Eio.Promise.await flight with
      | Ok (value, Miss) -> (value, Shared)
      | Ok result -> result
      | Error (Eio.Cancel.Cancelled _, _) ->
          Eio.Fiber.check ();
          get t ~url
      | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
  | None -> (
      let promise, resolve = Eio.Promise.create () in
      Hashtbl.add t.flights url promise;
      let result =
        try
          Ok
            (match lookup t url with
            | Some value -> (value, Hit)
            | None ->
                t.queued <- t.queued + 1;
                Fun.protect
                  ~finally:(fun () -> t.queued <- t.queued - 1)
                  (fun () -> Eio.Semaphore.acquire t.slots);
                Fun.protect
                  ~finally:(fun () -> Eio.Semaphore.release t.slots)
                  (fun () -> (download t url, Miss)))
        with exn -> Error (exn, Printexc.get_raw_backtrace ())
      in
      Hashtbl.remove t.flights url;
      Eio.Promise.resolve resolve result;
      match result with
      | Ok result -> result
      | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)

let with_open ~dir ~clock ~wall_clock ~random ?client ?(limits = default_limits)
    ?(upstreams = []) f =
  if
    limits.max_object_bytes < 1
    || limits.reserve_bytes < 0L || limits.downloads < 1
    || limits.max_entries < 1 || limits.timeout <= 0L
  then invalid_arg "Invalid cache limits";
  let client =
    Option.map
      (fun client ->
        if upstreams = [] then invalid_arg "Upstream roots are required";
        Fetch.restrict ~under:upstreams ~methods:[ `GET ] client)
      client
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Eioext.Path.require_private_dir dir;
  Eioext.Path.with_lock
    Eio.Path.(dir / ".lock")
    (fun () ->
      if
        List.exists
          (fun name ->
            String.ends_with ~suffix:".json" name
            || String.ends_with ~suffix:".data" name
            || String.ends_with ~suffix:".pending" name)
          (Eio.Path.read_dir dir)
      then invalid_arg "Legacy sparse cache detected; use a new cache directory";
      Eio.Switch.run (fun sw ->
          let blobs = Eio.Path.(dir / "blobs")
          and tmp = Eio.Path.(dir / "tmp") in
          List.iter
            (fun path ->
              Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 path;
              Eioext.Path.require_private_dir path)
            [ blobs; tmp ];
          Disk.sync dir;
          let db = D.open_ ~sw Eio.Path.(dir / "index.sqlite3") schema in
          let log = Logger.open_ ~sw ~clock:wall_clock dir in
          let live = ref true in
          Fun.protect
            ~finally:(fun () -> live := false)
            (fun () ->
              f
                {
                  dir;
                  blobs;
                  tmp;
                  db;
                  log;
                  client;
                  random;
                  clock :> Eio.Time.Mono.ty Eio.Resource.t;
                  limits;
                  slots = Eio.Semaphore.make limits.downloads;
                  flights = Hashtbl.create 16;
                  active = Hashtbl.create 4;
                  queued = 0;
                  publish = Eio.Mutex.create ();
                  space = Eio.Mutex.create ();
                  reserved = 0L;
                  started = false;
                  maintaining = false;
                  live;
                })))

let size e = e.size
let content_type e = e.content_type
let etag e = e.etag
let modified e = e.modified
let digest e = e.hash
let logger t = t.log

let resolve size = function
  | Whole -> { start = 0; stop = size }
  | From (off, len) ->
      if off < 0 || Option.fold ~none:false ~some:(fun n -> n <= 0) len then
        invalid_arg "Invalid byte range";
      if off >= size then raise (Unsatisfiable size);
      {
        start = off;
        stop = off + Option.fold ~none:(size - off) ~some:(min (size - off)) len;
      }
  | Suffix n ->
      if n <= 0 then invalid_arg "Invalid suffix range";
      if size = 0 then raise (Unsatisfiable size);
      { start = max 0 (size - n); stop = size }

let write (e : entry) r sink =
  if not !(e.live) then invalid_arg "Cache owner has closed";
  if r.start < 0 || r.stop < r.start || r.stop > e.size then
    invalid_arg "Invalid cached range";
  if r.start <> r.stop then
    Eio.Path.with_open_in ~follow:false e.path (fun file ->
        let buffer = Cstruct.create 65536 in
        let rec loop pos =
          if pos < r.stop then (
            let n = min 65536 (r.stop - pos) in
            let piece = Cstruct.sub buffer 0 n in
            Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int pos)
              [ piece ];
            Proffer.Body.Sink.write sink (Cstruct.to_string piece);
            loop (pos + n))
        in
        loop r.start)

let maintenance t f =
  check_live t;
  if t.started || t.maintaining then
    invalid_arg "Maintenance requires a fresh offline cache owner";
  t.maintaining <- true;
  Fun.protect ~finally:(fun () -> t.maintaining <- false) f

let forget t ~url =
  maintenance t (fun () ->
      D.run t.db (fun db ->
          D.execute db "DELETE FROM objects WHERE url=?" [ S.TEXT url ]))

type collected = { blobs : int; bytes : int64; temporary : int }

let gc t =
  maintenance t (fun () ->
      let blobs = ref 0 and bytes = ref 0L and temporary = ref 0 in
      List.iter
        (fun prefix ->
          if hex prefix 2 then (
            let dir = Eio.Path.(t.blobs / prefix) in
            Eioext.Path.require_private_dir dir;
            List.iter
              (fun hash ->
                if hex hash 64 && String.starts_with ~prefix hash then
                  let referenced =
                    D.run t.db (fun db ->
                        D.integer db "SELECT count(*) FROM objects WHERE hash=?"
                          [ S.TEXT hash ]
                        > 0L)
                  in
                  if not referenced then (
                    let path = Eio.Path.(dir / hash) in
                    let size = stat_size path in
                    Eio.Path.unlink path;
                    incr blobs;
                    bytes := Int64.add !bytes size))
              (Eio.Path.read_dir dir);
            Disk.sync dir))
        (Eio.Path.read_dir t.blobs);
      List.iter
        (fun name ->
          if
            String.length name = 37
            && String.ends_with ~suffix:".part" name
            && hex (String.sub name 0 32) 32
          then (
            let path = Eio.Path.(t.tmp / name) in
            ignore (stat_size path);
            Eio.Path.unlink path;
            incr temporary))
        (Eio.Path.read_dir t.tmp);
      Disk.sync t.tmp;
      D.run t.db (fun db ->
          D.execute db
            "DELETE FROM blobs WHERE NOT EXISTS(SELECT 1 FROM objects WHERE \
             objects.hash=blobs.hash)"
            []);
      { blobs = !blobs; bytes = !bytes; temporary = !temporary })

let summary ~sw dir =
  let db = D.open_ ~sw ~readonly:true Eio.Path.(dir / "index.sqlite3") schema in
  D.run db (fun db ->
      let count sql = D.integer db sql [] in
      Printf.sprintf
        {|{"urls":%Ld,"missing":%Ld,"blobs":%Ld,"indexed_blob_bytes":%Ld,"logical_bytes":%Ld}|}
        (count "SELECT count(*) FROM objects")
        (count "SELECT count(*) FROM objects WHERE status != 200")
        (count "SELECT count(*) FROM blobs")
        (count "SELECT coalesce(sum(size),0) FROM blobs")
        (count
           "SELECT coalesce(sum(b.size),0) FROM objects o JOIN blobs b ON \
            o.hash=b.hash"))
