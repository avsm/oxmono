module D = Proffer_dav
module X = Httpz_dav
module Path = D.Path
module Hash = Digestif.SHA256
external open_checked : Unix.file_descr -> string -> Unix.file_descr = "proffer_dav_open"
external create_file : Unix.file_descr -> string -> Unix.file_descr = "proffer_dav_create"
external unlink_file : Unix.file_descr -> string -> unit = "proffer_dav_unlink"
external publish : Unix.file_descr -> string -> string -> unit = "proffer_dav_publish"
external lock_file : Unix.file_descr -> unit = "proffer_dav_lock"
external readdir : Unix.file_descr -> int -> string list = "proffer_dav_readdir"
let fail code = raise (D.Error (code, []))
let safe f =
  try f () with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> fail 404
  | Unix.Unix_error ((Unix.EACCES | Unix.EPERM | Unix.ELOOP | Unix.EXDEV), _, _) -> fail 403
  | Unix.Unix_error (Unix.EEXIST, _, _) -> fail 412
  | Unix.Unix_error ((Unix.ENOSPC | Unix.EFBIG), _, _) -> fail 507
  | Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> fail 503
  | Unix.Unix_error _ -> fail 500
let thread f = safe (fun () -> Eio_unix.run_in_systhread f)
let use fd f = Eio_unix.Fd.use_exn "DAV directory" fd (fun fd -> thread (fun () -> f fd))
let root ~sw path =
  let subtree = Eio.Path.open_subtree ~sw path in
  (* Eio directory resources do not expose Unix_fd. Opening the already
     confined root as a file gives us a switch-owned directory descriptor. *)
  let directory = Eio.Path.open_in ~sw subtree in
  match Eio_unix.Resource.fd_opt directory with
  | Some fd ->
      use fd (fun raw -> Unix.close (open_checked raw "."));
      fd
  | None -> invalid_arg "Proffer_dav_eio: native directory required"
let with_fd root name f =
  let fd = use root (fun root -> open_checked root name) in
  match f fd with
  | value -> Unix.close fd; value
  | exception exn -> Unix.close fd; raise exn
let filename path = match Path.segments path with
  | [] -> "." | segments -> String.concat "/" segments
let stat fd = thread (fun () -> Unix.LargeFile.fstat fd)
let describe stat etag = {
  D.kind = (if stat.Unix.LargeFile.st_kind = Unix.S_DIR then D.Collection else D.File);
  length = stat.st_size; etag; modified = stat.st_mtime; properties = [] }
let digest fd =
  let buffer = Bytes.create 65536 in
  let rec loop ctx =
    let n = thread (fun () -> Unix.read fd buffer 0 (Bytes.length buffer)) in
    if n = 0 then Hash.to_hex (Hash.get ctx)
    else loop (Hash.feed_bytes ctx ~off:0 ~len:n buffer) in
  let value = loop Hash.empty in
  ignore (thread (fun () -> Unix.LargeFile.lseek fd 0L Unix.SEEK_SET));
  "\"" ^ value ^ "\""
let stream fd length sink =
  let remaining = ref length in
  let buffer = Bytes.create 65536 in
  let rec loop () =
    let wanted = Int64.to_int (min !remaining 65536L) in
    let n = thread (fun () -> Unix.read fd buffer 0 wanted) in
    if n = 0 && !remaining <> 0L then fail 500;
    remaining := Int64.sub !remaining (Int64.of_int n);
    if n > 0 then begin
      Proffer.Body.Sink.write_sub sink buffer ~off:0 ~len:n;
      loop ()
    end in loop ()
let metadata_etag info =
  if info.Unix.LargeFile.st_kind <> Unix.S_REG then None else
  Some (Printf.sprintf "W/\"%x-%x-%Lx-%.17g-%.17g\""
    info.st_dev info.st_ino info.st_size info.st_mtime info.st_ctime)
let reader ~sw ?(max_entries = 10000) path =
  if max_entries < 1 || max_entries > 100000 then invalid_arg "DAV entry limit";
  let root = root ~sw path in
  let get path = with_fd root (filename path) (fun fd ->
    let info = stat fd in
    describe info (metadata_etag info)) in
  let stat path = try Some (get path) with D.Error (404, _) -> None in
  let list path =
    let names = with_fd root (filename path) (fun fd ->
      thread (fun () -> readdir fd max_entries)) in
    List.sort String.compare names |> List.map (fun name ->
      let child = try Path.child path name with Invalid_argument _ -> fail 403 in
      name, get child) in
  let read path (fn @ local) =
    let fd = use root (fun root -> open_checked root (filename path)) in
    (match
      let info = thread (fun () -> Unix.LargeFile.fstat fd) in
      if info.st_kind <> Unix.S_REG then fail 405;
      let entry = describe info (metadata_etag info) in
      let () = fn entry (stream fd entry.length) in ()
    with
    | () -> Unix.close fd
    | exception exn -> Unix.close fd; raise exn) in
  D.Reader.v ~stat ~list ~read

type quota = { max_file_bytes : int64; max_storage_bytes : int64;
  max_staging_bytes : int64; max_entries : int; max_metadata_bytes : int }

type node = { entry : D.entry; object_name : string option }
type snapshot = { nodes : (Path.t * node) list; leases : D.lease list; saved_at : float }
let ns = "urn:proffer:dav:store:1"
let element name ?(attrs = []) children =
  X.element ~attrs:(List.map (fun (name, value) -> ("", name), value) attrs)
    (ns, name) children
let encoded_path path = String.concat "/" (List.map
  (Httpz_uri.percent_encode ~component:`Unreserved) (Path.segments path))
let parse_path value =
  if value = "" then Path.of_segments [] else
  let segments = String.split_on_char '/' value |> List.map (fun s ->
    match Httpz_uri.percent_decode s with This s -> s | Null -> fail 503) in
  try Path.of_segments segments with Invalid_argument _ -> fail 503
let object_name prefix name =
  String.length name = String.length prefix + 32 &&
  String.starts_with ~prefix name &&
  let suffix = String.sub name (String.length prefix) 32 in
  String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) suffix
let encode_snapshot ~max_bytes state =
  let node (path, node) =
    X.Element (element "node" ~attrs:([
      "path", encoded_path path;
      "kind", (if node.entry.kind = D.Collection then "collection" else "file");
      "length", Int64.to_string node.entry.length;
      "modified", Printf.sprintf "%.17g" node.entry.modified]
      @ (match node.entry.etag with None -> [] | Some s -> ["etag", s])
      @ (match node.object_name with None -> [] | Some s -> ["object", s]))
      (List.map (fun e -> X.Element e) node.entry.properties)) in
  let lease (lease : D.lease) =
    X.Element (element "lease" ~attrs:["path", encoded_path lease.path;
      "token", X.Token.to_string lease.token; "principal", lease.principal;
      "depth", X.encode_depth (lease.depth :> X.depth);
      "expires", Printf.sprintf "%.17g" lease.expires]
      (match lease.owner with None -> [] | Some owner -> [X.Element owner])) in
  X.encode_xml ~max_bytes (element "store" ~attrs:["version", "1";
    "saved-at", Printf.sprintf "%.17g" state.saved_at]
    (List.map node state.nodes @ List.map lease state.leases))
let decode_snapshot quota source =
  let bad () = fail 503 in
  let root = match X.parse_xml ~limits:{X.max_bytes=quota.max_metadata_bytes;
    max_depth=64; max_nodes=500000} source with Ok root -> root | Error _ -> bad () in
  let optional node name =
    List.assoc_opt ("", name) node.X.attrs in
  let attr node name = match optional node name with Some s -> s | None -> bad () in
  let number node name = match float_of_string_opt (attr node name) with
    | Some f when Float.is_finite f && f >= 0. -> f | _ -> bad () in
  let children e = List.filter_map (function
    | X.Element child -> Some child
    | X.Text s when String.trim s = "" -> None | _ -> bad ()) e.X.children in
  if root.name <> (ns, "store") || attr root "version" <> "1" then bad ();
  let saved_at = number root "saved-at" in
  let nodes = ref [] and leases = ref [] in
  List.iter (fun e ->
    let path = parse_path (attr e "path") in
    if e.name = (ns, "node") then begin
      let kind = match attr e "kind" with
        | "file" -> D.File | "collection" -> D.Collection | _ -> bad () in
      let length = match Int64.of_string_opt (attr e "length") with
        | Some n when n >= 0L && n <= quota.max_file_bytes -> n | _ -> bad () in
      let object_file = optional e "object" and etag = optional e "etag" in
      (match kind, object_file, etag with
      | D.Collection, None, None when length = 0L -> ()
      | D.File, Some name, Some tag when object_name "object-" name &&
          X.strong_etag tag -> ()
      | _ -> bad ());
      let properties = children e in
      if List.length properties > 128 || List.exists (fun e ->
        fst e.X.name = "DAV:" || String.length (X.encode_xml e) > 65536)
        properties then bad ();
      if List.mem_assoc path !nodes then bad ();
      nodes := (path, {entry={D.kind; length; etag; modified=number e "modified";
        properties}; object_name=object_file}) :: !nodes
    end else if e.name = (ns, "lease") then begin
      let token = match X.Token.of_string (attr e "token") with
        | Ok t -> t | Error _ -> bad () in
      let depth = match attr e "depth" with
        | "0" -> `Zero | "infinity" -> `Infinity | _ -> bad () in
      let owner = match children e with [] -> None
        | [e] when e.name = X.dav "owner" -> Some e | _ -> bad () in
      let expires = number e "expires" in
      if expires > saved_at +. 3601. || List.exists
        (fun l -> l.D.token = token) !leases then bad ();
      leases := {D.path; token; principal=attr e "principal";
        depth; expires; owner} :: !leases
    end else bad ()) (children root);
  if List.length !nodes > quota.max_entries || List.length !leases > 1024 then bad ();
  (match List.assoc_opt (Path.of_segments []) !nodes with
  | Some {entry={kind=D.Collection; _}; _} -> () | _ -> bad ());
  List.iter (fun (path, _) -> match Path.parent path with
    | None -> ()
    | Some parent -> match List.assoc_opt parent !nodes with
      | Some {entry={kind=D.Collection; _}; _} -> () | _ -> bad ()) !nodes;
  List.iter (fun lease -> if not (List.mem_assoc lease.D.path !nodes) then bad ()) !leases;
  {nodes=List.rev !nodes; leases=List.rev !leases; saved_at}
let empty now = {nodes=[Path.of_segments [], {entry={D.kind=D.Collection;
  length=0L; etag=None; modified=now; properties=[]}; object_name=None}];
  leases=[]; saved_at=now}
let all_names root limit = with_fd root "." (fun fd -> thread (fun () -> readdir fd limit))
let sync_dir root = with_fd root "." (fun fd -> thread (fun () -> Unix.fsync fd))
let read_string fd limit =
  let buffer = Buffer.create 4096 and scratch = Bytes.create 16384 in
  let rec loop () =
    let n = thread (fun () -> Unix.read fd scratch 0 (Bytes.length scratch)) in
    if n > 0 then begin
      if Buffer.length buffer > limit - n then fail 503;
      Buffer.add_subbytes buffer scratch 0 n;
      loop ()
    end in
  loop (); Buffer.contents buffer
let write_all fd bytes len =
  let rec loop off =
    if off < len then
      let n = thread (fun () -> Unix.write fd bytes off (len - off)) in
      if n = 0 then fail 507 else loop (off + n) in
  loop 0
let writer ~sw ~create ~quota ~clock ~mono_clock ~random store =
  if quota.max_file_bytes < 0L || quota.max_storage_bytes < quota.max_file_bytes
    || quota.max_staging_bytes < quota.max_file_bytes || quota.max_entries < 1
    || quota.max_entries > 100000 || quota.max_metadata_bytes < 1024 then
    invalid_arg "Proffer_dav_eio.writer: invalid quota";
  let root = root ~sw store in
  let directory = with_fd root "." stat in
  if directory.st_uid <> Unix.geteuid () || directory.st_perm land 0o777 <> 0o700
  then invalid_arg "Proffer_dav_eio.writer: store must be private (0700)";
  let initial_names = all_names root (quota.max_entries * 4 + 100) in
  if create && initial_names <> [] then
    invalid_arg "Proffer_dav_eio.writer: a new store must be empty";
  if not create && not (List.mem "manifest.xml" initial_names) then fail 503;
  let lock_fd = if create then use root (fun r -> create_file r ".writer-lock")
    else use root (fun r -> open_checked r ".writer-lock") in
  (try thread (fun () -> lock_file lock_fd)
   with exn -> Unix.close lock_fd; raise exn);
  ignore (Eio_unix.Fd.of_unix ~sw ~close_unix:true lock_fd);
  let wall = Eio.Time.now clock and mono = Eio.Time.Mono.now mono_clock in
  let now () = wall +. (Mtime.Span.to_float_ns
    (Mtime.span mono (Eio.Time.Mono.now mono_clock)) /. 1e9) in
  let random_name prefix =
    let bytes = Cstruct.create 16 in
    Eio.Flow.read_exact random bytes;
    prefix ^ String.concat "" (List.init 16 (fun i ->
      Printf.sprintf "%02x" (Cstruct.get_uint8 bytes i))) in
  let state = ref (if create then empty wall else
    with_fd root "manifest.xml" (fun fd ->
      decode_snapshot quota (read_string fd quota.max_metadata_bytes))) in
  if wall < (!state).saved_at then fail 503;
  let poisoned = ref false in
  let check () = if !poisoned then fail 503 in
  let mutex = Eio.Mutex.create () in
  let referenced snapshot name = List.exists (fun (_, node) ->
    node.object_name = Some name) snapshot.nodes in
  let collect snapshot names =
    List.iter (fun name ->
      if name = ".writer-lock" || name = "manifest.xml" then ()
      else if object_name "object-" name then begin
        if not (referenced snapshot name) then begin
          with_fd root name (fun _ -> ());
          use root (fun root -> unlink_file root name)
        end
      end else if object_name "manifest-" name then begin
        with_fd root name (fun _ -> ());
        use root (fun root -> unlink_file root name)
      end else fail 503) names in
  let save snapshot =
    let source = try encode_snapshot ~max_bytes:quota.max_metadata_bytes snapshot
      with X.Output_too_large -> fail 507 in
    if String.length source > quota.max_metadata_bytes then fail 507;
    let name = random_name "manifest-" in
    let fd = use root (fun root -> create_file root name) in
    let published = ref false in
    match
      write_all fd (Bytes.unsafe_of_string source) (String.length source);
      thread (fun () -> Unix.fsync fd);
      use root (fun root -> publish root name "manifest.xml");
      published := true;
      state := snapshot;
      sync_dir root
    with
    | () -> Unix.close fd
    | exception exn ->
        Unix.close fd;
        if !published then poisoned := true
        else (try use root (fun root -> unlink_file root name) with _ -> ());
        raise exn in
  if create then Eio.Cancel.protect (fun () -> save !state)
  else begin
    collect !state initial_names;
    List.iter (fun (_, node) -> match node.object_name with
      | None -> ()
      | Some name -> with_fd root name (fun fd ->
          let info = stat fd in
          if info.st_size <> node.entry.length || Some (digest fd) <> node.entry.etag
          then fail 503)) (!state).nodes
  end;
  let find snapshot path = List.assoc_opt path snapshot.nodes in
  let node snapshot path = match find snapshot path with Some n -> n | None -> fail 404 in
  let total snapshot = List.fold_left (fun total (_, node) ->
    if total > Int64.sub quota.max_storage_bytes node.entry.length then fail 507;
    Int64.add total node.entry.length) 0L snapshot.nodes in
  ignore (total !state);
  let stat_entry path = check (); Option.map (fun n -> n.entry) (find !state path) in
  let list path =
    check ();
    if (node !state path).entry.kind <> D.Collection then fail 405;
    List.filter_map (fun (p, n) -> if Path.parent p = Some path then
      Some (List.hd (List.rev (Path.segments p)), n.entry) else None) (!state).nodes in
  let read path (fn @ local) =
    let fd, entry = Eio.Mutex.use_ro mutex (fun () ->
      check ();
      let n = node !state path in
      match n.object_name with
      | None -> fail 405
      | Some name -> use root (fun r -> open_checked r name), n.entry) in
    (match let () = fn entry (stream fd entry.D.length) in () with
    | () -> Unix.close fd
    | exception exn -> Unix.close fd; raise exn) in
  let reader = D.Reader.v ~stat:stat_entry ~list ~read in
  let staging = ref 0L in
  let stage input =
    check ();
    let name = random_name "object-" in
    let fd = use root (fun root -> create_file root name) in
    let length = ref 0L and hash = ref Hash.empty in
    let buffer = Bytes.create 65536 in
    let rec receive () =
      let n = Proffer.Req.Input.read input buffer ~off:0 ~len:(Bytes.length buffer) in
      if n > 0 then begin
        let count = Int64.of_int n in
        if !length > Int64.sub quota.max_file_bytes count
          || !staging > Int64.sub quota.max_staging_bytes count then fail 507;
        length := Int64.add !length count;
        staging := Int64.add !staging count;
        hash := Hash.feed_bytes !hash ~off:0 ~len:n buffer;
        write_all fd buffer n;
        receive ()
      end in
    match receive (); thread (fun () -> Unix.fsync fd); sync_dir root with
    | () ->
        Unix.close fd;
        name, !length, "\"" ^ Hash.to_hex (Hash.get !hash) ^ "\""
    | exception exn ->
        Unix.close fd;
        staging := Int64.sub !staging !length;
        Eio.Cancel.protect (fun () ->
          try use root (fun root -> unlink_file root name) with _ -> ());
        raise exn in
  let current_leases () = check ();
    List.filter (fun lease -> lease.D.expires > now ()) (!state).leases in
  let mutate operation ~guard =
    let staged = match operation with D.Put (_, input) -> Some (stage input)
      | _ -> None in
    let created = ref [] in
    let cleanup () =
      List.iter (fun name -> if not (referenced !state name) then
        try use root (fun r -> unlink_file r name) with _ -> ()) !created;
      match staged with
      | None -> ()
      | Some (name, length, _) ->
          staging := Int64.sub !staging length;
          if not (referenced !state name) then
            (try use root (fun root -> unlink_file root name) with _ -> ()) in
    let commit () =
      check ();
      let before = !state in
      let active = current_leases () in
      guard reader active;
      let at = now () in
      let nodes = ref before.nodes and locks = ref active in
      let lookup path = match List.assoc_opt path !nodes with
        | Some n -> n | None -> fail 404 in
      let ensure_parent path = match Path.parent path with
        | None -> fail 403
        | Some parent -> match List.assoc_opt parent !nodes with
          | Some {entry={kind=D.Collection; _}; _} -> () | _ -> fail 409 in
      let remove path =
        nodes := List.filter (fun (p, _) -> not (Path.under ~prefix:path p)) !nodes;
        locks := List.filter (fun l -> not (Path.under ~prefix:path l.D.path)) !locks in
      let put path value = nodes := (path, value) :: List.remove_assoc path !nodes in
      let fresh_collection () = {entry={D.kind=D.Collection; length=0L; etag=None;
        modified=at; properties=[]}; object_name=None} in
      let empty_file () =
        let name = random_name "object-" in
        let fd = use root (fun root -> create_file root name) in
        created := name :: !created;
        (try thread (fun () -> Unix.fsync fd); Unix.close fd
         with exn -> Unix.close fd; raise exn);
        sync_dir root;
        {entry={D.kind=D.File; length=0L; etag=Some
          ("\"" ^ Hash.to_hex (Hash.digest_string "") ^ "\""); modified=at;
          properties=[]}; object_name=Some name} in
      let transfer src dst overwrite depth move =
        if Path.segments src = [] || Path.segments dst = []
          || Path.under ~prefix:src dst || Path.under ~prefix:dst src then fail 403;
        ignore (lookup src);
        ensure_parent dst;
        let exists = List.mem_assoc dst !nodes in
        if exists && not overwrite then fail 412;
        let copied = List.filter (fun (p, _) -> Path.equal p src ||
          depth = `Infinity && Path.under ~prefix:src p) !nodes in
        let rec suffix prefix path = match prefix, path with
          | [], path -> path | _ :: a, _ :: b -> suffix a b | _ -> assert false in
        let destination_locks = List.filter (fun l ->
          Path.equal l.D.path dst) !locks in
        remove dst;
        if move then remove src;
        locks := destination_locks @ !locks;
        let copied = List.map (fun (p, n) ->
          let target = Path.of_segments (Path.segments dst @
            suffix (Path.segments src) (Path.segments p)) in
          target, {n with entry={n.entry with modified=at}}) copied in
        nodes := copied @ !nodes;
        D.Completed (if exists then 204 else 201) in
      let result = match operation with
        | D.Put (path, _) ->
            ensure_parent path;
            let existing = List.assoc_opt path !nodes in
            (match existing with Some {entry={kind=D.Collection; _}; _} -> fail 405 | _ -> ());
            let name, length, etag = Option.get staged in
            let properties = match existing with None -> [] | Some n -> n.entry.properties in
            put path {entry={D.kind=D.File; length; etag=Some etag;
              modified=at; properties}; object_name=Some name};
            D.Written ((if existing = None then 201 else 204),
              (lookup path).entry)
        | D.Mkcol path ->
            ensure_parent path;
            if List.mem_assoc path !nodes then fail 405;
            put path (fresh_collection ()); D.Completed 201
        | D.Delete path ->
            if Path.segments path = [] then fail 403;
            ignore (lookup path); remove path; D.Completed 204
        | D.Copy {src;dst;overwrite;depth} -> transfer src dst overwrite depth false
        | D.Move {src;dst;overwrite} -> transfer src dst overwrite `Infinity true
        | D.Check path -> ignore (lookup path); D.Completed 200
        | D.Proppatch (path, updates) ->
            let old = lookup path in
            let properties = List.fold_left (fun properties -> function
              | X.Set values -> List.fold_left (fun props value -> value ::
                  List.filter (fun p -> p.X.name <> value.X.name) props) properties values
              | X.Remove names -> List.filter (fun p -> not (List.mem p.X.name names)) properties)
              old.entry.properties updates in
            if List.length properties > 128 || List.exists (fun p ->
              fst p.X.name = "DAV:" || String.length (X.encode_xml p) > 65536) properties then fail 507;
            put path {old with entry={old.entry with properties; modified=at}};
            D.Completed 207
        | D.Lock (path, principal, depth, seconds, owner) ->
            if List.length active >= 1024 || List.length (List.filter
              (fun l -> l.D.principal = principal) active) >= 64 then fail 507;
            if List.exists (fun l -> Path.equal l.D.path path ||
              l.depth = `Infinity && Path.under ~prefix:l.path path ||
              depth = `Infinity && Path.under ~prefix:path l.path) active then
              raise (D.Error (423, [X.dav "no-conflicting-lock"]));
            let exists = List.mem_assoc path !nodes in
            if not exists then (ensure_parent path; put path (empty_file ()));
            let token = match X.Token.of_string (random_name "urn:proffer-dav:lock:") with
              | Ok token -> token | Error _ -> assert false in
            let lease = {D.path; token; principal; depth;
              expires=at +. float_of_int seconds; owner} in
            locks := lease :: !locks;
            D.Locked ((if exists then 200 else 201), lease)
        | D.Refresh (path, principal, token, seconds) ->
            let lease = match List.find_opt (fun l -> l.D.token = token
              && l.path = path && l.principal = principal) active with
              | Some lease -> lease | None -> fail 412 in
            let lease = {lease with expires=at +. float_of_int seconds} in
            locks := lease :: List.filter (fun l -> l.D.token <> token) !locks;
            D.Locked (200, lease)
        | D.Unlock (path, principal, token) ->
            if not (List.exists (fun l -> l.D.token = token && l.path = path
              && l.principal = principal) active) then fail 409;
            locks := List.filter (fun l -> l.D.token <> token) !locks;
            D.Completed 204 in
      if List.length !nodes > quota.max_entries then fail 507;
      (* Collection dates track membership changes, including deletions. *)
      let paths nodes =
        let set = Hashtbl.create (List.length nodes) in
        List.iter (fun (p, _) -> Hashtbl.replace set p ()) nodes; set in
      let before_paths = paths before.nodes and after_paths = paths !nodes in
      let changed = List.filter_map (fun (p, _) ->
        if Hashtbl.mem after_paths p then None else Some p) before.nodes
        @ List.filter_map (fun (p, _) ->
          if Hashtbl.mem before_paths p then None else Some p) !nodes in
      let parents = List.filter_map Path.parent changed |> List.sort_uniq compare in
      List.iter (fun p -> match List.assoc_opt p !nodes with
        | Some n -> put p {n with entry={n.entry with modified=at}}
        | None -> ()) parents;
      let after = {nodes = !nodes; leases = !locks; saved_at=at} in
      ignore (total after);
      if (match operation with D.Check _ -> true | _ -> false) then result
      else begin
      save after;
      let obsolete = List.filter_map (fun (_, node) -> node.object_name) before.nodes
        |> List.sort_uniq String.compare in
      collect after obsolete;
      result
      end in
    match Eio.Mutex.use_ro mutex (fun () -> Eio.Cancel.protect commit) with
    | result -> Eio.Cancel.protect cleanup; result
    | exception exn -> Eio.Cancel.protect cleanup; raise exn in
  D.Writer.v ~reader ~now ~leases:current_leases ~mutate
