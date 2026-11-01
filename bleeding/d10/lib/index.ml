[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "d10.index"

module Log = (val Logs.src_log log_src : Logs.LOG)

type db = Sqlite3.db

type stats = {
  layers : int;
  binaries : int;
  files : int;
  findlib : int;
  tarballs : int;
}

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Fmt.failwith "layer_index sqlite: %s: %s" (Sqlite3.Rc.to_string rc) sql

(* Format-string variants for [exec] and the row-callback path so each call site
   reads as a SQL template rather than a stringly-allocated argument. *)
let execf db fmt = Fmt.kstr (exec db) fmt

let exec_rows db ~cb fmt =
  Fmt.kstr (Sqlite3.exec_not_null_no_headers db ~cb) fmt

(* Bump whenever [rebuild]'s extracted-rows logic changes shape — i.e. when
   an already-correct on-disk [index.db] would produce a *different* row set
   if regenerated from the same layer dirs. Mismatched stamps force a full
   per-os_key rebuild on next open, so existing caches converge to the new
   shape without a manual [rm index.db]. *)
let indexer_version = "2"

let schema_layers =
  {|
  CREATE TABLE IF NOT EXISTS layers (
    hash            TEXT PRIMARY KEY,
    os_key          TEXT NOT NULL,
    arch            TEXT NOT NULL,
    os              TEXT NOT NULL,
    distro          TEXT NOT NULL,
    os_version      TEXT NOT NULL,
    package_name    TEXT NOT NULL,
    package_ver     TEXT NOT NULL,
    exit_status     INTEGER NOT NULL,
    created         REAL NOT NULL,
    overlay_handle  TEXT,
    overlay_version TEXT,
    -- [tarball_sha256] / [tarball_size] populated when a [.tar.zst] exists
    -- alongside this index. Both NULL on a bin-index registry; both set on a
    -- layer-cache registry. Replaces the old OINDEX.txt sidecar.
    tarball_sha256  TEXT,
    tarball_size    INTEGER
  );
|}

let schema_layer_aux =
  {|
  CREATE TABLE IF NOT EXISTS layer_deps (
    layer_hash    TEXT NOT NULL,
    dep_name      TEXT NOT NULL,
    dep_version   TEXT NOT NULL,
    dep_hash      TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  CREATE TABLE IF NOT EXISTS layer_binaries (
    layer_hash    TEXT NOT NULL,
    binary_name   TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  -- Ocamlfind package metadata. Populated for every [lib/<dir>/META] file.
  -- Lets [oi search] answer "what opam package provides ocamlfind library X"
  -- without storing every file path. Multiple rows per (layer_hash,
  -- package_dir) are possible because META declares nested subpackages.
  CREATE TABLE IF NOT EXISTS layer_meta (
    layer_hash      TEXT NOT NULL,
    package_dir     TEXT NOT NULL, -- e.g. "cohttp" — the dir under lib/
    findlib_pkg     TEXT NOT NULL, -- e.g. "cohttp.async"
    archive         TEXT,          -- "cohttp_async.cma" / .cmxa, may be NULL
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  -- Optional file path index, populated only when
  -- [rebuild ~include_files:true]. Default false keeps [index.db] under a
  -- megabyte for typical tool sets, since [oi search] / [binaries_for] /
  -- [meta_for] don't read this table.
  CREATE TABLE IF NOT EXISTS layer_files (
    layer_hash    TEXT NOT NULL,
    path          TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );
|}

let schema_indices =
  {|
  CREATE INDEX IF NOT EXISTS idx_layers_os_key ON layers(os_key);
  CREATE INDEX IF NOT EXISTS idx_layers_arch ON layers(arch);
  CREATE INDEX IF NOT EXISTS idx_layers_distro ON layers(distro);
  CREATE INDEX IF NOT EXISTS idx_layers_name ON layers(package_name, os_key);
  CREATE INDEX IF NOT EXISTS idx_layers_overlay ON layers(overlay_handle, overlay_version);
  CREATE INDEX IF NOT EXISTS idx_binaries_name ON layer_binaries(binary_name);
  CREATE INDEX IF NOT EXISTS idx_deps_hash ON layer_deps(layer_hash);
  CREATE INDEX IF NOT EXISTS idx_files_hash ON layer_files(layer_hash);
  CREATE INDEX IF NOT EXISTS idx_meta_findlib ON layer_meta(findlib_pkg);
  CREATE INDEX IF NOT EXISTS idx_meta_hash ON layer_meta(layer_hash);
|}

let schema_index_meta =
  {|
  -- Per-os_key stamp of which indexer code shape last wrote the rows for
  -- that platform. [Layer_index.ensure_local] consults this and forces a
  -- full rebuild when on-disk doesn't match [indexer_version].
  CREATE TABLE IF NOT EXISTS index_meta (
    os_key          TEXT PRIMARY KEY,
    indexer_version TEXT NOT NULL
  );
|}

let schema =
  schema_layers ^ schema_layer_aux ^ schema_indices ^ schema_index_meta

let open_ ~fs ~path =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / Filename.dirname path);
  let db = Sqlite3.db_open path in
  exec db schema;
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  db

let close db = ignore (Sqlite3.db_close db)

let quote s =
  let s = String.split_on_char '\'' s |> String.concat "''" in
  Fmt.str "'%s'" s

(* -- Indexing ------------------------------------------------------------- *)

(* Classify a single fs/ entry: returns the action [scan_files] should take.
   Splitting this out keeps the [scan] body flat (the [match st.kind] used to
   live nested inside two iter+try clauses). *)
let classify_fs_entry ~dir name : [ `Skip | `File | `Recurse ] =
  try
    let st = Eio.Path.stat ~follow:false Eio.Path.(dir / name) in
    (* Symlinks count as files — many layers ship [bin/ocamlc] as a symlink
       to [bin/ocamlc.opt], and a regular-file-only scan would silently drop
       the user-facing name from the binary index. *)
    match st.kind with
    | `Regular_file | `Symbolic_link -> `File
    | `Directory -> `Recurse
    | _ -> `Skip
  with Eio.Exn.Io _ -> `Skip

let scan_files ~fs fs_dir =
  let files = ref [] in
  let base = Eio.Path.(fs / fs_dir) in
  let rel_of rel_dir name =
    if rel_dir = "" then name else Filename.concat rel_dir name
  in
  let rec scan rel_dir =
    let dir = if rel_dir = "" then base else Eio.Path.(base / rel_dir) in
    if Sysops.file_exists dir then
      List.iter (visit_entry ~dir ~rel_dir) (Eio.Path.read_dir dir)
  and visit_entry ~dir ~rel_dir name =
    match classify_fs_entry ~dir name with
    | `File -> files := rel_of rel_dir name :: !files
    | `Recurse -> scan (rel_of rel_dir name)
    | `Skip -> ()
  in
  scan "";
  !files

let parse_pkg_string s =
  try
    let pkg = OpamPackage.of_string s in
    ( OpamPackage.Name.to_string (OpamPackage.name pkg),
      OpamPackage.Version.to_string (OpamPackage.version pkg) )
  with Failure _ -> (s, "")

(* Best-effort findlib META parser. Looks at the bytes of a META file
   to extract:
   - the top-level package name (= containing directory under lib/)
   - any subpackages declared via [package "X" (...)]
   - the [archive(byte)] / [archive(native)] string for each, when
     present.

   Doesn't try to evaluate the full findlib predicate algebra — that
   would pull in [findlib]'s own parser. Good enough for [oi search]'s
   "what opam package provides ocamlfind library X" question, which
   only needs the name graph. Lines starting with [#] are comments;
   sub-packages that span multiple lines are caught by a simple
   brace counter. *)
(* Tiny scanner over a findlib [META] file. Each helper closes over the
   buffer; [contents]/[len] are passed in once via {!parse_meta_file} so the
   public entry point stays flat. *)
module Meta_scan = struct
  let rec eol_after ~contents ~len j =
    if j >= len then j
    else if contents.[j] = '\n' then j + 1
    else eol_after ~contents ~len (j + 1)

  let rec skip_ws ~contents ~len i =
    if i >= len then i
    else
      match contents.[i] with
      | ' ' | '\t' | '\n' | '\r' -> skip_ws ~contents ~len (i + 1)
      | '#' -> skip_ws ~contents ~len (eol_after ~contents ~len i)
      | _ -> i

  let rec read_quoted_loop ~contents ~len j buf =
    if j >= len then None
    else
      match contents.[j] with
      | '"' -> Some (Buffer.contents buf, j + 1)
      | '\\' when j + 1 < len ->
          Buffer.add_char buf contents.[j + 1];
          read_quoted_loop ~contents ~len (j + 2) buf
      | c ->
          Buffer.add_char buf c;
          read_quoted_loop ~contents ~len (j + 1) buf

  let read_quoted ~contents ~len i =
    if i >= len || contents.[i] <> '"' then None
    else read_quoted_loop ~contents ~len (i + 1) (Buffer.create 16)

  let starts_with ~contents ~len p i =
    i + String.length p <= len && String.sub contents i (String.length p) = p

  let skip_parens ~contents ~len j =
    if j < len && contents.[j] = '(' then
      let rec close k =
        if k >= len || contents.[k] = ')' then k + 1 else close (k + 1)
      in
      close j
    else j

  (* Match [archive(byte) = "X"] / [archive(native) = "X"] / [archive = "X"];
     return the first hit found in this scope. *)
  let rec scan_archive ~contents ~len i =
    if i >= len then None
    else if starts_with ~contents ~len "archive" i then
      let j = skip_ws ~contents ~len (i + 7) in
      let j = skip_parens ~contents ~len j in
      let j = skip_ws ~contents ~len j in
      let j = if j < len && contents.[j] = '=' then j + 1 else j in
      let j = skip_ws ~contents ~len j in
      match read_quoted ~contents ~len j with
      | Some (s, _) -> Some s
      | None -> scan_archive ~contents ~len (i + 1)
    else scan_archive ~contents ~len (i + 1)

  let qualified_name ~name_stack subname =
    match name_stack with [] -> subname | top :: _ -> top ^ "." ^ subname

  let rec scan_packages ~contents ~len ~push depth name_stack i =
    if i >= len then ()
    else
      let i = skip_ws ~contents ~len i in
      if i >= len then ()
      else if starts_with ~contents ~len "package" i then
        scan_package_decl ~contents ~len ~push depth name_stack i
      else if i < len && contents.[i] = ')' then
        scan_packages ~contents ~len ~push (depth - 1)
          (match name_stack with [] -> [] | _ :: t -> t)
          (i + 1)
      else scan_packages ~contents ~len ~push depth name_stack (i + 1)

  and scan_package_decl ~contents ~len ~push depth name_stack i =
    let j = skip_ws ~contents ~len (i + 7) in
    match read_quoted ~contents ~len j with
    | None -> scan_packages ~contents ~len ~push depth name_stack (i + 1)
    | Some (subname, k) ->
        let k = skip_ws ~contents ~len k in
        if k < len && contents.[k] = '(' then begin
          let archive = scan_archive ~contents ~len (k + 1) in
          let new_name = qualified_name ~name_stack subname in
          push new_name archive;
          scan_packages ~contents ~len ~push (depth + 1)
            (new_name :: name_stack) (k + 1)
        end
        else scan_packages ~contents ~len ~push depth name_stack (k + 1)
end

let parse_meta_file ~package_dir contents =
  let len = String.length contents in
  let acc = ref [] in
  let push name archive = acc := (name, archive) :: !acc in
  (* Top-level package = directory name. *)
  let archive = Meta_scan.scan_archive ~contents ~len 0 in
  push package_dir archive;
  Meta_scan.scan_packages ~contents ~len ~push 0 [ package_dir ] 0;
  List.rev !acc

let load_meta_file_at meta_path =
  try
    let st = Eio.Path.stat ~follow:false meta_path in
    match st.kind with
    | `Regular_file | `Symbolic_link -> Some (Eio.Path.load meta_path)
    | _ -> None
  with Eio.Exn.Io _ -> None

let metas_of_pkg_dir ~lib_dir pkg_dir =
  let meta_path = Eio.Path.(lib_dir / pkg_dir / "META") in
  match load_meta_file_at meta_path with
  | None -> []
  | Some contents ->
      parse_meta_file ~package_dir:pkg_dir contents
      |> List.map (fun (findlib_pkg, archive) ->
          (pkg_dir, findlib_pkg, archive))

(* Walk [<fs_dir>/lib/<dir>/META] for every immediate subdir of lib/.
   Returns [(package_dir, findlib_pkg, archive_opt)] triples. *)
let scan_meta ~fs fs_dir =
  let lib_dir = Eio.Path.(fs / fs_dir / "lib") in
  if not (Sysops.file_exists lib_dir) then []
  else
    let entries = try Eio.Path.read_dir lib_dir with Eio.Exn.Io _ -> [] in
    List.concat_map (metas_of_pkg_dir ~lib_dir) entries

let drop_os_key_rows db ~os_key =
  let scoped table =
    Fmt.str
      "DELETE FROM %s WHERE layer_hash IN (SELECT hash FROM layers WHERE \
       os_key = %s)"
      table (quote os_key)
  in
  exec db (scoped "layer_files");
  exec db (scoped "layer_binaries");
  exec db (scoped "layer_deps");
  exec db (scoped "layer_meta");
  execf db "DELETE FROM layers WHERE os_key = %s" (quote os_key)

let insert_layer_row db ~hash ~os_key ~(parts : Os_key.t) ~(info : Layer.meta)
    ~overlay_for =
  let { Os_key.distro; os_version; arch; os } = parts in
  let name, version = parse_pkg_string info.package in
  let oh, ov =
    match overlay_for ~hash with
    | None -> ("NULL", "NULL")
    | Some (o : Overlay.t) -> (quote o.handle, quote o.version)
  in
  execf db
    "INSERT OR REPLACE INTO layers (hash, os_key, arch, os, distro, \
     os_version, package_name, package_ver, exit_status, created, \
     overlay_handle, overlay_version) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, \
     %d, %f, %s, %s)"
    (quote hash) (quote os_key) (quote arch) (quote os) (quote distro)
    (quote os_version) (quote name) (quote version) info.exit_status
    info.created oh ov

let insert_dep_rows db ~hash (info : Layer.meta) =
  List.iteri
    (fun i dep_s ->
      let dep_name, dep_version = parse_pkg_string dep_s in
      let dep_hash =
        if i < List.length info.hashes then List.nth info.hashes i else ""
      in
      execf db
        "INSERT INTO layer_deps (layer_hash, dep_name, dep_version, dep_hash) \
         VALUES (%s, %s, %s, %s)"
        (quote hash) (quote dep_name) (quote dep_version) (quote dep_hash))
    info.deps

let binary_name_of_path path =
  if String.length path > 4 && String.sub path 0 4 = "bin/" then
    Some (String.sub path 4 (String.length path - 4))
  else if String.length path > 5 && String.sub path 0 5 = "sbin/" then
    Some (String.sub path 5 (String.length path - 5))
  else None

let insert_file_and_binary db ~hash ~include_files path =
  if include_files then
    execf db "INSERT INTO layer_files (layer_hash, path) VALUES (%s, %s)"
      (quote hash) (quote path);
  match binary_name_of_path path with
  | None -> ()
  | Some name ->
      execf db
        "INSERT INTO layer_binaries (layer_hash, binary_name) VALUES (%s, %s)"
        (quote hash) (quote name)

let insert_meta_row db ~hash (package_dir, findlib_pkg, archive) =
  let archive_q = match archive with None -> "NULL" | Some a -> quote a in
  execf db
    "INSERT INTO layer_meta (layer_hash, package_dir, findlib_pkg, archive) \
     VALUES (%s, %s, %s, %s)"
    (quote hash) (quote package_dir) (quote findlib_pkg) archive_q

(* Scan a single layer's fs/ tree: always extracts binaries + META
   (the primary "what does this layer ship" answer); records the full file
   path list only when [include_files=true] (the layer-cache mode). *)
let layer_fs db ~hash ~include_files ~fs ~layers_dir =
  let fs_dir = Eio.Path.(layers_dir / hash / "fs") in
  if not (Sysops.file_exists fs_dir) then ()
  else begin
    let files = scan_files ~fs (Eio.Path.native_exn fs_dir) in
    List.iter (insert_file_and_binary db ~hash ~include_files) files;
    let metas = scan_meta ~fs (Eio.Path.native_exn fs_dir) in
    List.iter (insert_meta_row db ~hash) metas
  end

let one_layer db ~os_key ~parts ~overlay_for ~include_files ~fs ~layers_dir hash
    =
  match Layer.load_meta Eio.Path.(layers_dir / hash / "layer.json") with
  | None -> ()
  | Some info ->
      insert_layer_row db ~hash ~os_key ~parts ~info ~overlay_for;
      insert_dep_rows db ~hash info;
      layer_fs db ~hash ~include_files ~fs ~layers_dir

let rebuild (c : Config.t) ?(overlay_for = fun ~hash:_ -> None)
    ?(include_files = false) db =
  let layers_dir = Eio.Path.(c.root / "layers" / c.os_key) in
  let os_key = c.os_key in
  if not (Sysops.file_exists layers_dir) then ()
  else
    let parts = Os_key.of_string os_key in
    Log.info (fun m ->
        m "Indexing layers for %s (%s/%s/%s)" os_key parts.distro parts.arch
          parts.os);
    drop_os_key_rows db ~os_key;
    let entries = Eio.Path.read_dir layers_dir in
    exec db "BEGIN TRANSACTION";
    List.iter
      (one_layer db ~os_key ~parts ~overlay_for ~include_files ~fs:c.Config.fs
         ~layers_dir)
      entries;
    execf db
      "INSERT OR REPLACE INTO index_meta (os_key, indexer_version) VALUES (%s, \
       %s)"
      (quote os_key) (quote indexer_version);
    exec db "COMMIT";
    Log.info (fun m ->
        m "Indexed %d layers for %s" (List.length entries) os_key)

let indexer_stamp db ~os_key : string option =
  let stmt =
    Sqlite3.prepare db
      "SELECT indexer_version FROM index_meta WHERE os_key = ? LIMIT 1"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT os_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Some (Sqlite3.column_text stmt 0)
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

(* -- Queries -------------------------------------------------------------- *)

let find_layer db ~name ~version ~os_key =
  let stmt =
    Sqlite3.prepare db
      "SELECT hash, exit_status FROM layers WHERE package_name = ? AND \
       package_ver = ? AND os_key = ? LIMIT 1"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT version));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT os_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        Some
          ( Sqlite3.column_text stmt 0,
            Sqlite3.Data.to_int_exn (Sqlite3.column stmt 1) )
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let overlay_of_cols oh ov =
  if oh = "" then None else Some { Overlay.handle = oh; version = ov }

let binaries_for db ~binary ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; hash; oh; ov |] ->
        results := (name, version, hash, overlay_of_cols oh ov) :: !results
    | _ -> ()
  in
  ignore
    (exec_rows db ~cb
       "SELECT l.package_name, l.package_ver, l.hash, \
        COALESCE(l.overlay_handle, ''), COALESCE(l.overlay_version, '') FROM \
        layer_binaries b JOIN layers l ON b.layer_hash = l.hash WHERE \
        b.binary_name = %s AND l.os_key = %s AND l.exit_status = 0"
       (quote binary) (quote os_key));
  (* Sort by opam version descending — latest version first *)
  List.sort
    (fun (_, v1, _, _) (_, v2, _, _) ->
      OpamPackage.Version.compare
        (OpamPackage.Version.of_string v2)
        (OpamPackage.Version.of_string v1))
    (List.rev !results)

let search_package db ~pattern ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; hash; oh; ov |] ->
        results := (name, version, hash, overlay_of_cols oh ov) :: !results
    | _ -> ()
  in
  let sql_pattern = String.map (fun c -> if c = '*' then '%' else c) pattern in
  let op = if String.contains sql_pattern '%' then "LIKE" else "=" in
  ignore
    (exec_rows db ~cb
       "SELECT package_name, package_ver, hash, COALESCE(overlay_handle, ''), \
        COALESCE(overlay_version, '') FROM layers WHERE package_name %s %s AND \
        os_key = %s AND exit_status = 0"
       op (quote sql_pattern) (quote os_key));
  List.sort
    (fun (n1, v1, _, _) (n2, v2, _, _) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c
      else
        OpamPackage.Version.compare
          (OpamPackage.Version.of_string v2)
          (OpamPackage.Version.of_string v1))
    (List.rev !results)

let search_binary db ~pattern ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| binary; name; version; hash; oh; ov |] ->
        results :=
          (binary, name, version, hash, overlay_of_cols oh ov) :: !results
    | _ -> ()
  in
  (* Convert user wildcards: * → % *)
  let sql_pattern = String.map (fun c -> if c = '*' then '%' else c) pattern in
  let op = if String.contains sql_pattern '%' then "LIKE" else "=" in
  (* COALESCE the nullable overlay columns so [exec_not_null_no_headers]
     (which skips rows containing NULLs) still sees every match. *)
  ignore
    (exec_rows db ~cb
       "SELECT b.binary_name, l.package_name, l.package_ver, l.hash, \
        COALESCE(l.overlay_handle, ''), COALESCE(l.overlay_version, '') FROM \
        layer_binaries b JOIN layers l ON b.layer_hash = l.hash WHERE \
        b.binary_name %s %s AND l.os_key = %s AND l.exit_status = 0"
       op (quote sql_pattern) (quote os_key));
  List.sort
    (fun (b1, _, v1, _, _) (b2, _, v2, _, _) ->
      let c = String.compare b1 b2 in
      if c <> 0 then c
      else
        OpamPackage.Version.compare
          (OpamPackage.Version.of_string v2)
          (OpamPackage.Version.of_string v1))
    (List.rev !results)

let meta_for db ~findlib_pkg ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; hash; oh; ov |] ->
        results := (name, version, hash, overlay_of_cols oh ov) :: !results
    | _ -> ()
  in
  let sql_pattern =
    String.map (fun c -> if c = '*' then '%' else c) findlib_pkg
  in
  let op = if String.contains sql_pattern '%' then "LIKE" else "=" in
  ignore
    (exec_rows db ~cb
       "SELECT l.package_name, l.package_ver, l.hash, \
        COALESCE(l.overlay_handle, ''), COALESCE(l.overlay_version, '') FROM \
        layer_meta m JOIN layers l ON m.layer_hash = l.hash WHERE \
        m.findlib_pkg %s %s AND l.os_key = %s AND l.exit_status = 0"
       op (quote sql_pattern) (quote os_key));
  List.sort
    (fun (_, v1, _, _) (_, v2, _, _) ->
      OpamPackage.Version.compare
        (OpamPackage.Version.of_string v2)
        (OpamPackage.Version.of_string v1))
    (List.rev !results)

let deps db ~hash =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; dep_hash |] ->
        results := (name, version, dep_hash) :: !results
    | _ -> ()
  in
  ignore
    (exec_rows db ~cb
       "SELECT dep_name, dep_version, dep_hash FROM layer_deps WHERE \
        layer_hash = %s ORDER BY dep_name"
       (quote hash));
  List.rev !results

let files db ~hash =
  let results = ref [] in
  let cb row =
    match row with [| path |] -> results := path :: !results | _ -> ()
  in
  ignore
    (exec_rows db ~cb
       "SELECT path FROM layer_files WHERE layer_hash = %s ORDER BY path"
       (quote hash));
  List.rev !results

let all_binaries db ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| binary; name; version |] ->
        results := (binary, name, version) :: !results
    | _ -> ()
  in
  ignore
    (exec_rows db ~cb
       "SELECT b.binary_name, l.package_name, l.package_ver FROM \
        layer_binaries b JOIN layers l ON b.layer_hash = l.hash WHERE l.os_key \
        = %s AND l.exit_status = 0 ORDER BY b.binary_name"
       (quote os_key));
  List.rev !results

(* -- Stats ---------------------------------------------------------------- *)

let stats db ~os_key =
  let get_count sql =
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT os_key));
    let n =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Sqlite3.Data.to_int_exn (Sqlite3.column stmt 0)
      | _ -> 0
    in
    ignore (Sqlite3.finalize stmt);
    n
  in
  let scoped table =
    Fmt.str
      "SELECT COUNT(*) FROM %s t JOIN layers l ON t.layer_hash = l.hash WHERE \
       l.os_key = ?"
      table
  in
  let layers = get_count "SELECT COUNT(*) FROM layers WHERE os_key = ?" in
  let binaries = get_count (scoped "layer_binaries") in
  let files = get_count (scoped "layer_files") in
  let findlib = get_count (scoped "layer_meta") in
  let tarballs =
    get_count
      "SELECT COUNT(*) FROM layers WHERE os_key = ? AND tarball_sha256 IS NOT \
       NULL"
  in
  { layers; binaries; files; findlib; tarballs }

(* -- Invalidation --------------------------------------------------------- *)

let in_clause hashes = String.concat ", " (List.map quote hashes)

let dependents db ~hashes ~os_key =
  match hashes with
  | [] -> []
  | _ ->
      let results = ref [] in
      let cb row =
        match row with [| h |] -> results := h :: !results | _ -> ()
      in
      ignore
        (exec_rows db ~cb
           "SELECT DISTINCT d.layer_hash FROM layer_deps d JOIN layers l ON \
            d.layer_hash = l.hash WHERE d.dep_hash IN (%s) AND l.os_key = %s"
           (in_clause hashes) (quote os_key));
      List.rev !results

let delete_layers db ~hashes =
  match hashes with
  | [] -> ()
  | _ ->
      let in_ = in_clause hashes in
      exec db "BEGIN TRANSACTION";
      execf db "DELETE FROM layer_files WHERE layer_hash IN (%s)" in_;
      execf db "DELETE FROM layer_binaries WHERE layer_hash IN (%s)" in_;
      execf db "DELETE FROM layer_deps WHERE layer_hash IN (%s)" in_;
      execf db "DELETE FROM layers WHERE hash IN (%s)" in_;
      exec db "COMMIT"
