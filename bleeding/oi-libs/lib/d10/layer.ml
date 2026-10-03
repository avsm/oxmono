[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "d10.layer" ~doc:"d10 layer operations"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Find the first packages_dir in which [<dir>/<name>/<name.version>/opam] exists,
   read it, and hash its effective_part. Returns [None] only if no dir has the file;
   [hash] below treats that as fatal, mirroring day10's [Option.get]. *)
let hash_opam_file packages_dirs pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  List.find_map
    (fun dir ->
      let p = dir / name_s / pkg_s / "opam" in
      if Sys.file_exists p then
        Some
          (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw p))
          |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
          |> OpamHash.compute_from_string |> OpamHash.to_string)
      else None)
    packages_dirs

let hash ~packages_dirs pkgs =
  let hashes =
    List.map
      (fun pkg ->
        match hash_opam_file packages_dirs pkg with
        | Some h -> h
        | None ->
            Fmt.failwith "layer_hash: no opam file for %s in packages_dirs [%s]"
              (OpamPackage.to_string pkg)
              (String.concat "; " packages_dirs))
      pkgs
  in
  let concat = String.concat " " hashes in
  if Sys.getenv_opt "DAY10_DEBUG_HASH" <> None then
    Fmt.epr "layer_hash input: [%s] -> %S\n%!"
      (String.concat "," (List.map OpamPackage.to_string pkgs))
      concat;
  Digest.string concat |> Digest.to_hex

type meta = {
  package : string;
  exit_status : int;
  deps : string list;
  hashes : string list;
  created : float;
}

let meta_jsont : meta Jsont.t =
  let open Jsont in
  Object.map ~kind:"layer meta" (fun package exit_status deps hashes created ->
      { package; exit_status; deps; hashes; created })
  |> Object.mem "package" string ~enc:(fun i -> i.package)
  |> Object.mem "exit_status" int ~enc:(fun i -> i.exit_status)
  |> Object.mem "deps" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun i -> i.deps)
  |> Object.mem "hashes" (list string)
       ~dec_absent:(fun () -> [])
       ~enc:(fun i -> i.hashes)
  |> Object.mem "created" number ~enc:(fun i -> i.created)
  |> Object.finish

let meta_codec = meta_jsont

let save_meta path meta =
  let dir = Eio.Path.split path |> Option.map fst in
  Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) dir;
  match Jsont_bytesrw.encode_string meta_jsont meta with
  | Ok s -> Eio.Path.save ~create:(`Or_truncate 0o644) path s
  | Error e -> Fmt.failwith "layer.json encode: %s" e

let load_meta path =
  try
    let s = Eio.Path.load path in
    let file =
      match Eio.Path.native path with Some s -> s | None -> "<layer.json>"
    in
    match Jsont_bytesrw.decode_string ~locs:true ~file meta_jsont s with
    | Ok meta -> Some meta
    | Error e ->
        Log.warn (fun m -> m "Bad layer.json: %s" e);
        None
  with Eio.Exn.Io _ -> None

let dir (c : Config.t) ~hash = Eio.Path.(c.root / "layers" / c.os_key / hash)
let json_path c ~hash = Eio.Path.(dir c ~hash / "layer.json")
let fs_path c ~hash = Eio.Path.(dir c ~hash / "fs")
let recipe_path c ~hash = Eio.Path.(dir c ~hash / "recipe.json")
let native p = Eio.Path.native_exn p
let dir_s c ~hash = native (dir c ~hash)
let exists c ~hash = Sysops.file_exists (json_path c ~hash)
let has_recipe c ~hash = Sysops.file_exists (recipe_path c ~hash)

let load_recipe_json c ~hash =
  if has_recipe c ~hash then
    try Some (Eio.Path.load (recipe_path c ~hash)) with Eio.Exn.Io _ -> None
  else None

let succeeded c ~hash =
  match load_meta (json_path c ~hash) with
  | Some m -> m.exit_status = 0
  | None -> false

(* Streaming copy that preserves the source file's mode bits. Only used
   as the [EXDEV] fallback for [link_or_copy] below — for the same-FS
   common case, [Unix.link] is cheaper and avoids the byte-pump. *)
let copy_file_preserve_mode ~fs ~src ~dst =
  let src_p = Eio.Path.(fs / src) in
  let dst_p = Eio.Path.(fs / dst) in
  let perm = (Eio.Path.stat ~follow:false src_p).perm in
  Eio.Path.with_open_in src_p @@ fun i ->
  Eio.Path.with_open_out ~create:(`Or_truncate perm) dst_p @@ fun o ->
  Eio.Flow.copy i o

(* Hardlink fast-path with a cross-device fallback. [Unix.link] fails with
   [EXDEV] when [src] and [dst] live on different filesystems — typical
   inside containers where the d10 cache is on a BuildKit cache mount and
   the per-build prefix is on the image's overlay. Fall back to a
   streaming copy so the layer's [fs/] tree still gets populated.
   The fallback is per-file, not per-tree, so a single cross-mount build
   doesn't disable hardlinks for the rest of the run.

   Eio has no hardlink primitive, so [Unix.link] stays here — the byte-pump
   fallback is Eio-native. *)
let link_or_copy ~fs ~src ~dst =
  try Unix.link src dst
  with Unix.Unix_error (Unix.EXDEV, _, _) ->
    copy_file_preserve_mode ~fs ~src ~dst

let replace_existing dst_p f =
  try f ()
  with
  | Unix.Unix_error (Unix.EEXIST, _, _)
  | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _)
  ->
    (try Eio.Path.unlink dst_p with Eio.Exn.Io _ -> ());
    f ()

(* WORKAROUND: dune ≥ 3 writes absolute install paths into the
   [(sections …)] block of [lib/<pkg>/dune-package] for any package
   with non-default install sections (notably [stublibs] and [doc] —
   ctypes, ctypes-foreign, and caqti hit this in practice). The
   absolute path is the build-time prefix, i.e. oi's per-layer staging
   dir, which no longer exists when a downstream consumer restores the
   layer under its *own* per-layer staging dir. Dune then reports
   every file as "File unavailable" when expanding [(package <pkg>)]
   in the consumer's build rules.

   To make layers relocatable across staging-dir hashes we substitute
   the build-time prefix with the sentinel below at store time and
   reverse the substitution against the consumer's staging dir at
   restore time. Remove once dune supports relocatable installs
   natively (its [dune-package] sections would then either be relative
   or rebased on read). *)
let prefix_sentinel = "__OI_PREFIX__"

(* dune emits non-default-section absolute paths in [lib/<pkg>/dune-package].
   Restricting to that exact layout keeps the rewrite scope tiny — every
   other file in the layer keeps its fast hardlink path. *)
let is_relocatable_dune_package rel_path =
  Filename.basename rel_path = "dune-package"
  &&
  match String.split_on_char '/' rel_path with
  | [ "lib"; _; "dune-package" ] -> true
  | _ -> false

let substitute ~from_ ~to_ s =
  if from_ = "" || from_ = to_ then s
  else
    let buf = Buffer.create (String.length s) in
    let n = String.length s and m = String.length from_ in
    let i = ref 0 in
    while !i < n do
      if !i + m <= n && String.sub s !i m = from_ then (
        Buffer.add_string buf to_;
        i := !i + m)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.contents buf

(* Read [src_p], substitute [from_] → [to_] in its content, write the
   result to [dst_p] with the source's mode bits. Always unlinks
   [dst_p] first so [save]'s in-place truncation does not mutate a
   hardlinked inode that some other restore-target still aliases.

   Used in place of [link_or_copy] for [dune-package] files where the
   stored content embeds the build-time staging-dir prefix and must be
   rewritten before consumers see it. *)
let install_with_substitution ~fs ~src ~dst ~from_ ~to_ =
  let src_p = Eio.Path.(fs / src) in
  let dst_p = Eio.Path.(fs / dst) in
  let perm = (Eio.Path.stat ~follow:false src_p).perm in
  let rewritten = substitute ~from_ ~to_ (Eio.Path.load src_p) in
  (try Eio.Path.unlink dst_p with Eio.Exn.Io _ -> ());
  Eio.Path.save ~create:(`Or_truncate perm) dst_p rewritten

let install_one_file (c : Config.t) ~prefix ~fs_dir rel_path =
  let src = prefix / rel_path in
  let dst = fs_dir / rel_path in
  let src_p = Eio.Path.(c.fs / src) in
  let dst_p = Eio.Path.(c.fs / dst) in
  let kind =
    try Some (Eio.Path.stat ~follow:false src_p).kind
    with Eio.Exn.Io _ -> None
  in
  match kind with
  | Some `Symbolic_link ->
      let target = Eio.Path.read_link src_p in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(c.fs / Filename.dirname dst);
      replace_existing dst_p (fun () -> Eio.Path.symlink ~link_to:target dst_p)
  | Some `Regular_file ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(c.fs / Filename.dirname dst);
      if is_relocatable_dune_package rel_path then
        install_with_substitution ~fs:c.fs ~src ~dst ~from_:prefix
          ~to_:prefix_sentinel
      else replace_existing dst_p (fun () -> link_or_copy ~fs:c.fs ~src ~dst)
  | _ -> ()

let do_store (c : Config.t) ~hash ~prefix ~files ~package ~deps ~parent_hashes
    ~exit_status ~recipe_json =
  let d = dir_s c ~hash in
  let fs_dir = d / "fs" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(c.fs / fs_dir);
  List.iter (install_one_file c ~prefix ~fs_dir) files;
  save_meta (json_path c ~hash)
    {
      package;
      exit_status;
      deps;
      hashes = parent_hashes;
      created = Eio.Time.now c.clock;
    };
  match recipe_json with
  | None -> ()
  | Some s -> Eio.Path.save ~create:(`Or_truncate 0o644) (recipe_path c ~hash) s

(* [store] is the inner write step at the tail of {!D10ir.Direct.build_one},
   which {b must} hold {!D10.Lock.with_layer} {!Exclusive} for [hash] across
   the whole unpack/run/diff/store window (else concurrent builders race on
   the per-hash staging and build directories long before [store] is
   reached). No lock is taken here — taking it again from inside the outer
   critical section would re-enter on the same lockfile path and, on
   release, drop {b both} fds' kernel-level lock state (POSIX fcntl record
   locks are owned per-process: closing any fd that referred to the file
   releases all of that process's locks on it). *)
let store (c : Config.t) ~hash ~prefix ~files ~package ~deps ~parent_hashes
    ~exit_status ?recipe_json () =
  if succeeded c ~hash then ()
  else
    do_store c ~hash ~prefix ~files ~package ~deps ~parent_hashes ~exit_status
      ~recipe_json

(* See {!prefix_sentinel}. Post-pass after [link_tree] rewrites every
   [lib/<pkg>/dune-package] file so the sentinel becomes the
   consumer's actual staging dir. The hardlink from [link_tree] is
   broken (unlink-then-save) so the canonical layer file in
   [layers/<hash>/fs/] keeps the sentinel form for the next restore
   into a different prefix. *)
let rewrite_dune_packages_after_restore (c : Config.t) ~prefix =
  let lib_p = Eio.Path.(c.fs / prefix / "lib") in
  let dir_kind p =
    try Some (Eio.Path.stat ~follow:false p).kind with Eio.Exn.Io _ -> None
  in
  match dir_kind lib_p with
  | Some `Directory ->
      let entries = try Eio.Path.read_dir lib_p with Eio.Exn.Io _ -> [] in
      List.iter
        (fun pkg ->
          let dp_p = Eio.Path.(lib_p / pkg / "dune-package") in
          match dir_kind dp_p with
          | Some `Regular_file ->
              let perm = (Eio.Path.stat ~follow:false dp_p).perm in
              let content = Eio.Path.load dp_p in
              let rewritten =
                substitute ~from_:prefix_sentinel ~to_:prefix content
              in
              if rewritten <> content then (
                (try Eio.Path.unlink dp_p with Eio.Exn.Io _ -> ());
                Eio.Path.save ~create:(`Or_truncate perm) dp_p rewritten)
          | _ -> ())
        entries
  | _ -> ()

let restore (c : Config.t) ~hash ~prefix =
  let fs_dir = fs_path c ~hash in
  if Sysops.file_exists fs_dir then (
    Sysops.link_tree c.sys ~src:fs_dir ~dst:Eio.Path.(c.fs / prefix);
    rewrite_dune_packages_after_restore c ~prefix)

(* -- Remote registry ----------------------------------------------------- *)

type remote = [ `Http_remote of string ]

(* -- Index --------------------------------------------------------------- *)

type index_entry = { sha256 : string; size : int64 }
type remote_index = (string, index_entry) Hashtbl.t

(* [fetch_remote_index] lives in {!Remote_index} to break the
   [Layer ↔ Index] cycle: reading the remote [index.db] needs
   [Index], and [Index.rebuild] already needs [Layer.load_meta].
   In-tree callers reach for {!Remote_index.fetch} directly. *)

(* -- Remote pull --------------------------------------------------------- *)

type fetch_phase = Fetching | Verifying | Extracting

let verify_sha256 ~hash ~tmp_file ~sha256 =
  match sha256 with
  | None -> true
  | Some expected ->
      let actual =
        OpamHash.contents
          (OpamHash.compute ~kind:`SHA256 (Eio.Path.native_exn tmp_file))
      in
      if actual = expected then true
      else (
        Log.warn (fun m ->
            m "Checksum mismatch for %s: expected %s, got %s" hash expected
              actual);
        false)

let extract_tarball (c : Config.t) ~hash ~tmp_file ~staging_dir =
  try
    Sysops.Tar.extract c.sys ~archive:tmp_file ~dst:staging_dir ();
    true
  with Eio.Io (Sysops.Cmd_failed _, _) as exn ->
    Log.warn (fun m ->
        m "Failed to extract %s: %s" hash (Printexc.to_string exn));
    false

let publish_staging (c : Config.t) ~hash ~staging_dir ~layer_dir =
  (* [rename(2)] on Linux fails with ENOTEMPTY if the destination is a
     non-empty directory; blow away any pre-existing [<hash>/] (already
     proved not [succeeded]) before publishing. *)
  (try Eio.Path.rmtree ~missing_ok:true layer_dir with Eio.Exn.Io _ -> ());
  try
    Eio.Path.rename staging_dir layer_dir;
    succeeded c ~hash
  with exn ->
    Log.warn (fun m ->
        m "Failed to publish layer %s: %s" hash (Printexc.to_string exn));
    false

(* Verify + extract + publish the [<hash>.partial/] staging dir, returning
   [true] iff the layer is now durably present. *)
let finalize_remote_layer (c : Config.t) ~hash ~tmp_file ~staging_dir ~layer_dir
    ~sha256 ~phase =
  phase Verifying;
  let cleanup_tmp () = try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> () in
  let cleanup_staging () =
    try Eio.Path.rmtree ~missing_ok:true staging_dir with Eio.Exn.Io _ -> ()
  in
  if not (verify_sha256 ~hash ~tmp_file ~sha256) then (
    cleanup_tmp ();
    false)
  else (
    phase Extracting;
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 staging_dir;
    let extract_ok = extract_tarball c ~hash ~tmp_file ~staging_dir in
    cleanup_tmp ();
    if not extract_ok then (
      cleanup_staging ();
      false)
    else if publish_staging c ~hash ~staging_dir ~layer_dir then true
    else (
      cleanup_staging ();
      false))

(* Best-effort companion fetch for the layer-manifest sidecar JSON. The
   sidecar lives at [<URL>/<os_key>/layers/<hash>.json] and downloads
   into [<cache>/layers/<os_key>/<hash>.json], next to the [<hash>/]
   layer dir that [pull_remote] just published. The [layers/] prefix
   matches the streaming uploader's S3 key and the Clickhouse indexer's
   [*/layers/*.json] glob — it is the single registry layout. Old
   registries that pre-date the sidecar scheme have only tarballs, so
   any HTTP error is silently swallowed. *)
let fetch_layer_sidecar (c : Config.t) ~session ~remote ~hash =
  let url =
    match remote with
    | `Http_remote base_url ->
        Fmt.str "%s/%s/layers/%s.json" base_url c.os_key hash
  in
  let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
  let dst = Eio.Path.(os_layer_dir / (hash ^ ".json")) in
  let tmp = Eio.Path.(os_layer_dir / (hash ^ ".json.tmp")) in
  if Sysops.file_exists dst then ()
  else if Sysops.Http.fetch_session session ~url ~dst:tmp then
    try Eio.Path.rename tmp dst
    with Eio.Exn.Io _ -> ( try Eio.Path.unlink tmp with Eio.Exn.Io _ -> ())
  else try Eio.Path.unlink tmp with Eio.Exn.Io _ -> ()

let pull_remote (c : Config.t) ~session ~remote ~hash ?on_progress ?on_phase
    ?sha256 () =
  if succeeded c ~hash then true
  else
    let url =
      match remote with
      | `Http_remote base_url ->
          Fmt.str "%s/%s/layers/%s.tar.zst" base_url c.os_key hash
    in
    let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
    let layer_dir = dir c ~hash in
    (* Atomic publication: extract into [<hash>.partial/] and rename to
       [<hash>/] only after the tar exits 0. A torn [tar xf] into [<hash>/]
       would leave the dir looking "succeeded" while [fs/] is half-empty —
       exactly the bug that produced corrupt [ppx_sexp_value] layers in the
       wild. Renaming a sibling sidesteps that: either [<hash>/] is whole,
       or it doesn't exist. *)
    let staging_dir = Eio.Path.(os_layer_dir / (hash ^ ".partial")) in
    let tmp_file = Eio.Path.(os_layer_dir / (hash ^ ".tar.zst.tmp")) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
    (* Wipe any leftover staging dir from a prior interrupted run. *)
    (try Eio.Path.rmtree ~missing_ok:true staging_dir with Eio.Exn.Io _ -> ());
    let phase p = match on_phase with Some f -> f p | None -> () in
    phase Fetching;
    if Sysops.Http.fetch_session ?on_progress session ~url ~dst:tmp_file then (
      let ok =
        finalize_remote_layer c ~hash ~tmp_file ~staging_dir ~layer_dir ~sha256
          ~phase
      in
      if ok then fetch_layer_sidecar c ~session ~remote ~hash;
      ok)
    else (
      (try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> ());
      false)

(* -- Export -------------------------------------------------------------- *)

let do_export (c : Config.t) ~hash ~os_dir ~dst_file =
  if Sysops.file_exists dst_file then false
  else (
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_dir;
    (* Atomic publication: stage into .tar.zst.tmp.<pid>, rename on
       success. Per-pid tmp suffix means two exporters that somehow
       bypass the shared/exclusive split (e.g. external tooling)
       still don't collide on rename. *)
    let tmp_file =
      Eio.Path.(os_dir / Fmt.str "%s.tar.zst.tmp.%d" hash (Unix.getpid ()))
    in
    (try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> ());
    Sysops.Tar.create_zstd c.sys ~src:(dir c ~hash) ~dst:tmp_file;
    Eio.Path.rename tmp_file dst_file;
    true)

(* Single-hash export only. The bulk [export_all] (every succeeded
   layer across every os_key) went away with [oi build --export]; the
   streaming uploader stages one layer at a time via [export] and PUTs
   it straight to S3. *)
let export (c : Config.t) ~hash ~dst =
  if not (exists c ~hash) then false
  else
    let os_dir = Eio.Path.(dst / c.os_key) in
    let dst_file = Eio.Path.(os_dir / (hash ^ ".tar.zst")) in
    do_export c ~hash ~os_dir ~dst_file
