module S = OpamFile.Dot_install

let log_src =
  Logs.Src.create "d10ir.install_file" ~doc:"d10ir .install file parsing"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

let pkgname_of_install_file path =
  let b = Filename.basename path in
  if Filename.check_suffix b ".install" then Filename.chop_suffix b ".install"
  else b

let is_under ~base p =
  let canon p =
    try Unix.realpath p
    with Unix.Unix_error _ -> (
      try Unix.realpath (Filename.dirname p) / Filename.basename p
      with Unix.Unix_error _ -> p)
  in
  let base = canon base in
  let p = canon p in
  let n = String.length base in
  String.length p = n
  || String.length p > n
     && String.sub p 0 n = base
     && p.[n] = Filename.dir_sep.[0]

let copy_file ~fs ~optional ~exec ~src:src_s ~dst:dst_s =
  let perm = if exec then 0o755 else 0o644 in
  let src_path = Eio.Path.(fs / src_s) in
  let dst_path = Eio.Path.(fs / dst_s) in
  match Eio.Path.stat ~follow:true src_path with
  | exception Eio.Exn.Io _ ->
      if optional then false
      else Fmt.failwith "install: required source not found: %s" src_s
  | _ ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst_s);
      (try
         Eio.Path.with_open_in src_path @@ fun i ->
         Eio.Path.with_open_out ~create:(`Or_truncate perm) dst_path @@ fun o ->
         Eio.Flow.copy i o
       with Eio.Exn.Io (e, _) ->
         Fmt.failwith "install: %s -> %s: %a" src_s dst_s Eio.Exn.pp_err e);
      (try Unix.chmod dst_s perm with Unix.Unix_error _ -> ());
      true

let install_entry ~fs ~build_dir ~dst_dir ~exec (base, dst_opt) =
  let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
  let src_s = build_dir / base_s in
  let dst_name =
    match dst_opt with
    | Some d -> OpamFilename.Base.to_string d
    | None -> Filename.basename base_s
  in
  let dst_s = dst_dir / dst_name in
  let _ : bool =
    copy_file ~fs ~optional:base.OpamTypes.optional ~exec ~src:src_s ~dst:dst_s
  in
  ()

let apply ~fs ~prefix ~build_dir ~install_file =
  let pkg = pkgname_of_install_file install_file in
  let inst =
    S.safe_read (OpamFile.make (OpamFilename.of_string install_file))
  in
  let pkg_dir sub = prefix / sub / pkg in
  let global_dir sub = prefix / sub in
  let sections =
    [
      (global_dir "bin", S.bin inst, true);
      (global_dir "sbin", S.sbin inst, true);
      (pkg_dir "lib", S.lib inst, false);
      (pkg_dir "lib", S.libexec inst, true);
      (global_dir "lib", S.lib_root inst, false);
      (global_dir "lib", S.libexec_root inst, true);
      (prefix / "lib" / "toplevel", S.toplevel inst, false);
      (prefix / "lib" / "stublibs", S.stublibs inst, true);
      (global_dir "man", S.man inst, false);
      (pkg_dir "share", S.share inst, false);
      (global_dir "share", S.share_root inst, false);
      (pkg_dir "etc", S.etc inst, false);
      (pkg_dir "doc", S.doc inst, false);
    ]
  in
  List.iter
    (fun (dst_dir, entries, exec) ->
      List.iter (install_entry ~fs ~build_dir ~dst_dir ~exec) entries)
    sections;
  List.iter
    (fun (base, dst) ->
      let dst_s = OpamFilename.to_string dst in
      if is_under ~base:prefix dst_s then
        let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
        let src_s = build_dir / base_s in
        let _ : bool =
          copy_file ~fs ~optional:base.OpamTypes.optional ~exec:false ~src:src_s
            ~dst:dst_s
        in
        ()
      else
        Log.warn (fun m ->
            m "install %s: skipping misc file outside prefix: %s" pkg dst_s))
    (S.misc inst)
