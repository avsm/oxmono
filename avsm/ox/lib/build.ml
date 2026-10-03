open Support

type built = { hash : string; closure : string list; installed : string list }

type t = {
  d10 : D10.Config.t;
  proc : Support.proc;
  identity : string;
  jobs : int;
  refresh : bool;
}

let unique xs =
  let seen = Hashtbl.create 32 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    xs

let prefix t hash =
  Eio.Path.native_exn t.d10.root / "prefixes" / t.d10.os_key / hash

let marker prefix = prefix / ".ox-ready"

let complete t hash =
  let p = prefix t hash in
  D10.Layer.succeeded t.d10 ~hash && exists (marker p) && read (marker p) = hash

let manifest root =
  let rec walk rel acc =
    let path = if rel = "" then root else root / rel in
    let stat = Unix.lstat path in
    match stat.Unix.st_kind with
    | Unix.S_DIR ->
        List.fold_left
          (fun acc n -> walk (if rel = "" then n else rel / n) acc)
          acc (sorted_dir path)
    | Unix.S_REG ->
        (rel, hash_fields [ string_of_int stat.st_perm; hash_file path ]) :: acc
    | Unix.S_LNK -> (rel, hash_fields [ "link"; Unix.readlink path ]) :: acc
    | _ -> fail "Unsupported installed file: %s" path
  in
  walk "" []
  |> List.filter (fun (p, _) -> p <> ".ox-ready")
  |> List.sort compare

let materialise t hashes destination =
  remove_tree destination;
  mkdir destination;
  D10.Prefix.assemble t.d10 ~layer_hashes:hashes
    ~dst:Eio.Path.(t.d10.fs / destination);
  (* Restore applies dune-package rebasing at the final path. Detach all
     hardlinks before an installer can modify a dependency's cached files. *)
  let detached = destination ^ ".copy" in
  remove_tree detached;
  D10.Sysops.copy_tree t.d10.sys
    ~src:Eio.Path.(t.d10.fs / destination)
    ~dst:Eio.Path.(t.d10.fs / detached);
  remove_tree destination;
  Unix.rename detached destination;
  List.iter
    (fun sub -> mkdir (destination / sub))
    [ "bin"; "lib"; "share"; "etc"; "doc"; "man"; "sbin" ]

let layers built = unique (List.concat_map (fun b -> b.closure) built)

let assemble t ~key ~layers destination =
  if (not (exists (marker destination))) || read (marker destination) <> key
  then (
    materialise t layers destination;
    atomic_write (marker destination) key)

let restore t hash =
  if not (complete t hash) then
    match D10.Layer.load_meta (D10.Layer.json_path t.d10 ~hash) with
    | Some { exit_status = 0; hashes; _ } ->
        assemble t ~key:hash ~layers:(hashes @ [ hash ]) (prefix t hash)
    | _ -> fail "Missing day10 layer %s" hash

let run t ~solution ~deps p =
  let layers = layers deps in
  let installed = unique (List.concat_map (fun d -> d.installed) deps) in
  let source, source_hash = Source.prepare ~refresh:t.refresh t.proc t.d10 p in
  let hash =
    hash_fields
      ([
         "ox-day10-v1";
         t.identity;
         OpamPackage.to_string p.Solve.id;
         OpamFile.OPAM.write_to_string p.opam;
         source_hash;
       ]
      @ layers)
  in
  let destination = prefix t hash in
  let built =
    {
      hash;
      closure = layers @ [ hash ];
      installed = unique (installed @ [ Recipe.name p ]);
    }
  in
  if D10.Layer.succeeded t.d10 ~hash then (
    restore t hash;
    log "Cached %s" (OpamPackage.to_string p.id);
    built)
  else (
    log "Building %s" (OpamPackage.to_string p.id);
    let work = Eio.Path.native_exn t.d10.root / "build" / hash in
    remove_tree work;
    mkdir (Filename.dirname work);
    D10.Sysops.copy_tree t.d10.sys
      ~src:Eio.Path.(t.d10.fs / source)
      ~dst:Eio.Path.(t.d10.fs / work);
    materialise t layers destination;
    let before = manifest destination in
    let resolve =
      Recipe.resolver ~solution ~installed ~prefix:destination ~build_dir:work
        ~jobs:t.jobs p
    in
    let env =
      Recipe.build_environment ~solution ~installed ~prefix:destination
        ~build_dir:work ~jobs:t.jobs p
    in
    let substs =
      OpamFile.OPAM.substs p.opam
      |> List.map (fun b ->
             Source.safe_relative (OpamFilename.Base.to_string b))
    in
    List.iter
      (fun base ->
        OpamFilter.expand_interpolations_in_file_full resolve
          ~src:(OpamFilename.raw (work / (base ^ ".in")))
          ~dst:(OpamFilename.raw (work / base)))
      substs;
    let patches =
      OpamFile.OPAM.patches p.opam
      |> List.filter_map (fun (file, condition) ->
             if OpamFilter.opt_eval_to_bool resolve condition then
               Some
                 [
                   "patch";
                   "-p1";
                   "-i";
                   Source.safe_relative (OpamFilename.Base.to_string file);
                 ]
             else None)
    in
    let commands =
      patches
      @ OpamFilter.commands resolve
          (OpamFile.OPAM.build p.opam @ OpamFile.OPAM.install p.opam)
    in
    let conf_name = Recipe.name p ^ ".config" in
    let config_dir = destination / ".ox/config" in
    let script =
      Recipe.shell commands ^ "\nif test -f " ^ Filename.quote conf_name
      ^ "; then\n"
      ^ Recipe.shell
          [
            [ "mkdir"; "-p"; config_dir ];
            [ "cp"; conf_name; config_dir / conf_name ];
          ]
      ^ "\nfi\n"
    in
    let archives = Eio.Path.native_exn t.d10.root / "sources/archives" in
    mkdir archives;
    let archive = archives / (hash ^ ".tar") in
    command t.proc [ "tar"; "-cf"; archive; "-C"; work; "." ];
    let node : D10ir.Plan.node =
      {
        package = { name = Recipe.name p; version = Recipe.version p };
        layer_hash = D10ir.Layer_hash.of_string hash;
        dep_layer_hashes = List.map D10ir.Layer_hash.of_string layers;
        archive =
          { path = archive; sha256 = hash_file archive; strip_components = 0 };
        script;
        env = Array.to_list env;
        depexts = [];
        prefix = destination;
        substs = [];
        subst_vars = [];
        overlay = None;
        opam_file_sha256 = Support.hash (OpamFile.OPAM.write_to_string p.opam);
      }
    in
    let log_path = Eio.Path.native_exn t.d10.root / "logs" / (hash ^ ".log") in
    mkdir (Filename.dirname log_path);
    (try
       Eio.Path.with_open_out ~create:(`Or_truncate 0o600)
         Eio.Path.(t.d10.fs / log_path)
         (fun output ->
           Eio.Flow.copy_string
             ("# " ^ OpamPackage.to_string p.id ^ "\n" ^ script ^ "\n")
             output;
           Eio.Process.run t.proc ~env
             ~cwd:Eio.Path.(t.d10.fs / work)
             ~stdout:output ~stderr:output
             [ "/bin/sh"; "-e"; "-c"; script ]);
       let install = work / (Recipe.name p ^ ".install") in
       if exists install then
         D10ir.Install_file.apply ~fs:t.d10.fs ~prefix:destination
           ~build_dir:work ~install_file:install;
       let after = manifest destination in
       let before_index = Hashtbl.of_seq (List.to_seq before) in
       let after_index = Hashtbl.of_seq (List.to_seq after) in
       List.iter
         (fun (path, _) ->
           if not (Hashtbl.mem after_index path) then
             fail "Package deleted a dependency file: %s" path)
         before;
       let files =
         after
         |> List.filter_map (fun (path, digest) ->
                if Hashtbl.find_opt before_index path = Some digest then None
                else Some path)
       in
       D10.Layer.store t.d10 ~hash ~prefix:destination ~files
         ~package:(OpamPackage.to_string p.id)
         ~deps:(List.map (fun d -> d.hash) deps)
         ~parent_hashes:layers ~exit_status:0
         ~recipe_json:(D10ir.Plan.encode_node node)
         ();
       atomic_write (marker destination) hash;
       remove_tree work
     with exn ->
       log "Build failed; log: %s" log_path;
       (if exists log_path then
          let log_text = read log_path in
          let start = max 0 (String.length log_text - 6000) in
          prerr_string
            (String.sub log_text start (String.length log_text - start)));
       raise exn);
    built)
