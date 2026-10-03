open Support

type config = {
  cache : string;
  data : string;
  compiler : string;
  repositories : string list;
  overlays : string list;
  from : string option;
  revision : string;
  refresh : bool;
  jobs : int;
  cache_tag : string;
}

type prepared = {
  prefix : string;
  root : string;
  switch : string;
  env : string array;
  binary : string;
}

let default_compiler () =
  match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
  | Some p -> p
  | None -> (
      let root = getenv "OPAMROOT" (home () / ".opam") in
      let config = OpamFile.Config.read (opam_file (root / "config")) in
      match OpamFile.Config.switch config with
      | None -> fail "Select an OxCaml switch or pass --compiler-prefix"
      | Some sw ->
          let s = OpamSwitch.to_string sw in
          if Filename.is_relative s then root / s else s / "_opam")

let opam_args root = function
  | cmd :: args -> [ "opam"; cmd; "--root=" ^ root ] @ args
  | [] -> assert false

let opam proc ?(env = clean_env ()) ~root args =
  command ~env proc (opam_args root args)

let opam_capture proc ~env ~root args = capture ~env proc (opam_args root args)

let source_overlay proc config source =
  let local = exists source in
  let checkout =
    if local then Unix.realpath source
    else
      let path = config.data / "sources" / hash source in
      if not (exists path) then (
        mkdir (Filename.dirname path);
        let tmp = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
        if exists tmp then remove_tree tmp;
        Fun.protect
          ~finally:(fun () -> if exists tmp then remove_tree tmp)
          (fun () ->
            let url =
              if String.starts_with ~prefix:"git+" source then
                String.sub source 4 (String.length source - 4)
              else source
            in
            command proc [ "git"; "clone"; "--"; url; tmp ];
            Unix.rename tmp path));
      if config.refresh then refresh_checkout proc path;
      path
  in
  let commit =
    git proc checkout [ "rev-parse"; "--verify"; config.revision ^ "^{commit}" ]
  in
  let path =
    config.data / "snapshots" / hash_fields [ source; checkout; commit; "v2" ]
  in
  if not (exists path) then (
    log "Stamping source snapshot %s" commit;
    ignore
      (Stamp.export proc ~repo:checkout ~revision:commit
         ~source:(if local then None else Some source)
         ~output:path));
  path

let system_path () =
  getenv "PATH" "/usr/bin:/bin"
  |> String.split_on_char ':'
  |> List.filter (fun p ->
         p <> "" && not (exists (Filename.dirname p / ".opam-switch")))
  |> String.concat ":"

let build_env ~prefix =
  replace_env (clean_env ())
    [
      ("PATH", (prefix / "bin") ^ ":" ^ system_path ());
      ("OCAMLLIB", prefix / "lib/ocaml");
      ("OCAMLFIND_LDCONF", "ignore");
    ]

let prepare proc ~clock ~fs config ~target ~with_packages ~dry_run =
  let version = capture proc [ "opam"; "--version" ] |> String.trim in
  if
    OpamVersion.compare
      (OpamVersion.of_string version)
      (OpamVersion.of_string "2.5")
    < 0
  then fail "opam 2.5 or newer is required (found %s)" version;
  if config.jobs < 1 then fail "--jobs must be positive";
  mkdir config.cache;
  mkdir config.data;
  let cache = Unix.realpath config.cache and data = Unix.realpath config.data in
  let config = { config with cache; data } in
  D10.Lock.with_lock ~clock ~fs ~path:(data / "metadata.lock") @@ fun _ ->
  D10.Lock.with_lock ~clock ~fs ~path:(cache / "lock") @@ fun _ ->
  let compiler = Toolchain.inspect proc config.compiler in
  let source_repos =
    Option.to_list config.from |> List.map (source_overlay proc config)
  in
  let local = config.data / "overlay" in
  let overlays = config.overlays @ if exists local then [ local ] else [] in
  let repositories =
    if config.repositories = [] then Repository.defaults
    else config.repositories
  in
  let repos =
    source_repos @ overlays @ repositories
    |> List.map (Repository.prepare proc ~data ~refresh:config.refresh)
  in
  let guards, constraints, repos = Repository.constrain ~data repos in
  let binary, roots = Repository.resolve_binary repos target with_packages in
  let roots = constraints @ roots in
  if Filename.basename binary <> binary || binary = "." || binary = ".." then
    fail "Expected a binary name, got %s" binary;
  let cc =
    try capture proc [ "cc"; "--version" ] with Eio.Exn.Io _ -> "unavailable"
  in
  let platform = capture proc [ "uname"; "-srm" ] in
  let build_flags =
    [
      "PATH";
      "CC";
      "CXX";
      "CFLAGS";
      "CXXFLAGS";
      "CPPFLAGS";
      "LDFLAGS";
      "LIBRARY_PATH";
      "CPATH";
      "C_INCLUDE_PATH";
      "CPLUS_INCLUDE_PATH";
      "OCAMLPARAM";
      "OCAMLRUNPARAM";
      "PKG_CONFIG_LIBDIR";
      "PKG_CONFIG_PATH";
      "SDKROOT";
      "MACOSX_DEPLOYMENT_TARGET";
    ]
    |> List.concat_map (fun name -> [ name; getenv name "" ])
  in
  let key =
    hash_fields
      ([
         "ox-cache-v3";
         cache;
         compiler.fingerprint;
         cc;
         platform;
         config.cache_tag;
       ]
      @ List.map (fun r -> r.Repository.digest) repos
      @ List.sort_uniq String.compare roots
      @ build_flags)
  in
  let switch = "env-" ^ key in
  let root = cache / "opam" in
  let prefix = root / switch in
  let env = build_env ~prefix in
  let ready = prefix / ".ox-ready" in
  let executable = prefix / "bin" / binary in
  if exists ready then (
    if read ready <> key ^ "\n" then fail "Invalid cache marker at %s" ready;
    if not (exists executable) then
      fail "Cached environment has no binary %s. Use --with PACKAGE." binary;
    log "Using cached environment %s" (String.sub key 0 12))
  else (
    log "Preparing environment %s" (String.sub key 0 12);
    let seed_key =
      hash_fields [ compiler.fingerprint; string_of_bool guards ]
    in
    let seed = data / "toolchains" / seed_key in
    if not (exists seed) then Toolchain.write_repository compiler ~guards seed;
    let seed_name = "compiler-" ^ String.sub seed_key 0 20 in
    if not (exists (root / "config")) then
      opam proc ~root
        [
          "init";
          "--bare";
          "--no-setup";
          "--no-opamrc";
          "--yes";
          seed_name;
          seed;
        ];
    let configured =
      opam_capture proc ~env:(clean_env ()) ~root
        [ "repository"; "list"; "--all"; "--short" ]
      |> lines
    in
    let add name path =
      if not (List.mem name configured) then
        opam proc ~root
          [ "repository"; "add"; "--dont-select"; "--yes"; name; path ]
    in
    add seed_name seed;
    List.iter (fun r -> add r.Repository.name r.path) repos;
    (* An incomplete switch has no reusable result. Opam removes its own state
       and build trees before retrying the same stable installation path. *)
    if exists prefix then
      opam proc ~root [ "switch"; "remove"; "--yes"; switch ];
    let repo_names = seed_name :: List.map (fun r -> r.Repository.name) repos in
    opam proc ~root
      [
        "switch";
        "create";
        switch;
        "--empty";
        "--no-switch";
        "--yes";
        "--repositories=" ^ String.concat "," repo_names;
      ];
    Toolchain.install proc compiler ~prefix;
    let sw = "--switch=" ^ switch in
    let host = "ox-host-toolchain." ^ compiler.fingerprint in
    let install =
      [
        "install";
        sw;
        "--yes";
        "--no-depexts";
        "--jobs=" ^ string_of_int config.jobs;
      ]
    in
    if dry_run then
      opam proc ~env ~root (install @ [ "--show-actions"; host ] @ roots)
    else (
      opam proc ~env ~root
        ([ "install"; sw; "--yes"; "--no-depexts"; host ] @ constraints);
      opam proc ~env ~root
        [ "switch"; "set-invariant"; sw; "--no-action"; host ];
      opam proc ~env ~root (install @ roots);
      (if not (exists executable) then
         let bins =
           if exists (prefix / "bin") then sorted_dir (prefix / "bin") else []
         in
         fail "No binary %s was installed. Available: %s. Use --with PACKAGE."
           binary
           (String.concat ", "
              (List.filter (fun b -> not (List.mem b compiler.tools)) bins)));
      opam proc ~env ~root
        [ "switch"; "export"; sw; "--full"; "--freeze"; prefix / "ox.locked" ];
      atomic_write ready (key ^ "\n")));
  { prefix; root; switch; env; binary }

let exec prepared args =
  let argv =
    Array.of_list
      ([
         "opam";
         "exec";
         "--root=" ^ prepared.root;
         "--switch=" ^ prepared.switch;
         "--";
         prepared.prefix / "bin" / prepared.binary;
       ]
      @ args)
  in
  Unix.execvpe "opam" argv prepared.env
