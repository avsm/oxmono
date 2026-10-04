open Support

type config = {
  cache : string;
  data : string;
  toolchain : string;
  repositories : string list;
  overlays : string list;
  from : string option;
  revision : string option;
  refresh : bool;
  jobs : int;
  cache_tag : string;
}

type prepared = { prefix : string; env : string array; binary : string }

let source_overlay proc config source =
  let source, fragment =
    if exists source then (source, None)
    else
      match OpamStd.String.cut_at source '#' with
      | None -> (source, None)
      | Some (source, "") -> fail "Empty source revision in %s#" source
      | Some (source, revision) -> (source, Some revision)
  in
  let revision =
    Option.value config.revision
      ~default:(Option.value fragment ~default:"HEAD")
  in
  let local = exists source in
  let checkout =
    if local then Unix.realpath source
    else
      let path = config.data / "sources" / hash source in
      if not (exists path) then
        publish_dir path (fun tmp ->
            command proc [ "git"; "clone"; "--"; git_url source; tmp ]);
      if config.refresh then refresh_checkout proc path;
      path
  in
  let revision =
    if local then revision
    else
      let remote = "refs/remotes/origin/" ^ revision in
      let refs =
        git proc checkout [ "for-each-ref"; "--format=%(refname)"; remote ]
      in
      if List.mem remote (lines refs) then remote else revision
  in
  let commit =
    git proc checkout
      [ "rev-parse"; "--verify"; "--end-of-options"; revision ^ "^{commit}" ]
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

let metadata proc config =
  let source_repos =
    Option.to_list config.from |> List.map (source_overlay proc config)
  in
  let overlays =
    config.overlays
    @
    if exists (config.data / "overlay") then [ config.data / "overlay" ] else []
  in
  let bases =
    if config.repositories = [] then Repository.defaults
    else config.repositories
  in
  let repos =
    List.map
      (Repository.prepare proc ~data:config.data ~refresh:config.refresh)
      (source_repos @ overlays @ bases)
  in
  repos

let repositories proc config ~target ~with_packages =
  let repos = metadata proc config in
  let constraints, repos = Repository.constrain ~data:config.data repos in
  let binary, roots = Repository.resolve_binary repos target with_packages in
  if Filename.basename binary <> binary || List.mem binary [ "."; ".." ] then
    fail "Expected a binary name: %s" binary;
  (repos, binary, (config.toolchain :: constraints) @ roots)

type action = Build | Test | Fetch | Depexts

let package_names repos =
  repos
  |> List.filter (fun r -> exists (r.Repository.path / "ox-source"))
  |> List.concat_map (fun r -> sorted_dir (r.Repository.path / "packages"))
  |> List.sort_uniq String.compare

let depexts (solution : Solve.t) =
  solution.Solve.packages
  |> List.concat_map (fun p ->
         OpamFile.OPAM.depexts p.Solve.opam
         |> List.filter_map (fun (names, filter) ->
                if
                  OpamFilter.eval_to_bool ~default:false
                    (fun v ->
                      Solve.platform_value solution.platform
                        (OpamVariable.Full.to_string v))
                    filter
                then
                  Some
                    (OpamSysPkg.Set.elements names
                    |> List.map OpamSysPkg.to_string)
                else None)
         |> List.concat)
  |> List.sort_uniq String.compare

let prepare_request proc ~clock ~fs ~sys ?test_roots config ~select ~action
    ~exclude ~dry_run =
  if config.jobs < 1 then fail "--jobs must be positive";
  mkdir config.cache;
  mkdir config.data;
  let cache = Unix.realpath config.cache and data = Unix.realpath config.data in
  let config = { config with cache; data } in
  D10.Lock.with_lock ~clock ~fs ~path:(data / "metadata.lock") @@ fun _ ->
  D10.Lock.with_lock ~clock ~fs ~path:(cache / "lock") @@ fun _ ->
  let platform = Osrel.detect ~proc_mgr:proc ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / cache); os_key }
  in
  let repos = metadata proc config in
  let binary, selected = select repos in
  let constraints, repos = Repository.constrain ~data:config.data repos in
  let roots = (config.toolchain :: constraints) @ selected in
  let names =
    List.map
      (fun atom ->
        OpamFormula.atom_of_string atom |> fst |> OpamPackage.Name.to_string)
      selected
  in
  let test_roots =
    if action = Test then Option.value test_roots ~default:names else []
  in
  let cc =
    try capture proc [ "cc"; "--version" ] with Eio.Exn.Io _ -> "unavailable"
  in
  let identity =
    hash_fields
      ([
         "ox-day10-v1";
         cache;
         os_key;
         capture proc [ "uname"; "-srm" ];
         cc;
         config.cache_tag;
       ]
      @ (Recipe.environment ~prefix:"/OX/PREFIX"
        |> Array.to_list |> List.sort String.compare))
  in
  let builder : Build.t =
    { d10; proc; identity; jobs = config.jobs; refresh = config.refresh }
  in
  let request =
    hash_fields
      ("ox-request-v5" :: identity
       :: (if action = Test then "test" else "build")
       :: String.concat "," exclude
       :: String.concat "," (List.sort_uniq String.compare test_roots)
       :: List.map (fun r -> r.Repository.digest) repos
      @ List.sort_uniq String.compare roots)
  in
  let receipt = cache / "requests" / (request ^ ".sexp") in
  let assemble layers =
    let key = hash_fields (request :: layers) in
    let run_prefix = cache / "runs" / key in
    if not dry_run then (
      List.iter (fun hash -> D10.Prefix.restore d10 ~hash) layers;
      D10.Prefix.ensure d10 ~key ~layer_hashes:layers
        ~dst:Eio.Path.(fs / run_prefix);
      if binary <> "" && not (exists (run_prefix / "bin" / binary)) then
        fail "Cached layers have no binary %s. Use --with PACKAGE." binary);
    run_prefix
  in
  let warm () =
    if
      dry_run || config.refresh || action <> Build
      || List.exists (fun r -> exists (r.Repository.path / "ox-worktree")) repos
      || not (exists receipt)
    then None
    else
      try
        match Parsexp.Single.parse_string_exn (read receipt) with
        | Sexplib0.Sexp.List [ List entries; List env_entries ] ->
            let strings =
              List.map (function
                | Sexplib0.Sexp.Atom s -> s
                | _ -> fail "Invalid cache receipt")
            in
            let layers = strings entries in
            let env = Array.of_list (strings env_entries) in
            if List.for_all (fun hash -> D10.Layer.succeeded d10 ~hash) layers
            then (
              let run_prefix = assemble layers in
              log "Using cached day10 layers %s" (String.sub request 0 12);
              Some { prefix = run_prefix; env; binary })
            else None
        | _ -> fail "Invalid cache receipt"
      with Parsexp.Parse_error _ -> fail "Invalid cache receipt: %s" receipt
  in
  match warm () with
  | Some p -> p
  | None ->
      let solution = Solve.run ~test_roots ~platform ~repos roots in
      let is_local p = List.mem (Recipe.name p) exclude in
      (* A repository package may itself need a library from the checkout.
         Build those prerequisites through the same recipe path. *)
      let required = Hashtbl.create 64 in
      let rec require p =
        if not (Hashtbl.mem required (Recipe.name p)) then (
          Hashtbl.add required (Recipe.name p) ();
          List.iter require (Solve.dependencies solution p))
      in
      List.iter (fun p -> if not (is_local p) then require p) solution.packages;
      let build_packages =
        if exclude = [] then solution.packages
        else
          List.filter
            (fun p -> Hashtbl.mem required (Recipe.name p))
            solution.packages
      in
      if action = Depexts && not dry_run then (
        List.iter print_endline (depexts solution);
        { prefix = ""; env = [||]; binary })
      else if action = Fetch && not dry_run then (
        List.iter
          (fun p -> ignore (Source.prepare ~refresh:config.refresh proc d10 p))
          build_packages;
        { prefix = ""; env = [||]; binary })
      else if dry_run then (
        List.iter
          (fun p -> log "Plan %s" (OpamPackage.to_string p.Solve.id))
          solution.packages;
        { prefix = cache / "runs" / request; env = [||]; binary })
      else
        let by_name = Hashtbl.create 64 in
        let built =
          List.map
            (fun p ->
              let deps =
                Solve.dependencies solution p
                |> List.map (fun d -> Hashtbl.find by_name (Recipe.name d))
              in
              let result = Build.run builder ~solution ~deps p in
              Hashtbl.replace by_name (Recipe.name p) result;
              result)
            build_packages
        in
        if action = Test && exclude = [] then
          List.iter
            (fun p ->
              if List.mem (Recipe.name p) names then
                let deps =
                  Solve.dependencies solution p
                  |> List.map (fun d -> Hashtbl.find by_name (Recipe.name d))
                in
                Build.test builder ~solution ~deps p)
            solution.packages;
        let layers = Build.layers built in
        let run_prefix = assemble layers in
        let env =
          Recipe.runtime_environment
            ~solution:{ solution with packages = build_packages }
            ~prefix:run_prefix ~jobs:config.jobs
        in
        let p = { prefix = run_prefix; env; binary } in
        let open Sexplib0.Sexp in
        let strings xs = List (List.map (fun s -> Atom s) xs) in
        atomic_write receipt
          (to_string_hum (List [ strings layers; strings (Array.to_list env) ]));
        p

let prepare proc ~clock ~fs ~sys config ~target ~with_packages ~dry_run =
  let select repos =
    let binary, roots = Repository.resolve_binary repos target with_packages in
    if Filename.basename binary <> binary || List.mem binary [ "."; ".." ] then
      fail "Expected a binary name: %s" binary;
    (binary, roots)
  in
  prepare_request proc ~clock ~fs ~sys config ~select ~action:Build ~exclude:[]
    ~dry_run

let packages proc ~clock ~fs ~sys config ~roots ~all ~action ?test_roots
    ?(exclude = []) ~dry_run () =
  let select repos =
    let roots = if all then roots @ package_names repos else roots in
    if roots = [] then
      fail "No package roots. Supply PACKAGE or --from SOURCE --all.";
    ("", List.map (Repository.snapshot_root repos) roots)
  in
  prepare_request proc ~clock ~fs ~sys ?test_roots config ~select ~action
    ~exclude ~dry_run

let prefix p = p.prefix
let environment p = replace_env (clean_env ()) (env_bindings p.env)

let exports p =
  Array.iter
    (fun entry ->
      match OpamStd.String.cut_at entry '=' with
      | Some (k, v) -> Printf.printf "export %s=%s\n" k (Filename.quote v)
      | None -> ())
    p.env

let exec_command p args =
  match args with
  | [] -> fail "Expected a command"
  | cmd :: _ ->
      let env = environment p in
      let path =
        List.assoc_opt "PATH" (env_bindings env) |> Option.value ~default:""
      in
      let executable =
        if String.contains cmd '/' then cmd
        else
          String.split_on_char ':' path
          |> List.find_map (fun dir ->
                 let file = dir / cmd in
                 try
                   Unix.access file [ Unix.X_OK ];
                   Some file
                 with Unix.Unix_error _ -> None)
          |> function
          | Some p -> p
          | None -> fail "Command not found: %s" cmd
      in
      Unix.execve executable (Array.of_list args) env

let exec p args =
  let executable = p.prefix / "bin" / p.binary in
  let overrides = env_bindings p.env in
  Unix.execve executable
    (Array.of_list (executable :: args))
    (replace_env (clean_env ()) overrides)
