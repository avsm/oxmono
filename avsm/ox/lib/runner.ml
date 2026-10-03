open Support

type config = {
  cache : string;
  data : string;
  toolchain : string;
  repositories : string list;
  overlays : string list;
  from : string option;
  revision : string;
  refresh : bool;
  jobs : int;
  cache_tag : string;
}

type prepared = { prefix : string; env : string array; binary : string }

let source_overlay proc config source =
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

let prepare proc ~clock ~fs ~sys config ~target ~with_packages ~dry_run =
  if config.jobs < 1 then fail "--jobs must be positive";
  mkdir config.cache;
  mkdir config.data;
  let cache = Unix.realpath config.cache and data = Unix.realpath config.data in
  let config = { config with cache; data } in
  D10.Lock.with_lock ~clock ~fs ~path:(data / "metadata.lock") @@ fun _ ->
  D10.Lock.with_lock ~clock ~fs ~path:(cache / "lock") @@ fun _ ->
  let platform = Osrel.detect ~proc_mgr:proc ~fs in
  let os_key =
    Osrel.OS.to_string platform.os ^ "-" ^ Osrel.Arch.to_string platform.arch
  in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / cache); os_key }
  in
  let source_repos =
    Option.to_list config.from |> List.map (source_overlay proc config)
  in
  let overlays =
    config.overlays
    @ if exists (data / "overlay") then [ data / "overlay" ] else []
  in
  let bases =
    if config.repositories = [] then Repository.defaults
    else config.repositories
  in
  let repos =
    List.map
      (Repository.prepare proc ~data ~refresh:config.refresh)
      (source_repos @ overlays @ bases)
  in
  let constraints, repos = Repository.constrain ~data repos in
  let binary, roots = Repository.resolve_binary repos target with_packages in
  if Filename.basename binary <> binary || List.mem binary [ "."; ".." ] then
    fail "Expected a binary name: %s" binary;
  let roots = (config.toolchain :: constraints) @ roots in
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
      ("ox-request-v4" :: identity
       :: List.map (fun r -> r.Repository.digest) repos
      @ List.sort_uniq String.compare roots)
  in
  let receipt = cache / "requests" / (request ^ ".sexp") in
  let assemble layers =
    let key = hash_fields (request :: layers) in
    let run_prefix = cache / "runs" / key in
    if not dry_run then (
      List.iter (Build.restore builder) layers;
      Build.assemble builder ~key ~layers run_prefix;
      if not (exists (run_prefix / "bin" / binary)) then
        fail "Cached layers have no binary %s. Use --with PACKAGE." binary);
    run_prefix
  in
  let warm () =
    if config.refresh || not (exists receipt) then None
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
      let solution = Solve.run ~platform ~repos roots in
      if dry_run then (
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
            solution.packages
        in
        let layers = Build.layers built in
        let run_prefix = assemble layers in
        let env =
          Recipe.runtime_environment ~solution ~prefix:run_prefix
            ~jobs:config.jobs
        in
        let p = { prefix = run_prefix; env; binary } in
        let open Sexplib0.Sexp in
        let strings xs = List (List.map (fun s -> Atom s) xs) in
        atomic_write receipt
          (to_string_hum (List [ strings layers; strings (Array.to_list env) ]));
        p

let exec p args =
  let executable = p.prefix / "bin" / p.binary in
  let overrides = env_bindings p.env in
  Unix.execve executable
    (Array.of_list (executable :: args))
    (replace_env (clean_env ()) overrides)
