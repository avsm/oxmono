open Support

type config = {
  cache : string;
  data : string;
  compiler : string option;
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
  let guards, constraints, repos = Repository.constrain ~data repos in
  let compiler = Option.map (Toolchain.inspect proc) config.compiler in
  let repos, toolchain_root =
    match compiler with
    | None -> (repos, config.toolchain)
    | Some compiler ->
        let key =
          hash_fields
            [
              "ox-supplied-metadata-v2";
              compiler.fingerprint;
              string_of_bool guards;
            ]
        in
        let seed = data / "supplied-compilers" / key in
        if not (exists seed) then
          Toolchain.write_repository compiler ~guards seed;
        ( Repository.prepare proc ~data ~refresh:false seed :: repos,
          "ox-host-toolchain." ^ compiler.fingerprint )
  in
  let binary, roots = Repository.resolve_binary repos target with_packages in
  if Filename.basename binary <> binary || List.mem binary [ "."; ".." ] then
    fail "Expected a binary name: %s" binary;
  let roots = (toolchain_root :: constraints) @ roots in
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
      ("ox-request-v3" :: identity
       :: List.map (fun r -> r.Repository.digest) repos
      @ List.sort_uniq String.compare roots)
  in
  let receipt = cache / "requests" / (request ^ ".sexp") in
  let assemble built =
    let layers =
      Build.unique (List.concat_map (fun b -> b.Build.closure) built)
    in
    let key = hash_fields (request :: layers) in
    let run_prefix = cache / "runs" / key in
    if not dry_run then (
      List.iter (Build.restore builder) built;
      if
        (not (exists (Build.marker run_prefix)))
        || read (Build.marker run_prefix) <> key
      then (
        Build.materialise builder layers run_prefix;
        atomic_write (Build.marker run_prefix) key);
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
            let built =
              List.map
                (function
                  | Sexplib0.Sexp.List
                      [ Atom hash; List closure; List installed ] ->
                      let strings =
                        List.map (function
                          | Sexplib0.Sexp.Atom x -> x
                          | _ -> fail "Invalid cache receipt")
                      in
                      {
                        Build.hash;
                        prefix = Build.prefix builder hash;
                        closure = strings closure;
                        installed = strings installed;
                      }
                  | _ -> fail "Invalid cache receipt")
                entries
            in
            if
              List.for_all
                (fun b ->
                  List.for_all
                    (fun h -> D10.Layer.succeeded d10 ~hash:h)
                    b.Build.closure)
                built
            then (
              let run_prefix = assemble built in
              let env =
                env_entries
                |> List.map (function
                     | Sexplib0.Sexp.Atom s -> s
                     | _ -> fail "Invalid cache environment")
                |> Array.of_list
              in
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
        let supplied = Option.map (Build.supplied builder) compiler in
        let by_name = Hashtbl.create 64 in
        let built =
          List.map
            (fun p ->
              let deps =
                Solve.dependencies solution p
                |> List.map (fun d -> Hashtbl.find by_name (Recipe.name d))
              in
              let is_supplied =
                match compiler with
                | None -> false
                | Some c ->
                    List.mem p.Solve.id c.packages
                    || Recipe.name p = "ox-host-toolchain"
              in
              let result =
                if is_supplied then Option.get supplied
                else
                  Build.run builder ~solution
                    ~deps:(Option.to_list supplied @ deps)
                    p
              in
              Hashtbl.replace by_name (Recipe.name p) result;
              result)
            solution.packages
        in
        let run_prefix = assemble built in
        let env =
          Recipe.runtime_environment ~solution ~prefix:run_prefix
            ~jobs:config.jobs
        in
        let p = { prefix = run_prefix; env; binary } in
        let open Sexplib0.Sexp in
        let entries =
          List.map
            (fun b ->
              List
                [
                  Atom b.Build.hash;
                  List (List.map (fun x -> Atom x) b.closure);
                  List (List.map (fun x -> Atom x) b.installed);
                ])
            built
        in
        atomic_write receipt
          (to_string_hum
             (List
                [
                  List entries;
                  List (Array.to_list env |> List.map (fun x -> Atom x));
                ]));
        p

let exec p args =
  let executable = p.prefix / "bin" / p.binary in
  let overrides =
    Array.to_list p.env
    |> List.filter_map (fun s ->
           match String.index_opt s '=' with
           | None -> None
           | Some i ->
               Some
                 ( String.sub s 0 i,
                   String.sub s (i + 1) (String.length s - i - 1) ))
  in
  Unix.execve executable
    (Array.of_list (executable :: args))
    (replace_env (clean_env ()) overrides)
