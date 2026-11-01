open Support

type t = { root : string; projects : string list; prepared : Runner.prepared }

let prepare proc ~clock ~fs ~sys (config : Runner.config) ~roots ~action
    ~dry_run =
  if config.from <> None || config.revision <> None then
    fail "Working-tree commands cannot use --from or --ref";
  let cwd = Unix.realpath (Sys.getcwd ()) in
  mkdir config.data;
  let root, overlay, snapshot =
    D10.Lock.with_lock ~clock ~fs ~path:(config.data / "metadata.lock")
    @@ fun _ -> Stamp.working proc ~repo:cwd ~data:config.data
  in
  let locals = snapshot.Stamp.packages in
  let selected =
    if roots <> [] then
      List.map
        (fun name ->
          match List.find_opt (fun p -> p.Stamp.name = name) locals with
          | Some p -> p
          | None -> fail "No working-tree package %s" name)
        roots
    else
      let beneath p =
        let dir = Unix.realpath (root / p.Stamp.project) in
        if cwd = root then not (String.starts_with ~prefix:"vendor/" p.project)
        else dir = cwd || String.starts_with ~prefix:(cwd ^ "/") dir
      in
      List.filter beneath locals
  in
  if selected = [] then fail "No project-root opam files beneath %s" cwd;
  let config = { config with overlays = overlay :: config.overlays } in
  let prepared =
    Runner.packages proc ~clock ~fs ~sys config
      ~roots:("dune" :: List.map (fun p -> p.Stamp.name) selected)
      ~all:false ~action
      ~test_roots:(List.map (fun p -> p.Stamp.name) selected)
      ~exclude:(List.map (fun p -> p.Stamp.name) locals)
      ~dry_run ()
  in
  {
    root;
    projects =
      List.map (fun p -> p.Stamp.project) selected
      |> List.sort_uniq String.compare;
    prepared;
  }

let execute t ~test ~profile ~jobs =
  let target project =
    "@"
    ^ (if project = "." then "" else project ^ "/")
    ^ if test then "runtest" else "all"
  in
  Unix.chdir t.root;
  Runner.exec_command t.prepared
    ([
       "dune";
       "build";
       "--root";
       t.root;
       "--profile";
       profile;
       "-j";
       string_of_int jobs;
     ]
    @ (if test then [ "--force" ] else [])
    @ List.map target t.projects)
