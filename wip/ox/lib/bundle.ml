open Support

let run proc ~clock ~fs ~sys config ~owner ~prepared ~profile ~output =
  if exists output then fail "Output already exists: %s" output;
  let selected = Layout.select proc ~repo:"." ~owner:(Some owner) () in
  let config = Layout.locked_config ~repo:selected.root config in
  let stage = Filename.temp_file "ox-bundle-" "" in
  Unix.unlink stage;
  Fun.protect
    ~finally:(fun () -> remove_tree stage)
    (fun () ->
      Layout.stage proc selected ~output:stage;
      command proc [ "git"; "init"; "-q"; stage ];
      let env =
        if prepared then clean_env ()
        else
          let cwd = Sys.getcwd () in
          Fun.protect
            ~finally:(fun () -> Unix.chdir cwd)
            (fun () ->
              Unix.chdir stage;
              let workspace =
                Workspace.prepare proc ~clock ~fs ~sys config
                  ~roots:selected.roots ~action:Runner.Build ~dry_run:false
              in
              Runner.environment workspace.prepared)
      in
      let packages =
        List.map (fun p -> p.Stamp.name) selected.snapshot.packages
      in
      command ~env proc
        [
          "dune";
          "build";
          "--root";
          stage;
          "--profile";
          profile;
          "--only-packages";
          String.concat "," packages;
          "@install";
          "-j";
          string_of_int config.jobs;
        ];
      publish_dir output (fun tmp ->
          command ~env proc
            ([
               "dune";
               "install";
               "--root";
               stage;
               "--profile";
               profile;
               "--prefix";
               tmp;
               "--relocatable";
             ]
            @ packages);
          let bin = tmp / "bin" in
          let binaries = if exists bin then sorted_dir bin else [] in
          List.iter
            (fun p ->
              if List.mem p.Stamp.name selected.roots then
                List.iter
                  (fun name ->
                    if not (List.mem name binaries) then
                      fail "Selected command was not installed: %s (%s)" name
                        p.name)
                  p.binaries)
            selected.snapshot.packages;
          write
            (tmp / "ox-selection.json")
            (OpamJson.to_string (Layout.json selected));
          write (tmp / "binaries.json")
            (OpamJson.to_string (`A (List.map (fun s -> `String s) binaries)));
          log "Installed %s's commands in %s" owner output))
