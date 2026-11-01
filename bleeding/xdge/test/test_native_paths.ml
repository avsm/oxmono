let check why value = if not value then failwith why

let () =
  let root = Filename.temp_file "xdge-native-" "" in
  Sys.remove root;
  let variables =
    [
      "XDG_CONFIG_HOME";
      "XDG_DATA_HOME";
      "XDG_CACHE_HOME";
      "XDG_STATE_HOME";
      "NATIVE_TEST_CONFIG_DIR";
      "NATIVE_TEST_DATA_DIR";
      "NATIVE_TEST_CACHE_DIR";
      "NATIVE_TEST_STATE_DIR";
    ]
  in
  let previous = List.map (fun name -> (name, Sys.getenv_opt name)) variables in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
        previous)
    (fun () ->
      List.iter (fun name -> Unix.putenv name "") variables;
      Unix.putenv "XDG_CONFIG_HOME" (Filename.concat root "config");
      Unix.putenv "XDG_DATA_HOME" (Filename.concat root "data");
      Unix.putenv "XDG_CACHE_HOME" (Filename.concat root "cache");
      Unix.putenv "XDG_STATE_HOME" (Filename.concat root "state");
      let compare () =
        Eio_main.run (fun env ->
            let native = Xdge.Paths.config_dir "native_test"
            and xdg = Xdge.create ~create_dirs:false env#fs "native_test" in
            check "native config agrees with Eio config"
              (native = Eio.Path.native_exn (Xdge.config_dir xdg));
            check "native data agrees with Eio data"
              (Xdge.Paths.data_dir "native_test"
              = Eio.Path.native_exn (Xdge.data_dir xdg));
            check "native cache agrees with Eio cache"
              (Xdge.Paths.cache_dir "native_test"
              = Eio.Path.native_exn (Xdge.cache_dir xdg));
            check "native state agrees with Eio state"
              (Xdge.Paths.state_dir "native_test"
              = Eio.Path.native_exn (Xdge.state_dir xdg)))
      in
      compare ();
      Unix.putenv "NATIVE_TEST_CONFIG_DIR" (Filename.concat root "override");
      compare ();
      check "application override appends app name"
        (Xdge.Paths.config_dir "native_test"
        = Filename.concat root "override/native_test");
      check "path queries create nothing" (not (Sys.file_exists root));
      check "relative expansion uses supplied base"
        (Xdge.Paths.expand ~base:root "relative"
        = Filename.concat root "relative");
      check "home expansion preserves suffix"
        (Xdge.Paths.expand ~base:root "~/notes"
        = Filename.concat (Xdge.Paths.home ()) "notes");
      Unix.putenv "NATIVE_TEST_CONFIG_DIR" "";
      Unix.putenv "XDG_CONFIG_HOME" "relative";
      (try
         ignore (Xdge.Paths.config_dir "native_test");
         failwith "relative XDG path accepted"
       with Xdge.Invalid_xdg_path _ -> ());
      print_endline
        "Native XDG paths, overrides and side-effect-free parity passed")
