let check why value = if not value then failwith why

let () =
  let root = Filename.temp_file "xdge-native-" "" in
  Sys.remove root;
  let unset =
    List.map
      (fun name -> (name, None))
      [
        "NATIVE_TEST_CONFIG_DIR";
        "NATIVE_TEST_DATA_DIR";
        "NATIVE_TEST_CACHE_DIR";
        "NATIVE_TEST_STATE_DIR";
      ]
  in
  let bindings =
    [
      ("XDG_CONFIG_HOME", Some (Filename.concat root "config"));
      ("XDG_DATA_HOME", Some (Filename.concat root "data"));
      ("XDG_CACHE_HOME", Some (Filename.concat root "cache"));
      ("XDG_STATE_HOME", Some (Filename.concat root "state"));
    ]
  in
  Testenv.with_bindings (unset @ bindings) (fun () ->
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
      check "XDG_CONFIG_HOME appends app name"
        (Xdge.Paths.config_dir "native_test"
        = Filename.concat root "config/native_test");
      check "XDG_STATE_HOME appends app name"
        (Xdge.Paths.state_dir "native_test"
        = Filename.concat root "state/native_test");
      Testenv.set "NATIVE_TEST_CONFIG_DIR"
        (Some (Filename.concat root "override"));
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
      Testenv.set "NATIVE_TEST_CONFIG_DIR" None;
      Testenv.set "XDG_CONFIG_HOME" (Some "relative");
      (try
         ignore (Xdge.Paths.config_dir "native_test");
         failwith "relative XDG path accepted"
       with Xdge.Invalid_xdg_path _ -> ());
      print_endline
        "Native XDG paths, overrides and side-effect-free parity passed")
