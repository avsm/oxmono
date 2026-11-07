let check name condition = if not condition then failwith name

let () =
  let name = "TESTENV_PROBE" in
  Testenv.set name None;
  check "unset before" (Sys.getenv_opt name = None);
  Testenv.with_bindings
    [ (name, Some "inner") ]
    (fun () -> check "bound inside" (Sys.getenv_opt name = Some "inner"));
  check "unset after" (Sys.getenv_opt name = None);
  Testenv.set name (Some "outer");
  (try
     Testenv.with_bindings
       [ (name, None) ]
       (fun () ->
         check "unset inside" (Sys.getenv_opt name = None);
         raise Exit)
   with Exit -> ());
  check "restored after raise" (Sys.getenv_opt name = Some "outer");
  Testenv.set name None;
  (match Testenv.set "BAD\000NAME" (Some "x") with
  | () -> failwith "null byte accepted"
  | exception Invalid_argument _ -> ());
  let previous_home = Sys.getenv_opt "HOME" in
  let root =
    Testenv.with_xdg (fun root ->
        check "home is root" (Sys.getenv_opt "HOME" = Some root);
        check "config under root"
          (Sys.getenv_opt "XDG_CONFIG_HOME"
          = Some (Filename.concat root "config"));
        check "runtime mode"
          ((Unix.stat (Filename.concat root "runtime")).Unix.st_perm = 0o700);
        check "data dirs unset" (Sys.getenv_opt "XDG_DATA_DIRS" = None);
        Out_channel.with_open_bin (Filename.concat root "config/f") (fun oc ->
            output_string oc "x");
        root)
  in
  check "home restored" (Sys.getenv_opt "HOME" = previous_home);
  check "root removed" (not (Sys.file_exists root));
  print_endline "testenv: ok"
