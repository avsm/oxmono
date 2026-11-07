let check name condition = if not condition then failwith name

let () =
  let name = "TESTENV_PROBE" in
  Xdge_testenv.set name None;
  check "unset before" (Sys.getenv_opt name = None);
  Xdge_testenv.with_bindings
    [ (name, Some "inner") ]
    (fun () -> check "bound inside" (Sys.getenv_opt name = Some "inner"));
  check "unset after" (Sys.getenv_opt name = None);
  Xdge_testenv.set name (Some "outer");
  (try
     Xdge_testenv.with_bindings
       [ (name, None) ]
       (fun () ->
         check "unset inside" (Sys.getenv_opt name = None);
         raise Exit)
   with Exit -> ());
  check "restored after raise" (Sys.getenv_opt name = Some "outer");
  Xdge_testenv.set name None;
  (match Xdge_testenv.set "BAD\000NAME" (Some "x") with
  | () -> failwith "null byte accepted"
  | exception Invalid_argument _ -> ());
  let previous_home = Sys.getenv_opt "HOME" in
  let root =
    Xdge_testenv.with_xdg (fun root ->
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

let () =
  let outside = Filename.temp_dir "testenv-outside-" "" in
  let keep = Filename.concat outside "keep" in
  Out_channel.with_open_bin keep (fun oc -> output_string oc "x");
  let root =
    Xdge_testenv.with_xdg (fun root ->
        Unix.symlink outside (Filename.concat root "data/link");
        Unix.symlink
          (Filename.concat root "missing")
          (Filename.concat root "data/dangling");
        root)
  in
  check "root with links removed" (not (Sys.file_exists root));
  check "link target kept" (Sys.file_exists keep);
  Sys.remove keep;
  Sys.rmdir outside;
  print_endline "testenv: links ok"
