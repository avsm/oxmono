(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let test_path_validation () =
  Printf.printf "Testing XDG path validation...\n";
  (* Test absolute path validation for environment variables *)
  let test_relative_path_rejection env_var relative_path =
    Printf.printf "Testing rejection of relative path in %s...\n" env_var;
    Unix.putenv env_var relative_path;
    try
      Eio_main.run @@ fun env ->
      let _ = Xdge.create env#fs "test_validation" in
      Printf.printf "ERROR: Should have rejected relative path\n";
      false
    with
    | Xdge.Invalid_xdg_path msg ->
        Printf.printf "SUCCESS: Correctly rejected relative path: %s\n" msg;
        true
    | exn ->
        Printf.printf "ERROR: Wrong exception: %s\n" (Printexc.to_string exn);
        false
  in
  let old_config_home = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let old_data_dirs = Sys.getenv_opt "XDG_DATA_DIRS" in
  let success1 =
    test_relative_path_rejection "XDG_CONFIG_HOME" "relative/path"
  in
  let success2 =
    test_relative_path_rejection "XDG_DATA_DIRS" "rel1:rel2:/abs/path"
  in
  (* Restore original env vars *)
  (match old_config_home with
  | Some v -> Unix.putenv "XDG_CONFIG_HOME" v
  | None -> ( try Unix.putenv "XDG_CONFIG_HOME" "" with _ -> ()));
  (match old_data_dirs with
  | Some v -> Unix.putenv "XDG_DATA_DIRS" v
  | None -> ( try Unix.putenv "XDG_DATA_DIRS" "" with _ -> ()));
  success1 && success2

let test_file_search () =
  Printf.printf "\nTesting XDG file search...\n";
  Eio_main.run @@ fun env ->
  let xdg = Xdge.create env#fs "search_test" in
  (* Create test files *)
  let config_file = Eio.Path.(Xdge.config_dir xdg / "test.conf") in
  let data_file = Eio.Path.(Xdge.data_dir xdg / "test.dat") in
  Eio.Path.save ~create:(`Or_truncate 0o644) config_file "config content";
  Eio.Path.save ~create:(`Or_truncate 0o644) data_file "data content";
  (* Test finding existing files *)
  (match Xdge.find_config_file xdg "test.conf" with
  | Some path ->
      let content = Eio.Path.load path in
      Printf.printf "Found config file: %s\n" (String.trim content)
  | None -> Printf.printf "ERROR: Config file not found\n");
  (match Xdge.find_data_file xdg "test.dat" with
  | Some path ->
      let content = Eio.Path.load path in
      Printf.printf "Found data file: %s\n" (String.trim content)
  | None -> Printf.printf "ERROR: Data file not found\n");
  (* Test non-existent file *)
  match Xdge.find_config_file xdg "nonexistent.conf" with
  | Some _ -> Printf.printf "ERROR: Should not have found nonexistent file\n"
  | None -> Printf.printf "Correctly handled nonexistent file\n"

let () =
  (* Check if we should run validation tests *)
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "--validate" then (
    let validation_success = test_path_validation () in
    test_file_search ();
    if validation_success then
      Printf.printf "\nAll path validation tests passed!\n"
    else Printf.printf "\nSome validation tests failed!\n")
  else
    (* Run original simple functionality test *)
    Eio_main.run @@ fun env ->
    let xdg = Xdge.create env#fs "path_test" in
    (* Test config subdirectory *)
    let profiles_path = Eio.Path.(Xdge.config_dir xdg / "profiles") in
    let profile_file = Eio.Path.(profiles_path / "default.json") in
    (try
       let content = Eio.Path.load profile_file in
       Printf.printf "config file content: %s" (String.trim content)
     with exn ->
       Printf.printf "config file error: %s" (Printexc.to_string exn));
    (* Test data subdirectory *)
    let db_path = Eio.Path.(Xdge.data_dir xdg / "databases") in
    let db_file = Eio.Path.(db_path / "main.db") in
    (try
       let content = Eio.Path.load db_file in
       Printf.printf "\ndata file content: %s" (String.trim content)
     with exn ->
       Printf.printf "\ndata file error: %s" (Printexc.to_string exn));
    (* Test cache subdirectory *)
    let cache_path = Eio.Path.(Xdge.cache_dir xdg / "thumbnails") in
    let cache_file = Eio.Path.(cache_path / "thumb1.png") in
    (try
       let content = Eio.Path.load cache_file in
       Printf.printf "\ncache file content: %s" (String.trim content)
     with exn ->
       Printf.printf "\ncache file error: %s" (Printexc.to_string exn));
    (* Test state subdirectory *)
    let logs_path = Eio.Path.(Xdge.state_dir xdg / "logs") in
    let log_file = Eio.Path.(logs_path / "app.log") in
    try
      let content = Eio.Path.load log_file in
      Printf.printf "\nstate file content: %s\n" (String.trim content)
    with exn ->
      Printf.printf "\nstate file error: %s\n" (Printexc.to_string exn)
