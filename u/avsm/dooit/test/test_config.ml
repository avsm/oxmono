module C = Dooit.Config
open Dooit.Common

let check message condition = if not condition then failwith message

let refuses f =
  match f () with
  | _ -> failwith "Expected configuration refusal"
  | exception Error _ -> ()

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let webdav =
  "[webdav]\n\
   url='https://dav.example.test/files/'\n\
   subdir='tasks'\n\
   username='me'\n\
   password_file='password'\n"

let () =
  let base = Filename.temp_file "dooit-config-test-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let path = Filename.concat base "config/config.toml" in
      let root = Filename.concat base "tasks" in
      let parse = C.parse ~path ~default_root:root in
      let reference =
        parse
          "[webdav]\n\
           url='https://example.invalid/'\n\
           subdir='tasks'\n\
           username='test'\n\
           secret='sekrit:shared/dav'\n"
      in
      (match (C.require_webdav reference).secret with
      | C.Reference reference ->
          check "named reference remains independent of config directory"
            (Sekrit.Reference.to_string reference = "sekrit:shared/dav")
      | _ -> failwith "expected secret reference");
      check "empty config selects Git" ((parse "").backend = C.Git);
      check "Git section selects Git" ((parse "[git]\n").backend = C.Git);
      check "WebDAV section selects WebDAV" ((parse webdav).backend = C.Webdav);
      check "explicit Local needs no remote"
        ((parse "backend='local'\n").backend = C.Local);
      let both = "[git]\nremote='/tmp/tasks.git'\n" ^ webdav in
      refuses (fun () -> parse both);
      List.iter
        (fun backend ->
          let config =
            parse ("backend='" ^ C.backend_name backend ^ "'\n" ^ both)
          in
          check "explicit backend resolves multiple configured remotes"
            (config.backend = backend
            && config.git.remote = "/tmp/tasks.git"
            && Option.is_some config.webdav))
        [ C.Git; C.Webdav; C.Local ];
      List.iter
        (fun raw -> refuses (fun () -> parse raw))
        [
          "backend='unknown'\n";
          "backend=3\n";
          "backend='Git'\n";
          "backend='webdav'\n";
          "backend='local'\n[git]\nbranch=3\n";
        ];
      ignore (C.require_git (parse ""));
      ignore (C.require_webdav (parse webdav));
      refuses (fun () -> C.require_git (parse webdav));
      refuses (fun () -> C.require_git (parse "backend='local'\n"));
      refuses (fun () -> C.require_webdav (parse ("backend='git'\n" ^ both)));
      let config = C.defaults ~fs ~path ~root ~backend:C.Local () in
      check "defaults do not create config or store"
        ((not (exists (Filename.dirname path))) && not (exists root));
      refuses (fun () -> C.load ~fs ~path ());
      refuses (fun () -> C.defaults ~fs ~path ~backend:C.Webdav ());
      C.save_new config;
      let loaded = C.load ~fs ~path () in
      check "saved Local selection and root reload exactly"
        (loaded.backend = C.Local && loaded.root = root
        && loaded.git = Dooit.Git.default);
      check "new config is private" ((Unix.stat path).st_perm land 0o077 = 0);
      check "saving config does not initialize the task store"
        (not (exists root));
      let override = Filename.concat base "override" in
      check "root override remains transient"
        ((C.load ~fs ~path ~root:override ()).root = override
        && (C.load ~fs ~path ()).root = root);
      let previous = read path ^ "\n# user's note\n" in
      atomic_write path previous;
      refuses (fun () ->
          C.save_new { config with root = override; backend = C.Git });
      check "existing config is never overwritten" (read path = previous);
      let invalid_path = Filename.concat base "invalid/config.toml" in
      refuses (fun () ->
          C.save_new { config with path = invalid_path; backend = C.Webdav });
      check "invalid defaults cannot create partial config"
        (not (exists (Filename.dirname invalid_path)));
      let dav_path = Filename.concat base "dav/config.toml" in
      let dav =
        C.parse ~path:dav_path ~default_root:root
          ("root='../tasks'\n" ^ webdav
         ^ "[jmap]\n\
            profile='personal'\n\
            account='account-1'\n\
            service='https://mail.example.test/'\n")
      in
      C.save_new dav;
      let saved = C.load ~fs ~path:dav_path () in
      check "WebDAV and source settings survive config creation"
        (saved.backend = C.Webdav && saved.webdav = dav.webdav
        && saved.jmap_profile = dav.jmap_profile
        && saved.jmap_account = dav.jmap_account
        && saved.jmap_service = dav.jmap_service);
      let variables =
        [ "HOME"; "XDG_CONFIG_HOME"; "XDG_DATA_HOME"; "XDG_RUNTIME_DIR" ]
      in
      let previous =
        List.map (fun key -> (key, Sys.getenv_opt key)) variables
      in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun (key, value) ->
              Unix.putenv key (Option.value value ~default:""))
            previous)
        (fun () ->
          Unix.putenv "HOME" base;
          Unix.putenv "XDG_CONFIG_HOME" (Filename.concat base "xdg-config");
          Unix.putenv "XDG_DATA_HOME" (Filename.concat base "xdg-data");
          let runtime = Filename.concat base "runtime" in
          Unix.mkdir runtime 0o700;
          Unix.putenv "XDG_RUNTIME_DIR" runtime;
          let defaults = C.defaults ~fs () and loaded = C.load ~fs () in
          let worker_defaults =
            Eio_unix.run_in_systhread (fun () -> C.defaults ~fs ())
          in
          check "native defaults are safe in a worker with a runtime directory"
            (worker_defaults = defaults);
          check "missing default config agrees with defaults constructor"
            (defaults = loaded && loaded.backend = C.Git);
          check "default loading creates no directories"
            ((not (exists (Filename.dirname loaded.path)))
            && not (exists loaded.root));
          let relative =
            C.defaults ~fs ~path:"relative/config.toml" ~root:"relative/tasks"
              ()
          in
          check "explicit relative paths resolve against current directory"
            (relative.path
             = Filename.concat (Sys.getcwd ()) "relative/config.toml"
            && relative.root = Filename.concat (Sys.getcwd ()) "relative/tasks"
            );
          C.save_new defaults;
          check "default Git configuration persists" (C.load ~fs () = defaults));
      print_endline "Dooit configuration tests passed")
