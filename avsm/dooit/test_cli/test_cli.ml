open Dooit
open Common

let check message value = if not value then failwith message

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let cli = absolute Sys.argv.(1) in
  let base = Filename.temp_file "dooit-cli-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove base)
    (fun () ->
      let config_home = Filename.concat base "config"
      and data_home = Filename.concat base "data" in
      let overrides =
        [
          ("XDG_CONFIG_HOME", config_home);
          ("XDG_DATA_HOME", data_home);
          ("GIT_CONFIG_GLOBAL", "/dev/null");
          ("GIT_CONFIG_NOSYSTEM", "1");
          ("GIT_AUTHOR_NAME", "Dooit CLI Test");
          ("GIT_AUTHOR_EMAIL", "dooit@example.invalid");
          ("GIT_COMMITTER_NAME", "Dooit CLI Test");
          ("GIT_COMMITTER_EMAIL", "dooit@example.invalid");
        ]
      in
      let environment =
        Array.of_list
          (List.map (fun (key, value) -> key ^ "=" ^ value) overrides
          @ List.filter
              (fun value ->
                not
                  (List.exists
                     (fun (key, _) ->
                       String.starts_with ~prefix:(key ^ "=") value)
                     overrides))
              (Array.to_list (Unix.environment ())))
      in
      let run ?(success = true) command =
        let stdout = Filename.concat base "stdout"
        and stderr = Filename.concat base "stderr" in
        let output =
          Unix.openfile stdout
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
            0o600
        and errors =
          Unix.openfile stderr
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
            0o600
        in
        let pid =
          Fun.protect
            ~finally:(fun () ->
              Unix.close output;
              Unix.close errors)
            (fun () ->
              Unix.create_process_env (List.hd command) (Array.of_list command)
                environment Unix.stdin output errors)
        in
        let rec wait () =
          try snd (Unix.waitpid [] pid)
          with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        in
        let status = wait () in
        let output = read stdout and errors = read stderr in
        if status = Unix.WEXITED 0 <> success then
          failwith (String.concat " " command ^ "\n" ^ output ^ errors);
        if success then output else errors
      in
      let dooit ?success args = run ?success (cli :: args) in
      let status = dooit [ "status"; "--json" ] |> json in
      check "status is read-only before setup"
        (get "initialized" status = `Bool false
        && (not (exists config_home))
        && not (exists data_home));
      let help = dooit [ "--help=plain" ] in
      List.iter
        (fun command ->
          check ("discoverable " ^ command) (contains help command))
        [ "init"; "sync"; "push"; "pull"; "task"; "email"; "maintenance" ];
      ignore (dooit ~success:false [ "git"; "push" ]);
      ignore (dooit ~success:false [ "config"; "init" ]);
      let initialized = dooit [ "init"; "--json" ] |> json in
      check "init defaults to Git" (field "backend" initialized = "git");
      let config = Filename.concat config_home "dooit/config.toml" in
      let a = Filename.concat data_home "dooit" in
      check "single init prepares config, metadata and repository"
        (exists config
        && exists (Filename.concat a "store.json")
        && exists (Filename.concat a ".git"));
      let original_config = read config in
      ignore (dooit [ "init" ]);
      check "repeated init preserves config" (read config = original_config);
      ignore (dooit ~success:false [ "init"; "--backend"; "local" ]);
      check "config show is human readable"
        (String.starts_with ~prefix:"Config:" (dooit [ "config"; "show" ]));
      let remote = Filename.concat base "remote.git" in
      ignore (run [ "git"; "init"; "--bare"; "--initial-branch=main"; remote ]);
      let settings =
        Printf.sprintf
          "backend=\"git\"\nroot=%S\n[git]\nremote=%S\nbranch=\"main\"\n" a
          remote
      in
      atomic_write config settings;
      ignore (run [ "git"; "-C"; a; "remote"; "set-url"; "origin"; remote ]);
      let first =
        dooit [ "task"; "add"; "First task"; "--tag"; "inbox"; "--json" ]
        |> json
      in
      let first_id = field "id" first in
      let synced = dooit [ "sync"; "--json" ] |> json in
      check "sync dispatches to Git" (field "backend" synced = "git");
      let remote_note id =
        run [ "git"; "--git-dir=" ^ remote; "show"; "main:notes/" ^ id ^ ".md" ]
        |> Doc.parse
      in
      check "first sync commits and pushes the task"
        (Doc.title (remote_note first_id) = "First task");
      let status = dooit [ "status"; "--json" ] |> json in
      check "status reports clean synchronized store"
        (get "dirty" (get "git" status) = `Bool false);
      let b = Filename.concat base "copy" in
      ignore (dooit [ "clone"; "--root"; b ]);
      check "clone preserves identity" ((Store.open_ a).id = (Store.open_ b).id);
      ignore
        (dooit
           [
             "task";
             "update";
             first_id;
             "--status";
             "blocked";
             "--due";
             "2026-11-30";
             "--add-tag";
             "waiting";
             "--remove-tag";
             "inbox";
           ]);
      let second =
        dooit [ "task"; "add"; "Second task"; "--root"; b; "--json" ] |> json
      in
      ignore (dooit [ "sync"; "--root"; b ]);
      ignore (dooit [ "sync" ]);
      check "sync merges disjoint local and remote changes"
        (Doc.status (remote_note first_id) = "blocked"
        && Doc.title (Store.get (Store.open_ a) (field "id" second))
           = "Second task");
      let found =
        dooit
          [
            "task";
            "list";
            "First";
            "--status";
            "blocked";
            "--tag";
            "waiting";
            "--json";
          ]
        |> json
      in
      check "list combines text and metadata filters"
        (List.length (items "notes" found) = 1);
      check "task list shows the due date"
        (contains (dooit [ "task"; "list"; "First" ]) "due 2026-11-30");
      let before = (Store.get (Store.open_ a) first_id).raw in
      ignore
        (dooit ~success:false
           [
             "task";
             "update";
             first_id;
             "--title";
             "Must not save";
             "--status";
             "invalid";
           ]);
      ignore
        (dooit ~success:false
           [ "task"; "update"; first_id; "--due"; "2026-11-30"; "--clear-due" ]);
      check "invalid compound updates are atomic"
        ((Store.get (Store.open_ a) first_id).raw = before);
      ignore (dooit [ "task"; "update"; first_id; "--clear-due" ]);
      check "clearing the due date removes it from the list"
        (not (contains (dooit [ "task"; "list"; "First" ]) "due 2026-11-30"));
      ignore (dooit [ "task"; "done"; first_id ]);
      ignore (dooit [ "push" ]);
      ignore (dooit [ "pull"; "--root"; b ]);
      check "top-level directional commands work"
        (Doc.status (Store.get (Store.open_ b) first_id) = "done");
      let ordered = dooit [ "task"; "list"; "--root"; b; "--json" ] |> json in
      check "completed tasks follow unfinished tasks after synchronization"
        (List.map (field "id") (items "notes" ordered)
        = [ field "id" second; first_id ]);
      ignore (dooit [ "task"; "delete"; first_id ]);
      ignore (dooit [ "sync" ]);
      check "delete retains remote tombstone"
        (Doc.deleted (remote_note first_id));
      ignore (dooit [ "task"; "restore"; first_id ]);
      let local_config = Filename.concat base "local.toml" in
      let local_root = Filename.concat base "local" in
      ignore
        (dooit
           [
             "init";
             "--config";
             local_config;
             "--root";
             local_root;
             "--backend";
             "local";
           ]);
      let local args = dooit (args @ [ "--config"; local_config ]) in
      check "local init does not create Git"
        (not (exists (Filename.concat local_root ".git")));
      ignore (local [ "task"; "add"; "Offline task" ]);
      check "local sync explains its behavior"
        (contains (local [ "sync" ]) "no remote");
      check "directional sync refuses wrong backend"
        (contains
           (dooit ~success:false [ "push"; "--config"; local_config ])
           "local");
      let webdav_config = Filename.concat base "webdav.toml" in
      atomic_write webdav_config
        (Printf.sprintf
           "root=%S\n\
            [webdav]\n\
            url=\"https://example.invalid/\"\n\
            subdir=\"tasks\"\n\
            username=\"test\"\n\
            password_file=\"missing-secret\"\n"
           (Filename.concat base "dav"));
      let webdav args = dooit (args @ [ "--config"; webdav_config ]) in
      ignore (webdav [ "init"; "--backend"; "webdav" ]);
      check "WebDAV-only configuration selects WebDAV"
        (field "backend" (webdav [ "config"; "show"; "--json" ] |> json)
        = "webdav");
      check "WebDAV sync dispatches to credential validation, not Git"
        (contains
           (dooit ~success:false [ "sync"; "--config"; webdav_config ])
           "missing-secret");
      check "WebDAV push gives backend error, not a request for Git setup"
        (contains
           (dooit ~success:false [ "push"; "--config"; webdav_config ])
           "webdav");
      print_endline
        "Dooit CLI setup, backend dispatch, tasks and synchronization passed")
