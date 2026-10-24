(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type config = { remote : string; branch : string }

let default = { remote = "fork.recoil.org:git/dooit"; branch = "main" }

let run ~process_mgr root ?(input = "") args =
  let output = Buffer.create 256 and errors = Buffer.create 256 in
  let env =
    Eio.Process.environment process_mgr
    |> Eio.Process.Env.override
         (List.map
            (fun name -> (name, None))
            [
              "GIT_DIR";
              "GIT_WORK_TREE";
              "GIT_INDEX_FILE";
              "GIT_COMMON_DIR";
              "GIT_OBJECT_DIRECTORY";
              "GIT_ALTERNATE_OBJECT_DIRECTORIES";
              "GIT_NAMESPACE";
            ])
  in
  let status =
    Eio.Switch.run (fun sw ->
        (* A missed SIGCHLD wakeup must not strand the store lock after Git
           exits. Keep Eio's process reaper responsive without reaping its
           children ourselves or changing the application's signal handler. *)
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec poll () =
              Eio_unix.sleep 0.25;
              Eio.Condition.broadcast Eio_unix.Process.sigchld;
              poll ()
            in
            poll ());
        let stdin, writer = Eio_unix.pipe sw in
        let reader, stdout = Eio_unix.pipe sw in
        let error_reader, stderr = Eio_unix.pipe sw in
        let fd = Eio_unix.Resource.fd in
        let child =
          Eio.Cancel.protect (fun () ->
              Eio_unix.Process.spawn_unix ~sw process_mgr ~env ~pgid:0
                ~fds:
                  [
                    (0, fd stdin, `Blocking);
                    (1, fd stdout, `Blocking);
                    (2, fd stderr, `Blocking);
                  ]
                ([
                   "git";
                   "--no-replace-objects";
                   "--no-optional-locks";
                   "-C";
                   root;
                   "-c";
                   "core.hooksPath=/dev/null";
                   "-c";
                   "commit.gpgSign=false";
                   "-c";
                   "protocol.ext.allow=never";
                 ]
                @ args))
        in
        let completed = ref false in
        Fun.protect
          ~finally:(fun () ->
            if not !completed then
              try Unix.kill (-Eio.Process.pid child) Sys.sigkill
              with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ())
          (fun () ->
            Eio.Flow.close stdin;
            Eio.Flow.close stdout;
            Eio.Flow.close stderr;
            Eio.Fiber.fork ~sw (fun () ->
                Fun.protect
                  ~finally:(fun () -> Eio.Flow.close writer)
                  (fun () -> Eio.Flow.copy_string input writer));
            Eio.Fiber.fork ~sw (fun () ->
                Eio.Flow.copy reader (Eio.Flow.buffer_sink output));
            Eio.Fiber.fork ~sw (fun () ->
                Eio.Flow.copy error_reader (Eio.Flow.buffer_sink errors));
            let status = Eio.Process.await child in
            completed := true;
            status))
  in
  (status, Buffer.contents output, String.trim (Buffer.contents errors))

let command ~process_mgr root ?input args =
  match run ~process_mgr root ?input args with
  | `Exited 0, output, _ -> output
  | _, _, error -> fail "Git %s failed: %s" (List.hd args) error

let nul s = String.split_on_char '\000' s |> List.filter (( <> ) "")
let line s = String.trim s

let note_name name =
  String.starts_with ~prefix:"notes/" name
  && String.ends_with ~suffix:".md" name
  && String.length name = 45
  && valid_uuid (String.sub name 6 36)

let allowed name = name = "store.json" || note_name name

let validate_config ~process_mgr root config =
  List.iter
    (fun value ->
      if
        value = ""
        || value.[0] = '-'
        || String.exists (fun c -> Char.code c < 32 || Char.code c = 127) value
      then fail "invalid Git remote or branch")
    [ config.remote; config.branch ];
  ignore
    (command ~process_mgr root
       [ "check-ref-format"; "--branch"; config.branch ])

let local_state_sync store =
  if
    Store.parse_metadata (read (Filename.concat store.Store.root "store.json"))
    <> store.id
  then fail "store identity changed";
  if Store.conflicts store <> [] then
    fail "resolve Dooit conflicts before Git synchronization";
  let ops = Store.load_state store "operations.json" (obj []) in
  if List.exists (fun (_, op) -> field "phase" op <> "done") (assoc ops) then
    fail "recover interrupted Dooit operations before Git synchronization";
  let notes, errors = Store.scan store in
  if errors <> [] then fail "fix invalid note files before Git synchronization";
  "store.json" :: List.map (fun note -> "notes/" ^ Doc.id note ^ ".md") notes

let local_state store =
  Eio_unix.run_in_systhread (fun () -> local_state_sync store)

let repo ~process_mgr store config =
  let root = store.Store.root in
  validate_config ~process_mgr root config;
  if not (exists (Filename.concat root ".git")) then fail "run dooit init first";
  let top =
    command ~process_mgr root [ "rev-parse"; "--show-toplevel" ] |> line
  in
  if Unix.realpath top <> root then
    fail "Git repository must be rooted at the Dooit store";
  let branch =
    command ~process_mgr root [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
    |> line
  in
  if branch <> config.branch then
    fail "Git branch differs from configured branch";
  List.iter
    (fun options ->
      let remote =
        command ~process_mgr root
          ([ "remote"; "get-url"; "--all" ] @ options @ [ "origin" ])
        |> line
      in
      if remote <> config.remote then
        fail "Git origin differs from configured remote")
    [ []; [ "--push" ] ];
  let names = command ~process_mgr root [ "ls-files"; "-z" ] |> nul in
  if List.exists (fun name -> not (allowed name)) names then
    fail "repository tracks files outside store.json and notes/*.md";
  if command ~process_mgr root [ "ls-files"; "--unmerged"; "-z" ] <> "" then
    fail "resolve Git conflicts before synchronization";
  List.iter
    (fun name ->
      let path =
        command ~process_mgr root [ "rev-parse"; "--git-path"; name ] |> line
      in
      let path =
        if Filename.is_relative path then Filename.concat root path else path
      in
      if exists path then
        fail "finish the current Git operation before synchronization")
    [
      "MERGE_HEAD";
      "CHERRY_PICK_HEAD";
      "REVERT_HEAD";
      "rebase-merge";
      "rebase-apply";
    ]

let exclude ~process_mgr root =
  let path =
    command ~process_mgr root [ "rev-parse"; "--git-path"; "info/exclude" ]
    |> line
  in
  let path =
    if Filename.is_relative path then Filename.concat root path else path
  in
  Eio_unix.run_in_systhread (fun () ->
      let previous = Option.value (read_opt path) ~default:"" in
      if not (List.mem "/.dooit/" (String.split_on_char '\n' previous)) then
        atomic_write path (previous ^ "\n/.dooit/\n"))

let setup ~process_mgr store config =
  Store.with_lock store (fun () ->
      let root = store.Store.root in
      validate_config ~process_mgr root config;
      ignore (local_state store);
      if not (exists (Filename.concat root ".git")) then
        ignore
          (command ~process_mgr root
             [ "init"; "--initial-branch=" ^ config.branch ]);
      (match run ~process_mgr root [ "remote"; "get-url"; "origin" ] with
      | `Exited 0, _, _ -> ()
      | `Exited 2, _, _ ->
          ignore
            (command ~process_mgr root
               [ "remote"; "add"; "--"; "origin"; config.remote ])
      | _, _, error -> fail "cannot inspect Git origin: %s" error);
      repo ~process_mgr store config;
      exclude ~process_mgr root;
      "Git origin configured; no commit or network request made")

let tree ~process_mgr root revision =
  command ~process_mgr root [ "ls-tree"; "-r"; "-z"; revision ]
  |> nul
  |> List.map (fun entry ->
         match String.split_on_char '\t' entry with
         | [ header; name ] -> (
             match String.split_on_char ' ' header with
             | [ "100644"; "blob"; oid ] when allowed name -> (name, oid)
             | _ -> fail "Git tree contains unsupported files: %s" name)
         | _ -> fail "invalid Git tree entry")

let validate_tree ~process_mgr root ?id ?(against = []) ?(history = true)
    revision =
  let history =
    if not history then []
    else
      command ~process_mgr root
        [
          "log";
          "--format=";
          "--name-only";
          "-z";
          "--full-history";
          "-m";
          revision;
        ]
      |> nul
  in
  if List.exists (fun path -> not (allowed path)) history then
    fail "Git history contains files outside store.json and notes/*.md";
  let entries = tree ~process_mgr root revision in
  let parents = List.map (tree ~process_mgr root) against in
  let objects = List.sort_uniq compare (List.concat (entries :: parents)) in
  if not (List.mem_assoc "store.json" entries) then
    fail "remote has no Dooit store metadata";
  let raw =
    command ~process_mgr root
      ~input:(String.concat "" (List.map (fun (_, oid) -> oid ^ "\n") objects))
      [ "cat-file"; "--batch" ]
  in
  Eio_unix.run_in_systhread (fun () ->
      let offset = ref 0 and store_id = ref "" in
      let notes = Hashtbl.create (List.length objects) in
      List.iter
        (fun (name, oid) ->
          let stop =
            match String.index_from_opt raw !offset '\n' with
            | Some stop -> stop
            | None -> fail "incomplete Git object output"
          in
          let header = String.sub raw !offset (stop - !offset) in
          let size =
            match String.split_on_char ' ' header with
            | [ _; "blob"; size ] -> int_of_string size
            | _ -> fail "invalid Git object header"
          in
          if size < 0 || size > 1024 * 1024 || size >= String.length raw - stop
          then fail "Git note or metadata exceeds size limit";
          let body = String.sub raw (stop + 1) size in
          offset := stop + size + 2;
          if name = "store.json" then (
            let parsed = Store.parse_metadata body in
            if !store_id <> "" && !store_id <> parsed then
              fail "Git synchronization changes store identity";
            store_id := parsed)
          else
            let note = Doc.parse body in
            if Doc.id note <> String.sub name 6 36 then
              fail "remote note filename and identity disagree";
            Hashtbl.replace notes oid note)
        objects;
      Option.iter
        (fun expected ->
          if expected <> !store_id then
            fail "remote belongs to a different Dooit store")
        id;
      let candidate = Hashtbl.of_seq (List.to_seq entries) in
      List.iter
        (List.iter (fun (name, oid) ->
             match Hashtbl.find_opt candidate name with
             | None ->
                 fail
                   "Git synchronization refuses removed notes; use Dooit \
                    tombstones"
             | Some next when name <> "store.json" ->
                 Doc.immutable (Hashtbl.find notes oid)
                   (Hashtbl.find notes next)
             | Some _ -> ()))
        parents;
      entries)

let head ~process_mgr root =
  match run ~process_mgr root [ "rev-parse"; "--verify"; "HEAD" ] with
  | `Exited 0, value, _ -> Some (line value)
  | `Exited 128, _, _ -> None
  | _, _, error -> fail "cannot inspect Git HEAD: %s" error

module Names = Set.Make (String)

let no_removed previous current =
  let names = Names.of_list (List.map fst current) in
  if List.exists (fun (name, _) -> not (Names.mem name names)) previous then
    fail "Git synchronization refuses removed notes; use Dooit tombstones"

let fetch ~process_mgr root config =
  let remote_ref = "refs/heads/" ^ config.branch in
  match
    run ~process_mgr root
      [ "ls-remote"; "--exit-code"; "--heads"; "--"; "origin"; remote_ref ]
  with
  | `Exited 2, _, _ -> None
  | `Exited 0, _, _ ->
      let local_ref = "refs/remotes/origin/" ^ config.branch in
      ignore
        (command ~process_mgr root
           [
             "fetch"; "--no-tags"; "--"; "origin"; remote_ref ^ ":" ^ local_ref;
           ]);
      Some
        (command ~process_mgr root [ "rev-parse"; "--verify"; local_ref ]
        |> line)
  | _, _, error -> fail "cannot inspect Git remote: %s" error

let ancestor ~process_mgr root older newer =
  match
    run ~process_mgr root [ "merge-base"; "--is-ancestor"; older; newer ]
  with
  | `Exited 0, _, _ -> true
  | `Exited 1, _, _ -> false
  | _, _, error -> fail "cannot compare Git histories: %s" error

let only_changed ~process_mgr root path =
  let changed =
    List.concat_map
      (fun args ->
        command ~process_mgr root (args @ [ "--"; "store.json"; "notes" ])
        |> nul)
      [
        [ "diff"; "--name-only"; "--no-renames"; "-z" ];
        [ "diff"; "--cached"; "--name-only"; "--no-renames"; "-z" ];
        [ "ls-files"; "--others"; "-z" ];
      ]
  in
  if List.exists (fun name -> name <> path) changed then
    fail
      "unrelated dirty notes or metadata must be resolved before retrying \
       synchronization"

let pull_locked ?(check = fun () -> ()) ~process_mgr store config =
  check ();
  repo ~process_mgr store config;
  ignore (local_state store);
  let root = store.Store.root in
  if
    command ~process_mgr root
      [
        "status";
        "--porcelain";
        "-z";
        "--untracked-files=all";
        "--";
        "store.json";
        "notes";
      ]
    <> ""
  then fail "commit or resolve dirty notes before pulling";
  let current =
    match head ~process_mgr root with
    | Some h -> h
    | None ->
        fail
          "run dooit push to initialize a new remote, or clone an existing \
           store"
  in
  check ();
  match fetch ~process_mgr root config with
  | None -> fail "remote branch does not exist"
  | Some incoming ->
      let files = validate_tree ~process_mgr root ~id:store.id incoming in
      check ();
      if ancestor ~process_mgr root incoming current then "Already up to date"
      else if not (ancestor ~process_mgr root current incoming) then
        fail "Git histories diverged; resolve them explicitly"
      else (
        no_removed (tree ~process_mgr root current) files;
        check ();
        ignore
          (command ~process_mgr root
             [ "merge"; "--ff-only"; "--no-autostash"; "--no-edit"; incoming ]);
        "Pulled notes by fast-forward")

let pull ~process_mgr store config =
  Store.with_lock store (fun () -> pull_locked ~process_mgr store config)

let commit_local ?only ~check ~process_mgr store =
  let files = local_state store and root = store.Store.root in
  Option.iter (only_changed ~process_mgr root) only;
  let current = head ~process_mgr root in
  Option.iter
    (fun h ->
      no_removed
        (validate_tree ~process_mgr root ~id:store.id h)
        (List.map (fun path -> (path, "")) files))
    current;
  check ();
  Option.iter (only_changed ~process_mgr root) only;
  check ();
  let files =
    match only with
    | None -> files
    | Some path ->
        if not (List.mem path files) then fail "operation note is missing";
        [ path ]
  in
  ignore
    (command ~process_mgr root
       ~input:(String.concat "" (List.map (fun path -> path ^ "\000") files))
       [ "add"; "--pathspec-from-file=-"; "--pathspec-file-nul" ]);
  check ();
  (match
     run ~process_mgr root
       ([ "diff"; "--cached"; "--quiet" ]
       @ match only with None -> [] | Some path -> [ "--"; path ])
   with
  | `Exited 0, _, _ -> ()
  | `Exited 1, _, _ ->
      check ();
      ignore
        (command ~process_mgr root
           ([ "commit"; "-m"; "Update Dooit task notes" ]
           @
           match only with None -> [] | Some path -> [ "--only"; "--"; path ]))
  | _, _, error -> fail "cannot inspect staged notes: %s" error);
  (* Attributes and clean filters can transform the validated working files.
         Validate the committed bytes and modes before sending any history. *)
  ignore
    (validate_tree ~process_mgr root ~id:store.id
       ~against:(Option.to_list current) "HEAD")

let push_locked ?only ?(check = fun () -> ()) ~process_mgr store config =
  check ();
  repo ~process_mgr store config;
  let root = store.Store.root in
  let verify_remote () =
    check ();
    match fetch ~process_mgr root config with
    | None -> ()
    | Some remote -> (
        ignore (validate_tree ~process_mgr root ~id:store.id remote);
        match head ~process_mgr root with
        | Some h when ancestor ~process_mgr root remote h -> ()
        | _ ->
            fail
              "pull the remote branch before pushing; divergent history \
               requires explicit resolution")
  in
  if only = None then verify_remote ();
  commit_local ?only ~check ~process_mgr store;
  if only <> None then verify_remote ();
  check ();
  ignore
    (command ~process_mgr root
       [
         "-c";
         "remote.origin.mirror=false";
         "push";
         "--no-force";
         "--no-follow-tags";
         "--";
         "origin";
         "HEAD:refs/heads/" ^ config.branch;
       ]);
  "Committed current notes and pushed origin"

let push ~process_mgr store config =
  Store.with_lock store (fun () -> push_locked ~process_mgr store config)

type status = {
  initialized : bool;
  branch : string option;
  dirty : bool;
  ahead : int option;
  behind : int option;
  conflicts : string list;
}

let status ~process_mgr store config =
  let root = store.Store.root in
  let conflicts =
    Eio_unix.run_in_systhread (fun () ->
        Store.conflicts store |> List.map (fun id -> "dooit:" ^ id))
  in
  if not (exists (Filename.concat root ".git")) then
    {
      initialized = false;
      branch = None;
      dirty = false;
      ahead = None;
      behind = None;
      conflicts;
    }
  else (
    validate_config ~process_mgr root config;
    let top =
      command ~process_mgr root [ "rev-parse"; "--show-toplevel" ] |> line
    in
    if Unix.realpath top <> root then
      fail "Git repository must be rooted at the Dooit store";
    let branch =
      match
        run ~process_mgr root [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
      with
      | `Exited 0, value, _ -> Some (line value)
      | `Exited 1, _, _ -> None
      | _, _, error -> fail "cannot inspect Git branch: %s" error
    in
    let dirty =
      command ~process_mgr root
        [
          "status";
          "--porcelain";
          "-z";
          "--untracked-files=all";
          "--";
          "store.json";
          "notes";
        ]
      <> ""
    in
    let unmerged =
      command ~process_mgr root
        [ "diff"; "--name-only"; "--diff-filter=U"; "-z" ]
      |> nul
    in
    let remote_ref = "refs/remotes/origin/" ^ config.branch in
    let ahead, behind =
      match head ~process_mgr root with
      | None -> (None, None)
      | Some _ -> (
          match
            run ~process_mgr root [ "rev-parse"; "--verify"; remote_ref ]
          with
          | `Exited 128, _, _ -> (None, None)
          | `Exited 0, _, _ -> (
              let counts =
                command ~process_mgr root
                  [
                    "rev-list";
                    "--left-right";
                    "--count";
                    "HEAD..." ^ remote_ref;
                  ]
                |> line
              in
              match String.split_on_char '\t' counts with
              | [ a; b ] -> (Some (int_of_string a), Some (int_of_string b))
              | _ -> fail "invalid Git ahead/behind counts")
          | _, _, error -> fail "cannot inspect cached Git remote: %s" error)
    in
    {
      initialized = true;
      branch;
      dirty;
      ahead;
      behind;
      conflicts = List.sort_uniq String.compare (conflicts @ unmerged);
    })

let sync ~process_mgr store config =
  Store.with_lock store (fun () ->
      repo ~process_mgr store config;
      commit_local ~check:(fun () -> ()) ~process_mgr store;
      let root = store.Store.root in
      let current =
        match head ~process_mgr root with
        | Some h -> h
        | None -> fail "synchronization did not create a local commit"
      in
      (match fetch ~process_mgr root config with
      | None -> ()
      | Some incoming ->
          ignore (validate_tree ~process_mgr root ~id:store.id incoming);
          if ancestor ~process_mgr root incoming current then ()
          else if ancestor ~process_mgr root current incoming then (
            ignore
              (validate_tree ~process_mgr root ~id:store.id ~against:[ current ]
                 incoming);
            ignore
              (command ~process_mgr root
                 [
                   "merge"; "--ff-only"; "--no-autostash"; "--no-edit"; incoming;
                 ]))
          else
            let merge_head =
              command ~process_mgr root
                [ "rev-parse"; "--git-path"; "MERGE_HEAD" ]
              |> line
            in
            let merge_head =
              if Filename.is_relative merge_head then
                Filename.concat root merge_head
              else merge_head
            in
            (* The repository was clean and had no merge in progress. Only
               abort the merge started here; the local commit remains intact. *)
            let finish () =
              Eio.Cancel.protect (fun () ->
                  if exists merge_head then
                    ignore (command ~process_mgr root [ "merge"; "--abort" ]))
            in
            Fun.protect ~finally:finish (fun () ->
                (match
                   run ~process_mgr root
                     [
                       "merge";
                       "--no-ff";
                       "--no-commit";
                       "--no-autostash";
                       "--no-edit";
                       incoming;
                     ]
                 with
                | `Exited 0, _, _ -> ()
                | _, output, error ->
                    fail
                      "synchronization conflict; local changes remain \
                       committed. Run git -C %s merge %s, resolve the \
                       conflicts and commit, then run dooit sync. %s"
                      (Filename.quote root)
                      (Filename.quote ("origin/" ^ config.branch))
                      (String.trim (output ^ "\n" ^ error)));
                let candidate =
                  command ~process_mgr root [ "write-tree" ] |> line
                in
                ignore
                  (validate_tree ~process_mgr root ~id:store.id ~history:false
                     ~against:[ current; incoming ] candidate);
                ignore (local_state store);
                ignore
                  (command ~process_mgr root
                     [ "commit"; "-m"; "Synchronize Dooit task notes" ]);
                ignore
                  (validate_tree ~process_mgr root ~id:store.id
                     ~against:[ current; incoming ] "HEAD")));
      ignore (local_state store);
      ignore
        (command ~process_mgr root
           [
             "-c";
             "remote.origin.mirror=false";
             "push";
             "--no-force";
             "--no-follow-tags";
             "--";
             "origin";
             "HEAD:refs/heads/" ^ config.branch;
           ]);
      "Synchronized task notes with origin")

exception
  Pending of {
    id : string;
    operation : string;
    revision : string;
    message : string;
  }

let apply ~process_mgr ~check store config ~id ~expected ~operation ~payload
    change =
  check_uuid id;
  check_uuid operation;
  Store.with_lock store (fun () ->
      check ();
      let prior =
        Eio_unix.run_in_systhread (fun () ->
            let ops = Store.load_state store "operations.json" (obj []) in
            match find operation ops with
            | None -> None
            | Some op ->
                if field "request" op <> Store.request ~id ~expected ~payload
                then fail "operation UUID already used with different arguments";
                let note = Doc.parse (Store.object_ store (field "after" op)) in
                Some (op, note))
      in
      let synced op =
        match find "git" op with
        | Some receipt ->
            if
              field "remote" receipt <> config.remote
              || field "branch" receipt <> config.branch
            then
              fail
                "operation was already synchronized to a different Git remote \
                 or branch";
            true
        | None -> false
      in
      match prior with
      | Some (op, note) when synced op ->
          check ();
          note
      | _ -> (
          let pending note message =
            raise
              (Pending { id; operation; revision = Doc.revision note; message })
          in
          let note =
            match prior with
            | None ->
                ignore (pull_locked ~check ~process_mgr store config);
                check ();
                Eio_unix.run_in_systhread (fun () ->
                    Store.apply_locked store ~id ~expected ~operation ~payload
                      change)
            | Some (op, note) -> (
                try
                  repo ~process_mgr store config;
                  only_changed ~process_mgr store.root ("notes/" ^ id ^ ".md");
                  check ();
                  Eio_unix.run_in_systhread (fun () ->
                      let current = Store.revision_opt (Store.raw store id) in
                      let after = Some (Doc.revision note) in
                      let before = string_opt (get "before" op) in
                      if
                        current <> after
                        && (field "phase" op = "done" || current <> before)
                      then
                        fail
                          "operation note changed after local application; \
                           resolve it before retrying";
                      Store.apply_locked store ~id ~expected ~operation ~payload
                        change)
                with
                | Error message -> pending note message
                | (Eio.Io _ | Unix.Unix_error _ | Sys_error _) as exn ->
                    pending note (Printexc.to_string exn))
          in
          try
            check ();
            ignore
              (push_locked
                 ~only:("notes/" ^ id ^ ".md")
                 ~check ~process_mgr store config);
            (* Persist confirmation before observing cancellation again. A retry
                must distinguish a pushed operation from later unrelated edits. *)
            Eio.Cancel.protect (fun () ->
                Eio_unix.run_in_systhread (fun () ->
                    let ops =
                      Store.load_state store "operations.json" (obj [])
                    in
                    let op = get operation ops in
                    let receipt =
                      obj
                        [
                          ("remote", str config.remote);
                          ("branch", str config.branch);
                        ]
                    in
                    Store.save_state store "operations.json"
                      (set operation (set "git" receipt op) ops)));
            check ();
            note
          with
          | Error message -> pending note message
          | (Eio.Io _ | Unix.Unix_error _ | Sys_error _) as exn ->
              pending note (Printexc.to_string exn)))

let rec remove_tree path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let clone ~process_mgr ~root config =
  let root = absolute root in
  if exists root then fail "clone destination already exists";
  let parent = Filename.dirname root in
  Eio_unix.run_in_systhread (fun () -> mkdir parent);
  let temporary = Filename.temp_file ~temp_dir:parent ".dooit-clone-" "" in
  Unix.unlink temporary;
  Unix.mkdir temporary 0o700;
  Fun.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
          Eio_unix.run_in_systhread (fun () ->
              if exists temporary then remove_tree temporary)))
    (fun () ->
      validate_config ~process_mgr temporary config;
      ignore
        (command ~process_mgr temporary
           [ "init"; "--initial-branch=" ^ config.branch ]);
      ignore
        (command ~process_mgr temporary
           [ "remote"; "add"; "--"; "origin"; config.remote ]);
      let incoming =
        match fetch ~process_mgr temporary config with
        | Some h -> h
        | None -> fail "remote branch does not exist"
      in
      ignore (validate_tree ~process_mgr temporary incoming);
      ignore
        (command ~process_mgr temporary
           [ "checkout"; "-B"; config.branch; incoming ]);
      mkdir (Filename.concat temporary "notes");
      let store = Store.open_ temporary in
      let committed_metadata =
        command ~process_mgr temporary [ "show"; incoming ^ ":store.json" ]
      in
      if Store.parse_metadata committed_metadata <> store.id then
        fail "Git checkout changed the remote store identity";
      ignore (local_state store);
      exclude ~process_mgr temporary;
      if exists root then fail "clone destination appeared during download";
      Eio_unix.run_in_systhread (fun () ->
          if exists root then fail "clone destination appeared during download";
          Unix.rename temporary root;
          fsync_dir parent);
      Store.open_ root)
