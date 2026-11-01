open Crowthebot
module C = Dooit.Common

let check name condition = if not condition then failwith name
let ok = function Ok value -> value | Error message -> failwith message
let noop () = ()
let admin = "@admin:example.org"
let room = "!tasks:example.org"

let contains text needle =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let rec remove path =
  if Sys.is_directory path then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let () =
  let root = Filename.temp_file "crow-dooit-test" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let config = Filename.concat root "config"
  and data = Filename.concat root "data" in
  Unix.mkdir config 0o700;
  Unix.mkdir data 0o700;
  Unix.putenv "XDG_CONFIG_HOME" config;
  Unix.putenv "XDG_DATA_HOME" data;
  Unix.putenv "GIT_CONFIG_GLOBAL" "/dev/null";
  Unix.putenv "GIT_CONFIG_NOSYSTEM" "1";
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env in
      let process_mgr = Eio.Stdenv.process_mgr env in
      let git args =
        let output = Buffer.create 128 and errors = Buffer.create 128 in
        Eio.Process.run process_mgr
          ~stdout:(Eio.Flow.buffer_sink output)
          ~stderr:(Eio.Flow.buffer_sink errors)
          ("git" :: args);
        String.trim (Buffer.contents output)
      in
      let store =
        Store.create ~now:(fun () -> 0.) (Sqlite3_eio.open_memory ~sw ()) ~admin
      in
      Store.add_room store room;
      let load () =
        Tasks.load ~fs ~process_mgr ~store ~profile:"test" ~max_bytes:4096 ()
      in
      check "missing task store disables tools" (load () = None);
      check "discovery creates no Dooit directories"
        ((not (Sys.file_exists (Filename.concat data "dooit")))
        && not (Sys.file_exists (Filename.concat config "dooit")));
      let first = Dooit.Store.init (Filename.concat root "first")
      and second = Dooit.Store.init (Filename.concat root "second") in
      let remote (task_store : Dooit.Store.t) = task_store.root ^ ".git" in
      let git_config task_store =
        Dooit.Git.{ remote = remote task_store; branch = "main" }
      in
      List.iter
        (fun (task_store : Dooit.Store.t) ->
          ignore (git [ "init"; "--bare"; remote task_store ]);
          ignore
            (Dooit.Git.setup ~process_mgr task_store (git_config task_store));
          ignore
            (git [ "-C"; task_store.root; "config"; "user.name"; "Crow test" ]);
          ignore
            (git
               [
                 "-C";
                 task_store.root;
                 "config";
                 "user.email";
                 "crow-test@example.org";
               ]);
          ignore
            (Dooit.Git.push ~process_mgr task_store (git_config task_store)))
        [ first; second ];
      let config_dir = Filename.concat config "dooit" in
      Unix.mkdir config_dir 0o700;
      let config_file = Filename.concat config_dir "config.toml" in
      let write_config raw =
        Out_channel.with_open_bin config_file (fun oc -> output_string oc raw)
      in
      let configure (task_store : Dooit.Store.t) =
        write_config
          (Printf.sprintf "root = %S\n[git]\nremote = %S\nbranch = \"main\"\n"
             task_store.root (remote task_store))
      in
      write_config
        (Printf.sprintf "root = %S\n[git]\nremote = %S\n" first.root
           (Filename.concat root "unreachable.git"));
      check "startup does not contact the configured remote"
        (Option.is_some (load ()));
      configure first;
      let before = Sys.readdir (Dooit.Store.state_dir first) in
      let discovered = Option.get (load ()) in
      check "loading existing store makes no state writes"
        (before = Sys.readdir (Dooit.Store.state_dir first));
      let invoke ?(actor = admin) ?(in_room = room) ?(event = "$event")
          ?(source_event = "$source") ?(call_id = "call") ?(check = noop) tasks
          name arguments =
        Tasks.invoke tasks ~actor ~room:in_room ~event ~source_event ~call_id
          ~check name arguments
      in
      let create_args = {|{"title":"Follow up","body":"Original context"}|} in
      check "unknown actor cannot create tasks"
        (Result.is_error
           (invoke ~actor:"@stranger:example.org" discovered "dooit_create"
              create_args));
      check "allowed actor cannot use unapproved room"
        (Result.is_error
           (invoke ~in_room:"!other:example.org" discovered "dooit_create"
              create_args));
      check "denied calls leave notes empty" (fst (Dooit.Store.scan first) = []);
      let created = ok (invoke discovered "dooit_create" create_args) in
      check "task creation is immediately pushed"
        (contains
           (git
              [
                "--git-dir=" ^ remote first;
                "ls-tree";
                "-r";
                "--name-only";
                "main";
              ])
           ("notes/" ^ C.field "id" (C.json created) ^ ".md"));
      check "same host call is retry safe"
        (ok (invoke discovered "dooit_create" create_args) = created
        && List.length (fst (Dooit.Store.scan first)) = 1);
      check "same call cannot silently change arguments"
        (Result.is_error
           (invoke discovered "dooit_create" {|{"title":"Changed"}|}));
      let id = C.field "id" (C.json created)
      and revision = C.field "revision" (C.json created) in
      let update =
        Printf.sprintf
          {|{"id":%S,"revision":%S,"operations":[{"op":"set_status","value":"done"}]}|}
          id revision
      in
      let changed =
        ok (invoke ~call_id:"update" discovered "dooit_update" update)
      in
      check "task update is immediately pushed"
        (Dooit.Doc.status
           (Dooit.Doc.parse
              (git
                 [
                   "--git-dir=" ^ remote first;
                   "show";
                   "main:notes/" ^ id ^ ".md";
                 ]))
        = "done");
      check "update retries replay recorded result"
        (ok (invoke ~call_id:"update" discovered "dooit_update" update)
        = changed);
      check "fresh operation with stale revision is rejected"
        (Result.is_error
           (invoke ~call_id:"stale" discovered "dooit_update" update));
      check "distinct event uses distinct operation"
        (C.field "id"
           (C.json
              (ok
                 (invoke ~event:"$other" discovered "dooit_create" create_args)))
        <> id);
      configure second;
      ignore (ok (invoke discovered "dooit_create" create_args));
      check "every invocation reloads the configured store"
        (List.length (fst (Dooit.Store.scan first)) = 2
        && List.length (fst (Dooit.Store.scan second)) = 1);
      write_config "not = [valid";
      check "malformed configuration is visible at invocation"
        (Result.is_error (invoke discovered "dooit_search" "{}"));
      check "malformed configuration is visible at startup"
        (try
           ignore (load ());
           false
         with Failure _ -> true);
      let backend_store =
        Dooit.Store.init (Filename.concat root "backend-selection")
      in
      write_config
        (Printf.sprintf "root = %S\nbackend = \"local\"\n[git]\nremote = %S\n"
           backend_store.root
           (Filename.concat root "unused-remote.git"));
      let local_result =
        ok
          (invoke ~call_id:"local-backend" discovered "dooit_create" create_args)
      in
      check "explicit Local backend saves without Git and reports local scope"
        (C.field "sync" (C.json local_result) = "local_only"
        && List.length (fst (Dooit.Store.scan backend_store)) = 1
        && not (Sys.file_exists (Filename.concat backend_store.root ".git")));
      write_config
        (Printf.sprintf
           "root = %S\n\
            backend = \"webdav\"\n\
            [webdav]\n\
            url = \"https://example.invalid/files/\"\n\
            subdir = \"tasks\"\n\
            username = \"fixture\"\n\
            password_file = \"unused-secret\"\n"
           backend_store.root);
      check "WebDAV backend permits local reads"
        (Result.is_ok (invoke discovered "dooit_search" "{}"));
      let rejected =
        invoke ~call_id:"webdav-write" discovered "dooit_create" create_args
      in
      check "WebDAV writes fail explicitly without note mutation"
        ((match rejected with
         | Error message -> contains message "WebDAV"
         | Ok _ -> false)
        && List.length (fst (Dooit.Store.scan backend_store)) = 1);
      configure first;
      let offline = Dooit.Store.init (Filename.concat root "offline") in
      let allowed = ref true in
      let direct =
        Tasks.create
          ~tools:(Dooit_agent.create_local ~store:offline ~max_bytes:4096)
          ~store_id:offline.id ~profile:"direct"
          ~allowed:(fun ~actor:_ ~room:_ -> !allowed)
      in
      let revoke_after_io () =
        if fst (Dooit.Store.scan offline) <> [] then allowed := false
      in
      check "revocation after I/O suppresses the response"
        (Result.is_error
           (invoke ~check:revoke_after_io direct "dooit_create" create_args));
      let count = List.length (fst (Dooit.Store.scan offline)) in
      check "revocation occurred after the local write" (count = 1);
      allowed := true;
      ignore (ok (invoke direct "dooit_create" create_args));
      check "committed write remains retry safe after revocation"
        (List.length (fst (Dooit.Store.scan offline)) = count);
      check "cancellation propagates"
        (try
           ignore
             (invoke
                ~check:(fun () -> raise (Eio.Cancel.Cancelled Exit))
                direct "dooit_search" "{}");
           false
         with Eio.Cancel.Cancelled Exit -> true);
      let scoped =
        Tasks.create
          ~tools:(Dooit_agent.create_local ~store:offline ~max_bytes:4096)
          ~store_id:offline.id ~profile:"different-profile"
          ~allowed:(fun ~actor:_ ~room:_ -> true)
      in
      check "profile identities have independent retry namespaces"
        (ok (invoke scoped "dooit_create" create_args)
        <> ok (invoke direct "dooit_create" create_args));
      check "source event identities have independent retry namespaces"
        (ok
           (invoke ~source_event:"$different-source" direct "dooit_create"
              create_args)
        <> ok (invoke direct "dooit_create" create_args));
      Store.add_direct_room store ~room:"!dm:example.org" ~peer:admin;
      check "accepted direct rooms use existing authorization"
        (Result.is_ok
           (invoke ~in_room:"!dm:example.org" discovered "dooit_search" "{}"));
      let rounds = ref 0 in
      let complete =
        Fake_model.v (fun messages definitions ->
            incr rounds;
            check "Dooit tools are registered"
              (List.for_all
                 (fun name ->
                   List.exists
                     (fun tool -> Agentkit.Agent.Tool.name tool = name)
                     definitions)
                 Dooit_agent.names);
            let prompt =
              Option.value ~default:"" (Agentkit.Chat.system_text messages)
            in
            check "task policy makes Dooit authoritative"
              (contains prompt "Dooit is the primary task store"
              && contains prompt "not a second task");
            if !rounds = 1 then
              ( None,
                [
                  Agentkit.Agent.
                    {
                      id = "engine-call";
                      name = "dooit_create";
                      arguments = {|{"title":"From engine"}|};
                    };
                ] )
            else (Some "Task recorded.", []))
      in
      let engine =
        Engine.create
          ~config:(Config.default ~admin ~homeserver:"https://example.org")
          ~store ~self:"@crow:example.org" ~plugins:[] ~complete ~now:(fun () ->
            0.)
        |> fun engine -> Engine.with_tasks engine discovered
      in
      let sent = ref [] in
      Engine.handle engine ~mentioned:true
        ~send:(fun text -> sent := text :: !sent)
        Engine.
          {
            room;
            sender = admin;
            id = "$engine-task";
            body = "Remember this task";
          };
      check "engine routes task calls and replies"
        (!rounds = 2 && List.length !sent = 1);
      check "task call is durably audited"
        (List.exists
           (fun (use : Store.tool_use) ->
             use.tool = "dooit_create" && use.status = "ok")
           (Store.tool_uses store ~day:"1970-01-01" ~after:0 ~through:max_int
              ~limit:20));
      let disabled_model =
        Fake_model.v (fun messages definitions ->
            check "disabled task store offers no Dooit tools"
              (not
                 (List.exists
                    (fun tool ->
                      Dooit_agent.is_tool (Agentkit.Agent.Tool.name tool))
                    definitions));
            let prompt =
              Option.value ~default:"" (Agentkit.Chat.system_text messages)
            in
            check "unavailable store never falls back to memory task state"
              (contains prompt "Do not use memory as a substitute task list");
            (Some "Configure an existing Dooit store first.", []))
      in
      let disabled =
        Engine.create
          ~config:(Config.default ~admin ~homeserver:"https://example.org")
          ~store ~self:"@crow:example.org" ~plugins:[] ~complete:disabled_model
          ~now:(fun () -> 0.)
      in
      Engine.handle disabled ~mentioned:true
        ~send:(fun _ -> ())
        Engine.
          {
            room;
            sender = admin;
            id = "$disabled-task";
            body = "Create another task";
          };
      let missing = Filename.concat root "absent" in
      write_config (Printf.sprintf "root = %S\n" missing);
      check "removed store returns explicit unavailable error"
        (Result.is_error (invoke discovered "dooit_search" "{}"));
      check "removed store is never reinitialized"
        (not (Sys.file_exists missing));
      let malformed = Filename.concat root "malformed" in
      Unix.mkdir malformed 0o700;
      Out_channel.with_open_bin (Filename.concat malformed "store.json")
        (fun oc -> output_string oc "{}");
      write_config (Printf.sprintf "root = %S\n" malformed);
      check "malformed metadata is an error, not silent disablement"
        (try
           ignore (load ());
           false
         with Failure _ -> true);
      print_endline
        "Crow Dooit discovery, authorization, retries and audit passed")
