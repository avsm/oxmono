open Dooit
open Common

let check name b = Alcotest.(check bool) name true b

let refuses f =
  match f () with
  | _ -> Alcotest.fail "Expected Git synchronization refusal"
  | exception Error _ -> ()

let rec remove_tree path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let note =
  {|---
schema: dooit/v1
id: 2d26ec67-139e-4cba-8bc3-6ec0f85b8735
title: Original
status: open
created_at: "2026-09-12T10:00:00Z"
tags: []
links: []
---
Body.
|}

let id = Doc.id (Doc.parse note)

let test () =
  let base = Filename.temp_file "dooit-git-test-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove_tree base)
    (fun () ->
      Eio_main.run (fun env ->
          let process_mgr = Eio.Stdenv.process_mgr env in
          let git root args =
            Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
              ([ "git"; "-C"; root ] @ args)
          in
          let author store =
            ignore
              (git store.Store.root [ "config"; "user.name"; "Dooit Test" ]);
            ignore
              (git store.root
                 [ "config"; "user.email"; "dooit@example.invalid" ])
          in
          let remote = Filename.concat base "origin.git" in
          ignore
            (git base [ "init"; "--bare"; "--initial-branch=main"; remote ]);
          let config = Git.{ remote; branch = "main" } in
          let a = Store.init (Filename.concat base "a") in
          ignore (Store.add a (Doc.parse note));
          ignore (Git.setup ~process_mgr a config);
          author a;
          atomic_write (Store.state_path a "private") "must not be committed";
          atomic_write (Filename.concat a.root "unrelated.txt") "unrelated";
          ignore (Git.push ~process_mgr a config);
          let tracked =
            git a.root [ "ls-files"; "-z" ]
            |> String.split_on_char '\000'
            |> List.filter (( <> ) "")
            |> List.sort String.compare
          in
          check "only metadata and note tracked"
            (tracked
            = List.sort String.compare [ "store.json"; "notes/" ^ id ^ ".md" ]);
          check "private state excluded"
            (String.trim (git a.root [ "check-ignore"; ".dooit/private" ])
            = ".dooit/private");
          let b =
            Git.clone ~process_mgr
              ~root:(Filename.concat base "nested/clone/b")
              config
          in
          author b;
          check "clone preserves store identity and revision"
            (b.id = a.id
            && Doc.revision (Store.get b id) = Doc.revision (Store.get a id));
          refuses (fun () -> Git.clone ~process_mgr ~root:b.root config);
          let old = Store.get b id in
          ignore
            (Store.modify a id
               (Doc.change "title" (Some (str "Remote update"))));
          ignore (Git.push ~process_mgr a config);
          ignore
            (Store.modify b id (Doc.change "title" (Some (str "Dirty local"))));
          let dirty = (Store.get b id).raw in
          refuses (fun () -> Git.pull ~process_mgr b config);
          check "pull preserves dirty notes" ((Store.get b id).raw = dirty);
          atomic_write (Store.note_path b id) old.raw;
          ignore (Git.pull ~process_mgr b config);
          check "fast-forward changes revision authority"
            (Doc.title (Store.get b id) = "Remote update"
            && Doc.revision (Store.get b id) <> Doc.revision old);
          (* A local-only new note must not make an already-ahead pull fail. *)
          let second_id = "3d26ec67-139e-4cba-8bc3-6ec0f85b8735" in
          let second =
            let p = String.index note '2' in
            String.sub note 0 p ^ second_id
            ^ String.sub note (p + 36) (String.length note - p - 36)
          in
          ignore (Store.add b (Doc.parse second));
          ignore (git b.root [ "add"; "--"; "notes/" ^ second_id ^ ".md" ]);
          ignore (git b.root [ "commit"; "-m"; "Local extra note" ]);
          check "pull while ahead preserves added notes"
            (Git.pull ~process_mgr b config = "Already up to date");
          Unix.unlink (Store.note_path b second_id);
          refuses (fun () -> Git.push ~process_mgr b config);
          check "deleted note is not committed"
            (String.trim
               (git b.root [ "show"; "HEAD:notes/" ^ second_id ^ ".md" ])
            <> "");
          atomic_write (Store.note_path b second_id) second;
          ignore
            (Store.modify a id
               (Doc.change "title" (Some (str "Divergent remote"))));
          ignore (Git.push ~process_mgr a config);
          let before = (Store.get b id).raw in
          refuses (fun () -> Git.pull ~process_mgr b config);
          refuses (fun () -> Git.push ~process_mgr b config);
          check "divergence preserves local notes"
            ((Store.get b id).raw = before);
          let c =
            Git.clone ~process_mgr ~root:(Filename.concat base "c") config
          in
          author c;
          Store.record_conflict c ~id ~base:None ~local:(Some before)
            ~remote:None "test";
          refuses (fun () -> Git.push ~process_mgr c config);
          check "Git refusal preserves conflict evidence"
            (Store.conflicts c = [ id ]);
          Store.clear_conflict c id;
          atomic_write (Filename.concat c.root "private.txt") "private";
          ignore (git c.root [ "add"; "private.txt" ]);
          refuses (fun () -> Git.push ~process_mgr c config);
          ignore (git c.root [ "rm"; "--cached"; "private.txt" ]);
          let other_remote = Filename.concat base "other.git" in
          ignore
            (git base
               [ "init"; "--bare"; "--initial-branch=main"; other_remote ]);
          let other_config = Git.{ remote = other_remote; branch = "main" } in
          let other = Store.init (Filename.concat base "other") in
          ignore (Git.setup ~process_mgr other other_config);
          author other;
          ignore (Git.push ~process_mgr other other_config);
          ignore (git c.root [ "remote"; "set-url"; "origin"; other_remote ]);
          refuses (fun () -> Git.pull ~process_mgr c other_config);
          check "foreign store refusal preserves identity"
            ((Store.open_ c.root).id = a.id);
          let executable =
            Git.clone ~process_mgr
              ~root:(Filename.concat base "executable")
              config
          in
          author executable;
          let remote_before = git remote [ "rev-parse"; "refs/heads/main" ] in
          ignore (git executable.root [ "config"; "core.fileMode"; "true" ]);
          Unix.chmod (Store.note_path executable id) 0o755;
          refuses (fun () -> Git.push ~process_mgr executable config);
          check "invalid committed mode never reaches remote"
            (git remote [ "rev-parse"; "refs/heads/main" ] = remote_before
            && git executable.root [ "rev-parse"; "HEAD" ] <> remote_before);
          let bad =
            Git.clone ~process_mgr ~root:(Filename.concat base "bad") config
          in
          author bad;
          atomic_write
            (Filename.concat bad.root "private.txt")
            "private history";
          ignore (git bad.root [ "add"; "private.txt" ]);
          ignore (git bad.root [ "commit"; "-m"; "Private historical file" ]);
          ignore (git bad.root [ "rm"; "private.txt" ]);
          ignore (git bad.root [ "commit"; "-m"; "Remove private file" ]);
          refuses (fun () -> Git.push ~process_mgr bad config);
          (* Simulate a remote changed by a client that does not use Dooit. *)
          ignore (git bad.root [ "push"; "origin"; "HEAD:main" ]);
          let rejected = Filename.concat base "rejected" in
          refuses (fun () -> Git.clone ~process_mgr ~root:rejected config);
          check "refused clone leaves destination absent"
            (not (exists rejected));
          (* The lock also excludes concurrent fibers in this process. *)
          let entered, signal = Eio.Promise.create () in
          let done_, finish = Eio.Promise.create () in
          ignore
            (Eio.Fiber.pair
               (fun () ->
                 Store.with_lock a (fun () ->
                     Eio.Promise.resolve signal ();
                     Eio.Promise.await done_))
               (fun () ->
                 Eio.Promise.await entered;
                 refuses (fun () -> Store.with_lock a (fun () -> ()));
                 Eio.Promise.resolve finish ()));
          check "unrelated local files retained"
            (read (Filename.concat a.root "unrelated.txt") = "unrelated")))

let cancellation () =
  let base = Filename.temp_file "dooit-git-cancel-" "" in
  Unix.unlink base;
  mkdir base;
  let original_path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" original_path;
      remove_tree base)
    (fun () ->
      let bin = Filename.concat base "bin" in
      mkdir bin;
      let script = Filename.concat bin "git" in
      let pid_file = Filename.concat base "child.pid" in
      atomic_write script
        "#!/bin/sh\n\
         /bin/sleep 30 &\n\
         child=$!\n\
         printf '%s\\n' \"$child\" > \"$DOOIT_GIT_TEST_PID\"\n\
         wait\n";
      Unix.chmod script 0o700;
      Unix.putenv "PATH" (bin ^ ":" ^ original_path);
      Unix.putenv "DOOIT_GIT_TEST_PID" pid_file;
      Eio_main.run (fun env ->
          let clock = Eio.Stdenv.clock env in
          let store = Store.init (Filename.concat base "store") in
          let rec until predicate =
            if not (predicate ()) then (
              Eio.Time.sleep clock 0.01;
              until predicate)
          in
          Eio.Time.with_timeout_exn clock 5. (fun () ->
              Eio.Fiber.first
                (fun () ->
                  ignore
                    (Git.setup
                       ~process_mgr:(Eio.Stdenv.process_mgr env)
                       store Git.default);
                  Alcotest.fail "fake Git unexpectedly completed")
                (fun () -> until (fun () -> exists pid_file)));
          let pid = int_of_string (String.trim (read pid_file)) in
          Eio.Time.with_timeout_exn clock 5. (fun () ->
              until (fun () ->
                  try
                    Unix.kill pid 0;
                    false
                  with Unix.Unix_error (Unix.ESRCH, _, _) -> true));
          Store.with_lock store (fun () -> ())))

let lost_notification () =
  let base = Filename.temp_file "dooit-git-notification-" "" in
  Unix.unlink base;
  mkdir base;
  let original_path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" original_path;
      remove_tree base)
    (fun () ->
      Eio_main.run (fun env ->
          let process_mgr = Eio.Stdenv.process_mgr env in
          let git =
            Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
              [ "/bin/sh"; "-c"; "command -v git" ]
            |> String.trim
          in
          let bin = Filename.concat base "bin" in
          mkdir bin;
          let script = Filename.concat bin "git" in
          atomic_write script
            ("#!/bin/sh\n/bin/sleep 0.05\nexec " ^ Filename.quote git
           ^ " \"$@\"\n");
          Unix.chmod script 0o700;
          Unix.putenv "PATH" (bin ^ ":" ^ original_path);
          let notifications = ref 0 in
          let previous =
            Sys.signal Sys.sigchld
              (Sys.Signal_handle (fun _ -> incr notifications))
          in
          Fun.protect
            ~finally:(fun () -> Sys.set_signal Sys.sigchld previous)
            (fun () ->
              let store = Store.init (Filename.concat base "store") in
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                  ignore (Git.setup ~process_mgr store Git.default));
              check "dropped notifications cannot strand Git completion"
                (!notifications > 0);
              Store.with_lock store (fun () -> ()))))

let apply_retry () =
  let base = Filename.temp_file "dooit-git-apply-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove_tree base)
    (fun () ->
      Eio_main.run (fun env ->
          let process_mgr = Eio.Stdenv.process_mgr env in
          let git root args =
            Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
              ([ "git"; "-C"; root ] @ args)
          in
          let author store =
            ignore
              (git store.Store.root [ "config"; "user.name"; "Dooit Test" ]);
            ignore
              (git store.root
                 [ "config"; "user.email"; "dooit@example.invalid" ])
          in
          let remote = Filename.concat base "origin.git" in
          ignore
            (git base [ "init"; "--bare"; "--initial-branch=main"; remote ]);
          let config = Git.{ remote; branch = "main" } in
          let a = Store.init (Filename.concat base "a") in
          ignore (Store.add a (Doc.parse note));
          ignore (Git.setup ~process_mgr a config);
          author a;
          let operation = new_uuid () in
          let expected = Some (Doc.revision (Store.get a id)) in
          let payload = "set title to Synced" in
          let count = ref 0 in
          let change = function
            | None -> failwith "missing note"
            | Some note ->
                incr count;
                Doc.change "title" (Some (str "Synced")) note
          in
          refuses (fun () ->
              Git.apply ~process_mgr
                ~check:(fun () -> ())
                a config ~id ~expected ~operation ~payload change);
          check "uninitialized remote refuses before task mutation" (!count = 0);
          ignore (Git.push ~process_mgr a config);
          let b =
            Git.clone ~process_mgr ~root:(Filename.concat base "b") config
          in
          author b;
          let before = git a.root [ "rev-parse"; "HEAD" ] in
          let offline = remote ^ ".offline" in
          let disconnected = ref false in
          let disconnect_after_apply () =
            if (not !disconnected) && Doc.title (Store.get a id) = "Synced" then (
              Unix.rename remote offline;
              disconnected := true)
          in
          (match
             Git.apply ~process_mgr ~check:disconnect_after_apply a config ~id
               ~expected ~operation ~payload change
           with
          | _ -> Alcotest.fail "offline push unexpectedly succeeded"
          | exception Git.Pending pending ->
              check "pending identifies saved operation"
                (pending.id = id
                && pending.operation = operation
                && pending.revision = Doc.revision (Store.get a id)));
          check "write committed before failed network push"
            (!count = 1
            && git a.root [ "rev-parse"; "HEAD" ] <> before
            && String.trim
                 (git a.root [ "status"; "--porcelain"; "--"; "notes" ])
               = "");
          Unix.rename offline remote;
          let other_id = "3d26ec67-139e-4cba-8bc3-6ec0f85b8735" in
          let p = String.index note '2' in
          let other =
            String.sub note 0 p ^ other_id
            ^ String.sub note (p + 36) (String.length note - p - 36)
          in
          ignore (Store.add a (Doc.parse other));
          (match
             Git.apply ~process_mgr
               ~check:(fun () -> ())
               a config ~id ~expected ~operation ~payload change
           with
          | _ -> Alcotest.fail "retry swept unrelated dirty note"
          | exception Git.Pending _ -> ());
          check "unrelated retry edit retained and not committed"
            (Store.raw a other_id = Some other
            && git remote [ "rev-parse"; "refs/heads/main" ] = before);
          Unix.unlink (Store.note_path a other_id);
          let result =
            Git.apply ~process_mgr
              ~check:(fun () -> ())
              a config ~id ~expected ~operation ~payload change
          in
          check "pending retry pushes without reapplying"
            (!count = 1
            && Doc.title result = "Synced"
            && git remote [ "rev-parse"; "refs/heads/main" ]
               = git a.root [ "rev-parse"; "HEAD" ]);
          let stale_operation = new_uuid () in
          refuses (fun () ->
              Git.apply ~process_mgr
                ~check:(fun () -> ())
                b config ~id ~expected ~operation:stale_operation ~payload
                change);
          check "pull happens before expected revision validation"
            (Doc.title (Store.get b id) = "Synced" && !count = 1);
          ignore
            (Store.modify a id
               (Doc.change "title" (Some (str "Later local edit"))));
          let confirmed = git remote [ "rev-parse"; "refs/heads/main" ] in
          let replay =
            Git.apply ~process_mgr
              ~check:(fun () -> ())
              a config ~id ~expected ~operation ~payload change
          in
          check "confirmed replay never republishes later edits"
            (Doc.revision replay = Doc.revision result
            && !count = 1
            && Doc.title (Store.get a id) = "Later local edit"
            && git remote [ "rev-parse"; "refs/heads/main" ] = confirmed);
          refuses (fun () ->
              Git.apply ~process_mgr
                ~check:(fun () -> ())
                a config ~id ~expected ~operation ~payload:"different" change);
          refuses (fun () ->
              Git.apply ~process_mgr
                ~check:(fun () -> ())
                a
                { config with branch = "elsewhere" }
                ~id ~expected ~operation ~payload change);
          match
            Git.apply ~process_mgr
              ~check:(fun () -> raise Exit)
              a config ~id ~expected ~operation ~payload change
          with
          | _ -> Alcotest.fail "confirmed replay bypassed authority"
          | exception Exit -> ()))

let full_sync () =
  let base = Filename.temp_file "dooit-git-sync-" "" in
  Unix.unlink base;
  mkdir base;
  Fun.protect
    ~finally:(fun () -> remove_tree base)
    (fun () ->
      Eio_main.run (fun env ->
          let process_mgr = Eio.Stdenv.process_mgr env in
          let git root args =
            Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
              ([ "git"; "-C"; root ] @ args)
          in
          let author store =
            ignore
              (git store.Store.root [ "config"; "user.name"; "Dooit Test" ]);
            ignore
              (git store.root
                 [ "config"; "user.email"; "dooit@example.invalid" ])
          in
          let remote = Filename.concat base "origin.git" in
          ignore
            (git base [ "init"; "--bare"; "--initial-branch=main"; remote ]);
          let config = Git.{ remote; branch = "main" } in
          let a = Store.init (Filename.concat base "a") in
          remove_tree (Store.state_dir a);
          let fresh = Git.status ~process_mgr a config in
          check "status without repository creates no private state"
            ((not fresh.initialized) && fresh.branch = None
            && not (exists (Store.state_dir a)));
          ignore (Store.add a (Doc.parse note));
          ignore (Git.setup ~process_mgr a config);
          author a;
          ignore (Git.sync ~process_mgr a config);
          let b =
            Git.clone ~process_mgr ~root:(Filename.concat base "b") config
          in
          author b;
          let id2 = "3d26ec67-139e-4cba-8bc3-6ec0f85b8735" in
          let p = String.index note '2' in
          let second =
            String.sub note 0 p ^ id2
            ^ String.sub note (p + 36) (String.length note - p - 36)
          in
          ignore
            (Store.modify a id (Doc.change "title" (Some (str "Peer A edit"))));
          ignore (Store.add b (Doc.parse second));
          ignore (Git.sync ~process_mgr a config);
          ignore (Git.sync ~process_mgr b config);
          ignore (Git.sync ~process_mgr a config);
          check "sync merges independent peer edits and task additions"
            (Doc.title (Store.get a id) = "Peer A edit"
            && Doc.title (Store.get b id) = "Peer A edit"
            && (Store.get a id2).raw = (Store.get b id2).raw
            && git a.root [ "rev-parse"; "HEAD" ]
               = git b.root [ "rev-parse"; "HEAD" ]);
          let index = Filename.concat a.root ".git/index" in
          let before_index = Unix.stat index in
          let journal = read (Store.state_path a "operations.json") in
          Unix.rename remote (remote ^ ".offline");
          let state = Git.status ~process_mgr a config in
          Unix.rename (remote ^ ".offline") remote;
          check "status is offline, clean and does not refresh index or journal"
            (state.initialized && state.branch = Some "main"
           && (not state.dirty) && state.ahead = Some 0 && state.behind = Some 0
           && state.conflicts = []
            && (Unix.stat index).st_mtime = before_index.st_mtime
            && read (Store.state_path a "operations.json") = journal);
          ignore
            (Store.modify a id (Doc.change "title" (Some (str "Left title"))));
          ignore
            (Store.modify b id (Doc.change "title" (Some (str "Right title"))));
          ignore (Git.sync ~process_mgr a config);
          refuses (fun () -> Git.sync ~process_mgr b config);
          let divergent = Git.status ~process_mgr b config in
          check "conflicting sync aborts own merge and retains committed task"
            (Doc.title (Store.get b id) = "Right title"
            && (not divergent.dirty) && divergent.ahead = Some 1
            && divergent.behind = Some 1
            && not (exists (Filename.concat b.root ".git/MERGE_HEAD")));
          check "conflicting sync never overwrites remote task"
            (Doc.title
               (Doc.parse (git remote [ "show"; "main:notes/" ^ id ^ ".md" ]))
            = "Left title");
          let c =
            Git.clone ~process_mgr ~root:(Filename.concat base "c") config
          in
          author c;
          ignore
            (Store.modify c id2
               (Doc.change "title" (Some (str "Local retained"))));
          (* A client outside Dooit can commit structurally valid metadata that
             breaks immutable task identity. Do not publish a merge of it. *)
          let invalid =
            Doc.change "created_at"
              (Some (str "2026-10-01T00:00:00Z"))
              (Store.get a id)
          in
          atomic_write (Store.note_path a id) invalid.raw;
          ignore (git a.root [ "add"; "notes/" ^ id ^ ".md" ]);
          ignore
            (git a.root [ "commit"; "-m"; "External immutable metadata edit" ]);
          ignore (git a.root [ "push"; "origin"; "HEAD:main" ]);
          refuses (fun () -> Git.sync ~process_mgr c config);
          check "invalid merged metadata aborts while retaining local commit"
            (field "created_at" (Store.get c id).meta = "2026-09-12T10:00:00Z"
            && Doc.title (Store.get c id2) = "Local retained"
            && (not (exists (Filename.concat c.root ".git/MERGE_HEAD")))
            && not (Git.status ~process_mgr c config).dirty)))

let () =
  Alcotest.run "Dooit Git"
    [
      ( "local repositories",
        [
          Alcotest.test_case "safe synchronization" `Quick test;
          Alcotest.test_case "cancellation releases children and lock" `Quick
            cancellation;
          Alcotest.test_case "write sync and operation retry" `Quick apply_retry;
          Alcotest.test_case "full sync and read-only status" `Quick full_sync;
          Alcotest.test_case "lost child-exit notifications" `Quick
            lost_notification;
        ] );
    ]
