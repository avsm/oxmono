module S = Bushel_knowledge_store
module K = Bushel_knowledge
module J = Retrieval.Json

let check name b = if not b then failwith name
let noop () = ()

let member name codec =
  Jsont.Object.map Fun.id
  |> Jsont.Object.mem name codec ~enc:Fun.id
  |> Jsont.Object.finish

let integer name json = J.decode (member name Jsont.int) json

let coverage json =
  let pair =
    Jsont.Object.map (fun indexed total -> (indexed, total))
    |> Jsont.Object.mem "indexed" Jsont.int ~enc:fst
    |> Jsont.Object.mem "total" Jsont.int ~enc:snd
    |> Jsont.Object.finish
  in
  J.decode (member "embedding_coverage" pair) json

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let rec remove path =
  if (Unix.lstat path).st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Sys.remove path

let holds_lock path =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0 in
  Unix.lockf fd Unix.F_LOCK 0;
  output_char stdout 'r';
  flush stdout;
  (try ignore (input_char stdin) with End_of_file -> ());
  Unix.close fd

let with_external_lock path f =
  let release_read, release_write = Unix.pipe ~cloexec:true () in
  let ready_read, ready_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "--hold-lock"; path |]
      release_read ready_write Unix.stderr
  in
  Unix.close release_read;
  Unix.close ready_write;
  let rec wait () =
    try ignore (Unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close release_write;
      Unix.close ready_read;
      wait ())
    (fun () ->
      let ready = Bytes.create 1 in
      check "child acquired file lock" (Unix.read ready_read ready 0 1 = 1);
      f ())

let index_until_killed config_file =
  Eio_main.run (fun env ->
      let fetch =
        Fetch_mock.client (fun request ->
            let body =
              match request.body with Fetch.String s -> s | _ -> assert false
            in
            let inputs =
              J.decode (member "input" (Jsont.list Jsont.string)) body
            in
            let rows =
              List.mapi
                (fun index _ ->
                  Printf.sprintf {|{"index":%d,"embedding":[1,0]}|} index)
                inputs
            in
            Fetch_mock.respond
              ("{\"data\":[" ^ String.concat "," rows ^ "]}")
              request)
      in
      S.with_store ~config_file ~fs:(Eio.Stdenv.fs env)
        ~clock:(Eio.Stdenv.clock env) ~fetch ~check:noop (fun store ->
          ignore
            (S.index_all store ~check:noop
               ~on_progress:(fun ~indexed ~total:_ ->
                 if indexed > 0 then (
                   output_char stdout 'r';
                   flush stdout;
                   while true do
                     Unix.sleep 3600
                   done)))))

let kill_after_batch config_file =
  let ready_read, ready_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "--index-until-killed"; config_file |]
      Unix.stdin ready_write Unix.stderr
  in
  Unix.close ready_write;
  let reaped = ref false in
  let rec wait () =
    try
      let _, status = Unix.waitpid [] pid in
      reaped := true;
      status
    with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close ready_read;
      if not !reaped then (
        (try Unix.kill pid Sys.sigkill
         with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        ignore (wait ())))
    (fun () ->
      let readable, _, _ = Unix.select [ ready_read ] [] [] 30. in
      check "indexing child reports a committed batch before timeout"
        (readable <> []);
      let signal = Bytes.create 1 in
      check "indexing child reached its first progress callback"
        (Unix.read ready_read signal 0 1 = 1 && Bytes.get signal 0 = 'r');
      Unix.kill pid Sys.sigkill;
      check "indexing process was killed without cleanup"
        (wait () = Unix.WSIGNALED Sys.sigkill))

let run () =
  let root = Filename.temp_file "bushel-shared-knowledge" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let directory name =
    let path = Filename.concat root name in
    Unix.mkdir path 0o700;
    path
  in
  let data = directory "data" in
  let bushel = directory "bushel" in
  let sortal = directory "sortal" in
  let notes = Filename.concat bushel "notes" in
  Unix.mkdir notes 0o700;
  let note n body =
    write
      (Filename.concat notes (Printf.sprintf "%02d.md" n))
      (Printf.sprintf
         "---\ntitle: Note %d\ndate: 2026-01-01\nslug: note-%d\n---\n%s\n" n n
         body)
  in
  for n = 0 to 19 do
    note n "sharedcorpusmarker"
  done;
  let config_file = Filename.concat root "config.toml" in
  let configure ?(model = "fixture") ?key ?(corpus = bushel) () =
    write config_file
      (Printf.sprintf
         "[data]\n\
          local_dir = %S\n\
          [embeddings]\n\
          url = \"https://embedding.example/v1/embeddings\"\n\
          model = %S\n\
          dimensions = 2\n\
          %s"
         corpus model
         (Option.fold ~none:""
            ~some:(fun path -> Printf.sprintf "api_key_file = %S\n" path)
            key))
  in
  Unix.putenv "XDG_DATA_HOME" data;
  Unix.putenv "SORTAL_DATA_DIR" root;
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      configure ();
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env and clock = Eio.Stdenv.clock env in
      let calls = ref [] in
      let invalid_response = ref false in
      let invalid_after = ref None in
      let fetch =
        Fetch_mock.client (fun request ->
            let body =
              match request.body with Fetch.String s -> s | _ -> assert false
            in
            let inputs =
              J.decode (member "input" (Jsont.list Jsont.string)) body
            in
            calls := inputs :: !calls;
            let invalid =
              !invalid_response
              || Option.fold ~none:false
                   ~some:(fun count -> List.length !calls >= count)
                   !invalid_after
            in
            let rows =
              List.mapi
                (fun index _ ->
                  Printf.sprintf {|{"index":%d,"embedding":[%d,0]}|} index
                    (if invalid then 0 else 1))
                inputs
            in
            Fetch_mock.respond
              ("{\"data\":[" ^ String.concat "," rows ^ "]}")
              request)
      in
      let session ?(check = noop) f =
        S.with_store ~config_file ~fs ~clock ~fetch ~check f
      in
      check "discovers both sources"
        (S.sources ~config_file ~fs () = (bushel, sortal));
      let path =
        session (fun store ->
            let status = S.status store in
            check "fresh corpus counts" (integer "documents" status = 20);
            check "fresh coverage" (coverage status = (0, 20));
            check "status makes no provider request" (!calls = []);
            S.database store)
      in
      check "index is private" ((Unix.stat path).st_perm land 0o777 = 0o600);
      let progress = ref [] in
      let on_progress ~indexed ~total =
        progress := (indexed, total) :: !progress
      in
      let first =
        session (fun store ->
            S.index ~on_progress store ~check:noop ~max_batches:1)
      in
      check "bounded batch" (integer "newly_indexed" first = 16);
      check "partial coverage" (coverage first = (16, 20));
      check "limited progress counts the whole corpus"
        (List.rev !progress = [ (0, 20); (16, 20) ]);
      progress := [];
      let second =
        session (fun store ->
            S.index ~on_progress store ~check:noop ~max_batches:1)
      in
      check "second consumer resumes" (integer "newly_indexed" second = 4);
      check "full coverage" (coverage second = (20, 20));
      check "resumed progress includes cached passages"
        (List.rev !progress = [ (16, 20); (20, 20) ]);
      progress := [];
      let before = List.length !calls in
      let third =
        session (fun store ->
            S.index ~on_progress store ~check:noop ~max_batches:1)
      in
      check "cache hit reports initial complete coverage only"
        (!progress = [ (20, 20) ]);
      check "completed index reuses vectors"
        (integer "newly_indexed" third = 0 && List.length !calls = before);
      let search =
        session (fun store ->
            Bushel_agent.invoke
              (Bushel_agent.of_store store ~max_bytes:65536)
              ~check:noop "knowledge_search"
              {|{"query":"compiler performance","mode":"semantic"}|})
      in
      check "agent consumer searches same index" (Result.is_ok search);
      check "search embeds only query" (List.length !calls = before + 1);
      configure ~model:"killed-fixture" ();
      kill_after_batch config_file;
      check "killed index leaves its recovery WAL available"
        (Sys.file_exists (path ^ "-wal")
        && (Unix.stat (path ^ "-wal")).st_size > 0);
      let before_resume = List.length !calls in
      session (fun store ->
          check "SIGKILL preserves the completed batch in the reopened store"
            (coverage (S.status store) = (16, 20));
          let resumed = S.index_all store ~check:noop in
          check "SIGKILL restart embeds only the four remaining passages"
            (integer "newly_indexed" resumed = 4
            && coverage resumed = (20, 20)
            && List.length !calls = before_resume + 1
            && List.length (List.hd !calls) = 4));
      session (fun store ->
          check "SIGKILL recovery remains cached in another session"
            (integer "newly_indexed" (S.index_all store ~check:noop) = 0
            && List.length !calls = before_resume + 1));
      configure ();
      note 0 "revisedcorpusmarker";
      Sys.remove (Filename.concat notes "01.md");
      session (fun store ->
          check "edits and deletes invalidate only affected vectors"
            (coverage (S.status store) = (18, 19));
          check "changed passage indexed"
            (integer "newly_indexed" (S.index store ~check:noop ~max_batches:1)
            = 1));
      configure ~model:"different" ();
      session (fun store ->
          check "new model never reuses old vectors"
            (coverage (S.status store) = (0, 19)));
      progress := [];
      let before_failure = List.length !calls in
      check "cancelled index reports only its initial coverage"
        (try
           session (fun store ->
               ignore
                 (S.index ~on_progress store
                    ~check:(fun () -> failwith "cancel index")
                    ~max_batches:1));
           false
         with Failure message ->
           message = "cancel index"
           && !progress = [ (0, 19) ]
           && List.length !calls = before_failure);
      progress := [];
      invalid_response := true;
      Fun.protect
        ~finally:(fun () -> invalid_response := false)
        (fun () ->
          check "invalid inference never advances corpus progress"
            (try
               session (fun store ->
                   ignore
                     (S.index ~on_progress store ~check:noop ~max_batches:1));
               false
             with Invalid_argument _ -> !progress = [ (0, 19) ]));
      session (fun store ->
          check "failed inference leaves coverage unchanged"
            (coverage (S.status store) = (0, 19)));
      configure ~model:"all-complete" ();
      progress := [];
      let before_all = List.length !calls in
      let all =
        session (fun store -> S.index_all ~on_progress store ~check:noop)
      in
      check "unbounded index completes multiple provider batches"
        (integer "newly_indexed" all = 19
        && coverage all = (19, 19)
        && List.length !calls = before_all + 2
        && List.rev !progress = [ (0, 19); (16, 19); (19, 19) ]);
      configure ~model:"all-resume" ();
      progress := [];
      invalid_after := Some (List.length !calls + 2);
      Fun.protect
        ~finally:(fun () -> invalid_after := None)
        (fun () ->
          check "unbounded failure reports only completed batches"
            (try
               session (fun store ->
                   ignore (S.index_all ~on_progress store ~check:noop));
               false
             with Invalid_argument _ ->
               List.rev !progress = [ (0, 19); (16, 19) ]));
      session (fun store ->
          check "unbounded failure preserves completed batch coverage"
            (coverage (S.status store) = (16, 19)));
      progress := [];
      let resumed =
        session (fun store -> S.index_all ~on_progress store ~check:noop)
      in
      check "unbounded retry resumes the outstanding tail"
        (integer "newly_indexed" resumed = 3
        && coverage resumed = (19, 19)
        && List.rev !progress = [ (16, 19); (19, 19) ]);
      progress := [];
      let before_cached = List.length !calls in
      let cached =
        session (fun store -> S.index_all ~on_progress store ~check:noop)
      in
      check "unbounded cache hit reports existing coverage without inference"
        (integer "newly_indexed" cached = 0
        && List.length !calls = before_cached
        && !progress = [ (19, 19) ]);
      check "unbounded cancellation is checked even for a complete cache"
        (try
           session (fun store ->
               ignore
                 (S.index_all store ~check:(fun () ->
                      failwith "cancel complete index")));
           false
         with Failure message -> message = "cancel complete index");
      configure ();
      session (fun store ->
          check "original model remains cached"
            (coverage (S.status store) = (19, 19)));
      write config_file
        (Printf.sprintf
           "[data]\n\
            local_dir=%S\n\
            [embeddings]\n\
            backend='mlx'\n\
            model_dir=%S\n\
            dimensions=32\n"
           bushel
           (Filename.concat root "missing-text-model"));
      session (fun store ->
          let agent = Bushel_agent.of_store store ~max_bytes:65536 in
          let invoke name args =
            Bushel_agent.invoke agent ~check:noop name args
          in
          check "lexical agent search does not construct a missing native model"
            (Result.is_ok
               (invoke "knowledge_search" {|{"query":"revisedcorpusmarker"}|}));
          let id = K.entry_id "note-0" in
          let args = "{\"id\":" ^ J.encode Jsont.string id ^ "}" in
          check "agent source reads do not construct a native model"
            (Result.is_ok (invoke "knowledge_get" args));
          check "agent contact resolution does not construct a native model"
            (Result.is_ok (invoke "contacts_resolve" {|{"query":"unknown"}|}));
          List.iter
            (fun mode ->
              check "semantic agent search still validates its native model"
                (try
                   ignore
                     (invoke "knowledge_search"
                        (Printf.sprintf {|{"query":"query","mode":%S}|} mode));
                   false
                 with Unix.Unix_error (Unix.ENOENT, _, _) -> true))
            [ "semantic"; "hybrid" ];
          check "provider failure does not break later lexical agent searches"
            (Result.is_ok
               (invoke "knowledge_search" {|{"query":"revisedcorpusmarker"}|})));
      let missing_key = Filename.concat root "missing-key" in
      configure ~key:missing_key ();
      session (fun store ->
          ignore (S.status store);
          let result =
            Bushel_agent.invoke
              (Bushel_agent.of_store store ~max_bytes:2048)
              ~check:noop "knowledge_search" {|{"query":"sharedcorpusmarker"}|}
          in
          check "lexical search does not need provider credentials"
            (Result.is_ok result));
      check "semantic inference requires configured key"
        (try
           session (fun store ->
               ignore
                 (K.search (S.corpus store) ?provider:(S.provider store)
                    ~check:noop ~mode:"semantic" ~filter:Retrieval.Index.all
                    "query"));
           false
         with Unix.Unix_error (Unix.ENOENT, _, _) -> true);
      Unix.mkfifo missing_key 0o600;
      check "non-regular key cannot block the scheduler"
        (try
           session (fun store ->
               ignore
                 (K.search (S.corpus store) ?provider:(S.provider store)
                    ~check:noop ~mode:"semantic" ~filter:Retrieval.Index.all
                    "query"));
           false
         with Invalid_argument _ -> true);
      Sys.remove missing_key;
      write missing_key "fixture-private-token\n";
      Unix.chmod missing_key 0o644;
      check "public key file rejected"
        (try
           session (fun store ->
               ignore
                 (K.search (S.corpus store) ?provider:(S.provider store)
                    ~check:noop ~mode:"semantic" ~filter:Retrieval.Index.all
                    "query"));
           false
         with Invalid_argument _ -> true);
      Unix.chmod missing_key 0o600;
      session (fun store ->
          ignore
            (K.search (S.corpus store) ?provider:(S.provider store) ~check:noop
               ~mode:"semantic" ~filter:Retrieval.Index.all "query"));
      configure ();
      check "failed sessions leave lock usable"
        (try session (fun _ -> failwith "test failure")
         with Failure _ -> session (fun _ -> true));
      let entered, enter = Eio.Promise.create () in
      let release, unlock = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          session (fun _ ->
              Eio.Promise.resolve enter ();
              Eio.Promise.await release))
        (fun () ->
          Eio.Promise.await entered;
          Fun.protect
            ~finally:(fun () -> Eio.Promise.resolve unlock ())
            (fun () ->
              check "waiting fiber can be cancelled"
                (try
                   Eio.Time.with_timeout_exn clock 0.03 (fun () ->
                       session (fun _ -> ()));
                   false
                 with Eio.Time.Timeout -> true)));
      with_external_lock (path ^ ".lock") (fun () ->
          check "cross-process contention yields and can be cancelled"
            (try
               Eio.Time.with_timeout_exn clock 0.03 (fun () ->
                   session (fun _ -> ()));
               false
             with Eio.Time.Timeout -> true));
      check "active session can be cancelled"
        (try
           Eio.Time.with_timeout_exn clock 0.03 (fun () ->
               session (fun _ -> Eio.Fiber.await_cancel ()))
         with Eio.Time.Timeout -> true);
      session (fun _ -> ());
      let alias = Filename.concat root "bushel-alias" in
      Unix.symlink bushel alias;
      configure ~corpus:alias ();
      session (fun store ->
          check "canonical roots share index" (S.database store = path));
      let other = directory "other" in
      configure ~corpus:other ();
      session (fun store ->
          check "different corpus is isolated"
            (S.database store <> path
            && integer "documents" (S.status store) = 0));
      configure ();
      check "original corpus survives isolated refresh"
        (session (fun store -> coverage (S.status store)) = (19, 19));
      write (Filename.concat notes "broken.md") "---\ntitle: [broken\n---\nbody";
      check "corrupt source fails refresh"
        (try
           session (fun _ -> ());
           false
         with Failure _ -> true);
      Sys.remove (Filename.concat notes "broken.md");
      check "failed refresh preserves cached vectors"
        (session (fun store -> coverage (S.status store)) = (19, 19));
      let model_dir = directory "mlx-model" in
      write (Filename.concat model_dir "config.json") "{}";
      write (Filename.concat model_dir "model.safetensors") "fixture weights";
      write (Filename.concat model_dir "tokenizer.json") "fixture tokenizer";
      write config_file
        (Printf.sprintf
           "[data]\n\
            local_dir = %S\n\
            [embeddings]\n\
            backend = \"mlx\"\n\
            model_dir = %S\n\
            dimensions = 32\n\
            max_tokens = 128\n\
            query_instruction = \"Find relevant passages.\"\n"
           bushel model_dir);
      let before_native_status = List.length !calls in
      let model_identity () =
        session (fun store ->
            let first, second =
              Eio.Fiber.pair
                (fun () -> S.provider store)
                (fun () -> S.provider store)
            in
            check "concurrent native provider access shares initialization"
              (Option.get first == Option.get second);
            let status = S.status store in
            check "native status preserves corpus coverage without inference"
              (coverage status = (0, 19));
            let identity =
              J.decode
                (member "embedding_provider"
                   (member "model_identity" Jsont.string))
                status
            in
            let result =
              Bushel_agent.invoke
                (Bushel_agent.of_store store ~max_bytes:2048)
                ~check:noop "knowledge_search"
                {|{"query":"sharedcorpusmarker"}|}
            in
            check "native lexical search does not load invalid fixture weights"
              (Result.is_ok result);
            identity)
      in
      let native_identity = model_identity () in
      check "native sessions share the same model identity"
        (model_identity () = native_identity);
      write (Filename.concat model_dir "tokenizer.json") "changed tokenizer";
      check "native tokenizer revisions select a new vector index"
        (model_identity () <> native_identity);
      check "native status and lexical search make no HTTP requests"
        (List.length !calls = before_native_status);
      configure ();
      let allowed = ref true in
      check "authority checked after operation"
        (try
           session
             ~check:(fun () -> if not !allowed then failwith "revoked")
             (fun _ -> allowed := false);
           false
         with Failure _ -> true);
      print_endline
        "shared knowledge indexing, reuse, locking and configuration tests \
         passed")

let () =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "--hold-lock" then
    holds_lock Sys.argv.(2)
  else if Array.length Sys.argv = 3 && Sys.argv.(1) = "--index-until-killed"
  then index_until_killed Sys.argv.(2)
  else run ()
