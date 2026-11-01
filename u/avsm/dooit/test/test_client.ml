open Dooit
open Common

let check why value = if not value then failwith why

let refused f =
  try
    ignore (f ());
    failwith "expected refusal"
  with Error _ -> ()

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let () =
  let root = Filename.temp_file "dooit-client-" "" in
  Unix.unlink root;
  mkdir root;
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      Eio_main.run (fun env ->
          let process_mgr = env#process_mgr in
          let git root args =
            Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
              ([ "git"; "-C"; root ] @ args)
            |> String.trim
          in
          let store = Store.init (Filename.concat root "tasks") in
          let origin = Filename.concat root "remote.git" in
          let config =
            Config.parse
              ~path:(Filename.concat root "config.toml")
              ~default_root:store.root
              (Printf.sprintf "backend='git'\n[git]\nremote=%S\nbranch='main'\n"
                 origin)
          in
          let client = Client.of_config ~process_mgr store config in
          check "construction leaves Git alone"
            (not (exists (Filename.concat store.root ".git")));
          (match Client.sync ~dry_run:true client with
          | Git_status status ->
              check "preview does not initialize" (not status.initialized)
          | _ -> failwith "expected Git preview");
          let note = Doc.create ~title:"Local policy" () in
          (match Client.add client ~policy:Local_only note with
          | Applied (_, Local) -> ()
          | _ -> failwith "expected local result");
          check "local policy does not initialize Git"
            (not (exists (Filename.concat store.root ".git")));
          ignore
            (git root [ "init"; "--bare"; "--initial-branch=main"; origin ]);
          ignore (Git.setup ~process_mgr store config.git);
          ignore (git store.root [ "config"; "user.name"; "Dooit Client Test" ]);
          ignore
            (git store.root [ "config"; "user.email"; "test@example.invalid" ]);
          (match Client.sync ~dry_run:false client with
          | Git_synced _ -> ()
          | _ -> failwith "expected Git sync");
          let id = new_uuid () and operation = new_uuid () in
          let moved = origin ^ ".offline" in
          let offline = ref false in
          let check_after_write () =
            Eio.Fiber.check ();
            if (not !offline) && exists (Store.note_path store id) then (
              Unix.rename origin moved;
              offline := true)
          in
          let apply check =
            Client.apply client ~policy:Synchronize ~check ~id ~expected:None
              ~operation ~payload:"test-client-create" (fun _ ->
                Doc.create ~id ~title:"Pending task" ())
          in
          let pending = apply check_after_write in
          (match pending with
          | Pending pending ->
              check "typed pending receipt identifies saved task"
                (pending.id = id && pending.operation = operation);
              check "typed pending receipt has saved revision"
                (pending.revision = Doc.revision (Store.get store id))
          | _ -> failwith "expected pending publication");
          Unix.rename moved origin;
          (match apply Eio.Fiber.check with
          | Applied (result, Pushed) ->
              check "retry publishes saved revision"
                (git origin [ "show"; "main:notes/" ^ id ^ ".md" ]
                = String.trim result.raw)
          | _ -> failwith "expected confirmed publication");
          let local = Client.local store in
          (match
             Client.modify local ~policy:Synchronize (Doc.id note)
               (Doc.change "status" (Some (str "done")))
           with
          | Applied (_, Local) -> ()
          | _ -> failwith "local backend tried remote sync");
          let webdav =
            Config.parse ~path:"/unused/config.toml" ~default_root:store.root
              "backend='webdav'\n\
               [webdav]\n\
               url='https://example.invalid/'\n\
               subdir='tasks'\n\
               username='test'\n\
               password='test'\n"
          in
          let webdav = Client.of_config ~process_mgr store webdav in
          let remote_only = Doc.create ~title:"Must not save" () in
          refused (fun () -> Client.add webdav ~policy:Synchronize remote_only);
          check "unsupported write leaves no task"
            (not (exists (Store.note_path store (Doc.id remote_only))));
          (match Client.add webdav ~policy:Local_only remote_only with
          | Applied (_, Local) -> ()
          | _ -> failwith "explicit local WebDAV edit failed");
          let expired =
            Store.with_transaction store (fun transaction ->
                refused (fun () ->
                    Store.add store (Doc.create ~title:"Contender" ()));
                transaction)
          in
          (try
             ignore
               (Store.apply_transaction expired ~id:(new_uuid ()) ~expected:None
                  ~operation:(new_uuid ()) ~payload:"expired" (fun _ ->
                    remote_only));
             failwith "expired transaction accepted"
           with Invalid_argument _ -> ());
          print_endline
            "Dooit client policies, typed publication and transaction scope \
             passed"))
