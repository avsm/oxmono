let orphan_candidates db =
  let names=ref [] in
  Imap_store.Blob.iter_orphan_candidates db (fun name ->
    names := name :: !names);
  List.sort String.compare !names

let check condition message = if not condition then failwith message
let directory_handles path =
  if not (Sys.file_exists "/proc/self/fd") then 0 else
  Sys.readdir "/proc/self/fd" |> Array.fold_left (fun n entry ->
    try if Unix.readlink ("/proc/self/fd/" ^ entry)=path then n+1 else n
    with Unix.Unix_error _ -> n) 0
let run env =
  let root=Filename.temp_file "imap-blob-gc-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let archive=Filename.concat root "archive" in
  Unix.mkdir archive 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name ->
      let path=Filename.concat archive name in
      if (Unix.lstat path).Unix.st_kind=Unix.S_DIR then Unix.rmdir path
      else Unix.unlink path) (Sys.readdir archive);
    Unix.rmdir archive;
    Array.iter (fun name -> Unix.unlink (Filename.concat root name)) (Sys.readdir root);
    Unix.rmdir root) (fun () -> Eio.Switch.run (fun sw ->
      let db=Imap_store.open_path ~sw
        ~blob_dir:Eio.Path.(Eio.Stdenv.fs env / archive)
        Eio.Path.(Eio.Stdenv.fs env / Filename.concat root "state.db") in
      let raw=Sqlite3.db_open ~mode:`READONLY (Filename.concat root "state.db") in
      Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close raw)) (fun () ->
        let plan=ref [] in
        Sqlite3.Rc.check (Sqlite3.exec raw ~cb:(fun row _ ->
          Array.iter (Option.iter (fun cell -> plan := cell :: !plan)) row)
          "EXPLAIN QUERY PLAN SELECT EXISTS (SELECT 1 FROM blob_refs WHERE sha256='a') OR EXISTS (SELECT 1 FROM sync_operations WHERE blob_sha256='a' AND state NOT IN ('committed','rejected')) OR EXISTS (SELECT 1 FROM intents WHERE digest='a' AND state NOT IN ('confirmed','rejected'))");
        List.iter (fun index ->
          check (List.exists (fun line ->
            String.starts_with ~prefix:"SEARCH " line &&
            List.mem index (String.split_on_char ' ' line)) !plan)
            ("reachability query does not search index " ^ index))
          ["blob_refs_hash";"sync_operations_blob_pending";"intents_blob_pending"]);
      let count=1031 in
      for n=1 to count do
        let name=if n mod 2=0 then Printf.sprintf "sha256-%064x" n
          else Printf.sprintf ".tmp-%d" n in
        Out_channel.with_open_bin (Filename.concat archive name) (fun _ -> ())
      done;
      Out_channel.with_open_bin (Filename.concat archive "unrelated") (fun _ -> ());
      Unix.mkdir (Filename.concat archive ".tmp-directory") 0o700;
      Unix.symlink "missing" (Filename.concat archive ".tmp-dangling");
      let seen=Hashtbl.create count in
      Imap_store.Blob.iter_orphan_candidates db (fun name ->
        check (not (Hashtbl.mem seen name)) "duplicate callback";
        Hashtbl.add seen name ();
        (* Callback does not inherit the database mutex. *)
        ignore (Imap_store.find_intent db ~id:"absent"));
      check (Hashtbl.length seen=count) "multi-batch iterator lost entries";
      check (directory_handles archive=0) "normal iteration leaked directory";
      (try Imap_store.Blob.iter_orphan_candidates db (fun _ -> raise Exit)
       with Exit -> ());
      check (directory_handles archive=0) "exception leaked directory";
      let removed=ref 0 in
      (try Imap_store.Blob.reap_orphans_iter db ~removed:(fun _ ->
        incr removed; raise Exit) with Exit -> ());
      check (!removed=1) "exception callback did not follow unlink";
      check (directory_handles archive=0) "failed reaper leaked directory";
      let entered,release=Eio.Promise.create () in
      Eio.Fiber.first
        (fun () -> Imap_store.Blob.reap_orphans_iter db ~removed:(fun _ ->
          incr removed;
          Eio.Promise.resolve release ();
          Eio.Fiber.await_cancel ()))
        (fun () -> Eio.Promise.await entered);
      check (!removed=2) "cancellation callback did not follow unlink";
      check (directory_handles archive=0) "cancelled reaper leaked directory";
      Imap_store.Blob.reap_orphans_iter db ~removed:(fun _ -> incr removed);
      check (!removed=count) "restart after interruption lost candidates";
      check (orphan_candidates db=[]) "candidates remain";
      check (Sys.file_exists (Filename.concat archive "unrelated")) "unknown file removed";
      check (Sys.file_exists (Filename.concat archive ".tmp-directory")) "directory removed"))
let () = Eio_main.run run
