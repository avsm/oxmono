module M = Siftbot_model
module S = Siftbot.Store

let check name b = if not b then failwith name

let () =
  Eio_main.run (fun env ->
      let path = Filename.temp_file "siftbot-store-" ".sqlite3" in
      let root = Filename.temp_file "siftbot-dooit-" "" in
      Unix.unlink root;
      let first = List.hd Siftbot.Demo.messages in
      let approved =
        M.
          {
            id = "teaching";
            name = "Teaching";
            definition = "Teaching responsibility";
            exclusions = "";
            parent = None;
            status = "proposed";
            revision = 0;
          }
      in
      Eio.Switch.run (fun sw ->
          let db = S.open_db ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
          Siftbot.Demo.seed db;
          S.propose db [ approved ] [];
          S.edit db ~revision:(S.revision db) (M.Approve approved.id)
            ~explanation:"Teaching is my responsibility";
          let a =
            M.
              {
                target = first.source;
                conversation = false;
                dimension = Area;
                value = approved.id;
                origin = Admin;
                version = "test";
                source_revision = first.revision;
                explanation = "Confirmed";
                evidence = [ first.source ];
                score = None;
              }
          in
          S.edit db ~revision:(S.revision db) (M.Assign a)
            ~explanation:"Teaching example";
          let overview = Siftbot.Overview.area db ~area_id:approved.id in
          check "source-linked overview" (String.length overview > 100);
          let id, reports =
            Siftbot.Evaluation.freeze_baseline db ~cutoff:"2026-07-01T00:00:00Z"
          in
          check "frozen reports by dimension" (List.length reports = 16);
          check "frozen evaluation persisted"
            (List.mem_assoc id (S.evaluations db));
          S.snooze db ~revision:(S.revision db) ~target:first.source
            ~until_date:"2026-12-01T00:00:00Z"
            ~explanation:"Term preparation later";
          check "local snooze"
            (S.snoozed db ~now:"2026-11-01T00:00:00Z" first.source);
          check "snooze expires"
            (not (S.snoozed db ~now:"2026-12-01T00:00:00Z" first.source));
          ignore (Dooit.Store.init root);
          let preview =
            Siftbot.Task.preview first ~title:"Review teaching timetable"
              ~deadline:"2026-12-01T12:00:00Z" ()
          in
          let ids =
            Siftbot.Task.capture db ~revision:(S.revision db) ~dooit_root:root
              preview
          in
          let repeated =
            Siftbot.Task.capture db ~revision:(S.revision db) ~dooit_root:root
              preview
          in
          check "Dooit capture receipt deduplicates retries"
            (ids = repeated && List.length ids = 1));
      Eio.Switch.run (fun sw ->
          let db = S.open_db ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
          check "source survives restart" (S.get db first.source = Some first);
          check "decisions survive restart"
            (List.exists
               (fun a -> a.M.origin = Admin)
               (S.snapshot db ~query:"").assignments);
          check "evaluation survives restart"
            (List.length (S.evaluations db) = 1);
          let preview =
            Siftbot.Task.preview first ~title:"Review teaching timetable"
              ~deadline:"2026-12-01T12:00:00Z" ()
          in
          ignore
            (Siftbot.Task.capture db ~revision:(S.revision db) ~dooit_root:root
               preview);
          let notes, errors = Dooit.Store.scan (Dooit.Store.open_ root) in
          check "capture restart creates no duplicate"
            (List.length notes = 1 && errors = []);
          check "Dooit owns the structured due date"
            (Dooit.Common.field "due" (List.hd notes).Dooit.Doc.meta
            = "2026-12-01"));
      let rec remove path =
        if Sys.is_directory path then (
          Array.iter
            (fun file -> remove (Filename.concat path file))
            (Sys.readdir path);
          Unix.rmdir path)
        else Unix.unlink path
      in
      remove root;
      List.iter
        (fun suffix ->
          let name = path ^ suffix in
          if Sys.file_exists name then Unix.unlink name)
        [ ""; "-wal"; "-shm" ];
      print_endline
        "Persistence, evaluation, snooze and Dooit lifecycle tests passed")
