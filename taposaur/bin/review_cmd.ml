open Cmdliner
module R = Taposaur_review
module S = Taposaur_review_store
module X = Taposaur_review_run
module C = Taposaur_catalog
let ( / ) = Eio.Path.( / )
let execute ?action f = try
  Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
    match action with
    | None -> f env sw None
    | Some action ->
        let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
          ~component:"taposaur.review" in
        Taposaur_audit.with_operation ~session:audit ~action
          ~subject:action (fun audit -> f env sw audit)));
  Ok ()
  with
  | Failure e | Invalid_argument e | Sqlite3.Error e | Sqlite3.SqliteError e ->
      Error e
  | Eio.Io _ as ex -> Error (Printexc.to_string ex)
  | Eio.Buf_read.Buffer_limit_exceeded -> Error "Input exceeds its byte budget"
  | S.Not_found -> Error "Review record or input blob not found"
let read ?(max_bytes=32 * 1024 * 1024) env path =
  if max_bytes < 1 || max_bytes > 900 * 1024 * 1024 then
    invalid_arg "Input byte budget must be 1..900 MiB";
  Eio.Path.with_open_in (env#fs / path) (fun flow ->
    Eio.Buf_read.(parse_exn take_all ~max_size:max_bytes flow))
let write_store ?audit env sw path =
  let audit = match audit with Some audit -> audit | None ->
    Taposaur_audit_cli.open_session ~sw ~fs:env#fs ~component:"taposaur.review" in
  S.open_write ~audit ~sw (env#fs / path)
let read_store env sw path = S.open_read ~sw (env#fs / path)
let catalogue env sw path = C.open_read ~sw (env#fs / path)
let identity store slug title requester owner actor =
  match S.find store slug with
  | Some r -> Option.value ~default:r.title title,
      Option.value ~default:r.requester requester,
      Option.value ~default:r.owner owner
  | None ->
      let require name = function Some v -> v
        | None -> invalid_arg ("New review requires --" ^ name) in
      require "title" title,require "requester" requester,
      Option.value ~default:actor owner
let reviews_db = Arg.(value & opt string "reviews.db" &
  info ["reviews-db"] ~doc:"Durable review SQLite database.")
let db = Arg.(value & opt string "taposaur.db" &
  info ["db"] ~doc:"Read-only paper catalogue.")
let actor = Arg.(value & opt string "system" &
  info ["by"] ~doc:"Actor responsible for this operation.")
let reason = Arg.(required & opt (some string) None &
  info ["reason"] ~doc:"Reason for the definition revision.")
let expected = Arg.(required & opt (some int) None & info ["expected"]
  ~doc:"Expected current revision, or zero for a new review.")
let slug = Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG")
let file position = Arg.(required & pos position (some string) None &
  info [] ~docv:"FILE")
let title = Arg.(value & opt (some string) None & info ["title"])
let requester = Arg.(value & opt (some string) None & info ["requester"])
let owner = Arg.(value & opt (some string) None & info ["owner"])
let format = Arg.(value & opt
  (enum ["doi_lines",R.Definition.Doi_lines;
         "metadata_jsonl",R.Definition.Metadata_jsonl])
  R.Definition.Doi_lines & info ["input-format"])
let label = Arg.(required & opt (some string) None & info ["label"])
let parent = Arg.(value & opt (some int) None & info ["parent"])
let id name = Arg.(required & pos 0 (some int) None & info [] ~docv:name)
let partial = Arg.(value & flag & info ["partial"]
  ~doc:"Record incomplete results, including invalid input lines.")
let max_members = Arg.(value & opt int 100000 & info ["max-members"]
  ~doc:"Explicit occurrence or snapshot member budget (maximum 2000000).")
let max_bytes = Arg.(value & opt int (256 * 1024 * 1024) & info ["max-bytes"]
  ~doc:"Frozen observation byte budget (maximum 900 MiB).")
let capture_seconds = Arg.(value & opt float 60. & info ["capture-seconds"]
  ~doc:"Consistent snapshot capture deadline (maximum 3600 seconds).")
let cmd name doc term = Cmd.v (Cmd.info name ~doc) term
let command = Cmd.group (Cmd.info "review"
  ~doc:"Version review inputs, capture metadata and export frozen results.") [
  cmd "blob" "Retain an input and print its SHA-256 reference."
    Term.(const (fun path file max_bytes -> execute ~action:"cli.review.blob" (fun env sw audit ->
      let raw = read ~max_bytes env file in
      print_endline (S.put_blob (write_store ?audit env sw path) raw))) $
      reviews_db $ file 0 $ max_bytes);
  cmd "define" "Append a validated query definition revision."
    Term.(const (fun path slug file title requester owner actor reason expected ->
      execute ~action:"cli.review.define" (fun env sw audit ->
        let definition = match R.Definition.of_string (read env file) with
          | Ok d -> d | Error e -> invalid_arg e in
        let store = write_store ?audit env sw path in
        let title,requester,owner =
          identity store slug title requester owner actor in
        let n = S.define store ~slug ~title ~requester ~owner ~actor ~reason
          ~expected definition in
        Printf.printf "Revision %d\n" n)) $
      reviews_db $ slug $ file 1 $ title $ requester $ owner $ actor $
      reason $ expected);
  cmd "import" "Import a DOI list or metadata JSONL and create a snapshot."
    Term.(const (fun path db slug file title requester owner actor reason
        expected format provider query parameters partial label -> execute ~action:"cli.review.import" (fun env sw audit ->
      let raw = read env file in
      let parsed = R.parse format raw in
      if parsed.diagnostics <> [] && not partial then
        invalid_arg "Invalid input lines. Correct them or explicitly use --partial";
      let store = write_store ?audit env sw path in
      let catalog = catalogue env sw db in
      let title,requester,owner = identity store slug title requester owner actor in
      let blob = S.put_blob store raw in
      let parameters = Option.map (fun file ->
        match R.decode Jsont.json (read env file) with
        | Ok json -> json | Error e -> invalid_arg e) parameters in
      let source = match provider,query with
        | None,None when parameters = None ->
            {R.Definition.kind=List;format;blob=Some blob;
             local=None;provider=None;query=None;parameters=None}
        | Some provider,Some query -> {R.Definition.kind=Recorded_search;
            format;blob=None;local=None;provider=Some provider;
            query=Some query;parameters}
        | _ -> invalid_arg "Use --provider and --query together" in
      let definition = {R.Definition.version=1;
        inputs=[{key="main";label="Imported input";source}];
        include_any=["main"];require_all=[];exclude_any=[]} in
      let n = S.define store ~slug ~title ~requester ~owner ~actor ~reason
        ~expected definition in
      let revision = S.revision store ~slug ~number:(Some n) in
      let run_id = X.execute store catalog revision ~actor
        [{key="main";bytes=raw;partial}] in
      let snapshot = X.snapshot store catalog ~run_id ~parent:None ~label ~actor in
      Printf.printf "Revision %d; run %d; snapshot %d\n" n run_id snapshot)) $
      reviews_db $ db $ slug $ file 1 $ title $ requester $ owner $ actor $
      reason $ expected $ format $
      Arg.(value & opt (some string) None & info ["provider"]) $
      Arg.(value & opt (some string) None & info ["query"]
        ~doc:"Exact native query for externally recorded search results.") $
      Arg.(value & opt (some string) None & info ["parameters"]
        ~doc:"JSON file of native search parameters to retain with the query.") $
      partial $ label);
  cmd "run" "Execute a definition using catalogue or recorded inputs."
    Term.(const (fun path db slug revision actor bindings partial max_members ->
      execute ~action:"cli.review.run" (fun env sw audit ->
        let store = write_store ?audit env sw path in
        let bindings = List.map (fun binding ->
          match String.index_opt binding '=' with
          | None -> invalid_arg "Input binding must be KEY=FILE"
          | Some i ->
              let key = String.sub binding 0 i in
              let file = String.sub binding (i+1)
                (String.length binding-i-1) in
              {X.key;bytes=read env file;partial=List.mem key partial}) bindings in
        List.iter (fun key -> if not (List.exists (fun b -> b.X.key = key)
          bindings) then invalid_arg "Partial input requires a binding") partial;
        let revision = S.revision store ~slug ~number:(Some revision) in
        let id = X.execute ~max_members store (catalogue env sw db)
          revision ~actor bindings in
        Printf.printf "Run %d\n" id)) $
      reviews_db $ db $ slug $
      Arg.(required & opt (some int) None & info ["revision"]) $ actor $
      Arg.(value & opt_all string [] & info ["input"] ~doc:"KEY=FILE results.") $
      Arg.(value & opt_all string [] & info ["partial-input"]) $ max_members);
  cmd "snapshot" "Capture current metadata for a fixed run membership."
    Term.(const (fun path db run_id parent label actor max_members max_bytes
        capture_seconds -> execute ~action:"cli.review.snapshot" (fun env sw audit ->
      let store = write_store ?audit env sw path in
      let mirrors = Taposaur_mirrors.open_configured ~sw ~fs:env#fs in
      let publishers = match mirrors with None -> [] | Some mirrors ->
        List.filter_map (fun (m : R.member) ->
          Taposaur_mirrors.find mirrors (Result.get_ok (Taposaur.Doi.of_string m.doi)))
          (S.members store run_id) in
      let id = X.snapshot ~publishers ~max_members ~max_bytes ~capture_seconds
        store (catalogue env sw db)
        ~run_id ~parent ~label ~actor in
      Printf.printf "Snapshot %d\n" id)) $
      reviews_db $ db $ id "RUN" $ parent $ label $ actor $
      max_members $ max_bytes $ capture_seconds);
  cmd "export" "Read an immutable cached snapshot export or manifest."
    Term.(const (fun path id format output -> execute (fun env sw _audit ->
      let store = read_store env sw path in
      let bytes = match format with
        | "manifest" -> S.blob store (S.snapshot store id).manifest ^ "\n"
        | "doi" -> S.export store id R.Dois
        | "csv" -> S.export store id R.Csv
        | _ -> S.export store id R.Jsonl in
      match output with None -> print_string bytes
      | Some path ->
          let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
            ~component:"taposaur.review" in
          Taposaur_audit.with_operation ~session:audit
            ~action:"review.export.write" ~subject:("snapshot:" ^ string_of_int id)
            ~details:["path",path;"format",format;
              "bytes",string_of_int (String.length bytes)]
            (fun _ -> Eio.Path.save ~create:(`Exclusive 0o600)
              (env#fs / path) bytes))) $
      reviews_db $ id "SNAPSHOT" $
      Arg.(value & opt (enum ["doi","doi";"csv","csv";
        "jsonl","jsonl";"manifest","manifest"]) "csv" & info ["format"]) $
      Arg.(value & opt (some string) None & info ["output"]));
  cmd "cache-statistics" "Cache coverage from frozen snapshot observations."
    Term.(const (fun path id -> execute ~action:"cli.review.cache-statistics" (fun env sw audit ->
      S.cache_statistics (write_store ?audit env sw path) id;
      Printf.printf "Cached statistics for snapshot %d\n" id)) $
      reviews_db $ id "SNAPSHOT");
  cmd "show" "Print a saved definition without executing it."
    Term.(const (fun path slug number -> execute (fun env sw _audit ->
      let r = S.revision (read_store env sw path) ~slug ~number in
      print_endline (R.Definition.to_string r.definition))) $
      reviews_db $ slug $
      Arg.(value & opt (some int) None & info ["revision"]));
  cmd "list" "List review requests and their current definition revisions."
    Term.(const (fun path -> execute (fun env sw _audit ->
      S.list (read_store env sw path) |> List.iter (fun (r : S.review) ->
        Printf.printf "%s\tr%d\t%s\t%s\n" r.slug r.revision r.requester r.title))) $
      reviews_db);
  cmd "history" "List runs and snapshots for a review."
    Term.(const (fun path slug -> execute (fun env sw _audit ->
      let store = read_store env sw path in
      List.iter (fun (r : S.run) -> Printf.printf
        "run %d\trevision %d\t%d members\tpartial=%b\n"
        r.id r.revision r.members r.partial) (S.runs store ~slug);
      List.iter (fun (s : S.snapshot) -> Printf.printf
        "snapshot %d\trun %d\t%d abstracts/%d members\t%s\n"
        s.id s.run_id s.abstracts s.members s.label) (S.snapshots store ~slug))) $
      reviews_db $ slug);
  cmd "compare" "Compare two snapshots of the same review."
    Term.(const (fun path before after -> execute (fun env sw _audit ->
      let d = S.compare (read_store env sw path) ~before ~after in
      Printf.printf "Added %d; removed %d; changed %d; new abstracts %d\n"
        d.added d.removed d.changed d.abstracts_added)) $
      reviews_db $ id "BEFORE" $
      Arg.(required & pos 1 (some int) None & info [] ~docv:"AFTER"))]
