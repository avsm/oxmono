open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module I = Taposaur_indexing.Import
module E = Taposaur_indexing.Enrich
open Cmdliner
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> failwith e
let identify s = get (Uid.of_input s)

let execute f =
  try
    Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f env sw));
    Ok ()
  with
  | Taposaur_store.Busy -> Error "Another writer owns this staged paper"
  | A.Parse_error e -> Error (A.error_to_string e)
  | Failure e | Invalid_argument e -> Error e
  | Eio.Io _ as e -> Error (Printexc.to_string e)
  | Sqlite3.Error e | Sqlite3.SqliteError e -> Error e

let mutate ?(details=[]) action subject f = execute (fun env sw ->
  let audit = Taposaur_audit_cli.open_session ~sw ~fs:env#fs
    ~component:"taposaur" in
  Taposaur_audit.with_operation ~session:audit ~details ~action ~subject
    (fun scoped -> f env sw (Option.get scoped)))

let archive env sw root = A.open_root ~sw (env#fs / root)
let catalog env sw db = C.open_read ~sw (env#fs / db)
let paper a id = match A.find a (identify id) with
  | Some p -> p | None -> failwith ("Paper not found: " ^ id)
let inspect root id = execute (fun env sw ->
  let p = paper (archive env sw root) id in
  print_endline (get (Manifest.to_string (A.manifest p))))
let events root id = execute (fun env sw ->
  A.iter_events (paper (archive env sw root) id) (fun _ e ->
    print_endline (get (Event.to_string e))))

let import root db force limit prefix shard =
  mutate ~details:["archive_root",root;"force",string_of_bool force;
    "prefix",prefix] "catalog.import" db (fun env sw audit ->
  let a = archive env sw root in
  let c = C.open_write ~sw (env#fs / db) in
  let progress shard result =
    let outcome = match result with I.Loaded _ -> "loaded"
      | I.Unchanged -> "unchanged" | I.Failed _ -> "failed" in
    (match result with I.Unchanged -> () | _ ->
      Taposaur_audit.record audit ~action:"catalog.shard.import" ~subject:shard
        ~details:(["outcome",outcome;"database",db] @
          match result with I.Loaded n -> ["rows",string_of_int n] | _ -> []) ());
    match result with
    | I.Loaded n -> Printf.eprintf "%s: loaded %d papers\n%!" shard n
    | I.Unchanged -> ()
    | I.Failed e -> Printf.eprintf "%s: %s\n%!" shard e in
  match shard with
  | Some shard ->
      let r = I.shard ~force c a ~shard in
      progress shard r;
      (match r with I.Failed e -> failwith e | _ -> ())
  | None ->
      let s = I.sync ~force ?limit ~prefix ~progress c a in
      Taposaur_audit.record audit ~action:"catalog.import.result" ~subject:db
        ~details:["scanned",string_of_int s.scanned;"loaded",string_of_int s.loaded;
          "failed",string_of_int s.failed;"unchanged",string_of_int s.unchanged] ();
      Printf.printf "Discovered %d; loaded %d; unchanged %d; failed %d\n"
        s.scanned s.loaded s.unchanged s.failed;
      if s.failed > 0 then failwith "Some shards failed; see import diagnostics")

let describe root id = execute (fun env sw ->
  let p = Taposaur_reader.load (paper (archive env sw root) id) in
  let m = p.metadata in
  Printf.printf "%s\n" (Uid.to_string p.manifest.id);
  let field name show field =
    List.iteri (fun i (e : _ Taposaur_paper.evidence) ->
      Printf.printf "%s%s: %s [%s]\n"
        name (if i = 0 then "" else " (alternative)") (show e.value)
        (Taposaur_paper.source_name e.source))
      (Taposaur_paper.Field.candidates field) in
  let authors xs = String.concat "; " (List.map (fun (a : Author.t) ->
    String.trim (Option.value ~default:"" a.given ^ " " ^ a.family)) xs) in
  field "Title" Fun.id m.title; field "Authors" authors m.authors;
  field "Journal" Fun.id m.journal; field "Publisher" Fun.id m.publisher;
  field "Published" Fun.id m.published; field "Year" string_of_int m.year;
  field "Abstract" Fun.id m.abstract; field "Volume" Fun.id m.volume;
  field "Issue" Fun.id m.issue; field "Pages" Fun.id m.pages;
  field "ISSN" Fun.id m.issn;
  List.iter (fun (d : Taposaur_paper.document) -> match d.bibliography with
    | Ok _ -> ()
    | Error e -> Printf.printf "Metadata error (%s): %s\n" d.artifact.file e)
    p.documents)

let enrich root db force retry_errors limit uid jobs order progress_file =
  mutate ~details:["archive_root",root;"force",string_of_bool force;
    "jobs",string_of_int jobs;"uid",Option.value ~default:"" uid]
    "catalog.enrich" db (fun env sw audit ->
  let a = archive env sw root in
  let c = C.open_write ~sw (env#fs / db) in
  let count = ref 0 in
  let progress_lock = Eio.Mutex.create () in
  let started = Unix.gettimeofday () and last_uid = ref "" in
  let last_shard = ref "" in
  let save_progress state = Eio.Mutex.use_rw ~protect:true progress_lock
    (fun () -> Option.iter (fun file ->
    let quote s = get (Jsont_bytesrw.encode_string Jsont.string s) in
    let body = Printf.sprintf
      {|{"pid":%d,"state":%s,"started":%.3f,"updated":%.3f,
"jobs":%d,"order":%s,"examined":%d,"details_cached":%d,"last_uid":%s,
"last_shard":%s,"database":%s}|}
      (Unix.getpid ()) (quote state) started (Unix.gettimeofday ()) jobs
      (quote (match order with E.Directory -> "directory" | Uid -> "uid")) !count
      (C.Metadata.detail_count c) (quote !last_uid) (quote !last_shard)
      (quote db) in
    Eio.Path.save ~create:(`Or_truncate 0o600)
      (env#fs / (file ^ ".tmp")) (body ^ "\n");
    Eio.Path.rename (env#fs / (file ^ ".tmp")) (env#fs / file)) progress_file) in
  let progress uid result =
    incr count;
    last_uid := Uid.to_string uid;
    last_shard := Layout.index uid;
    (match result with E.Unavailable message ->
      Printf.eprintf "%s: %s\n%!" (Uid.to_string uid) message | _ -> ());
    if !count mod 100 = 0 then begin
      Printf.eprintf "Resolved %d papers; details cached: %d\n%!" !count
        (C.Metadata.detail_count c);
      save_progress "running"
    end in
  save_progress "running";
  let mirrors = Taposaur_mirrors.open_configured ~sw ~fs:env#fs in
  let publisher_lookup doi = Option.bind mirrors (fun s -> Taposaur_mirrors.find s doi) in
  let result = try E.run ~publisher_lookup ~audit ~force ~retry_errors ~order ?limit
    ?uid:(Option.map identify uid) ~jobs ~progress c a
    with ex -> save_progress "failed"; raise ex in
  save_progress (if result.unavailable > 0 then "finished_with_errors"
    else "finished");
  Taposaur_audit.record audit ~action:"catalog.enrich.result" ~subject:db
    ~details:["examined",string_of_int result.examined;
      "updated",string_of_int result.updated;"partial",string_of_int result.partial;
      "unavailable",string_of_int result.unavailable;
      "superseded",string_of_int result.superseded] ();
  Printf.printf "Examined %d; updated %d; unavailable %d; superseded %d\n%!"
    result.examined result.updated result.unavailable result.superseded;
  Printf.printf "Partial metadata: %d\n" result.partial;
  if result.unavailable > 0 then failwith "Some paper manifests were unavailable")

let search db text limit after = execute (fun env sw ->
  let c = catalog env sw db in
  let result = C.search c { (C.Query.make ~text ()) with limit; after } in
  List.iter (fun e -> print_endline (get (Summary.to_string e))) result.papers;
  Option.iter (fun s -> Printf.eprintf "Next cursor: %s\n" s) result.next)

let stats db = execute (fun env sw ->
  let c = catalog env sw db in
  let s = C.stats c in
  Printf.printf
    "Papers: %d\nPDF: %d\nXML: %d\nTEI: %d\nS2ORC: %d\nLoaded shards: %d\nDiscovered shards: %s\nImport errors: %d\n"
    s.papers s.pdf s.xml s.tei s.s2orc s.loaded_shards
    (Option.fold ~none:"unknown" ~some:string_of_int s.discovered_shards) s.errors;
  Printf.printf
    "Titles: %d\nAbstracts: %d\nPublishers: %d\nMetadata resolved: %d\nMetadata errors: %d\n"
    s.titles s.abstracts s.publishers s.enriched s.enrichment_errors;
  Printf.printf "Partial metadata: %d\nStale metadata: %d\n" s.partial s.stale;
  Printf.printf "Paper details cached: %d\n" (C.Metadata.detail_count c);
  List.iter (fun (p,e) -> Printf.printf "%s: %s\n" p e) (C.Shard.errors c);
  List.iter (fun (uid,e) -> Printf.printf "%s: %s\n" (Uid.to_string uid) e)
    (C.Metadata.errors c))

let validate root limit shard artifacts = execute (fun env sw ->
  let count = Taposaur_indexing.Audit.validate ~limit ?shard ~artifacts
    (archive env sw root) in
  Printf.printf "Validated %d papers%s\n" count
    (if artifacts then " and artifact sizes" else ""))

let coverage db file = execute (fun env sw ->
  let c = catalog env sw db in
  A.iter_dois (env#fs / file) (fun _ uid ->
    let status = Taposaur_indexing.Audit.coverage c uid in
    Printf.printf "%s\t%s\n" (Uid.to_string uid)
      (Taposaur_indexing.Audit.coverage_name status)))

let freshness root db limit after =
  mutate "catalog.freshness.check" db (fun env sw audit ->
  let a = archive env sw root in
  let c = C.open_write ~sw (env#fs / db) in
  let result = E.check ~limit ?after c a in
  Taposaur_audit.record audit ~action:"catalog.freshness.result" ~subject:db
    ~details:["checked",string_of_int result.checked;"stale",string_of_int result.stale] ();
  Printf.printf "Checked %d; stale %d\n" result.checked result.stale;
  Option.iter (fun cursor -> Printf.printf "Next cursor: %s\n" cursor)
    result.next)

let retire root db shard =
  mutate ~details:["database",db;"archive_root",root]
    "catalog.shard.retire" shard (fun env sw _audit ->
  I.retire (C.open_write ~sw (env#fs / db)) (archive env sw root) ~shard)

let fetch staging timeout max_bytes url format publisher_pdf id =
  mutate ~details:["staging",staging;"format",format;
    "explicit_url",string_of_bool (Option.is_some url)]
    "cli.fetch" (Uid.to_string (identify id)) (fun env sw audit ->
  let module S = Taposaur_services in
  let getenv name = match Sys.getenv_opt name with
    | None | Some "" -> None | Some value -> Some value in
  let http = Fetch_main.std ~sw ~user_agent:"taposaur/0.1"
    ~max_concurrent:6
    ~retry:(Fetch.Retry.v ~max_retries:0 ()) env in
  let paced seconds = Fetch.with_limits ~clock:env#mono_clock
    ~max_concurrent:1 ~min_interval:(Duration.of_sec seconds) http in
  let glutton = Option.map (fun server -> S.Glutton.client ~server http)
    (getenv "GLUTTON_SERVER_URL") in
  let datacite = Option.map (fun server -> S.Datacite.client ~server http)
    (getenv "DATACITE_SERVER_URL") in
  let elsevier = Option.map (fun key ->
    let inst_token = Option.map (fun s () -> s) (getenv "ELSEVIER_INST_TOKEN") in
    S.Elsevier.client ~api_key:(fun () -> key) ?inst_token (paced 2))
    (getenv "ELSEVIER_API_KEY") in
  let springer = Option.map (fun api_key -> S.Springer.client ~api_key (paced 2))
    (getenv "SPRINGER_API_KEY") in
  let wiley = Option.bind (getenv "WILEY_TOKENS") (fun values ->
    let tokens = String.split_on_char ',' values |> List.map String.trim
      |> List.filter ((<>) "") in
    if tokens = [] then None else Some (S.Wiley.client ~tokens (paced 2))) in
  let core = Option.map (fun token -> S.Core.client ~token:(fun () -> token) (paced 2))
    (getenv "CORE_API_TOKEN") in
  let cup = S.Cup.client (paced 30) in
  let plos = S.Plos.client (paced 2) and tandf = S.Tandf.client (paced 2) in
  let springer_pdf = if publisher_pdf then
    Some (S.Springer.pdf_client (paced 2)) else None in
  let repositories = Fetch.with_headers Fetch.Header.[
    user_agent, "Mozilla/5.0 (compatible; taposaur/1.0; +mailto:avsm2@cam.ac.uk)"
  ] (paced 2) in
  let downloader = Taposaur_download.create ~audit ~timeout ~max_bytes
    ?glutton ?datacite ?elsevier ?springer ?wiley ?core
    ~cup ~plos ~tandf ?springer_pdf ~clock:env#clock repositories in
  let uid = identify id in
  let format = match format with
    | "pdf" -> Manifest.Format.Pdf | "xml" -> Xml
    | _ -> invalid_arg "format must be pdf or xml" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (env#fs / staging);
  let store = Taposaur_store.open_root ~audit ~sw (env#fs / staging) in
  let result = match url with
    | None -> Taposaur_download.fetch downloader store uid
    | Some url -> Taposaur_download.fetch_url downloader store uid ~format url in
  List.iter (fun (a : Manifest.Artifact.t) ->
    Printf.printf "%s/%s\n" (Layout.paper result.uid) a.file) result.artifacts;
  List.iter (fun (a : Taposaur_download.attempt) ->
    Printf.eprintf "%s: %s\n" a.source
      (Taposaur_download.failure_message a.failure)) result.attempts;
  if result.artifacts = [] then failwith "No artifact acquired")

let audit_tei root work_list jobs = execute (fun env sw ->
  if jobs < 1 || jobs > 64 then invalid_arg "jobs must be 1..64";
  let a = archive env sw root in
  let seen = Hashtbl.create 128 and papers = ref [] in
  A.iter_jsonl ~decode:Uid.of_input (env#fs / work_list) (fun _ uid ->
    let key = Uid.to_string uid in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.add seen key ();
      papers := uid :: !papers
    end);
  Eio.Fiber.List.iter ~max_fibers:jobs (fun uid ->
    Taposaur_indexing.Tei_audit.paper a uid |> List.iter (fun report ->
      print_endline (Taposaur_indexing.Tei_audit.to_json report)))
    (List.rev !papers))

let root = Arg.(value & opt string "/data/papers/taposaur" &
  info ["root"] ~doc:"Archive root containing papers and index directories.")
let db = Arg.(value & opt string "taposaur.db" &
  info ["db"] ~doc:"Local SQLite catalogue filename.")
let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"IDENTIFIER")
let limit = Arg.(value & opt (some int) None &
  info ["limit"] ~doc:"Maximum number of shard imports.")
let shard = Arg.(value & opt (some string) None &
  info ["shard"] ~doc:"One index-relative shard, such as doi/10.1016/06.jsonl.")
let force = Arg.(value & flag & info ["force"] ~doc:"Reload unchanged shards.")
let prefix = Arg.(value & opt string "" & info ["prefix"] ~doc:"Shard path prefix.")
let command name doc term = Cmd.v (Cmd.info name ~doc) term
let commands = [
  Taposaur_audit_cli.command;
  Review_cmd.command;
  command "audit-tei" "Validate TEI files and report regeneration evidence."
    Term.(const audit_tei $ root $
      Arg.(required & opt (some string) None & info ["work-list"]
        ~doc:"File containing one DOI or PMCID per line. Reports JSONL.") $
      Arg.(value & opt int 8 & info ["jobs"]
        ~doc:"Concurrent paper audits, 1..64."));
  command "fetch" "Download one paper into a dedicated staging directory."
    Term.(const fetch $
      Arg.(required & opt (some string) None & info ["staging"]
        ~doc:"Staging directory. Accepted papers and indexes are not changed.") $
      Arg.(value & opt float 120. & info ["timeout"]
        ~doc:"Whole-transfer timeout in seconds.") $
      Arg.(value & opt int (128 * 1024 * 1024) & info ["max-bytes"]
        ~doc:"Maximum artifact size.") $
      Arg.(value & opt (some string) None & info ["url"]
        ~doc:"Fetch an explicit URL instead of discovering sources.") $
      Arg.(value & opt string "pdf" & info ["format"]
        ~doc:"Expected content for --url: pdf or xml.") $
      Arg.(value & flag & info ["publisher-pdf"]
        ~doc:"Also try Springer's PDF route using network entitlement.") $ id);
  command "check-freshness" "Mark changed archive papers for enrichment."
    Term.(const freshness $ root $ db $
      Arg.(value & opt int 1000 & info ["limit"] ~doc:"Papers to check, 1..1000.") $
      Arg.(value & opt (some string) None & info ["after"] ~doc:"UID cursor."));
  command "retire-shard" "Remove catalogue rows for a missing source shard."
    Term.(const retire $ root $ db $ id);
  command "inspect" "Read one paper manifest."
    Term.(const inspect $ root $ id);
  command "describe" "Resolve paper metadata and show its evidence."
    Term.(const describe $ root $ id);
  command "enrich" "Resolve metadata into the local search catalogue."
    Term.(const enrich $ root $ db $
      Arg.(value & flag & info ["force"] ~doc:"Refresh completed metadata.") $
      Arg.(value & flag & info ["retry-errors"]
        ~doc:"Retry document and manifest failures.") $
      Arg.(value & opt (some int) None & info ["limit"]
        ~doc:"Maximum papers to resolve. Repeated runs resume.") $
      Arg.(value & opt (some string) None & info ["uid"]
        ~doc:"Resolve one imported identifier.") $
      Arg.(value & opt int 8 & info ["jobs"]
        ~doc:"Concurrent paper reads, 1..64.") $
      Arg.(value & opt (enum ["directory", E.Directory; "uid", E.Uid])
        E.Directory & info ["order"]
        ~doc:"Scan by archive directory (default) or identifier.") $
      Arg.(value & opt (some string) None & info ["progress-file"]
        ~doc:"Atomically write JSON progress for this run to a local file."));
  command "events" "Stream a paper's event log as JSONL."
    Term.(const events $ root $ id);
  command "import" "Load index shards into a local SQLite catalogue."
    Term.(const import $ root $ db $ force $ limit $ prefix $ shard);
  command "search" "Search the catalogue and print JSONL."
    Term.(const search $ db $
      Arg.(value & pos 0 string "" & info [] ~docv:"QUERY") $
      Arg.(value & opt int 25 & info ["limit"] ~doc:"Page size, 1..100.") $
      Arg.(value & opt (some string) None & info ["after"] ~doc:"UID cursor."));
  command "stats" "Show catalogue coverage and import errors."
    Term.(const stats $ db);
  command "validate" "Check sampled index rows against paper manifests."
    Term.(const validate $ root $
      Arg.(value & opt int 100 & info ["limit"] ~doc:"Maximum papers to check.") $
      shard $ Arg.(value & flag & info ["artifacts"] ~doc:"Check artifact sizes."));
  command "coverage" "Compare a DOI work list with the loaded catalogue."
    Term.(const coverage $ db $ id)]

let () = exit (Cmd.eval_result (Cmd.group
  (Cmd.info "taposaur" ~doc:"Read, search and inspect the paper archive.") commands))
