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
  | A.Parse_error e -> Error (A.error_to_string e)
  | Failure e | Invalid_argument e -> Error e
  | Eio.Io _ as e -> Error (Printexc.to_string e)
  | Sqlite3.Error e | Sqlite3.SqliteError e -> Error e

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

let import root db force limit prefix shard = execute (fun env sw ->
  let a = archive env sw root in
  let c = C.open_write ~sw (env#fs / db) in
  let progress shard result = match result with
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

let enrich root db force limit uid jobs = execute (fun env sw ->
  let a = archive env sw root in
  let c = C.open_write ~sw (env#fs / db) in
  let count = ref 0 in
  let progress uid result =
    incr count;
    (match result with E.Unavailable message ->
      Printf.eprintf "%s: %s\n%!" (Uid.to_string uid) message | _ -> ());
    if !count mod 100 = 0 then
      Printf.eprintf "Resolved %d papers\n%!" !count in
  let result = E.run ~force ?limit ?uid:(Option.map identify uid)
    ~jobs ~progress c a in
  Printf.printf "Examined %d; updated %d; unavailable %d; superseded %d\n%!"
    result.examined result.updated result.unavailable result.superseded;
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
  command "inspect" "Read one paper manifest."
    Term.(const inspect $ root $ id);
  command "describe" "Resolve paper metadata and show its evidence."
    Term.(const describe $ root $ id);
  command "enrich" "Resolve metadata into the local search catalogue."
    Term.(const enrich $ root $ db $
      Arg.(value & flag & info ["force"] ~doc:"Refresh completed metadata.") $
      Arg.(value & opt (some int) None & info ["limit"]
        ~doc:"Maximum papers to resolve. Repeated runs resume.") $
      Arg.(value & opt (some string) None & info ["uid"]
        ~doc:"Resolve one imported identifier.") $
      Arg.(value & opt int 8 & info ["jobs"]
        ~doc:"Concurrent paper reads, 1..64."));
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
