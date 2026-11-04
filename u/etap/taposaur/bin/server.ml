open Cmdliner
let serve root db port reviews_db journal_links audit_db jobs_db =
  let absolute path =
    try Unix.realpath path with Unix.Unix_error _ ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
      else path in
  let storage : Taposaur_web.storage = {
    mirrors_file = (let p=Taposaur_mirrors.default_path () in
      if Sys.file_exists p then Some (absolute p) else None);
    abstracts_file = (let p=Taposaur_abstracts.default_path () in
      if Sys.file_exists p then Some (absolute p) else None);
    audit_file = Option.map absolute audit_db;
    archive_root = absolute root; catalog_file = absolute db;
    reviews_file = Option.map absolute reviews_db;
    jobs_file = Option.map absolute jobs_db;
    journal_links_file = Option.map absolute journal_links;
    working_directory = Sys.getcwd (); executable = absolute Sys.executable_name;
  } in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let archive = Taposaur_archive.open_root ~sw Eio.Path.(env#fs / root) in
  let catalog = Taposaur_catalog.open_read ~sw
    Eio.Path.(env#fs / db) in
  let reviews = Option.map (fun path -> Taposaur_review_store.open_read ~sw
    Eio.Path.(env#fs / path)) reviews_db in
  let journal_links = Option.map (fun path ->
    let data = Eio.Path.with_open_in Eio.Path.(env#fs / path) (fun flow ->
      Eio.Buf_read.(parse_exn take_all ~max_size:(16 * 1024 * 1024) flow)) in
    match Taposaur_web.Journal_links.of_jsonl data with
    | Ok links -> links | Error e -> failwith e) journal_links in
  let audit = Option.map (fun path -> Taposaur_audit.open_read ~sw
    Eio.Path.(env#fs / path)) audit_db in
  let jobs = Option.map (fun path -> Duraml_sqlite.open_ ~sw ~clock:env#clock
    ~random:env#secure_random ~read_only:true Eio.Path.(env#fs / path)) jobs_db in
  let mirrors=Taposaur_mirrors.open_configured ~sw ~fs:env#fs in
  let publisher_lookup doi = Option.bind mirrors (fun m -> Taposaur_mirrors.find m doi) in
  let abstracts=Taposaur_abstracts.open_configured ~sw ~fs:env#fs in
  let abstract_lookup doi = Option.bind abstracts (fun a -> Taposaur_abstracts.find a doi) in
  let state = Taposaur_web.create ~publisher_lookup ~abstract_lookup ?audit ~storage ?reviews ?jobs
    ~mono_clock:env#mono_clock ?journal_links ~archive ~catalog () in
  Proffer_httpz.run ~sw ~port env ~env:state Taposaur_web.site

let term = Term.(const serve $
  Arg.(value & opt string "/data/papers/taposaur" & info ["root"]
    ~doc:"Archive root. The browser reads this tree without modifying it.") $
  Arg.(value & opt string "taposaur.db" & info ["db"]
    ~doc:"Existing local catalogue, opened read-only.") $
  Arg.(value & opt int 8765 & info ["port"] ~doc:"Loopback HTTP port.") $
  Arg.(value & opt (some string) None & info ["reviews-db"]
    ~doc:"Existing review database, opened read-only.") $
  Arg.(value & opt (some string) None & info ["journal-links"]
    ~doc:"Local OpenAlex journal/source-ID JSONL cache for citation links.") $
  Arg.(value & opt (some string) None & info ["audit-db"]
    ~doc:"Existing audit database, opened read-only for /audit queries.") $
  Arg.(value & opt (some string) None & info ["jobs-db"]
    ~doc:"Existing Duraml job queue, opened read-only for /jobs pages."))
let () = exit (Cmd.eval (Cmd.v
  (Cmd.info "taposaur-web" ~doc:"Browse the paper archive with Proffer.") term))
