open Taposaur
open Proffer
module A = Taposaur_archive
module C = Taposaur_catalog
module Reviews = Taposaur_review_store
module Journal_links = Journal_links

type page = Home | Search | Paper | Events | Download | Pdf | Read | Debug | Audit
  | Journals | Script | Katex | Reviews | Review | Review_definition
  | Review_snapshot | Review_papers | Review_export | Review_input | Review_compare
  | Jobs | Job | Activity_page

type t = {
  handle : page -> (Req.t @ local -> Resp.respond @ local -> unit) @ local;
}

type storage = {
  audit_file : string option;
  mirrors_file : string option;
  abstracts_file : string option;
  archive_root : string;
  catalog_file : string;
  reviews_file : string option;
  jobs_file : string option;
  journal_links_file : string option;
  working_directory : string;
  executable : string;
}

let get params key = Option.value (List.assoc_opt key params) ~default:""
let uid params =
  match Uid.of_string (get params "uid") with
  | Ok uid -> uid | Error e -> invalid_arg e
let number params key = match get params key with
  | "" -> None
  | s -> match int_of_string_opt s with
    | Some n -> Some n | None -> invalid_arg (key ^ " must be an integer")

let search_query params =
  let scheme = match get params "scheme" with
    | "" -> None | s -> (match Uid.Scheme.of_string s with
        | Ok s -> Some s | Error e -> invalid_arg e) in
  let holdings = match get params "has" with
    | "" -> C.Query.Any | "pdf" -> C.Query.Pdf
    | "xml" -> C.Query.Xml | "tei" -> C.Query.Tei
    | "s2orc" -> C.Query.S2orc | "unconverted" -> C.Query.Unconverted
    | _ -> invalid_arg "Unknown content filter" in
  let metadata = match get params "metadata" with
    | "" -> []
    | "title" -> [C.Query.Title, C.Query.Present]
    | "abstract" -> [C.Query.Abstract, C.Query.Present]
    | "publisher" -> [C.Query.Publisher, C.Query.Present]
    | "no-title" -> [C.Query.Title, C.Query.Missing]
    | "no-abstract" -> [C.Query.Abstract, C.Query.Missing]
    | "no-publisher" -> [C.Query.Publisher, C.Query.Missing]
    | _ -> invalid_arg "Unknown metadata filter" in
  { (C.Query.make ~text:(get params "q") ()) with scheme; holdings; metadata;
    publisher = get params "publisher"; journal = get params "journal";
    year_from = number params "from"; year_to = number params "to";
    after = List.assoc_opt "after" params }

type response = Html of Status.t * string
  | Artifact of A.paper * Manifest.Artifact.t
  | Media of string * string
  | Attachment of string * string * string

let journal_codec =
  Jsont.Object.map (fun name papers -> {C.name; papers})
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun (j : C.journal) -> j.name)
  |> Jsont.Object.mem "papers" Jsont.int ~enc:(fun (j : C.journal) -> j.papers)
  |> Jsont.Object.finish

let page_name = function
  | Home -> "home" | Search -> "search" | Paper -> "paper" | Events -> "events"
  | Download -> "artifact" | Pdf -> "pdf" | Read -> "read" | Debug -> "debug"
  | Audit -> "audit" | Journals -> "journals" | Script -> "script"
  | Katex -> "katex" | Reviews -> "reviews" | Review -> "review"
  | Review_definition -> "review-definition"
  | Review_snapshot -> "review-snapshot" | Review_papers -> "review-papers"
  | Review_export -> "review-export" | Review_input -> "review-input"
  | Review_compare -> "review-compare" | Jobs -> "jobs" | Job -> "job"
  | Activity_page -> "activity"

let create ?(publisher_lookup=fun _ -> None) ?(abstract_lookup=fun _ -> None)
    ?audit ?storage ?reviews ?jobs
    ?mono_clock ?(journal_links=Journal_links.empty) ~archive ~catalog () =
  let activity = Activity.create () in
  let elapsed () = match mono_clock with
    | None -> fun () -> 0.
    | Some clock ->
        let start = Eio.Time.Mono.now clock in
        fun () -> Mtime.Span.to_float_ns (Mtime.span start
          (Eio.Time.Mono.now clock)) /. 1e6 in
  let supplement p =
    let doi = Uid.doi p.Taposaur_paper.manifest.id in
    let p = match Option.bind doi publisher_lookup with
      | None -> p | Some e -> Taposaur_paper.with_publisher e p in
    match Option.bind doi abstract_lookup with
    | None -> p | Some e -> Taposaur_paper.with_abstract e p in
  let rec prepare page params =
    let html s = Html (Httpz.Res.Success, s) in
    let in_panel = get params "view" = "panel" in
    let content title body =
      if in_panel then View.fragment title body else View.page title body in
    let review_store () = match reviews with
      | Some store -> store
      | None -> invalid_arg "Reviews are not configured on this server" in
    let required_number key = match number params key with
      | Some n when n > 0 -> n
      | _ -> invalid_arg (key ^ " must be a positive integer") in
    if page = Activity_page then
      (if get params "format" = "json" then
        let cursor key = match get params key with
          | "" -> None
          | s -> (match Int64.of_string_opt s with
              | Some n when n >= 0L -> Some n
              | _ -> invalid_arg ("Invalid activity cursor " ^ key)) in
        let web_after = Int64.to_int (Option.value ~default:0L (cursor "web")) in
        let before = cursor "before" and jobs_after = cursor "jobs" in
        let snapshot = get params "snapshot" = "1" in
        (* A snapshot seeds the page from the newest events and the jobs
           still running. Polling then follows the log forward from the
           newest sequence it saw, and older pages walk the tail back. *)
        let described store events = List.map (fun e -> e,
          Taposaur_jobs.Inspect.find store (Duraml.Event.job e)) events in
        let job_events, running, jobs_cursor, older = match jobs with
          | None -> [], [], 0L, None
          | Some store ->
              let tail = match before, jobs_after with
                | Some before, _ -> Some (Duraml.tail ~before ~limit:300 store)
                | None, None -> Some (Duraml.tail ~limit:300 store)
                | None, Some _ -> None in
              (match tail with
               | Some tail ->
                   let events = List.rev tail in
                   let running = if snapshot then Taposaur_jobs.Inspect.list
                       ~state:Duraml.Running ~limit:200 store else [] in
                   let newest = match List.rev events with
                     | [] -> Option.value ~default:0L jobs_after
                     | last :: _ -> Duraml.Event.sequence last in
                   let older = match events with
                     | [] -> None
                     | first :: _ -> Some (Duraml.Event.sequence first) in
                   described store events, running, newest, older
               | None ->
                   let after = Option.get jobs_after in
                   let events = Duraml.events ~after ~limit:500 store in
                   let newest = List.fold_left (fun best e ->
                     max best (Duraml.Event.sequence e)) after events in
                   described store events, [], newest, None) in
        let counts = match jobs with
          | None -> 0, 0
          | Some store ->
              let total state = List.fold_left (fun n queue ->
                n + Duraml.count store (Duraml.query ~queue ~state ()))
                0 Taposaur_jobs.Queues.names in
              total Duraml.Queued, total Duraml.Running in
        Media ("application/json; charset=utf-8",
          Activity_view.feed ~web:(Activity.since activity web_after)
            ~web_cursor:(Activity.cursor activity) ~jobs:job_events ~running
            ~jobs_cursor ~older ~counts)
      else html (Activity_view.page ~jobs:(jobs <> None)))
    else if page = Jobs || page = Job then
      (match jobs with
       | None -> html (View.page "Jobs"
           [View.p "The job queue is not configured. Start the server with --jobs-db."])
       | Some store when page = Job ->
           let id = match Duraml.Id.of_string (get params "id") with
             | Ok id -> id | Error e -> invalid_arg e in
           (match Taposaur_jobs.Inspect.find store id with
            | None -> Html (Httpz.Res.Not_found, View.page "Job not found" [])
            | Some j ->
                let records = match audit with
                  | None -> []
                  | Some store ->
                      let subject = Option.value j.uid
                        ~default:("job:" ^ j.id) in
                      Taposaur_audit.query ~subject ~limit:500 store in
                html (Jobs_view.job j (Taposaur_jobs.Inspect.children store id)
                  records))
       | Some store ->
           let opt key = match get params key with "" -> None | s -> Some s in
           let state = Option.map (fun s ->
             match Taposaur_jobs.Inspect.state_of_string s with
             | Some state -> state
             | None -> invalid_arg "Unknown job state") (opt "state") in
           let after = Option.map (fun s -> match Duraml.Id.of_string s with
             | Ok id -> id | Error e -> invalid_arg e) (opt "after") in
           let jobs = Taposaur_jobs.Inspect.list ?queue:(opt "queue") ?state
             ?after ~limit:100 store in
           html (Jobs_view.index params (Taposaur_jobs.Inspect.counts store)
             jobs))
    else if page = Audit then
      (match audit with
       | None -> html (View.page "Audit log"
           [View.p "Audit database is not configured. Start the server with --audit-db."])
       | Some store ->
         let opt key = match get params key with "" -> None | s -> Some s in
         let before = Option.map (fun s -> match Int64.of_string_opt s with
           | Some n when n > 0L -> n
           | _ -> invalid_arg "Invalid audit cursor") (opt "before") in
         let events = Taposaur_audit.query ?actor:(opt "actor")
           ?action:(opt "action") ?subject:(opt "subject") ?run:(opt "run")
           ?operation:(opt "operation")
           ?phase:(Option.map Taposaur_audit.phase_of_string (opt "phase"))
           ~incomplete:(get params "incomplete" = "1") ?before store in
         if get params "format" = "jsonl" then
           Media ("application/x-ndjson; charset=utf-8",
             String.concat "" (List.map (fun e -> Taposaur_audit.to_json e ^ "\n") events))
         else html (Audit_view.render params events))
    else if page = Debug then
      let rows = match storage with
        | None -> []
        | Some s ->
          let path name file description = name, file, description in
          let optional name file description = path name
            (Option.value ~default:"Not configured" file) description in
          [optional "Mirror evidence (TAPOSAUR_MIRRORS_DB)" s.mirrors_file
             "Durable DOI responses, publisher evidence and original CSV fallback. Read-only here.";
           optional "Abstract evidence (TAPOSAUR_ABSTRACTS_DB)" s.abstracts_file
             "Retained OpenAlex responses and the abstracts decoded from them, applied when the archive has none. Read-only here.";
           optional "Audit log (--audit-db)" s.audit_file
             "Append-only operational audit. Separate from per-paper history and review snapshots.";
           path "Archive root (--root)" s.archive_root
             "Source archive. The website reads it without modification.";
           path "Paper files" (Filename.concat s.archive_root "papers")
             "PDFs, publisher XML, TEI and S2ORC, grouped by paper identifier.";
           path "Import shards" (Filename.concat s.archive_root "index")
             "JSONL source indexes imported into the local catalogue.";
           path "Catalogue (--db)" s.catalog_file
             "SQLite search index, resolved metadata and cached paper details. Rebuildable.";
           optional "Reviews (--reviews-db)" s.reviews_file
             "Durable review definitions, retained inputs, runs, snapshots and exports.";
           optional "Job queue (--jobs-db)" s.jobs_file
             "Duraml acquisition, conversion and refresh jobs. Opened read-only here; workers run separately.";
           optional "Journal links (--journal-links)" s.journal_links_file
             "OpenAlex citation-link mappings loaded at startup.";
           path "Working directory" s.working_directory
             "Relative command-line paths are resolved here.";
           path "Executable" s.executable "The running OCaml website binary."] in
      let paper_rows = match storage, get params "uid" with
        | Some s, identifier when identifier <> "" ->
          (match A.find archive (uid params) with
           | None -> []
           | Some paper ->
             let dir = Filename.concat s.archive_root (A.directory paper) in
             ["Paper directory",dir,"Archive directory for this paper.";
              "Manifest",Filename.concat dir "paper.json",
                "Artifact list and projected paper state.";
              "Event log",Filename.concat dir "events.jsonl",
                "Append-only source history."] @
             List.map (fun (a : Manifest.Artifact.t) ->
               a.file,Filename.concat dir a.file,
               Manifest.Format.to_string a.format ^ " · " ^
               Manifest.Source.to_string a.source)
               (A.manifest paper).artifacts)
        | _ -> [] in
      let table rows = Taposaur_ui.table
        ["Location",false;"Host path",false;"Purpose",false]
        (List.map (fun (name,path,description) ->
          [Taposaur_ui.cell name [View.txt name];
           Taposaur_ui.cell path [View.el "code" [View.txt path]];
           Taposaur_ui.cell description [View.txt description]]) rows) in
      let capabilities = Taposaur_ui.table
        ["Publisher",false;"Formats / strategy",false;"CLI access requirements",false]
        (List.map (fun (c : Taposaur_download.capability) ->
          [Taposaur_ui.cell c.publisher [View.txt c.publisher];
           Taposaur_ui.cell c.formats [View.txt c.formats];
           Taposaur_ui.cell c.access [View.txt c.access]])
          Taposaur_download.publisher_capabilities) in
      html (content "Server storage"
        ([View.link (View.url "/audit"
             (if get params "uid" = "" then [] else ["subject",get params "uid"]))
             "Query audit log →";
          View.p "Paths reported by this running server. The web process opens both databases read-only; CLI jobs perform imports and updates.";
          (if rows = [] then View.p "Storage paths were not supplied by this server." else table rows);
          View.p "SQLite may keep active transactions in adjacent -wal and -shm files. Keep these with their database when backing up a running instance.";
          View.p "PDFs have no separate server setting: each paper manifest names its files beneath the archive root. Fetch staging directories are chosen per CLI invocation with --staging. Converter services and batch logs are not configured by this website."] @
         (if paper_rows = [] then [] else
           [View.el "h2" [View.txt "Current paper"];table paper_rows]) @
         [View.el "h2" [View.txt "Publisher fetch capabilities"];
          View.p "Implemented in the OCaml acquisition library and taposaur fetch CLI. This read-only web server does not fetch papers or load publisher credentials. Live access has not been checked by this panel.";
          capabilities;
          View.el "h2" [View.txt "Discovery, fallback and conversion"];
          Taposaur_ui.table ["Capability",false;"Behaviour",false]
            (List.map (fun (name,description) ->
              [Taposaur_ui.cell name [View.txt name];
               Taposaur_ui.cell description [View.txt description]])
              ["Glutton / DataCite",
                 "DOI/PMCID metadata via GLUTTON_SERVER_URL. Optional DATACITE_SERVER_URL fallback for a missing DOI record.";
               "Open access / CORE",
                 "Open-access PDF discovery via Glutton, then CORE search/download with CORE_API_TOKEN.";
               "Explicit URL",
                 "taposaur fetch --url with --format pdf or xml; content validation before staging.";
               "GROBID",
                 "PDF to TEI with header/reference consolidation. Explicitly configured HTTP client; not called by the website.";
               "Pub2TEI",
                 "Publisher XML to TEI. Explicitly configured HTTP client; not called by the website."]);
          View.p "Fetches validate content and record provenance in a per-paper staging tree. Publisher requests are paced and busy responses retried. Staging is separate from the accepted archive."]))
    else if page = Reviews then
      (match reviews with
       | None -> html (View.page "Reviews"
           [View.p "Reviews are not configured on this server."])
       | Some store -> html (Review_view.index (Reviews.list store)))
    else if page = Review then
      let store = review_store () and slug = get params "slug" in
      (match Reviews.find store slug with
       | None -> Html (Httpz.Res.Not_found, View.page "Review not found" [])
       | Some review -> html (Review_view.review review
           (Reviews.revisions store ~slug) (Reviews.runs store ~slug)
           (Reviews.snapshots store ~slug)))
    else if page = Review_definition then
      let revision = Reviews.revision (review_store ())
        ~slug:(get params "slug") ~number:(number params "revision") in
      Media ("application/json; charset=utf-8",
        Taposaur_review.Definition.to_string revision.definition)
    else if page = Review_snapshot then
      let store = review_store () and id = required_number "id" in
      html (Review_view.snapshot (Reviews.snapshot store id)
        (Reviews.statistics store id))
    else if page = Review_papers then
      let store = review_store () and id = required_number "id" in
      let snapshot = Reviews.snapshot store id in
      let publisher = List.assoc_opt "publisher" params in
      let filter = match List.assoc_opt "field" params with
        | None -> if List.mem_assoc "availability" params then
            invalid_arg "Availability requires a field" else None
        | Some label ->
            let field = match Taposaur_review.Statistics.of_label label with
              | Some field -> field | None -> invalid_arg "Unknown coverage field" in
            let present = match get params "availability" with
              | "present" -> true | "missing" -> false
              | _ -> invalid_arg "Availability must be present or missing" in
            Some (field,present) in
      let stats = match Reviews.statistics store id with
        | Some stats -> stats | None -> invalid_arg "Snapshot statistics are not cached" in
      let coverage = match publisher with None -> Some stats.overall
        | Some name -> List.assoc_opt name stats.publishers in
      let total = match coverage with None -> 0 | Some c ->
        match filter with None -> c.total | Some (field,present) ->
          let n = List.assoc (Taposaur_review.Statistics.label field) c.present in
          if present then n else c.total - n in
      html (Review_view.papers snapshot ~publisher ~filter ~total
        (Reviews.browse ~after:(get params "after") ~limit:51
          ?publisher ?filter store id))
    else if page = Review_export then
      let store = review_store () and id = required_number "id" in
      let snapshot = Reviews.snapshot store id in
      let format = get params "format" in
      let content_type,body = match format with
        | "manifest" -> "application/json; charset=utf-8",
            Reviews.blob store snapshot.manifest
        | _ ->
            let f = match format with
              | "doi" -> Taposaur_review.Dois | "csv" -> Csv | "jsonl" -> Jsonl
              | _ -> invalid_arg "Unknown review export format" in
            Taposaur_review.export_type f,Reviews.export store id f in
      Attachment (content_type,Printf.sprintf "%s-snapshot-%d.%s"
        snapshot.slug id (if format = "manifest" then "json" else format),body)
    else if page = Review_input then
      let store = review_store () in
      let run = Reviews.run store (required_number "run") in
      let key = get params "key" in
      let input = List.find_opt (fun (i : Reviews.input_result) -> i.key = key)
        run.inputs in
      (match input with
       | None -> Html (Httpz.Res.Not_found,View.page "Input not found" [])
       | Some i -> Attachment ("text/plain; charset=utf-8",
           Printf.sprintf "%s-run-%d-input.txt" run.slug run.id,
           Reviews.blob store i.blob))
    else if page = Review_compare then
      let store = review_store () in
      let before = required_number "before" and after = required_number "after" in
      html (Review_view.compare (Reviews.snapshot store before)
        (Reviews.snapshot store after) (Reviews.compare store ~before ~after))
    else if page = Script then Media ("text/javascript; charset=utf-8", Client.source)
    else if page = Katex then Media ("text/javascript; charset=utf-8", Katex.source)
    else if page = Journals then
      let journals = C.journals catalog ~text:(get params "q") in
      let body = match Jsont_bytesrw.encode_string (Jsont.list journal_codec)
        journals with Ok body -> body | Error e -> failwith e in
      Media ("application/json; charset=utf-8", body)
    else if page = Home then
      html (View.home ~details_cached:(C.Metadata.detail_count catalog)
        (C.stats catalog) (C.Shard.errors catalog))
    else if page = Search then
      html (View.search params (C.search catalog (search_query params)))
    else if page = Paper && get params "view" <> "live" then
      let cached = Option.bind (C.Metadata.detail catalog (uid params))
        (fun detail -> match Taposaur_paper.of_cache_string
          ~index:detail.index detail.json with
          | Ok paper -> Some (detail, supplement paper) | Error _ -> None) in
      (match cached with
       | Some (detail, paper) ->
           let at = Unix.gmtime detail.saved_at in
           let saved = Printf.sprintf "Details saved %04d-%02d-%02d %02d:%02d UTC."
             (at.tm_year + 1900) (at.tm_mon + 1) at.tm_mday at.tm_hour at.tm_min in
           let status = if detail.error <> None then
             " The latest refresh failed; saved details are shown."
             else if detail.stale then " These details may be out of date."
             else if detail.diagnostics <> [] then
               " Some source documents could not be parsed." else "" in
           html (View.paper ~catalogue_status:(saved ^ status) paper)
       | None ->
           let summary = if get params "view" = "full" then None
             else C.find catalog (uid params) in
           match summary with
           | Some summary -> html (View.paper_preview summary)
           | None -> prepare Paper (("view", "live") ::
               List.remove_assoc "view" params))
    else
      let id = uid params in
      match A.find archive id with
      | None -> Html (Httpz.Res.Not_found, View.page "Paper not found" [])
      | Some paper ->
          let m = A.manifest paper in
          if page = Paper then begin
            let catalogue_status = match C.Metadata.status catalog id with
              | None -> Some "This paper is not indexed for search."
              | Some s when s.stale ||
                  s.source_stamp <> Some (A.fingerprint paper) ->
                  Some "Search metadata is stale or has not been checked."
              | Some s when s.error <> None ->
                  Some "The last search metadata refresh failed."
              | Some s when s.diagnostics <> [] ->
                  Some "Search metadata contains document parsing failures."
              | Some _ -> None in
            html (View.paper ?catalogue_status (supplement (Taposaur_reader.load
              ?index:(C.find_index catalog id) paper)))
          end
          else if page = Events then begin
            let events = ref [] and bytes = ref 0 in
            let exception Enough in
            (try A.iter_events ~max_line:(1024 * 1024) paper (fun line e ->
              if line > 1000 then raise Enough;
              let s = match Event.to_string e with Ok s -> s | Error e -> e in
              bytes := !bytes + String.length s;
              if !bytes > 1024 * 1024 then raise Enough;
              events := View.el "pre" [View.txt s] :: !events)
             with Enough -> ());
            html (content "Event history" (View.p
              "Up to 1,000 events or 1 MiB in file order. \
               Sequence numbers restart per run."
              :: List.rev !events))
          end else
            let file = get params "file" in
            match List.find_opt (fun (a : Manifest.Artifact.t) -> a.file = file)
              m.artifacts with
            | None ->
                Html (Httpz.Res.Not_found, View.page "Artifact not found" [])
            | Some a ->
                if page = Download then Artifact (paper, a)
                else if page = Pdf && a.format = Manifest.Format.Pdf then
                  html (View.page file [Htmlit.El.v "iframe"
                    ~at:[Htmlit.At.v "src" (View.url "/artifact" params);
                      Htmlit.At.v "title" "Paper PDF"] []])
                else if page = Read && a.format = Manifest.Format.Tei then
                  (let cache = Hashtbl.create 64 and budget = ref 2048 in
                   let resolve_doi doi = match Hashtbl.find_opt cache doi with
                     | Some result -> result
                     | None when !budget = 0 ->
                         if Journal_links.known_doi journal_links doi then
                           Some {Bibliography.empty with doi=Some doi} else None
                     | None -> decr budget;
                         let result = match Uid.of_input doi with
                           | Error _ -> None
                           | Ok uid -> Option.map (fun (s : Summary.t) ->
                               s.bibliography) (C.find catalog uid) in
                         let result = match result with
                           | None when Journal_links.known_doi journal_links doi ->
                               Some {Bibliography.empty with doi=Some doi}
                           | _ -> result in
                         Hashtbl.add cache doi result; result in
                   match A.with_artifact paper file
                    (Taposaur_document.Tei.render_html ~resolve_doi
                      ~journal_url:(Journal_links.find journal_links)
                      ~source_url:(Journal_links.source journal_links)) with
                   | Ok body -> html (content "Read paper"
                       ((if in_panel then [] else
                         [View.link (View.url "/paper" ["uid",Uid.to_string id])
                           "← Paper and sources"]) @ [Htmlit.El.unsafe_raw body]))
                   | Error e -> Html (Httpz.Res.Unprocessable_entity,
                       View.page "Cannot render TEI" [View.p e]))
                else Html (Httpz.Res.Bad_request,
                  View.page "Unexpected artifact format" [])
  in
  let handle page req (respond @ local) =
    let params = Req.query req in
    let elapsed = elapsed () in
    let response = try prepare page params with
      | Reviews.Not_found -> Html (Httpz.Res.Not_found,
          View.page "Review record not found" [])
      | Invalid_argument e -> Html (Httpz.Res.Bad_request,
          View.page "Invalid request" [View.p e])
      | A.Parse_error e -> Html (Httpz.Res.Unprocessable_entity,
          View.page "Invalid archive record" [View.p (A.error_to_string e)])
      | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
          Html (Httpz.Res.Not_found, View.page "File not found" [])
    in
    (* The feed itself is not recorded, or polling would fill the log. *)
    if not (page = Activity_page && get params "format" = "json") then
      Activity.record activity ~time:(Unix.gettimeofday ())
        ~page:(page_name page) ~query:params
        ~status:(match response with
          | Html (status, _) -> Proffer.Status.code status
          | Media _ | Attachment _ | Artifact _ -> 200)
        ~elapsed_ms:(elapsed ());
    match response with
    (* Pages are public, so they may be kept, and TEI figures may come from
       anywhere. Artifact pages frame this site's own PDFs. *)
    | Html (status, body) -> Sitekit.respond_html respond ~status
        ~headers:(Sitekit.Headers.html ~cache:`Revalidate
          ~img_src:["https:"; "http:"] ~frame_src:["'self'"]
          ~frame_ancestors:["'self'"] ()) body
    | Media (content_type, body) -> Resp.media respond
        ~headers:[Resp.other "X-Content-Type-Options" "nosniff";
          Resp.other "Cache-Control" "no-store"] content_type body
    | Attachment (content_type,filename,body) -> Resp.media respond
        ~headers:[Resp.other "X-Content-Type-Options" "nosniff";
          Resp.other "Cache-Control" "private, max-age=31536000, immutable";
          Resp.other "Content-Disposition" ("attachment; filename=\"" ^
            filename ^ "\"")] content_type body
    | Artifact (paper, a) ->
        Artifact.send paper a req respond
  in { handle }

let site =
  let open Route in
  Site.of_routes ([
    get (s "audit") (fun env req respond -> env.handle Audit req respond);
    get root (fun env req respond -> env.handle Home req respond);
    get (s "debug") (fun env req respond -> env.handle Debug req respond);
    get (s "reviews") (fun env req respond -> env.handle Reviews req respond);
    get (s "jobs") (fun env req respond -> env.handle Jobs req respond);
    get (s "activity")
      (fun env req respond -> env.handle Activity_page req respond);
    get (s "job") (fun env req respond -> env.handle Job req respond);
    get (s "review") (fun env req respond -> env.handle Review req respond);
    get (s "review-definition")
      (fun env req respond -> env.handle Review_definition req respond);
    get (s "review-snapshot")
      (fun env req respond -> env.handle Review_snapshot req respond);
    get (s "review-papers")
      (fun env req respond -> env.handle Review_papers req respond);
    get (s "review-export")
      (fun env req respond -> env.handle Review_export req respond);
    get (s "review-input")
      (fun env req respond -> env.handle Review_input req respond);
    get (s "review-compare")
      (fun env req respond -> env.handle Review_compare req respond);
    get (s "journals") (fun env req respond -> env.handle Journals req respond);
    get (s "assets" / s "interface.js")
      (fun env req respond -> env.handle Script req respond);
    get (s "assets" / s "katex.js")
      (fun env req respond -> env.handle Katex req respond);
    get (s "search") (fun env req respond -> env.handle Search req respond);
    get (s "paper") (fun env req respond -> env.handle Paper req respond);
    get (s "events") (fun env req respond -> env.handle Events req respond);
    get (s "artifact") (fun env req respond -> env.handle Download req respond);
    get (s "pdf") (fun env req respond -> env.handle Pdf req respond);
    get (s "read") (fun env req respond -> env.handle Read req respond)]
    @ Sitekit.Asset.routes View.assets)
