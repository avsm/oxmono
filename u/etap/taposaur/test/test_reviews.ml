open Taposaur
module R = Taposaur_review
module D = R.Definition
module S = Taposaur_review_store
module X = Taposaur_review_run
module C = Taposaur_catalog
module W = Proffer_mock
let ( / ) = Eio.Path.( / )
let get = Tap_test.get
let equal = Alcotest.(check string)
let count = Alcotest.(check int)
let check = Alcotest.(check bool)
let rejects f = try f (); false with Invalid_argument _ -> true
let includes hay needle =
  let n = String.length needle in
  let rec loop i = i+n <= String.length hay &&
    (String.sub hay i n = needle || loop (i+1)) in
  loop 0
let with_temp f = Tap_test.with_temp "taposaur-reviews-" f
let make_index doi =
  get (Index.of_string (Printf.sprintf
    {|{"uid":"doi:%s","scheme":"doi","pdf_path":"","pdf_size":0,"pdf_mtime":0,"tei_path":"","tei_mtime":0}|}
    doi))
let load c rows =
  let shards = Hashtbl.create 10 in
  List.iter (fun (row : Index.t) ->
    let shard = Layout.index row.uid in
    Hashtbl.replace shards shard
      (row :: Option.value ~default:[] (Hashtbl.find_opt shards shard))) rows;
  Hashtbl.iter (fun shard rows ->
    ignore (C.Shard.replace c ~shard {size=1L;mtime=1.}
      (fun emit -> List.iter emit rows))) shards
let source kind = {D.kind;format=D.Doi_lines;blob=None;local=None;
  provider=None;query=None;parameters=None}
let definition inputs = {D.version=1;inputs;
  include_any=List.map (fun (i : D.input) -> i.key) inputs;
  require_all=[];exclude_any=[]}
let list_input store key raw =
  {D.key;label=key;source={ (source D.List) with blob=Some (S.put_blob store raw)}}
let define store slug definition expected =
  S.define store ~slug ~title:"Review <script>" ~requester:"Researcher"
    ~owner:"Librarian" ~actor:"agent" ~reason:"Initial import"
    ~expected definition
let record doi = {R.doi;bibliography=Bibliography.empty;line=1}
let definitions () =
  let input key = {D.key;label=key;
    source={ (source D.Recorded_search) with
      provider=Some "provider";query=Some "Native query OR syntax"}} in
  let d = { (definition (List.map input ["a";"b";"required";"excluded"])) with
    include_any=["a";"b"];require_all=["required"];exclude_any=["excluded"]} in
  let results = ["a",List.map record ["10.1000/a";"10.1000/a";"10.1000/b"];
    "b",List.map record ["10.1000/b";"10.1000/c"];
    "required",List.map record ["10.1000/a";"10.1000/b"];
    "excluded",[record "10.1000/a"]] in
  let rows = R.select d results in
  count "set expression" 1 (List.length rows);
  equal "selected DOI" "10.1000/b" (List.hd rows).doi;
  count "all input occurrences retained" 3 (List.length (List.hd rows).supplied);
  check "codec" true (get (D.of_string (D.to_string d)) = d);
  check "unknown reference" true
    (Result.is_error (D.validate {d with include_any=["missing"]}));
  check "future version" true (Result.is_error (D.validate {d with version=2}));
  check "unknown key rejected" true (Result.is_error (D.of_string
    {|{"version":1,"inputs":[],"include_any":[],"typo":true}|}));
  let p = R.parse D.Doi_lines
    "# Input\nhttps://doi.org/10.1000/A\n10.1000/a\nnot-a-doi\n" in
  count "duplicates preserved" 2 (List.length p.records);
  count "invalid retained as diagnostic" 1 (List.length p.diagnostics);
  count "line number" 4 (List.hd p.diagnostics).line
let snapshots () = with_temp (fun _ sw root path ->
  let c = C.open_write ~sw (root / "catalog.db") in
  let read = C.open_read ~sw (root / "catalog.db") in
  let index = { (make_index "10.1000/a") with
    title=Some "Catalogue title"} in
  load c [index];
  let audit_db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let audit = Taposaur_audit.session ~component:"test.review" audit_db in
  let store = S.open_write ~audit ~sw (root / "reviews.db") in
  let raw = "{\"doi\":\"10.1000/a\",\"title\":\"Imported title\",\"abstract\":\"A, \\\"quoted\\\"\\nabstract\"}\n" ^
    "{\"doi\":\"10.1000/missing\",\"title\":\"Outside corpus\"}\n" in
  let input = list_input store "main" raw in
  let input = {input with source={input.source with format=D.Metadata_jsonl}} in
  let d = definition [input] in
  count "first revision" 1 (define store "education" d 0);
  check "stale edit rejected" true
    (rejects (fun () -> ignore (define store "education" d 0)));
  count "no extra revision" 1 (List.length (S.revisions store ~slug:"education"));
  let rev = S.revision store ~slug:"education" ~number:None in
  let run = X.execute store read rev ~actor:"agent" [] in
  count "missing paper remains" 2 (S.run store run).members;
  let first = X.snapshot store read ~run_id:run ~parent:None
    ~label:"First delivery" ~actor:"agent" in
  let events = Taposaur_audit.query ~action:"review.snapshot.publish"
    ~phase:Succeeded audit_db in
  count "snapshot audit event" 1 (List.length events);
  check "snapshot ID in audit" true (includes (List.hd events).details
    (Printf.sprintf "\"snapshot_id\":\"%d\"" first));
  count "failed conflicting revision audited" 1
    (List.length (Taposaur_audit.query ~action:"review.definition.publish"
      ~phase:Failed audit_db));
  let original = S.export store first R.Csv in
  let selected = S.browse ~filter:(R.Statistics.Abstract,true) store first in
  count "abstract drill-down" 1 (List.length selected);
  equal "abstract member" "10.1000/a" (List.hd selected).doi;
  count "missing abstract drill-down" 1
    (List.length (S.browse ~filter:(R.Statistics.Abstract,false) store first));
  count "unknown publisher drill-down" 2
    (List.length (S.browse ~publisher:"" store first));
  count "publisher restriction" 0
    (List.length (S.browse ~publisher:"Not a publisher" store first));
  count "combined predicate" 1
    (List.length (S.browse ~publisher:"" ~filter:(R.Statistics.Abstract,true)
      store first));
  count "exclusive browse cursor" 1
    (List.length (S.browse ~after:"10.1000/a" store first));
  let stats = Option.get (S.statistics store first) in
  count "cached total" 2 stats.overall.total;
  count "cached abstract coverage" 1
    (List.assoc "Abstract" stats.overall.present);
  count "unknown publisher coverage" 2
    (List.assoc "" stats.publishers).total;
  S.cache_statistics store first;
  check "cache is reproducible" true
    (S.statistics store first = Some stats);
  let manifest = (S.snapshot store first).manifest in
  let writer = Sqlite3.db_open (path ^ "/reviews.db") in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close writer)) (fun () ->
    Sqlite3.Rc.check (Sqlite3.exec writer
      "DELETE FROM exports WHERE format='statistics-v1'"));
  check "older snapshot has no statistics" true
    (S.statistics store first = None);
  S.cache_statistics store first;
  check "backfill matches original statistics" true
    (S.statistics store first = Some stats);
  equal "backfill preserves manifest" manifest (S.snapshot store first).manifest;
  equal "backfill preserves export" original (S.export store first R.Csv);
  check "proper multiline CSV" true (includes original "\"A, \"\"quoted\"\"\nabstract\"");
  let first_row = List.hd (S.rows store first) in
  let formats = R.Statistics.collect (fun consume ->
    consume {first_row with holdings={Summary.pdf=true;xml=false;
      tei=true;s2orc=false;text=None}};
    consume {first_row with holdings={Summary.pdf=false;xml=false;
      tei=false;s2orc=false;text=None}};
    (* A converted document whose body is known to be empty is not full
       text, even though the TEI and XML are held. *)
    consume {first_row with holdings={Summary.pdf=false;xml=false;
      tei=true;s2orc=false;text=Some false}}) in
  count "overlapping formats count one full text" 1
    (List.assoc "Full text" formats.overall.present);
  count "PDF coverage" 1 (List.assoc "PDF" formats.overall.present);
  count "TEI coverage" 2 (List.assoc "TEI" formats.overall.present);
  count "missing XML" 0
    (List.assoc "Publisher XML" formats.overall.present);
  equal "catalogue title wins" "Catalogue title"
    (Option.get first_row.bibliography.title);
  equal "input abstract fallback" "A, \"quoted\"\nabstract"
    (Option.get first_row.bibliography.abstract);
  check "exact input provenance" true (List.exists (fun (e : R.evidence) ->
    e.field = "abstract" && e.source = "input:main:line:1") first_row.evidence);
  let candidate = List.hd (C.Metadata.pending ~uid:index.uid ~force:true
    ~limit:1 ~version:1 c) in
  ignore (C.Metadata.publish c candidate ~version:1
    (Ok {Bibliography.empty with title=Some "Updated title";
      abstract=Some "New abstract"}));
  let second = X.snapshot store read ~run_id:run ~parent:(Some first)
    ~label:"Refreshed" ~actor:"agent" in
  equal "old export immutable" original (S.export store first R.Csv);
  equal "filtered metadata remains frozen" "Catalogue title"
    (Option.get (List.hd (S.browse ~filter:(R.Statistics.Abstract,true)
      store first)).bibliography.title);
  check "new export" true (includes (S.export store second R.Csv) "New abstract");
  let diff = S.compare store ~before:first ~after:second in
  count "fixed membership added" 0 diff.added;
  count "fixed membership removed" 0 diff.removed;
  count "changed metadata" 1 diff.changed;
  check "bad membership refused" true (rejects (fun () ->
    ignore (S.publish_snapshot store ~run_id:run ~parent:None ~label:"Bad"
      ~actor:"agent" [])));
  count "no partial publication" 2
    (List.length (S.snapshots store ~slug:"education"));
  count "next revision" 2 (define store "education" d 1);
  count "old run still r1" 1 (S.run store run).revision;
  let next_input = list_input store "main"
    "10.1000/new\n10.1000/missing\n" in
  ignore (define store "education" (definition [next_input]) 2);
  let next_run = X.execute store read
    (S.revision store ~slug:"education" ~number:None) ~actor:"agent" [] in
  check "parent from another run refused" true (rejects (fun () ->
    ignore (X.snapshot store read ~run_id:next_run ~parent:(Some first)
      ~label:"Invalid parent" ~actor:"agent")));
  let next_snapshot = X.snapshot store read ~run_id:next_run ~parent:None
    ~label:"Changed search results" ~actor:"agent" in
  let membership_diff = S.compare store ~before:first ~after:next_snapshot in
  count "new membership addition" 1 membership_diff.added;
  count "new membership removal" 1 membership_diff.removed;
  let reopened = S.open_read ~sw (root / "reviews.db") in
  equal "reopen export byte for byte" original (S.export reopened first R.Csv);
  let manifest = S.blob reopened (S.snapshot reopened first).manifest in
  check "manifest valid JSON" true (Result.is_ok (R.decode Jsont.json manifest));
  check "manifest export checksum" true (includes manifest (R.digest original)))
let partial_inputs () = with_temp (fun _ sw root _ ->
  ignore (C.open_write ~sw (root / "catalog.db"));
  let catalog = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  let input = {D.key="external";label="Recorded query";
    source={ (source D.Recorded_search) with provider=Some "External";
      query=Some "exact query"}} in
  ignore (define store "soil" (definition [input]) 0);
  let rev = S.revision store ~slug:"soil" ~number:None in
  let raw = "10.1000/a\nbroken\n" in
  check "invalid complete input refused" true (rejects (fun () ->
    ignore (X.execute store catalog rev ~actor:"agent"
      [{key="external";bytes=raw;partial=false}])));
  count "failed run not published" 0 (List.length (S.runs store ~slug:"soil"));
  let run = X.execute store catalog rev ~actor:"agent"
    [{key="external";bytes=raw;partial=true}] in
  check "partial retained" true (S.run store run).partial;
  let snapshot = X.snapshot store catalog ~run_id:run ~parent:None
    ~label:"Partial" ~actor:"agent" in
  check "snapshot partial" true (S.snapshot store snapshot).partial;
  equal "original bytes retained" raw (S.blob store
    (List.hd (S.run store run).inputs).blob);
  let bound = [{X.key="external";bytes="10.1000/a\n10.1000/b\n";partial=false}] in
  check "budget abort" true (rejects (fun () ->
    ignore (X.execute ~max_members:1 store catalog rev ~actor:"agent" bound)));
  count "budget no publication" 1 (List.length (S.runs store ~slug:"soil")))
let local_search () = with_temp (fun _ sw root path ->
  let c = C.open_write ~sw (root / "catalog.db") in
  let rows = List.init 125 (fun n -> { (make_index
    (Printf.sprintf "10.1000/paper-%03d" n)) with title=Some "Soil carbon"}) in
  load c rows;
  let read = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  let input = {D.key="local";label="Local carbon search";
    source={ (source D.Catalogue) with
      local=Some {D.empty_local with text="Soil carbon"}}} in
  ignore (define store "carbon" (definition [input]) 0);
  let rev = S.revision store ~slug:"carbon" ~number:None in
  let run = X.execute store read rev ~actor:"agent" [] in
  count "all pages captured" 125 (S.run store run).members;
  check "local budget abort" true (rejects (fun () ->
    ignore (X.execute ~max_members:100 store read rev ~actor:"agent" [])));
  let row = List.hd rows in
  let escaped = ref None in
  C.Snapshot.with_read read (fun snapshot ->
    escaped := Some snapshot;
    let before = Option.get (C.Snapshot.find snapshot row.uid) in
    let writer = Sqlite3.db_open (path ^ "/catalog.db") in
    Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close writer)) (fun () ->
      Sqlite3.Rc.check (Sqlite3.exec writer
        "UPDATE papers SET bibliography_json='{\"title\":\"Concurrent update\"}'"));
    let after = Option.get (C.Snapshot.find snapshot row.uid) in
    check "one read transaction" true (before.summary = after.summary));
  equal "next read sees update" "Concurrent update"
    (Option.get (Option.get (C.find read row.uid)).bibliography.title);
  check "expired token rejected" true (rejects (fun () ->
    ignore (C.Snapshot.find (Option.get !escaped) row.uid)));
  (try C.Snapshot.with_read read (fun _ -> raise Exit) with Exit -> ());
  check "exception rolls back snapshot" true (Option.is_some (C.find read row.uid));
  let snap = X.snapshot store read ~run_id:run ~parent:None
    ~label:"Captured" ~actor:"agent" in
  count "snapshot all pages" 125 (S.snapshot store snap).members;
  let page1 = S.rows store snap in
  let page2 = S.rows ~after:(List.hd (List.rev page1)).doi store snap in
  count "page1" 50 (List.length page1);
  count "page2" 50 (List.length page2))
let web () = with_temp (fun _ sw root _ ->
  ignore (C.open_write ~sw (root / "catalog.db"));
  let catalog = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  ignore (define store "education"
    (definition [list_input store "main" "10.1000/missing\n"]) 0);
  let run = X.execute store catalog
    (S.revision store ~slug:"education" ~number:None) ~actor:"agent" [] in
  let snap = X.snapshot store catalog ~run_id:run ~parent:None
    ~label:"First <script>" ~actor:"agent" in
  let publishers = [{Publisher_evidence.doi="10.1000/missing";
    publisher="Wiley";provider="test";endpoint="test";
    sha256=String.make 64 'a';retrieved=0.}] in
  let second = X.snapshot ~publishers store catalog ~run_id:run ~parent:(Some snap)
    ~label:"Second" ~actor:"agent" in
  let archive = Taposaur_archive.open_root ~sw root in
  let reviews = S.open_read ~sw (root / "reviews.db") in
  let state = Taposaur_web.create ~reviews ~archive ~catalog () in
  let request ?(meth=Httpz.Method.Get) target = W.request
    ~on_error:(fun ex -> raise ex) Taposaur_web.site state meth target in
  let code r = Proffer.Status.code (W.status r) in
  let index = request "/reviews" in
  count "reviews index" 200 (code index);
  check "escape title" true (includes (W.body index) "&lt;script&gt;");
  let review = request "/review?slug=education" in
  count "review" 200 (code review);
  check "comparison form" true (includes (W.body review) "Compare snapshots");
  count "comparison" 200 (code (request (Printf.sprintf
    "/review-compare?before=%d&after=%d" snap second)));
  count "missing review" 404 (code (request "/review?slug=missing"));
  count "missing snapshot" 404
    (code (request "/review-snapshot?id=99999"));
  let page = request (Printf.sprintf "/review-snapshot?id=%d" snap) in
  count "snapshot" 200 (code page);
  check "individual DOI omitted" false (includes (W.body page) "10.1000/missing");
  check "coverage visible" true (includes (W.body page) "Coverage and missing data");
  check "publisher gaps visible" true (includes (W.body page) "Missing abstract");
  check "sortable headers" true (includes (W.body page) "data-sort-column");
  check "coverage spark bars" true (includes (W.body page) "coverage-spark");
  check "browse link" true (includes (W.body page) "/review-papers?id=");
  let missing = request (Printf.sprintf
    "/review-papers?id=%d&field=Title&availability=missing" snap) in
  count "filtered page" 200 (code missing);
  check "filtered DOI visible" true (includes (W.body missing) "10.1000/missing");
  check "frozen count shown" true (includes (W.body missing) "1 matching papers");
  check "unknown strategy explicit" true
    (includes (W.body missing) "No publisher strategy");
  let supported = request (Printf.sprintf "/review-papers?id=%d" second) in
  List.iter (fun text -> check text true (includes (W.body supported) text))
    ["Download strategy";"Strategy available";"Wiley";
     "API token required";"Full text missing"];
  count "invalid field" 400 (code (request (Printf.sprintf
    "/review-papers?id=%d&field=Bad&availability=present" snap)));
  count "invalid availability" 400 (code (request (Printf.sprintf
    "/review-papers?id=%d&field=Title&availability=maybe" snap)));
  equal "export exact cached bytes" (S.export store snap R.Csv)
    (W.body (request (Printf.sprintf "/review-export?id=%d&format=csv" snap)));
  count "no writes" 405 (code (request ~meth:Httpz.Method.Post "/reviews"));
  equal "HEAD" "" (W.body (request ~meth:Httpz.Method.Head
    (Printf.sprintf "/review-export?id=%d&format=csv" snap))))
let browse_migration () = with_temp (fun _ sw root path ->
  ignore (C.open_write ~sw (root / "catalog.db"));
  let catalog = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  ignore (define store "legacy"
    (definition [list_input store "main" "10.1000/legacy\n"]) 0);
  let run = X.execute store catalog
    (S.revision store ~slug:"legacy" ~number:None) ~actor:"agent" [] in
  let snapshot = X.snapshot store catalog ~run_id:run ~parent:None
    ~label:"Legacy" ~actor:"agent" in
  let manifest = (S.snapshot store snapshot).manifest in
  let raw = Sqlite3.db_open (path ^ "/reviews.db") in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close raw)) (fun () ->
    Sqlite3.Rc.check (Sqlite3.exec raw
      "DROP TABLE snapshot_browse_ready; DROP TABLE snapshot_facets; \
       PRAGMA user_version=1"));
  let old = S.open_read ~sw (root / "reviews.db") in
  count "v1 remains readable" 1 (List.length (S.rows old snapshot));
  check "v1 requires backfill" true
    (rejects (fun () -> ignore (S.browse old snapshot)));
  let upgraded = S.open_write ~sw (root / "reviews.db") in
  check "migration does not pretend indexes are populated" true
    (rejects (fun () -> ignore (S.browse upgraded snapshot)));
  S.cache_statistics upgraded snapshot;
  count "migrated filtered membership" 1 (List.length
    (S.browse ~filter:(R.Statistics.Title,false) upgraded snapshot));
  equal "migration preserves manifest" manifest (S.snapshot upgraded snapshot).manifest)
(* A corpus export joins frozen rows with archive prose: members the
   archive does not hold still appear, missing values encode as null, and
   the text comes from the preferred TEI at export time. *)
let corpus () = with_temp (fun env sw root _ ->
  let c = C.open_write ~sw (root / "catalog.db") in
  load c [{ (make_index "10.1000/a") with title=Some "Held paper";
    abstract=Some "Frozen abstract"}];
  let catalog = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  ignore (define store "corpus"
    (definition [list_input store "main" "10.1000/b\n10.1000/a\n"]) 0);
  let run = X.execute store catalog
    (S.revision store ~slug:"corpus" ~number:None) ~actor:"agent" [] in
  let snap = X.snapshot store catalog ~run_id:run ~parent:None
    ~label:"Corpus" ~actor:"agent" in
  let uid = get (Uid.of_string "doi:10.1000/a") in
  let dir = root / "papers" / Layout.paper uid in
  Eio.Path.mkdirs ~perm:0o700 dir;
  let tei converter body = Printf.sprintf
    {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title>Held paper</title></titleStmt></fileDesc></teiHeader><text><body>%s</body></text></TEI>|}
    body |> fun s -> ignore converter; s in
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "grobid.tei.xml")
    (tei "grobid" "<div><head>Intro</head><p>GROBID prose.</p></div>");
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "pub2tei.tei.xml")
    (tei "pub2tei" "<figure><figDesc>Only a caption</figDesc></figure>");
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "paper.json")
    {|{"v":1,"id":"doi:10.1000/a","artifacts":[{"file":"grobid.tei.xml","format":"tei","source":"tei","converter":"grobid","bytes":1,"at":"2026-09-14T05:54:04.598Z"},{"file":"pub2tei.tei.xml","format":"tei","source":"tei","converter":"pub2tei","bytes":1,"at":"2026-09-14T05:54:04.598Z"}],"updated":"2026-09-14T05:54:04.598Z"}|};
  let archive = Taposaur_archive.open_root ~sw root in
  let rows = ref [] in
  let summary = Taposaur_corpus.export ~fibers:2 ~archive ~store ~snapshot:snap
    (fun row -> rows := row :: !rows) in
  let rows = List.rev !rows in
  count "two rows" 2 (List.length rows);
  count "summary rows" 2 summary.rows;
  count "summary held" 1 summary.held;
  count "summary texts" 1 summary.texts;
  count "summary errors" 0 summary.errors;
  let a = List.nth rows 0 and b = List.nth rows 1 in
  equal "doi order" "10.1000/a" a.doi;
  equal "uid" "doi:10.1000/a" a.uid;
  equal "frozen title" "Held paper" (Option.get a.title);
  equal "frozen abstract" "Frozen abstract" (Option.get a.abstract);
  equal "title source" "catalogue" (Option.get a.title_source);
  equal "falls past empty preferred TEI" "Intro\n\nGROBID prose."
    (Option.get a.text);
  equal "text file" "grobid.tei.xml" (Option.get a.tei_file);
  equal "text converter" "grobid" (Option.get a.tei_converter);
  count "digest length" 64 (String.length (Option.get a.tei_sha256));
  count "summary characters" (String.length (Option.get a.text))
    summary.characters;
  check "unheld member kept" true (b.text = None && b.text_error = None
    && b.tei_file = None && b.title = None && not b.in_catalogue);
  let line = Taposaur_corpus.encode_row b in
  check "null for missing values" true (includes line "\"title\":null");
  check "snapshot column" true (includes line
    (Printf.sprintf "\"snapshot\":%d" snap));
  let back = get (R.decode Taposaur_corpus.row_jsont line) in
  check "round trip" true (back = b);
  load c [{ (make_index "10.1000/a") with title=Some "Updated title";
    abstract=Some ""; journal=Some "Live journal"}];
  let evidence = {Abstract_evidence.doi="10.1000/b";abstract="Evidence abstract";
    provider="openalex";endpoint="works";sha256=String.make 64 'b';retrieved=0.} in
  let abstracts d = if Doi.to_string d = "10.1000/b" then Some evidence else None in
  let live = ref [] in
  ignore (Taposaur_corpus.export ~catalogue:catalog ~abstracts ~archive ~store
    ~snapshot:snap (fun row -> live := row :: !live));
  let la = List.nth (List.rev !live) 0 and lb = List.nth (List.rev !live) 1 in
  equal "live title supersedes" "Updated title" (Option.get la.title);
  equal "live journal added" "Live journal" (Option.get la.journal);
  equal "blank live field keeps frozen" "Frozen abstract" (Option.get la.abstract);
  equal "live source" "catalogue" (Option.get la.title_source);
  equal "evidence abstract" "Evidence abstract" (Option.get lb.abstract);
  equal "evidence source" (Abstract_evidence.label evidence)
    (Option.get lb.abstract_source);
  check "unheld stays unheld" true (not lb.in_catalogue && lb.text = None);
  check "frozen rows unchanged" true
    (List.length (S.rows store snap) = 2 &&
     (List.hd (S.rows store snap)).bibliography.title = Some "Held paper");
  let pool = Eio.Executor_pool.create ~sw ~domain_count:2
    (Eio.Stdenv.domain_mgr env) in
  let pooled = ref [] in
  let again = Taposaur_corpus.export ~pool ~fibers:4 ~archive ~store
    ~snapshot:snap (fun row -> pooled := row :: !pooled) in
  check "pooled export agrees" true (List.rev !pooled = rows && again = summary);
  check "fiber bound" true (rejects (fun () -> ignore
    (Taposaur_corpus.export ~fibers:0 ~archive ~store ~snapshot:snap ignore))))

let () = Alcotest.run "reviews" ["review",[
  Alcotest.test_case "query shape and sets" `Quick definitions;
  Alcotest.test_case "snapshots and immutable exports" `Quick snapshots;
  Alcotest.test_case "partial inputs and budgets" `Quick partial_inputs;
  Alcotest.test_case "local queries and consistent reads" `Quick local_search;
  Alcotest.test_case "read-only website" `Quick web;
  Alcotest.test_case "corpus export with body text" `Quick corpus;
  Alcotest.test_case "browse index migration" `Quick browse_migration]]
