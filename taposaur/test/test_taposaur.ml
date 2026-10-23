open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module I = Taposaur_indexing.Import
module E = Taposaur_indexing.Enrich
module P = Taposaur_paper
module W = Proffer_mock
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> Alcotest.fail e
let uid = fun s -> get (Uid.of_string s)
let equal = Alcotest.(check string)
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let rejected = function Error _ -> true | Ok _ -> false
let includes text sub =
  let n = String.length sub in
  let rec find i = i + n <= String.length text &&
    (String.sub text i n = sub || find (i + 1)) in
  find 0

let identifiers () =
  let d = get (Doi.of_string " HTTPS://doi.org/10.1038/NATURE12373 ") in
  equal "case" "10.1038/nature12373" (Doi.to_string d);
  equal "encoded presentation" "10.1000/a/b"
    (Doi.to_string (get (Doi.of_string "doi:10.1000/a%2Fb")));
  equal "bare PMCID" "pmc:PMC123"
    (Uid.to_string (get (Uid.of_input " PMC123 ")));
  equal "bare DOI input" "doi:10.1000/example"
    (Uid.to_string (get (Uid.of_input "10.1000/Example")));
  check "bare numbers ambiguous" true (rejected (Uid.of_input "123"));
  let u = uid "doi:10.1016/j.ijer.2018.10.003" in
  equal "index shard" "doi/10.1016/06.jsonl" (Layout.index u);
  equal "doi path" "doi/10.1016/06/j.ijer.2018.10.003" (Layout.paper u);
  let p = uid "pmc:PMC4460142" in
  equal "pmc paper" "pmc/PMC4460/PMC4460142" (Layout.paper p);
  equal "pmc index" "pmc/446/3e.jsonl" (Layout.index p);
  equal "short pmc" "pmc/PMC12/PMC12" (Layout.paper (uid "pmc:PMC12"));
  check "nested suffix" true
    (includes (Layout.paper (uid "doi:10.1044/1092-4388(2007/068)"))
      "/1092-4388(2007/068)");
  check "punctuation" true
    (includes (Layout.paper
      (uid "doi:10.1643/0045-8511(2001)[0835:dcihub]2.0.co;2"))
      "0045-8511(2001)%5B0835:dcihub%5D2.0.co;2");
  List.iter (fun s -> check s true (rejected (Doi.of_string s)))
    ["10.2/";"x/y";"10..2/a";"10.2/a\000";"https://doi.org/10.2/%xx"];
  check "unsafe DOI path" true
    (try ignore (Layout.paper (uid "doi:10.2/../escape")); false
     with Invalid_argument _ -> true)

let manifest_json id = Printf.sprintf
  {|{"v":1,"id":%S,"artifacts":[],"updated":"2026-09-14T05:54:04.598Z"}|}
  (Uid.to_string id)
let index_json id = Printf.sprintf
  {|{"uid":%S,"scheme":"doi","pdf_path":"","pdf_size":0,"pdf_mtime":0,"tei_path":"","tei_mtime":0}|}
  (Uid.to_string id)
let event =
  {|{"v":1,"seq":1,"at":"2026-09-14T05:54:04.598Z","cmd":"migrate","outcome":{"future_kind":{"value":42}}}|}

let records () =
  let id = uid "doi:10.1000/a" in
  let m = get (Manifest.of_string (manifest_json id)) in
  count "missing corrections" 0 (List.length m.corrections);
  let e = get (Index.of_string (index_json id)) in
  check "absent flags" false e.has_xml;
  count "absent authors" 0 (List.length e.authors);
  let parsed = get (Event.of_string event) in
  equal "outcome envelope" "outcome" (Event.kind parsed);
  check "unknown outcome retained" true
    (includes (get (Event.to_string parsed)) "future_kind");
  let duplicate_payload = String.sub event 0 (String.length event - 1)
    ^ ",\"summary\":{}}" in
  check "multiple payloads rejected" true
    (rejected (Event.of_string duplicate_payload));
  check "version rejected" true
    (rejected (Manifest.of_string
      {|{"v":2,"id":"pmc:PMC12","artifacts":[],"updated":"x"}|}));
  check "missing artifacts rejected" true
    (rejected (Manifest.of_string
      {|{"v":1,"id":"pmc:PMC12","updated":"x"}|}));
  check "missing required index field" true
    (rejected (Index.of_string {|{"uid":"pmc:PMC12","scheme":"pmc"}|}));
  check "wrong scheme" true (rejected (Index.of_string
    {|{"uid":"pmc:PMC12","scheme":"doi","pdf_path":"","pdf_size":0,"pdf_mtime":0,"tei_path":"","tei_mtime":0}|}));
  let failure = get (Jsont_bytesrw.decode_string Manifest.Failure.jsont
    {|{"route":"grobid","at":"x","attempts":430,"reason":"refused"}|}) in
  check "legacy permanent" false failure.permanent;
  check "unsafe filename" true
    (rejected (Jsont_bytesrw.decode_string Manifest.Artifact.jsont
      {|{"file":"../secret","format":"pdf","source":"publisher","bytes":1,"at":"x"}|}))

let with_temp f =
  Eio_main.run @@ fun env ->
  let name = Filename.temp_file "taposaur-test-" "" in
  Unix.unlink name;
  let root = env#fs / name in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () -> Eio.Switch.run (fun sw -> f env sw root))
let save root path text =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (root / Filename.dirname path);
  Eio.Path.save ~create:(`Or_truncate 0o600) (root / path) text

let streaming () = with_temp (fun _ _ root ->
  save root "events" (event ^ "\r\n" ^ event);
  let lines = ref [] in
  A.iter_jsonl ~decode:Event.of_string (root / "events")
    (fun n _ -> lines := n :: !lines);
  Alcotest.(check (list int)) "line order" [2;1] !lines;
  save root "bad" (event ^ "\n{bad}\n");
  (try A.iter_jsonl ~decode:Event.of_string (root / "bad") (fun _ _ -> ());
    Alcotest.fail "expected line error"
   with A.Parse_error e -> Alcotest.(check (option int)) "line" (Some 2) e.line);
  let exception Callback in
  check "callback exceptions propagate" true
    (try A.iter_jsonl ~decode:Event.of_string (root / "events")
      (fun _ _ -> raise Callback); false with Callback -> true);
  check "buffer bounded" true
    (try A.iter_jsonl ~max_line:16 ~decode:Event.of_string (root / "events")
      (fun _ _ -> ()); false with A.Parse_error _ -> true);
  save root "work.doi" "# A journal\n\n10.1000/A\n# comment\n10.1000/b";
  let ids = ref [] in
  A.iter_dois (root / "work.doi") (fun _ u -> ids := Uid.to_string u :: !ids);
  count "work list" 2 (List.length !ids))

let pdf = "%PDF-1.4\n0123456789\n%%EOF\n"
let tei = {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title>Bird <hi>migration</hi></title></titleStmt><publicationStmt><publisher>Example</publisher></publicationStmt><sourceDesc><biblStruct><analytic><author><persName><forename>Ada</forename><surname>Bird</surname></persName></author></analytic></biblStruct></sourceDesc></fileDesc></teiHeader><text><body><div><head>Results</head><p>Birds &amp; habitats.<hi>Italic</hi><ref target="javascript:evil()">Link</ref><script>alert(1)</script></p></div></body></text></TEI>|}

let setup root =
  let id = uid "doi:10.1000/nested/paper" in
  let dir = "papers/" ^ Layout.paper id in
  let m = get (Manifest.of_string (manifest_json id)) in
  let artifact file format bytes =
    get (Jsont_bytesrw.decode_string Manifest.Artifact.jsont
      (Printf.sprintf
        {|{"file":%S,"format":%S,"source":"publisher","bytes":%d,"at":"2026-09-14T05:54:04Z"}|}
        file format bytes)) in
  let m = { m with artifacts = [artifact "publisher.pdf" "pdf" (String.length pdf);
    artifact "tei.xml" "tei" (String.length tei)] } in
  save root (dir ^ "/paper.json") (get (Manifest.to_string m));
  save root (dir ^ "/events.jsonl") event;
  save root (dir ^ "/publisher.pdf") pdf;
  save root (dir ^ "/tei.xml") tei;
  let e = get (Index.of_string (index_json id)) in
  let e = { e with title = Some "Bird migration <script>";
    authors = [{Author.given = Some "Ada"; family = "Bird"}];
    abstract = Some "Seasonal movement and habitat";
    pdf_path = "/old/root/publisher.pdf"; pdf_size = Int64.of_int (String.length pdf);
    tei_path = "/old/root/tei.xml"; tei_source = Some "grobid";
    publisher = Some "Example"; year = Some 2020 } in
  let shard = Layout.index id in
  save root ("index/" ^ shard) (get (Index.to_string e) ^ "\n");
  id, shard, e

let archive_reads () = with_temp (fun _ sw root ->
  let id, _, _ = setup root in
  let a = A.open_root ~sw root in
  let p = Option.get (A.find a id) in
  equal "artifact" pdf (A.read_artifact p "publisher.pdf");
  check "unlisted file refused" true
    (try ignore (A.read_artifact p "events.jsonl"); false
      with Invalid_argument _ -> true);
  let found = ref 0 in
  A.iter_papers a (fun _ -> incr found);
  count "nested marker" 1 !found;
  save root ("staging/legacy/" ^ Layout.flat_paper id ^ "/paper.json")
    (manifest_json id);
  check "legacy staging" true (Option.is_some (A.find_staged a ~batch:"legacy" id));
  check "bad batch" true
    (try ignore (A.find_staged a ~batch:"../escape" id); false
      with Invalid_argument _ -> true);
  let file = root / "papers" / Layout.paper id / "publisher.pdf" in
  Eio.Path.unlink file;
  Eio.Path.symlink ~link_to:"/etc/passwd" file;
  check "artifact symlink refused" true
    (try ignore (A.read_artifact p "publisher.pdf"); false with Eio.Io _ -> true))

let documents () =
  let h = get (Taposaur_document.Tei.metadata_string tei) in
  Alcotest.(check (option string)) "nested title" (Some "Bird migration") h.title;
  count "authors" 1 (List.length h.authors);
  let body = get (Taposaur_document.Tei.render_html (Eio.Flow.string_source tei)) in
  check "exact byte limit" false (rejected (Taposaur_document.Tei.render_html
    ~max_bytes:(String.length tei) (Eio.Flow.string_source tei)));
  check "text escaped" true (includes body "Birds &amp; habitats.");
  check "no script element" false (includes body "<script>");
  check "no source URL" false (includes body "javascript:");
  check "body structure" true (includes body "<h2>Results</h2>");
  check "bounded input" true (rejected (Taposaur_document.Tei.render_html
    ~max_bytes:16 (Eio.Flow.string_source tei)));
  check "bounded output" true (rejected (Taposaur_document.Tei.render_html
    ~max_output:10 (Eio.Flow.string_source tei)));
  check "malformed" true (rejected (Taposaur_document.Tei.render_html
    (Eio.Flow.string_source "<TEI><text>")));
  check "non-TEI" true (rejected (Taposaur_document.Tei.render_html
    (Eio.Flow.string_source "<html><body>hello</body></html>")))

let publisher_metadata () = Eio_main.run (fun env ->
  let source = Eio.Path.load
    (env#cwd / "fixtures" / "elsevier-coredata.xml") in
  let parse ?max_bytes source = Taposaur_document.Elsevier.metadata
    ?max_bytes (Eio.Flow.string_source source) in
  let h = get (parse source) in
  Alcotest.(check (option string)) "journal"
    (Some "Accident Analysis & Prevention") h.journal;
  Alcotest.(check (option string)) "date"
    (Some "1969-11-30") h.published;
  equal "first author" "Roosmark" (List.hd h.authors).family;
  Alcotest.(check (option string)) "given name"
    (Some "Per-Olov") (List.hd h.authors).given;
  count "authors" 2 (List.length h.authors);
  Alcotest.(check (option string)) "page" (Some "310") h.pages;
  Alcotest.(check (option string)) "empty abstract" None h.abstract;
  check "bounded metadata" true (rejected (parse ~max_bytes:128 source));
  check "unsupported format" true (rejected (parse tei));
  check "truncated XML" true
    (rejected (parse (String.sub source 0 1500)));
  let closing = "</full-text-retrieval-response>" in
  let i = String.length source - String.length closing - 1 in
  check "body is not parsed" false
    (rejected (parse (String.sub source 0 i ^ "<broken body"))))

let extended_metadata () =
  let tei = {|<TEI><teiHeader><fileDesc><sourceDesc><biblStruct>
<monogr><imprint><biblScope unit="volume">7</biblScope>
<biblScope unit="issue">2</biblScope>
<biblScope unit="page" from="10" to="15">10-15</biblScope></imprint>
<idno type="ISSN">1234-5678</idno></monogr>
<idno type="DOI">10.1000/a</idno><idno type="DOI">10.1000/a</idno>
</biblStruct></sourceDesc></fileDesc></teiHeader><text><body/></text></TEI>|} in
  let h = get (Taposaur_document.Tei.metadata_string tei) in
  equal "TEI volume" "7" (Option.get h.volume);
  equal "TEI issue" "2" (Option.get h.issue);
  equal "page attributes not duplicated" "10–15" (Option.get h.pages);
  equal "repeated DOI not concatenated" "10.1000/a" (Option.get h.doi);
  check "non-TEI header rejected" true
    (rejected (Taposaur_document.Tei.metadata_string "<html/>"));
  let json = {|{"title":"JSON title","authors":["A. Writer"],"abstract":"Summary","body":{"text":"Ignored"}}|} in
  let parse ?max_bytes s = Taposaur_document.S2orc.metadata ?max_bytes
    (Eio.Flow.string_source s) in
  let h = get (parse json) in
  equal "S2ORC title" "JSON title" (Option.get h.title);
  equal "S2ORC author verbatim" "A. Writer" (List.hd h.authors).family;
  equal "S2ORC abstract" "Summary" (Option.get h.abstract);
  check "S2ORC exact bound" false
    (rejected (parse ~max_bytes:(String.length json) json));
  check "S2ORC bound" true (rejected (parse ~max_bytes:10 json))

let resolution () =
  let id = uid "doi:10.1000/resolution" in
  let base = get (Manifest.of_string (manifest_json id)) in
  let artifact file converter = get
    (Jsont_bytesrw.decode_string Manifest.Artifact.jsont
      (Printf.sprintf
        {|{"file":%S,"format":"tei","source":"tei","converter":%S,"bytes":10,"at":"now"}|}
        file converter)) in
  let primary = artifact "tei.xml" "pub2tei" in
  let other = artifact "tei-2.xml" "grobid" in
  let broken = artifact "tei-3.xml" "s2orc" in
  let m = {base with artifacts = [other; primary; broken]} in
  let h = Bibliography.empty in
  let docs = [
    {P.artifact = other; bibliography = Ok {h with title = Some "PDF title";
      abstract = Some "Only the other TEI has an abstract"}};
    {P.artifact = primary; bibliography = Ok {h with title = Some "Publisher title";
      abstract = Some "  "; published = Some "2024-10-23"}};
    {P.artifact = broken; bibliography = Error "Malformed XML"}] in
  let index = get (Index.of_string (index_json id)) in
  let index = {index with title = Some "Old index title";
    publisher = Some "Index-only publisher"} in
  let resolved = P.resolve ~index m docs in
  let v = P.Field.value in
  equal "preferred title" "Publisher title" (Option.get (v resolved.metadata.title));
  equal "per-field fallback" "Only the other TEI has an abstract"
    (Option.get (v resolved.metadata.abstract));
  equal "index fallback" "Index-only publisher"
    (Option.get (v resolved.metadata.publisher));
  count "derived year" 2024 (Option.get (v resolved.metadata.year));
  count "all title evidence" 3
    (List.length (P.Field.candidates resolved.metadata.title));
  count "failed document retained" 3 (List.length resolved.documents);
  equal "preferred reading" "tei.xml"
    (Option.get resolved.preferred_tei).file;
  check "metadata coverage" true (P.presence resolved).abstract;
  let projected = P.bibliography resolved in
  equal "selected bibliography" "Publisher title" (Option.get projected.title);
  equal "raw index unmodified" "Old index title"
    (Option.get (Option.get resolved.index).title);
  let correction value = {Manifest.Correction.field = Manifest.Field.Title;
    value; by = "reviewer"; at = "now"; note = None} in
  let corrected = P.resolve ~index
    {m with corrections = [correction "Earlier"; correction "Latest"]} docs in
  equal "latest correction wins" "Latest"
    (Option.get (v corrected.metadata.title));
  let docs = List.map (fun (d : P.document) ->
    if d.artifact.file = primary.file then {d with bibliography = Error "bad"}
    else d) docs in
  let fallback = P.resolve m docs in
  equal "readable alternate" "tei-2.xml" (Option.get fallback.preferred_tei).file;
  let tie = {primary with file = "tei-other.xml"} in
  let first = {P.artifact = primary; bibliography = Ok {h with title = Some "First"}} in
  let second = {P.artifact = tie; bibliography = Ok {h with title = Some "Second"}} in
  let tie_manifest = {m with artifacts = [primary; tie]} in
  let tied = P.resolve tie_manifest [second; first] in
  equal "ties follow manifest order" "First"
    (Option.get (v tied.metadata.title));
  let published = {tie with version = Some Manifest.Version.Published} in
  let versioned = P.resolve {m with artifacts = [primary; published]}
    [first; {second with artifact = published}] in
  equal "published version priority" "Second"
    (Option.get (v versioned.metadata.title));
  let explicit_year = P.resolve m
    [{P.artifact = primary;
      bibliography = Ok {h with year = Some 1987}}] in
  count "format-independent explicit year" 1987
    (Option.get (v explicit_year.metadata.year));
  check "identity mismatch" true
    (try ignore (P.resolve ~index:{index with uid = uid "doi:10.1000/other"}
      m []); false with Invalid_argument _ -> true)

let enrichment () = with_temp (fun _ sw root ->
  let id, shard, _ = setup root in
  let a = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  save root ("index/" ^ shard) (index_json id);
  ignore (I.shard c a ~shard);
  count "no indexed title" 0 (C.stats c).titles;
  count "empty FTS" 0 (List.length (C.search c (C.Query.make ~text:"migration" ())).papers);
  let first = E.run ~limit:1 c a in
  count "one enriched" 1 first.updated;
  count "title available" 1 (C.stats c).titles;
  check "raw row preserved" true ((Option.get (C.find_index c id)).title = None);
  let results = (C.search c (C.Query.make ~text:"migration" ())).papers in
  count "enriched FTS" 1 (List.length results);
  equal "enriched result" "Bird migration" (Option.get (List.hd results).Summary.bibliography.title);
  count "resume skips" 0 (E.run c a).examined;
  count "with title" 1 (List.length (C.search c
    {(C.Query.make ()) with metadata = [C.Query.Title,C.Query.Present]}).papers);
  count "without title" 0 (List.length (C.search c
    {(C.Query.make ()) with metadata = [C.Query.Title,C.Query.Missing]}).papers);
  let path = "papers/" ^ Layout.paper id ^ "/paper.json" in
  let m = get (Manifest.of_string (Eio.Path.load (root / path))) in
  let correction = {Manifest.Correction.field = Manifest.Field.Title;
    value = "Unique revised title"; by = "reviewer"; at = "now"; note = None} in
  save root path (get (Manifest.to_string {m with corrections = [correction]}));
  count "force refresh" 1 (E.run ~force:true c a).updated;
  count "old FTS removed" 0
    (List.length (C.search c (C.Query.make ~text:"migration" ())).papers);
  count "new FTS added" 1
    (List.length (C.search c (C.Query.make ~text:"revised" ())).papers);
  ignore (I.shard ~force:true c a ~shard);
  count "reimport invalidates" 0 (C.stats c).enriched;
  count "metadata cache cleared" 0
    (List.length (C.search c (C.Query.make ~text:"revised" ())).papers);
  Eio.Path.unlink (root / path);
  count "missing manifest recorded" 1 (E.run c a).unavailable;
  count "metadata error diagnostics" 1 (List.length (C.Metadata.errors c));
  count "failed attempt resumes" 0 (E.run c a).examined;
  save root path (get (Manifest.to_string m));
  count "explicit retry" 1 (E.run ~force:true c a).updated;
  count "error cleared" 0 (C.stats c).enrichment_errors)

let storage_snapshots () = with_temp (fun _ sw root ->
  let c = C.open_write ~sw (root / "catalog.db") in
  let row = get (Index.of_string (index_json (uid "doi:10.1000/snapshot"))) in
  let shard = Layout.index row.uid in
  let stamp = {C.Shard.size = 10L; mtime = 1.} in
  let replace () = C.Shard.replace c ~shard stamp (fun emit -> emit row) in
  count "storage accepts an index stream" 1 (replace ());
  let candidate () = List.hd (C.Metadata.pending ~force:true ~limit:1
    ~version:1 c) in
  let stale = candidate () in
  ignore (replace ());
  let bibliography = {Bibliography.empty with title = Some "Resolved";
    volume = Some "19"; pages = Some "12–20"; publisher = Some "Example"} in
  check "identical reimport invalidates snapshot" false
    (C.Metadata.publish c stale ~version:1 (Ok bibliography));
  check "fresh snapshot publishes" true
    (C.Metadata.publish c (candidate ()) ~version:1 (Ok bibliography));
  let summary = Option.get (C.find c row.uid) in
  equal "full bibliography retained" "19"
    (Option.get summary.bibliography.volume);
  equal "page range retained" "12–20" (Option.get summary.bibliography.pages);
  let roundtrip = get (Summary.of_string (get (Summary.to_string summary))) in
  check "summary codec" true (summary = roundtrip);
  check "raw index stays raw" true
    ((Option.get (C.find_index c row.uid)).title = None);
  let exception Source_failed in
  check "source exception propagates" true
    (try ignore (C.Shard.replace c ~shard stamp (fun emit ->
       emit row; raise Source_failed)); false with Source_failed -> true);
  equal "failed replacement retains metadata" "Resolved"
    (Option.get (Option.get (C.find c row.uid)).bibliography.title))

let catalogue_migration () = with_temp (fun env sw root ->
  let path = root / "legacy.db" in
  let legacy = Sqlite3_eio.open_path ~sw path in
  let row = get (Index.of_string
    (index_json (uid "doi:10.1000/migration"))) in
  let raw = {row with title = Some "Original title"} in
  let resolved = {row with title = Some "Resolved title"} in
  let schema = Eio.Path.load (env#cwd / "fixtures" / "catalog-v2.sql") in
  Sqlite3_eio.run legacy (fun db ->
    Sqlite3.Rc.check (Sqlite3.exec db schema);
    let s = Sqlite3.prepare db
      "INSERT INTO papers(uid,shard,scheme,title,authors,journal,publisher,\
       year,abstract,pdf,xml,tei,s2orc,json,resolved_json,metadata_version) \
       VALUES(?,?,?,'Resolved title','','','',NULL,'',0,0,0,0,?,?,1)" in
    Fun.protect ~finally:(fun () -> ignore (Sqlite3.finalize s)) (fun () ->
      Sqlite3.Rc.check (Sqlite3.bind_values s
        (List.map (fun value -> Sqlite3.Data.TEXT value)
          [Uid.to_string row.uid; Layout.index row.uid; "doi";
           get (Index.to_string raw); get (Index.to_string resolved)]));
      Sqlite3.Rc.check (Sqlite3.step s)));
  check "old schema needs writable migration" true
    (try ignore (C.open_read ~sw path); false with Failure _ -> true);
  let c = C.open_write ~sw path in
  let read = C.open_read ~sw path in
  equal "migration retains raw row" "Original title"
    (Option.get (Option.get (C.find_index read row.uid)).title);
  equal "migration retains cached metadata" "Resolved title"
    (Option.get (Option.get (C.find read row.uid)).bibliography.title);
  count "migration retains search index" 1
    (List.length (C.search read (C.Query.make ~text:"Resolved" ())).papers);
  let candidate = List.hd (C.Metadata.pending ~force:true ~limit:1 ~version:1 c) in
  let b = {Bibliography.empty with title = Some "New title";
    volume = Some "5"} in
  check "migrated cache accepts full bibliography" true
    (C.Metadata.publish c candidate ~version:1 (Ok b));
  equal "new metadata read after legacy cache" "5"
    (Option.get (Option.get (C.find read row.uid)).bibliography.volume))

let catalogue () = with_temp (fun _ sw root ->
  let id, shard, e = setup root in
  let a = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  (match I.shard c a ~shard with I.Loaded 1 -> () | _ -> Alcotest.fail "import");
  count "stats" 1 (C.stats c).papers;
  check "unchanged" true (I.shard c a ~shard = I.Unchanged);
  count "FTS" 1 (List.length (C.search c (C.Query.make ~text:"habitat" ())).papers);
  check "find" true (Option.is_some (C.find c id));
  List.iter (fun text -> ignore (C.search c (C.Query.make ~text ())))
    ["\""; "*"; "NEAR("; ":"; "' OR 1=1 --"];
  count "literal operators" 0 (List.length
    (C.search c (C.Query.make ~text:"bird OR nonexistent" ())).papers);
  count "publisher filter" 0 (List.length
    (C.search c {(C.Query.make ()) with publisher = "Missing"}).papers);
  save root ("index/" ^ shard) (get (Index.to_string e) ^ "\n{broken}\n");
  check "bad import" true (match I.shard ~force:true c a ~shard with
    | I.Failed _ -> true | _ -> false);
  count "old data retained" 1 (C.stats c).papers;
  count "error recorded" 1 (C.stats c).errors;
  save root ("index/" ^ shard)
    (get (Index.to_string {e with title = Some "Ocean currents";
      abstract = None; authors = []}) ^ "\n");
  ignore (I.shard ~force:true c a ~shard);
  count "error cleared" 0 (C.stats c).errors;
  count "old FTS removed" 0 (List.length
    (C.search c (C.Query.make ~text:"habitat" ())).papers);
  count "new FTS" 1 (List.length (C.search c (C.Query.make ~text:"ocean" ())).papers);
  let ro = C.open_read ~sw (root / "catalog.db") in
  count "read-only search" 1
    (List.length (C.search ro (C.Query.make ())).papers);
  save root ("index/" ^ shard) "";
  ignore (I.shard ~force:true c a ~shard);
  count "empty shard retires rows" 0 (C.stats c).papers)

let incremental () = with_temp (fun _ sw root ->
  let id, shard, e = setup root in
  let second = uid "doi:10.1000/second" in
  let shard2 = Layout.index second in
  check "different shards" true (shard <> shard2);
  save root ("index/" ^ shard2)
    (get (Index.to_string {e with uid = second}) ^ "\n");
  let a = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  let first = I.sync ~limit:1 c a in
  count "limited import" 1 first.loaded;
  Alcotest.(check (option int)) "coverage discovered" (Some 2)
    (C.stats c).discovered_shards;
  let next = I.sync ~limit:1 c a in
  count "resume advances" 1 next.loaded;
  count "all rows" 2 (C.stats c).papers;
  let first = C.search c {(C.Query.make ()) with limit = 1} in
  let second_page = C.search c {(C.Query.make ()) with limit = 1; after = first.next} in
  count "first page" 1 (List.length first.papers);
  count "second page" 1 (List.length second_page.papers);
  check "cursor does not repeat" true
    (Uid.compare (List.hd first.papers).uid (List.hd second_page.papers).uid <> 0);
  check "last page" true (second_page.next = None);
  save root ("index/" ^ shard2) (get (Index.to_string {e with uid = id}));
  check "misplaced UID" true
    (match I.shard ~force:true c a ~shard:shard2 with
     | I.Failed _ -> true | _ -> false);
  count "misplaced import preserves rows" 2 (C.stats c).papers;
  save root ("index/" ^ shard2)
    (String.concat "\n" [get (Index.to_string {e with uid = second});
      get (Index.to_string {e with uid = second})]);
  check "duplicate UID" true
    (match I.shard ~force:true c a ~shard:shard2 with
     | I.Failed _ -> true | _ -> false);
  count "duplicate import preserves rows" 2 (C.stats c).papers)

let corpus_fixtures () = Eio_main.run (fun env ->
  List.iter (fun prefix ->
    let file suffix = env#cwd / "fixtures" / (prefix ^ "-" ^ suffix) in
    let m = get (Manifest.of_string (Eio.Path.load (file "paper.json"))) in
    let i = get (Index.of_string (Eio.Path.load (file "index.json"))) in
    equal "fixture UID" (Uid.to_string m.id) (Uid.to_string i.uid);
    let count = ref 0 in
    A.iter_jsonl ~decode:Event.of_string (file "events.jsonl")
      (fun _ _ -> incr count);
    check "fixture events" true (!count > 0)) ["doi"; "pmc"])

let website () = with_temp (fun env sw root ->
  let id, shard, _ = setup root in
  let a = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  ignore (I.shard c a ~shard);
  let read = C.open_read ~sw (root / "catalog.db") in
  let state = Taposaur_web.create ~archive:a ~catalog:read in
  let request ?headers ?(meth = Httpz.Method.Get) target =
    W.request ?headers ~on_error:(fun ex -> raise ex) Taposaur_web.site state meth target in
  let code r = Proffer.Status.code (W.status r) in
  count "home" 200 (code (request "/"));
  let results = request "/search?q=bird" in
  count "search" 200 (code results);
  check "title escaped" true (includes (W.body results) "&lt;script&gt;");
  let q = "?uid=" ^ Uriz.pct_encode ~component:`Unreserved (Uid.to_string id) in
  let paper = request ("/paper" ^ q) in
  count "paper" 200 (code paper);
  check "embedded PDF" true
    (includes (W.body paper) ("src=\"/artifact" ^ q ^
      "&amp;file=publisher.pdf\""));
  check "TEI reading link" true
    (includes (W.body paper) ("href=\"/read" ^ q ^
      "&amp;file=tei.xml\""));
  count "events" 200 (code (request ("/events" ^ q)));
  let target = "/artifact" ^ q ^ "&file=publisher.pdf" in
  equal "whole PDF" pdf (W.body (request target));
  let r = request ~headers:["Range","bytes=0-4"] target in
  count "range" 206 (code r); equal "range bytes" "%PDF-" (W.body r);
  let suffix = request ~headers:["Range","bytes=-6"] target in
  equal "suffix range" "%%EOF\n" (W.body suffix);
  count "unsatisfiable" 416
    (code (request ~headers:["Range","bytes=9999-"] target));
  equal "HEAD empty" "" (W.body (request ~meth:Httpz.Method.Head target));
  count "HEAD ignores Range" 200 (code (request ~meth:Httpz.Method.Head
    ~headers:["Range","bytes=0-4"] target));
  count "unverifiable If-Range" 200 (code (request
    ~headers:["Range","bytes=0-4"; "If-Range","\"old\""] target));
  count "unlisted file" 404
    (code (request ("/artifact" ^ q ^ "&file=..%2Fsecret")));
  count "TEI view" 200 (code (request ("/read" ^ q ^ "&file=tei.xml")));
  count "bad query" 400 (code (request "/search?from=bad"));
  let mutation = request ~meth:Httpz.Method.Post "/paper" in
  check "no mutation route" true (code mutation = 404 || code mutation = 405);
  let dir = "papers/" ^ Layout.paper id in
  let manifest_path = dir ^ "/paper.json" in
  let m = get (Manifest.of_string (Eio.Path.load (root / manifest_path))) in
  let publisher = Eio.Path.load
    (env#cwd / "fixtures" / "elsevier-coredata.xml") in
  let artifact = get (Jsont_bytesrw.decode_string Manifest.Artifact.jsont
    (Printf.sprintf
      {|{"file":"publisher.xml","format":"xml","source":"publisher","bytes":%d,"at":"x"}|}
      (String.length publisher))) in
  save root (dir ^ "/publisher.xml") publisher;
  save root manifest_path (get (Manifest.to_string
    { m with artifacts = artifact :: m.artifacts }));
  let rich = W.body (request ("/paper" ^ q)) in
  check "index retained as alternative" true
    (includes rich "Bird migration &lt;script&gt;");
  save root ("index/" ^ shard) (index_json id);
  ignore (I.shard ~force:true c a ~shard);
  let sparse = W.body (request ("/paper" ^ q)) in
  List.iter (fun value -> check ("publisher fallback: " ^ value) true
    (includes sparse value))
    ["Recherche des accidents"; "Per-Olov Roosmark"; "Rolf Fräki";
     "Accident Analysis &amp; Prevention"; "1969-11-30"; "310"];
  let corrected = { Manifest.Correction.field = Manifest.Field.Title;
    value = "Corrected title"; by = "reviewer"; at = "now"; note = None } in
  save root manifest_path (get (Manifest.to_string
    { m with corrections = [corrected] }));
  let corrected_page = W.body (request ("/paper" ^ q)) in
  check "correction priority" true
    (includes corrected_page "<h1>Corrected title</h1>");
  save root manifest_path (get (Manifest.to_string m));
  let from_tei = W.body (request ("/paper" ^ q)) in
  check "TEI title fallback" true
    (includes from_tei "<h1>Bird migration</h1>");
  check "TEI authors fallback" true (includes from_tei "Ada Bird");
  save root (dir ^ "/tei.xml") "<broken>";
  count "bad header preserves paper page" 200
    (code (request ("/paper" ^ q))))

let () = Alcotest.run "taposaur" [
  "core", [Alcotest.test_case "identities and layouts" `Quick identifiers;
    Alcotest.test_case "records" `Quick records;
    Alcotest.test_case "production fixtures" `Quick corpus_fixtures];
  "archive", [Alcotest.test_case "streaming" `Quick streaming;
    Alcotest.test_case "confined reads" `Quick archive_reads];
  "document", [Alcotest.test_case "TEI parsing and rendering" `Quick documents;
    Alcotest.test_case "publisher metadata" `Quick publisher_metadata;
    Alcotest.test_case "TEI bibliography and S2ORC" `Quick extended_metadata];
  "paper", [Alcotest.test_case "field evidence and multiple TEIs"
    `Quick resolution];
  "catalogue", [
    Alcotest.test_case "storage snapshots" `Quick storage_snapshots;
    Alcotest.test_case "schema migration" `Quick catalogue_migration;
    Alcotest.test_case "metadata enrichment" `Quick enrichment;
    Alcotest.test_case "transactional import and search" `Quick catalogue;
    Alcotest.test_case "incremental imports and paging" `Quick incremental];
  "web", [Alcotest.test_case "read-only routes" `Quick website]]
