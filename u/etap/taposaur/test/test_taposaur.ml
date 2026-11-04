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
    publisher = Some "Example"; journal = Some "Journal of Birds";
    year = Some 2020 } in
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

let fingerprints () = with_temp (fun _ sw root ->
  let id, _, _ = setup root in
  let archive = A.open_root ~sw root in
  let paper = Option.get (A.find archive id) in
  let manifest = A.manifest paper in
  (* Existing healthy stamps must survive the change from open/fstat to
     lstat so that the optimisation does not invalidate cached metadata. *)
  let old_stamps = List.map (fun (a : Manifest.Artifact.t) ->
    A.with_artifact paper a.file (fun file ->
      let s = Eio.File.stat file in
      Printf.sprintf "%s:%Ld:%.17g:%.17g" a.file
        (Optint.Int63.to_int64 s.size) s.mtime s.ctime)) manifest.artifacts in
  let encoded = get (Manifest.to_string manifest) in
  let old = Digestif.SHA256.(to_hex (digest_string
    (String.concat "\000" (encoded :: old_stamps)))) in
  equal "healthy stamp compatibility" old (A.fingerprint paper);
  let file = root / "papers" / Layout.paper id / "publisher.pdf" in
  let native = Eio.Path.native_exn file in
  let before = Eio.Path.stat ~follow:false file in
  Unix.sleepf 0.002;
  Unix.chmod native 0o400;
  let after = Eio.Path.stat ~follow:false file in
  check "permission change preserves size and mtime" true
    (before.size = after.size && before.mtime = after.mtime);
  check "ctime change invalidates stamp" true (A.fingerprint paper <> old);
  Unix.chmod native 0o600;
  Eio.Path.unlink file;
  let missing = A.fingerprint paper in
  check "missing artifact changes stamp" true (missing <> old);
  Eio.Path.symlink ~link_to:"/etc/passwd" file;
  let linked = A.fingerprint paper in
  check "symlink differs from regular artifact" true (linked <> old);
  check "symlink differs from missing artifact" true (linked <> missing);
  Eio.Path.unlink file;
  Eio.Path.symlink ~link_to:"/missing/outside/archive" file;
  equal "symlink target is not inspected" linked (A.fingerprint paper);
  Eio.Path.unlink file;
  Eio.Path.mkdir ~perm:0o700 file;
  check "directory cannot masquerade as regular file" true
    (A.fingerprint paper <> old))

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
  check "body structure" true (includes body ">Results</h2>");
  let references = {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><text>
<body><p>See <ref target="#b0">[1]</ref>.</p></body><back><listBibl>
<biblStruct xml:id="b0"><note type="raw_reference">First &amp; last.</note><idno type="s2corpus">123</idno></biblStruct>
<biblStruct xml:id="b1"><analytic><author><persName><forename>Ada</forename><surname>Bird</surname></persName></author><title>Birds</title></analytic><monogr><title>Journal</title><imprint><date>2020</date></imprint></monogr></biblStruct>
</listBibl></back></text></TEI>|} in
  let rendered = get (Taposaur_document.Tei.render_html
    (Eio.Flow.string_source references)) in
  check "reference anchor" true
    (includes rendered "id=\"tei-6230\" class=\"tei-reference\"");
  check "citation link" true
    (includes rendered "href=\"#tei-6230\">[1]</a>");
  check "raw reference escaped" true
    (includes rendered "First &amp; last.");
  check "identifier separated and labelled" true
    (includes rendered "class=\"tei-identifier\">s2corpus: 123</span>");
  check "structured names separated" true
    (includes rendered ">Ada</span> <span");
  check "bounded input" true (rejected (Taposaur_document.Tei.render_html
    ~max_bytes:16 (Eio.Flow.string_source tei)));
  check "bounded output" true (rejected (Taposaur_document.Tei.render_html
    ~max_output:10 (Eio.Flow.string_source tei)));
  check "malformed" true (rejected (Taposaur_document.Tei.render_html
    (Eio.Flow.string_source "<TEI><text>")));
  check "non-TEI" true (rejected (Taposaur_document.Tei.render_html
    (Eio.Flow.string_source "<html><body>hello</body></html>")))

let tei_audit () = with_temp (fun _ sw root ->
  let module T = Taposaur_document.Tei in
  let module Audit = Taposaur_indexing.Tei_audit in
  check "complete document" false (rejected (T.validate_string tei));
  List.iter (fun text -> check "invalid complete document" true
    (rejected (T.validate_string text)))
    [""; "<TEI><text><body></text></TEI>"; "<html/>";
     "<TEI/><TEI/>"; "<TEI xmlns='urn:wrong'/>";
     "<!DOCTYPE TEI [<!ENTITY x SYSTEM 'file:///etc/passwd'>]>\
      <TEI><text>&x;</text></TEI>"];
  check "validation byte bound" true
    (rejected (T.validate_string ~max_bytes:10 tei));
  let id, _, _ = setup root in
  let archive = A.open_root ~sw root in
  let dir = "papers/" ^ Layout.paper id in
  let manifest_path = dir ^ "/paper.json" in
  let manifest = A.manifest (Option.get (A.find archive id)) in
  let artifacts = List.map (fun (a : Manifest.Artifact.t) ->
    if a.format = Manifest.Format.Tei then
      {a with converter = Some "grobid"; from = Some "publisher.pdf"}
    else a) manifest.artifacts in
  save root manifest_path
    (get (Manifest.to_string {manifest with artifacts}));
  let report () = List.hd (Audit.paper archive id) in
  equal "valid TEI stays valid" "well_formed_tei" (report ()).status;
  let failure = "[NO_BLOCKS] PDF parsing resulted in empty content" in
  save root (dir ^ "/tei.xml") failure;
  let r = report () in
  equal "GROBID refusal recognised" "grobid_error_NO_BLOCKS" r.status;
  equal "source PDF checked" "pdf_signature_and_size_match" r.pdf_status;
  equal "refusal requires input review"
    "inspect_pdf_for_ocr_or_replacement" r.action;
  equal "source unchanged" failure (Eio.Path.load (root / dir / "tei.xml"));
  count "evidence digest" 64 (String.length r.sha256);
  check "JSON evidence encodes" true (includes (Audit.to_json r) "NO_BLOCKS");
  save root (dir ^ "/tei.xml")
    "[GENERAL] An exception occurred while running Grobid.";
  equal "general error candidate" "candidate_for_grobid_regeneration"
    (report ()).action;
  Eio.Path.unlink (root / dir / "publisher.pdf");
  equal "missing PDF blocks retry" "repair_pdf_input_before_regeneration"
    (report ()).action;
  save root (dir ^ "/tei.xml") "<?xml version='1.0'?>";
  equal "truncated XML is separate" "invalid_tei" (report ()).status)

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

(* GROBID's header model sometimes leaves the abstract in the body, as a
   section headed "Abstract" or as unheaded leading paragraphs. The reader
   recovers those, never invents one from an introduction, and reports
   whether the body holds prose at all. *)
let body_abstracts () =
  let header abstract = Printf.sprintf
    {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title>Whales</title></titleStmt></fileDesc><profileDesc><abstract>%s</abstract></profileDesc></teiHeader>|}
    abstract in
  let back = {|<back><listBibl><biblStruct><analytic><title>Not an abstract</title></analytic></biblStruct></listBibl></back>|} in
  let doc ?(abstract = "") body =
    header abstract ^ "<text><body>" ^ body ^ "</body>" ^ back ^ "</text></TEI>" in
  let inspect s = get (Taposaur_document.Tei.inspect_string s) in
  let abstract (i : Taposaur_document.Tei.inspection) =
    Option.value ~default:"" i.bibliography.abstract in
  let long = String.concat " "
    (List.init 60 (fun i -> "Sentence number " ^ string_of_int i ^ ".")) in
  let intro = {|<div><head>Introduction</head><p>Intro text.</p></div>|} in
  let headed = doc ({|<div><head>IntroductIon</head><p>Intro text.</p></div><div><head>Abstract.</head><p>First point.</p><p>Second <hi>point</hi>.</p></div><div><head>Methods</head><p>M</p></div>|}) in
  let i = inspect headed in
  equal "abstract from headed section" "First point. Second point."
    (abstract i);
  check "derived" true i.derived_abstract;
  check "body text seen" true i.text;
  equal "metadata derives too" "First point. Second point."
    (Option.get (get (Taposaur_document.Tei.metadata_string headed)).abstract);
  let i = inspect (doc ("<div><p>" ^ long ^ "</p></div>" ^ intro)) in
  equal "unheaded leading section" long (abstract i);
  check "leading section is derived" true i.derived_abstract;
  let i = inspect (doc ("<p>" ^ long ^ "</p>" ^ intro)) in
  equal "paragraphs before any section" long (abstract i);
  let run_on = String.concat " " (List.init 250 (fun i ->
    "Introduction sentence " ^ string_of_int i ^ ".")) in
  let i = inspect (doc ("<div><p>" ^ long ^ "</p><p>" ^ run_on ^ "</p></div>"
    ^ intro)) in
  equal "first paragraph of a run-on block" long (abstract i);
  let i = inspect (doc ("<div><p>Aim.</p><p>" ^ long ^ "</p></div>" ^ intro))
  in
  equal "short structured block kept whole" ("Aim. " ^ long) (abstract i);
  let i = inspect (doc {|<div><head>Abstract</head><p><s>One.</s><s>Two.</s></p></div>|}) in
  equal "sentence elements are spaced" "One. Two." (abstract i);
  let i = inspect (doc (intro ^ {|<div><head>Methods</head><p>M</p></div>|})) in
  check "introduction is not an abstract" true
    (i.bibliography.abstract = None && i.text && not i.derived_abstract);
  let i = inspect (doc ({|<div><p>Keywords: whales, sound.</p></div>|} ^ intro)) in
  check "short leading block is not an abstract" true
    (i.bibliography.abstract = None);
  let i = inspect (doc ~abstract:"From the header."
    {|<div><head>Abstract</head><p>From the body.</p></div>|}) in
  equal "header abstract kept" "From the header." (abstract i);
  check "header abstract is not derived" false i.derived_abstract;
  let i = inspect (doc {|<div><p/></div>|}) in
  check "empty body has no text" false i.text;
  check "empty body has no abstract" true (i.bibliography.abstract = None);
  let i = inspect (doc {|<div><figure><head>Fig 1</head><p>Caption only.</p></figure></div>|}) in
  check "figure captions are not prose" false i.text;
  let i = inspect (doc {|<div><p>Prose.</p><note>Marginal.</note></div>|}) in
  check "prose is text" true i.text;
  let i = inspect (doc ({|<div><head>A</head><p>a</p></div><div><head>B</head><p>b</p></div><div><head>C</head><p>c</p></div><div><head>D</head><p>d</p></div><div><head>Abstract</head><p>Too late to count.</p></div>|})) in
  check "scan is bounded to the first sections" true
    (i.bibliography.abstract = None);
  let front = header "" ^ {|<text><front><div type="abstract"><abstract><p>Front matter.</p></abstract></div></front><body><div><head>Abstract</head><p>Body copy.</p></div></body></text></TEI>|} in
  equal "Pub2TEI front abstract still wins" "Front matter."
    (String.trim (abstract (inspect front)));
  let primary = get (Jsont_bytesrw.decode_string Manifest.Artifact.jsont
    {|{"file":"tei.xml","format":"tei","source":"tei","converter":"grobid","bytes":10,"at":"now"}|}) in
  let m = {(get (Manifest.of_string (manifest_json (uid "doi:10.1000/text"))))
    with artifacts = [primary]} in
  let resolved text = (P.resolve m
    [{P.artifact = primary; bibliography = Ok Bibliography.empty; text}]).text in
  check "paper text from documents" true
    (resolved (Some true) = Some true && resolved (Some false) = Some false
     && resolved None = None)

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
      abstract = Some "Only the other TEI has an abstract"}; text = None};
    {P.artifact = primary; bibliography = Ok {h with title = Some "Publisher title";
      abstract = Some "  "; published = Some "2024-10-23"}; text = None};
    {P.artifact = broken; bibliography = Error "Malformed XML"; text = None}] in
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
  let first = {P.artifact = primary; bibliography = Ok {h with title = Some "First"}; text = None} in
  let second = {P.artifact = tie; bibliography = Ok {h with title = Some "Second"}; text = None} in
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
      bibliography = Ok {h with year = Some 1987}; text = None}] in
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
  count "listed resolved uid skipped" 0 (E.run ~uids:[id] c a).examined;
  count "listed unknown uid ignored" 0
    (E.run ~uids:[uid "doi:10.1000/none"] c a).examined;
  count "listed uid forced" 1 (E.run ~force:true ~uids:[id] c a).updated;
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
  save root path (String.make (17 * 1024 * 1024) ' ');
  count "oversized manifest recorded" 1 (E.run ~retry_errors:true c a).unavailable;
  check "oversized manifest diagnostic" true
    (includes (snd (List.hd (C.Metadata.errors c))) "Manifest exceeds 16 MiB");
  count "oversized manifest skipped on resume" 0 (E.run c a).examined;
  save root path (get (Manifest.to_string m));
  count "explicit retry" 1 (E.run ~force:true c a).updated;
  count "error cleared" 0 (C.stats c).enrichment_errors)

let detail_cache () = with_temp (fun _ sw root ->
  let id, shard, index = setup root in
  let archive = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  ignore (I.shard c archive ~shard);
  let original = Taposaur_reader.load ~index
    (Option.get (A.find archive id)) in
  let json = get (P.cache_string original) in
  let restored = get (P.of_cache_string ~index json) in
  check "all evidence and preferred files survive round trip" true
    (original = restored);
  check "cache identity must match index" true
    (rejected (P.of_cache_string
      ~index:{index with uid = uid "doi:10.1000/other"} json));
  let manifest = get (Manifest.to_string original.manifest) in
  let invalid version documents = Printf.sprintf
    {|{"version":%d,"manifest":%s,"documents":%s}|}
    version manifest documents in
  List.iter (fun json -> check "invalid cache rejected" true
    (rejected (P.of_cache_string ~index json)))
    [invalid 3 "[]"; invalid 1 {|[{"file":"unknown","error":"bad"}]|};
     invalid 1 {|[{"file":"tei.xml","error":"bad","bibliography":{}}]|};
     invalid 1 {|[{"file":"tei.xml","error":"a"},{"file":"tei.xml","error":"b"}]|}];
  let candidate () = List.hd (C.Metadata.pending ~force:true ~limit:1
    ~version:P.version c) in
  let stale = candidate () in
  ignore (E.run c archive);
  count "detail counter" 1 (C.Metadata.detail_count c);
  check "body text recorded in the catalogue" true
    ((Option.get (C.find c id)).holdings.text <> None);
  let saved = Option.get (C.Metadata.detail c id) in
  check "atomic detail publication" true
    (get (P.of_cache_string ~index:saved.index saved.json) = original);
  check "stale detail publication refused" false
    (C.Metadata.publish ~detail_cache:"invalid" c stale ~version:P.version
      (Ok (P.bibliography original)));
  equal "stale update preserved cache" saved.json
    (Option.get (C.Metadata.detail c id)).json;
  ignore (C.Metadata.publish c (candidate ()) ~version:P.version
    (Error "Refresh failed"));
  let failed = Option.get (C.Metadata.detail c id) in
  equal "failed refresh preserved cache" saved.json failed.json;
  check "failed refresh preserved cache timestamp" true
    (saved.saved_at = failed.saved_at);
  ignore (C.Metadata.publish c (candidate ()) ~version:P.version
    (Ok (P.bibliography original)));
  count "successful publication without details clears cache" 0
    (C.Metadata.detail_count c);
  count "previous metadata is backfilled" 1 (E.run c archive).updated;
  count "backfill resumes" 0 (E.run c archive).examined;
  let read = C.open_read ~sw (root / "catalog.db") in
  let state = Taposaur_web.create ~archive ~catalog:read () in
  Eio.Path.rmtree (root / "papers");
  let q = "?uid=" ^ Uriz.pct_encode ~component:`Unreserved (Uid.to_string id) in
  let request target = W.request ~on_error:(fun ex -> raise ex)
    Taposaur_web.site state Httpz.Method.Get target in
  List.iter (fun suffix ->
    let response = request ("/paper" ^ q ^ suffix) in
    count "cached details work without archive" 200
      (Proffer.Status.code (W.status response));
    check "cached page includes files and panel" true
      (includes (W.body response) "publisher.pdf" &&
       includes (W.body response) {|id="paper-panel"|})) [""; "&view=full"];
  ignore (I.shard ~force:true c archive ~shard);
  count "reimport removes details" 0 (C.Metadata.detail_count c);
  check "reimport invalidates cache" true (C.Metadata.detail c id = None))

let freshness () = with_temp (fun _ sw root ->
  let id, shard, _ = setup root in
  let a = A.open_root ~sw root in
  let c = C.open_write ~sw (root / "catalog.db") in
  ignore (I.shard c a ~shard);
  ignore (E.run c a);
  count "unchanged source" 0 (E.check c a).stale;
  let dir = "papers/" ^ Layout.paper id in
  let m = get (Manifest.of_string (Eio.Path.load (root / dir / "paper.json"))) in
  save root (dir ^ "/paper.json")
    (get (Manifest.to_string {m with title = Some "New manifest title"}));
  count "changed manifest" 1 (E.check c a).stale;
  count "stale counter" 1 (C.stats c).stale;
  count "stale excluded from complete" 0 (C.stats c).enriched;
  count "stale automatically enriched" 1 (E.run c a).updated;
  count "stale cleared" 0 (C.stats c).stale;
  save root (dir ^ "/tei.xml") "<broken>";
  count "changed document" 1 (E.check c a).stale;
  count "partial result" 1 (E.run c a).partial;
  count "partial counter" 1 (C.stats c).partial;
  count "partial not complete" 0 (C.stats c).enriched;
  count "document diagnostic retained" 1
    (List.length (Option.get (C.Metadata.status c id)).diagnostics);
  check "failed TEI not preferred" true
    ((Taposaur_reader.load (Option.get (A.find a id))).preferred_tei = None);
  save root (dir ^ "/tei.xml") tei;
  count "targeted retry" 1 (E.run ~retry_errors:true c a).updated;
  count "diagnostics cleared" 0 (C.stats c).partial;
  count "complete restored" 1 (C.stats c).enriched;
  let missing = dir ^ "/paper.json" in
  Eio.Path.unlink (root / missing);
  count "missing source stale" 1 (E.check c a).stale;
  count "missing source attempt" 1 (E.run c a).unavailable;
  count "failed stale source does not starve resume" 0 (E.run c a).examined;
  check "existing shard retirement refused" true
    (try I.retire c a ~shard; false with Invalid_argument _ -> true);
  Eio.Path.unlink (root / "index" / shard);
  I.retire c a ~shard;
  count "retirement removes rows" 0 (C.stats c).papers;
  count "retirement removes metadata counters" 0 (C.stats c).enriched;
  count "retirement removes titles" 0 (C.stats c).titles)

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
  let snapshot = candidate () in
  check "newer publication succeeds" true
    (C.Metadata.publish c snapshot ~version:1 (Ok bibliography));
  check "older publication cannot overwrite" false
    (C.Metadata.publish c snapshot ~version:1 (Error "old failure"));
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
  let raw = {row with title = Some "Original title";
    journal = Some "Legacy Journal"} in
  let resolved = {row with title = Some "Resolved title";
    journal = Some "Legacy Journal"} in
  let schema = Eio.Path.load (env#cwd / "fixtures" / "catalog-v2.sql") in
  Sqlite3_eio.run legacy (fun db ->
    Sqlite3.Rc.check (Sqlite3.exec db schema);
    let s = Sqlite3.prepare db
      "INSERT INTO papers(uid,shard,scheme,title,authors,journal,publisher,\
       year,abstract,pdf,xml,tei,s2orc,json,resolved_json,metadata_version) \
       VALUES(?,?,?,'Resolved title','','Legacy Journal','',NULL,'',0,0,0,0,?,?,1)" in
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
  equal "migration seeds journals" "Legacy Journal"
    (List.hd (C.journals read ~text:"legacy")).name;
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

let journal_selection () = with_temp (fun _ sw root ->
  let c = C.open_write ~sw (root / "catalog.db") in
  let id = uid "doi:10.1000/journals" in
  let shard = Layout.index id in
  let rec second n =
    let candidate = uid ("doi:10.1000/journal-" ^ string_of_int n) in
    if Layout.index candidate = shard then candidate else second (n + 1) in
  let row = get (Index.of_string (index_json id)) in
  let row = {row with journal = Some "Journal of Birds"} in
  let other = {row with uid = second 1} in
  let stamp = {C.Shard.size = 10L; mtime = 1.} in
  ignore (C.Shard.replace c ~shard stamp (fun emit -> emit row; emit other));
  let suggestions text = C.journals c ~text in
  count "journal grouping" 1 (List.length (suggestions ""));
  count "paper count" 2 (List.hd (suggestions "birds")).papers;
  equal "case-insensitive substring" "Journal of Birds"
    (List.hd (suggestions "OF BIRDS")).name;
  let candidate = List.hd (C.Metadata.pending ~uid:id ~limit:1 ~version:1 c) in
  ignore (C.Metadata.publish c candidate ~version:1
    (Ok {Bibliography.empty with journal = Some "Birds & 100% Ecology"}));
  count "old journal decremented" 1 (List.hd (suggestions "Journal")).papers;
  equal "prefix before substring" "Birds & 100% Ecology"
    (List.hd (suggestions "birds")).name;
  count "percent is literal" 1 (List.length (suggestions "%"));
  count "underscore is literal" 0 (List.length (suggestions "_"));
  count "SQL punctuation is literal" 0 (List.length (suggestions "' OR 1=1"));
  count "bounded suggestions" 1 (List.length (C.journals ~limit:1 c ~text:""));
  let candidate = List.hd (C.Metadata.pending ~force:true ~uid:id
    ~limit:1 ~version:1 c) in
  ignore (C.Metadata.publish c candidate ~version:1 (Error "unavailable"));
  count "failed enrichment retains journal" 1 (List.length (suggestions "%"));
  let exception Interrupted in
  (try ignore (C.Shard.replace c ~shard stamp (fun emit ->
    emit {row with journal = Some "Discarded"}; raise Interrupted))
   with Interrupted -> ());
  count "failed import retains journals" 2 (List.length (suggestions ""));
  C.Shard.retire c ~shard;
  count "retirement removes journals" 0 (List.length (suggestions "")))

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
  let storage : Taposaur_web.storage = {
    mirrors_file=None;abstracts_file=None;audit_file=None;archive_root="/host/archive";catalog_file="/host/catalog.db";
    reviews_file=None;jobs_file=None;journal_links_file=Some "/host/links<&>.jsonl";
    working_directory="/host/work";executable="/host/taposaur-web";
  } in
  let audit_write = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let session = Taposaur_audit.session ~component:"test.web" audit_write in
  Taposaur_audit.record session ~action:"test.event" ~subject:"<script>" ();
  let audit = Taposaur_audit.open_read ~sw (root / "audit.db") in
  let jobs = Duraml_memory.create ~sw ~clock:env#clock
    ~random:env#secure_random () in
  let queues = Taposaur_jobs.Queues.v jobs in
  let job = match Taposaur_jobs.Submit.entries queues
    [{doi = Uid.to_string id; publisher = "Elsevier <b>";
      decision = Acquire {uid = id; strategy = "elsevier"; revision = 1;
        publisher = "Elsevier <b>"; requested_by = "tester";
        review = Some {slug = "soil"; snapshot = 7}}}] with
    | _, [_, Some id] -> Duraml.Id.to_string id
    | _ -> Alcotest.fail "job not submitted" in
  let state = Taposaur_web.create ~audit ~storage ~jobs ~archive:a
    ~catalog:read () in
  let request ?headers ?(meth = Httpz.Method.Get) target =
    W.request ?headers ~on_error:(fun ex -> raise ex) Taposaur_web.site state meth target in
  let code r = Proffer.Status.code (W.status r) in
  count "home" 200 (code (request "/"));
  let jobs_page = request "/jobs" in
  count "jobs page" 200 (code jobs_page);
  check "jobs page lists the queue" true
    (includes (W.body jobs_page) "acquire.elsevier");
  check "jobs page links the paper" true
    (includes (W.body jobs_page) "/paper?uid=");
  check "jobs page links the review snapshot" true
    (includes (W.body jobs_page) "soil &#183; snapshot 7" ||
     includes (W.body jobs_page) "soil · snapshot 7");
  count "jobs filtered by state" 200 (code (request "/jobs?state=queued"));
  count "bad job state" 400 (code (request "/jobs?state=bogus"));
  count "bad job cursor" 400 (code (request "/jobs?after=x"));
  Taposaur_audit.with_operation ~session ~action:"jobs.acquire"
    ~subject:(Uid.to_string id) ~details:["job",job;"attempt","1"]
    ~result_details:(fun () -> ["outcome","retry";"note","busy:elsevier:429"])
    (fun scoped -> Taposaur_audit.record (Option.get scoped)
      ~action:"fetch.transfer.result" ~subject:(Uid.to_string id)
      ~details:["outcome","http_error";"http_status","429"] ());
  Taposaur_audit.record session ~action:"queue.event" ~subject:(Uid.to_string id)
    ~details:["job",job;"attempt","1";"kind","retry_scheduled";
      "detail","busy:elsevier:429"] ();
  let job_page = request ("/job?id=" ^ job) in
  count "job page" 200 (code job_page);
  check "job page nests the transfer result" true
    (includes (W.body job_page) "fetch.transfer.result" &&
     includes (W.body job_page) "http_status");
  check "job page explains the retry" true
    (includes (W.body job_page) "busy:elsevier:429");
  check "job page shows the queue timeline" true
    (includes (W.body job_page) "retry_scheduled");
  check "job page escapes payload" true
    (includes (W.body job_page) "Elsevier &lt;b&gt;");
  check "job page shows key" true (includes (W.body job_page)
    ("acquire:" ^ Uid.to_string id ^ ":elsevier:v1"));
  count "missing job" 404 (code (request "/job?id=999"));
  count "bad job id" 400 (code (request "/job?id=abc"));
  count "jobs still queued" 1 (List.length (Duraml.list jobs
    (Duraml.query ~state:Duraml.Queued ())));
  count "activity page" 200 (code (request "/activity"));
  let feed = request "/activity?format=json&snapshot=1" in
  count "activity snapshot" 200 (code feed);
  let body = W.body feed in
  check "feed records the jobs page" true (includes body {|"event":"jobs 200"|});
  check "feed summarises jobs" true (includes body {|"queue":"acquire.elsevier"|}
    && includes body {|"timeline":"enqueued |} && includes body {|"attempts":[]|}
    && includes body {|"review":"soil #7"|});
  check "feed names the paper" true (includes body
    ({|"uid":"|} ^ Uid.to_string id ^ {|"|}));
  check "feed counts queued work" true (includes body {|"counts":{"queued":1,"running":0}|});
  let wc, jc, bc = Scanf.sscanf body
    {|{"cursors":{"web":%d,"jobs":%Ld,"before":%Ld}|} (fun w j b -> w, j, b) in
  check "cursors advance" true (wc > 0 && jc > 0L && bc > 0L);
  let again = W.body (request (Printf.sprintf
    "/activity?format=json&web=%d&jobs=%Ld" wc jc)) in
  check "feed requests are not recorded" true (includes again {|"web":[]|});
  check "nothing new after cursors" true (includes again {|"jobs":[],"web":[]|});
  let old = W.body (request (Printf.sprintf
    "/activity?format=json&before=%Ld&web=%d" bc wc)) in
  check "older page ends the walk" true
    (includes old {|"jobs":[]|} && not (includes old {|"before"|}));
  count "bad activity cursor" 400 (code (request "/activity?format=json&web=x"));
  let audit_page = request "/audit?actor=system" in
  count "audit query page" 200 (code audit_page);
  check "audit subject escaped" true (includes (W.body audit_page) "&lt;script&gt;");
  let audit_json = request "/audit?format=jsonl&action=test.event" in
  count "audit JSONL query" 200 (code audit_json);
  check "audit JSONL event" true (includes (W.body audit_json) "test.event");
  count "bad audit cursor" 400 (code (request "/audit?before=bad"));
  count "bad audit phase" 400 (code (request "/audit?phase=bad"));
  let journals = request "/journals?q=birds" in
  count "journal suggestions" 200 (code journals);
  check "journal suggestions contain count" true
    (includes (W.body journals) {|"papers":1|});
  check "journal suggestions contain name" true
    (includes (W.body journals) "Journal of Birds");
  count "journal query bound" 400
    (code (request ("/journals?q=" ^ String.make 257 'x')));
  count "interface script" 200 (code (request "/assets/interface.js"));
  let results = request "/search?q=bird" in
  count "search" 200 (code results);
  check "title escaped" true (includes (W.body results) "&lt;script&gt;");
  let q = "?uid=" ^ Uriz.pct_encode ~component:`Unreserved (Uid.to_string id) in
  let debug = request ("/debug" ^ q ^ "&view=panel") in
  count "debug panel" 200 (code debug);
  check "debug includes configured catalogue" true
    (includes (W.body debug) "/host/catalog.db");
  check "debug escapes configured paths" true
    (includes (W.body debug) "/host/links&lt;&amp;&gt;.jsonl");
  check "debug includes current paper directory" true
    (includes (W.body debug) ("/host/archive/papers/" ^ Layout.paper id));
  check "debug lists publisher strategy" true
    (includes (W.body debug) "Cambridge University Press");
  check "debug clarifies CLI access" true
    (includes (W.body debug) "does not fetch papers or load publisher credentials");
  let preview = request ("/paper" ^ q) in
  count "paper preview" 200 (code preview);
  check "preview uses saved bibliography" true
    (includes (W.body preview) "Bird migration &lt;script&gt;");
  check "preview loads details progressively" true
    (includes (W.body preview) "data-details-url");
  let manifest_path = "papers/" ^ Layout.paper id ^ "/paper.json" in
  let manifest = Eio.Path.load (root / manifest_path) in
  Eio.Path.unlink (root / manifest_path);
  count "preview survives unavailable archive paper" 200
    (code (request ("/paper" ^ q)));
  count "full details require archive paper" 404
    (code (request ("/paper" ^ q ^ "&view=full")));
  save root manifest_path manifest;
  let paper = request ("/paper" ^ q ^ "&view=full") in
  count "paper" 200 (code paper);
  check "sidebar available" true (includes (W.body paper) {|id="paper-panel"|});
  check "history opens panel" true (includes (W.body paper) {|data-panel="history"|});
  let panel = request ("/read" ^ q ^ "&file=tei.xml&view=panel") in
  count "TEI panel" 200 (code panel);
  check "panel content" true (includes (W.body panel) "data-panel-content");
  check "panel omits page chrome" false (includes (W.body panel) "<header");
  check "panel escapes source" false (includes (W.body panel) "<script");
  let history = request ("/events" ^ q ^ "&view=panel") in
  count "history panel" 200 (code history);
  check "history fragment" true (includes (W.body history) "data-panel-content");
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
  let rich = W.body (request ("/paper" ^ q ^ "&view=full")) in
  check "index retained as alternative" true
    (includes rich "Bird migration &lt;script&gt;");
  save root ("index/" ^ shard) (index_json id);
  ignore (I.shard ~force:true c a ~shard);
  let sparse = W.body (request ("/paper" ^ q ^ "&view=full")) in
  List.iter (fun value -> check ("publisher fallback: " ^ value) true
    (includes sparse value))
    ["Recherche des accidents"; "Per-Olov Roosmark"; "Rolf Fräki";
     "Accident Analysis &amp; Prevention"; "1969-11-30"; "310"];
  let corrected = { Manifest.Correction.field = Manifest.Field.Title;
    value = "Corrected title"; by = "reviewer"; at = "now"; note = None } in
  save root manifest_path (get (Manifest.to_string
    { m with corrections = [corrected] }));
  let corrected_page = W.body (request ("/paper" ^ q ^ "&view=full")) in
  check "correction priority" true
    (includes corrected_page "<h1>Corrected title</h1>");
  save root manifest_path (get (Manifest.to_string m));
  let from_tei = W.body (request ("/paper" ^ q ^ "&view=full")) in
  check "TEI title fallback" true
    (includes from_tei "<h1>Bird migration</h1>");
  check "TEI authors fallback" true (includes from_tei "Ada Bird");
  save root (dir ^ "/tei.xml") "<broken>";
  count "bad header preserves paper page" 200
    (code (request ("/paper" ^ q ^ "&view=full"))))

let directory_enrichment () = with_temp (fun _ sw root ->
  let first = "doi/10.1000/00.jsonl" and second = "doi/10.1000/01.jsonl" in
  let rec collect n a b =
    if List.length a = 530 && List.length b = 3 then a, b else
      let id = uid ("doi:10.1000/order-" ^ string_of_int n) in
      let shard = Layout.index id in
      collect (n + 1)
        (if shard = first && List.length a < 530 then id :: a else a)
        (if shard = second && List.length b < 3 then id :: b else b) in
  let a, b = collect 0 [] [] in
  let c = C.open_write ~sw (root / "catalog.db") in
  List.iter (fun (shard, ids) ->
    ignore (C.Shard.replace c ~shard {size = 0L; mtime = 0.} (fun emit ->
      List.iter (fun id ->
        save root ("papers/" ^ Layout.paper id ^ "/paper.json")
          (manifest_json id);
        emit (get (Index.of_string (index_json id)))) ids)))
    [first, a; second, b];
  Alcotest.(check (list string)) "shard cursor" [second]
    (C.Shard.paths ~after:first ~limit:1 c);
  let rec page after acc =
    let rows = C.Metadata.pending ~shard:first ~after_row:after
      ~limit:73 ~version:P.version c in
    match List.rev rows with
    | [] -> acc
    | last :: _ -> page (C.Metadata.row_id last) (rows @ acc) in
  let rows = page 0L [] in
  count "all shard rows paged" 530 (List.length rows);
  count "no repeated rows" 530 (List.length (List.sort_uniq Int64.compare
    (List.map C.Metadata.row_id rows)));
  check "other shard excluded" true (List.for_all (fun row ->
    Layout.index (C.Metadata.index row).uid = first) rows);
  let archive = A.open_root ~sw root in
  let seen = ref [] in
  count "limit across 512-row batch" 513 (E.run ~jobs:8 ~limit:513
    ~progress:(fun id _ -> seen := id :: !seen) c archive).updated;
  check "finish first directory before second" true
    (List.for_all (fun id -> Layout.index id = first) !seen);
  count "resume only unfinished rows" 20 (E.run c archive).updated;
  count "all cached" 533 (C.Metadata.detail_count c);
  count "completed scan skips everything" 0 (E.run c archive).examined;
  count "single UID bypasses scan" 1
    (E.run ~force:true ~uid:(List.hd b) c archive).examined;
  seen := [];
  ignore (E.run ~force:true ~order:E.Uid ~jobs:1 ~limit:4
    ~progress:(fun id _ -> seen := id :: !seen) c archive);
  let expected = List.sort Uid.compare (a @ b)
    |> List.filteri (fun i _ -> i < 4) |> List.map Uid.to_string in
  Alcotest.(check (list string)) "UID order remains available" expected
    (List.rev_map Uid.to_string !seen))

let () = Alcotest.run "taposaur" [
  "core", [Alcotest.test_case "identities and layouts" `Quick identifiers;
    Alcotest.test_case "records" `Quick records;
    Alcotest.test_case "production fixtures" `Quick corpus_fixtures];
  "archive", [Alcotest.test_case "streaming" `Quick streaming;
    Alcotest.test_case "confined reads" `Quick archive_reads;
    Alcotest.test_case "confined compatible fingerprints" `Quick fingerprints];
  "document", [Alcotest.test_case "TEI parsing and rendering" `Quick documents;
    Alcotest.test_case "TEI regeneration evidence" `Quick tei_audit;
    Alcotest.test_case "publisher metadata" `Quick publisher_metadata;
    Alcotest.test_case "TEI bibliography and S2ORC" `Quick extended_metadata;
    Alcotest.test_case "abstracts left in TEI bodies" `Quick body_abstracts];
  "paper", [Alcotest.test_case "field evidence and multiple TEIs"
    `Quick resolution];
  "catalogue", [
    Alcotest.test_case "directory enrichment and resumption" `Quick directory_enrichment;
    Alcotest.test_case "persistent paper details" `Quick detail_cache;
    Alcotest.test_case "journal suggestions and publication" `Quick journal_selection;
    Alcotest.test_case "freshness and partial resolution" `Quick freshness;
    Alcotest.test_case "storage snapshots" `Quick storage_snapshots;
    Alcotest.test_case "schema migration" `Quick catalogue_migration;
    Alcotest.test_case "metadata enrichment" `Quick enrichment;
    Alcotest.test_case "transactional import and search" `Quick catalogue;
    Alcotest.test_case "incremental imports and paging" `Quick incremental];
  "web", [Alcotest.test_case "read-only routes" `Quick website]]
