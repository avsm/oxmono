open Taposaur
module J = Taposaur_jobs
module C = Duraml_client
module D = Duraml
module Accept = Taposaur_accept
module A = Taposaur_archive
module S = Taposaur_services
module R = Taposaur_review
module RS = Taposaur_review_store
module X = Taposaur_review_run
module Cat = Taposaur_catalog
let ( / ) = Eio.Path.( / )
let get = Tap_test.get
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let equal = Alcotest.(check string)
let includes hay needle =
  let n = String.length needle in
  let rec loop i = i + n <= String.length hay &&
    (String.sub hay i n = needle || loop (i + 1)) in
  loop 0
let uid = get (Uid.of_input "10.1000/x")
let at = "2026-11-03T12:00:00.000Z"
let pdf = "%PDF-1.4\n0123456789\n%%EOF\n"
let sha s = Digestif.SHA256.(to_hex (digest_string s))
let review = {J.Attribution.slug = "education"; snapshot = 1}
let tei ?(doi = "10.1000/x") ?(title = "Study") () = Printf.sprintf
  {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title level="a" type="main">%s</title></titleStmt><publicationStmt><publisher>Test Press</publisher></publicationStmt><sourceDesc><biblStruct><analytic><title>%s</title><idno type="DOI">%s</idno></analytic><monogr><imprint/></monogr></biblStruct></sourceDesc></fileDesc></teiHeader><text><body><p>Text</p></body></text></TEI>|}
  title title doi

let with_temp f = Tap_test.with_temp "taposaur-jobs-" f
let memory env sw =
  Duraml_memory.create ~sw ~clock:env#clock ~random:env#secure_random ()
let make_archive ~sw root =
  let dir = root / "archive" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (dir / "papers");
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (dir / "index");
  dir, Accept.open_root ~sw dir, A.open_root ~sw dir
let artifact ?(file = "publisher.pdf") ?(format = Manifest.Format.Pdf) data =
  {Manifest.Artifact.file; format; source = Publisher; version = None;
   converter = None; from = None; strategy = Some "test"; status = Some 200;
   bytes = Int64.of_int (String.length data); sha256 = Some (sha data); at;
   url = None; origin = None; confidence = None}
let hold archive ?file ?format uid data = Accept.accept archive ~cmd:"test" ~at
  uid [{source = Bytes data; artifact = artifact ?file ?format data}]
(* Runs one worker until the first settlement, including a retry that
   returns the job to the queue. *)
let run_one env queue handler =
  let stop, resolve = Eio.Promise.create () in
  C.Queue.run ~mono_clock:env#mono_clock ~concurrency:1 ~stop
    ~on_settled:(fun _ _ -> ignore (Eio.Promise.try_resolve resolve ()))
    queue handler
let added = function
  | C.Queue.Added job -> job
  | _ -> Alcotest.fail "Not added"
let jobs store queue = D.list store (D.query ~queue ())
let manifest reader uid = A.manifest (Option.get (A.find reader uid))
let roundtrip name codec value =
  let text = get (Jsont_bytesrw.encode_string codec value) in
  check name true (get (Jsont_bytesrw.decode_string codec text) = value)

let acquisition_request = {J.Acquisition.uid; strategy = "elsevier";
  revision = 2; publisher = "Elsevier Ltd"; review = Some review;
  requested_by = "tester"}
let input = {J.Artifact.file = "publisher.pdf"; format = Pdf;
  sha256 = sha pdf; bytes = Int64.of_int (String.length pdf)}

let codecs () =
  roundtrip "acquisition request" J.Acquisition.request_jsont
    acquisition_request;
  roundtrip "acquisition result" J.Acquisition.result_jsont
    {artifacts = [input]; attempts = ["elsevier", "HTTP 403"]};
  let conversion = {J.Conversion.uid; input; route = Grobid;
    converter = "grobid-1"} in
  roundtrip "conversion request" J.Conversion.request_jsont conversion;
  roundtrip "conversion result" J.Conversion.result_jsont
    {output = {input with file = "tei.xml"; format = Tei};
     publisher = Some "Test Press"};
  roundtrip "refresh request" J.Refresh.request_jsont
    {uid; manifest_sha256 = "abc"};
  roundtrip "refresh result" J.Refresh.result_jsont
    {index = "added"; catalogue = "updated"};
  equal "acquisition key" "acquire:doi:10.1000/x:elsevier:v2"
    (J.Acquisition.key acquisition_request);
  equal "conversion key" ("convert:doi:10.1000/x:grobid:" ^ sha pdf ^ ":grobid-1")
    (J.Conversion.key conversion);
  equal "refresh key" "refresh:doi:10.1000/x:abc"
    (J.Refresh.key {uid; manifest_sha256 = "abc"});
  equal "acquisition queue" "acquire.elsevier"
    (J.Acquisition.queue ~strategy:"elsevier");
  equal "conversion queue" "convert.pub2tei" (J.Conversion.queue Pub2tei);
  equal "refresh queue" "catalogue.refresh" J.Refresh.queue;
  count "every queue named" 10 (List.length J.Queues.names);
  let refused f = try ignore (f ()); false with Invalid_argument _ -> true in
  check "nonpositive revision refused" true (refused (fun () ->
    J.Acquisition.key {acquisition_request with revision = 0}));
  check "unknown strategy refused" true (refused (fun () ->
    J.Acquisition.key {acquisition_request with strategy = "ieee"}));
  check "s2orc route refused" true (refused (fun () ->
    J.Conversion.queue S2orc));
  check "invalid payload refused by decoder" true (Result.is_error
    (Jsont_bytesrw.decode_string J.Acquisition.request_jsont
      {|{"uid":"doi:10.1000/x","strategy":"ieee","revision":1,"publisher":"","requested_by":"x"}|}))

(* A review of three papers, with the Elsevier one already holding a PDF. *)
let reviewed ?raw sw root =
  let catalog = Cat.open_write ~sw (root / "catalog.db") in
  ignore catalog;
  let read = Cat.open_read ~sw (root / "catalog.db") in
  let store = RS.open_write ~sw (root / "reviews.db") in
  let raw = Option.value raw ~default:(String.concat "\n" [
    {|{"doi":"10.1000/a","publisher":"Elsevier Ltd"}|};
    {|{"doi":"10.1000/b","publisher":"Unknown Press"}|};
    {|{"doi":"10.1000/c","publisher":"Springer Nature"}|}] ^ "\n") in
  let source = {R.Definition.kind = List; format = Metadata_jsonl;
    blob = Some (RS.put_blob store raw); local = None; provider = None;
    query = None; parameters = None} in
  let definition = {R.Definition.version = 1;
    inputs = [{key = "main"; label = "main"; source}];
    include_any = ["main"]; require_all = []; exclude_any = []} in
  ignore (RS.define store ~slug:"education" ~title:"Education"
    ~requester:"Researcher" ~owner:"Librarian" ~actor:"tester"
    ~reason:"Test" ~expected:0 definition);
  let revision = RS.revision store ~slug:"education" ~number:None in
  let run = X.execute store read revision ~actor:"tester" [] in
  let snapshot = X.snapshot store read ~run_id:run ~parent:None ~label:"First"
    ~actor:"tester" in
  store, snapshot

let decisions entries =
  List.map (fun (e : J.Plan.entry) -> match e.decision with
    | J.Plan.Acquire r -> "acquire " ^ r.strategy
    | Convert r -> "convert " ^ Manifest.Route.to_string r.route
    | Held -> "held" | Refused s -> "refused " ^ s | Unsupported _ -> "unsupported"
    | Unconfigured s -> "unconfigured " ^ s | Invalid s -> "invalid " ^ s)
    entries
let strings = Alcotest.(check (list string))

let planning () = with_temp (fun _ sw root _ ->
  let store, snapshot = reviewed sw root in
  let _, archive, reader = make_archive ~sw root in
  let a = get (Uid.of_input "10.1000/a") in
  ignore (hold archive a pdf);
  let plan ?archive ~configured ~converters target =
    J.Plan.review ?archive ~configured ~converters ~target ~revision:1
      ~requested_by:"tester" store ~snapshot in
  let entries = plan ~archive:reader ~configured:["elsevier"; "springer"]
    ~converters:[] Fulltext in
  strings "fulltext decisions" ["held"; "unsupported"; "acquire springer"]
    (decisions entries);
  (match (List.nth entries 2).decision with
   | Acquire r ->
       check "review attribution" true (r.review = Some {slug = "education";
         snapshot});
       equal "publisher evidence" "Springer Nature" r.publisher
   | _ -> Alcotest.fail "Expected acquisition");
  let entries = plan ~archive:reader ~configured:["elsevier"] ~converters:[]
    Fulltext in
  strings "unconfigured strategy" ["held"; "unsupported"; "unconfigured springer"]
    (decisions entries);
  let s = J.Plan.summarize entries in
  check "summary" true (s = {total = 3; acquire = 0; convert = 0; held = 1; refused = 0;
    unsupported = 1; unconfigured = 1; invalid = 0});
  let entries = plan ~archive:reader ~configured:[] ~converters:[Grobid] Tei in
  equal "PDF converts with GROBID" "convert grobid"
    (List.hd (decisions entries));
  (match (List.hd entries).decision with
   | Convert r ->
       equal "converter identity" "grobid-1" r.converter;
       equal "input digest" (sha pdf) r.input.sha256
   | _ -> Alcotest.fail "Expected conversion");
  equal "unconfigured converter" "unconfigured grobid"
    (List.hd (decisions (plan ~archive:reader ~configured:[] ~converters:[] Tei)));
  equal "TEI planning needs the archive" "invalid archive required"
    (List.hd (decisions (plan ~configured:[] ~converters:[Grobid] Tei)));
  count "limit bounds rows" 2 (List.length (J.Plan.review ~limit:2
    ~configured:[] ~converters:[] ~target:Fulltext ~revision:1
    ~requested_by:"tester" store ~snapshot));
  (* The catalogue row for c records a PDF the archive does not hold, so
     planning with the catalogue trusts the cache and never opens it. *)
  let writer = Cat.open_write ~sw (root / "catalog.db") in
  let row = get (Index.of_string (Printf.sprintf
    {|{"uid":"doi:%s","scheme":"doi","pdf_path":"/x/c.pdf","pdf_size":1,"pdf_mtime":1,"tei_path":"","tei_mtime":0}|}
    "10.1000/c")) in
  ignore (Cat.Shard.replace writer ~shard:(Layout.index row.uid)
    {size = 1L; mtime = 1.} (fun emit -> emit row));
  let catalog = Cat.open_read ~sw (root / "catalog.db") in
  strings "catalogue holdings decide" ["unconfigured elsevier"; "unsupported";
    "held"]
    (decisions (J.Plan.review ~catalog ~archive:reader
      ~configured:["springer"] ~converters:[] ~target:Fulltext ~revision:1
      ~requested_by:"tester" store ~snapshot));
  strings "catalogue without archive" ["unconfigured elsevier"; "unsupported";
    "held"]
    (decisions (J.Plan.review ~catalog ~configured:[] ~converters:[]
      ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot));
  let encoded = get (Jsont_bytesrw.encode_string (Jsont.list J.Plan.entry_jsont)
    entries) in
  check "entries decode" true
    (get (Jsont_bytesrw.decode_string (Jsont.list J.Plan.entry_jsont) encoded)
     = entries))

(* Unindexed members with no publisher evidence route by DOI prefix. *)
let prefix_routing () = with_temp (fun _ sw root _ ->
  let store, snapshot = reviewed ~raw:(String.concat "\n" [
    {|{"doi":"10.1111/j.1365-2656.2001.00001.x"}|};
    {|{"doi":"10.2307/2404123"}|};
    {|{"doi":"10.1017/s0007485300000001","publisher":"Unknown Press"}|}] ^ "\n")
    sw root in
  let entries = J.Plan.review ~configured:["wiley"; "cup"] ~converters:[]
    ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot in
  (* Snapshot rows are planned in DOI order. *)
  strings "prefix decisions" ["unsupported"; "acquire wiley"; "unsupported"]
    (decisions entries);
  (match (List.nth entries 1).decision with
   | Acquire r -> equal "inferred publisher" "Wiley" r.publisher
   | _ -> Alcotest.fail "Expected acquisition");
  equal "entry keeps the evidence" "" (List.nth entries 1).publisher;
  equal "evidence wins over prefix" "Unknown Press"
    (List.hd entries).publisher;
  strings "unconfigured prefix strategy" ["unconfigured wiley"]
    [List.nth (decisions (J.Plan.review ~configured:[] ~converters:[]
      ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot)) 1])

let submission () = with_temp (fun env sw root _ ->
  let store, snapshot = reviewed sw root in
  let queues = J.Queues.v (memory env sw) in
  let plan = J.Plan.review ~configured:["elsevier"; "springer"] ~converters:[]
    ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot in
  let first, ids = J.Submit.entries queues plan in
  count "added" 2 first.added;
  count "skipped" 1 first.skipped;
  count "identifiers" 2 (List.length (List.filter_map snd ids));
  let second, again = J.Submit.entries queues plan in
  count "nothing added again" 0 second.added;
  count "existing" 2 second.existing;
  check "same jobs" true (List.map snd ids = List.map snd again);
  let lanes queues = List.map (fun (j : J.Inspect.job) -> j.lane)
    (J.Inspect.list (J.Queues.store queues)) in
  check "unnamed lane by default" true
    (List.for_all (fun l -> l = "") (lanes queues));
  let queues = J.Queues.v (memory env sw) in
  ignore (J.Submit.entries ~lane:"snapshot:1" queues plan);
  check "lane recorded on every job" true
    (lanes queues = ["snapshot:1"; "snapshot:1"]))

let lookup = "https://metadata.test/service/lookup"
let publisher_mock respond = Fetch_mock.client (fun request ->
  let url = Fetch.Middleware.Url.to_string request.Fetch.Middleware.url in
  if String.starts_with ~prefix:lookup url then Fetch_mock.respond
    {|{"DOI":"10.1000/x","publisher":"Elsevier Ltd","title":"Study"}|} request
  else if String.starts_with ~prefix:"https://metadata.test" url then
    Fetch_mock.respond "{}" request
  else if String.starts_with ~prefix:S.Elsevier.origin url then respond request
  else Fetch_mock.respond ~status:404 "" request)
let xml_body = "<article><title>Study</title></article>"
let wrapped_body = "<?xml version=\"1.0\"?><!DOCTYPE response [ <!ENTITY % article \
  SYSTEM \"http://jats.example/JATS.dtd\"> ]><response><apiMessage>Provided\
</apiMessage><records>" ^ xml_body ^ "</records></response>"
let xml_response request = Fetch_mock.respond
  ~headers:(Http.Header.of_list ["content-type", "application/xml"])
  xml_body request
let acquisition env sw root http =
  let staging = root / "staging" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 staging;
  let _, archive, reader = make_archive ~sw root in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let elsevier = S.Elsevier.client ~api_key:(fun () -> "key") http in
  {J.Worker.downloader = Taposaur_download.create ~clock:env#clock ~glutton
     ~elsevier ~busy_retries:0 http;
   reader; sw;
   staging_root = (staging :> Eio.Fs.dir_ty Eio.Path.t); archive;
   converters = [Grobid; Pub2tei];
   converter = (fun r -> Manifest.Route.to_string r ^ "-1");
   clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
   audit = None; cmd = "test"}, reader
let acquire env store config =
  let queues = J.Queues.v store in
  let queue = J.Queues.acquisition queues ~strategy:"elsevier" in
  let job = added (C.Queue.submit ~key:(J.Acquisition.key acquisition_request)
    queue acquisition_request) in
  run_one env queue (J.Worker.acquisition_handler config queues);
  job

(* Records written by the download, staging and acceptance libraries nest
   beneath the handler's operation, which names the job and attempt. *)
let audit_nesting () = with_temp (fun env sw root _ ->
  let db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let session = Taposaur_audit.session ~component:"test.jobs" db in
  let config, _ = acquisition env sw root (publisher_mock xml_response) in
  let config = {config with audit = Some session} in
  let job = acquire env (memory env sw) config in
  check "acquired" true (match C.Job.inspect job with C.Job.Done _ -> true | _ -> false);
  let id = D.Id.to_string (C.Job.id job) in
  let events = Taposaur_audit.query ~limit:500 db in
  let handler = List.find (fun (e : Taposaur_audit.event) ->
    e.action = "jobs.acquire" && e.phase = Started &&
    includes e.details ({|"job":"|} ^ id ^ {|"|})) events in
  check "attempt stamped" true (includes handler.details {|"attempt":"1"|});
  let result = List.find (fun (e : Taposaur_audit.event) ->
    e.operation = handler.operation && e.phase = Succeeded) events in
  check "outcome noted" true (includes result.details {|"outcome":"succeeded"|});
  let nested action = List.exists (fun (e : Taposaur_audit.event) ->
    e.action = action && e.parent = Some handler.operation) events in
  check "transfer nested" true (nested "fetch.paper");
  check "acceptance nested" true (nested "archive.accept"))

let acquisition_success () = with_temp (fun env sw root _ ->
  let config, reader = acquisition env sw root (publisher_mock xml_response) in
  let store = memory env sw in
  let job = acquire env store config in
  (match C.Job.inspect job with
   | Done r -> count "accepted artifact" 1 (List.length r.artifacts)
   | _ -> Alcotest.fail "Acquisition did not succeed");
  let dir = root / "archive" / "papers" / Layout.paper uid in
  check "publisher XML archived" true (Eio.Path.is_file (dir / "publisher.xml"));
  let m = manifest reader uid in
  let xml = List.find (fun (a : Manifest.Artifact.t) ->
    a.file = "publisher.xml") m.artifacts in
  check "digest recorded" true (xml.sha256 = Some (sha xml_body));
  equal "identity filled" "Elsevier Ltd" (Option.get m.publisher);
  let conversions = jobs store "convert.pub2tei" in
  count "conversion successor" 1 (List.length conversions);
  let expected = {J.Conversion.uid; route = Pub2tei; converter = "pub2tei-1";
    input = Option.get (J.Artifact.of_manifest xml)} in
  check "conversion key" true (D.Spec.key (D.Job.spec (List.hd conversions))
    = Some (J.Conversion.key expected));
  count "no GROBID successor without PDF" 0
    (List.length (jobs store "convert.grobid"));
  count "refresh successor" 1 (List.length (jobs store "catalogue.refresh"));
  let again = acquire env (memory env sw) config in
  check "repeat succeeds" true
    (match C.Job.inspect again with Done _ -> true | _ -> false);
  check "no duplicate file" false (Eio.Path.is_file (dir / "publisher-2.xml"));
  count "single archived artifact" 1
    (List.length (manifest reader uid).artifacts))

let acquisition_refusals () = with_temp (fun env sw root _ ->
  let busy request = Fetch_mock.respond ~status:429
    ~headers:(Http.Header.of_list ["retry-after", "45"]) "slow down" request in
  let config, _ = acquisition env sw root (publisher_mock busy) in
  let before = Eio.Time.now env#clock in
  let job = acquire env (memory env sw) config in
  let snapshot = Option.get (C.Job.snapshot job) in
  check "busy job requeued" true (D.Job.state snapshot = D.Queued);
  let delay = Ptime.to_float_s (D.Job.available_at snapshot) -. before in
  check "server delay honoured" true (delay > 43. && delay < 47.);
  check "busy reason" true (match D.Job.outcome snapshot with
    | Some (Retry {reason; _}) -> String.starts_with ~prefix:"busy:" reason
    | _ -> false);
  let denied request = Fetch_mock.respond ~status:403 "denied" request in
  let config, _ = acquisition env sw root (publisher_mock denied) in
  let job = acquire env (memory env sw) config in
  check "refusal is permanent" true (match C.Job.inspect job with
    | Failed {code = "http_403"; _} -> true | _ -> false))

let grobid_mock response =
  let http = Fetch_mock.client (fun request -> !response request) in
  S.Grobid.client ~server:"http://grobid.test" http
let tei_response body request = Fetch_mock.respond
  ~headers:(Http.Header.of_list ["content-type", "application/xml"])
  body request
let conversion_config env reader archive grobid =
  {J.Worker.reader; archive; grobid = Some grobid; pub2tei = None;
   timeout = 10.; max_bytes = 1024 * 1024;
   clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
   mono_clock = (env#mono_clock :> Eio.Time.Mono.ty Eio.Resource.t);
   audit = None; cmd = "test"}
let convert_request env store config (request : J.Conversion.request) =
  let queues = J.Queues.v store in
  let queue = J.Queues.conversion queues request.route in
  let job = added (C.Queue.submit ~key:(J.Conversion.key request) queue
    request) in
  run_one env queue (J.Worker.conversion_handler config queues);
  job
let convert env store config converter =
  convert_request env store config {J.Conversion.uid; input; route = Grobid;
    converter}

let conversion () = with_temp (fun env sw root _ ->
  let _, archive, reader = make_archive ~sw root in
  ignore (hold archive uid pdf);
  let response = ref (tei_response (tei ())) in
  let config = conversion_config env reader archive (grobid_mock response) in
  let store = memory env sw in
  let job = convert env store config "grobid-1" in
  (match C.Job.inspect job with
   | Done r ->
       equal "TEI output" "tei.xml" r.output.file;
       check "publisher from TEI" true (r.publisher = Some "Test Press")
   | _ -> Alcotest.fail "Conversion did not succeed");
  let tei_record = List.find (fun (a : Manifest.Artifact.t) ->
    a.file = "tei.xml") (manifest reader uid).artifacts in
  check "converter recorded" true (tei_record.converter = Some "grobid");
  check "input recorded" true (tei_record.from = Some "publisher.pdf");
  count "refresh successor" 1 (List.length (jobs store "catalogue.refresh"));
  response := (fun request -> Fetch_mock.respond ~status:503 "busy" request);
  let job = convert env store config "grobid-2" in
  check "busy converter retries" true (C.Job.inspect job = C.Job.Queued);
  response := (fun request -> Fetch_mock.respond ~status:500
    ~headers:(Http.Header.of_list ["content-type", "text/plain"])
    "java.lang.NullPointerException" request);
  let job = convert env store config "grobid-3" in
  check "converter error permanent" true (match C.Job.inspect job with
    | Failed {code = "converter_error"; detail} ->
        not (includes detail "NullPointer")
    | _ -> false);
  check "failure recorded" true (List.exists (fun (f : Manifest.Failure.t) ->
    f.route = Grobid && f.permanent) (manifest reader uid).conversions_failed);
  response := tei_response (tei ~doi:"10.1000/other" ());
  let job = convert env store config "grobid-4" in
  check "identity mismatch" true (match C.Job.inspect job with
    | Failed {code = "identity_mismatch"; _} -> true | _ -> false);
  let dir = root / "archive" / "papers" / Layout.paper uid in
  let original = Eio.Path.load (dir / "tei.xml") in
  response := tei_response (tei ~title:"Revised" ());
  let job = convert env store config "grobid-5" in
  check "revised conversion" true (match C.Job.inspect job with
    | Done r -> r.output.file = "tei-2.xml" | _ -> false);
  equal "existing TEI unchanged" original (Eio.Path.load (dir / "tei.xml"));
  check "success clears failure" true
    ((manifest reader uid).conversions_failed = []))

let refresh () = with_temp (fun env sw root _ ->
  let dir, archive, reader = make_archive ~sw root in
  ignore (hold archive uid pdf);
  let receipt = Accept.accept archive ~cmd:"test" ~at uid
    [{source = Bytes (tei ()); artifact = {(artifact ~file:"tei.xml"
      ~format:Tei (tei ())) with source = Tei; converter = Some "grobid";
      from = Some "publisher.pdf"}}] in
  let catalog = Cat.open_write ~sw (root / "catalog.db") in
  let config = {J.Worker.reader; archive; catalog;
    publisher_lookup = (fun _ -> None); abstract_lookup = (fun _ -> None);
    clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t); audit = None} in
  let store = memory env sw in
  let queues = J.Queues.v store in
  let request = {J.Refresh.uid; manifest_sha256 = receipt.manifest_sha256} in
  let job = added (C.Queue.submit ~key:(J.Refresh.key request)
    (J.Queues.refresh queues) request) in
  run_one env (J.Queues.refresh queues) (J.Worker.refresh_handler config queues);
  (match C.Job.inspect job with
   | Done r -> equal "index row added" "added" r.index
   | Failed {code; detail} -> Alcotest.fail (code ^ ": " ^ detail)
   | _ -> Alcotest.fail "Refresh did not succeed");
  check "index shard written" true
    (Eio.Path.is_file (dir / "index" / Layout.index uid));
  (match Cat.find catalog uid with
   | None -> Alcotest.fail "Paper missing from catalogue"
   | Some summary ->
       check "PDF held" true summary.holdings.pdf;
       check "TEI held" true summary.holdings.tei);
  (* A later publication changes one row in a short transaction, keeps
     the shard stamp current and leaves other rows alone. *)
  let shard = Layout.index uid in
  let size, mtime = A.shard_stat reader shard in
  check "stamp current after import" true
    (Cat.Shard.unchanged catalog ~shard {size; mtime});
  let receipt = Accept.accept archive ~cmd:"test" ~at uid
    [{source = Bytes "<article/>"; artifact = artifact ~file:"publisher.xml"
      ~format:Xml "<article/>"}] in
  check "index row updated" true
    (Accept.refresh_index archive uid = Updated);
  let row = Option.get (Accept.index_row archive uid) in
  let size, mtime = A.shard_stat reader shard in
  check "single row published" true
    (Cat.Shard.upsert catalog ~shard {size; mtime} row = `Updated);
  check "repeat is unchanged" true
    (Cat.Shard.upsert catalog ~shard {size; mtime} row = `Unchanged);
  check "stamp current after upsert" true
    (Cat.Shard.unchanged catalog ~shard {size; mtime});
  check "XML now held" true (match Cat.find catalog uid with
    | Some s -> s.holdings.xml && s.holdings.pdf && s.holdings.tei
    | None -> false);
  count "shard counts follow" 1 (Cat.stats catalog).xml;
  check "unimported shard refused" true (try
    ignore (Cat.Shard.upsert catalog ~shard:"doi/10.9999/zz.jsonl" {size; mtime}
      {row with uid = get (Uid.of_input "10.9999/zz")}); false
    with Invalid_argument _ -> true);
  let request = {J.Refresh.uid; manifest_sha256 = receipt.manifest_sha256} in
  let job = added (C.Queue.submit ~key:(J.Refresh.key request)
    (J.Queues.refresh queues) request) in
  run_one env (J.Queues.refresh queues) (J.Worker.refresh_handler config queues);
  check "second refresh succeeds through upsert" true
    (match C.Job.inspect job with Done r -> r.index = "unchanged" | _ -> false))

let submitted env sw =
  let store = memory env sw in
  let queues = J.Queues.v store in
  let queue = J.Queues.acquisition queues ~strategy:"elsevier" in
  let first = added (C.Queue.submit ~key:(J.Acquisition.key
    acquisition_request) queue acquisition_request) in
  let other = {acquisition_request with revision = 3} in
  let second = added (C.Queue.submit ~key:(J.Acquisition.key other) queue
    other) in
  ignore (C.Job.cancel second);
  ignore (D.enqueue store (D.Spec.v ~queue:"catalogue.refresh" ~kind:"refresh"
    ~version:1 ~payload:"not json" ()));
  store, first

let audit_bridge () = with_temp (fun env sw root path ->
  let store, _ = submitted env sw in
  let audit_db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let session = Taposaur_audit.session ~component:"test.jobs" audit_db in
  let cursor = Filename.concat path "cursor.json" in
  let events = List.length (D.events store) in
  count "first delivery" events (J.Audit_bridge.deliver ~cursor store session);
  count "second delivery" 0 (J.Audit_bridge.deliver ~cursor store session);
  let recorded () = Taposaur_audit.query ~action:"queue.event" ~limit:500
    audit_db in
  count "audit records" events (List.length (recorded ()));
  Sys.remove cursor;
  count "lost cursor delivers nothing" 0
    (J.Audit_bridge.deliver ~cursor store session);
  count "no repeated records" events (List.length (recorded ()));
  check "cursor names the store" true (includes
    (In_channel.with_open_bin cursor In_channel.input_all) (D.identity store));
  let attributed = List.filter (fun (e : Taposaur_audit.event) ->
    e.subject = "doi:10.1000/x") (recorded ()) in
  check "paper attribution" true (attributed <> []);
  check "review attribution" true (List.for_all (fun (e : Taposaur_audit.event) ->
    includes e.details {|"review_slug":"education"|}) attributed);
  check "undecodable payload attributed to job" true (List.exists
    (fun (e : Taposaur_audit.event) -> String.starts_with ~prefix:"job:"
      e.subject) (recorded ()));
  let stop, resolve = Eio.Promise.create () in
  Eio.Promise.resolve resolve ();
  J.Audit_bridge.run ~mono_clock:env#mono_clock ~stop ~cursor store session)

let inspection () = with_temp (fun env sw _ _ ->
  let store, first = submitted env sw in
  let counts = J.Inspect.counts store in
  let elsevier = List.find (fun (c : J.Inspect.counts) ->
    c.queue = "acquire.elsevier") counts in
  count "queued" 1 elsevier.queued;
  count "cancelled" 1 elsevier.cancelled;
  count "refresh queued" 1 (List.find (fun (c : J.Inspect.counts) ->
    c.queue = "catalogue.refresh") counts).queued;
  let job = Option.get (J.Inspect.find store (C.Job.id first)) in
  equal "queue" "acquire.elsevier" job.queue;
  equal "state" "queued" job.state;
  check "paper" true (job.uid = Some "doi:10.1000/x");
  check "review" true (job.review = Some review);
  check "key" true (job.key = Some (J.Acquisition.key acquisition_request));
  check "paper URL" true
    (J.Inspect.paper_url job = Some "/paper?uid=doi%3A10.1000%2Fx");
  let undecodable = List.hd (J.Inspect.list ~queue:"catalogue.refresh" store) in
  check "undecodable payload unattributed" true (undecodable.uid = None);
  count "state filter" 1 (List.length (J.Inspect.list
    ~state:D.Cancelled store)))

let paper_json root =
  Eio.Path.load (root / "archive" / "papers" / Layout.paper uid / "paper.json")
let refresh_key root = Some (J.Refresh.key {uid;
  manifest_sha256 = sha (paper_json root)})
let key_of job = D.Spec.key (D.Job.spec job)
let is_done job = match C.Job.inspect job with Done _ -> true | _ -> false
let retry_reason job = match D.Job.outcome (Option.get (C.Job.snapshot job))
  with Some (Retry {reason; _}) -> reason | _ -> ""

let conversion_replay () = with_temp (fun env sw root _ ->
  let _, archive, reader = make_archive ~sw root in
  ignore (hold archive uid pdf);
  let response = ref (tei_response (tei ())) in
  let config = conversion_config env reader archive (grobid_mock response) in
  check "first conversion" true
    (is_done (convert env (memory env sw) config "grobid-1"));
  let calls = ref 0 in
  response := (fun request -> incr calls;
    Fetch_mock.respond ~status:500 "unexpected" request);
  let store = memory env sw in
  let job = convert env store config "grobid-1" in
  count "converter not called" 0 !calls;
  let recorded = List.find (fun (a : Manifest.Artifact.t) ->
    a.file = "tei.xml") (manifest reader uid).artifacts in
  (match C.Job.inspect job with
   | Done r -> check "recorded TEI reused" true
       (Some r.output = J.Artifact.of_manifest recorded)
   | _ -> Alcotest.fail "Replay did not succeed");
  let dir = root / "archive" / "papers" / Layout.paper uid in
  check "no second TEI" false (Eio.Path.is_file (dir / "tei-2.xml"));
  match jobs store "catalogue.refresh" with
  | [r] -> check "refresh names the manifest" true
      (key_of r = refresh_key root)
  | _ -> Alcotest.fail "One refresh successor expected")

let acquisition_replay () = with_temp (fun env sw root _ ->
  let config, reader = acquisition env sw root (publisher_mock xml_response) in
  check "first acquisition" true (is_done (acquire env (memory env sw) config));
  let requests = ref 0 in
  let config, _ = acquisition env sw root (Fetch_mock.client (fun request ->
    incr requests; Fetch_mock.respond ~status:500 "unexpected" request)) in
  let store = memory env sw in
  let job = acquire env store config in
  count "no request" 0 !requests;
  let xml = List.find (fun (a : Manifest.Artifact.t) ->
    a.file = "publisher.xml") (manifest reader uid).artifacts in
  (match C.Job.inspect job with
   | Done r -> check "held artifact listed" true
       (r.artifacts = Option.to_list (J.Artifact.of_manifest xml))
   | _ -> Alcotest.fail "Replay did not succeed");
  let dir = root / "archive" / "papers" / Layout.paper uid in
  check "no second XML" false (Eio.Path.is_file (dir / "publisher-2.xml"));
  count "conversion successor" 1 (List.length (jobs store "convert.pub2tei"));
  match jobs store "catalogue.refresh" with
  | [r] -> check "refresh names the manifest" true
      (key_of r = refresh_key root)
  | _ -> Alcotest.fail "One refresh successor expected")

let shared_successors () = with_temp (fun env sw root _ ->
  let config, reader = acquisition env sw root (publisher_mock xml_response) in
  let store = memory env sw in
  let queues = J.Queues.v store in
  let queue = J.Queues.acquisition queues ~strategy:"elsevier" in
  let submit revision review =
    let r = {acquisition_request with revision; review = Some review} in
    added (C.Queue.submit ~key:(J.Acquisition.key r) queue r) in
  let first = submit 1 {slug = "education"; snapshot = 1} in
  let second = submit 2 {slug = "biology"; snapshot = 4} in
  run_one env queue (J.Worker.acquisition_handler config queues);
  run_one env queue (J.Worker.acquisition_handler config queues);
  check "first done" true (is_done first);
  check "second done" true (is_done second);
  let xml = List.find (fun (a : Manifest.Artifact.t) ->
    a.file = "publisher.xml") (manifest reader uid).artifacts in
  let expected = {J.Conversion.uid; route = Pub2tei; converter = "pub2tei-1";
    input = Option.get (J.Artifact.of_manifest xml)} in
  match jobs store "convert.pub2tei" with
  | [c] -> check "conversion key" true
      (key_of c = Some (J.Conversion.key expected));
      check "payload has no attribution" false
        (includes (D.Spec.payload (D.Job.spec c)) "education" ||
         includes (D.Spec.payload (D.Job.spec c)) "biology");
      check "successor records its first parent" true
        (D.Job.parent c = Some (C.Job.id first));
      check "describe reports the parent" true
        ((J.Inspect.describe c).parent
          = Some (D.Id.to_string (C.Job.id first)));
      check "children follow the lineage" true
        (List.exists (fun (j : J.Inspect.job) -> j.queue = "convert.pub2tei")
          (J.Inspect.children store (C.Job.id first)));
      check "reconciled successor keeps no second parent" true
        (J.Inspect.children store (C.Job.id second)
         |> List.for_all (fun (j : J.Inspect.job) -> j.queue <> "convert.pub2tei"))
  | l -> Alcotest.failf "One conversion expected, found %d" (List.length l))

(* A permanent refusal leaves a marker in the archive, which planning with
   skip_refused honours, and a later acquisition through that strategy
   clears it. *)
let refusal_marker () = with_temp (fun env sw root _ ->
  let denied request = Fetch_mock.respond ~status:403 "denied" request in
  let config, reader = acquisition env sw root (publisher_mock denied) in
  let job = acquire env (memory env sw) config in
  check "refused job failed" true (match C.Job.inspect job with
    | C.Job.Failed {code; _} -> code = "http_403" | _ -> false);
  let m = manifest reader uid in
  check "archive created without artifacts" true (m.artifacts = []);
  (match m.acquisitions_failed with
   | [r] -> equal "refusal strategy" "elsevier" r.strategy;
       check "refusal status" true (r.status = Some 403);
       check "refusal permanent" true r.permanent;
       check "refusal reason" true (includes r.reason "HTTP 403")
   | _ -> Alcotest.fail "One refusal expected");
  let again = acquire env (memory env sw) config in
  check "second refusal counted" true (match C.Job.inspect again with
    | C.Job.Failed _ -> (List.hd (manifest reader uid).acquisitions_failed).attempts = 2
    | _ -> false);
  let store, snapshot = reviewed ~raw:(Printf.sprintf
    {|{"doi":"%s","publisher":"Elsevier Ltd"}|} (Uid.id uid) ^ "\n") sw root in
  let decisions = List.map (fun (e : J.Plan.entry) -> match e.decision with
    | Refused _ -> "refused" | Acquire _ -> "acquire" | _ -> "other")
    (J.Plan.review ~archive:reader ~skip_refused:true ~configured:["elsevier"]
      ~converters:[] ~target:Fulltext ~revision:1 ~requested_by:"tester"
      store ~snapshot) in
  equal "refused member skipped" "refused" (List.hd decisions);
  equal "refusal ignored without the flag" "acquire" (List.hd (List.map
    (fun (e : J.Plan.entry) -> match e.decision with
      | Acquire _ -> "acquire" | _ -> "other")
    (J.Plan.review ~archive:reader ~configured:["elsevier"] ~converters:[]
      ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot)));
  let config, _ = acquisition env sw root (publisher_mock xml_response) in
  let config = {config with reader} in
  ignore (acquire env (memory env sw) config);
  check "success clears the refusal" true
    ((manifest reader uid).acquisitions_failed = []))

(* Springer's response envelope is reduced to its article before Pub2TEI
   sees it, and an envelope without an article is a permanent failure. *)
let envelope () = with_temp (fun env sw root _ ->
  let seen = ref "" in
  let pub2tei = Fetch_mock.client (fun request ->
    (match request.body with String s -> seen := s | _ -> ());
    tei_response (tei ()) request) in
  let _, archive, reader = make_archive ~sw root in
  let wrapped = wrapped_body in
  ignore (hold archive uid ~file:"publisher.xml" ~format:Xml wrapped);
  let config = {(conversion_config env reader archive
      (S.Grobid.client ~server:"http://grobid.test" pub2tei)) with
    grobid = None; pub2tei = Some (S.Pub2tei.client ~server:"http://pub2tei.test"
      pub2tei)} in
  let input = {J.Artifact.file = "publisher.xml"; format = Xml;
    sha256 = sha wrapped; bytes = Int64.of_int (String.length wrapped)} in
  let queues = J.Queues.v (memory env sw) in
  let queue = J.Queues.conversion queues Pub2tei in
  let request = {J.Conversion.uid; input; route = Pub2tei;
    converter = "pub2tei-1"} in
  let job = added (C.Queue.submit ~key:(J.Conversion.key request) queue request) in
  run_one env queue (J.Worker.conversion_handler config queues);
  check "converted" true (match C.Job.inspect job with
    | C.Job.Done _ -> true | _ -> false);
  check "converter saw the bare article" true
    (includes !seen "<article><title>Study</title></article>" &&
     not (includes !seen "<response>"));
  check "pure unwrap" true
    (Taposaur_document.Publisher_xml.unwrap wrapped
      = Ok ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^ xml_body));
  check "bare input untouched" true
    (Taposaur_document.Publisher_xml.unwrap xml_body = Ok xml_body);
  (* Pub2TEI output from the paper's own XML may carry no DOI. *)
  let plain = {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title level="a" type="main">Study</title></titleStmt><publicationStmt><publisher>Test Press</publisher></publicationStmt><sourceDesc><biblStruct><analytic><title>Study</title></analytic><monogr><imprint/></monogr></biblStruct></sourceDesc></fileDesc></teiHeader><text><body><p>Text</p></body></text></TEI>|} in
  let headerless = Fetch_mock.client (fun request -> tei_response plain request) in
  let config = {config with pub2tei = Some (S.Pub2tei.client
    ~server:"http://pub2tei.test" headerless)} in
  let request = {request with converter = "pub2tei-2"} in
  let job = added (C.Queue.submit ~key:(J.Conversion.key request) queue request) in
  run_one env queue (J.Worker.conversion_handler config queues);
  check "headerless Pub2TEI output accepted" true (match C.Job.inspect job with
    | C.Job.Done _ -> true | _ -> false);
  (* A conversion whose body holds no prose is refused rather than held
     as full text. *)
  let hollow = Fetch_mock.client (fun request -> tei_response
    {|<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt><title level="a" type="main">Study</title></titleStmt><publicationStmt><publisher>Test Press</publisher></publicationStmt><sourceDesc><biblStruct><analytic><title>Study</title></analytic><monogr><imprint/></monogr></biblStruct></sourceDesc></fileDesc></teiHeader><text><body><div><p/></div></body></text></TEI>|}
    request) in
  let config = {config with pub2tei = Some (S.Pub2tei.client
    ~server:"http://pub2tei.test" hollow)} in
  let request = {request with converter = "pub2tei-3"} in
  let job = added (C.Queue.submit ~key:(J.Conversion.key request) queue request) in
  run_one env queue (J.Worker.conversion_handler config queues);
  check "empty body refused" true (match C.Job.inspect job with
    | C.Job.Failed {code = "empty_body"; _} -> true | _ -> false);
  check "empty envelope refused" true
    (Result.is_error (Taposaur_document.Publisher_xml.unwrap
      "<response><records/></response>")))

let retry_clamp () = with_temp (fun env sw root _ ->
  let delay after =
    let busy request = Fetch_mock.respond ~status:429
      ~headers:(Http.Header.of_list ["retry-after", after]) "slow" request in
    let config, _ = acquisition env sw root (publisher_mock busy) in
    let before = Eio.Time.now env#clock in
    let job = acquire env (memory env sw) config in
    let snapshot = Option.get (C.Job.snapshot job) in
    check ("requeued " ^ after) true (D.Job.state snapshot = D.Queued);
    Ptime.to_float_s (D.Job.available_at snapshot) -. before in
  let short = delay "1" in
  check (Printf.sprintf "short delay raised (%.1f)" short) true (short >= 29.);
  let long = delay "999999" in
  check (Printf.sprintf "long delay capped (%.1f)" long) true (long <= 86401.))

let converter_busy () = with_temp (fun env sw root _ ->
  let _, archive, reader = make_archive ~sw root in
  ignore (hold archive uid pdf);
  let response = ref (tei_response (tei ())) in
  let config = conversion_config env reader archive (grobid_mock response) in
  let store = memory env sw in
  List.iter (fun status ->
    response := (fun request -> Fetch_mock.respond ~status "busy" request);
    let job = convert env store config ("grobid-" ^ string_of_int status) in
    check "requeued" true (C.Job.inspect job = C.Job.Queued);
    equal "retry reason" ("busy:" ^ string_of_int status) (retry_reason job))
    [502; 429];
  check "no failure recorded" true
    ((manifest reader uid).conversions_failed = []))

let quoted_input () = with_temp (fun env sw root _ ->
  let _, archive, reader = make_archive ~sw root in
  let file = "pub\"lisher.pdf" in
  ignore (hold archive ~file uid pdf);
  let calls = ref 0 and body = ref "" in
  let response = ref (fun (request : Fetch.Middleware.request) ->
    incr calls;
    (match request.body with String s -> body := s | _ -> ());
    tei_response (tei ()) request) in
  let config = conversion_config env reader archive (grobid_mock response) in
  let job = convert_request env (memory env sw) config
    {J.Conversion.uid; input = {input with file}; route = Grobid;
     converter = "grobid-1"} in
  count "converter called" 1 !calls;
  check "fixed multipart filename" true
    (includes !body {|filename="input.pdf"|} && not (includes !body file));
  match C.Job.inspect job with
  | Done r -> equal "TEI output" "tei.xml" r.output.file
  | Failed {code; detail} -> Alcotest.fail (code ^ ": " ^ detail)
  | _ -> Alcotest.fail "Conversion did not succeed")

let audit_lock () = with_temp (fun env sw root path ->
  let store, _ = submitted env sw in
  let audit_db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let session = Taposaur_audit.session ~component:"test.jobs" audit_db in
  let cursor = Filename.concat path "cursor.json" in
  let held = Taposaur_lock.acquire (cursor ^ ".lock") in
  Fun.protect ~finally:(fun () -> Taposaur_lock.release held) (fun () ->
    check "concurrent delivery refused" true
      (try ignore (J.Audit_bridge.deliver ~cursor store session); false
       with Taposaur_lock.Busy _ -> true));
  check "no cursor written" false (Sys.file_exists cursor))

let counting () = with_temp (fun env sw _ _ ->
  let store = memory env sw in
  let queues = J.Queues.v store in
  let queue = J.Queues.refresh queues in
  List.iter (fun manifest_sha256 ->
    let r = {J.Refresh.uid; manifest_sha256} in
    ignore (added (C.Queue.submit ~key:(J.Refresh.key r) queue r)))
    ["a"; "b"];
  run_one env queue (fun ~sw:_ _ ->
    C.Completion.succeed {J.Refresh.index = "added"; catalogue = "updated"});
  let counts = J.Inspect.counts store in
  count "every queue counted" (List.length J.Queues.names) (List.length counts);
  List.iter (fun (c : J.Inspect.counts) ->
    let queued, done_ = if c.queue = J.Refresh.queue then 1, 1 else 0, 0 in
    count (c.queue ^ " queued") queued c.queued;
    count (c.queue ^ " done") done_ c.done_;
    count (c.queue ^ " running") 0 c.running;
    count (c.queue ^ " failed") 0 c.failed;
    count (c.queue ^ " cancelled") 0 c.cancelled) counts;
  List.iter (fun s -> check (D.state_name s) true
    (J.Inspect.state_of_string (D.state_name s) = Some s))
    [D.Queued; Running; Done; Failed_job; Cancelled];
  check "unknown state" true (J.Inspect.state_of_string "bogus" = None))

let long_key () = with_temp (fun _ sw root _ ->
  let doi = "10.1000/" ^ String.make 240 'a' in
  check "uid accepted" true (Result.is_ok (Uid.of_input doi));
  let raw = Printf.sprintf {|{"doi":"%s","publisher":"Elsevier Ltd"}|} doi
    ^ "\n" in
  let store, snapshot = reviewed ~raw sw root in
  let entries = J.Plan.review ~configured:["elsevier"] ~converters:[]
    ~target:Fulltext ~revision:1 ~requested_by:"tester" store ~snapshot in
  strings "invalid key" ["invalid Job key exceeds 256 bytes"]
    (decisions entries))

let submit_conflict () = with_temp (fun env sw _ _ ->
  let queues = J.Queues.v (memory env sw) in
  let entry r = {J.Plan.doi = "10.1000/x"; publisher = "Elsevier Ltd";
    decision = Acquire r} in
  let first, ids = J.Submit.entries queues [entry acquisition_request] in
  count "added" 1 first.added;
  let second, again = J.Submit.entries queues
    [entry {acquisition_request with requested_by = "other"}] in
  count "conflict" 1 second.conflicts;
  count "nothing added" 0 second.added;
  count "not existing" 0 second.existing;
  check "existing id returned" true
    (List.map snd again = List.map snd ids && List.map snd ids <> [None]))

let () = Alcotest.run "taposaur-jobs" [
  "payloads", [Alcotest.test_case "codecs and keys" `Quick codecs];
  "planning", [
    Alcotest.test_case "review decisions" `Quick planning;
    Alcotest.test_case "prefix routing" `Quick prefix_routing;
    Alcotest.test_case "submission reconciles" `Quick submission;
    Alcotest.test_case "long key invalid" `Quick long_key;
    Alcotest.test_case "submission conflict" `Quick submit_conflict];
  "workers", [
    Alcotest.test_case "acquisition success" `Quick acquisition_success;
    Alcotest.test_case "audit nesting" `Quick audit_nesting;
    Alcotest.test_case "refusal marker" `Quick refusal_marker;
    Alcotest.test_case "publisher envelope" `Quick envelope;
    Alcotest.test_case "acquisition refusals" `Quick acquisition_refusals;
    Alcotest.test_case "conversion" `Quick conversion;
    Alcotest.test_case "catalogue refresh" `Quick refresh;
    Alcotest.test_case "conversion replay" `Quick conversion_replay;
    Alcotest.test_case "acquisition replay" `Quick acquisition_replay;
    Alcotest.test_case "shared successors" `Quick shared_successors;
    Alcotest.test_case "retry clamp" `Quick retry_clamp;
    Alcotest.test_case "converter busy" `Quick converter_busy;
    Alcotest.test_case "quoted input name" `Quick quoted_input];
  "inspection", [
    Alcotest.test_case "audit bridge" `Quick audit_bridge;
    Alcotest.test_case "queue inspection" `Quick inspection;
    Alcotest.test_case "audit bridge lock" `Quick audit_lock;
    Alcotest.test_case "state counts" `Quick counting]]
