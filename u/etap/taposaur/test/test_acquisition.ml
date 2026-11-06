open Taposaur
module Store = Taposaur_store
module D = Taposaur_download
module S = Taposaur_services
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> Alcotest.fail e
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let equal = Alcotest.(check string)
let uid = get (Uid.of_input "10.1000/staged/paper")
let pdf = "%PDF-1.4\n0123456789\n%%EOF\n"
let at = "2026-10-23T12:00:00.000Z"
let with_temp f = Eio_main.run (fun env ->
  let name = Filename.temp_file "taposaur-acquisition-" "" in
  Unix.unlink name;
  let root = env#fs / name in
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () -> Eio.Switch.run (fun sw -> f env sw root)))
let save path value = Eio.Path.save ~create:(`Or_truncate 0o600) path value
let add paper data = Store.add paper ~at ~source:Manifest.Source.Url
  ~format:Manifest.Format.Pdf ~strategy:"url" ~status:200
  ~url:"https://example.org/paper.pdf" ~validate:(fun _ -> ())
  (Eio.Flow.string_source data)
let lines path = String.split_on_char '\n' (Eio.Path.load path)
  |> List.filter ((<>) "")

let publication () = with_temp (fun _ sw root ->
  let store = Store.open_root ~sw root in
  let second_handle = Store.open_root ~sw root in
  let dir = root / Layout.paper uid in
  Store.with_paper store uid (fun p ->
    check "same-process second writer refused" true
      (try Store.with_paper second_handle uid (fun _ -> ()); false
       with Store.Busy -> true);
    equal "first filename" "url.pdf" (add p pdf).file;
    equal "identical retry" "url.pdf" (add p pdf).file;
    count "single artifact" 1 (List.length (Store.manifest p).artifacts);
    count "single event" 1 (List.length (lines (dir / "events.jsonl")));
    equal "changed bytes retain first copy" "url-2.pdf" (add p (pdf ^ "new")).file);
  Store.with_paper second_handle uid (fun p ->
    count "reopened projection" 2 (List.length (Store.manifest p).artifacts));
  equal "original bytes" pdf (Eio.Path.load (dir / "url.pdf"));
  check "no archive or index created" false (Eio.Path.is_directory (root / "index"));
  Eio.Path.unlink (dir / "paper.json");
  Store.with_paper store uid (fun p ->
    count "projection rebuilt from events" 2 (List.length (Store.manifest p).artifacts));
  check "manifest restored" true (Eio.Path.is_file (dir / "paper.json")))

let interrupted () = with_temp (fun _ sw root ->
  let store = Store.open_root ~sw root in
  Store.with_paper store uid (fun p -> ignore (add p pdf));
  let dir = root / Layout.paper uid in
  let log = Eio.Path.load (dir / "events.jsonl") in
  (* Simulate termination during append, after the intent was synced. *)
  let intent = Printf.sprintf {|{"offset":0,"event":%s}|}
    (get (Jsont_bytesrw.encode_string Jsont.string log)) in
  save (dir / ".pending.json") intent;
  save (dir / "events.jsonl") (String.sub log 0 17);
  Eio.Path.unlink (dir / "paper.json");
  Store.with_paper store uid (fun p -> ignore (add p pdf));
  equal "append recovered byte-for-byte" log (Eio.Path.load (dir / "events.jsonl"));
  count "no duplicate publication" 1 (List.length (lines (dir / "events.jsonl")));
  (* Simulate termination after durable intent, before artifact rename. *)
  save (dir / ".pending.json") intent;
  save (dir / "events.jsonl") "";
  Eio.Path.rename (dir / "url.pdf") (dir / ".transfer.tmp");
  Store.with_paper store uid (fun p -> count "intent publishes artifact" 1
    (List.length (Store.manifest p).artifacts));
  equal "artifact recovered" pdf (Eio.Path.load (dir / "url.pdf"));
  (* Simulate termination after the complete append. *)
  save (dir / ".pending.json") intent;
  Store.with_paper store uid (fun _ -> ());
  equal "complete append is not repeated" log (Eio.Path.load (dir / "events.jsonl")))

let refused_content () = with_temp (fun _ sw root ->
  let store = Store.open_root ~sw root in
  let dir = root / Layout.paper uid in
  Store.with_paper store uid (fun p ->
    let exception Rejected in
    check "validation failure propagates" true
      (try ignore (Store.add p ~at ~source:Manifest.Source.Url
        ~format:Manifest.Format.Pdf ~strategy:"url" ~status:200 ~url:"https://example.org"
        ~validate:(fun _ -> raise Rejected) (Eio.Flow.string_source pdf)); false
       with Rejected -> true);
    count "nothing published" 0 (List.length (Store.manifest p).artifacts);
    check "transfer cleaned" false (Eio.Path.is_file (dir / ".transfer.tmp"));
    check "byte limit" true
      (try ignore (Store.add ~max_bytes:4 p ~at ~source:Manifest.Source.Url
        ~format:Manifest.Format.Pdf ~strategy:"url" ~status:200 ~url:"https://example.org"
        ~validate:(fun _ -> ()) (Eio.Flow.string_source pdf)); false
       with Store.Limit_exceeded -> true));
  let other = get (Uid.of_input "10.1000/foreign") in
  let dir = root / Layout.paper other in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  save (dir / "paper.json") "legacy";
  check "existing archive refused" true
    (try Store.with_paper store other (fun _ -> ()); false with Failure _ -> true);
  equal "existing data unchanged" "legacy" (Eio.Path.load (dir / "paper.json")))

let mock_pdf data request = Fetch_mock.respond
  ~headers:(Http.Header.of_list ["content-type", "application/pdf"]) data request
let direct () = with_temp (fun env sw root ->
  let audit_db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let audit = Taposaur_audit.session ~component:"test.fetch" audit_db in
  let calls = ref 0 in
  let http = Fetch_mock.client (fun request -> incr calls; mock_pdf pdf request) in
  let downloader = D.create ~audit ~clock:env#clock http in
  let store = Store.open_root ~audit ~sw root in
  let result = D.fetch_url downloader store uid ~format:Manifest.Format.Pdf
    "https://example.org/paper.pdf?api_key=secret" in
  count "downloaded" 1 (List.length result.artifacts);
  equal "credential omitted from provenance" "https://example.org/paper.pdf"
    (Option.get (List.hd result.artifacts).url);
  ignore (D.fetch_url downloader store uid ~format:Manifest.Format.Pdf
    "https://example.org/paper.pdf");
  count "retry downloads then deduplicates" 2 !calls;
  count "no duplicate event" 1 (List.length
    (lines (root / Layout.paper uid / "events.jsonl")));
  let bad = D.create ~audit ~clock:env#clock (Fetch_mock.client (mock_pdf "<html>login</html>")) in
  let result = D.fetch_url bad store uid ~format:Manifest.Format.Pdf "https://example.org" in
  count "HTML not filed as PDF" 0 (List.length result.artifacts);
  check "invalid content classified" true
    (match (List.hd result.attempts).failure with D.Invalid_content _ -> true | _ -> false);
  let small = D.create ~audit ~clock:env#clock ~max_bytes:4 http in
  count "size refusal is a typed outcome" 1
    (List.length (D.fetch_url small store uid ~format:Manifest.Format.Pdf
      "https://example.org").attempts);
  let events = Taposaur_audit.query ~limit:500 audit_db in
  check "fetch success audited" true
    (List.exists (fun (e : Taposaur_audit.event) ->
      e.action="fetch.url" && e.phase=Succeeded) events);
  check "fetch validation failure audited" true
    (List.exists (fun (e : Taposaur_audit.event) ->
      e.action="fetch.transfer" && e.phase=Failed) events);
  check "artifact publication audited" true
    (List.exists (fun (e : Taposaur_audit.event) ->
      e.action="staging.artifact.stored") events);
  let encoded = String.concat "" (List.map Taposaur_audit.to_json events) in
  check "credential absent from audit" false
    (let needle="secret" in let rec contains i = i+String.length needle <= String.length encoded &&
      (String.sub encoded i (String.length needle)=needle || contains (i+1)) in contains 0))

let fallback () = with_temp (fun env sw root ->
  let requests = ref [] in
  let http = Fetch_mock.client (fun request ->
    let url = Fetch.Middleware.Url.to_string request.Fetch.Middleware.url in
    requests := url :: !requests;
    if String.starts_with ~prefix:"https://metadata.test/service/lookup" url then
      Fetch_mock.respond
        {|{"DOI":"10.1000/staged/paper","publisher":"Elsevier Ltd","oaLink":"https://oa.test/paper.pdf"}|} request
    else if String.starts_with ~prefix:S.Elsevier.origin url then begin
      equal "publisher credential scoped" "publisher-secret"
        (Option.get (Http.Header.get request.headers "x-els-apikey"));
      Fetch_mock.respond ~status:403 "denied" request
    end else begin
      check "no publisher credential at OA" true
        (Http.Header.get request.headers "x-els-apikey" = None);
      mock_pdf pdf request
    end) in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let elsevier = S.Elsevier.client ~api_key:(fun () -> "publisher-secret") http in
  let downloader = D.create ~clock:env#clock ~glutton ~elsevier http in
  let result = D.fetch downloader (Store.open_root ~sw root) uid in
  count "lookup publisher OA" 3 (List.length !requests);
  equal "OA artifact" "oa.pdf" (List.hd result.artifacts).file;
  check "publisher refusal retained" true
    ((List.hd result.attempts).failure = D.Http 403))

let timeout () = with_temp (fun env sw root ->
  let http = Fetch_mock.client (fun _ -> Eio.Time.sleep env#clock 1.; assert false) in
  let d = D.create ~clock:env#clock ~timeout:0.01 http in
  let result = D.fetch_url d (Store.open_root ~sw root) uid
    ~format:Manifest.Format.Pdf "https://example.org/paper.pdf" in
  check "timeout classified" true ((List.hd result.attempts).failure = D.Timeout);
  count "no timed-out publication" 0 (List.length result.artifacts))

let service_contracts () = with_temp (fun _ sw _ ->
  let doi s = get (Doi.of_string s) in
  equal "production Springer query"
    "https://api.springernature.com/xmldata/jats?q=(doi:10.1007/a%20OR%20doi:10.1007/b)&p=25"
    (S.Springer.url ~dois:[doi "10.1007/A"; doi "10.1007/B"]);
  equal "production PMC lookup"
    "https://glutton.test/service/lookup?pmc=PMC4221854"
    (S.Glutton.lookup_url ~server:"https://glutton.test" ~pmcid:"4221854" ());
  equal "production DOI lookup"
    "https://glutton.test/service/lookup?doi=10.1038/nature12373"
    (S.Glutton.lookup_url ~server:"https://glutton.test/"
      ~doi:(doi "10.1038/Nature12373") ());
  let calls = ref 0 in
  let http = Fetch_mock.client (fun req ->
    incr calls;
    Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list ["location", "https://elsewhere.test/steal"])
      "" req) in
  let client = S.Elsevier.client ~api_key:(fun () -> "secret") http in
  check "credentialed redirect outside origin refused" true
    (try ignore (S.Elsevier.article ~sw client (doi "10.1000/test")); false
     with Eio.Io _ -> true);
  count "no request to outside origin" 1 !calls)

let xml_and_identity () = with_temp (fun env sw root ->
  let store = Store.open_root ~sw root in
  let xml body = D.create ~clock:env#clock (Fetch_mock.client (fun req ->
    Fetch_mock.respond
      ~headers:(Http.Header.of_list ["content-type", "application/xml"])
      body req)) in
  List.iter (fun body ->
    let result = D.fetch_url (xml body) store uid ~format:Manifest.Format.Xml
      "https://example.org/paper.xml" in
    count "invalid XML refused" 0 (List.length result.artifacts))
    ["<html><body>login</body></html>"; "<article><broken>"; "<article/><article/>"; "";
     "<response><records/></response>";
     "<response><records><article/><article/></records></response>"];
  let result = D.fetch_url (xml "<article><title>Study</title></article>")
    store uid ~format:Manifest.Format.Xml "https://example.org/paper.xml" in
  count "complete publisher XML" 1 (List.length result.artifacts);
  let result = D.fetch_url
    (xml "<response><records><article><title>Study</title></article></records></response>")
    store uid ~format:Manifest.Format.Xml "https://example.org/paper.xml" in
  count "wrapped Springer XML" 1 (List.length result.artifacts);
  let http = Fetch_mock.client (fun req -> Fetch_mock.respond
    {|{"DOI":"10.1000/another-paper"}|} req) in
  let glutton = S.Glutton.client ~server:"https://glutton.test" http in
  let result = D.fetch (D.create ~glutton ~clock:env#clock http) store uid in
  check "identity mismatch refused" true
    (match (List.hd result.attempts).failure with D.Invalid_content _ -> true | _ -> false))

let body_timeout () = with_temp (fun env sw root ->
  let closed = ref false in
  let module Slow = struct
    type t = unit
    let read_methods = []
    let single_read () (_ @ local) = Eio.Time.sleep env#clock 1.; raise End_of_file
  end in
  let http = Fetch_mock.client (fun req ->
    Fetch.Middleware.Pi.response ~status:200
      ~headers:(Http.Header.of_list ["content-type", "application/pdf"])
      ~version:`HTTP_1_1
      ~body:(Eio.Resource.T ((), Eio.Flow.Pi.source (module Slow)))
      ~close:(fun () -> closed := true) ~url:req.Fetch.Middleware.url ()) in
  let d = D.create ~clock:env#clock ~timeout:0.01 http in
  let store = Store.open_root ~sw root in
  let result = D.fetch_url d store uid ~format:Manifest.Format.Pdf
    "https://example.org/paper.pdf" in
  check "body deadline classified" true ((List.hd result.attempts).failure = D.Timeout);
  check "response closed after cancellation" true !closed;
  check "unfinished body removed" false
    (Eio.Path.is_file (root / Layout.paper uid / ".transfer.tmp"));
  Store.with_paper store uid (fun p -> count "lock released" 0
    (List.length (Store.manifest p).artifacts)))

let cup () = with_temp (fun env sw root ->
  let article = "https://www.cambridge.org/core/journals/example/article/title/ABCDEF0123456789" in
  check "canonical article" true (S.Cup.id_of_url article = Some "ABCDEF0123456789");
  List.iter (fun url -> check "not an article" true (S.Cup.id_of_url url = None))
    ["https://evil.test/core/journals/a/article/ABCDEF0123456789";
     "https://www.cambridge.org/core/login/ABCDEF0123456789";
     S.Cup.identifier_url ~doi:(Option.get (Uid.doi uid))];
  let requests = ref 0 in
  let http = Fetch_mock.client (fun req ->
    incr requests;
    let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
    if String.starts_with ~prefix:"https://metadata.test" url then
      Fetch_mock.respond {|{"DOI":"10.1000/staged/paper","publisher":"Cambridge University Press"}|} req
    else if String.starts_with ~prefix:(S.Cup.origin ^ "/core/product/") url then
      Fetch_mock.respond ~status:302
        ~headers:(Http.Header.of_list ["location", article]) "" req
    else if url = article then Fetch_mock.respond "article page" req
    else mock_pdf pdf req) in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let result = D.fetch (D.create ~glutton ~clock:env#clock http)
    (Store.open_root ~sw root) uid in
  count "Cambridge lookup, redirect, PDF" 4 !requests;
  count "Cambridge artifact" 1 (List.length result.artifacts);
  count "Cambridge no errors" 0 (List.length result.attempts))

let core_identity () = List.iter (fun matching -> with_temp (fun env sw root ->
  let downloads = ref 0 in
  let http = Fetch_mock.client (fun req ->
    let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
    if String.starts_with ~prefix:"https://metadata.test/service/lookup" url then
      Fetch_mock.respond {|{"DOI":"10.1000/staged/paper"}|} req
    else if String.starts_with ~prefix:"https://metadata.test" url then
      Fetch_mock.respond "{}" req
    else if String.starts_with ~prefix:(S.Core.origin ^ "/v3/search/") url then
      Fetch_mock.respond {|{"results":[{"id":42}]}|} req
    else if url = S.Core.metadata_url ~id:"42" then
      Fetch_mock.respond (if matching then {|{"doi":"10.1000/staged/paper"}|}
        else {|{"doi":"10.1000/other"}|}) req
    else begin incr downloads; mock_pdf pdf req end) in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let core = S.Core.client ~token:(fun () -> "secret") http in
  let result = D.fetch (D.create ~glutton ~core ~clock:env#clock http)
    (Store.open_root ~sw root) uid in
  count "only matching CORE output downloaded" (if matching then 1 else 0) !downloads;
  count "only matching CORE artifact stored" (if matching then 1 else 0)
    (List.length result.artifacts))) [true; false]

let publisher_retry () = with_temp (fun env sw root ->
  let requests = ref 0 and closed = ref 0 in
  let http = Fetch_mock.client (fun req ->
    let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
    if String.starts_with ~prefix:"https://metadata.test" url then
      Fetch_mock.respond {|{"DOI":"10.1000/staged/paper","publisher":"Wiley"}|} req
    else begin
      incr requests;
      if !requests < 3 then Fetch.Middleware.Pi.response ~status:503
        ~headers:(Http.Header.of_list []) ~version:`HTTP_1_1
        ~body:(Eio.Flow.string_source "busy") ~url:req.url
        ~close:(fun () -> incr closed) ()
      else mock_pdf pdf req
    end) in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let wiley = S.Wiley.client ~tokens:["secret"] http in
  (* The wait exceeds the transfer deadline, so a wait inside the deadline
     would time out. The deadline must still cover a real store write. *)
  let result = D.fetch (D.create ~glutton ~wiley ~busy_wait:0.6
    ~timeout:0.5 ~clock:env#clock http) (Store.open_root ~sw root) uid in
  count "busy retries outside transfer deadline" 1 (List.length result.artifacts);
  count "bounded attempts" 3 !requests;
  count "busy responses closed" 2 !closed)

let metadata_deadline () = with_temp (fun env sw root ->
  let http = Fetch_mock.client (fun req ->
    Eio.Time.sleep env#clock 0.1;
    Fetch_mock.respond "{}" req) in
  let glutton = S.Glutton.client ~server:"https://metadata.test" http in
  let result = D.fetch (D.create ~glutton ~lookup_timeout:0.001
    ~timeout:1. ~clock:env#clock http) (Store.open_root ~sw root) uid in
  check "separate metadata deadline" true
    ((List.hd result.attempts).failure = D.Timeout))

let converter_clients () = with_temp (fun _ sw _ ->
  let urls = ref [] in
  let contains s fragment =
    let rec loop i = i + String.length fragment <= String.length s &&
      (String.sub s i (String.length fragment) = fragment || loop (i + 1)) in
    loop 0 in
  let http = Fetch_mock.client (fun req ->
    check "converter uses POST" true (req.Fetch.Middleware.meth = `POST);
    let url = Fetch.Middleware.Url.to_string req.url in
    urls := url :: !urls;
    let body = match req.body with
      | Fetch.Middleware.String s -> s
      | _ -> Alcotest.fail "Expected a replayable converter body" in
    if String.ends_with ~suffix:"processText" url then
      check "urlencoded conversion options" true
        (contains body "consolidateReferences=1" && contains body "text=")
    else begin
      check "multipart input" true (contains body "name=\"input\"");
      check "explicit sentence segmentation" true
        (contains body "name=\"segmentSentences\"\r\n\r\n1");
      if String.ends_with ~suffix:"processFulltextDocument" url then begin
        check "PDF bytes preserved" true (contains body pdf);
        check "coordinate option repeated" true
          (contains body "name=\"teiCoordinates\"\r\n\r\nref" &&
           contains body "name=\"teiCoordinates\"\r\n\r\np");
        check "funder consolidation disabled" true
          (contains body "name=\"consolidateFunders\"\r\n\r\n0")
      end else check "XML bytes preserved" true (contains body "<article/>")
    end;
    Fetch_mock.respond "<TEI/>" req) in
  let grobid = S.Grobid.client ~server:"http://grobid.test/" http in
  let pub2tei = S.Pub2tei.client ~server:"http://pub2tei.test/" http in
  Fetch.close (S.Grobid.process_fulltext ~sw grobid pdf);
  Fetch.close (S.Pub2tei.process_xml ~sw pub2tei "<article/>");
  Fetch.close (S.Pub2tei.process_text ~sw pub2tei "<article/>");
  Alcotest.(check (list string)) "conversion endpoints"
    ["http://pub2tei.test/service/processText";
     "http://pub2tei.test/service/processXML";
     "http://grobid.test/api/processFulltextDocument"] !urls)

let publisher_capabilities () =
  List.iter (fun (name,expected) ->
    equal name expected (Option.get (D.publisher_capability name)).publisher)
    ["Elsevier BV","Elsevier";"Springer Nature","Springer";
     "John Wiley & Sons","Wiley";"Informa UK Limited","Taylor & Francis / Informa";
     "Cambridge University Press (CUP)","Cambridge University Press";
     "Public Library of Science (PLoS)","PLOS";"PLOS","PLOS"];
  List.iter (fun name -> check "no unsupported claims" true
    (D.publisher_capability name=None)) ["";"Unknown";"MDPI AG"]

let () = Alcotest.run "taposaur acquisition" [
  "store", [
    Alcotest.test_case "locking and idempotent publication" `Quick publication;
    Alcotest.test_case "interrupted publication recovery" `Quick interrupted;
    Alcotest.test_case "validation and confinement" `Quick refused_content];
  "services", [Alcotest.test_case "production URL and credential contracts"
    `Quick service_contracts;
    Alcotest.test_case "converter clients" `Quick converter_clients];
  "download", [
    Alcotest.test_case "shared publisher capabilities" `Quick publisher_capabilities;
    Alcotest.test_case "Cambridge routing" `Quick cup;
    Alcotest.test_case "CORE output identity" `Quick core_identity;
    Alcotest.test_case "publisher busy retries" `Quick publisher_retry;
    Alcotest.test_case "metadata deadline" `Quick metadata_deadline;
    Alcotest.test_case "XML validation and identity" `Quick xml_and_identity;
    Alcotest.test_case "body deadline and cancellation cleanup" `Quick body_timeout;
    Alcotest.test_case "direct transfer and validation" `Quick direct;
    Alcotest.test_case "publisher fallback and credential scope" `Quick fallback;
    Alcotest.test_case "transfer deadline" `Quick timeout]]
