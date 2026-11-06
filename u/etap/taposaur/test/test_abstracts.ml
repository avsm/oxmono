open Taposaur
module B = Taposaur_abstracts
module C = Taposaur_catalog
module P = Taposaur_paper
module R = Taposaur_review
module S = Taposaur_review_store
module X = Taposaur_review_run
let ( / ) = Eio.Path.( / )
let get = Tap_test.get
let check = Alcotest.(check bool)
let equal = Alcotest.(check string)
let count = Alcotest.(check int)
let doi s = get (Doi.of_string s)
let with_temp f = Tap_test.with_temp "abstract-test-"
  (fun env sw root _ -> f env sw root)
let work ?(inverted={|{"Birds":[0],"migrate":[1],"south.":[2]}|}) d =
  Printf.sprintf {|{"id":"https://openalex.org/W1","doi":"https://doi.org/%s","abstract_inverted_index":%s}|} d inverted
let results works = {|{"meta":{"count":1},"results":[|} ^ String.concat "," works ^ "]}"

let reconstruction () =
  let index = Jsont.String_map.(empty |> add "south." [2] |> add "Birds" [0;4] |> add "migrate" [1]) in
  equal "ordered by position" "Birds migrate south. Birds" (Option.get (B.reconstruct index));
  check "empty index" true (B.reconstruct Jsont.String_map.empty = None);
  check "absurd position" true
    (B.reconstruct (Jsont.String_map.singleton "x" [1_000_000]) = None);
  let e = get (B.decode ~provider:"openalex" ~endpoint:"http://x" ~doi:(doi "10.1000/A")
    (work "10.1000/a")) in
  equal "decoded abstract" "Birds migrate south." e.abstract;
  equal "canonical doi" "10.1000/a" e.doi;
  check "mismatch refused" true (Result.is_error (B.decode ~provider:"openalex"
    ~endpoint:"http://x" ~doi:(doi "10.1000/b") (work "10.1000/a")));
  check "null index refused" true (Result.is_error (B.decode ~provider:"openalex"
    ~endpoint:"http://x" ~doi:(doi "10.1000/a") (work ~inverted:"null" "10.1000/a")));
  check "garbage refused" true (Result.is_error (B.decode ~provider:"openalex"
    ~endpoint:"http://x" ~doi:(doi "10.1000/a") "nope"))

let batching () =
  let long = doi ("10.1000/" ^ String.make 200 'x') in
  let dois = List.init 250 (fun i -> doi (Printf.sprintf "10.1000/%d" i)) in
  let sizes = List.map List.length (B.batches dois) in
  check "hundreds" true (sizes = [100; 100; 50]);
  let sizes = List.map List.length (B.batches (List.init 60 (fun _ -> long))) in
  check "bounded by url length" true (List.for_all (fun n -> n < 40) sizes &&
    List.fold_left (+) 0 sizes = 60);
  check "order kept" true (List.concat (B.batches dois) = dois)

let lookups () = with_temp (fun env sw root ->
  let audit_db=Taposaur_audit.open_write ~sw (root / "audit") in
  let audit=Taposaur_audit.session ~component:"test" audit_db in
  let store=B.open_write ~audit ~sw (root / "abstracts") in
  let calls=ref 0 in
  let http=Fetch_mock.client (fun req -> incr calls;
    let url = Fetch.Middleware.Url.to_string req.url in
    check "api key sent" true (String.ends_with ~suffix:"&api_key=secret" url);
    let single = Option.is_some (String.index_opt url '?') &&
      not (List.exists (fun part -> String.starts_with ~prefix:"filter=" part)
        (String.split_on_char '&' (List.nth (String.split_on_char '?' url) 1))) in
    check "filter or single-work path" true (single || List.exists (fun part ->
      String.starts_with ~prefix:"filter=doi:" part) (String.split_on_char '?' url
        |> List.concat_map (String.split_on_char '&')));
    match !calls with
    | 1 -> Fetch_mock.respond (results [work "10.1000/a";
        work ~inverted:"null" "10.1000/b"]) req
    | 2 -> Fetch_mock.respond ~status:429 "slow down" req
    | 3 -> Fetch_mock.respond ~status:403 "budget" req
    | 4 -> Fetch_mock.respond ~status:400 "bad" req
    | 5 -> check "single-work read" true single;
        Fetch_mock.respond ~status:404 "{}" req
    | _ -> check "single-work read" true single;
        Fetch_mock.respond (work "10.1000/c") req) in
  let client=B.client ~audit ~api_key:"secret" ~clock:env#clock http in
  let a = doi "10.1000/a" and b = doi "10.1000/b" and c = doi "10.1000/c" in
  let found = get (Result.map_error B.refusal_name (B.lookup_many client store [a; b; c])) in
  count "one accepted" 1 (List.length found);
  equal "accepted abstract" "Birds migrate south." (Option.get (B.find store a)).abstract;
  check "missing abstract attempted" true (B.attempted store b && B.find store b = None);
  check "absent result attempted" true (B.attempted store c);
  equal "body retained" (results [work "10.1000/a"; work ~inverted:"null" "10.1000/b"])
    (Option.get (B.record store ~sha256:(Option.get (B.find store a)).sha256));
  check "rate limit reported" true (B.lookup_many client store [c] = Error B.Rate_limited);
  check "budget reported" true (B.lookup_many client store [c] = Error B.Budget);
  check "http failure reported" true (B.lookup_many client store [c] = Error (B.Http 400));
  equal "single failure recorded" "http_error" (
    let s = Sqlite3.db_open ~mode:`READONLY (Eio.Path.native_exn (root / "abstracts")) in
    let st = Sqlite3.prepare s "SELECT decision FROM attempts WHERE doi='10.1000/c' ORDER BY id DESC LIMIT 1" in
    ignore (Sqlite3.step st); let d = Sqlite3.column_text st 0 in
    ignore (Sqlite3.finalize st); ignore (Sqlite3.db_close s); d);
  check "single read not found" true (B.lookup client store c = Ok None);
  check "accepted later" true (match B.lookup client store c with
    | Ok (Some e) -> e.abstract = "Birds migrate south." | _ -> false);
  (let n = !calls in ignore (B.lookup client store c);
   count "cached afterwards" n !calls);
  check "known evidence skips the network" true
    (let n = !calls in B.lookup_many client store [a] = Ok [] && !calls = n);
  let events=Taposaur_audit.query ~action:"abstracts.lookup" audit_db in
  check "lookups audited" true (List.length events >= 12);
  let ro=B.open_read ~sw (root / "abstracts") in
  count "reopened" 2 (B.count ro);
  let seen = ref [] in B.iter ro (fun e -> seen := e.doi :: !seen);
  check "iteration in doi order" true (List.rev !seen = ["10.1000/a"; "10.1000/c"]))

let evidence d = {Abstract_evidence.doi=Doi.to_string d; abstract="From OpenAlex.";
  provider="openalex"; endpoint="http://x"; sha256=String.make 64 'a'; retrieved=1.}

let resolver () =
  let manifest = get (Manifest.of_string {|{"v":1,"id":"doi:10.1000/a","artifacts":[],"updated":"2026-09-14T05:54:04.598Z"}|}) in
  let p = P.resolve manifest [] in
  let p = P.with_abstract (evidence (doi "10.1000/a")) p in
  equal "abstract filled" "From OpenAlex." (Option.get (P.bibliography p).abstract);
  equal "source labelled" ("openalex · " ^ String.make 64 'a')
    (P.source_name (Option.get p.metadata.abstract.selected).source);
  let cached = get (P.of_cache_string ~index:(get (Index.of_string
    {|{"uid":"doi:10.1000/a","scheme":"doi","pdf_path":"/x/a.pdf","pdf_size":1,"pdf_mtime":1,"tei_path":"","tei_mtime":0}|})) (get (P.cache_string p))) in
  equal "cache reapplies evidence" "From OpenAlex." (Option.get (P.bibliography cached).abstract);
  let index = get (Index.of_string {|{"uid":"doi:10.1000/a","scheme":"doi","abstract":"Indexed.","pdf_path":"/x/a.pdf","pdf_size":1,"pdf_mtime":1,"tei_path":"","tei_mtime":0}|}) in
  let q = P.with_abstract (evidence (doi "10.1000/a")) (P.resolve ~index manifest []) in
  equal "existing abstract wins" "Indexed." (Option.get (P.bibliography q).abstract);
  check "evidence not recorded when unused" true (q.abstract_evidence = None);
  check "doi mismatch refused" true
    (try ignore (P.with_abstract (evidence (doi "10.1000/b")) p); false
     with Invalid_argument _ -> true)

let catalogue () = with_temp (fun _ sw root ->
  let catalog = C.open_write ~sw (root / "catalog.db") in
  let row = get (Index.of_string {|{"uid":"doi:10.1000/a","scheme":"doi","title":"A","pdf_path":"/x/a.pdf","pdf_size":1,"pdf_mtime":1,"tei_path":"","tei_mtime":0}|}) in
  ignore (C.Shard.replace catalog ~shard:(Layout.index (get (Uid.of_input "10.1000/a"))) {size=1L; mtime=1.}
    (fun emit -> emit row));
  let uid = get (Uid.of_input "10.1000/a") in
  let e = evidence (doi "10.1000/a") in
  check "filled" true (C.Metadata.fill_abstract catalog ~uid ~abstract:e.abstract
    ~update_detail:(fun _ raw -> raw));
  equal "searchable" "From OpenAlex."
    (Option.get (List.hd (C.search catalog (C.Query.make ~text:"OpenAlex" ())).papers).Summary.bibliography.abstract);
  check "second fill is a no-op" false (C.Metadata.fill_abstract catalog ~uid ~abstract:"Other"
    ~update_detail:(fun _ raw -> raw));
  check "absent paper" false (C.Metadata.fill_abstract catalog
    ~uid:(get (Uid.of_input "10.1000/none")) ~abstract:"x" ~update_detail:(fun _ raw -> raw)))

let snapshots () = with_temp (fun _ sw root ->
  let catalog = C.open_write ~sw (root / "catalog.db") in
  let row = get (Index.of_string {|{"uid":"doi:10.1000/a","scheme":"doi","abstract":"Indexed.","pdf_path":"/x/a.pdf","pdf_size":1,"pdf_mtime":1,"tei_path":"","tei_mtime":0}|}) in
  ignore (C.Shard.replace catalog ~shard:(Layout.index (get (Uid.of_input "10.1000/a"))) {size=1L; mtime=1.}
    (fun emit -> emit row));
  let read = C.open_read ~sw (root / "catalog.db") in
  let store = S.open_write ~sw (root / "reviews.db") in
  let raw = "10.1000/a\n10.1000/b\n" in
  let source = {R.Definition.kind = List; format = Doi_lines;
    blob = Some (S.put_blob store raw); local = None; provider = None;
    query = None; parameters = None} in
  let definition = {R.Definition.version = 1;
    inputs = [{key = "main"; label = "main"; source}];
    include_any = ["main"]; require_all = []; exclude_any = []} in
  ignore (S.define store ~slug:"r" ~title:"R" ~requester:"x" ~owner:"y" ~actor:"t"
    ~reason:"Test" ~expected:0 definition);
  let revision = S.revision store ~slug:"r" ~number:None in
  let run_id = X.execute store read revision ~actor:"t" [] in
  let abstracts = [evidence (doi "10.1000/a"); evidence (doi "10.1000/b")] in
  let id = X.snapshot ~abstracts store read ~run_id ~parent:None ~label:"L" ~actor:"t" in
  let rows = S.rows store id in
  let find d = List.find (fun (o : R.observation) -> o.doi = d) rows in
  equal "catalogued abstract kept" "Indexed." (Option.get (find "10.1000/a").bibliography.abstract);
  equal "uncatalogued member filled" "From OpenAlex." (Option.get (find "10.1000/b").bibliography.abstract);
  check "evidence labelled" true (List.exists (fun (e : R.evidence) ->
    e.field = "abstract" && String.starts_with ~prefix:"openalex" e.source)
    (find "10.1000/b").evidence);
  check "not catalogued" false (find "10.1000/b").in_catalogue)

let () = Alcotest.run "abstracts" [
  "decoding", [Alcotest.test_case "inverted index" `Quick reconstruction;
               Alcotest.test_case "batching" `Quick batching];
  "store", [Alcotest.test_case "lookups" `Quick lookups];
  "resolver", [Alcotest.test_case "evidence" `Quick resolver];
  "catalogue", [Alcotest.test_case "fill" `Quick catalogue];
  "reviews", [Alcotest.test_case "snapshot evidence" `Quick snapshots]]
