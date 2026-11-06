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
  Printf.sprintf {|{"id":"https://openalex.org/W1","doi":"https://doi.org/%s","title":"Bird migration","display_name":"Bird migration","authorships":[{"author_position":"first","author":{"id":"https://openalex.org/A1","display_name":"Ian S. Pearse"},"raw_author_name":"PEARSE, IAN"},{"author":{"display_name":null},"raw_author_name":"Smith, J."},{"author":{"display_name":"Plato"}}],"primary_location":{"source":{"id":"https://openalex.org/S30982003","display_name":"Ecological Entomology","issn_l":"0307-6946","issn":["0307-6946","1365-2311"],"host_organization_name":"Wiley","type":"journal"}},"publication_year":2011,"publication_date":"2011-09-13","biblio":{"volume":"36","issue":"5","first_page":"635","last_page":"642"},"ids":{"openalex":"https://openalex.org/W1","pmid":"https://pubmed.ncbi.nlm.nih.gov/12345","pmcid":"https://www.ncbi.nlm.nih.gov/pmc/articles/PMC67"},"abstract_inverted_index":%s}|} d inverted
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

let works () =
  let w = get (B.decode_work ~provider:"openalex" ~endpoint:"http://x"
    ~doi:(doi "10.1000/A") (work ~inverted:"null" "10.1000/a")) in
  let b = w.bibliography in
  equal "canonical doi" "10.1000/a" w.doi;
  equal "title" "Bird migration" (Option.get b.title);
  check "authors split at the last space, comma or kept whole" true
    (b.authors = [{Author.given=Some "Ian S."; family="Pearse"};
                  {given=Some "J."; family="Smith"};
                  {given=None; family="Plato"}]);
  equal "journal" "Ecological Entomology" (Option.get b.journal);
  equal "publisher" "Wiley" (Option.get b.publisher);
  equal "issn-l preferred" "0307-6946" (Option.get b.issn);
  equal "pages" "635\xe2\x80\x93642" (Option.get b.pages);
  equal "volume" "36" (Option.get b.volume);
  equal "issue" "5" (Option.get b.issue);
  equal "date" "2011-09-13" (Option.get b.published);
  check "year" true (b.year = Some 2011);
  equal "pmid from url" "12345" (Option.get b.pmid);
  equal "pmcid from url" "PMC67" (Option.get b.pmcid);
  check "no abstract in work evidence" true (b.abstract = None);
  check "venue" true (w.venue = Some {Work_evidence.id="S30982003";
    name=Some "Ecological Entomology"; issn_l=Some "0307-6946";
    issns=["0307-6946";"1365-2311"]; kind=Some "journal"; host=Some "Wiley"});
  let v = get (B.decode_source {|{"id":"https://openalex.org/S1","display_name":"PLoS ONE","issn_l":"1932-6203","issn":["1932-6203"],"type":"journal","host_organization_name":"Public Library of Science"}|}) in
  equal "source id" "S1" v.id;
  check "source without id refused" true (Result.is_error (B.decode_source {|{"display_name":"x"}|}));
  check "fields named" true (Work_evidence.fields w =
    ["title";"authors";"journal";"publisher";"published";"year";"volume";
     "issue";"pages";"issn";"doi";"pmcid";"pmid"]);
  equal "label" ("openalex \xc2\xb7 " ^ w.sha256) (Work_evidence.label w);
  check "mismatch refused" true (Result.is_error (B.decode_work ~provider:"openalex"
    ~endpoint:"http://x" ~doi:(doi "10.1000/b") (work "10.1000/a")));
  let sparse = get (B.decode_work ~provider:"openalex" ~endpoint:"http://x"
    ~doi:(doi "10.1000/a") {|{"doi":"https://doi.org/10.1000/a","title":" ","biblio":{"first_page":"e12","last_page":"e12"},"ids":{"pmcid":"https://www.ncbi.nlm.nih.gov/pmc/articles/5193348"}}|}) in
  check "blank title dropped" true (sparse.bibliography.title = None);
  equal "single page" "e12" (Option.get sparse.bibliography.pages);
  equal "bare pmc number prefixed" "PMC5193348" (Option.get sparse.bibliography.pmcid);
  check "sparse fields" true (Work_evidence.fields sparse = ["pages";"doi";"pmcid"]);
  let back = get (Jsont_bytesrw.decode_string Work_evidence.jsont
    (get (Jsont_bytesrw.encode_string Work_evidence.jsont w))) in
  check "round trip" true (back = w)

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
  equal "work stored with the abstract" "Bird migration"
    (Option.get (Option.get (B.find_work store a)).bibliography.title);
  equal "work stored without an abstract" "Bird migration"
    (Option.get (Option.get (B.find_work store b)).bibliography.title);
  check "known once the work is stored" true (B.known store a && B.known store b
    && not (B.known store c));
  count "works counted" 2 (B.count_works store);
  check "work names the retained body" true
    ((Option.get (B.find_work store b)).sha256 = (Option.get (B.find store a)).sha256);
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
    (let n = !calls in B.lookup_many client store [a; b] = Ok [] && !calls = n);
  check "known work without abstract skips the network" true
    (let n = !calls in B.lookup client store b = Ok None && !calls = n);
  let events=Taposaur_audit.query ~action:"abstracts.lookup" audit_db in
  check "lookups audited" true (List.length events >= 12);
  let ro=B.open_read ~sw (root / "abstracts") in
  count "reopened" 2 (B.count ro);
  count "works reopened" 3 (B.count_works ro);
  let seen = ref [] in B.iter ro (fun e -> seen := e.doi :: !seen);
  check "iteration in doi order" true (List.rev !seen = ["10.1000/a"; "10.1000/c"]))

(* A version 1 store, which held abstracts only, is read as is and
   migrated to the current version by the first writable open. *)
let migration () = with_temp (fun env sw root ->
  let path = root / "abstracts" in
  let a = doi "10.1000/a" in
  Eio.Switch.run (fun sw ->
    let store = B.open_write ~sw path in
    let http = Fetch_mock.client (fun req -> Fetch_mock.respond (work "10.1000/a") req) in
    let client = B.client ~clock:env#clock http in
    check "accepted" true (match B.lookup client store a with
      | Ok (Some _) -> true | _ -> false));
  let raw sql =
    let s = Sqlite3.db_open (Eio.Path.native_exn path) in
    Sqlite3.Rc.check (Sqlite3.exec s sql); ignore (Sqlite3.db_close s) in
  raw "DROP TABLE sources; PRAGMA user_version=2";
  let ro = B.open_read ~sw path in
  check "version 2 read with works but no sources" true
    (B.find_work ro a <> None && B.known ro a && B.find_source ro "issn:0307-6946" = None);
  raw "DROP TABLE works; PRAGMA user_version=1";
  let ro = B.open_read ~sw path in
  check "version 1 read without works" true
    (B.find_work ro a = None && B.count_works ro = 0 && not (B.known ro a));
  check "abstract still found" true (B.find ro a <> None);
  let rw = B.open_write ~sw path in
  check "migrated store answers" true (B.find_work rw a = None && B.known rw a = false
    && B.count_works rw = 0);
  let version =
    let s = Sqlite3.db_open ~mode:`READONLY (Eio.Path.native_exn path) in
    let st = Sqlite3.prepare s "PRAGMA user_version" in
    ignore (Sqlite3.step st); let v = Sqlite3.column_int st 0 in
    ignore (Sqlite3.finalize st); ignore (Sqlite3.db_close s); v in
  count "schema version 3" 3 version;
  raw "PRAGMA user_version=4";
  check "newer version refused" true
    (try ignore (B.open_read ~sw path); false with Failure _ -> true))

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in go 0

(* Works stored before the decoder knew a field are rebuilt from their
   retained bodies; sources are read singly and retained. *)
let venues () = with_temp (fun env sw root ->
  let path = root / "abstracts" in
  let store = B.open_write ~sw path in
  let calls = ref 0 in
  let http = Fetch_mock.client (fun req -> incr calls;
    let url = Fetch.Middleware.Url.to_string req.url in
    if contains ~sub:"filter=doi:" url then
      Fetch_mock.respond (results [work "10.1000/a"]) req
    else if contains ~sub:"/sources/" url then begin
      check "source select" true (contains ~sub:("select=" ^ B.source_select) url);
      if contains ~sub:"0000-0000" url then Fetch_mock.respond ~status:404 "{}" req
      else Fetch_mock.respond {|{"id":"https://openalex.org/S30982003","display_name":"Ecological Entomology","issn_l":"0307-6946","issn":["0307-6946"],"type":"journal"}|} req
    end else Fetch_mock.respond ~status:500 "unexpected" req) in
  let client = B.client ~clock:env#clock http in
  let a = doi "10.1000/a" in
  ignore (get (Result.map_error B.refusal_name (B.lookup_many client store [a])));
  let w = Option.get (B.find_work store a) in
  check "venue decoded on fetch" true (Option.map (fun v -> v.Work_evidence.id) w.venue = Some "S30982003");
  (* Forget the venue, as a version 2 store would have stored the work. *)
  let s = Sqlite3.db_open (Eio.Path.native_exn path) in
  let stripped = get (Jsont_bytesrw.encode_string Work_evidence.jsont {w with venue = None}) in
  let st = Sqlite3.prepare s "UPDATE works SET evidence=? WHERE doi='10.1000/a'" in
  Sqlite3.Rc.check (Sqlite3.bind_text st 1 stripped); ignore (Sqlite3.step st);
  ignore (Sqlite3.finalize st); ignore (Sqlite3.db_close s);
  check "venue forgotten" true ((Option.get (B.find_work store a)).venue = None);
  let examined, changed = B.redecode store in
  count "works examined" 1 examined; count "works rewritten" 1 changed;
  check "venue restored" true (Option.get (B.find_work store a) = w);
  check "redecode is idempotent" true (B.redecode store = (1, 0));
  let n = !calls in
  let v = get (Result.map_error B.refusal_name (B.lookup_source client store "issn:0307-6946")) in
  equal "source by issn" "Ecological Entomology" (Option.get (Option.get v).name);
  check "source retained" true (B.find_source store "issn:0307-6946" <> None && !calls = n + 1);
  check "retained source skips the network" true
    (B.lookup_source client store "issn:0307-6946" = Ok v && !calls = n + 1);
  check "unknown issn" true (B.lookup_source client store "issn:0000-0000" = Ok None);
  check "bad key refused" true
    (try ignore (B.lookup_source client store "junk"); false with Invalid_argument _ -> true);
  let seen = ref [] in B.iter_sources store (fun k _ -> seen := k :: !seen);
  check "sources iterated" true (!seen = ["issn:0307-6946"]))

let evidence d = {Abstract_evidence.doi=Doi.to_string d; abstract="From OpenAlex.";
  provider="openalex"; endpoint="http://x"; sha256=String.make 64 'a'; retrieved=1.}
let work_evidence ?(title="From OpenAlex") d =
  {Work_evidence.doi=Doi.to_string d; provider="openalex"; endpoint="http://x";
   sha256=String.make 64 'c'; retrieved=1.; venue=None;
   bibliography={Bibliography.empty with title=Some title; journal=Some "Journal";
     year=Some 2001; authors=[{Author.given=None; family="Plato"}]}}

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
  let works = [work_evidence (doi "10.1000/a"); work_evidence (doi "10.1000/b")] in
  let id = X.snapshot ~abstracts ~works store read ~run_id ~parent:None ~label:"L" ~actor:"t" in
  let rows = S.rows store id in
  let find d = List.find (fun (o : R.observation) -> o.doi = d) rows in
  equal "catalogued abstract kept" "Indexed." (Option.get (find "10.1000/a").bibliography.abstract);
  equal "uncatalogued member filled" "From OpenAlex." (Option.get (find "10.1000/b").bibliography.abstract);
  check "evidence labelled" true (List.exists (fun (e : R.evidence) ->
    e.field = "abstract" && String.starts_with ~prefix:"openalex" e.source)
    (find "10.1000/b").evidence);
  check "not catalogued" false (find "10.1000/b").in_catalogue;
  let b = find "10.1000/b" and a = find "10.1000/a" in
  equal "uncatalogued title filled" "From OpenAlex" (Option.get b.bibliography.title);
  equal "catalogued row's blank title filled" "From OpenAlex" (Option.get a.bibliography.title);
  check "work fields filled" true (b.bibliography.journal = Some "Journal"
    && b.bibliography.year = Some 2001 && List.length b.bibliography.authors = 1);
  let source field (o : R.observation) = List.find_map (fun (e : R.evidence) ->
    if e.field = field then Some e.source else None) o.evidence in
  equal "title source is the work label" (Work_evidence.label (List.hd works))
    (Option.get (source "title" b));
  equal "year source" (Work_evidence.label (List.hd works)) (Option.get (source "year" a));
  check "abstract source unchanged" true
    (Option.map (String.starts_with ~prefix:"openalex \xc2\xb7 aaaa") (source "abstract" b) = Some true);
  let o = R.fill {(List.hd works) with doi="10.1000/other"} b in
  check "fill ignores another doi" true (o = b))

let () = Alcotest.run "abstracts" [
  "decoding", [Alcotest.test_case "inverted index" `Quick reconstruction;
               Alcotest.test_case "work bibliography" `Quick works;
               Alcotest.test_case "batching" `Quick batching];
  "store", [Alcotest.test_case "lookups" `Quick lookups;
            Alcotest.test_case "version 1 migration" `Quick migration;
            Alcotest.test_case "venues and redecoding" `Quick venues];
  "resolver", [Alcotest.test_case "evidence" `Quick resolver];
  "catalogue", [Alcotest.test_case "fill" `Quick catalogue];
  "reviews", [Alcotest.test_case "snapshot evidence" `Quick snapshots]]
