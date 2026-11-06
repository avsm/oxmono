open Taposaur
module M = Taposaur_mirrors
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
let doi = get (Doi.of_string "10.1000/a")
let uid = get (Uid.of_input "10.1000/a")
let body = {|{"DOI":"10.1000/A","publisher":"Publisher & Co"}|}
let evidence () = get (M.decode ~provider:"crossref.glutton" ~endpoint:"http://mirror" ~doi body)
let with_temp f = Tap_test.with_temp "mirror-test-"
  (fun env sw root _ -> f env sw root)
let validation () =
  equal "canonical match" "10.1000/a" (evidence ()).doi;
  List.iter (fun body -> check "refuse invalid evidence" true
    (Result.is_error (M.decode ~provider:"test" ~endpoint:"http://mirror" ~doi body)))
    ["broken";"{}";{|{"DOI":"10.1000/b","publisher":"Wrong"}|};
     {|{"DOI":"10.1000/a","publisher":" "}|}]
let lookup () = with_temp (fun env sw root ->
  let audit_db=Taposaur_audit.open_write ~sw (root / "audit") in
  let audit=Taposaur_audit.session ~component:"test" audit_db in
  let store=M.open_write ~audit ~sw (root / "mirrors") in
  let calls=ref 0 in
  let http=Fetch_mock.client (fun req -> incr calls;
    Fetch_mock.respond (if !calls=1 then
      {|{"DOI":"10.1000/wrong","publisher":"Wrong"}|} else body) req) in
  let client=M.client ~audit ~clock:env#clock
    ~providers:[M.Glutton "http://mirror";M.Datacite_clickhouse "http://clickhouse"] http in
  let e=Option.get (M.lookup client store doi) in
  equal "fallback provider" "datacite.clickhouse" e.provider;
  equal "raw record retained" body (Option.get (M.record store ~sha256:e.sha256));
  ignore (M.lookup client store doi); count "cache avoids requests" 2 !calls;
  let events=Taposaur_audit.query ~action:"mirrors.publisher.lookup" audit_db in
  count "two starts and terminals" 4 (List.length events);
  check "mismatch audited as failed" true
    (List.exists (fun (e:Taposaur_audit.event) -> e.phase=Failed) events);
  check "default actor" true (List.for_all (fun (e:Taposaur_audit.event) -> e.actor="system") events);
  let ro=M.open_read ~sw (root / "mirrors") in
  equal "reopened evidence" e.publisher (Option.get (M.find ro doi)).publisher)
let timeouts () = with_temp (fun env sw root ->
  let store=M.open_write ~sw (root / "mirrors") in
  let http=Fetch_mock.client (fun _ -> Eio.Time.sleep env#clock 1.;assert false) in
  let c=M.client ~timeout:0.01 ~clock:env#clock ~providers:[Glutton "http://mirror"] http in
  check "timeout is recoverable" true (M.lookup c store doi=None);
  let c=M.client ~clock:env#clock ~providers:[] http in
  check "no services configured" true (M.lookup c store doi=None))
let csv () = with_temp (fun _ sw root ->
  let store=M.open_write ~sw (root / "mirrors") in
  M.import_csv store ~path:"original.csv"
    "doi,publisher\r\n10.1000/a,\"Publisher, \"\"One\"\"\"\r\n10.1000/b,First\r\n10.1000/b,Second\r\n";
  equal "quoted field" "Publisher, \"One\"" (Option.get (M.fallback store doi)).publisher;
  check "conflicting duplicates refused" true
    (M.fallback store (get (Doi.of_string "10.1000/b"))=None);
  check "bad CSV rejected" true (try M.import_csv store ~path:"bad.csv"
    "doi,publisher\n10.1000/c,C\nwrong-width\n";false with Invalid_argument _ -> true);
  check "whole import rolled back" true
    (M.fallback store (get (Doi.of_string "10.1000/c"))=None))
let make_index () = get (Index.of_string
  {|{"uid":"doi:10.1000/a","scheme":"doi","pdf_path":"","pdf_size":0,"pdf_mtime":0,"tei_path":"","tei_mtime":0}|})
let catalogue () = with_temp (fun _ sw root ->
  let c=C.open_write ~sw (root / "catalog") in
  let index=make_index () in
  ignore (C.Shard.replace c ~shard:(Layout.index uid) {size=1L;mtime=1.}
    (fun emit -> emit index));
  let candidate ()=List.hd (C.Metadata.pending ~force:true ~uid ~limit:1 ~version:P.version c) in
  let manifest=get (Manifest.of_string
    {|{"v":1,"id":"doi:10.1000/a","artifacts":[],"updated":"2026-10-31T00:00:00Z"}|}) in
  let p=P.resolve ~index manifest [] in
  let old=get (P.cache_string p) in
  ignore (C.Metadata.publish ~detail_cache:old ~source_stamp:"original" c (candidate ())
    ~version:P.version (Ok (P.bibliography p)));
  let stale_candidate=candidate () in
  ignore (C.Metadata.invalidate c stale_candidate);
  let in_flight=candidate () in
  let before=Option.get (C.Metadata.detail c uid) in
  let apply e=C.Metadata.fill_publisher c ~uid ~publisher:e.Publisher_evidence.publisher
    ~update_detail:(fun index raw -> get (P.cache_string
      (P.with_publisher e (get (P.of_cache_string ~index raw))))) in
  check "first fill applied" true (apply (evidence ()));
  check "second fill refused" false (apply {(evidence ()) with publisher="Different"});
  check "old resolver fenced" false
    (C.Metadata.publish c in_flight ~version:P.version (Ok Bibliography.empty));
  let after=Option.get (C.Metadata.detail c uid) in
  check "freshness preserved" true (before.stale=after.stale && before.saved_at=after.saved_at);
  let p=get (P.of_cache_string ~index after.json) in
  equal "cached publisher" "Publisher & Co" (Option.get (P.bibliography p).publisher);
  check "provenance round trips" true (p.publisher_evidence<>None);
  check "old cache still readable" true ((get (P.of_cache_string ~index old)).publisher_evidence=None);
  count "cache counts unchanged" 1 (C.Metadata.detail_count c))
let snapshots () = with_temp (fun _ sw root ->
  let s=S.open_write ~sw (root / "reviews") in
  ignore (C.open_write ~sw (root / "catalog"));
  let c=C.open_read ~sw (root / "catalog") in
  let module D=R.Definition in
  let blob=S.put_blob s "10.1000/a\n" in
  let input={D.key="list";label="List";source={D.kind=List;format=Doi_lines;
    blob=Some blob;local=None;provider=None;query=None;parameters=None}} in
  let definition={D.version=1;inputs=[input];include_any=["list"];require_all=[];exclude_any=[]} in
  ignore (S.define s ~slug:"test" ~title:"Test" ~requester:"Researcher" ~owner:"system"
    ~actor:"system" ~reason:"Test" ~expected:0 definition);
  let run_id=X.execute s c (S.revision s ~slug:"test" ~number:None) ~actor:"system" [] in
  let first=X.snapshot s c ~run_id ~parent:None ~label:"Before" ~actor:"system" in
  let second=X.snapshot ~publishers:[evidence ()] s c ~run_id ~parent:(Some first)
    ~label:"Recovered" ~actor:"system" in
  let before=List.hd (S.rows s first) and after=List.hd (S.rows s second) in
  check "old snapshot unchanged" true (before.bibliography.publisher=None);
  check "no catalogue paper fabricated" false after.in_catalogue;
  equal "outside catalogue publisher" "Publisher & Co" (Option.get after.bibliography.publisher);
  check "source frozen" true
    (List.exists (fun (e:R.evidence) -> e.field="publisher" && e.source=Publisher_evidence.label (evidence ())) after.evidence))
let () = Alcotest.run "publisher mirrors" ["mirrors",List.map (fun (n,f) ->
  Alcotest.test_case n `Quick f) ["validation",validation;"lookup and audit",lookup;
    "timeouts",timeouts;"csv",csv;"catalogue fencing and cache",catalogue;
    "immutable snapshots",snapshots]]
