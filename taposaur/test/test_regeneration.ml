open Taposaur
module P = Taposaur_regeneration
let get = function Ok x -> x | Error e -> failwith e
let xml ?(doi="10.1002/agj2.21016") ?(publisher="Wiley") () =
  Printf.sprintf {|<TEI xmlns="http://www.tei-c.org/ns/1.0">
<teiHeader><fileDesc><titleStmt><title>Soil carbon</title></titleStmt>
<publicationStmt><publisher>%s</publisher></publicationStmt>
<sourceDesc><biblStruct status="consolidated" source="glutton">
<analytic><idno type="DOI">%s</idno></analytic><monogr/>
</biblStruct></sourceDesc></fileDesc></teiHeader>
<text><body><p>Retained paper text.</p></body></text></TEI>|} publisher doi
let uid = get (Uid.of_input "10.1002/agj2.21016")
let refuses f = match f () with
  | _ -> Alcotest.fail "Expected refusal"
  | exception Failure _ -> ()
let validation () =
  ignore (P.validate uid (xml ()));
  refuses (fun () -> P.validate uid (xml ~doi:"10.1002/wrong" ()));
  refuses (fun () -> P.validate uid (xml ~publisher:"" ()));
  refuses (fun () -> P.validate uid
    (String.concat "" (String.split_on_char 'g' (xml ()))));
  refuses (fun () -> P.validate uid (xml () ^ "<broken>"))
let write path s = let ch = open_out_bin path in
  output_string ch s; close_out ch
let read path = let ch = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ch) (fun () ->
    really_input_string ch (in_channel_length ch))
let rec mkdir p = if not (Sys.file_exists p) then begin
  mkdir (Filename.dirname p); Unix.mkdir p 0o700 end
let publication () = Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
  let root = Filename.temp_file "taposaur-regeneration" "" in
  Unix.unlink root; Unix.mkdir root 0o700;
  let dir = Filename.concat (root ^ "/papers") (Layout.paper uid) in
  mkdir dir;
  let work = root ^ "/work" in mkdir work;
  let a = {Manifest.Artifact.file="publisher.pdf";format=Pdf;
    source=Publisher;version=None;converter=None;from=None;strategy=None;
    status=None;bytes=4L;sha256=None;at="old";url=None;origin=None;
    confidence=None} in
  let tei = {a with file="tei.xml";format=Tei;source=Tei;
    converter=Some "grobid"} in
  let m = {Manifest.v=1;id=uid;doi=Some "10.1002/agj2.21016";
    pmcid=None;pmid=None;title=None;publisher=None;year=None;
    corrections=[];artifacts=[a;tei];conversions_failed=[];updated="old"} in
  let old = get (Manifest.to_string m) in
  write (dir ^ "/paper.json") old;
  write (dir ^ "/tei.xml") "original";
  write (dir ^ "/events.jsonl") "old events\n";
  let audit_db = Taposaur_audit.open_write ~sw Eio.Path.(env#fs / (root ^ "/audit.db")) in
  let audit = Taposaur_audit.session ~component:"test.regeneration" audit_db in
  let publish () = P.publish ~audit ~root ~work uid ~input:a.file ~xml:(xml ()) in
  publish ();
  let events = read (dir ^ "/events.jsonl") in
  Alcotest.(check string) "backup" "original" (read (work ^ "/old.tei.xml"));
  Alcotest.(check string) "new TEI" (xml ()) (read (dir ^ "/tei.xml"));
  let projected = get (Manifest.of_string (read (dir ^ "/paper.json"))) in
  let artifact = List.nth projected.artifacts 1 in
  Alcotest.(check bool) "hash retained" true (artifact.sha256 <> None);
  publish ();
  Alcotest.(check string) "retry is idempotent" events
    (read (dir ^ "/events.jsonl"));
  (* Simulate interruption part-way through the event append. *)
  write (dir ^ "/paper.json") old;
  write (dir ^ "/events.jsonl") (String.sub events 0 30);
  publish ();
  Alcotest.(check string) "partial append recovered" events
    (read (dir ^ "/events.jsonl"));
  write (dir ^ "/events.jsonl") (events ^ "other writer\n");
  refuses publish;
  Alcotest.(check int) "successful publications audited" 3
    (List.length (Taposaur_audit.query ~action:"archive.tei.replace"
      ~phase:Succeeded audit_db));
  Alcotest.(check int) "concurrent-change refusal audited" 1
    (List.length (Taposaur_audit.query ~action:"archive.tei.replace"
      ~phase:Failed audit_db))))
let () = Alcotest.run "regeneration" ["header", [
  Alcotest.test_case "validation" `Quick validation;
  Alcotest.test_case "publication and recovery" `Quick publication]]
