open Taposaur
module A = Taposaur_accept
module L = Taposaur_lock
let ( / ) = Eio.Path.( / )
let get = function Ok x -> x | Error e -> Alcotest.fail e
let check = Alcotest.(check bool)
let count = Alcotest.(check int)
let equal = Alcotest.(check string)
let uid = get (Uid.of_input "10.1000/accept/paper")
let pdf = "%PDF-1.4\n0123456789\n%%EOF\n"
let tei = "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\"/>\n"
let at = "2026-10-23T12:00:00.000Z"
let sha s = Digestif.SHA256.(to_hex (digest_string s))
let temp_name prefix =
  let name = Filename.temp_file prefix "" in
  Unix.unlink name; Unix.mkdir name 0o700; Unix.realpath name
let rec rm path =
  if (Unix.lstat path).st_kind = Unix.S_DIR then begin
    Array.iter (fun n -> rm (Filename.concat path n)) (Sys.readdir path);
    Unix.rmdir path
  end else Unix.unlink path
let with_temp f = Eio_main.run (fun env ->
  let name = temp_name "taposaur-accept-" in
  let root = env#fs / name in
  Eio.Path.mkdir ~perm:0o700 (root / "papers");
  Eio.Path.mkdir ~perm:0o700 (root / "index");
  Fun.protect ~finally:(fun () -> rm name)
    (fun () -> Eio.Switch.run (fun sw -> f env sw name root)))
let save path value = Eio.Path.save ~create:(`Or_truncate 0o600) path value
let lines path = String.split_on_char '\n' (Eio.Path.load path)
  |> List.filter ((<>) "")
let artifact ?(format = Manifest.Format.Pdf)
    ?(source = Manifest.Source.Publisher) ?converter file =
  { Manifest.Artifact.file; format; source; version = None; converter;
    from = None; strategy = None; status = None; bytes = 0L; sha256 = None;
    at; url = None; origin = None; confidence = None }
let bytes ?format ?source ?converter file data =
  { A.source = A.Bytes data; artifact = artifact ?format ?source ?converter file }
let identity = { Bibliography.empty with title = Some "Accepted title";
  publisher = Some "Example Press"; year = Some 2024;
  doi = Some "10.1000/accept/paper" }
let paper_dir root = root / "papers" / Layout.paper uid
let accept ?identity ?clear_failures t additions =
  A.accept t ?identity ?clear_failures ~cmd:"test-accept" ~at uid additions

let lock () =
  let dir = temp_name "taposaur-lock-" in
  Fun.protect ~finally:(fun () -> rm dir) @@ fun () ->
  let path = Filename.concat dir ".lock" in
  let held = L.acquire ~component:"test.lock" path in
  (match L.acquire path with
   | _ -> Alcotest.fail "second in-process acquire succeeded"
   | exception L.Busy (Some o) ->
       count "owner pid" (Unix.getpid ()) o.pid;
       equal "owner component" "test.lock" o.component
   | exception L.Busy None -> Alcotest.fail "owner record missing");
  (match L.holder path with
   | Some o -> count "holder pid" (Unix.getpid ()) o.pid
   | None -> Alcotest.fail "holder missing");
  L.release held;
  L.release held;
  L.release (L.acquire path);
  let pid = Unix.create_process "/usr/bin/flock"
    [| "flock"; path; "-c"; "sleep 2" |] Unix.stdin Unix.stdout Unix.stderr in
  let busy () = match L.acquire path with
    | l -> L.release l; false | exception L.Busy _ -> true in
  let deadline = Unix.gettimeofday () +. 1. in
  let rec wait () =
    if busy () then true
    else if Unix.gettimeofday () > deadline then false
    else (Unix.sleepf 0.02; wait ()) in
  check "other process holds lock" true (wait ());
  ignore (Unix.waitpid [] pid);
  check "released after exit" false (busy ())

let new_paper () = with_temp (fun _ sw name root ->
  let t = A.open_root ~sw root in
  let r = accept ~identity t [bytes "paper.pdf" pdf] in
  let dir = paper_dir root in
  check "directory" true (Eio.Path.is_directory dir);
  equal "bytes" pdf (Eio.Path.load (dir / "paper.pdf"));
  count "events" 2 (List.length (lines (dir / "events.jsonl")));
  let kinds = List.map (fun l -> Event.kind (get (Event.of_string l)))
    (lines (dir / "events.jsonl")) in
  check "outcome events" true (kinds = ["outcome"; "outcome"]);
  (match r.added with
   | [a] -> equal "final name" "paper.pdf" a.file;
       check "digest" true (a.sha256 = Some (sha pdf));
       check "size" true (a.bytes = Int64.of_int (String.length pdf))
   | _ -> Alcotest.fail "one addition expected");
  count "no existing" 0 (List.length r.existing);
  let text = Eio.Path.load (dir / "paper.json") in
  equal "manifest digest" (sha text) r.manifest_sha256;
  let m = get (Manifest.of_string text) in
  check "title filled" true (m.title = Some "Accepted title");
  equal "updated" at m.updated;
  check "no intent" false (Eio.Path.is_file (dir / ".accept-pending.json"));
  check "archive reads paper" true
    (Taposaur_archive.find (Taposaur_archive.open_root ~sw root) uid <> None);
  check "row pdf path" true (match A.index_row t uid with
    | Some row -> row.pdf_path = name ^ "/papers/" ^ Layout.paper uid
        ^ "/paper.pdf"
    | None -> false);
  let again = accept ~identity t [bytes "paper.pdf" pdf] in
  count "repeat adds nothing" 0 (List.length again.added);
  count "repeat existing" 1 (List.length again.existing);
  equal "repeat digest" r.manifest_sha256 again.manifest_sha256;
  count "repeat events" 2 (List.length (lines (dir / "events.jsonl")));
  count "repeat files" 4 (Array.length (Sys.readdir
    (Eio.Path.native_exn dir))))

let naming () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  let dir = paper_dir root in
  let r = accept t [bytes "paper.pdf" (pdf ^ "v2")] in
  equal "numbered" "paper-2.pdf"
    (match r.added with [a] -> a.file | _ -> "");
  equal "original untouched" pdf (Eio.Path.load (dir / "paper.pdf"));
  equal "new bytes" (pdf ^ "v2") (Eio.Path.load (dir / "paper-2.pdf"));
  save (dir / "foreign.pdf") "foreign";
  let r = accept t [bytes "foreign.pdf" (pdf ^ "v3")] in
  equal "foreign avoided" "foreign-2.pdf"
    (match r.added with [a] -> a.file | _ -> "");
  equal "foreign untouched" "foreign" (Eio.Path.load (dir / "foreign.pdf"));
  check "unsafe name" true
    (try ignore (accept t [bytes "../x.pdf" pdf]); false
     with Invalid_argument _ -> true);
  check "dot name" true
    (try ignore (accept t [bytes ".hidden" pdf]); false
     with Invalid_argument _ -> true);
  let wrong = { (bytes "w.pdf" pdf) with artifact =
    { (artifact "w.pdf") with sha256 = Some (sha "other") } } in
  check "digest mismatch" true
    (try ignore (accept t [wrong]); false with Invalid_argument _ -> true);
  let staged = root / "staged.pdf" in
  save staged (pdf ^ "file");
  let r = accept t [{ A.source = A.File staged; artifact = artifact "f.pdf" }] in
  equal "file source" (pdf ^ "file")
    (Eio.Path.load (dir / (List.hd r.added).file)))

let identity_fill () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  let first = { Bibliography.empty with title = Some "Original";
    doi = Some "10.1000/accept/paper" } in
  ignore (accept ~identity:first t [bytes "paper.pdf" pdf]);
  let r = accept ~identity t [] in
  let m = Option.get (A.manifest t uid) in
  check "title retained" true (m.title = Some "Original");
  check "publisher filled" true (m.publisher = Some "Example Press");
  check "year filled" true (m.year = Some 2024);
  count "identity event" 3
    (List.length (lines (paper_dir root / "events.jsonl")));
  count "no additions" 0 (List.length r.added);
  let blank = { Bibliography.empty with pmid = Some " " } in
  ignore (accept ~identity:blank t []);
  check "blank ignored" true ((Option.get (A.manifest t uid)).pmid = None);
  count "no blank event" 3
    (List.length (lines (paper_dir root / "events.jsonl")));
  let other = { identity with doi = Some "10.1000/other" } in
  check "doi mismatch" true
    (try ignore (accept ~identity:other t []); false
     with Invalid_argument _ -> true))

let failures () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  let failure = { Manifest.Failure.route = Manifest.Route.Grobid; at;
    attempts = 0; reason = "converter refused"; permanent = false } in
  check "missing paper" true
    (try A.record_failure t ~cmd:"test" ~at uid failure; false
     with Failure _ -> true);
  ignore (accept t [bytes "paper.pdf" pdf]);
  A.record_failure t ~cmd:"test" ~at uid failure;
  A.record_failure t ~cmd:"test" ~at uid failure;
  let m = Option.get (A.manifest t uid) in
  (match m.conversions_failed with
   | [f] -> count "attempts" 2 f.attempts
   | _ -> Alcotest.fail "one failure expected");
  let errors = List.filter (fun l ->
    Event.kind (get (Event.of_string l)) = "error")
    (lines (paper_dir root / "events.jsonl")) in
  count "error events" 2 (List.length errors);
  ignore (accept ~clear_failures:[Manifest.Route.Grobid] t
    [bytes ~format:Tei ~source:Tei ~converter:"grobid" "tei.xml" tei]);
  count "cleared" 0
    (List.length (Option.get (A.manifest t uid)).conversions_failed))

let interrupted () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  let r = accept ~identity t [bytes "paper.pdf" pdf] in
  let dir = paper_dir root in
  let log = Eio.Path.load (dir / "events.jsonl") in
  let manifest = Eio.Path.load (dir / "paper.json") in
  let quote s = get (Jsont_bytesrw.encode_string Jsont.string s) in
  let intent = Printf.sprintf
    {|{"offset":0,"event":%s,"manifest":%s,"files":[{"tmp":".accept-paper.pdf.tmp","final":"paper.pdf","sha256":"%s","bytes":%d}]}|}
    (quote log) (quote manifest) (sha pdf) (String.length pdf) in
  save (dir / ".accept-pending.json") intent;
  Eio.Path.rename (dir / "paper.pdf") (dir / ".accept-paper.pdf.tmp");
  save (dir / "events.jsonl") (String.sub log 0 17);
  Eio.Path.unlink (dir / "paper.json");
  check "manifest recovered" true (A.manifest t uid <> None);
  equal "file restored" pdf (Eio.Path.load (dir / "paper.pdf"));
  check "tmp gone" false (Eio.Path.is_file (dir / ".accept-paper.pdf.tmp"));
  equal "log restored" log (Eio.Path.load (dir / "events.jsonl"));
  equal "manifest restored" manifest (Eio.Path.load (dir / "paper.json"));
  check "intent removed" false (Eio.Path.is_file (dir / ".accept-pending.json"));
  save (dir / ".accept-pending.json") intent;
  ignore (A.manifest t uid);
  equal "complete intent leaves log" log (Eio.Path.load (dir / "events.jsonl"));
  check "complete intent removed" false
    (Eio.Path.is_file (dir / ".accept-pending.json"));
  let again = accept ~identity t [bytes "paper.pdf" pdf] in
  equal "same digest" r.manifest_sha256 again.manifest_sha256;
  Eio.Path.unlink (dir / "paper.json");
  check "missing manifest refused" true
    (try ignore (A.manifest t uid); false with Failure _ -> true))

let index () = with_temp (fun _ sw name root ->
  let t = A.open_root ~sw root in
  check "no manifest" true
    (try ignore (A.refresh_index t uid); false with Failure _ -> true);
  ignore (accept ~identity t [bytes "paper.pdf" pdf]);
  check "added" true (A.refresh_index t uid = A.Added);
  let shard = root / "index" / Layout.index uid in
  let base = name ^ "/papers/" ^ Layout.paper uid ^ "/" in
  (match lines shard with
   | [l] -> let row = get (Index.of_string l) in
       equal "pdf path" (base ^ "paper.pdf") row.pdf_path;
       equal "no tei" "" row.tei_path;
       check "title" true (row.title = Some "Accepted title");
       check "pdf size" true (row.pdf_size = Int64.of_int (String.length pdf))
   | _ -> Alcotest.fail "one row expected");
  ignore (accept t [bytes ~format:Tei ~source:Tei ~converter:"grobid"
    "tei.xml" tei; bytes ~format:Xml ~source:Publisher "paper.xml" "<a/>"]);
  check "updated" true (A.refresh_index t uid = A.Updated);
  (match lines shard with
   | [l] -> let row = get (Index.of_string l) in
       equal "tei path" (base ^ "tei.xml") row.tei_path;
       check "tei source" true (row.tei_source = Some "grobid");
       check "has xml" true row.has_xml;
       check "no s2orc" false row.has_s2orc
   | _ -> Alcotest.fail "one row expected");
  check "unchanged" true (A.refresh_index t uid = A.Unchanged);
  let rec sibling n =
    let u = get (Uid.of_input (Printf.sprintf "10.1000/other/%d" n)) in
    if Layout.index u = Layout.index uid then u else sibling (n + 1) in
  let other = Uid.to_string (sibling 1) in
  let foreign = Printf.sprintf
    {|{"uid":%S,  "scheme":"doi","pdf_path":"","pdf_size":0,"pdf_mtime":0,"tei_path":"","tei_mtime":0}|}
    other in
  let ours = List.hd (lines shard) in
  let row = get (Index.of_string ours) in
  let kept = get (Index.to_string { row with title = Some "Indexed title" }) in
  save shard (foreign ^ "\n" ^ kept ^ "\n");
  check "stored bibliography kept" true (A.refresh_index t uid = A.Unchanged);
  save shard (foreign ^ "\n");
  check "appended" true (A.refresh_index t uid = A.Added);
  (match lines shard with
   | [f; l] -> equal "foreign bytes" foreign f;
       equal "our row" (Uid.to_string uid)
         (Uid.to_string (get (Index.of_string l)).uid)
   | _ -> Alcotest.fail "two rows expected"))

let store_lock () = with_temp (fun _ sw name root ->
  let store = Taposaur_store.open_root ~sw root in
  let path = name ^ "/" ^ Layout.paper uid ^ "/.lock" in
  Taposaur_store.with_paper store uid (fun _ ->
    match L.holder path with
    | Some o -> equal "component" "taposaur.store" o.component;
        count "pid" (Unix.getpid ()) o.pid
    | None -> Alcotest.fail "owner missing");
  check "released" true (L.holder path = None);
  let t = A.open_root ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  let held = L.acquire (Eio.Path.native_exn (paper_dir root) ^ "/.lock") in
  check "accept busy" true
    (try ignore (accept t []); false with A.Busy _ -> true);
  L.release held)

let audit () = with_temp (fun _ sw _ root ->
  let db = Taposaur_audit.open_write ~sw (root / "audit.db") in
  let session = Taposaur_audit.session ~component:"test.accept" db in
  let t = A.open_root ~audit:session ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  ignore (A.refresh_index t uid);
  let events = Taposaur_audit.query ~limit:500 db in
  let has action = List.exists (fun (e : Taposaur_audit.event) ->
    e.action = action && e.phase = Taposaur_audit.Succeeded &&
    e.subject = Uid.to_string uid) events in
  check "accept audited" true (has "archive.accept");
  check "refresh audited" true (has "archive.index.refresh"))

let mode path = (Unix.stat path).st_perm land 0o777
let listing dir = List.sort compare (Array.to_list (Sys.readdir dir))
let raises_failure f = try ignore (f ()); false with Failure _ -> true

let reserved () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  List.iter (fun name ->
    check ("reserved " ^ name) true
      (try ignore (accept t [bytes name (pdf ^ name)]); false
       with Invalid_argument _ -> true))
    ["paper.json"; "events.jsonl"; "x.tmp"; ".hidden"];
  count "nothing added" 1
    (List.length (Option.get (A.manifest t uid)).artifacts))

let torn_log () = with_temp (fun _ sw _ root ->
  let t = A.open_root ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  let dir = paper_dir root in
  let native = Eio.Path.native_exn dir in
  let log = Eio.Path.load (dir / "events.jsonl") in
  let torn = String.sub log 0 (String.length log - 1) in
  save (dir / "events.jsonl") torn;
  let before = listing native in
  check "incomplete line refused" true
    (raises_failure (fun () -> accept t [bytes "other.pdf" (pdf ^ "v2")]));
  check "no new file" true (listing native = before);
  equal "log bytes kept" torn (Eio.Path.load (dir / "events.jsonl")))

let permissions () = with_temp (fun _ sw name root ->
  let t = A.open_root ~sw root in
  ignore (accept ~identity t [bytes "paper.pdf" pdf]);
  let dir = name ^ "/papers/" ^ Layout.paper uid in
  count "directory mode" 0o755 (mode dir);
  count "artifact mode" 0o644 (mode (dir ^ "/paper.pdf"));
  count "manifest mode" 0o644 (mode (dir ^ "/paper.json"));
  check "added" true (A.refresh_index t uid = A.Added);
  let shard = name ^ "/index/" ^ Layout.index uid in
  count "shard mode" 0o644 (mode shard);
  Unix.chmod shard 0o640;
  ignore (accept t [bytes ~format:Tei ~source:Tei ~converter:"grobid"
    "tei.xml" tei]);
  check "updated" true (A.refresh_index t uid = A.Updated);
  count "shard mode kept" 0o640 (mode shard))

let lock_link () =
  let dir = temp_name "taposaur-lock-" in
  Fun.protect ~finally:(fun () -> rm dir) @@ fun () ->
  let target = Filename.concat dir "target" in
  Out_channel.with_open_bin target (fun oc -> output_string oc "keep me\n");
  let path = Filename.concat dir ".lock" in
  Unix.symlink target path;
  check "link refused" true (raises_failure (fun () -> L.acquire path));
  equal "target unchanged" "keep me\n"
    (In_channel.with_open_bin target In_channel.input_all);
  check "no holder through link" true (L.holder path = None)

let accept_lock_link () = with_temp (fun _ sw name root ->
  let t = A.open_root ~sw root in
  ignore (accept t [bytes "paper.pdf" pdf]);
  let dir = paper_dir root in
  let native = Eio.Path.native_exn dir in
  let target = name ^ "/target" in
  Out_channel.with_open_bin target (fun oc -> output_string oc "keep me\n");
  Unix.unlink (native ^ "/.lock");
  Unix.symlink target (native ^ "/.lock");
  let log = Eio.Path.load (dir / "events.jsonl") in
  let manifest = Eio.Path.load (dir / "paper.json") in
  let before = listing native in
  check "linked lock refused" true
    (try ignore (accept t [bytes "other.pdf" (pdf ^ "v2")]); false with
     | Failure _ -> true | A.Busy _ -> false);
  check "no new file" true (listing native = before);
  equal "log unchanged" log (Eio.Path.load (dir / "events.jsonl"));
  equal "manifest unchanged" manifest (Eio.Path.load (dir / "paper.json"));
  equal "target unchanged" "keep me\n"
    (In_channel.with_open_bin target In_channel.input_all))

let () = Alcotest.run "taposaur accept" [
  "lock", [Alcotest.test_case "ownership and contention" `Quick lock;
    Alcotest.test_case "symbolic link refused" `Quick lock_link];
  "accept", [
    Alcotest.test_case "new paper and repeat" `Quick new_paper;
    Alcotest.test_case "naming and validation" `Quick naming;
    Alcotest.test_case "identity" `Quick identity_fill;
    Alcotest.test_case "failures" `Quick failures;
    Alcotest.test_case "interrupted publication" `Quick interrupted;
    Alcotest.test_case "store lock" `Quick store_lock;
    Alcotest.test_case "audit" `Quick audit;
    Alcotest.test_case "reserved names" `Quick reserved;
    Alcotest.test_case "incomplete event log" `Quick torn_log;
    Alcotest.test_case "permissions" `Quick permissions;
    Alcotest.test_case "linked directory lock" `Quick accept_lock_link];
  "index", [Alcotest.test_case "refresh" `Quick index];
]
