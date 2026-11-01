open Taposaur
let get = function Ok x -> x | Error e -> failwith e
let consolidated xml =
  let input = Xmlm.make_input (`String (0, xml)) in
  let stack = ref [] and found = ref false in
  while not (Xmlm.eoi input) do
    match Xmlm.input input with
    | `El_start ((ns, name), attrs) ->
        if ns = "http://www.tei-c.org/ns/1.0" && name = "biblStruct" &&
           !stack = ["sourceDesc"; "fileDesc"; "teiHeader"; "TEI"] then
          found := List.assoc_opt ("", "status") attrs = Some "consolidated"
            && List.assoc_opt ("", "source") attrs = Some "glutton";
        stack := name :: !stack
    | `El_end -> stack := List.tl !stack
    | _ -> ()
  done;
  !found
let validate uid xml =
  get (Taposaur_document.Tei.validate_string xml);
  let b = get (Taposaur_document.Tei.metadata_string xml) in
  let doi = match b.doi with
    | None -> failwith "Consolidated header has no DOI"
    | Some s -> get (Uid.of_input s) in
  if doi <> uid then failwith "Consolidated header DOI mismatch";
  if not (Bibliography.presence b).publisher then
    failwith "Consolidated header has no publisher";
  if not (consolidated xml) then
    failwith "TEI has no Glutton consolidation evidence";
  b
let load path =
  let ch = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ch) (fun () ->
    really_input_string ch (in_channel_length ch))
let sync_dir path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd)
let atomic path bytes =
  let tmp = path ^ ".header-new" in
  (match Unix.lstat tmp with
   | st when st.st_kind = Unix.S_REG -> Unix.unlink tmp
   | _ -> failwith "Publication temporary path is not a regular file"
   | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let fd = Unix.openfile tmp
    [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec write i = if i < String.length bytes then
      let n = Unix.write_substring fd bytes i (String.length bytes - i) in
      if n = 0 then failwith "Short write" else write (i+n) in
    write 0; Unix.fsync fd);
  Unix.rename tmp path; sync_dir (Filename.dirname path)
let now () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z"
    (t.tm_year+1900) (t.tm_mon+1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
let quote s = get (Jsont_bytesrw.encode_string Jsont.string s)
let regular path =
  if (Unix.lstat path).st_kind <> Unix.S_REG then
    failwith ("Not a regular file: " ^ path)
let publish ~root ~work uid ~input ~xml =
  ignore (validate uid xml);
  let dir = Filename.concat (Filename.concat root "papers") (Layout.paper uid) in
  let file n = Filename.concat dir n and saved n = Filename.concat work n in
  let lock = Unix.openfile (saved "publication.lock")
    [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close lock) (fun () ->
  Unix.lockf lock Unix.F_TLOCK 0;
  regular (file "paper.json"); regular (file "events.jsonl");
  if not (Sys.file_exists (saved "intent")) then begin
    let old = load (file "paper.json") in
    let m = get (Manifest.of_string old) in
    if m.id <> uid then failwith "Manifest identity mismatch";
    if not (List.exists (fun (a : Manifest.Artifact.t) ->
      a.file = input && a.format = Manifest.Format.Pdf) m.artifacts) then
      failwith "Input PDF is not in manifest";
    let existing = List.find_opt (fun (a : Manifest.Artifact.t) ->
      a.format = Manifest.Format.Tei && a.converter = Some "grobid")
      m.artifacts in
    let target = match existing with Some a -> a.file | None ->
      let rec next n =
        let name = if n = 1 then "tei.xml"
          else Printf.sprintf "tei-%d.xml" n in
        if Sys.file_exists (file name) || List.exists
          (fun (a : Manifest.Artifact.t) -> a.file = name) m.artifacts
        then next (n+1) else name in next 1 in
    if Sys.file_exists (file target) then begin
      regular (file target); atomic (saved "old.tei.xml") (load (file target))
    end;
    let at = now () in
    let artifact = {Manifest.Artifact.file = target; format = Tei;
      source = Tei; version = None; converter = Some "grobid";
      from = Some input; strategy = Some "consolidate-header=1";
      status = Some 200; bytes = Int64.of_int (String.length xml);
      sha256 = Some Digestif.SHA256.(to_hex (digest_string xml));
      at; url = None; origin = None; confidence = Some Observed} in
    let artifacts = match existing with
      | None -> m.artifacts @ [artifact]
      | Some _ -> List.map (fun (a : Manifest.Artifact.t) ->
          if a.file = target then artifact else a) m.artifacts in
    let manifest = {m with artifacts; updated = at;
      conversions_failed = List.filter (fun (f : Manifest.Failure.t) ->
        f.route <> Grobid) m.conversions_failed} in
    let event seq payload = get (Event.to_string {Event.v=1; seq; at;
      cmd="consolidate-headers"; subject=Some (Uid.to_string uid);
      payload=Outcome (get (Jsont_bytesrw.decode_string Jsont.json payload))})
      ^ "\n" in
    let conversion = Printf.sprintf
      "{\"input\":%s,\"output\":%s,\"converter\":\"grobid\",\"bytes\":%d}"
      (quote input) (quote target) (String.length xml) in
    let details = get (Jsont_bytesrw.encode_string Manifest.Artifact.jsont
      artifact) in
    let events = load (file "events.jsonl") in
    if events <> "" && not (String.ends_with ~suffix:"\n" events) then
      failwith "Archive event log has an incomplete final line";
    atomic (saved "old.paper.json") old;
    atomic (saved "old.events.jsonl") events;
    atomic (saved "new.tei.xml") xml;
    atomic (saved "new.paper.json") (get (Manifest.to_string manifest));
    atomic (saved "append.events.jsonl") (event 1 conversion ^ event 2 details);
    atomic (saved "intent") target
  end;
  let target = load (saved "intent") in
  let before = load (saved "old.paper.json") in
  let after = load (saved "new.paper.json") in
  let current = load (file "paper.json") in
  if current <> before && current <> after then
    failwith "Manifest changed since publication was prepared";
  let old_events = load (saved "old.events.jsonl") in
  let extra = load (saved "append.events.jsonl") in
  let current_events = load (file "events.jsonl") in
  let expected = old_events ^ extra in
  if not (String.starts_with ~prefix:old_events current_events &&
          String.starts_with ~prefix:current_events expected) then
    failwith "Event log changed since publication was prepared";
  let replacement = load (saved "new.tei.xml") in
  ignore (validate uid replacement);
  if Sys.file_exists (file target) then begin
    regular (file target);
    let bytes = load (file target) in
    if bytes <> replacement &&
       (not (Sys.file_exists (saved "old.tei.xml")) ||
        bytes <> load (saved "old.tei.xml")) then
      failwith "TEI changed since publication was prepared"
  end;
  atomic (file target) replacement;
  let offset = String.length current_events in
  let fd = Unix.openfile (file "events.jsonl")
    [Unix.O_WRONLY; Unix.O_APPEND] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec append i = if i < String.length expected then
      let n = Unix.write_substring fd expected i (String.length expected-i) in
      if n = 0 then failwith "Short event write" else append (i+n) in
    append offset; Unix.fsync fd);
  atomic (file "paper.json") after;
  atomic (saved "published") (now ()))

let publish ?audit ~root ~work uid ~input ~xml =
  Taposaur_audit.with_operation ?session:audit
    ~action:"archive.tei.replace" ~subject:(Uid.to_string uid)
    ~details:["directory",Filename.concat (Filename.concat root "papers") (Layout.paper uid);
      "recovery_directory",work;"input",input;
      "sha256",Digestif.SHA256.(to_hex (digest_string xml));
      "bytes",string_of_int (String.length xml)]
    ~result_details:(fun () ->
      let previous = Filename.concat work "old.tei.xml" in
      ["target",load (Filename.concat work "intent");
       "previous_sha256",(if Sys.file_exists previous then
         Digestif.SHA256.(to_hex (digest_string (load previous))) else "")])
    (fun _ -> publish ~root ~work uid ~input ~xml)
