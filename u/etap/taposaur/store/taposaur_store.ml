open Taposaur
let ( / ) = Eio.Path.( / )
exception Busy
exception Limit_exceeded
exception Empty_artifact
type t = { root : Eio.Fs.dir_ty Eio.Path.t; audit : Taposaur_audit.session option }
type paper = {
  audit : Taposaur_audit.session option;
  dir : Eio.Fs.dir_ty Eio.Path.t;
  mutable manifest : Manifest.t;
}
let get = function Ok x -> x | Error e -> failwith e
let manifest p = p.manifest
let open_root ?audit ~sw root = {root = Eio.Path.open_subtree ~sw root; audit}
let sync_dir = Taposaur_files.sync_dir
let exists = Taposaur_files.exists
let load = Taposaur_files.load
let atomic = Taposaur_files.atomic
let empty = Manifest.empty
let events dir =
  if exists (dir / "events.jsonl") then load (dir / "events.jsonl") else ""
let outcome json =
  let encoded = get (Jsont_bytesrw.encode_string Jsont.json json) in
  match Jsont_bytesrw.decode_string Manifest.Artifact.jsont encoded with
  | Ok a -> `Artifact a
  | Error _ ->
      let b = get (Bibliography.of_string encoded) in
      if b.doi = None then failwith "Unsupported staging outcome";
      `Identity b
let identified (m : Manifest.t) (b : Bibliography.t) =
  let fill old value = match old with None -> value | Some _ -> old in
  if m.doi <> b.doi then failwith "Identity names another DOI";
  {m with title = fill m.title b.title; publisher = fill m.publisher b.publisher;
    year = fill m.year b.year; pmcid = fill m.pmcid b.pmcid;
    pmid = fill m.pmid b.pmid}
let project id log =
  let lines = String.split_on_char '\n' log in
  let rec fold m = function
    | [] | [""] -> m
    | "" :: _ -> failwith "Empty staging event"
    | line :: rest ->
        let event = get (Event.of_string line) in
        if event.subject <> Some (Uid.to_string id) then
          failwith "Staging event identifies another paper";
        let m = match event.payload with
          | Event.Outcome json ->
              (match outcome json with
               | `Identity b -> { (identified m b) with updated = event.at }
               | `Artifact a ->
                   let artifacts = if List.exists
                     (fun (b : Manifest.Artifact.t) -> b.file = a.file)
                     m.Manifest.artifacts
                     then List.map (fun (b : Manifest.Artifact.t) ->
                       if b.file = a.file then a else b) m.artifacts
                     else m.artifacts @ [a] in
                   {m with artifacts; updated = event.at})
          | Error _ -> {m with updated = event.at}
          | _ -> failwith "Unsupported staging event" in
        fold m rest in
  fold (empty id) lines
let intent_codec =
  Jsont.Object.map (fun offset event -> offset, event)
  |> Jsont.Object.mem "offset" Jsont.int ~enc:fst
  |> Jsont.Object.mem "event" Jsont.string ~enc:snd
  |> Jsont.Object.finish
let digest = Taposaur_files.digest
let recover p =
  let pending = p.dir / ".pending.json" in
  if exists pending then begin
    let offset, event = get (Jsont_bytesrw.decode_string intent_codec
      (load pending)) in
    let log = events p.dir in
    let written = String.length log - offset in
    if offset < 0 || written < 0 || written > String.length event ||
        String.sub log offset written <> String.sub event 0 written then
      failwith "Staging log disagrees with pending publication";
    let decoded = get (Event.of_string (String.trim event)) in
    (match decoded.payload with
     | Event.Outcome json ->
         (match outcome json with
         | `Identity _ -> ()
         | `Artifact a ->
         if not (exists (p.dir / a.file)) then begin
           Eio.Path.rename (p.dir / ".transfer.tmp") (p.dir / a.file);
           sync_dir p.dir
         end;
         let sha, size = Eio.Path.with_open_in ~follow:false
           (p.dir / a.file) digest in
         if Some sha <> a.sha256 || Int64.of_int size <> a.bytes then
           failwith "Pending staging artifact is corrupt")
     | _ -> ());
    Eio.Path.with_open_out ~follow:false ~append:true
      ~create:(`If_missing 0o600) (p.dir / "events.jsonl") (fun file ->
        Eio.Flow.copy_string
          (String.sub event written (String.length event - written)) file;
        Eio.File.sync file);
    let m = project p.manifest.id (events p.dir) in
    atomic p.dir "paper.json" (get (Manifest.to_string m));
    Eio.Path.unlink pending;
    sync_dir p.dir;
    p.manifest <- m
  end else begin
    let m = project p.manifest.id (events p.dir) in
    if exists (p.dir / "paper.json") then begin
      let stored = get (Manifest.of_string (load (p.dir / "paper.json"))) in
      if stored <> m then atomic p.dir "paper.json" (get (Manifest.to_string m))
    end else if events p.dir <> "" then
      atomic p.dir "paper.json" (get (Manifest.to_string m));
    p.manifest <- m
  end
let with_paper (t : t) id f =
  let dir = t.root / Layout.paper id in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let rec sync_parents relative =
    if relative = "." then sync_dir t.root else begin
      sync_dir (t.root / relative);
      sync_parents (Filename.dirname relative)
    end in
  sync_parents (Layout.paper id);
  Eio.Path.with_subtree dir (fun dir ->
    let owned () = exists (dir / ".taposaur-stage") in
    let refuse_foreign () =
      if not (owned ()) && (exists (dir / "paper.json") ||
          exists (dir / "events.jsonl")) then
        failwith "Refusing to modify a paper not created by this staging writer" in
    refuse_foreign ();
    let lock = try Taposaur_lock.acquire ~component:"taposaur.store"
        (Eio.Path.native_exn (dir / ".lock"))
      with Taposaur_lock.Busy _ -> raise Busy in
    Fun.protect ~finally:(fun () -> Taposaur_lock.release lock) (fun () ->
      refuse_foreign ();
      if owned () then begin
        if load (dir / ".taposaur-stage") <> "1\n" then
          failwith "Unsupported staging writer version"
      end else atomic dir ".taposaur-stage" "1\n";
      let p = {dir; manifest = empty id; audit=t.audit} in
      Eio.Cancel.protect (fun () -> recover p);
      f p))
let append p ~at payload =
  let log = events p.dir in
  let seq = 1 + List.length (List.filter ((<>) "")
    (String.split_on_char '\n' log)) in
  let event = get (Event.to_string {Event.v = 1; seq; at; cmd = "fetch";
    subject = Some (Uid.to_string p.manifest.id); payload}) ^ "\n" in
  if String.length log + String.length event >= 16 * 1024 * 1024 then
    failwith "Staging event log exceeds 16 MiB";
  atomic p.dir ".pending.json"
    (get (Jsont_bytesrw.encode_string intent_codec (String.length log, event)));
  recover p
let identify p ~at bibliography =
  if identified p.manifest bibliography <> p.manifest then begin
    let json = get (Jsont_bytesrw.decode_string Jsont.json
      (get (Bibliography.to_string bibliography))) in
    Eio.Cancel.protect (fun () -> append p ~at (Event.Outcome json))
  end
let add ?(max_bytes = 128 * 1024 * 1024) p ~at ~source ~format
    ~strategy ~status ~url ~validate flow =
  if max_bytes < 1 then invalid_arg "max_bytes must be positive";
  let tmp = p.dir / ".transfer.tmp" in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () ->
      if exists tmp && not (exists (p.dir / ".pending.json")) then
        Eio.Path.unlink tmp))
    (fun () ->
      let bytes = ref 0 and ctx = ref Digestif.SHA256.empty in
      Eio.Path.with_open_out ~follow:false ~create:(`Or_truncate 0o600) tmp
        (fun file ->
          let buffer = Cstruct.create 65536 in
          (try while true do
            let n = Eio.Flow.single_read flow buffer in
            if n > max_bytes - !bytes then raise Limit_exceeded;
            bytes := !bytes + n;
            ctx := Digestif.SHA256.feed_string !ctx
              (Cstruct.to_string ~len:n buffer);
            Eio.Flow.write file [Cstruct.sub buffer 0 n]
          done with End_of_file -> ());
          if !bytes = 0 then raise Empty_artifact;
          Eio.File.sync file);
      Eio.Path.with_open_in ~follow:false tmp validate;
      let sha256 = Digestif.SHA256.(to_hex (get !ctx)) in
      match List.find_opt (fun (a : Manifest.Artifact.t) ->
        a.source = source && a.format = format && a.sha256 = Some sha256)
        p.manifest.artifacts with
      | Some a ->
          let actual, size = Eio.Path.with_open_in ~follow:false
            (p.dir / a.file) digest in
          if actual <> sha256 || Int64.of_int size <> a.bytes then
            failwith "Existing staging artifact is corrupt";
          a
      | None ->
          let rec name n =
            let file = Manifest.Source.to_string source ^
              (if n = 1 then "" else "-" ^ string_of_int n) ^ "." ^
              Manifest.Format.to_string format in
            if exists (p.dir / file) then name (n + 1) else file in
          let file = name 1 in
          let a = {Manifest.Artifact.file; source; format;
            version = None; converter = None; from = None;
            strategy = Some strategy; status = Some status;
            bytes = Int64.of_int !bytes; sha256 = Some sha256; at;
            url = Some url; origin = None; confidence = Some Manifest.Confidence.Observed}
          in
          let json = get (Jsont_bytesrw.decode_string Jsont.json
            (get (Jsont_bytesrw.encode_string Manifest.Artifact.jsont a))) in
          Eio.Cancel.protect (fun () ->
            append p ~at (Event.Outcome json));
          a)
let record_error p ~at ~source message =
  if List.mem source ["grobid"; "pub2tei"; "s2orc"] then
    invalid_arg "Converter errors require a conversion writer";
  Eio.Cancel.protect (fun () ->
    append p ~at (Event.Error {message; source = Some source}))

let with_paper (t : t) id f =
  Taposaur_audit.with_operation ?session:t.audit
    ~action:"staging.paper" ~subject:(Uid.to_string id)
    ~details:["directory",Eio.Path.native_exn (t.root / Layout.paper id)]
    (fun audit -> with_paper {t with audit} id f)
let identify p ~at bibliography =
  Taposaur_audit.with_operation ?session:p.audit
    ~action:"staging.identify" ~subject:(Uid.to_string p.manifest.id)
    (fun _ -> identify p ~at bibliography)
let add ?max_bytes p ~at ~source ~format ~strategy ~status ~url ~validate flow =
  Taposaur_audit.with_operation ?session:p.audit
    ~action:"staging.artifact.publish" ~subject:(Uid.to_string p.manifest.id)
    ~details:["source",Manifest.Source.to_string source;
      "format",Manifest.Format.to_string format;"strategy",strategy;
      "http_status",string_of_int status]
    (fun audit ->
      let a = add ?max_bytes p ~at ~source ~format ~strategy ~status ~url
        ~validate flow in
      Option.iter (fun session -> Taposaur_audit.record session
        ~action:"staging.artifact.stored" ~subject:(Uid.to_string p.manifest.id)
        ~details:["file",a.file;"sha256",Option.value ~default:"" a.sha256;
          "bytes",Int64.to_string a.bytes] ()) audit;
      a)
let record_error p ~at ~source message =
  Taposaur_audit.with_operation ?session:p.audit
    ~action:"staging.failure.record" ~subject:(Uid.to_string p.manifest.id)
    ~details:["source",source] (fun _ -> record_error p ~at ~source message)
