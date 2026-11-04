open Taposaur
let ( / ) = Eio.Path.( / )
type t = { root : Eio.Fs.dir_ty Eio.Path.t; native : string;
  audit : Taposaur_audit.session option }
exception Busy of Taposaur_lock.owner option
let mutable_fields = ["title"; "publisher"; "year"; "pmid"; "pmcid"]
type source = File of Eio.Fs.dir_ty Eio.Path.t | Bytes of string
type addition = { source : source; artifact : Manifest.Artifact.t }
type receipt = {
  uid : Uid.t;
  added : Manifest.Artifact.t list;
  existing : Manifest.Artifact.t list;
  manifest_sha256 : string;
}
type index_change = Added | Updated | Unchanged

let () = Printexc.Safe.register_printer (function
  | Busy o -> Some (Printexc.to_string (Taposaur_lock.Busy o))
  | _ -> None)

let get = function Ok x -> x | Error e -> failwith e
let open_root ?audit ~sw root =
  let native = Unix.realpath (Eio.Path.native_exn root) in
  { root = (Eio.Path.open_subtree ~sw root :> Eio.Fs.dir_ty Eio.Path.t);
    native; audit }
let sync_dir = Taposaur_files.sync_dir
let exists = Taposaur_files.exists
let load ?max path = Taposaur_files.load ?max_bytes:max path
let log_limit = 256 * 1024 * 1024
let atomic dir name value =
  Taposaur_files.atomic ~perm:(Taposaur_files.mode_of dir name 0o644)
    dir name value
let sha_string s = Digestif.SHA256.(to_hex (digest_string s))
let digest_path = Taposaur_files.digest_path
let json_of codec v = get (Jsont_bytesrw.decode_string Jsont.json
  (get (Jsont_bytesrw.encode_string codec v)))

(* Lock files live beside the papers, which may be on CephFS, so the
   blocking open and flock run off the scheduler. *)
let locked path f =
  let lock = try Eio_unix.run_in_systhread (fun () ->
      Taposaur_lock.acquire ~component:"taposaur.accept" path)
    with Taposaur_lock.Busy o -> raise (Busy o) in
  Fun.protect ~finally:(fun () ->
    Eio_unix.run_in_systhread (fun () -> Taposaur_lock.release lock)) f

(* The intent names every effect of one publication, so recovery needs no
   other state: the files to rename, the log suffix and the manifest. *)
type file = { tmp : string; final : string; sha256 : string; bytes : int }
type intent = { offset : int; event : string; manifest : string;
  files : file list }
let file_codec =
  Jsont.Object.map (fun tmp final sha256 bytes -> { tmp; final; sha256; bytes })
  |> Jsont.Object.mem "tmp" Jsont.string ~enc:(fun f -> f.tmp)
  |> Jsont.Object.mem "final" Jsont.string ~enc:(fun f -> f.final)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun f -> f.sha256)
  |> Jsont.Object.mem "bytes" Jsont.int ~enc:(fun f -> f.bytes)
  |> Jsont.Object.finish
let intent_codec =
  Jsont.Object.map (fun offset event manifest files ->
    { offset; event; manifest; files })
  |> Jsont.Object.mem "offset" Jsont.int ~enc:(fun i -> i.offset)
  |> Jsont.Object.mem "event" Jsont.string ~enc:(fun i -> i.event)
  |> Jsont.Object.mem "manifest" Jsont.string ~enc:(fun i -> i.manifest)
  |> Jsont.Object.mem "files" (Jsont.list file_codec) ~enc:(fun i -> i.files)
  |> Jsont.Object.finish
let pending_name = ".accept-pending.json"
let events dir =
  if exists (dir / "events.jsonl") then load ~max:log_limit
    (dir / "events.jsonl") else ""

let valid dir f =
  let p = dir / f.tmp in
  exists p && digest_path p = (f.sha256, f.bytes)

(* [link] refuses an existing target, unlike [rename], so a file created
   by a writer outside this protocol is never replaced. *)
let place dir f =
  let native name = Eio.Path.native_exn (dir / name) in
  Eio_unix.run_in_systhread (fun () ->
    Unix.link (native f.tmp) (native f.final);
    Unix.unlink (native f.tmp))

let recover dir =
  let pending = dir / pending_name in
  if exists pending then begin
    let i = get (Jsont_bytesrw.decode_string intent_codec (load pending)) in
    let log = events dir in
    let written = String.length log - i.offset in
    if i.offset < 0 || written < 0 || written > String.length i.event ||
        String.sub log i.offset written <> String.sub i.event 0 written then
      failwith "Archive log disagrees with pending publication";
    let finals = List.exists (fun f -> exists (dir / f.final)) i.files in
    if written = 0 && not finals && not (List.for_all (valid dir) i.files)
    then begin
      (* Copying never finished, so nothing became visible. Abandon it. *)
      List.iter (fun f -> if exists (dir / f.tmp) then
        Eio.Path.unlink (dir / f.tmp)) i.files;
      Eio.Path.unlink pending;
      sync_dir dir
    end else begin
      List.iter (fun f ->
        if exists (dir / f.final) then begin
          if digest_path (dir / f.final) <> (f.sha256, f.bytes) then
            failwith ("Archive artifact disagrees with pending publication: "
              ^ f.final)
        end else if valid dir f then place dir f
        else failwith ("Pending archive artifact is missing: " ^ f.final))
        i.files;
      if i.files <> [] then sync_dir dir;
      if written < String.length i.event then
        Eio.Path.with_open_out ~follow:false ~append:true
          ~create:(`If_missing 0o644) (dir / "events.jsonl") (fun file ->
            Eio.Flow.copy_string (String.sub i.event written
              (String.length i.event - written)) file;
            Eio.File.sync file);
      atomic dir "paper.json" i.manifest;
      Eio.Path.unlink pending;
      sync_dir dir
    end
  end

let paper_rel uid = "papers/" ^ Layout.paper uid
let read_manifest uid dir =
  let path = dir / "paper.json" in
  if exists path then begin
    let text = load path in
    let m = get (Manifest.of_string text) in
    if Uid.compare m.Manifest.id uid <> 0 then
      failwith "Archive manifest names a different paper";
    Some (m, text)
  end else if exists (dir / "events.jsonl") then
    failwith "Archive manifest missing beside its event log"
  else None

let with_dir ?(create = false) t uid f =
  let rel = paper_rel uid in
  let dir = t.root / rel in
  if create then begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dir;
    let rec sync_parents relative =
      if relative = "." then sync_dir t.root else begin
        sync_dir (t.root / relative);
        sync_parents (Filename.dirname relative)
      end in
    sync_parents rel
  end;
  if not create && not (exists dir) then None else
  Some (locked (t.native ^ "/" ^ rel ^ "/.lock") (fun () ->
    Eio.Cancel.protect (fun () -> recover dir);
    f dir))

let empty = Manifest.empty

let count_lines log =
  List.length (List.filter ((<>) "") (String.split_on_char '\n' log))

(* [publish dir ~log m events files] makes [m] and [events] durable. The
   temporary files are copied by [copy] after the intent is synced. *)
let publish dir ~log ~manifest ~events:ev ~files copy =
  if log <> "" && not (String.ends_with ~suffix:"\n" log) then
    failwith "Archive event log has an incomplete final line";
  let i = { offset = String.length log; event = String.concat "" ev;
    manifest; files } in
  if String.length log + String.length i.event >= log_limit then
    failwith "Archive event log exceeds 256 MiB";
  Eio.Cancel.protect (fun () ->
    atomic dir pending_name (get (Jsont_bytesrw.encode_string intent_codec i));
    (try copy () with e ->
      let bt = Printexc.get_raw_backtrace () in
      List.iter (fun f -> if exists (dir / f.tmp) then
        Eio.Path.unlink (dir / f.tmp)) files;
      Eio.Path.unlink (dir / pending_name);
      sync_dir dir;
      Printexc.raise_with_backtrace e bt);
    recover dir)

let event_line ~cmd ~uid ~seq ~at payload =
  get (Event.to_string { Event.v = 1; seq; at; cmd;
    subject = Some (Uid.to_string uid); payload }) ^ "\n"

(* Names the writer itself uses can never be artifact names, or an
   artifact could be appended to or replaced by bookkeeping. *)
let reserved = ["paper.json"; "events.jsonl"]
let check_file name =
  if name = "" || name = "." || name = ".." || String.contains name '/' ||
      String.contains name '\\' || String.contains name '\000' ||
      String.starts_with ~prefix:"." name || List.mem name reserved ||
      Filename.check_suffix name ".tmp" then
    invalid_arg ("Unsafe artifact filename: " ^ name)

let source_digest = function
  | Bytes s -> sha_string s, String.length s
  | File p -> Eio.Path.with_open_in p Taposaur_files.digest

let blank = function None -> true | Some s -> String.trim s = ""
let same_doi a b = match Doi.of_string a, Doi.of_string b with
  | Ok a, Ok b -> Doi.equal a b
  | _ -> String.lowercase_ascii a = String.lowercase_ascii b

let fill_identity (m : Manifest.t) (b : Bibliography.t) =
  (match m.doi, b.doi with
   | Some a, Some b when not (same_doi a b) ->
       invalid_arg "Identity names another DOI"
   | _ -> ());
  let fill old v = match old with
    | None when not (blank v) -> v | _ -> old in
  { m with title = fill m.title b.title;
    publisher = fill m.publisher b.publisher;
    year = (match m.year with None -> b.year | y -> y);
    pmid = fill m.pmid b.pmid; pmcid = fill m.pmcid b.pmcid }

let split_name name =
  match String.rindex_opt name '.' with
  | Some i when i > 0 -> String.sub name 0 i, String.sub name i
      (String.length name - i)
  | _ -> name, ""

let copy_to dir f source =
  Eio.Path.with_open_out ~follow:false ~create:(`Or_truncate 0o644)
    (dir / f.tmp) (fun out ->
      (match source with
       | Bytes s -> Eio.Flow.copy_string s out
       | File p -> Eio.Path.with_open_in p (fun src -> Eio.Flow.copy src out));
      Eio.File.sync out);
  if digest_path (dir / f.tmp) <> (f.sha256, f.bytes) then
    failwith ("Artifact source changed during acceptance: " ^ f.final)

let accept t ?identity ?(clear_failures = []) ~cmd ~at uid additions =
  let prepared = List.map (fun a ->
    check_file a.artifact.Manifest.Artifact.file;
    let sha256, bytes = source_digest a.source in
    (match a.artifact.sha256 with
     | Some s when String.lowercase_ascii s <> sha256 ->
         invalid_arg ("Artifact digest mismatch: " ^ a.artifact.file)
     | _ -> ());
    (match a.artifact.bytes with
     | n when n <> 0L && n <> Int64.of_int bytes ->
         invalid_arg ("Artifact size mismatch: " ^ a.artifact.file)
     | _ -> ());
    a, { a.artifact with sha256 = Some sha256; bytes = Int64.of_int bytes })
    additions in
  Option.get @@ with_dir ~create:true t uid @@ fun dir ->
  let old, old_text = match read_manifest uid dir with
    | Some (m, text) -> m, Some text | None -> empty uid, None in
  let taken = Hashtbl.create 8 in
  List.iter (fun (a : Manifest.Artifact.t) -> Hashtbl.replace taken a.file ())
    old.artifacts;
  let free name = not (Hashtbl.mem taken name) && not (exists (dir / name))
    && not (exists (dir / (".accept-" ^ name ^ ".tmp"))) in
  let rec choose name n =
    let stem, ext = split_name name in
    let file = if n = 1 then name else stem ^ "-" ^ string_of_int n ^ ext in
    if free file then file else choose name (n + 1) in
  let held = ref old.artifacts in
  let added = ref [] and existing = ref [] and files = ref [] in
  List.iter (fun (a, (r : Manifest.Artifact.t)) ->
    match List.find_opt (fun (b : Manifest.Artifact.t) ->
      b.sha256 = r.sha256 && b.format = r.format) !held with
    | Some b -> existing := b :: !existing
    | None ->
        let file = choose r.file 1 in
        Hashtbl.replace taken file ();
        let r = { r with file } in
        held := !held @ [r];
        added := r :: !added;
        files := ({ tmp = ".accept-" ^ file ^ ".tmp"; final = file;
          sha256 = Option.get r.sha256; bytes = Int64.to_int r.bytes },
          a.source) :: !files) prepared;
  let added = List.rev !added and existing = List.rev !existing in
  let files = List.rev !files in
  let filled = match identity with
    | None -> old | Some b -> fill_identity old b in
  let identity_changed = filled <> old in
  let failures = List.filter (fun (f : Manifest.Failure.t) ->
    not (List.mem f.route clear_failures)) old.conversions_failed in
  let strategies = List.filter_map (fun (a : Manifest.Artifact.t) ->
    a.strategy) added in
  let refusals = List.filter (fun (r : Manifest.Refusal.t) ->
    not (List.mem r.strategy strategies)) old.acquisitions_failed in
  let changed = added <> [] || identity_changed ||
    List.length failures <> List.length old.conversions_failed ||
    List.length refusals <> List.length old.acquisitions_failed ||
    old_text = None in
  let manifest_sha256 =
    if not changed then sha_string (Option.get old_text) else begin
      let m = { filled with artifacts = !held; conversions_failed = failures;
        acquisitions_failed = refusals; updated = at } in
      let text = get (Manifest.to_string m) in
      let log = events dir in
      let first = count_lines log + 1 in
      let ev = List.mapi (fun n (r : Manifest.Artifact.t) ->
        event_line ~cmd ~uid ~seq:(first + n) ~at:r.at
          (Event.Outcome (json_of Manifest.Artifact.jsont r))) added in
      let ev = if identity_changed then
          ev @ [event_line ~cmd ~uid ~seq:(first + List.length added) ~at
            (Event.Outcome (json_of Bibliography.jsont (Option.get identity)))]
        else ev in
      publish dir ~log ~manifest:text ~events:ev ~files:(List.map fst files)
        (fun () -> List.iter (fun (f, s) -> copy_to dir f s) files);
      sha_string text
    end in
  { uid; added; existing; manifest_sha256 }

let record_failure t ~cmd ~at uid (failure : Manifest.Failure.t) =
  match with_dir t uid @@ fun dir ->
    match read_manifest uid dir with
    | None -> failwith "Paper has no manifest"
    | Some (old, _) ->
        let prior = List.find_opt (fun (f : Manifest.Failure.t) ->
          f.route = failure.route) old.conversions_failed in
        let failure = { failure with attempts =
          1 + Option.fold ~none:0 ~some:(fun (f : Manifest.Failure.t) ->
            f.attempts) prior } in
        let conversions_failed = match prior with
          | None -> old.conversions_failed @ [failure]
          | Some _ -> List.map (fun (f : Manifest.Failure.t) ->
              if f.route = failure.route then failure else f)
              old.conversions_failed in
        let m = { old with conversions_failed; updated = at } in
        let log = events dir in
        let ev = event_line ~cmd ~uid ~seq:(count_lines log + 1) ~at
          (Event.Error { message = failure.reason;
            source = Some (Manifest.Route.to_string failure.route) }) in
        publish dir ~log ~manifest:(get (Manifest.to_string m)) ~events:[ev]
          ~files:[] ignore
  with None -> failwith "Paper has no manifest" | Some () -> ()

let record_refusal t ~cmd ~at uid (refusal : Manifest.Refusal.t) =
  Option.get @@ with_dir ~create:true t uid @@ fun dir ->
  let old, old_text = match read_manifest uid dir with
    | Some (m, text) -> m, Some text | None -> empty uid, None in
  ignore old_text;
  let prior = List.find_opt (fun (r : Manifest.Refusal.t) ->
    r.strategy = refusal.strategy) old.acquisitions_failed in
  let refusal = { refusal with attempts =
    1 + Option.fold ~none:0 ~some:(fun (r : Manifest.Refusal.t) -> r.attempts)
      prior } in
  let acquisitions_failed = match prior with
    | None -> old.acquisitions_failed @ [refusal]
    | Some _ -> List.map (fun (r : Manifest.Refusal.t) ->
        if r.strategy = refusal.strategy then refusal else r)
        old.acquisitions_failed in
  let m = { old with acquisitions_failed; updated = at } in
  let log = events dir in
  let ev = event_line ~cmd ~uid ~seq:(count_lines log + 1) ~at
    (Event.Error { message = refusal.reason; source = Some refusal.strategy }) in
  publish dir ~log ~manifest:(get (Manifest.to_string m)) ~events:[ev]
    ~files:[] ignore

let manifest t uid =
  Option.join (with_dir t uid (fun dir ->
    Option.map fst (read_manifest uid dir)))

let shard_path t uid = t.native ^ "/index/" ^ Layout.index uid
let shard_rel uid = "index/" ^ Layout.index uid
let uid_codec =
  Jsont.Object.map Fun.id |> Jsont.Object.mem "uid" Uid.jsont ~enc:Fun.id
  |> Jsont.Object.skip_unknown |> Jsont.Object.finish
let row_uid line = match Jsont_bytesrw.decode_string uid_codec line with
  | Ok u -> Some u | Error _ -> None
let read_shard t uid =
  let p = t.root / shard_rel uid in
  if exists p then load ~max:(1024 * 1024 * 1024) p else ""
let lines text = match List.rev (String.split_on_char '\n' text) with
  | "" :: rest -> List.rev rest | l -> List.rev l
let stored_row uid text =
  List.find_map (fun line -> match row_uid line with
    | Some u when Uid.compare u uid = 0 ->
        Some (get (Index.of_string line))
    | _ -> None) (lines text)

let epoch at = match Ptime.of_rfc3339 at with
  | Ok (t, _, _) -> Ptime.to_float_s t | Error _ -> 0.
let tei_rank (a : Manifest.Artifact.t) = match a.converter with
  | Some "pub2tei" -> 0 | Some "grobid" -> 1 | _ -> 2

let row t uid (m : Manifest.t) (stored : Index.t option) =
  let path (a : Manifest.Artifact.t) =
    t.native ^ "/" ^ paper_rel uid ^ "/" ^ a.file in
  let pdf = List.find_opt (fun (a : Manifest.Artifact.t) ->
    a.format = Manifest.Format.Pdf) m.artifacts in
  let teis = List.stable_sort (fun a b -> compare (tei_rank a) (tei_rank b))
    (List.filter (fun (a : Manifest.Artifact.t) ->
      a.format = Manifest.Format.Tei) m.artifacts) in
  let tei, alt = match teis with
    | [] -> None, None | [a] -> Some a, None | a :: b :: _ -> Some a, Some b in
  let corrected = List.fold_left (fun acc (c : Manifest.Correction.t) ->
    let f = Manifest.Field.to_string c.field in
    if List.mem f acc then acc else acc @ [f]) [] m.corrections in
  let base = { Index.uid; doi = m.doi; pmcid = m.pmcid; pmid = m.pmid;
    pdf_path = Option.fold ~none:"" ~some:path pdf;
    pdf_size = Option.fold ~none:0L ~some:(fun (a : Manifest.Artifact.t) ->
      a.bytes) pdf;
    pdf_mtime = Option.fold ~none:0. ~some:(fun (a : Manifest.Artifact.t) ->
      epoch a.at) pdf;
    source = Option.map (fun (a : Manifest.Artifact.t) ->
      Manifest.Source.to_string a.source) pdf;
    has_xml = List.exists (fun (a : Manifest.Artifact.t) ->
      a.format = Manifest.Format.Xml) m.artifacts;
    has_s2orc = List.exists (fun (a : Manifest.Artifact.t) ->
      a.source = Manifest.Source.S2orc) m.artifacts;
    tei_path = Option.fold ~none:"" ~some:path tei;
    tei_alt_path = Option.map path alt;
    tei_mtime = Option.fold ~none:0. ~some:(fun (a : Manifest.Artifact.t) ->
      epoch a.at) tei;
    tei_source = Option.bind tei (fun (a : Manifest.Artifact.t) ->
      a.converter);
    title = m.title; authors = []; journal = None; publisher = m.publisher;
    published = None; year = m.year; abstract = None; corrected } in
  match stored with
  | None -> base
  | Some (s : Index.t) ->
      (* Resolved metadata in the shard outranks manifest identity fields,
         but a value the shard lacks still comes from the manifest. *)
      let keep stored manifest = match stored with
        | Some _ -> stored | None -> manifest in
      { base with title = keep s.title base.title; authors = s.authors;
        journal = s.journal; publisher = keep s.publisher base.publisher;
        published = s.published; year = keep s.year base.year;
        abstract = s.abstract;
        corrected = if corrected = [] then s.corrected else corrected }

let index_row t uid =
  Option.map (fun m -> row t uid m (stored_row uid (read_shard t uid)))
    (manifest t uid)

let refresh_index t uid =
  match manifest t uid with
  | None -> failwith "Paper has no manifest"
  | Some m ->
      let rel = shard_rel uid in
      let parent = t.root / Filename.dirname rel in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent;
      locked (shard_path t uid ^ ".lock") @@ fun () ->
      let old = read_shard t uid in
      let found = ref false in
      let rows = ref [] in
      let line = get (Index.to_string (row t uid m (stored_row uid old))) in
      List.iter (fun l -> match row_uid l with
        | Some u when Uid.compare u uid = 0 ->
            if not !found then (found := true; rows := line :: !rows)
        | _ -> rows := l :: !rows) (lines old);
      let was = !found in
      if not was then rows := line :: !rows;
      let text = String.concat "" (List.rev_map (fun l -> l ^ "\n") !rows) in
      if text = old then Unchanged else begin
        Eio.Cancel.protect (fun () ->
          atomic parent (Filename.basename rel) text);
        if was then Updated else Added
      end

let session t audit = match audit with Some _ -> {t with audit} | None -> t
let accept ?audit t ?identity ?clear_failures ~cmd ~at uid additions =
  let t = session t audit in
  Taposaur_audit.with_operation ?session:t.audit ~action:"archive.accept"
    ~subject:(Uid.to_string uid)
    ~details:["directory", t.native ^ "/" ^ paper_rel uid;
      "additions", string_of_int (List.length additions)]
    ~result_details:(fun r -> ["added", string_of_int (List.length r.added);
      "existing", string_of_int (List.length r.existing);
      "manifest_sha256", r.manifest_sha256])
    (fun audit -> accept { t with audit } ?identity ?clear_failures ~cmd ~at
      uid additions)
let record_failure ?audit t ~cmd ~at uid (failure : Manifest.Failure.t) =
  let t = session t audit in
  Taposaur_audit.with_operation ?session:t.audit
    ~action:"archive.failure.record" ~subject:(Uid.to_string uid)
    ~details:["route", Manifest.Route.to_string failure.route;
      "permanent", string_of_bool failure.permanent]
    (fun audit -> record_failure { t with audit } ~cmd ~at uid failure)
let record_refusal ?audit t ~cmd ~at uid (refusal : Manifest.Refusal.t) =
  let t = session t audit in
  Taposaur_audit.with_operation ?session:t.audit
    ~action:"archive.refusal.record" ~subject:(Uid.to_string uid)
    ~details:["strategy", refusal.strategy;
      "permanent", string_of_bool refusal.permanent]
    (fun audit -> record_refusal { t with audit } ~cmd ~at uid refusal)
let refresh_index ?audit t uid =
  let t = session t audit in
  Taposaur_audit.with_operation ?session:t.audit
    ~action:"archive.index.refresh" ~subject:(Uid.to_string uid)
    ~details:["shard", Layout.index uid]
    ~result_details:(fun c -> ["change", match c with
      | Added -> "added" | Updated -> "updated" | Unchanged -> "unchanged"])
    (fun audit -> refresh_index { t with audit } uid)
