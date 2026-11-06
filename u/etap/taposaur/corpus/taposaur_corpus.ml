open Taposaur
module A = Taposaur_archive
module R = Taposaur_review
module S = Taposaur_review_store
module Tei = Taposaur_document.Tei

type text = {
  file : string; converter : string option; sha256 : string; bytes : int;
  body : string option; error : string option;
}

type row = {
  snapshot : int; doi : string; uid : string;
  title : string option; abstract : string option; authors : string list;
  journal : string option; publisher : string option; year : int option;
  published : string option; volume : string option; issue : string option;
  pages : string option; issn : string option; pmid : string option;
  pmcid : string option;
  title_source : string option; abstract_source : string option;
  publisher_source : string option; sources : (string * string) list;
  in_catalogue : bool; pdf : bool; xml : bool; tei : bool; s2orc : bool;
  tei_file : string option; tei_converter : string option;
  tei_sha256 : string option; tei_bytes : int option;
  text : string option; text_error : string option;
}

let opt codec = Jsont.option codec
let row_jsont =
  let mem name codec enc = Jsont.Object.mem name codec ~enc in
  let omem name codec enc =
    Jsont.Object.mem name (opt codec) ~dec_absent:(fun () -> None) ~enc in
  Jsont.Object.map ~kind:"Corpus row"
    (fun snapshot doi uid title abstract authors journal publisher year
      published volume issue pages issn pmid pmcid title_source
      abstract_source publisher_source sources in_catalogue pdf xml tei s2orc
      tei_file tei_converter tei_sha256 tei_bytes text text_error ->
      {snapshot; doi; uid; title; abstract; authors; journal; publisher; year;
       published; volume; issue; pages; issn; pmid; pmcid; title_source;
       abstract_source; publisher_source; sources; in_catalogue; pdf; xml;
       tei; s2orc; tei_file; tei_converter; tei_sha256; tei_bytes; text;
       text_error})
  |> mem "snapshot" Jsont.int (fun r -> r.snapshot)
  |> mem "doi" Jsont.string (fun r -> r.doi)
  |> mem "uid" Jsont.string (fun r -> r.uid)
  |> omem "title" Jsont.string (fun r -> r.title)
  |> omem "abstract" Jsont.string (fun r -> r.abstract)
  |> mem "authors" (Jsont.list Jsont.string) (fun r -> r.authors)
  |> omem "journal" Jsont.string (fun r -> r.journal)
  |> omem "publisher" Jsont.string (fun r -> r.publisher)
  |> omem "year" Jsont.int (fun r -> r.year)
  |> omem "published" Jsont.string (fun r -> r.published)
  |> omem "volume" Jsont.string (fun r -> r.volume)
  |> omem "issue" Jsont.string (fun r -> r.issue)
  |> omem "pages" Jsont.string (fun r -> r.pages)
  |> omem "issn" Jsont.string (fun r -> r.issn)
  |> omem "pmid" Jsont.string (fun r -> r.pmid)
  |> omem "pmcid" Jsont.string (fun r -> r.pmcid)
  |> omem "title_source" Jsont.string (fun r -> r.title_source)
  |> omem "abstract_source" Jsont.string (fun r -> r.abstract_source)
  |> omem "publisher_source" Jsont.string (fun r -> r.publisher_source)
  |> Jsont.Object.mem "sources"
       (Jsont.Object.as_string_map Jsont.string
        |> Jsont.map
             ~dec:(fun m -> List.rev (Jsont.String_map.fold
               (fun k v acc -> (k, v) :: acc) m []))
             ~enc:(fun l -> List.fold_left (fun m (k, v) ->
               Jsont.String_map.add k v m) (Jsont.String_map.create ()) l))
       ~dec_absent:(fun () -> []) ~enc:(fun r -> r.sources)
  |> mem "in_catalogue" Jsont.bool (fun r -> r.in_catalogue)
  |> mem "pdf" Jsont.bool (fun r -> r.pdf)
  |> mem "xml" Jsont.bool (fun r -> r.xml)
  |> mem "tei" Jsont.bool (fun r -> r.tei)
  |> mem "s2orc" Jsont.bool (fun r -> r.s2orc)
  |> omem "tei_file" Jsont.string (fun r -> r.tei_file)
  |> omem "tei_converter" Jsont.string (fun r -> r.tei_converter)
  |> omem "tei_sha256" Jsont.string (fun r -> r.tei_sha256)
  |> omem "tei_bytes" Jsont.int (fun r -> r.tei_bytes)
  |> omem "text" Jsont.string (fun r -> r.text)
  |> omem "text_error" Jsont.string (fun r -> r.text_error)
  |> Jsont.Object.finish

let encode_row row = match Jsont_bytesrw.encode_string row_jsont row with
  | Ok s -> s | Error e -> invalid_arg e

let nonblank = function
  | Some s when String.trim s <> "" -> Some (String.trim s) | _ -> None
let author (a : Author.t) = match nonblank a.given with
  | Some given -> given ^ " " ^ a.family | None -> a.family

let of_observation ~snapshot ?text (o : R.observation) =
  let uid = match Uid.of_input o.doi with
    | Ok u -> Uid.to_string u | Error e -> invalid_arg e in
  let source field = List.find_map (fun (e : R.evidence) ->
    if e.field = field then Some e.source else None) o.evidence in
  let b = o.bibliography and h = o.holdings in
  let tei_sha = Option.bind text (fun t ->
    if t.sha256 = "" then None else Some t.sha256) in
  { snapshot; doi = o.doi; uid;
    title = nonblank b.title; abstract = nonblank b.abstract;
    authors = List.map author b.authors;
    journal = nonblank b.journal; publisher = nonblank b.publisher;
    year = b.year; published = nonblank b.published;
    volume = nonblank b.volume; issue = nonblank b.issue;
    pages = nonblank b.pages; issn = nonblank b.issn;
    pmid = nonblank b.pmid; pmcid = nonblank b.pmcid;
    title_source = source "title"; abstract_source = source "abstract";
    publisher_source = source "publisher";
    sources = List.sort_uniq compare
      (List.map (fun (e : R.evidence) -> e.field, e.source) o.evidence);
    in_catalogue = o.in_catalogue;
    pdf = h.pdf; xml = h.xml; tei = h.tei; s2orc = h.s2orc;
    tei_file = Option.map (fun t -> t.file) text;
    tei_converter = Option.bind text (fun t -> t.converter);
    tei_sha256 = tei_sha;
    tei_bytes = Option.bind text (fun t ->
      if t.sha256 = "" then None else Some t.bytes);
    text = Option.bind text (fun t -> t.body);
    text_error = Option.bind text (fun t -> t.error) }

(* The resolver's preference among converted documents: Pub2TEI output
   carries publisher structure, GROBID reads PDFs, and published versions
   outrank accepted manuscripts and preprints. *)
let rank (a : Manifest.Artifact.t) =
  let converter = match a.converter with
    | Some "pub2tei" -> 1 | Some "grobid" -> 2 | Some "s2orc" -> 3 | _ -> 4 in
  let version = match a.version with
    | Some Manifest.Version.Published -> 0 | Some Accepted -> 1
    | Some Preprint -> 2 | Some Unknown | None -> 3 in
  converter, version

let text_of_paper ?max_bytes ?max_output paper =
  let candidates = List.filter (fun (a : Manifest.Artifact.t) ->
    a.format = Manifest.Format.Tei) (A.manifest paper).artifacts
    |> List.stable_sort (fun a b -> compare (rank a) (rank b)) in
  let read (a : Manifest.Artifact.t) =
    let failed error =
      {file = a.file; converter = a.converter; sha256 = ""; bytes = 0;
       body = None; error = Some error} in
    match A.read_artifact ?max_bytes paper a.file with
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
      failed "TEI exceeds byte limit"
    | exception (Eio.Io _ | Failure _ | Invalid_argument _ as ex) ->
      failed (Printexc.to_string ex)
    | s ->
      let sha256 = Digestif.SHA256.(to_hex (digest_string s)) in
      let bytes = String.length s in
      match Tei.body_text_string ?max_output s with
      | Ok body -> {file = a.file; converter = a.converter; sha256; bytes;
                    body; error = None}
      | Error e -> {file = a.file; converter = a.converter; sha256; bytes;
                    body = None; error = Some e} in
  let rec go best = function
    | [] -> best
    | a :: rest ->
      let t = read a in
      if t.body <> None then Some t
      else go (match best with
        | Some b when b.error = None -> Some b
        | Some _ when t.error <> None -> best
        | _ -> Some t) rest in
  go None candidates

module C = Taposaur_catalog

(* The snapshot's catalogue-then-input policy, replayed against the live
   catalogue: a nonblank live value supersedes the frozen one and its
   evidence entry; blank live fields leave the frozen value in place. *)
let refresh ?catalogue ?abstracts ?works (o : R.observation) =
  let uid = match Uid.of_input o.doi with
    | Ok u -> u | Error e -> invalid_arg e in
  let o = match Option.bind catalogue (fun c -> C.find c uid) with
    | None -> o
    | Some (s : Summary.t) ->
      let evidence = ref o.evidence in
      let take field present value frozen =
        if present value then begin
          evidence := {R.field; source = "catalogue"} ::
            List.filter (fun (e : R.evidence) -> e.field <> field) !evidence;
          value
        end else frozen in
      let text field value frozen =
        take field (fun v -> nonblank v <> None) (nonblank value) frozen in
      let b = s.bibliography and f = o.bibliography in
      let bibliography = {
        Bibliography.title = text "title" b.title f.title;
        abstract = text "abstract" b.abstract f.abstract;
        authors = take "authors" ((<>) []) b.authors f.authors;
        journal = text "journal" b.journal f.journal;
        publisher = text "publisher" b.publisher f.publisher;
        published = text "published" b.published f.published;
        year = take "year" Option.is_some b.year f.year;
        volume = text "volume" b.volume f.volume;
        issue = text "issue" b.issue f.issue;
        pages = text "pages" b.pages f.pages;
        issn = text "issn" b.issn f.issn;
        doi = f.doi;
        pmcid = text "pmcid" b.pmcid f.pmcid;
        pmid = text "pmid" b.pmid f.pmid } in
      {o with bibliography; holdings = s.holdings; in_catalogue = true;
       evidence = List.rev !evidence} in
  let o = match abstracts, nonblank o.bibliography.abstract, Uid.doi uid with
    | Some find, None, Some doi ->
      (match find doi with
       | Some (e : Abstract_evidence.t) when String.trim e.abstract <> "" ->
         {o with bibliography = {o.bibliography with abstract = Some e.abstract};
          evidence = o.evidence @
            [{R.field = "abstract"; source = Abstract_evidence.label e}]}
       | _ -> o)
    | _ -> o in
  match works, Uid.doi uid with
  | Some find, Some doi ->
    (match find doi with Some w -> R.fill w o | None -> o)
  | _ -> o

type summary = {
  rows : int; held : int; texts : int; errors : int; characters : int;
}
let summary_jsont =
  Jsont.Object.map ~kind:"Corpus summary"
    (fun rows held texts errors characters ->
      {rows; held; texts; errors; characters})
  |> Jsont.Object.mem "rows" Jsont.int ~enc:(fun s -> s.rows)
  |> Jsont.Object.mem "held" Jsont.int ~enc:(fun s -> s.held)
  |> Jsont.Object.mem "texts" Jsont.int ~enc:(fun s -> s.texts)
  |> Jsont.Object.mem "errors" Jsont.int ~enc:(fun s -> s.errors)
  |> Jsont.Object.mem "characters" Jsont.int ~enc:(fun s -> s.characters)
  |> Jsont.Object.finish

let page_size = 100

let export ?pool ?(fibers = 8) ?max_bytes ?max_output ?catalogue ?abstracts
    ?works ~archive ~store ~snapshot emit =
  if fibers < 1 || fibers > 256 then invalid_arg "fibers must be 1..256";
  let totals = ref {rows = 0; held = 0; texts = 0; errors = 0; characters = 0} in
  let lookup (o : R.observation) =
    let uid = match Uid.of_input o.doi with
      | Ok u -> u | Error e -> invalid_arg e in
    match A.find archive uid with
    | None -> false, None
    | Some paper -> true, text_of_paper ?max_bytes ?max_output paper
    | exception (Failure e | Invalid_argument e) ->
      true, Some {file = ""; converter = None; sha256 = ""; bytes = 0;
                  body = None; error = Some e}
    | exception (Eio.Io _ as ex) ->
      true, Some {file = ""; converter = None; sha256 = ""; bytes = 0;
                  body = None; error = Some (Printexc.to_string ex)} in
  (* Regular-file reads block their domain, so a pool of domains is what
     lets archive latency overlap; members the snapshot did not find in
     the catalogue are not looked up. *)
  let build (o : R.observation) =
    let o = if catalogue = None && abstracts = None && works = None then o
      else refresh ?catalogue ?abstracts ?works o in
    let held, text =
      if not o.in_catalogue then false, None
      else match pool with
        | None -> lookup o
        | Some pool -> Eio.Executor_pool.submit_exn pool ~weight:1.0
            (fun () -> lookup o) in
    held, of_observation ~snapshot ?text o in
  (* Batches of several pages keep every reader busy while the slowest
     file in a page is still arriving. *)
  let batch_rows = max page_size (8 * fibers) in
  let rec collect after acc n =
    let rows = S.rows ?after ~limit:page_size store snapshot in
    let acc = List.rev_append rows acc and n = n + List.length rows in
    if List.length rows < page_size then List.rev acc, None
    else
      let last = (List.nth rows (page_size - 1)).doi in
      if n >= batch_rows then List.rev acc, Some last
      else collect (Some last) acc n in
  let rec batches after =
    let rows, next = collect after [] 0 in
    let built = Eio.Fiber.List.map ~max_fibers:fibers build rows in
    List.iter (fun (held, row) ->
      let t = !totals in
      totals := {rows = t.rows + 1; held = t.held + Bool.to_int held;
        texts = t.texts + Bool.to_int (row.text <> None);
        errors = t.errors + Bool.to_int (row.text_error <> None);
        characters = t.characters +
          Option.fold ~none:0 ~some:String.length row.text};
      emit row) built;
    Option.iter (fun last -> batches (Some last)) next in
  batches None;
  !totals
