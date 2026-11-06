open Taposaur
module A = Taposaur_archive
module C = Taposaur_catalog
module P = Taposaur_paper
module R = Taposaur_review
module S = Taposaur_review_store
module Tei = Taposaur_document.Tei

type provenance = {
  kind : string; provider : string option; artifact : string option;
  converter : string option; version : string option; digest : string option;
  retrieved : string option;
}

type acquisition = {
  file : string; format : string; source : string; strategy : string option;
  origin : string option; url : string option; status : int option;
  acquired_version : string option; confidence : string option;
  acquired_digest : string option; acquired_at : string;
}

type text = {
  file : string; converter : string option; sha256 : string; bytes : int;
  body : string option; error : string option;
  entry : Manifest.Artifact.t option;
  acquired : Manifest.Artifact.t option;
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
  title_provenance : provenance option;
  abstract_provenance : provenance option;
  authors_provenance : provenance option;
  journal_provenance : provenance option;
  publisher_provenance : provenance option;
  year_provenance : provenance option;
  pmid_provenance : provenance option;
  pmcid_provenance : provenance option;
  journal_openalex_id : string option;
  journal_openalex_name : string option;
  journal_openalex_match : string option;
  in_catalogue : bool; pdf : bool; xml : bool; tei : bool; s2orc : bool;
  tei_file : string option; tei_converter : string option;
  tei_sha256 : string option; tei_bytes : int option;
  text : string option; text_error : string option;
  text_provenance : provenance option;
  text_acquisition : acquisition option;
}

let opt codec = Jsont.option codec
let ostr name enc = Jsont.Object.mem name (opt Jsont.string)
  ~dec_absent:(fun () -> None) ~enc
let provenance_jsont =
  Jsont.Object.map ~kind:"Provenance"
    (fun kind provider artifact converter version digest retrieved ->
      {kind; provider; artifact; converter; version; digest; retrieved})
  |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun (p : provenance) -> p.kind)
  |> ostr "provider" (fun (p : provenance) -> p.provider)
  |> ostr "artifact" (fun (p : provenance) -> p.artifact)
  |> ostr "converter" (fun (p : provenance) -> p.converter)
  |> ostr "version" (fun (p : provenance) -> p.version)
  |> ostr "digest" (fun (p : provenance) -> p.digest)
  |> ostr "retrieved" (fun (p : provenance) -> p.retrieved)
  |> Jsont.Object.finish
let acquisition_jsont =
  Jsont.Object.map ~kind:"Acquisition"
    (fun file format source strategy origin url status acquired_version
      confidence acquired_digest acquired_at ->
      {file; format; source; strategy; origin; url; status; acquired_version;
       confidence; acquired_digest; acquired_at})
  |> Jsont.Object.mem "file" Jsont.string ~enc:(fun (a : acquisition) -> a.file)
  |> Jsont.Object.mem "format" Jsont.string ~enc:(fun (a : acquisition) -> a.format)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun (a : acquisition) -> a.source)
  |> ostr "strategy" (fun (a : acquisition) -> a.strategy)
  |> ostr "origin" (fun (a : acquisition) -> a.origin)
  |> ostr "url" (fun (a : acquisition) -> a.url)
  |> Jsont.Object.mem "status" (opt Jsont.int) ~dec_absent:(fun () -> None)
       ~enc:(fun (a : acquisition) -> a.status)
  |> ostr "version" (fun (a : acquisition) -> a.acquired_version)
  |> ostr "confidence" (fun (a : acquisition) -> a.confidence)
  |> ostr "digest" (fun (a : acquisition) -> a.acquired_digest)
  |> Jsont.Object.mem "retrieved" Jsont.string ~enc:(fun (a : acquisition) -> a.acquired_at)
  |> Jsont.Object.finish

let row_jsont =
  let mem name codec enc = Jsont.Object.mem name codec ~enc in
  let omem name codec enc =
    Jsont.Object.mem name (opt codec) ~dec_absent:(fun () -> None) ~enc in
  Jsont.Object.map ~kind:"Corpus row"
    (fun snapshot doi uid title abstract authors journal publisher year
      published volume issue pages issn pmid pmcid title_source
      abstract_source publisher_source sources title_provenance
      abstract_provenance authors_provenance journal_provenance
      publisher_provenance year_provenance pmid_provenance pmcid_provenance
      journal_openalex_id journal_openalex_name journal_openalex_match
      in_catalogue pdf xml tei s2orc tei_file tei_converter tei_sha256
      tei_bytes text text_error text_provenance text_acquisition ->
      {snapshot; doi; uid; title; abstract; authors; journal; publisher; year;
       published; volume; issue; pages; issn; pmid; pmcid; title_source;
       abstract_source; publisher_source; sources; title_provenance;
       abstract_provenance; authors_provenance; journal_provenance;
       publisher_provenance; year_provenance; pmid_provenance;
       pmcid_provenance; journal_openalex_id; journal_openalex_name;
       journal_openalex_match; in_catalogue; pdf; xml; tei; s2orc; tei_file;
       tei_converter; tei_sha256; tei_bytes; text; text_error;
       text_provenance; text_acquisition})
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
  |> omem "title_provenance" provenance_jsont (fun r -> r.title_provenance)
  |> omem "abstract_provenance" provenance_jsont (fun r -> r.abstract_provenance)
  |> omem "authors_provenance" provenance_jsont (fun r -> r.authors_provenance)
  |> omem "journal_provenance" provenance_jsont (fun r -> r.journal_provenance)
  |> omem "publisher_provenance" provenance_jsont (fun r -> r.publisher_provenance)
  |> omem "year_provenance" provenance_jsont (fun r -> r.year_provenance)
  |> omem "pmid_provenance" provenance_jsont (fun r -> r.pmid_provenance)
  |> omem "pmcid_provenance" provenance_jsont (fun r -> r.pmcid_provenance)
  |> omem "journal_openalex_id" Jsont.string (fun r -> r.journal_openalex_id)
  |> omem "journal_openalex_name" Jsont.string (fun r -> r.journal_openalex_name)
  |> omem "journal_openalex_match" Jsont.string (fun r -> r.journal_openalex_match)
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
  |> omem "text_provenance" provenance_jsont (fun r -> r.text_provenance)
  |> omem "text_acquisition" acquisition_jsont (fun r -> r.text_acquisition)
  |> Jsont.Object.finish

let encode_row row = match Jsont_bytesrw.encode_string row_jsont row with
  | Ok s -> s | Error e -> invalid_arg e

let nonblank = function
  | Some s when String.trim s <> "" -> Some (String.trim s) | _ -> None
let author (a : Author.t) = match nonblank a.given with
  | Some given -> given ^ " " ^ a.family | None -> a.family

type evidence = {
  abstract : Doi.t -> Abstract_evidence.t option;
  work : Doi.t -> Work_evidence.t option;
  venue_by_issn : string -> Work_evidence.venue option;
  venue_by_name : string -> Work_evidence.venue option;
}
let no_evidence = {abstract = (fun _ -> None); work = (fun _ -> None);
  venue_by_issn = (fun _ -> None); venue_by_name = (fun _ -> None)}

(* ISSNs arrive with or without their hyphen, in either case, and often
   inside text a converter misread as the ISSN ("0030-1299 OIKOS 96:1");
   the first ISSN-shaped run is taken. Venue names are compared
   case-folded with punctuation and spacing removed. *)
let issn_key s =
  let n = String.length s in
  let digit i = i < n && s.[i] >= '0' && s.[i] <= '9' in
  let check i = i < n && (digit i || s.[i] = 'x' || s.[i] = 'X') in
  let rec scan i =
    if i + 8 > n then None
    else if digit i && digit (i+1) && digit (i+2) && digit (i+3) then
      let j = if i + 4 < n && s.[i+4] = '-' then i + 5 else i + 4 in
      if digit j && digit (j+1) && digit (j+2) && check (j+3) then
        Some (String.sub s i 4 ^ "-" ^ String.sub s j 3 ^
              String.make 1 (Char.uppercase_ascii s.[j+3]))
      else scan (i + 1)
    else scan (i + 1) in
  scan 0
let name_key s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | 'a'..'z' | '0'..'9' -> Buffer.add_char b c
    | 'A'..'Z' -> Buffer.add_char b (Char.lowercase_ascii c)
    | c when Char.code c >= 128 -> Buffer.add_char b c
    | _ -> ()) s;
  let s = Buffer.contents b in
  let prefix = "the" in
  if String.length s > 3 && String.starts_with ~prefix s then
    String.sub s 3 (String.length s - 3) else s

let rfc3339 t = match Ptime.of_float_s t with
  | Some t -> Some (Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 t) | None -> None

(* Where a resolver source class came from, named for a reader rather than
   by the archive's internal vocabulary. *)
let of_source ?index (s : P.source) : provenance = match s with
  | P.Artifact a ->
    let kind = match a.format, a.converter, a.source with
      | _, Some "pub2tei", _ -> "pub2tei"
      | _, Some "grobid", _ -> "grobid"
      | _, Some "s2orc", _ | Manifest.Format.Json, _, Manifest.Source.S2orc -> "s2orc"
      | Manifest.Format.Xml, _, Manifest.Source.Publisher -> "publisher_xml"
      | Manifest.Format.Xml, _, _ -> "xml"
      | Manifest.Format.Pdf, _, _ -> "pdf"
      | _, Some c, _ -> c
      | _ -> Manifest.Format.to_string a.format in
    {kind; provider = (match a.strategy with Some s -> Some s
       | None -> Some (Manifest.Source.to_string a.source));
     artifact = Some a.file; converter = a.converter;
     version = Option.map Manifest.Version.to_string a.version;
     digest = a.sha256; retrieved = Some a.at}
  | P.Index -> {kind = "index";
      provider = Option.bind index (fun (i : Index.t) -> nonblank i.source);
      artifact = None; converter = None; version = None; digest = None;
      retrieved = None}
  | P.Manifest -> {kind = "manifest"; provider = None; artifact = None;
      converter = None; version = None; digest = None; retrieved = None}
  | P.Correction c -> {kind = "correction"; provider = Some c.by;
      artifact = None; converter = None; version = None; digest = None;
      retrieved = Some c.at}
  | P.Mirror e -> {kind = e.provider; provider = Some e.provider;
      artifact = None; converter = None; version = None;
      digest = Some e.sha256; retrieved = rfc3339 e.retrieved}
  | P.External e -> {kind = e.provider; provider = Some e.provider;
      artifact = None; converter = None; version = None;
      digest = Some e.sha256; retrieved = rfc3339 e.retrieved}

let external_label = " \xc2\xb7 "
(* A frozen evidence label is "catalogue", "input:KEY:LOCATION" or
   "PROVIDER · SHA256"; the catalogue label is refined to the resolver's
   source when the live resolution still selects the frozen value. *)
let of_label ?paper ?index ~selected ~same ~retrieved label : provenance =
  match label with
  | "catalogue" ->
    (match Option.bind paper selected with
     | Some (e : _ P.evidence) when same e.value -> of_source ?index e.source
     | _ -> {kind = "catalogue"; provider = None; artifact = None;
         converter = None; version = None; digest = None; retrieved = None})
  | l when String.starts_with ~prefix:"input:" l ->
    let rest = String.sub l 6 (String.length l - 6) in
    let key, location = match String.index_opt rest ':' with
      | Some i -> String.sub rest 0 i, Some (String.sub rest (i + 1) (String.length rest - i - 1))
      | None -> rest, None in
    {kind = "review_input"; provider = Some key; artifact = location;
     converter = None; version = None; digest = None; retrieved = None}
  | l ->
    (match Stdlib.String.split_on_char '\xc2' l with
     | provider :: _ when String.length l > String.length provider + 3 ->
       let provider = String.trim provider in
       let digest = String.trim (String.sub l (String.length provider + 4)
         (String.length l - String.length provider - 4)) in
       {kind = provider; provider = Some provider; artifact = None;
        converter = None; version = None; digest = Some digest;
        retrieved = Option.bind (retrieved digest) rfc3339}
     | _ -> {kind = l; provider = None; artifact = None; converter = None;
         version = None; digest = None; retrieved = None})

let acquisition_of (a : Manifest.Artifact.t) : acquisition =
  {file = a.file; format = Manifest.Format.to_string a.format;
   source = Manifest.Source.to_string a.source; strategy = a.strategy;
   origin = a.origin; url = a.url; status = a.status;
   acquired_version = Option.map Manifest.Version.to_string a.version;
   confidence = Option.map Manifest.Confidence.to_string a.confidence;
   acquired_digest = a.sha256; acquired_at = a.at}

let of_observation ~snapshot ?text ?paper ?(evidence = no_evidence)
    (o : R.observation) =
  let uid = match Uid.of_input o.doi with
    | Ok u -> Uid.to_string u | Error e -> invalid_arg e in
  let doi = Result.to_option (Doi.of_string o.doi) in
  let source field = List.find_map (fun (e : R.evidence) ->
    if e.field = field then Some e.source else None) o.evidence in
  let b = o.bibliography and h = o.holdings in
  let index = Option.bind paper (fun (p : P.t) -> p.index) in
  let work = lazy (Option.bind doi evidence.work) in
  let abstract_evidence = lazy (Option.bind doi evidence.abstract) in
  let retrieved_work digest = match Lazy.force work with
    | Some (w : Work_evidence.t) when w.sha256 = digest -> Some w.retrieved
    | _ -> None in
  let retrieved_abstract digest = match Lazy.force abstract_evidence with
    | Some (e : Abstract_evidence.t) when e.sha256 = digest -> Some e.retrieved
    | _ -> match retrieved_work digest with
      | Some t -> Some t | None -> None in
  let text_field field ?(retrieved = retrieved_work) value selected =
    match nonblank value, source field with
    | None, _ | _, None -> None
    | Some v, Some label ->
      Some (of_label ?paper ?index ~retrieved label
        ~selected:(fun (p : P.t) -> (selected p.metadata).P.Field.selected)
        ~same:(fun s -> String.trim s = v)) in
  let tei_sha = Option.bind text (fun t ->
    if t.sha256 = "" then None else Some t.sha256) in
  let text_provenance = Option.bind text (fun t -> Option.map (fun a ->
    {(of_source ?index (P.Artifact a)) with digest = tei_sha}) t.entry) in
  let text_acquisition = Option.bind text (fun t ->
    Option.map acquisition_of t.acquired) in
  let journal_openalex =
    let from_work = Option.bind (Lazy.force work) (fun (w : Work_evidence.t) ->
      Option.map (fun v -> v, "work") w.venue) in
    let from_issn () = Option.bind (Option.bind (nonblank b.issn) issn_key)
      (fun k -> Option.map (fun v -> v, "issn") (evidence.venue_by_issn k)) in
    let from_name () = Option.bind (nonblank b.journal) (fun n ->
      Option.map (fun v -> v, "name") (evidence.venue_by_name (name_key n))) in
    match from_work with
    | Some v -> Some v
    | None -> match from_issn () with Some v -> Some v | None -> from_name () in
  let venue f = Option.bind journal_openalex f in
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
    title_provenance = text_field "title" b.title (fun m -> m.title);
    abstract_provenance = text_field "abstract" ~retrieved:retrieved_abstract
      b.abstract (fun m -> m.abstract);
    authors_provenance = (match b.authors, source "authors" with
      | [], _ | _, None -> None
      | authors, Some label ->
        Some (of_label ?paper ?index ~retrieved:retrieved_work label
          ~selected:(fun (p : P.t) -> p.metadata.authors.P.Field.selected)
          ~same:(fun a -> a = authors)));
    journal_provenance = text_field "journal" b.journal (fun m -> m.journal);
    publisher_provenance = text_field "publisher" b.publisher (fun m -> m.publisher);
    year_provenance = (match b.year, source "year" with
      | None, _ | _, None -> None
      | Some y, Some label ->
        Some (of_label ?paper ?index ~retrieved:retrieved_work label
          ~selected:(fun (p : P.t) -> p.metadata.year.P.Field.selected)
          ~same:(fun v -> v = y)));
    pmid_provenance = text_field "pmid" b.pmid (fun m -> m.pmid);
    pmcid_provenance = text_field "pmcid" b.pmcid (fun m -> m.pmcid);
    journal_openalex_id = venue (fun (v, _) -> Some v.Work_evidence.id);
    journal_openalex_name = venue (fun (v, _) -> v.Work_evidence.name);
    journal_openalex_match = venue (fun (_, m) -> Some m);
    in_catalogue = o.in_catalogue;
    pdf = h.pdf; xml = h.xml; tei = h.tei; s2orc = h.s2orc;
    tei_file = Option.map (fun t -> t.file) text;
    tei_converter = Option.bind text (fun t -> t.converter);
    tei_sha256 = tei_sha;
    tei_bytes = Option.bind text (fun t ->
      if t.sha256 = "" then None else Some t.bytes);
    text = Option.bind text (fun t -> t.body);
    text_error = Option.bind text (fun t -> t.error);
    text_provenance; text_acquisition }

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

(* The artifact a TEI was converted from: the one its manifest entry
   names, else the first of the format its converter reads. *)
let acquired_of (manifest : Manifest.t) (a : Manifest.Artifact.t) =
  let find p = List.find_opt p manifest.artifacts in
  match Option.bind a.from (fun f -> find (fun x -> x.Manifest.Artifact.file = f)) with
  | Some x -> Some x
  | None -> match a.converter with
    | Some "grobid" -> find (fun x -> x.format = Manifest.Format.Pdf)
    | Some "pub2tei" -> find (fun x -> x.format = Manifest.Format.Xml)
    | Some "s2orc" -> find (fun x -> x.format = Manifest.Format.Json)
    | _ -> None

let text_of_paper ?max_bytes ?max_output paper =
  let manifest = A.manifest paper in
  let candidates = List.filter (fun (a : Manifest.Artifact.t) ->
    a.format = Manifest.Format.Tei) manifest.artifacts
    |> List.stable_sort (fun a b -> compare (rank a) (rank b)) in
  let read (a : Manifest.Artifact.t) =
    let acquired = acquired_of manifest a in
    let failed error : text =
      {file = a.file; converter = a.converter; sha256 = ""; bytes = 0;
       body = None; error = Some error; entry = Some a; acquired} in
    match A.read_artifact ?max_bytes paper a.file with
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
      failed "TEI exceeds byte limit"
    | exception (Eio.Io _ | Failure _ | Invalid_argument _ as ex) ->
      failed (Printexc.to_string ex)
    | s ->
      let sha256 = Digestif.SHA256.(to_hex (digest_string s)) in
      let bytes = String.length s in
      match Tei.body_text_string ?max_output s with
      | Ok body -> ({file = a.file; converter = a.converter; sha256; bytes;
                    body; error = None; entry = Some a; acquired} : text)
      | Error e -> ({file = a.file; converter = a.converter; sha256; bytes;
                    body = None; error = Some e; entry = Some a; acquired} : text) in
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

(* The snapshot's catalogue-then-input policy, replayed against the live
   catalogue: a nonblank live value supersedes the frozen one and its
   evidence entry; blank live fields leave the frozen value in place. *)
let refresh ?catalogue ?(evidence = no_evidence) (o : R.observation) =
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
  let o = match nonblank o.bibliography.abstract, Uid.doi uid with
    | None, Some doi ->
      (match evidence.abstract doi with
       | Some (e : Abstract_evidence.t) when String.trim e.abstract <> "" ->
         {o with bibliography = {o.bibliography with abstract = Some e.abstract};
          evidence = o.evidence @
            [{R.field = "abstract"; source = Abstract_evidence.label e}]}
       | _ -> o)
    | _ -> o in
  match Uid.doi uid with
  | Some doi -> (match evidence.work doi with Some w -> R.fill w o | None -> o)
  | None -> o

(* The live resolution of a catalogued paper, for field provenance. *)
let paper_of catalogue uid =
  match C.Metadata.detail catalogue uid with
  | None -> None
  | Some d -> Result.to_option (P.of_cache_string ~index:d.index d.json)

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

let export ?pool ?(fibers = 8) ?max_bytes ?max_output ?catalogue ?evidence
    ?provenance ~archive ~store ~snapshot emit =
  if fibers < 1 || fibers > 256 then invalid_arg "fibers must be 1..256";
  let totals = ref {rows = 0; held = 0; texts = 0; errors = 0; characters = 0} in
  let lookup (o : R.observation) =
    let uid = match Uid.of_input o.doi with
      | Ok u -> u | Error e -> invalid_arg e in
    match A.find archive uid with
    | None -> false, None
    | Some paper -> true, text_of_paper ?max_bytes ?max_output paper
    | exception (Failure e | Invalid_argument e) ->
      true, Some ({file = ""; converter = None; sha256 = ""; bytes = 0;
                  body = None; error = Some e; entry = None; acquired = None} : text)
    | exception (Eio.Io _ as ex) ->
      true, Some ({file = ""; converter = None; sha256 = ""; bytes = 0;
                  body = None; error = Some (Printexc.to_string ex);
                  entry = None; acquired = None} : text) in
  (* Regular-file reads block their domain, so a pool of domains is what
     lets archive latency overlap; members the snapshot did not find in
     the catalogue are not looked up. *)
  let build (o : R.observation) =
    let o = if catalogue = None && evidence = None then o
      else refresh ?catalogue ?evidence o in
    let held, text =
      if not o.in_catalogue then false, None
      else match pool with
        | None -> lookup o
        | Some pool -> Eio.Executor_pool.submit_exn pool ~weight:1.0
            (fun () -> lookup o) in
    let paper = match provenance with
      | Some c when o.in_catalogue ->
        (match Uid.of_input o.doi with
         | Ok uid -> paper_of c uid | Error _ -> None)
      | _ -> None in
    held, of_observation ~snapshot ?text ?paper ?evidence o in
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
