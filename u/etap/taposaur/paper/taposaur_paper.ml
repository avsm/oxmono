open Taposaur
module H = Bibliography
let version = 2

type source = Correction of Manifest.Correction.t | Manifest | Index
  | Artifact of Manifest.Artifact.t | Mirror of Publisher_evidence.t
  | External of Abstract_evidence.t

type 'a evidence = { value : 'a; source : source }
module Field = struct
  type 'a t = {
    selected : 'a evidence option;
    alternatives : 'a evidence list;
  }
  let value t = Option.map (fun e -> e.value) t.selected
  let candidates t = Option.to_list t.selected @ t.alternatives
  let of_list = function
    | [] -> { selected = None; alternatives = [] }
    | x :: xs -> { selected = Some x; alternatives = xs }
end

let source_name = function
  | Mirror e -> Publisher_evidence.label e
  | External e -> Abstract_evidence.label e
  | Correction c -> "Correction by " ^ c.by ^ " at " ^ c.at
  | Manifest -> "Manifest"
  | Index -> "Index"
  | Artifact a -> a.file ^ Option.fold ~none:""
      ~some:(fun converter -> " (" ^ converter ^ ")") a.converter

type metadata = {
  title : string Field.t;
  authors : Author.t list Field.t;
  journal : string Field.t;
  publisher : string Field.t;
  published : string Field.t;
  year : int Field.t;
  abstract : string Field.t;
  volume : string Field.t;
  issue : string Field.t;
  pages : string Field.t;
  issn : string Field.t;
  doi : string Field.t;
  pmcid : string Field.t;
  pmid : string Field.t;
}

type document = {
  artifact : Manifest.Artifact.t;
  bibliography : (H.t, string) result;
  text : bool option;
}

type t = {
  publisher_evidence : Publisher_evidence.t option;
  abstract_evidence : Abstract_evidence.t option;
  manifest : Manifest.t;
  index : Index.t option;
  metadata : metadata;
  documents : document list;
  preferred_tei : Manifest.Artifact.t option;
  preferred_pdf : Manifest.Artifact.t option;
  text : bool option;
}

let rank (a : Manifest.Artifact.t) =
  let source = match a.format, a.converter with
    | Manifest.Format.Xml, _ when a.source = Manifest.Source.Publisher -> 0
    | _, Some "pub2tei" -> 1
    | _, Some "grobid" -> 2
    | _, Some "s2orc" -> 3
    | Json, _ when a.source = Manifest.Source.S2orc -> 3
    | _ -> 4 in
  let version = match a.version with
    | Some Manifest.Version.Published -> 0
    | Some Accepted -> 1 | Some Preprint -> 2 | Some Unknown | None -> 3 in
  source, version

let nonblank = function
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let year_of_date s =
  if String.length s < 4 then None else
  let year = String.sub s 0 4 in
  if String.for_all (function '0'..'9' -> true | _ -> false) year
  then int_of_string_opt year else None

let resolve ?index (manifest : Manifest.t) documents =
  Option.iter (fun (i : Index.t) ->
    if Uid.compare i.uid manifest.id <> 0 then
      invalid_arg "Index and manifest identify different papers") index;
  List.iter (fun d ->
    if not (List.mem d.artifact manifest.artifacts) then
      invalid_arg "Document is not listed in the manifest") documents;
  let documents = List.concat_map (fun artifact ->
    List.filter (fun d -> d.artifact = artifact) documents)
    manifest.artifacts
    |> List.stable_sort (fun a b ->
      compare (rank a.artifact) (rank b.artifact)) in
  let observed f = List.filter_map (fun d -> match d.bibliography with
    | Error _ -> None
    | Ok h -> Option.map (fun value -> { value; source = Artifact d.artifact })
        (f h)) documents in
  let evidence source value =
    Option.to_list (Option.map (fun value -> { value; source }) value) in
  let correction field parse =
    List.rev manifest.corrections |> List.filter_map (fun c ->
      if c.Manifest.Correction.field <> field then None else
      Option.map (fun value -> { value; source = Correction c })
        (parse c.value)) in
  let choose ?field ?(manifest_value = None) ~index_value header =
    let corrected = Option.fold ~none:[] ~some:(fun field ->
      correction field (fun s -> nonblank (Some s))) field in
    Field.of_list (corrected @ evidence Manifest (nonblank manifest_value) @
      observed (fun h -> nonblank (header h)) @
      evidence Index (nonblank (Option.bind index index_value))) in
  let no_index _ = None in
  let title = choose ~field:Manifest.Field.Title ~manifest_value:manifest.title
    ~index_value:(fun i -> i.Index.title) (fun h -> h.H.title) in
  let authors = Field.of_list
    (correction Manifest.Field.Authors (fun s ->
       match Author.of_correction s with [] -> None | xs -> Some xs) @
     observed (fun h -> if h.H.authors = [] then None else Some h.authors) @
     evidence Index (Option.bind index (fun i ->
       if i.Index.authors = [] then None else Some i.authors))) in
  let journal = choose ~field:Manifest.Field.Journal
    ~index_value:(fun i -> i.Index.journal) (fun h -> h.H.journal) in
  let publisher = choose ~field:Manifest.Field.Publisher
    ~manifest_value:manifest.publisher
    ~index_value:(fun i -> i.Index.publisher) (fun h -> h.H.publisher) in
  let published = choose ~index_value:(fun i -> i.Index.published)
    (fun h -> h.H.published) in
  let year = Field.of_list
    (correction Manifest.Field.Year int_of_string_opt @
     evidence Manifest manifest.year @
     observed (fun h -> match h.H.year with
       | Some _ as year -> year
       | None -> Option.bind h.published year_of_date) @
     evidence Index (Option.bind index (fun i ->
       match i.Index.year with Some _ as year -> year
       | None -> Option.bind i.published year_of_date))) in
  let abstract = choose ~field:Manifest.Field.Abstract
    ~index_value:(fun i -> i.Index.abstract) (fun h -> h.H.abstract) in
  let volume = choose ~index_value:no_index (fun h -> h.H.volume) in
  let issue = choose ~index_value:no_index (fun h -> h.H.issue) in
  let pages = choose ~index_value:no_index (fun h -> h.H.pages) in
  let issn = choose ~index_value:no_index (fun h -> h.H.issn) in
  let doi = choose ~manifest_value:manifest.doi
    ~index_value:(fun i -> i.Index.doi) (fun h -> h.H.doi) in
  let pmcid = choose ~manifest_value:manifest.pmcid
    ~index_value:(fun i -> i.Index.pmcid) (fun h -> h.H.pmcid) in
  let pmid = choose ~manifest_value:manifest.pmid
    ~index_value:(fun i -> i.Index.pmid) (fun h -> h.H.pmid) in
  let teis = List.filter (fun d ->
    d.artifact.format = Manifest.Format.Tei) documents in
  let preferred_tei =
    Option.map (fun d -> d.artifact)
      (List.find_opt (fun d -> Result.is_ok d.bibliography) teis) in
  let preferred_pdf = List.find_opt (fun (a : Manifest.Artifact.t) ->
    a.format = Manifest.Format.Pdf) manifest.artifacts in
  (* Any document with prose settles it; documents that could not say
     leave it open. *)
  let text = if List.exists (fun (d : document) -> d.text = Some true) documents then
      Some true
    else if List.exists (fun (d : document) -> d.text = Some false) documents then
      Some false else None in
  { publisher_evidence=None; abstract_evidence=None; manifest; index; documents;
    preferred_tei; preferred_pdf;
    text;
    metadata = { title; authors; journal; publisher; published; year;
      abstract; volume; issue; pages; issn; doi; pmcid; pmid } }

let with_publisher (e : Publisher_evidence.t) p =
  let doi = match Doi.of_string e.doi with Ok d -> d | Error x -> invalid_arg x in
  if Uid.doi p.manifest.id <> Some doi then invalid_arg "Publisher evidence DOI mismatch";
  if String.trim e.publisher = "" || String.length e.sha256 <> 64 then
    invalid_arg "Invalid publisher evidence";
  if Field.value p.metadata.publisher <> None then p else
    {p with publisher_evidence=Some e;metadata={p.metadata with
      publisher=Field.of_list [{value=e.publisher;source=Mirror e}]}}

let with_abstract (e : Abstract_evidence.t) p =
  let doi = match Doi.of_string e.doi with Ok d -> d | Error x -> invalid_arg x in
  if Uid.doi p.manifest.id <> Some doi then invalid_arg "Abstract evidence DOI mismatch";
  if String.trim e.abstract = "" || String.length e.sha256 <> 64 then
    invalid_arg "Invalid abstract evidence";
  if Field.value p.metadata.abstract <> None then p else
    {p with abstract_evidence=Some e;metadata={p.metadata with
      abstract=Field.of_list [{value=e.abstract;source=External e}]}}

let bibliography t =
  let m = t.metadata in
  let value = Field.value in
  { Bibliography.title = value m.title;
    authors = Option.value ~default:[] (value m.authors);
    journal = value m.journal; publisher = value m.publisher;
    published = value m.published; year = value m.year;
    abstract = value m.abstract; volume = value m.volume;
    issue = value m.issue; pages = value m.pages; issn = value m.issn;
    doi = value m.doi; pmcid = value m.pmcid; pmid = value m.pmid }

let presence paper = Bibliography.presence (bibliography paper)

let observation_codec =
  Jsont.Object.map (fun file bibliography error text ->
    file, bibliography, error, text)
  |> Jsont.Object.mem "file" Jsont.string ~enc:(fun (f,_,_,_) -> f)
  |> Jsont.Object.opt_mem "bibliography" Bibliography.jsont
       ~enc:(fun (_,b,_,_) -> b)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun (_,_,e,_) -> e)
  |> Jsont.Object.opt_mem "text" Jsont.bool ~enc:(fun (_,_,_,t) -> t)
  |> Jsont.Object.finish

let cache_codec =
  Jsont.Object.map (fun version manifest documents publisher abstract ->
    version,manifest,documents,publisher,abstract)
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun (v,_,_,_,_) -> v)
  |> Jsont.Object.mem "manifest" Manifest.jsont ~enc:(fun (_,m,_,_,_) -> m)
  |> Jsont.Object.mem "documents" (Jsont.list observation_codec)
       ~enc:(fun (_,_,d,_,_) -> d)
  |> Jsont.Object.opt_mem "publisher_evidence" Publisher_evidence.jsont
       ~enc:(fun (_,_,_,p,_) -> p)
  |> Jsont.Object.opt_mem "abstract_evidence" Abstract_evidence.jsont
       ~enc:(fun (_,_,_,_,a) -> a)
  |> Jsont.Object.finish

(* Cache version 2 adds the body text observation; version 1 caches
   decode with it unknown. *)
let cache_version = 2
let cache_string p =
  let documents = List.map (fun d -> match d.bibliography with
    | Ok b -> d.artifact.file, Some b, None, d.text
    | Error e -> d.artifact.file, None, Some e, d.text) p.documents in
  Jsont_bytesrw.encode_string cache_codec
    (cache_version, p.manifest, documents, p.publisher_evidence,
     p.abstract_evidence)

let of_cache_string ~index json =
  match Jsont_bytesrw.decode_string cache_codec json with
  | Error e -> Error e
  | Ok (version, manifest, observations, publisher, abstract) ->
      try
        if version < 1 || version > cache_version then
          invalid_arg "Unsupported paper cache version";
        let seen = Hashtbl.create 8 in
        let documents = List.map (fun (file, bibliography, error, text) ->
          if Hashtbl.mem seen file then invalid_arg "Duplicate cached document";
          Hashtbl.add seen file ();
          let artifact = match List.find_opt (fun (a : Manifest.Artifact.t) ->
            a.file = file) manifest.artifacts with
            | Some a -> a | None -> invalid_arg "Unlisted cached document" in
          let bibliography = match bibliography, error with
            | Some b, None -> Ok b | None, Some e -> Error e
            | _ -> invalid_arg "Invalid cached document outcome" in
          {artifact; bibliography; text}) observations in
        let p = resolve ~index manifest documents in
        let p = Option.fold ~none:p ~some:(fun e -> with_publisher e p) publisher in
        Ok (Option.fold ~none:p ~some:(fun e -> with_abstract e p) abstract)
      with Invalid_argument e -> Error e
