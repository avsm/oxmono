open Taposaur
module H = Bibliography
let version = 1

type source = Correction of Manifest.Correction.t | Manifest | Index
  | Artifact of Manifest.Artifact.t

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
}

type t = {
  manifest : Manifest.t;
  index : Index.t option;
  metadata : metadata;
  documents : document list;
  preferred_tei : Manifest.Artifact.t option;
  preferred_pdf : Manifest.Artifact.t option;
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
    match List.find_opt (fun d -> Result.is_ok d.bibliography) teis with Some d -> Some d.artifact
    | None ->
        List.filter (fun (a : Manifest.Artifact.t) ->
          a.format = Manifest.Format.Tei) manifest.artifacts
        |> List.stable_sort (fun a b -> compare (rank a) (rank b))
        |> fun artifacts -> List.nth_opt artifacts 0 in
  let preferred_pdf = List.find_opt (fun (a : Manifest.Artifact.t) ->
    a.format = Manifest.Format.Pdf) manifest.artifacts in
  { manifest; index; documents; preferred_tei; preferred_pdf;
    metadata = { title; authors; journal; publisher; published; year;
      abstract; volume; issue; pages; issn; doi; pmcid; pmid } }

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
