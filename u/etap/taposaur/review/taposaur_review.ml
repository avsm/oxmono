open Taposaur
let encode codec value = match Jsont_bytesrw.encode_string codec value with
  | Ok s -> s | Error e -> invalid_arg e
let decode codec bytes = Jsont_bytesrw.decode_string codec bytes
let digest s = Digestif.SHA256.(to_hex (digest_string s))
let strings = Jsont.list Jsont.string
let nonblank s = String.trim s <> ""
let valid_key s = String.length s > 0 && String.length s <= 80 &&
  String.for_all (function
    | 'a'..'z' | '0'..'9' | '-' | '_' -> true | _ -> false) s
let valid_hash s = String.length s = 64 && String.for_all (function
  | 'a'..'f' | '0'..'9' -> true | _ -> false) s

module Definition = struct
  type format = Doi_lines | Metadata_jsonl
  type kind = List | Catalogue | Recorded_search
  type holdings = Any | Pdf | Xml | Tei | S2orc | Unconverted
  type field = Title | Abstract | Publisher
  type availability = Present | Missing
  type predicate = { field : field; availability : availability }
  type local = {
    text : string; journal : string; publisher : string;
    year_from : int option; year_to : int option;
    holdings : holdings; metadata : predicate list;
  }
  let empty_local = {text=""; journal=""; publisher="";
    year_from=None; year_to=None; holdings=Any; metadata=[]}
  type source = {
    kind : kind; format : format; blob : string option;
    local : local option; provider : string option; query : string option;
    parameters : Jsont.json option;
  }
  type input = { key : string; label : string; source : source }
  type t = {
    version : int; inputs : input list;
    include_any : string list; require_all : string list;
    exclude_any : string list;
  }
  let predicate_codec =
    Jsont.Object.map (fun field availability -> {field; availability})
    |> Jsont.Object.mem "field" (Jsont.enum
         ["title",Title;"abstract",Abstract;"publisher",Publisher])
         ~enc:(fun p -> p.field)
    |> Jsont.Object.mem "availability" (Jsont.enum
         ["present",Present;"missing",Missing]) ~enc:(fun p -> p.availability)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
  let local_codec =
    Jsont.Object.map (fun text journal publisher year_from year_to holdings
      metadata -> {text; journal; publisher; year_from; year_to; holdings;
        metadata})
    |> Jsont.Object.mem "text" Jsont.string ~dec_absent:(fun () -> "")
         ~enc:(fun (q : local) -> q.text)
    |> Jsont.Object.mem "journal" Jsont.string ~dec_absent:(fun () -> "")
         ~enc:(fun (q : local) -> q.journal)
    |> Jsont.Object.mem "publisher" Jsont.string ~dec_absent:(fun () -> "")
         ~enc:(fun (q : local) -> q.publisher)
    |> Jsont.Object.opt_mem "year_from" Jsont.int ~enc:(fun q -> q.year_from)
    |> Jsont.Object.opt_mem "year_to" Jsont.int ~enc:(fun q -> q.year_to)
    |> Jsont.Object.mem "holdings" (Jsont.enum ["any",Any;"pdf",Pdf;
         "xml",Xml;"tei",Tei;"s2orc",S2orc;"unconverted",Unconverted])
         ~dec_absent:(fun () -> Any)
         ~enc:(fun q -> q.holdings)
    |> Jsont.Object.mem "metadata" (Jsont.list predicate_codec)
         ~dec_absent:(fun () -> [])
         ~enc:(fun q -> q.metadata)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
  let format_codec = Jsont.enum
    ["doi_lines", Doi_lines; "metadata_jsonl", Metadata_jsonl]
  let source_codec =
    Jsont.Object.map (fun kind format blob local provider query parameters ->
      {kind; format; blob; local; provider; query; parameters})
    |> Jsont.Object.mem "kind" (Jsont.enum ["list",List;
         "catalogue",Catalogue; "recorded_search",Recorded_search])
         ~enc:(fun s -> s.kind)
    |> Jsont.Object.mem "format" format_codec
         ~dec_absent:(fun () -> Doi_lines) ~enc:(fun s -> s.format)
    |> Jsont.Object.opt_mem "blob" Jsont.string ~enc:(fun s -> s.blob)
    |> Jsont.Object.opt_mem "local" local_codec ~enc:(fun s -> s.local)
    |> Jsont.Object.opt_mem "provider" Jsont.string ~enc:(fun s -> s.provider)
    |> Jsont.Object.opt_mem "query" Jsont.string ~enc:(fun s -> s.query)
    |> Jsont.Object.opt_mem "parameters" Jsont.json
         ~enc:(fun s -> s.parameters)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
  let input_codec =
    Jsont.Object.map (fun key label source -> {key; label; source})
    |> Jsont.Object.mem "key" Jsont.string ~enc:(fun i -> i.key)
    |> Jsont.Object.mem "label" Jsont.string ~enc:(fun i -> i.label)
    |> Jsont.Object.mem "source" source_codec ~enc:(fun i -> i.source)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
  let jsont =
    Jsont.Object.map (fun version inputs include_any require_all exclude_any ->
      {version; inputs; include_any; require_all; exclude_any})
    |> Jsont.Object.mem "version" Jsont.int ~enc:(fun d -> d.version)
    |> Jsont.Object.mem "inputs" (Jsont.list input_codec)
         ~enc:(fun d -> d.inputs)
    |> Jsont.Object.mem "include_any" strings ~enc:(fun d -> d.include_any)
    |> Jsont.Object.mem "require_all" strings ~dec_absent:(fun () -> [])
         ~enc:(fun d -> d.require_all)
    |> Jsont.Object.mem "exclude_any" strings ~dec_absent:(fun () -> [])
         ~enc:(fun d -> d.exclude_any)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
  let validate d =
    let exception Invalid of string in
    let need condition message =
      if not condition then raise (Invalid message) in
    try
      need (d.version = 1) "Unsupported review definition version";
      need (d.inputs <> [] && List.length d.inputs <= 32)
        "Definitions require 1..32 inputs";
      let keys = List.map (fun i -> i.key) d.inputs in
      need (List.for_all valid_key keys) "Invalid input key";
      need (List.length (List.sort_uniq String.compare keys) = List.length keys)
        "Duplicate input key";
      need (d.include_any <> []) "include_any must not be empty";
      let refs = d.include_any @ d.require_all @ d.exclude_any in
      need (List.for_all (fun k -> List.mem k keys) refs)
        "Unknown input reference";
      need (List.for_all (fun k -> List.mem k refs) keys) "Unused input";
      List.iter (fun i ->
        need (nonblank i.label) "Input label must not be blank";
        let s = i.source in
        match s.kind, s.blob, s.local, s.provider, s.query, s.parameters with
        | List, Some blob, None, None, None, None ->
            need (valid_hash blob) "List blob must be a SHA-256 digest"
        | Recorded_search, None, None, Some p, Some q, _ ->
            need (nonblank p && nonblank q) "Provider and query are required"
        | Catalogue, None, Some q, None, None, None ->
            need (String.length q.text <= 512) "Query exceeds 512 bytes";
            let fields = List.map (fun p -> p.field) q.metadata in
            need (List.length fields =
              List.length (List.sort_uniq compare fields))
              "Metadata fields must have one predicate each";
            List.iter (fun year -> Option.iter (fun n ->
              need (n >= 1 && n <= 9999) "Year must be 1..9999") year)
              [q.year_from;q.year_to];
            (match q.year_from, q.year_to with
             | Some a, Some b -> need (a <= b) "Inverted year range"
             | _ -> ())
        | _ -> raise (Invalid "Fields do not match the source kind")) d.inputs;
      Ok ()
    with Invalid e -> Error e
  let of_string bytes = match decode jsont bytes with
    | Error e -> Error e
    | Ok d -> Result.map (fun () -> d) (validate d)
  let to_string d = match validate d with
    | Ok () -> encode jsont d | Error e -> invalid_arg e
end

type record = { doi : string; bibliography : Bibliography.t; line : int }
type diagnostic = { line : int; message : string }
type parsed = { records : record list; diagnostics : diagnostic list }
(* The bibliography codec accepts the flat interchange row directly. *)
let record_jsont =
  Jsont.Object.map (fun doi bibliography line -> {doi; bibliography; line})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "bibliography" Bibliography.jsont
       ~enc:(fun r -> r.bibliography)
  |> Jsont.Object.mem "line" Jsont.int ~dec_absent:(fun () -> 0)
       ~enc:(fun (r : record) -> r.line)
  |> Jsont.Object.finish
let diagnostic_jsont =
  Jsont.Object.map (fun line message -> {line; message})
  |> Jsont.Object.mem "line" Jsont.int ~enc:(fun d -> d.line)
  |> Jsont.Object.mem "message" Jsont.string ~enc:(fun d -> d.message)
  |> Jsont.Object.finish
let parse format bytes =
  let records = ref [] and diagnostics = ref [] in
  String.split_on_char '\n' bytes |> List.iteri (fun i raw ->
    let line = String.trim raw in
    if line <> "" && line.[0] <> '#' then
      let result = match format with
        | Definition.Doi_lines -> Result.map (fun d ->
            {doi=Doi.to_string d; bibliography=Bibliography.empty;line=i+1})
            (Doi.of_string line)
        | Metadata_jsonl ->
            (match Bibliography.of_string line with
             | Error e -> Error e
             | Ok b -> match b.doi with
               | None -> Error "Metadata row has no DOI"
               | Some doi -> Result.map (fun d ->
                   let doi = Doi.to_string d in
                   {doi; bibliography={b with doi=Some doi};line=i+1})
                   (Doi.of_string doi)) in
      match result with
      | Ok r -> records := r :: !records
      | Error message -> diagnostics := {line=i+1; message} :: !diagnostics);
  {records=List.rev !records; diagnostics=List.rev !diagnostics}

type supplied = { input : string; record : record }
let supplied_codec =
  Jsont.Object.map (fun input record -> {input; record})
  |> Jsont.Object.mem "input" Jsont.string ~enc:(fun s -> s.input)
  |> Jsont.Object.mem "record" record_jsont ~enc:(fun s -> s.record)
  |> Jsont.Object.finish
type member = { doi : string; origins : string list; supplied : supplied list }
let member_jsont =
  Jsont.Object.map (fun doi origins supplied -> {doi; origins; supplied})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun (m : member) -> m.doi)
  |> Jsont.Object.mem "origins" strings ~enc:(fun m -> m.origins)
  |> Jsont.Object.mem "supplied" (Jsont.list supplied_codec)
       ~enc:(fun m -> m.supplied)
  |> Jsont.Object.finish
let select (d : Definition.t) results =
  (match Definition.validate d with Ok () -> () | Error e -> invalid_arg e);
  let inputs = List.map (fun (i : Definition.input) ->
    let records = match List.assoc_opt i.key results with
      | Some r -> r | None -> invalid_arg ("Missing input " ^ i.key) in
    let table = Hashtbl.create (List.length records) in
    List.iter (fun (r : record) ->
      Hashtbl.replace table r.doi
        (r :: Option.value ~default:[] (Hashtbl.find_opt table r.doi))) records;
    i.key, table) d.inputs in
  let has k doi = Hashtbl.mem (List.assoc k inputs) doi in
  let candidates = Hashtbl.create 128 in
  List.iter (fun k -> Hashtbl.iter (fun doi _ ->
    Hashtbl.replace candidates doi ()) (List.assoc k inputs)) d.include_any;
  Hashtbl.fold (fun doi () acc ->
    if List.for_all (fun k -> has k doi) d.require_all &&
       not (List.exists (fun k -> has k doi) d.exclude_any) then
      let origins = List.filter_map (fun (k,t) ->
        if Hashtbl.mem t doi then Some k else None) inputs in
      let supplied = List.concat_map (fun (input,t) ->
        Option.value ~default:[] (Hashtbl.find_opt t doi) |> List.rev
        |> List.map (fun record -> {input; record})) inputs in
      {doi; origins; supplied} :: acc
    else acc) candidates []
  |> List.sort (fun (a : member) b -> String.compare a.doi b.doi)

type evidence = { field : string; source : string }
type observation = {
  doi : string; bibliography : Bibliography.t;
  holdings : Summary.holdings; in_catalogue : bool;
  origins : string list; evidence : evidence list;
  catalogue_stamp : string option;
  stale : bool; error : string option; diagnostics : string list;
}
let evidence_codec =
  Jsont.Object.map (fun field source -> {field; source})
  |> Jsont.Object.mem "field" Jsont.string ~enc:(fun e -> e.field)
  |> Jsont.Object.mem "source" Jsont.string ~enc:(fun e -> e.source)
  |> Jsont.Object.finish
let holdings_codec =
  Jsont.Object.map (fun pdf xml tei s2orc text ->
    {Summary.pdf; xml; tei; s2orc; text})
  |> Jsont.Object.mem "pdf" Jsont.bool ~enc:(fun h -> h.Summary.pdf)
  |> Jsont.Object.mem "xml" Jsont.bool ~enc:(fun h -> h.Summary.xml)
  |> Jsont.Object.mem "tei" Jsont.bool ~enc:(fun h -> h.Summary.tei)
  |> Jsont.Object.mem "s2orc" Jsont.bool ~enc:(fun h -> h.Summary.s2orc)
  |> Jsont.Object.opt_mem "text" Jsont.bool ~enc:(fun h -> h.Summary.text)
  |> Jsont.Object.finish
let observation_jsont =
  Jsont.Object.map (fun doi bibliography holdings in_catalogue origins evidence
    catalogue_stamp stale error diagnostics ->
    {doi; bibliography; holdings; in_catalogue; origins; evidence;
      catalogue_stamp; stale; error; diagnostics})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun o -> o.doi)
  |> Jsont.Object.mem "bibliography" Bibliography.jsont
       ~enc:(fun o -> o.bibliography)
  |> Jsont.Object.mem "holdings" holdings_codec ~enc:(fun o -> o.holdings)
  |> Jsont.Object.mem "in_catalogue" Jsont.bool ~enc:(fun o -> o.in_catalogue)
  |> Jsont.Object.mem "origins" strings ~enc:(fun o -> o.origins)
  |> Jsont.Object.mem "evidence" (Jsont.list evidence_codec)
       ~enc:(fun o -> o.evidence)
  |> Jsont.Object.opt_mem "catalogue_stamp" Jsont.string
       ~enc:(fun o -> o.catalogue_stamp)
  |> Jsont.Object.mem "stale" Jsont.bool ~enc:(fun o -> o.stale)
  |> Jsont.Object.opt_mem "error" Jsont.string ~enc:(fun o -> o.error)
  |> Jsont.Object.mem "diagnostics" strings ~enc:(fun o -> o.diagnostics)
  |> Jsont.Object.finish
let freeze (member : member) ~summary ~catalogue_stamp ~stale ~error
    ~diagnostics =
  let evidence = ref [] in
  let sources = (match summary with None -> []
    | Some (s : Summary.t) -> ["catalogue", s.bibliography]) @
    List.mapi (fun n s ->
      let location = if s.record.line > 0 then
        "line:" ^ string_of_int s.record.line
        else "record:" ^ string_of_int (n+1) in
      "input:" ^ s.input ^ ":" ^ location, s.record.bibliography)
      member.supplied in
  let pick field get present default =
    match List.find_opt (fun (_,b) -> present (get b)) sources with
    | None -> default
    | Some (source,b) -> evidence := {field;source} :: !evidence; get b in
  let text field get = pick field get
    (function Some s -> nonblank s | None -> false) None in
  let bibliography = {
    Bibliography.title=text "title" (fun b -> b.Bibliography.title);
    abstract=text "abstract" (fun b -> b.Bibliography.abstract);
    authors=pick "authors" (fun b -> b.Bibliography.authors) ((<>) []) [];
    journal=text "journal" (fun b -> b.Bibliography.journal);
    publisher=text "publisher" (fun b -> b.Bibliography.publisher);
    published=text "published" (fun b -> b.Bibliography.published);
    year=pick "year" (fun b -> b.Bibliography.year) Option.is_some None;
    volume=text "volume" (fun b -> b.Bibliography.volume);
    issue=text "issue" (fun b -> b.Bibliography.issue);
    pages=text "pages" (fun b -> b.Bibliography.pages);
    issn=text "issn" (fun b -> b.Bibliography.issn);
    doi=Some member.doi;
    pmcid=text "pmcid" (fun b -> b.Bibliography.pmcid);
    pmid=text "pmid" (fun b -> b.Bibliography.pmid)} in
  let holdings = match summary with Some s -> s.Summary.holdings
    | None -> {Summary.pdf=false;xml=false;tei=false;s2orc=false;text=None}
  in
  {doi=member.doi; bibliography; holdings;
    in_catalogue=Option.is_some summary; origins=member.origins;
    evidence=List.rev !evidence; catalogue_stamp; stale; error; diagnostics}

let fill (w : Work_evidence.t) (o : observation) =
  if w.doi <> o.doi then o else
  let evidence = ref (List.rev o.evidence) in
  let note field = evidence := {field; source = Work_evidence.label w} ::
    List.filter (fun e -> e.field <> field) !evidence in
  let blank = function Some s -> String.trim s = "" | None -> true in
  let text field (supplied : string option) frozen =
    if blank frozen && not (blank supplied) then (note field; supplied) else frozen in
  let b = o.bibliography and s = w.bibliography in
  let bibliography = {
    Bibliography.title = text "title" s.title b.title;
    abstract = b.abstract;
    authors = if b.authors = [] && s.authors <> [] then (note "authors"; s.authors)
      else b.authors;
    journal = text "journal" s.journal b.journal;
    publisher = text "publisher" s.publisher b.publisher;
    published = text "published" s.published b.published;
    year = (match b.year, s.year with
      | None, Some y -> note "year"; Some y | y, _ -> y);
    volume = text "volume" s.volume b.volume;
    issue = text "issue" s.issue b.issue;
    pages = text "pages" s.pages b.pages;
    issn = text "issn" s.issn b.issn;
    doi = b.doi;
    pmcid = text "pmcid" s.pmcid b.pmcid;
    pmid = text "pmid" s.pmid b.pmid } in
  {o with bibliography; evidence = List.rev !evidence}

type export_format = Dois | Csv | Jsonl
let export_name = function Dois -> "doi" | Csv -> "csv" | Jsonl -> "jsonl"
let export_type = function
  | Dois -> "text/plain; charset=utf-8"
  | Csv -> "text/csv; charset=utf-8"
  | Jsonl -> "application/x-ndjson; charset=utf-8"
let export format rows =
  let rows = List.sort (fun a b -> String.compare a.doi b.doi) rows in
  let quote s = "\"" ^
    String.concat "\"\"" (String.split_on_char '"' s) ^ "\"" in
  match format with
  | Dois -> String.concat "" (List.map (fun o -> o.doi ^ "\n") rows)
  | Jsonl -> String.concat "" (List.map (fun o ->
      encode observation_jsont o ^ "\n") rows)
  | Csv -> "doi,title,abstract\r\n" ^ String.concat "" (List.map (fun o ->
      String.concat "," (List.map quote [o.doi;
        Option.value ~default:"" o.bibliography.title;
        Option.value ~default:"" o.bibliography.abstract]) ^ "\r\n") rows)

module Statistics = struct
  type field = Title | Abstract | Catalogue | Full_text | Pdf | Xml | Tei
    | S2orc | Publisher | Stale | Error
  let fields = [Title;Abstract;Catalogue;Full_text;Pdf;Xml;Tei;S2orc;
    Publisher;Stale;Error]
  let label = function Title -> "Title" | Abstract -> "Abstract"
    | Catalogue -> "In catalogue" | Full_text -> "Full text" | Pdf -> "PDF"
    | Xml -> "Publisher XML" | Tei -> "TEI" | S2orc -> "S2ORC"
    | Publisher -> "Publisher" | Stale -> "Stale metadata"
    | Error -> "Metadata error"
  let of_label s = List.find_opt (fun f -> label f = s) fields
  let publisher (o : observation) =
    Option.fold ~none:"" ~some:String.trim o.bibliography.publisher
  let values (o : observation) =
    let b = Taposaur.Bibliography.presence o.bibliography in
    let h = o.holdings in
    List.combine fields [b.title;b.abstract;o.in_catalogue;
      Summary.full_text h;h.pdf;h.xml;h.tei;h.s2orc;
      publisher o <> "";o.stale;o.error <> None]
  type coverage = { total : int; present : (string * int) list }
  type t = { overall : coverage; publishers : (string * coverage) list }
  let pair codec = Jsont.Object.map (fun name value -> name,value)
    |> Jsont.Object.mem "name" Jsont.string ~enc:fst
    |> Jsont.Object.mem "value" codec ~enc:snd
    |> Jsont.Object.finish
  let coverage_jsont = Jsont.Object.map (fun total present -> {total;present})
    |> Jsont.Object.mem "total" Jsont.int ~enc:(fun c -> c.total)
    |> Jsont.Object.mem "present" (Jsont.list (pair Jsont.int))
         ~enc:(fun c -> c.present)
    |> Jsont.Object.finish
  let jsont = Jsont.Object.map (fun overall publishers -> {overall;publishers})
    |> Jsont.Object.mem "overall" coverage_jsont ~enc:(fun s -> s.overall)
    |> Jsont.Object.mem "publishers" (Jsont.list (pair coverage_jsont))
         ~enc:(fun s -> s.publishers)
    |> Jsont.Object.finish
  let collect iter =
    let groups = Hashtbl.create 64 in
    let empty () = Array.make (List.length fields + 1) 0 in
    let overall = empty () in
    let update counts flags =
      counts.(0) <- counts.(0) + 1;
      List.iteri (fun i present -> if present then
        counts.(i+1) <- counts.(i+1) + 1) flags in
    iter (fun (o : observation) ->
      let publisher = publisher o in
      let flags = List.map snd (values o) in
      update overall flags;
      let counts = match Hashtbl.find_opt groups publisher with
        | Some c -> c | None -> let c = empty () in
            Hashtbl.add groups publisher c; c in
      update counts flags);
    let coverage counts = {total=counts.(0);
      present=List.mapi (fun i field -> label field,counts.(i+1)) fields} in
    {overall=coverage overall;
     publishers=Hashtbl.fold (fun name counts acc ->
       (name,coverage counts)::acc) groups [] |> List.sort (fun (a,x) (b,y) ->
         let n = Int.compare y.total x.total in
         if n = 0 then String.compare a b else n)}
end
