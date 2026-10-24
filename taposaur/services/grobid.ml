type consolidation = Off | Full | Doi_only

let consolidation = function Off -> "0" | Full -> "1" | Doi_only -> "2"

let flag b = if b then "1" else "0"

type t = { server : string; c : Fetch.plain }

let default_coordinates =
  [ "ref"; "biblStruct"; "persName"; "figure"; "formula"; "head"; "s"; "p";
    "note"; "title"; "affiliation" ]

(* Every field is sent on every request: the server defaults them all
   off, so leaving one out would be asking for less than the richest
   extraction. *)
let params ?(segment_sentences = true) ?(generate_ids = true)
    ?(consolidate_header = Full) ?(consolidate_citations = Full)
    ?(consolidate_funders = Off) ?(include_raw_citations = true)
    ?(include_raw_affiliations = true) ?(include_raw_copyrights = true)
    ?(include_discarded_text = true)
    ?(tei_coordinates = default_coordinates) () =
  [ ("segmentSentences", flag segment_sentences);
    ("generateIDs", flag generate_ids);
    ("consolidateHeader", consolidation consolidate_header);
    ("consolidateCitations", consolidation consolidate_citations);
    ("consolidateFunders", consolidation consolidate_funders);
    ("includeRawCitations", flag include_raw_citations);
    ("includeRawAffiliations", flag include_raw_affiliations);
    ("includeRawCopyrights", flag include_raw_copyrights);
    ("includeDiscardedText", flag include_discarded_text) ]
  (* The endpoint reads [teiCoordinates] as a list of same-named parts
     rather than one delimited value, so an element name travels in a
     part of its own and no name at all means no coordinates. *)
  @ List.map (fun e -> ("teiCoordinates", e)) tei_coordinates

let client ~server t =
  { server = Service.server ~caller:"Grobid.client" server;
    c = Service.plain t }

let server t = t.server

let isalive ~sw t = Fetch.get ~sw t.c (t.server ^ "/api/isalive")
    ~headers:Fetch.Header.[ accept, [ pref "text/plain" ] ]

let process_fulltext ~sw ?segment_sentences ?generate_ids
    ?consolidate_header ?consolidate_citations ?consolidate_funders
    ?include_raw_citations ?include_raw_affiliations ?include_raw_copyrights
    ?include_discarded_text ?tei_coordinates ?(filename = "input.pdf") ?boundary
    t doc =
  let ps =
    params ?segment_sentences ?generate_ids ?consolidate_header
      ?consolidate_citations ?consolidate_funders ?include_raw_citations
      ?include_raw_affiliations ?include_raw_copyrights ?include_discarded_text
      ?tei_coordinates ()
  in
  let headers, body =
    Fetch.Form.multipart ?boundary
      (Fetch.Form.file ~name:"input" ~filename ~content_type:"application/pdf"
         doc
       :: List.map (fun (k, v) -> Fetch.Form.field k v) ps)
  in
  Fetch.post ~sw t.c
    (t.server ^ "/api/processFulltextDocument")
    ~body
    ~headers:
      (Fetch.Header.append headers
         Fetch.Header.[ (accept, [ pref "application/xml" ]) ])
