type consolidation = Off | Full | Doi_only

let consolidation = function Off -> "0" | Full -> "1" | Doi_only -> "2"

let flag b = if b then "1" else "0"

type t = { server : string; c : Fetch.plain }

(* Every field is sent on every request: the server defaults them all
   off, so leaving one out would be asking for less than the richest
   conversion. *)
let params ?(segment_sentences = true) ?(grobid_refine = true)
    ?(generate_ids = true) ?(consolidate_references = Full) () =
  [ ("segmentSentences", flag segment_sentences);
    ("grobidRefine", flag grobid_refine);
    ("generateIDs", flag generate_ids);
    ("consolidateReferences", consolidation consolidate_references) ]

let client ~server t =
  { server = Service.server ~caller:"Pub2tei.client" server;
    c = Service.plain t }

let server t = t.server

let isalive ~sw t = Fetch.get ~sw t.c (t.server ^ "/service/isalive")
    ~headers:Fetch.Header.[ accept, [ pref "text/plain" ] ]

let process_xml ~sw ?segment_sentences ?grobid_refine ?generate_ids
    ?consolidate_references ?(filename = "input.xml") ?boundary t doc =
  let ps =
    params ?segment_sentences ?grobid_refine ?generate_ids
      ?consolidate_references ()
  in
  let headers, body =
    Fetch.Form.multipart ?boundary
      (Fetch.Form.file ~name:"input" ~filename ~content_type:"application/xml"
         doc
       :: List.map (fun (k, v) -> Fetch.Form.field k v) ps)
  in
  Fetch.post ~sw t.c
    (t.server ^ "/service/processXML")
    ~body
    ~headers:
      (Fetch.Header.append headers
         Fetch.Header.[ (accept, [ pref "application/xml" ]) ])

let process_text ~sw ?segment_sentences ?grobid_refine ?generate_ids
    ?consolidate_references t text =
  let ps =
    ("text", text)
    :: params ?segment_sentences ?grobid_refine ?generate_ids
         ?consolidate_references ()
  in
  let headers, body = Fetch.Form.urlencoded ps in
  Fetch.post ~sw t.c
    (t.server ^ "/service/processText")
    ~body
    ~headers:
      (Fetch.Header.append headers
         Fetch.Header.[ (accept, [ pref "application/xml" ]) ])
