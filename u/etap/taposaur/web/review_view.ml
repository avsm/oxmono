open View
module S = Taposaur_review_store
module R = Taposaur_review
let h2 s = el "h2" [txt s]
let tag s = el "code" [txt s]
let link_to path params label = link (url path params) label
let id n = string_of_int n
let snapshot_link (s : S.snapshot) = link_to "/review-snapshot"
  ["id",id s.id] (Printf.sprintf "Snapshot %d: %s" s.id s.label)
let stamp f =
  let d = Unix.gmtime f in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC"
    (d.tm_year+1900) (d.tm_mon+1) d.tm_mday d.tm_hour d.tm_min
let status partial = if partial then "Partial inputs" else "Complete inputs"
module Ui = Taposaur_ui
let node = Ui.node
let table headers rows =
  Ui.table (List.map (fun label -> label,
    List.mem label ["Membership";"Abstracts";"Members"]) headers)
    (List.map (List.map (fun nodes -> node "td" [] nodes)) rows)
let percent n total = if total = 0 then 0. else 100. *. float n /. float total
let paper_params snapshot ?publisher ?filter () =
  ["id",id snapshot] @
  Option.fold ~none:[] ~some:(fun name -> ["publisher",name]) publisher @
  Option.fold ~none:[] ~some:(fun (field,present) ->
    ["field",R.Statistics.label field;
     "availability",if present then "present" else "missing"]) filter
let papers_link snapshot ?publisher ?filter n =
  link_to "/review-papers" (paper_params snapshot ?publisher ?filter ()) (View.count n)
let sort_cell = Ui.cell
let spark present total = Ui.coverage ~present ~total
let sortable_table = Ui.table
let compare_form snapshots = match snapshots with
  | after :: before :: _ ->
      let select name selected label = el "label" [txt label;
        Htmlit.El.v "select" ~at:[Htmlit.At.v "name" name]
          (List.map (fun (s : S.snapshot) ->
            Htmlit.El.v "option" ~at:([Htmlit.At.v "value" (id s.id)] @
              if selected = s.id then [Htmlit.At.v "selected" ""] else [])
              [txt (Printf.sprintf "%d: %s" s.id s.label)]) snapshots)] in
      Htmlit.El.v "form" ~at:[Htmlit.At.v "method" "get";
        Htmlit.At.v "action" "/review-compare"]
        [Htmlit.El.v "div" ~at:[Htmlit.At.v "class" "search-line"]
          [select "before" before.id "Before";
           select "after" after.id "After";
           el "button" [txt "Compare snapshots"]]]
  | _ -> el "span" []
let input_preview (i : R.Definition.input) =
  let source = i.source in
  let description = match source.kind with
    | R.Definition.List ->
        [p "Fixed input list";tag (Option.get source.blob)]
    | Recorded_search -> [p ("Recorded search: " ^ Option.get source.provider);
        el "pre" [txt (Option.get source.query)]] @
        (match source.parameters with None -> []
         | Some json -> [el "pre" [txt (R.encode Jsont.json json)]])
    | Catalogue ->
        let q = Option.get source.local in
        let holdings = match q.holdings with
          | R.Definition.Any -> "Any holdings" | Pdf -> "Has PDF"
          | Xml -> "Has publisher XML" | Tei -> "Has TEI"
          | S2orc -> "Has S2ORC" | Unconverted -> "Unconverted" in
        let metadata = List.map (fun (p : R.Definition.predicate) ->
          let field = match p.field with Title -> "Title" | Abstract -> "Abstract"
            | Publisher -> "Publisher" in
          field ^ (match p.availability with Present -> " present"
            | Missing -> " missing")) q.metadata in
        [p "Local catalogue search · DOI records · literal AND semantics";
         el "pre" [txt (if q.text = "" then "All DOI records" else q.text)];
         p (String.concat " · " (List.filter ((<>) "")
           [q.journal;q.publisher;
            Option.fold ~none:"" ~some:(fun n -> "From " ^ id n) q.year_from;
            Option.fold ~none:"" ~some:(fun n -> "To " ^ id n) q.year_to;
            holdings] @ metadata))] in
  el "details" [el "summary" [txt (i.label ^ " (" ^ i.key ^ ")")];
    el "div" description]
let index reviews = page "Reviews" [
  p "Research requests, saved query definitions and frozen deliveries.";
  if reviews = [] then p "No reviews have been imported yet."
  else table ["Review";"Requested by";"Owner";"Definition"]
    (List.map (fun (r : S.review) ->
      [[link_to "/review" ["slug",r.slug] r.title; el "br" [];tag r.slug];
       [txt r.requester];[txt r.owner];[txt ("r" ^ id r.revision)]]) reviews)]
let review (r : S.review) revisions runs snapshots =
  page r.title [link "/reviews" "← Reviews";
    p ("Requested by " ^ r.requester ^ " · Owner: " ^ r.owner);
    h2 "Snapshots";
    p "Snapshots preserve membership and metadata. \
       Later imports and refreshes do not change them.";
    (if snapshots = [] then p "No snapshot has been created yet."
     else table ["Delivery";"Created";"Membership";"Abstracts";"Inputs"]
       (List.map (fun (s : S.snapshot) ->
         [[snapshot_link s];[txt (stamp s.created)];[txt (id s.members)];
          [txt (id s.abstracts)];[txt (status s.partial)]]) snapshots));
    compare_form snapshots;
    h2 "Current inputs";
    el "div" (match revisions with [] -> []
      | (d : S.revision) :: _ -> List.map input_preview d.definition.inputs);
    h2 "Definition revisions";
    table ["Revision";"Reason";"Actor";"Created";"Definition"]
      (List.map (fun (d : S.revision) ->
        [[txt ("r" ^ id d.number)];[txt d.reason];[txt d.actor];
         [txt (stamp d.created)];[link_to "/review-definition"
           ["slug",r.slug;"revision",id d.number] "Query JSON"]]) revisions);
    h2 "Recorded runs";
    p "Local searches use literal words joined by AND. \
       Recorded external queries retain the provider's native syntax.";
    table ["Run";"Revision";"Members";"Inputs and evidence"]
      (List.map (fun (run : S.run) ->
        [[txt (id run.id);p (stamp run.created);p ("By " ^ run.actor)];
         [txt ("r" ^ id run.revision)];[txt (id run.members)];
         List.concat_map (fun (i : S.input_result) ->
           [link_to "/review-input" ["run",id run.id;"key",i.key] i.key;
            p (Printf.sprintf "%d occurrences · %s · %d invalid lines"
              i.occurrences (status i.partial) (List.length i.diagnostics));
            tag i.blob]) run.inputs]) runs)]
let snapshot (s : S.snapshot) statistics =
  let exports = List.map (fun (format,label) ->
    link_to "/review-export" ["id",id s.id;"format",format] label)
    ["csv","Export CSV";"doi","DOI list";
     "jsonl","Metadata JSONL";"manifest","Manifest"]
    @ [link "/docs/corpus" "Corpus schema"] in
  let coverage = match statistics with
    | None -> [p "Coverage statistics have not been cached yet."]
    | Some (stats : R.Statistics.t) ->
        let c = stats.overall in
        let count c field = List.assoc field c.R.Statistics.present in
        [h2 "Coverage and missing data";
         p "Frozen at capture. Formats overlap; holdings do not guarantee successful parsing.";
         node "p" ["class","coverage-legend"]
           [node "span" ["class","legend-present"] [txt "Present"];
            node "span" ["class","legend-missing"] [txt "Missing"];
            txt " · Click a count to browse papers. Click a header to sort."];
         sortable_table ~class_name:"review-coverage" ["Metadata or format",false;"Present",true;
           "Missing",true;"Coverage",true]
           (List.map (fun (name,n) ->
             let field = Option.get (R.Statistics.of_label name) in
             [sort_cell name [txt name];
              sort_cell (id n) [papers_link s.id ~filter:(field,true) n];
              sort_cell (id (c.total-n))
                [papers_link s.id ~filter:(field,false) (c.total-n)];
              sort_cell (Printf.sprintf "%.12g" (percent n c.total))
                [spark n c.total;node "span" ["class","coverage-label"]
                  [txt (if c.total = 0 then "No papers" else
                    Printf.sprintf "%.1f%% present" (percent n c.total))]]])
             (List.filter (fun (name,_) ->
               name <> "Stale metadata" && name <> "Metadata error") c.present));
         el "p" [papers_link s.id ~filter:(R.Statistics.Stale,true)
           (count c "Stale metadata"); txt " stale metadata records · ";
           papers_link s.id ~filter:(R.Statistics.Error,true)
             (count c "Metadata error");txt " metadata errors"];
         h2 "Publisher breakdown";
         p "Publisher names are retained as captured. Unknown publishers need metadata enrichment.";

         sortable_table ~class_name:"review-publishers" ~gap_sort:true ["Publisher",false;"Papers",true;
           "Missing title",true;"Missing abstract",true;
           "Missing full text",true;"Not in catalogue",true]
           (List.map (fun (name,c) ->
             [sort_cell (if name = "" then "Unknown publisher" else name)
                [link_to "/review-papers" (paper_params s.id ~publisher:name ())
                  (if name = "" then "Unknown publisher" else name)];
              sort_cell (id c.R.Statistics.total)
                [papers_link s.id ~publisher:name c.total]] @
             List.map (fun field ->
               let present = count c (R.Statistics.label field) in
               let missing = c.total - present in
               let count_link = papers_link s.id ~publisher:name
                 ~filter:(field,false) missing in
               sort_cell ~ratio:(percent missing c.total) (id missing)
                 (if missing = 0 then [count_link] else
                  [node "div" ["class","gap-count"]
                    [count_link;
                     el "small" [txt (if c.total = 0 then "—" else
                       Printf.sprintf "%.1f%% missing" (percent missing c.total))]];
                  spark present c.total]))
               [R.Statistics.Title;Abstract;Full_text;Catalogue])
             stats.publishers)] in
  page (Printf.sprintf "%s · Snapshot %d" s.title s.id)
    ([link_to "/review" ["slug",s.slug] "← Review history";
      p (s.label ^ " · " ^ stamp s.created ^ " · By " ^ s.actor);
      p ("Requested by " ^ s.requester ^ " · Owner: " ^ s.owner);
      el "p" [Ui.badge ~tone:(if s.partial then `Warning else `Good) (status s.partial);
        txt (Printf.sprintf " · Definition r%d · Run %d" s.revision s.run_id)];
      node "div" ["class","ui-metrics"]
        [Ui.metric ~label:"papers" (papers_link s.id s.members);
         Ui.metric ~label:"titles" (papers_link s.id ~filter:(R.Statistics.Title,true) s.titles);
         Ui.metric ~label:"abstracts" (papers_link s.id ~filter:(R.Statistics.Abstract,true) s.abstracts);
         Ui.metric ~label:"in catalogue" (papers_link s.id ~filter:(R.Statistics.Catalogue,true) s.held)];
      node "nav" ["class","ui-exports";"aria-label","Snapshot exports"] exports;
      (match s.parent with None -> el "span" []
       | Some before -> link_to "/review-compare"
           ["before",id before;"after",id s.id] "Compare with parent snapshot")]
     @ coverage)
let compare (before : S.snapshot) (after : S.snapshot) (d : S.difference) =
  page "Compare snapshots" [snapshot_link before;p "Compared with";
    snapshot_link after;
    p (Printf.sprintf
      "%d added · %d removed · %d changed among shared members · %d gained abstracts"
      d.added d.removed d.changed d.abstracts_added)]
let download_capability (o : R.observation) =
  let publisher = Option.value ~default:"" o.bibliography.publisher in
  let strategy = match Taposaur_download.publisher_capability publisher with
    | None -> [Ui.badge "No publisher strategy";
        el "small" [txt (if String.trim publisher = "" then
          "Publisher unknown" else "OA discovery may still find a PDF")]]
    | Some c -> [Ui.badge ~tone:`Good "Strategy available";
        p (c.publisher ^ " · " ^ c.formats);
        el "small" [txt c.requirements]] in
  let h = o.holdings in
  let held = h.pdf || h.xml || h.tei || h.s2orc in
  strategy @ [p (if held then "Full text held" else "Full text missing")]

let papers (s : S.snapshot) ~publisher ~filter ~total rows =
  let params = paper_params s.id ?publisher ?filter () in
  let more = List.length rows > 50 in
  let rows = if more then List.filteri (fun i _ -> i < 50) rows else rows in
  let description = Option.fold ~none:"All publishers" ~some:(function
    | "" -> "Unknown publisher" | s -> s) publisher ^ " · " ^
    Option.fold ~none:"All papers" ~some:(fun (field,present) ->
      R.Statistics.label field ^ if present then " present" else " missing") filter in
  page (Printf.sprintf "%s · Snapshot %d papers" s.title s.id)
    [link_to "/review-snapshot" ["id",id s.id] "← Snapshot coverage";
     h2 description;
     p (Printf.sprintf "%s matching papers · Frozen %s"
       (View.count total) (stamp s.created));
     p "These values belong to this snapshot. Current paper details may differ.";
     p "Strategies use the saved publisher and current download capabilities. \
       Credentials and access still need verification.";
     (if rows = [] then p "No papers match this filter." else
       table ["Paper";"Abstract";"Download strategy"] (List.map (fun (o : R.observation) ->
         let title = Option.value ~default:"Title unavailable"
           o.bibliography.title in
         [[el "strong" [if o.in_catalogue then
             link_to "/paper" ["uid","doi:" ^ o.doi] title else txt title];
           el "br" [];tag o.doi];
          [match o.bibliography.abstract with None -> txt "Abstract unavailable"
           | Some abstract -> el "details" [el "summary" [txt "Read abstract"];
               node "p" ["class","abstract"] [txt abstract]]];
          download_capability o]) rows));
     (if more then
       let last = List.hd (List.rev rows) in
       link_to "/review-papers" (params @ ["after",last.R.doi]) "Next 50 papers →"
      else el "span" [])]
