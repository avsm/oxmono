open Taposaur
module R = Taposaur_review
module S = Taposaur_review_store
module C = Taposaur_catalog
type binding = { key : string; bytes : string; partial : bool }
let query (local : R.Definition.local) =
  let open R.Definition in
  let holdings = match local.holdings with
    | Any -> C.Query.Any | Pdf -> Pdf | Xml -> Xml | Tei -> Tei
    | S2orc -> S2orc | Unconverted -> Unconverted in
  let metadata = List.map (fun p ->
    (match p.field with Title -> C.Query.Title | Abstract -> Abstract
     | Publisher -> Publisher),
    (match p.availability with Present -> C.Query.Present | Missing -> Missing))
    local.metadata in
  {C.Query.text=local.text;scheme=Some Uid.Scheme.Doi;
   publisher=local.publisher;journal=local.journal;
   year_from=local.year_from;year_to=local.year_to;
   holdings;metadata;after=None;limit=100}
let budget max_members = if max_members < 1 || max_members > 2000000 then
  invalid_arg "Review member budget must be 1..2000000"
let execute ?(max_members=100000) store catalog
    (revision : S.revision) ~actor bindings =
  budget max_members;
  let definition = revision.definition in
  (match R.Definition.validate definition with
   | Ok () -> () | Error e -> invalid_arg e);
  let keys = List.map (fun (b : binding) -> b.key) bindings in
  if List.length keys <> List.length (List.sort_uniq String.compare keys) then
    invalid_arg "Duplicate input binding";
  List.iter (fun key ->
    match List.find_opt (fun (i : R.Definition.input) -> i.key = key)
      definition.inputs with
    | Some i when i.source.kind <> R.Definition.Catalogue -> ()
    | _ -> invalid_arg ("Unexpected input binding " ^ key)) keys;
  let count = ref 0 and bytes = ref 0 in
  let account n size =
    count := !count + n; bytes := !bytes + size;
    if !count > max_members then invalid_arg
      "Input occurrence budget exceeded. Refine the query or split the inputs";
    if !bytes > 64 * 1024 * 1024 then invalid_arg
      "Review inputs exceed 64 MiB" in
  let prepared = List.map (fun (input : R.Definition.input) ->
    let binding = List.find_opt (fun b -> b.key = input.key) bindings in
    let supplied = match input.source.kind with
      | R.Definition.Catalogue -> None
      | List ->
          let raw = S.blob store (Option.get input.source.blob) in
          Option.iter (fun b -> if b.bytes <> raw then
            invalid_arg "A fixed list cannot be replaced within a run") binding;
          Some (raw, Option.fold ~none:false ~some:(fun b -> b.partial) binding)
      | Recorded_search ->
          (match binding with
           | None -> invalid_arg ("Recorded search needs --input " ^ input.key)
           | Some b -> Some (b.bytes,b.partial)) in
    match supplied with
    | None -> input,None
    | Some (raw,partial) ->
        if String.length raw > 32 * 1024 * 1024 then
          invalid_arg "Input exceeds 32 MiB";
        let parsed = R.parse input.source.format raw in
        if parsed.diagnostics <> [] && not partial then invalid_arg
          (Printf.sprintf
            "%s: %d invalid lines. Correct them or mark the input partial"
            input.key (List.length parsed.diagnostics));
        account (List.length parsed.records) (String.length raw);
        input,Some (raw,partial,parsed)) definition.inputs in
  let local_results = C.Snapshot.with_read catalog (fun snapshot ->
    let started = Unix.gettimeofday () in
    List.filter_map (fun (input,prepared) -> match prepared with
      | Some _ -> None
      | None ->
          let q = query (Option.get input.R.Definition.source.local) in
          let rec collect after acc =
            if Unix.gettimeofday () -. started > 60. then
              invalid_arg "Catalogue capture exceeded 60 seconds";
            let page = C.Snapshot.search snapshot {q with after} in
            let records = List.map (fun (s : Summary.t) ->
              {R.doi=Uid.id s.uid; bibliography=s.bibliography;line=0})
              page.papers in
            account (List.length records) 0;
            let acc = List.rev_append records acc in
            match page.next with None -> List.rev acc
            | Some next -> collect (Some next) acc in
          Some (input.key,collect None [])) prepared) in
  let results = List.map (fun (input,prepared) ->
    match prepared with
    | Some (raw,partial,parsed) ->
        input.R.Definition.key,raw,partial,parsed
    | None ->
        let records = List.assoc input.key local_results in
        let raw = String.concat "" (List.map (fun (r : R.record) ->
          R.encode Bibliography.jsont {r.bibliography with doi=Some r.doi}
          ^ "\n") records) in
        account 0 (String.length raw);
        input.key,raw,false,{R.records;diagnostics=[]}) prepared in
  let inputs = List.map (fun (key,raw,partial,parsed) ->
    let input = List.find (fun (i : R.Definition.input) -> i.key = key)
      definition.inputs in
    let format = if input.source.kind = R.Definition.Catalogue then
      R.Definition.Metadata_jsonl else input.source.format in
    {S.key;blob=S.put_blob store raw;partial;
      format;
      occurrences=List.length parsed.R.records;diagnostics=parsed.diagnostics})
    results in
  S.publish_run store revision ~actor inputs
let snapshot ?(publishers=[]) ?(abstracts=[]) ?(works=[]) ?(max_members=100000) ?(max_bytes=256 * 1024 * 1024)
    ?(capture_seconds=60.) store catalog ~run_id ~parent ~label ~actor =
  budget max_members;
  if max_bytes < 1 || max_bytes > 4 * 1024 * 1024 * 1024 then
    invalid_arg "Snapshot byte budget must be 1..4 GiB";
  if not (capture_seconds > 0. && capture_seconds <= 3600.) then
    invalid_arg "Capture time budget must be 0..3600 seconds";
  let members = S.members store run_id in
  if List.length members > max_members then
    invalid_arg "Snapshot member budget exceeded";
  let evidence = Hashtbl.create (List.length publishers) in
  List.iter (fun (e : Publisher_evidence.t) -> Hashtbl.replace evidence e.doi e) publishers;
  let abstract_evidence = Hashtbl.create (List.length abstracts) in
  List.iter (fun (e : Abstract_evidence.t) ->
    Hashtbl.replace abstract_evidence e.doi e) abstracts;
  let work_evidence = Hashtbl.create (List.length works) in
  List.iter (fun (w : Work_evidence.t) ->
    Hashtbl.replace work_evidence w.doi w) works;
  let observations = C.Snapshot.with_read catalog (fun snapshot ->
    let started = Unix.gettimeofday () and bytes = ref 0 in
    List.map (fun (member : R.member) ->
      if Unix.gettimeofday () -. started > capture_seconds then
        invalid_arg "Catalogue capture exceeded its time budget";
      let uid = match Uid.of_input member.doi with
        | Ok uid -> uid | Error e -> failwith e in
      let observation = match C.Snapshot.find snapshot uid with
        | None -> R.freeze member ~summary:None ~catalogue_stamp:None
            ~stale:false ~error:None ~diagnostics:[]
        | Some row -> R.freeze member ~summary:(Some row.summary)
            ~catalogue_stamp:(Some row.stamp) ~stale:row.stale
            ~error:row.error ~diagnostics:row.diagnostics in
      let observation = match Hashtbl.find_opt evidence member.doi with
        | Some e when Option.fold ~none:true ~some:(fun p -> String.trim p = "" || p=e.publisher)
            observation.bibliography.publisher ->
          {observation with bibliography={observation.bibliography with publisher=Some e.publisher};
            evidence={R.field="publisher";source=Publisher_evidence.label e} ::
              List.filter (fun (e:R.evidence) -> e.field<>"publisher") observation.evidence}
        | _ -> observation in
      let observation = match Hashtbl.find_opt abstract_evidence member.doi with
        | Some e when Option.fold ~none:true ~some:(fun a -> String.trim a = "")
            observation.bibliography.abstract ->
          {observation with bibliography={observation.bibliography with abstract=Some e.abstract};
            evidence={R.field="abstract";source=Abstract_evidence.label e} ::
              List.filter (fun (e:R.evidence) -> e.field<>"abstract") observation.evidence}
        | _ -> observation in
      let observation = match Hashtbl.find_opt work_evidence member.doi with
        | Some w -> R.fill w observation
        | None -> observation in
      bytes := !bytes +
        String.length (R.encode R.observation_jsont observation);
      if !bytes > max_bytes then
        invalid_arg "Snapshot exceeds its byte budget";
      observation) members) in
  S.publish_snapshot ~max_bytes store ~run_id ~parent ~label ~actor observations
