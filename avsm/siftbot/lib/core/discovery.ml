module M = Siftbot_model

let words s =
  String.lowercase_ascii s
  |> String.map (function 'a' .. 'z' as c -> c | _ -> ' ')
  |> String.split_on_char ' '
  |> List.filter (fun w ->
         String.length w > 3
         && not
              (List.mem w
                 [
                   "with";
                   "from";
                   "this";
                   "that";
                   "your";
                   "have";
                   "will";
                   "reply";
                   "please";
                   "about";
                   "hello";
                   "thanks";
                   "regards";
                   "subject";
                   "would";
                 ]))
  |> List.sort_uniq String.compare

let distinct ms =
  let seen = Hashtbl.create 32 in
  List.filter
    (fun (m : M.message) ->
      let key =
        if m.message_ids = [] then M.key m.source
        else String.concat "," m.message_ids ^ ":" ^ m.sender ^ ":" ^ m.text
      in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    ms

let propose ~before ms =
  let before =
    match Ptime.of_rfc3339 before with
    | Ok (t, _, _) -> Ptime.to_rfc3339 ~tz_offset_s:0 t
    | Error _ -> invalid_arg "Use an RFC3339 discovery cutoff"
  in
  let ms =
    List.filter (fun (m : M.message) -> m.date < before) ms
    |> List.sort (fun (a : M.message) b ->
           compare (a.date, M.key a.source) (b.date, M.key b.source))
    |> distinct
  in
  let volume = Hashtbl.create 32 in
  let ms =
    List.filter
      (fun (m : M.message) ->
        let list_id =
          List.find_map
            (fun (k, v) ->
              if String.lowercase_ascii k = "list-id" then Some v else None)
            m.headers
        in
        let group = Option.value ~default:m.sender list_id in
        let count = Option.value ~default:0 (Hashtbl.find_opt volume group) in
        Hashtbl.replace volume group (count + 1);
        count < 4)
      ms
  in
  let counts = Hashtbl.create 32 in
  List.iter
    (fun (m : M.message) ->
      List.iter
        (fun word ->
          let group = Option.value ~default:[] (Hashtbl.find_opt counts word) in
          Hashtbl.replace counts word (m :: group))
        (words m.subject))
    ms;
  Hashtbl.to_seq counts |> List.of_seq
  |> List.map (fun (word, ms) ->
         let conversations =
           List.map
             (fun (m : M.message) -> M.key { m.source with id = m.thread })
             ms
           |> List.sort_uniq String.compare
         in
         (word, ms, List.length conversations))
  |> List.sort (fun (a, _, na) (b, _, nb) ->
         let c = compare nb na in
         if c = 0 then String.compare a b else c)
  |> List.filteri (fun i _ -> i < 12)
  |> List.map (fun (word, ms, n) ->
         let examples =
           ms
           |> List.map (fun (m : M.message) -> m.subject)
           |> List.sort_uniq String.compare
           |> List.filteri (fun i _ -> i < 3)
         in
         M.
           {
             id = "candidate-" ^ word;
             name = String.capitalize_ascii word;
             definition =
               Printf.sprintf "%s. %d distinct conversations. Examples: %s" word
                 n
                 (String.concat "; " examples);
             exclusions = "Review scope and exclusions before approval.";
             parent = None;
             status = "proposed";
             revision = 0;
           })

let header m name =
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k = name then Some (String.trim v) else None)
    m.M.headers

let classify areas ms =
  List.concat_map
    (fun (m : M.message) ->
      let assignment dim value score explanation =
        M.
          {
            target = m.source;
            conversation = false;
            dimension = dim;
            value;
            origin = Rule;
            version = "baseline/v1";
            source_revision = m.revision;
            explanation;
            evidence = [ m.source ];
            score;
          }
      in
      let delivery, explanation =
        match (header m "auto-submitted", header m "list-id") with
        | Some value, _ when String.lowercase_ascii value <> "no" ->
            ("automated", "Auto-Submitted header reports automated delivery.")
        | _, Some _ ->
            ("mailing-list", "List-Id identifies a list, not spam or broadcast.")
        | _ -> ("uncertain", "Delivery headers provide insufficient evidence.")
      in
      let base =
        [
          assignment Delivery delivery None explanation;
          assignment Triage "uncertain" None "Needs explicit relevance review.";
        ]
      in
      let terms = words (m.subject ^ " " ^ m.text) in
      let ranked =
        List.filter_map
          (fun (a : M.area) ->
            if a.status <> "approved" then None
            else
              let wanted = words (a.name ^ " " ^ a.definition) in
              let excluded = words a.exclusions in
              let hits = List.filter (fun w -> List.mem w terms) wanted in
              if hits = [] || List.exists (fun w -> List.mem w terms) excluded
              then None
              else
                Some
                  ( a,
                    float (List.length hits)
                    /. float (max 1 (List.length wanted)) ))
          areas
      in
      base
      @ List.map
          (fun ((a : M.area), score) ->
            assignment Area a.id (Some score)
              ("Definition-term overlap with approved area " ^ a.name
             ^ ". Ranking signal only."))
          ranked)
    (distinct ms)

let run db ~before =
  let ms = Store.messages db in
  Store.propose db (propose ~before ms) [];
  let areas = (Store.snapshot db ~query:"").areas in
  Store.propose db [] (classify areas ms)
