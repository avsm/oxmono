module M = Siftbot_model

type split = {
  training : M.message list;
  testing : M.message list;
  excluded_duplicates : int;
}

let split ~cutoff ms =
  let cutoff =
    match Ptime.of_rfc3339 cutoff with
    | Ok (t, _, _) -> Ptime.to_rfc3339 ~tz_offset_s:0 t
    | Error _ -> invalid_arg "Use an RFC3339 evaluation cutoff"
  in
  let seen = Hashtbl.create 32 in
  let ms =
    List.sort
      (fun (a : M.message) b ->
        let c = String.compare a.date b.date in
        if c = 0 then String.compare (M.key a.source) (M.key b.source) else c)
      ms
  in
  let dedup =
    List.filter
      (fun (m : M.message) ->
        let k =
          if m.message_ids = [] then M.key m.source
          else String.concat "," m.message_ids ^ m.sender ^ m.text
        in
        if Hashtbl.mem seen k then false
        else (
          Hashtbl.add seen k ();
          true))
      ms
  in
  let training, testing =
    List.partition (fun (m : M.message) -> m.date < cutoff) dedup
  in
  {
    training;
    testing;
    excluded_duplicates = List.length ms - List.length dedup;
  }

type report = {
  reviewed : int;
  predicted : int;
  correct : int;
  abstained : int;
  false_spam : int;
  missed_relevant : int;
}

let score ~threshold ~truth predictions =
  if (not (Float.is_finite threshold)) || threshold < 0. || threshold > 1. then
    invalid_arg "Score threshold must be between zero and one";
  let truth = List.filter (fun (a : M.assignment) -> a.origin = Admin) truth in
  let same (a : M.assignment) (b : M.assignment) =
    a.target = b.target
    && a.conversation = b.conversation
    && a.dimension = b.dimension
  in
  let representatives =
    List.fold_left
      (fun acc a -> if List.exists (same a) acc then acc else a :: acc)
      [] truth
  in
  List.fold_left
    (fun r (a : M.assignment) ->
      let choices =
        List.filter
          (fun (b : M.assignment) ->
            same a b && b.origin <> Admin
            && Option.fold ~none:false
                 ~some:(fun score -> score >= threshold)
                 b.score)
          predictions
      in
      let values xs =
        List.map (fun (a : M.assignment) -> a.value) xs
        |> List.sort_uniq String.compare
      in
      let wanted = List.filter (same a) truth |> values in
      let got = values choices in
      let predicted = got <> [] in
      let correct = got = wanted in
      let false_spam =
        a.dimension = Triage
        && (not (List.mem "spam" wanted))
        && List.mem "spam" got
      in
      let missed =
        a.dimension = Triage && List.mem "relevant" wanted
        && not (List.mem "relevant" got)
      in
      {
        reviewed = r.reviewed + 1;
        predicted = r.predicted + Bool.to_int predicted;
        correct = r.correct + Bool.to_int correct;
        abstained = r.abstained + Bool.to_int (not predicted);
        false_spam = r.false_spam + Bool.to_int false_spam;
        missed_relevant = r.missed_relevant + Bool.to_int missed;
      })
    {
      reviewed = 0;
      predicted = 0;
      correct = 0;
      abstained = 0;
      false_spam = 0;
      missed_relevant = 0;
    }
    representatives

let format r =
  Printf.sprintf
    "Reviewed target/dimension groups: %d; predicted: %d; correct: %d/%d; \
     abstained: %d/%d; false spam: %d; missed relevant: %d. Small samples are \
     inconclusive. Scores are not calibrated probabilities."
    r.reviewed r.predicted r.correct r.reviewed r.abstained r.reviewed
    r.false_spam r.missed_relevant

let freeze_baseline db ~cutoff =
  let revision = Store.revision db in
  let ms = Store.messages db in
  let parts = split ~cutoff ms in
  let snapshot = Store.snapshot db ~query:"" in
  let test_ids = List.map (fun (m : M.message) -> m.source) parts.testing in
  let train_ids = List.map (fun (m : M.message) -> m.source) parts.training in
  let current (a : M.assignment) =
    Option.fold ~none:false
      ~some:(fun (m : M.message) -> m.revision = a.source_revision)
      (Store.get db a.target)
  in
  let truth =
    List.filter
      (fun (a : M.assignment) ->
        current a && a.origin = Admin && (not a.conversation)
        && List.mem a.target test_ids)
      snapshot.assignments
  in
  let training =
    List.filter
      (fun (a : M.assignment) ->
        current a && a.origin = Admin && (not a.conversation)
        && List.mem a.target train_ids)
      snapshot.assignments
  in
  let predictions = Discovery.classify snapshot.areas parts.testing in
  let reports =
    List.concat_map
      (fun dim ->
        List.map
          (fun threshold ->
            let subset =
              List.filter (fun (a : M.assignment) -> a.dimension = dim)
            in
            Printf.sprintf "%s threshold %.2f: %s" (M.dimension dim) threshold
              (format
                 (score ~threshold ~truth:(subset truth) (subset predictions))))
          [ 0.; 0.25; 0.5; 0.75 ])
      [ M.Area; Triage; Delivery; Attention ]
  in
  let json codec x =
    match Jsont.Json.encode codec x with Ok x -> x | Error e -> failwith e
  in
  let str = Jsont.Json.string in
  let obj fields =
    Jsont.Json.object'
      (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fields)
  in
  let assignments xs =
    Jsont.Json.list
      (List.map
         (fun (a : M.assignment) ->
           obj
             [
               ("target", json M.source_jsont a.target);
               ("dimension", str (M.dimension a.dimension));
               ("value", str a.value);
               ("sourceRevision", str a.source_revision);
               ("explanation", str a.explanation);
               ("version", str a.version);
             ])
         xs)
  in
  let payload =
    obj
      [
        ("version", str "baseline/v1");
        ("cutoff", str cutoff);
        ("corpusRevision", Jsont.Json.int revision);
        ("excludedDuplicates", Jsont.Json.int parts.excluded_duplicates);
        ("trainingMessages", json (Jsont.list M.message_jsont) parts.training);
        ("testMessages", json (Jsont.list M.message_jsont) parts.testing);
        ("trainingDecisions", assignments training);
        ("reviewedTruth", assignments truth);
        ("predictions", assignments predictions);
        ( "areas",
          Jsont.Json.list
            (List.map
               (fun (a : M.area) ->
                 obj
                   [
                     ("id", str a.id);
                     ("name", str a.name);
                     ("definition", str a.definition);
                     ("exclusions", str a.exclusions);
                     ("status", str a.status);
                     ("revision", Jsont.Json.int a.revision);
                   ])
               snapshot.areas) );
        ("reports", json (Jsont.list Jsont.string) reports);
      ]
    |> Jmap_eio.Codec.encode_exn Jsont.json
  in
  let id = Digestif.SHA256.(to_hex (digest_string payload)) in
  Store.freeze db ~id ~revision ~payload;
  (id, reports)
