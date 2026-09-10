(* SPDX-License-Identifier: ISC *)
open Json

let now engine = Eio.Time.now engine.Engine.runner.system#clock
let position time = Int64.of_float (time *. 1e9)
let dedup ~repo ~ref_ ~sha = "ref/" ^ repo ^ "/" ^ ref_ ^ "/" ^ sha

let gap store ~source ~now ~cursor ~reason =
  let previous = Option.map decode (Store.get store "gap" source) in
  let details =
    match previous with
    | Some value when get "status" value = "pending" ->
        List.map
          (fun key -> (key, required key value))
          [ "detectedAt"; "fromCursor"; "reason" ]
    | _ ->
        [
          ("detectedAt", Jsont.Json.number now);
          ("fromCursor", str cursor);
          ("reason", str reason);
        ]
  in
  Store.put store "gap" source
    (encode
       (obj
          (details
          @ [
              ("source", str source);
              ("status", str "pending");
              ("historicalEventsComplete", bool false);
            ])));
  Store.schedule store "recover-source" source

let resume store (policy : Operations.t) ~source ~jetstream ~now =
  let factor = if jetstream then 1e6 else 1e9 in
  let current = Int64.of_float (now *. factor) in
  match Store.get store "cursor" source with
  | None ->
      Store.put store "cursor" source (Int64.to_string current);
      false
  | Some cursor ->
      let value = Int64.of_string cursor in
      let stale =
        Int64.to_float value /. factor
        < now -. float_of_int (policy.replay_hours * 3600)
      in
      let pruned =
        Option.fold ~none:false
          ~some:(fun floor -> value <= Int64.of_string floor)
          (Store.get store "replay-floor" source)
      in
      let legacy =
        Option.fold ~none:false
          ~some:(fun floor ->
            Int64.to_float value /. factor <= float_of_string floor)
          (Store.get store "replay-floor" "legacy-seconds")
      in
      if stale || pruned || legacy then (
        gap store ~source ~now ~cursor
          ~reason:
            (if pruned || legacy then "local replay receipts expired"
             else "upstream replay window exceeded");
        (* Persist the recovery task before advancing. A crash here can repeat
           reconciliation but cannot silently abandon the missing interval. *)
        Store.put store "cursor" source (Int64.to_string current);
        true)
      else false

let seed engine =
  if Store.get engine.Engine.store "schema" "ref-checkpoints" = None then (
    Store.fold engine.store "pipeline-view" ~init:()
      ~f:(fun () (_, raw) ->
        let value = decode raw in
        let trigger = required "trigger" value in
        if Engine.kind trigger = Job.Push then (
          let repo = get "repo" value and ref_ = get "ref" trigger in
          let at = Atp.Tid.timestamp_us (Atp.Tid.of_string (get "id" value)) in
          let sha = get "commit" value in
          Store.checkpoint_ref engine.store ~repo ~ref_ ~sha
            ~position:(Int64.mul at 1000L);
          Store.put engine.store "dispatch" (dedup ~repo ~ref_ ~sha)
            (get "id" value)))
      ();
    Store.put engine.store "schema" "ref-checkpoints" "1")

let parse_refs raw =
  let heads = Hashtbl.create 32 and peeled = Hashtbl.create 8 in
  let default = ref None in
  String.split_on_char '\n' raw
  |> List.iter (fun line ->
      if line <> "" then
        match String.split_on_char '\t' line with
        | [ value; "HEAD" ] when String.starts_with ~prefix:"ref: " value ->
            default := Some (String.sub value 5 (String.length value - 5))
        | [ value; name ]
          when String.starts_with ~prefix:"refs/heads/" name
               || String.starts_with ~prefix:"refs/tags/" name ->
            let value = sha value in
            if String.ends_with ~suffix:"^{}" name then
              Hashtbl.replace peeled
                (String.sub name 0 (String.length name - 3))
                value
            else Hashtbl.replace heads name value
        | [ _; "HEAD" ] -> ()
        | _ -> invalid "invalid Git ref advertisement");
  Hashtbl.fold
    (fun name hash refs ->
      let hash = Option.value ~default:hash (Hashtbl.find_opt peeled name) in
      (name, hash, !default = Some name) :: refs)
    heads []
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)

let repo engine id =
  match Catalog.managed engine.Engine.catalog id with
  | None -> ()
  | Some repo ->
      let started = now engine in
      let input : Runner.input =
        {
          id = "recovery";
          repo = id;
          source = repo.source;
          commit = String.make 40 '0';
          metadata = obj [];
        }
      in
      let raw =
        Eio.Time.with_timeout_exn engine.runner.system#clock 15. (fun () ->
            Runner.capture engine.runner input
              [
                "git";
                "-c";
                "core.hooksPath=/dev/null";
                "ls-remote";
                "--symref";
                "--";
                repo.source;
              ])
      in
      List.iter
        (fun (ref_, sha, default_ref) ->
          let previous = Store.ref_state engine.store ~repo:id ~ref_ in
          let unchanged =
            match previous with
            | Some (previous, at) -> previous = sha || at > position started
            | None -> false
          in
          if unchanged then Store.touch_ref engine.store ~repo:id ~ref_
          else (
            gap engine.store ~source:repo.knot ~now:started
              ~cursor:
                (Option.value ~default:"0"
                   (Store.get engine.store "cursor" repo.knot))
              ~reason:"current Git ref was not checkpointed by the event stream";
            let old =
              Option.fold ~none:(String.make 40 '0') ~some:fst previous
            in
            let request =
              obj
                [
                  ("repo", str id);
                  ( "trigger",
                    obj
                      [
                        ("$type", str "sh.tangled.ci.trigger#push");
                        ("ref", str ref_);
                        ("newSha", str sha);
                        ("oldSha", str old);
                      ] );
                  ( "recovery",
                    obj
                      [
                        ("mode", str "current_refs");
                        ("historicalEventsComplete", bool false);
                        ("changedFilesKnown", bool false);
                        ("committerKnown", bool false);
                      ] );
                ]
            in
            ignore
              (Engine.create engine
                 ~dedup:(dedup ~repo:id ~ref_ ~sha)
                 ~automatic:true ~default_ref
                 ~actor:("did:web:" ^ engine.hostname)
                 request);
            Store.checkpoint_ref engine.store ~repo:id ~ref_ ~sha
              ~position:(position started)))
        (parse_refs raw)

let schedule engine ~source =
  let store = engine.Engine.store in
  List.iter
    (fun id -> Store.schedule store "recover" id)
    (Catalog.repositories engine.catalog);
  if source = "jetstream" then
    List.iter
      (fun owner -> Store.schedule store "recover-pulls" owner)
      (Catalog.members engine.catalog)

let settled store ~now =
  if
    Store.pending_source store "recovery" = 0
    && List.for_all
         (fun ns ->
           let count, _, _ = Store.usage store ns in
           count = 0)
         [ "recover-source"; "recover"; "recover-pulls"; "reconcile" ]
  then
    Store.list store "gap"
    |> List.iter (fun (key, raw) ->
        let value = decode raw in
        if get "status" value = "pending" then
          Store.put store "gap" key
            (encode
               (obj
                  (("status", str "reconciled")
                  :: ("reconciledAt", Jsont.Json.number now)
                  :: List.remove_assoc "status" (members value)))))
