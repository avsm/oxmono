(* SPDX-License-Identifier: ISC *)
open Json

let collections =
  [ "sh.tangled.spindle.member"; "sh.tangled.repo"; "sh.tangled.repo.pull" ]

let report source exn =
  Printf.eprintf "spindle observer %s: %s\n%!" source (Printexc.to_string exn)

let ws_url url =
  if String.starts_with ~prefix:"https://" url then
    "wss://" ^ String.sub url 8 (String.length url - 8)
  else if String.starts_with ~prefix:"http://" url then
    "ws://" ^ String.sub url 7 (String.length url - 7)
  else url

let timestamp event key =
  let n = number (required key event) in
  if n < 0. || n >= Int64.to_float Int64.max_int || Float.floor n <> n then
    invalid "invalid event timestamp";
  Int64.of_float n

let connect ~system ~store ~source ~url ~jetstream ~health ~policy =
  let reset =
    Recovery.resume store policy ~source ~jetstream
      ~now:(Eio.Time.now system#clock)
  in
  let cursor =
    match Store.get store "cursor" source with
    | Some value -> value
    | None ->
        let unit_ = if jetstream then 1e6 else 1e9 in
        let value =
          Int64.to_string (Int64.of_float (Eio.Time.now system#clock *. unit_))
        in
        Store.put store "cursor" source value;
        value
  in
  let url =
    Httpz_uri.of_string_exn (ws_url url) |> fun uri ->
    let uri = Httpz_uri.remove_query_param uri "cursor" in
    (if reset && jetstream then uri
     else Httpz_uri.add_query_param uri ~key:"cursor" ~value:cursor)
    |> Httpz_uri.to_string
  in
  Httpz_websocket_eio.with_connection
    ~on_activity:(fun () ->
      Health.activity health source ~now:(Eio.Time.now system#clock))
    system url
  @@ fun socket ->
  Health.connected health source ~now:(Eio.Time.now system#clock);
  Store.schedule store "recover-source" source;
  while
    Httpz_websocket.receive socket ~f:(fun kind bytes ~off ~len ->
        if kind <> Httpz_websocket.Text then invalid "event must be JSON text";
        let raw = Bytes.sub_string bytes off len in
        let event = decode raw in
        let timestamp =
          timestamp event (if jetstream then "time_us" else "created")
        in
        (* JSON numbers are doubles. Overlap knot timestamps by one microsecond
       to preserve nanosecond events that round down. Event keys deduplicate. *)
        let cursor =
          Int64.max 0L (Int64.sub timestamp (if jetstream then 0L else 1024L))
        in
        let id =
          if jetstream then
            source ^ "/" ^ Int64.to_string timestamp ^ "/" ^ get "did" event
            ^
            match field "commit" event with
            | None -> "/" ^ get "kind" event
            | Some commit ->
                "/" ^ get "collection" commit ^ "/" ^ get "rkey" commit ^ "/"
                ^ get "operation" commit
          else source ^ "/" ^ get "nsid" event ^ "/" ^ get "rkey" event
        in
        let pending =
          obj
            [
              ("source", str source);
              ("jetstream", bool jetstream);
              ("event", event);
            ]
        in
        Store.enqueue store ~source ~cursor:(Int64.to_string cursor) ~key:id
          ~value:(encode pending);
        Health.event health source
          ~now:(Eio.Time.now system#clock)
          ~at:(Int64.to_float timestamp /. if jetstream then 1e6 else 1e9))
  do
    ()
  done

let push engine source event key =
  if get "nsid" event = "sh.tangled.git.refUpdate" then
    let position = timestamp event "created" in
    let event = required "event" event in
    let repo = did (get "repo" event) in
    match Catalog.managed engine.Engine.catalog repo with
    | Some canonical when canonical.knot = source ->
        let options = strings "pushOptions" event in
        let commit = sha (get "newSha" event) in
        let ref_ = get "ref" event in
        (if
           commit <> String.make 40 '0'
           && not
                (List.exists
                   (fun option -> List.mem option [ "skip-ci"; "ci-skip" ])
                   options)
         then
           let trigger =
             obj
               [
                 ("$type", str "sh.tangled.ci.trigger#push");
                 ("ref", str (get "ref" event));
                 ("newSha", str commit);
                 ("oldSha", str (sha (get "oldSha" event)));
               ]
           in
           let default_ref =
             match field "meta" event with
             | Some meta -> (
                 match field "isDefaultRef" meta with
                 | Some (Jsont.Bool (value, _)) -> value
                 | _ -> false)
             | None -> false
           in
           ignore
             (Engine.create engine
                ~dedup:(Recovery.dedup ~repo ~ref_ ~sha:commit)
                ~automatic:true
                ~changed_files:(strings "changedFiles" event)
                ~default_ref
                ~actor:(did (get "committerDid" event))
                (obj
                   [
                     ("repo", str repo);
                     ("trigger", trigger);
                     ("eventKey", str key);
                   ])));
        Store.checkpoint_ref engine.store ~repo ~ref_ ~sha:commit ~position
    | _ -> ()

let pull engine network owner rkey record =
  match field "source" record with
  | None -> ()
  | Some source when field "repo" source <> None -> ()
  | Some source ->
      let target = required "target" record in
      let repo = did (get "repo" target) in
      if Catalog.managed engine.Engine.catalog repo <> None then
        let revision = Patch.pull network engine.runner.system owner record in
        let uri = "at://" ^ owner ^ "/sh.tangled.repo.pull/" ^ rkey in
        let trigger =
          obj
            [
              ("$type", str "sh.tangled.ci.trigger#pullRequest");
              ("sourceSha", str revision);
              ("sourceBranch", str (get "branch" source));
              ("targetBranch", str (get "branch" target));
              ("pull", str uri);
            ]
        in
        ignore
          (Engine.create engine
             ~dedup:(uri ^ "/" ^ revision)
             ~automatic:true ~actor:owner
             (obj [ ("repo", str repo); ("trigger", trigger) ]))

let commit engine network event =
  if get "kind" event = "commit" then
    let owner = did (get "did" event) in
    let commit = required "commit" event in
    let collection = get "collection" commit in
    let rkey = get "rkey" commit in
    let record =
      match get "operation" commit with
      | "delete" -> None
      | "create" | "update" -> Some (required "record" commit)
      | _ -> invalid "unknown commit operation"
    in
    if collection = "sh.tangled.repo.pull" then
      Option.iter (pull engine network owner rkey) record
    else Catalog.notice engine.Engine.catalog ~owner ~collection ~rkey

let recover_once ~engine ~network =
  let store = engine.Engine.store and system = engine.runner.system in
  let now = Eio.Time.now system#clock in
  List.iter
    (fun (source, value) ->
      Recovery.schedule engine ~source;
      ignore
        (Store.complete store "recover-source" source ~value ~puts:[]
           ~deletes:[]))
    (Store.ready store "recover-source" ~now ~limit:16);
  let tasks namespace f =
    Eio.Fiber.List.iter ~max_fibers:2
      (fun (key, value) ->
        try
          Eio.Time.with_timeout_exn system#clock 30. (fun () -> f key);
          ignore
            (Store.complete store namespace key ~value ~puts:[] ~deletes:[])
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            Store.defer store namespace key ~now:(Eio.Time.now system#clock);
            report ("recovery " ^ key) exn)
      (Store.ready store namespace ~now ~limit:16)
  in
  Eio.Fiber.both
    (fun () -> tasks "recover" (Recovery.repo engine))
    (fun () ->
      tasks "recover-pulls" (fun owner ->
          if Catalog.member engine.catalog owner then
            Network.records network owner "sh.tangled.repo.pull"
            |> List.iter (fun item ->
                if not (Catalog.member engine.catalog owner) then
                  raise Catalog.Pending;
                let uri = Atp.At_uri.of_string_exn (get "uri" item) in
                if
                  Atp.At_uri.authority uri <> owner
                  || Atp.At_uri.collection uri <> Some "sh.tangled.repo.pull"
                then
                  invalid "PDS returned a pull outside the requested collection";
                let rkey =
                  match Atp.At_uri.rkey uri with
                  | Some value -> value
                  | None -> invalid "missing pull key"
                in
                let uri = Atp.At_uri.to_string uri in
                let cid = get "cid" item in
                if Store.get store "pull-seen" uri = Some cid then
                  Store.put store "pull-seen" uri cid
                else
                  let now = Eio.Time.now system#clock in
                  let event =
                    obj
                      [
                        ("kind", str "commit");
                        ("did", str owner);
                        ("time_us", Jsont.Json.number (now *. 1e6));
                        ( "commit",
                          obj
                            [
                              ("collection", str "sh.tangled.repo.pull");
                              ("rkey", str rkey);
                              ("operation", str "update");
                              ("record", required "value" item);
                            ] );
                      ]
                  in
                  Store.enqueue store ~source:"recovery"
                    ~cursor:(Int64.to_string (Int64.of_float (now *. 1e6)))
                    ~key:("pull-recovery/" ^ uri ^ "/" ^ cid)
                    ~value:
                      (encode
                         (obj
                            [
                              ("source", str "jetstream");
                              ("jetstream", bool true);
                              ("event", event);
                            ]));
                  Store.put store "pull-seen" uri cid)));
  Recovery.settled store ~now:(Eio.Time.now system#clock)

let run ~engine ~network ~jetstream ~health ~policy =
  let store = engine.Engine.store and system = engine.runner.system in
  let rec retry source f =
    Health.starting health source ~now:(Eio.Time.now system#clock);
    (try
       f ();
       Health.failed health source
         ~now:(Eio.Time.now system#clock)
         (Failure "event stream closed")
     with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
        Health.failed health source ~now:(Eio.Time.now system#clock) exn;
        (match exn with
        | Httpz_websocket_eio.Upgrade_rejected 410 ->
            let now = Eio.Time.now system#clock in
            Recovery.gap store ~source ~now
              ~cursor:
                (Option.value ~default:"0" (Store.get store "cursor" source))
              ~reason:"upstream refused replay with HTTP 410";
            Store.put store "cursor" source
              (Int64.to_string
                 (Int64.of_float
                    (now *. if source = "jetstream" then 1e6 else 1e9)))
        | _ -> ());
        report source exn);
    Eio.Time.sleep system#clock 2.;
    retry source f
  in
  let initial_ns = Int64.of_float (Eio.Time.now system#clock *. 1e9) in
  let initial = Int64.of_float (Eio.Time.now system#clock *. 1e6) in
  if Store.get store "cursor" "jetstream" = None then
    Store.put store "cursor" "jetstream" (Int64.to_string initial);
  Catalog.bootstrap engine.catalog;
  Recovery.seed engine;
  Health.require health "jetstream";
  Store.schedule store "recover-source" "jetstream";
  let subscriptions = Hashtbl.create 8 in
  let reconcile () =
    while true do
      let wanted = Catalog.knots engine.catalog in
      Hashtbl.filter_map_inplace
        (fun knot cancel ->
          if List.mem knot wanted then Some cancel
          else (
            cancel ();
            Health.remove health knot;
            None))
        subscriptions;
      List.iter
        (fun knot ->
          if not (Hashtbl.mem subscriptions knot) then (
            if Store.get store "cursor" knot = None then
              Store.put store "cursor" knot (Int64.to_string initial_ns);
            let cancel, promise = Eio.Promise.create () in
            Hashtbl.add subscriptions knot (fun () ->
                Eio.Promise.resolve promise ());
            Eio.Fiber.fork ~sw:engine.sw (fun () ->
                Eio.Fiber.first
                  (fun () -> Eio.Promise.await cancel)
                  (fun () ->
                    retry knot (fun () ->
                        connect ~system ~store ~source:knot ~health ~policy
                          ~url:(knot ^ "/events") ~jetstream:false)))))
        wanted;
      Eio.Time.sleep system#clock 1.
    done
  in
  let refresh () =
    while true do
      Eio.Fiber.List.iter ~max_fibers:4
        (fun (key, value) ->
          try
            Eio.Time.with_timeout_exn system#clock 30. (fun () ->
                Catalog.refresh engine.catalog key ~value);
            Store.schedule store "recover-source" "jetstream"
          with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
              Store.defer store "reconcile" key ~now:(Eio.Time.now system#clock);
              report key exn)
        (Store.ready store "reconcile"
           ~now:(Eio.Time.now system#clock)
           ~limit:16);
      Eio.Time.sleep system#clock 0.5
    done
  in
  let work () =
    while true do
      Eio.Fiber.List.iter ~max_fibers:4
        (fun (key, raw) ->
          try
            let value = decode raw in
            let event = required "event" value in
            Eio.Time.with_timeout_exn system#clock 30. (fun () ->
                match required "jetstream" value with
                | Jsont.Bool (true, _) -> commit engine network event
                | _ -> push engine (get "source" value) event key);
            Store.batch store
              ~puts:[ ("done", key, "") ]
              ~deletes:[ ("inbox", key) ]
          with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | Invalid message ->
              Store.batch store
                ~puts:[ ("rejected", key, message); ("done", key, "") ]
                ~deletes:[ ("inbox", key) ];
              report key (Invalid message)
          | exn ->
              Store.defer store "inbox" key ~now:(Eio.Time.now system#clock);
              report key exn)
        (Store.ready store "inbox" ~now:(Eio.Time.now system#clock) ~limit:64);
      Eio.Time.sleep system#clock 0.5
    done
  in
  let recover () =
    let next_scan =
      ref
        (Eio.Time.now system#clock
        +. float_of_int policy.Operations.reconcile_seconds)
    in
    while true do
      let now = Eio.Time.now system#clock in
      if now >= !next_scan then (
        Catalog.bootstrap engine.catalog;
        Store.schedule store "recover-source" "jetstream";
        next_scan := now +. float_of_int policy.reconcile_seconds);
      recover_once ~engine ~network;
      Eio.Time.sleep system#clock 2.
    done
  in
  let url =
    Httpz_uri.of_string_exn jetstream |> fun uri ->
    List.fold_left
      (fun uri collection ->
        Httpz_uri.add_query_param uri ~key:"wantedCollections" ~value:collection)
      uri collections
    |> Httpz_uri.to_string
  in
  Eio.Fiber.all
    [
      reconcile;
      refresh;
      work;
      recover;
      (fun () ->
        retry "jetstream" (fun () ->
            connect ~system ~store ~source:"jetstream" ~url ~jetstream:true
              ~health ~policy));
    ]
