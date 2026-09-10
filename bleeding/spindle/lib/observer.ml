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

let connect ~system ~store ~source ~url ~jetstream =
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
    Httpz_uri.add_query_param
      (Httpz_uri.remove_query_param uri "cursor")
      ~key:"cursor" ~value:cursor
    |> Httpz_uri.to_string
  in
  Httpz_websocket_eio.with_connection system url @@ fun socket ->
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
          ~value:(encode pending))
  do
    ()
  done

let push engine source event key =
  if get "nsid" event = "sh.tangled.git.refUpdate" then
    let event = required "event" event in
    let repo = did (get "repo" event) in
    match Catalog.managed engine.Engine.catalog repo with
    | Some canonical when canonical.knot = source ->
        let options = strings "pushOptions" event in
        let commit = sha (get "newSha" event) in
        if
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
            (Engine.create engine ~dedup:key ~automatic:true
               ~changed_files:(strings "changedFiles" event)
               ~default_ref
               ~actor:(did (get "committerDid" event))
               (obj [ ("repo", str repo); ("trigger", trigger) ]))
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

let run ~engine ~network ~jetstream =
  let store = engine.Engine.store and system = engine.runner.system in
  let rec retry source f =
    (try f () with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> report source exn);
    Eio.Time.sleep system#clock 2.;
    retry source f
  in
  let initial_ns = Int64.of_float (Eio.Time.now system#clock *. 1e9) in
  let initial = Int64.of_float (Eio.Time.now system#clock *. 1e6) in
  if Store.get store "cursor" "jetstream" = None then
    Store.put store "cursor" "jetstream" (Int64.to_string initial);
  Catalog.bootstrap engine.catalog;
  let subscriptions = Hashtbl.create 8 in
  let reconcile () =
    while true do
      let wanted = Catalog.knots engine.catalog in
      Hashtbl.filter_map_inplace
        (fun knot cancel ->
          if List.mem knot wanted then Some cancel
          else (
            cancel ();
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
                        connect ~system ~store ~source:knot
                          ~url:(knot ^ "/events") ~jetstream:false)))))
        wanted;
      Eio.Time.sleep system#clock 1.
    done
  in
  let refresh () =
    while true do
      List.iter
        (fun (key, value) ->
          try
            Eio.Time.with_timeout_exn system#clock 30. (fun () ->
                Catalog.refresh engine.catalog key ~value)
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
      (fun () ->
        retry "jetstream" (fun () ->
            connect ~system ~store ~source:"jetstream" ~url ~jetstream:true));
    ]
