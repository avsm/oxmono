(* SPDX-License-Identifier: ISC *)
open Json

let manifest = "org.tangled.repo.manifest"
let patch = "org.tangled.review.patch"
let ticket = "org.tangled.track.ticket"
let collections = [ manifest; patch ]

let report source exn =
  Printf.eprintf "spindle observer %s: %s\n%!" source (Printexc.to_string exn)

let timestamp event =
  let n = number (required "time_us" event) in
  if n < 0. || n >= Int64.to_float Int64.max_int || Float.floor n <> n then
    invalid "invalid event timestamp";
  Int64.of_float n

(* A cursor older than the replay window would be refused or silently
   truncated upstream. Start from now, and say so. *)
let cursor store (policy : Operations.t) ~now =
  let current = Int64.of_float (now *. 1e6) in
  match Store.get store "cursor" "jetstream" with
  | Some value
    when Int64.to_float (Int64.of_string value) /. 1e6
         >= now -. float_of_int (policy.replay_hours * 3600) ->
      value
  | previous ->
      Option.iter
        (fun _ ->
          Printf.eprintf
            "spindle observer: cursor is older than the replay window\n%!")
        previous;
      let value = Int64.to_string current in
      Store.put store "cursor" "jetstream" value;
      value

let connect ~system ~store ~url ~health ~policy =
  let source = "jetstream" in
  let cursor = cursor store policy ~now:(Eio.Time.now system#clock) in
  let url =
    Httpz_uri.of_string_exn url |> fun uri ->
    Httpz_uri.remove_query_param uri "cursor" |> fun uri ->
    Httpz_uri.add_query_param uri ~key:"cursor" ~value:cursor
    |> Httpz_uri.to_string
  in
  Httpz_websocket_eio.with_connection
    ~on_activity:(fun () ->
      Health.activity health source ~now:(Eio.Time.now system#clock))
    system url
  @@ fun socket ->
  Health.connected health source ~now:(Eio.Time.now system#clock);
  while
    Httpz_websocket.receive socket ~f:(fun kind bytes ~off ~len ->
        if kind <> Httpz_websocket.Text then invalid "event must be JSON text";
        let event = decode (Bytes.sub_string bytes off len) in
        let timestamp = timestamp event in
        let id =
          String.concat "/"
            ([ source; Int64.to_string timestamp; get "did" event ]
            @
            match field "commit" event with
            | None -> [ get "kind" event ]
            | Some commit ->
                [
                  get "collection" commit;
                  get "rkey" commit;
                  get "operation" commit;
                ])
        in
        Store.enqueue store ~source ~cursor:(Int64.to_string timestamp) ~key:id
          ~value:(encode event);
        Health.event health source
          ~now:(Eio.Time.now system#clock)
          ~at:(Int64.to_float timestamp /. 1e6))
  do
    ()
  done

(* An event only says that a patch exists. Its contents, its ticket and its
   author are read from the knot, so a forged event can start nothing the
   knot does not hold. *)
let pull engine network ~repo ~rkey =
  match Catalog.managed engine.Engine.catalog repo with
  | None -> ()
  | Some served -> (
      let read collection rkey =
        Option.map (required "value")
          (Network.record network ~service:served.knot ~did:repo ~collection
             ~rkey)
      in
      match read patch rkey with
      | None -> ()
      | Some value -> (
          let number = get "ticket" value in
          match read ticket number with
          | None -> ()
          | Some t -> (
              match field "targetBranch" t with
              | None -> ()
              | Some target ->
                  let state = Option.map string (field "state" t) in
                  if state = Some "open" then begin
                    let author = did (get "x-tngl-editor" value) in
                    let head = sha (get "oid" (required "head" value)) in
                    let uri = "at://" ^ repo ^ "/" ^ ticket ^ "/" ^ number in
                    let source = field "source" value in
                    let action =
                      if Store.get engine.store "pull-seen" uri <> None then
                        "synchronize"
                      else "opened"
                    in
                    let trigger =
                      obj
                        ([
                           ("$type", str Engine.pull_request);
                           ("action", str action);
                           ("pull", str uri);
                           ("sourceSha", str head);
                           ("targetBranch", target);
                         ]
                        @
                        match source with
                        | None -> []
                        | Some s ->
                            [
                              ("sourceRepo", str (did (get "repo" s)));
                              ("sourceBranch", str (get "branch" s));
                            ])
                    in
                    (* A pull request from someone without push access waits
                       for a collaborator's approval. *)
                    let blocked =
                      not (Catalog.authorized engine.catalog served author)
                    in
                    ignore
                      (Engine.create engine
                         ~dedup:("pull/" ^ uri ^ "/" ^ head)
                         ~blocked ~automatic:true ~actor:author
                         (obj [ ("repo", str repo); ("trigger", trigger) ]));
                    Store.put engine.store "pull-seen" uri head
                  end)))

let commit engine network event =
  if get "kind" event = "commit" then
    let repo = did (get "did" event) in
    let commit = required "commit" event in
    let collection = get "collection" commit in
    let operation = get "operation" commit in
    if collection = manifest then Catalog.forget engine.Engine.catalog repo
    else if collection = patch && operation = "create" then
      pull engine network ~repo ~rkey:(get "rkey" commit)

let run ~engine ~network ~jetstream ~health ~policy =
  let store = engine.Engine.store and system = engine.runner.system in
  let rec retry f =
    Health.starting health "jetstream" ~now:(Eio.Time.now system#clock);
    (try
       f ();
       Health.failed health "jetstream"
         ~now:(Eio.Time.now system#clock)
         (Failure "event stream closed")
     with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
        Health.failed health "jetstream" ~now:(Eio.Time.now system#clock) exn;
        report "jetstream" exn);
    Eio.Time.sleep system#clock 2.;
    retry f
  in
  Health.require health "jetstream";
  let work () =
    while true do
      Eio.Fiber.List.iter ~max_fibers:4
        (fun (key, raw) ->
          try
            Eio.Time.with_timeout_exn system#clock 30. (fun () ->
                commit engine network (decode raw));
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
  Eio.Fiber.both work (fun () ->
      retry (fun () -> connect ~system ~store ~url ~health ~policy))
