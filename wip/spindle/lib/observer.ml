(* SPDX-License-Identifier: ISC *)
open Json

let manifest = "org.tangled.repo.manifest"
let patch = "org.tangled.review.patch"
let ticket = "org.tangled.track.ticket"
let collections = [ manifest; patch; ticket ]

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

(* What is known of a pull request between events: its newest head, its
   author, its number of revisions and the state last seen. Older stores
   hold only the head. *)
type seen = { head : string; author : string; rounds : int; state : string }

let load_seen engine uri =
  Option.map
    (fun raw ->
      match decode raw with
      | Jsont.Object _ as v ->
          {
            head = get "head" v;
            author = get "author" v;
            rounds = int_of_float (number (required "rounds" v));
            state = get "state" v;
          }
      | _ | (exception Invalid _) ->
          { head = raw; author = ""; rounds = 1; state = "open" })
    (Store.get engine.Engine.store "pull-seen" uri)

let save_seen engine uri s =
  Store.put engine.Engine.store "pull-seen" uri
    (encode
       (obj
          [
            ("head", str s.head);
            ("author", str s.author);
            ("rounds", int s.rounds);
            ("state", str s.state);
          ]))

let text field_ value =
  match Option.bind (field field_ value) (field "text") with
  | Some t -> string t
  | None -> ""

(* The [pull_request] object of a webhook payload, as the reference spindle
   writes it. *)
let pull_view ~ticket:t ~seen ~source =
  obj
    ([
       ("number", int 0);
       ("title", str (text "title" t));
       ("body", str (text "body" t));
       ( "state",
         str
           (match seen.state with
           | "completed" -> "merged"
           | "closed" -> "closed"
           | _ -> "open") );
       ("target_branch", required "targetBranch" t);
       ("round_number", int seen.rounds);
       ("owner", obj [ ("did", str seen.author) ]);
       ("created_at", Option.value ~default:(str "") (field "createdAt" t));
     ]
    @
    match source with
    | None -> [ ("source", obj [ ("branch", str ""); ("sha", str seen.head) ]) ]
    | Some s ->
        [
          ( "source",
            obj
              [
                ("branch", required "branch" s);
                ("repo", required "repo" s);
                ("sha", str seen.head);
              ] );
        ])

(* Dispatch a pull request event and notify its webhooks. A closed or merged
   pull request runs only workflows that ask for it. *)
let dispatch engine hooks (served : Catalog.repo) ~uri ~ticket:t ~seen ~source
    ~action ~event ~sender =
  if
    Catalog.admitted engine.Engine.catalog served.owner
    && not
         (Settings.banned engine.settings seen.author
         || Settings.banned engine.settings sender)
  then begin
    let dedup =
      String.concat "/"
        ([ "pull"; uri; seen.head ]
        @ if List.mem action [ "opened"; "synchronize" ] then [] else [ action ])
    in
    (* A retried event notifies webhooks once. *)
    if Engine.first engine "pull-hook" dedup then
      Hooks.pull_request hooks served ~event ~sender
        (pull_view ~ticket:t ~seen ~source);
    let trigger =
      obj
        ([
           ("$type", str Engine.pull_request);
           ("action", str action);
           ("pull", str uri);
           ("sourceSha", str seen.head);
           ("targetBranch", required "targetBranch" t);
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
    (* A pull request from someone without push access waits for a
       collaborator's approval, as does a change by such a sender. *)
    let blocked =
      not
        (Catalog.authorized engine.catalog served seen.author
        && Catalog.authorized engine.catalog served sender)
    in
    ignore
      (Engine.create engine ~dedup ~blocked ~automatic:true ~actor:sender
         (obj [ ("repo", str served.did); ("trigger", trigger) ]))
  end

(* An event only says that a patch or ticket changed. Its contents, its
   ticket and its author are read from the knot, so a forged event can start
   nothing the knot does not hold. *)
let read network (served : Catalog.repo) collection rkey =
  Option.map (required "value")
    (Network.record network ~service:served.knot ~did:served.did ~collection
       ~rkey)

let pull engine network hooks ~repo ~rkey =
  match Catalog.managed engine.Engine.catalog repo with
  | None -> ()
  | Some served -> (
      match read network served patch rkey with
      | None -> ()
      | Some value -> (
          let number = get "ticket" value in
          match read network served ticket number with
          | None -> ()
          | Some t -> (
              match field "targetBranch" t with
              | None -> ()
              | Some _ ->
                  let state =
                    Option.fold ~none:"open" ~some:string (field "state" t)
                  in
                  if state = "open" then begin
                    let author = did (get "x-tngl-editor" value) in
                    let head = sha (get "oid" (required "head" value)) in
                    let uri = "at://" ^ repo ^ "/" ^ ticket ^ "/" ^ number in
                    let previous = load_seen engine uri in
                    if Option.map (fun s -> s.head) previous <> Some head
                    then begin
                      let seen =
                        {
                          head;
                          author =
                            (match previous with
                            | Some { author = first; _ } when first <> "" ->
                                first
                            | _ -> author);
                          rounds =
                            1
                            + Option.fold ~none:0
                                ~some:(fun s -> s.rounds)
                                previous;
                          state;
                        }
                      in
                      let action, event =
                        if previous = None then
                          ("opened", "pull_request:created")
                        else ("synchronize", "pull_request:resubmitted")
                      in
                      dispatch engine hooks served ~uri ~ticket:t ~seen
                        ~source:(field "source" value) ~action ~event
                        ~sender:author;
                      save_seen engine uri seen
                    end
                  end)))

(* A pull request's state changes when its ticket is closed, reopened or
   completed, which is how a merge is recorded. A ticket seen before its first
   patch starts nothing, as in the reference spindle. *)
let lifecycle engine network hooks ~repo ~rkey =
  match Catalog.managed engine.Engine.catalog repo with
  | None -> ()
  | Some served -> (
      let uri = "at://" ^ repo ^ "/" ^ ticket ^ "/" ^ rkey in
      match load_seen engine uri with
      | None -> ()
      | Some seen -> (
          match read network served ticket rkey with
          | None -> ()
          | Some t ->
              let state =
                Option.fold ~none:"open" ~some:string (field "state" t)
              in
              if
                state <> seen.state
                && List.mem state [ "open"; "closed"; "completed" ]
              then begin
                let seen = { seen with state } in
                let action, event =
                  match state with
                  | "completed" -> ("merged", "pull_request:merged")
                  | "closed" -> ("closed", "pull_request:closed")
                  | _ -> ("reopened", "pull_request:reopened")
                in
                let sender =
                  Option.fold ~none:seen.author ~some:string
                    (field "x-tngl-editor" t)
                in
                (* The newest patch says where the change comes from. *)
                let source =
                  Option.bind
                    (List.find_opt
                       (fun p ->
                         get "ticket" (required "value" p) = rkey
                         && Option.map
                              (fun h -> get "oid" h)
                              (field "head" (required "value" p))
                            = Some seen.head)
                       (Network.records_at network ~service:served.knot
                          ~did:repo patch))
                    (fun p -> field "source" (required "value" p))
                in
                dispatch engine hooks served ~uri ~ticket:t ~seen ~source
                  ~action ~event ~sender;
                save_seen engine uri seen
              end))

let commit engine network hooks event =
  if get "kind" event = "commit" then
    let repo = did (get "did" event) in
    let commit = required "commit" event in
    let collection = get "collection" commit in
    let operation = get "operation" commit in
    if collection = manifest then Catalog.forget engine.Engine.catalog repo
    else if collection = patch && operation = "create" then
      pull engine network hooks ~repo ~rkey:(get "rkey" commit)
    else if collection = ticket && operation <> "delete" then
      lifecycle engine network hooks ~repo ~rkey:(get "rkey" commit)

let run ~engine ~network ~hooks ~jetstream ~health ~policy =
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
                commit engine network hooks (decode raw));
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
