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

(* Jetstream sends identity and account events for the whole network,
   whatever the wanted collections. Only an owner whose account went inactive
   concerns this spindle. *)
(* A relay also marks an account inactive while it is throttled or out of
   sync, which passes. *)
let lasting = function
  | Some ("deleted" | "deactivated" | "takendown" | "suspended") -> true
  | _ -> false

let wanted settings event =
  try
    match get "kind" event with
    | "commit" ->
        List.mem (get "collection" (required "commit" event)) collections
    | "account" ->
        let account = required "account" event in
        field "active" account = Some (Jsont.Json.bool false)
        && lasting (Option.map string (field "status" account))
        && Settings.repos_of settings (did (get "did" event)) <> []
    | _ -> false
  with Invalid _ -> false

let connect ~system ~store ~settings ~url ~health ~policy =
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
        (* A frame that is not an event with a time cannot move the cursor,
           so it is reported and passed over, and the next event moves it. *)
        match
          if kind <> Httpz_websocket.Text then invalid "event must be JSON text";
          let event = decode (Bytes.sub_string bytes off len) in
          (event, timestamp event)
        with
        | exception Invalid m -> report source (Invalid m)
        | event, timestamp ->
            let cursor = Int64.to_string timestamp in
            (* An event this spindle cannot name or does not want is passed over,
           so that it cannot stall the stream. *)
            let id =
              try
                if not (wanted settings event) then None
                else
                  Some
                    (String.concat "/"
                       ([ source; cursor; get "did" event ]
                       @
                       match field "commit" event with
                       | None -> [ get "kind" event ]
                       | Some commit ->
                           [
                             get "collection" commit;
                             get "rkey" commit;
                             get "operation" commit;
                           ]))
              with Invalid _ -> None
            in
            (match id with
            | None -> Store.advance store ~source ~cursor
            | Some id ->
                Store.enqueue store ~source ~cursor ~key:id
                  ~value:(encode event));
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
            rounds = integer (required "rounds" v);
            state = get "state" v;
          }
      | _ | (exception Invalid _) ->
          { head = raw; author = ""; rounds = 1; state = "open" })
    (Store.get (Engine.store engine) "pull-seen" uri)

let save_seen engine uri s =
  Store.put (Engine.store engine) "pull-seen" uri
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
let dispatch ?round engine hooks (served : Catalog.repo) ~uri ~ticket:t ~seen
    ~source ~action ~event ~sender =
  if
    Catalog.admitted (Engine.catalog engine) served.owner
    && not
         (Settings.banned (Engine.settings engine) seen.author
         || Settings.banned (Engine.settings engine) sender)
  then begin
    let dedup =
      String.concat "/"
        ([ "pull"; uri; seen.head ]
        @ if List.mem action [ "opened"; "synchronize" ] then [] else [ action ]
        )
    in
    (* A retried event notifies webhooks once. Each round of a pull request
       does, even one resubmitting the same commit. *)
    if
      Engine.first engine "pull-hook"
        (String.concat "/" (dedup :: Option.to_list round))
    then
      Hooks.pull_request hooks served ~event ~sender
        (pull_view ~ticket:t ~seen ~source);
    (* The target repository's knot holds a pull request's commits, so the
       pipeline runs from it, as the reference spindle runs it. A fork's
       branch names nothing in the target, so it is left out. *)
    let fork =
      match source with
      | Some s -> did (get "repo" s) <> served.did
      | None -> false
    in
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
        | Some s when not fork -> [ ("sourceBranch", str (get "branch" s)) ]
        | _ -> [])
    in
    (* A pull request from someone without push access waits for a
       collaborator's approval, as does a change by such a sender. *)
    let blocked =
      not
        (Catalog.authorized (Engine.catalog engine) served seen.author
        && Catalog.authorized (Engine.catalog engine) served sender)
    in
    ignore
      (Engine.create engine ~dedup ~blocked ~untrusted:fork ~automatic:true
         ~actor:sender
         (obj [ ("repo", str served.did); ("trigger", trigger) ]))
  end

(* An event only says that a patch or ticket changed. Its contents, its
   ticket and its author are read from the knot, so a forged event can start
   nothing the knot does not hold. *)
let read network (served : Catalog.repo) collection rkey =
  Option.map (required "value")
    (Network.record network ~service:served.knot ~did:served.did ~collection
       ~rkey)

(* Patches and tickets arrive for every repository on the network. Only one
   this spindle has accepted is looked up, so the rest cost no requests. *)
let pull engine network hooks ~repo ~rkey =
  match
    if Catalog.known (Engine.catalog engine) repo then
      Catalog.managed (Engine.catalog engine) repo
    else None
  with
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
                    (* A patch without a valid editor has no known author,
                       so its pipeline waits for approval. *)
                    let author =
                      match field "x-tngl-editor" value with
                      | Some (Jsont.String (editor, _))
                        when Atp.Did.is_valid editor ->
                          editor
                      | _ -> ""
                    in
                    let head = sha (get "oid" (required "head" value)) in
                    let uri = "at://" ^ repo ^ "/" ^ ticket ^ "/" ^ number in
                    let previous = load_seen engine uri in
                    (* Each patch record is a round, as the reference spindle
                       counts them, whatever its head. *)
                    let round = uri ^ "/" ^ rkey in
                    if Store.get (Engine.store engine) "pull-patch" round = None
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
                      dispatch ~round:rkey engine hooks served ~uri ~ticket:t
                        ~seen ~source:(field "source" value) ~action ~event
                        ~sender:author;
                      save_seen engine uri seen;
                      Store.put (Engine.store engine) "pull-patch" round ""
                    end
                  end)))

(* A pull request's state changes when its ticket is closed, reopened or
   completed, which is how a merge is recorded. A ticket seen before its first
   patch starts nothing, as in the reference spindle. *)
let lifecycle engine network hooks ~repo ~rkey ~round =
  let uri = "at://" ^ repo ^ "/" ^ ticket ^ "/" ^ rkey in
  match load_seen engine uri with
  | None -> ()
  | Some seen -> (
      match Catalog.managed (Engine.catalog engine) repo with
      | None -> ()
      | Some served -> (
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
                dispatch ~round engine hooks served ~uri ~ticket:t ~seen ~source
                  ~action ~event ~sender;
                save_seen engine uri seen
              end))

(* A manifest naming this spindle registers its repository at once, so that
   its pull requests are followed. A deleted repository's settings and
   history are wiped, as the reference spindle wipes them. *)
let assignment engine ~wipe ~repo ~operation commit =
  let catalog = Engine.catalog engine in
  Catalog.forget catalog repo;
  if operation = "delete" then (
    if Catalog.known catalog repo && Catalog.deleted catalog repo then wipe repo)
  else
    match field "record" commit with
    | Some record when Catalog.names_self catalog record ->
        ignore (Catalog.managed catalog repo)
    | _ -> ()

(* An owner whose account is deactivated, suspended or deleted loses what is
   kept for their repositories. *)
let account engine network ~wipe event =
  let owner = did (get "did" event) in
  (* The event is not trusted, so the account's PDS must say so too. *)
  let inactive =
    let pds = Network.pds network owner in
    let status =
      Network.json network
        (Network.query
           (pds ^ "/xrpc/com.atproto.sync.getRepoStatus")
           [ ("did", owner) ])
    in
    field "active" status = Some (Jsont.Json.bool false)
    || List.mem
         (Option.fold ~none:"" ~some:string (field "error" status))
         [ "RepoNotFound"; "RepoDeactivated"; "RepoTakendown"; "RepoSuspended" ]
  in
  if inactive then (
    Printf.eprintf
      "spindle observer: account %s is inactive, wiping its repos\n%!" owner;
    List.iter wipe (Settings.repos_of (Engine.settings engine) owner))

let handle engine network hooks ~wipe event =
  match get "kind" event with
  | "commit" ->
      let repo = did (get "did" event) in
      let commit = required "commit" event in
      let collection = get "collection" commit in
      let operation = get "operation" commit in
      if collection = manifest then
        assignment engine ~wipe ~repo ~operation commit
      else if collection = patch && operation = "create" then
        pull engine network hooks ~repo ~rkey:(get "rkey" commit)
      else if collection = ticket then
        if operation = "delete" then
          (* Only a ticket the knot no longer holds is forgotten. *)
          let rkey = get "rkey" commit in
          match
            if Catalog.known (Engine.catalog engine) repo then
              Catalog.managed (Engine.catalog engine) repo
            else None
          with
          | Some served when read network served ticket rkey = None ->
              Store.delete (Engine.store engine) "pull-seen"
                ("at://" ^ repo ^ "/" ^ ticket ^ "/" ^ rkey)
          | _ -> ()
        else
          (* Each change of the ticket is its own round, so a second close
             notifies as the first did. *)
          lifecycle engine network hooks ~repo ~rkey:(get "rkey" commit)
            ~round:
              (match field "cid" commit with
              | Some (Jsont.String (cid, _)) -> cid
              | _ -> get "rev" commit)
  | "account" -> account engine network ~wipe event
  | _ -> ()

(* A failing event is retried, its backoff doubling to an hour, and given up
   after this many attempts, about five hours. *)
let max_attempts = 16

(* An inbox key is the source, the event's time and its DID, then more. *)
let repo_of_key key =
  match String.split_on_char '/' key with
  | _ :: _ :: did :: _ -> did
  | _ -> "\000" ^ key

(* Events of one repository are handled in order, since a pull request's
   state carries from one to the next. Repositories proceed in parallel. *)
let groups ready =
  let tagged =
    List.map (fun ((key, _) as item) -> (repo_of_key key, item)) ready
    |> List.stable_sort (fun (_, (a, _)) (_, (b, _)) -> String.compare a b)
  in
  let repos =
    List.fold_left
      (fun seen (r, _) -> if List.mem r seen then seen else r :: seen)
      [] tagged
  in
  List.rev_map
    (fun r ->
      List.filter_map (fun (r', i) -> if r = r' then Some i else None) tagged)
    repos

let run ~engine ~network ~hooks ~wipe ~jetstream ~health ~policy =
  let store = Engine.store engine and system = Engine.system engine in
  (* Reconnecting waits 2 seconds, doubling to a minute while connections
     fail, and 2 again after one that lasted. *)
  let rec retry ?(delay = 2.) f =
    Health.starting health "jetstream" ~now:(Eio.Time.now system#clock);
    let began = Eio.Time.Mono.now system#mono_clock in
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
    let lasted =
      Mtime.Span.to_float_ns
        (Mtime.span began (Eio.Time.Mono.now system#mono_clock))
      /. 1e9
    in
    let delay = if lasted > 60. then 2. else delay in
    Eio.Time.sleep system#clock delay;
    retry ~delay:(Float.min 60. (delay *. 2.)) f
  in
  Health.require health "jetstream";
  (* A failing pass, such as a store error, is reported and the next one
     tries again, rather than stopping the spindle. *)
  let work () =
    while true do
      try
        let reject key message =
          Store.batch store
            ~puts:[ ("rejected", key, message); ("done", key, "") ]
            ~deletes:[ ("inbox", key) ]
        in
        (* [one key raw] is whether the event is settled. A failure leaves it, and
         the later events of its repository, for another attempt. *)
        let one (key, raw) =
          try
            Eio.Time.with_timeout_exn system#clock 30. (fun () ->
                handle engine network hooks ~wipe (decode raw));
            Store.batch store
              ~puts:[ ("done", key, "") ]
              ~deletes:[ ("inbox", key) ];
            true
          with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | Invalid message ->
              reject key message;
              report key (Invalid message);
              true
          | Xrpc_error.E (status, code, message)
            when Httpz.Res.status_code status < 500 ->
              reject key (code ^ ": " ^ message);
              true
          | Engine.Capacity ->
              (* A full queue is no fault of the event. *)
              Store.delay store "inbox" key
                ~until:(Eio.Time.now system#clock +. 5.);
              false
          | exn ->
              report key exn;
              if
                Store.defer store "inbox" key ~now:(Eio.Time.now system#clock)
                >= max_attempts
              then (
                reject key ("gave up: " ^ Printexc.to_string exn);
                true)
              else false
        in
        let now = Eio.Time.now system#clock in
        (* An event waits behind an earlier one of its repository that is
         waiting to be retried, whichever batch it arrives in. *)
        let waiting =
          List.fold_left
            (fun acc key ->
              let repo = repo_of_key key in
              if List.mem_assoc repo acc then acc else (repo, key) :: acc)
            []
            (Store.waiting store "inbox" ~now)
        in
        Eio.Fiber.List.iter ~max_fibers:4
          (fun items ->
            ignore
              (List.fold_left
                 (fun failed ((key, _) as item) ->
                   let behind =
                     match failed with
                     | Some _ -> failed
                     | None -> (
                         match List.assoc_opt (repo_of_key key) waiting with
                         | Some earlier when String.compare earlier key < 0 ->
                             Some earlier
                         | _ -> None)
                   in
                   match behind with
                   | None -> if one item then None else Some key
                   | Some behind ->
                       Store.hold store "inbox" key ~behind;
                       Some behind)
                 None items))
          (groups (Store.ready store "inbox" ~now ~limit:64));
        Eio.Time.sleep system#clock 0.5
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          report "inbox" exn;
          Eio.Time.sleep system#clock 5.
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
      retry (fun () ->
          connect ~system ~store ~settings:(Engine.settings engine) ~url ~health
            ~policy))
