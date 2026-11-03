(* SPDX-License-Identifier: ISC *)
module Job = Job
module Executor = Executor
module Service_auth = Auth
module Operations = Operations
module Catalog = Catalog
module Settings = Settings
open Json
open Proffer
module Status = Httpz.Res
module Method = Httpz.Method

type config = {
  hostname : string;
  owner : string;
  admins : string list;
  admission : Catalog.admission;
  repo : (string * string) option;
  plc : string;
  state_dir : string;
  port : int;
  jobs : Job.t list;
  jetstream : string option;
  allow_http : bool;
  executor : Executor.t;
  workflow_timeout : float;
  quotas : Settings.defaults;
  private_webhooks : bool;
  cors_origins : string list;
}

type state = {
  config : config;
  engine : Engine.t;
  network : Network.t;
  system : Eio_unix.Stdenv.base;
  health : Health.t;
  settings : Settings.t;
  hooks : Hooks.t;
}

exception Not_ready of Jsont.json

let reject = Xrpc_error.fail
let optional = Runner.optional

(* [fields req name] is every value of the request field [name], which must
   be lowercase. *)
let fields req name =
  let values = ref [] in
  Headers.iter
    (fun _ field value ->
      if String.lowercase_ascii (Req.globalize field) = name then
        values := Req.globalize value :: !values)
    (Req.headers req);
  !values

let bearer req =
  match fields req "authorization" with
  | [ s ] when String.starts_with ~prefix:"Bearer " s ->
      String.sub s 7 (String.length s - 7)
  | _ -> raise Auth.Rejected

let authorize state meth req =
  let token = bearer req in
  let now = Eio.Time.now state.system#clock in
  let actor =
    Auth.authenticate
      ~resolve:(Network.resolve state.network)
      ~consume:(fun ~issuer ~jti ~expires ->
        let now = Eio.Time.now state.system#clock in
        expires > now
        && Store.consume state.engine.store ~now ~issuer ~jti ~expires)
      ~audience:(Catalog.web_did state.config.hostname)
      ~meth ~now token
  in
  if Settings.banned state.settings actor then
    reject Status.Forbidden "AccessControl"
      "This DID is banned on this spindle.";
  actor

let find state ~repo id =
  match Engine.find state.engine id with
  | Some p when p.repo = repo -> p
  | _ -> reject Status.Not_found "PipelineNotFound" "pipeline not found"

let query req k =
  match List.filter (fun (name, _) -> name = k) (Req.query req) with
  | [] -> None
  | [ (_, value) ] -> Some value
  | _ -> invalid ("duplicate query parameter " ^ k)

let query_required req k =
  match query req k with Some v -> v | None -> invalid (k ^ " is required")

let query_list req k =
  List.filter_map
    (fun (name, value) -> if name = k then Some value else None)
    (Req.query req)

let query_pipelines state req =
  let repo = did (query_required req "repo") in
  let limit =
    match query req "limit" with
    | None -> 50
    | Some s -> (
        match int_of_string_opt s with
        | Some n when n >= 1 && n <= 250 -> n
        | _ -> invalid "limit must be between 1 and 250")
  in
  let cursor = query req "cursor" in
  Option.iter
    (fun id -> if not (Atp.Tid.is_valid id) then invalid "invalid cursor")
    cursor;
  let triggers = query_list req "triggers" in
  let commits = query_list req "commits" in
  Engine.query state.engine ~repo ~limit ~cursor ~triggers ~commits

let frame event =
  let kind = get "type" event in
  let body = obj (List.remove_assoc "type" (members event)) in
  let cbor j =
    match Atp.Dagcbor.of_json j with
    | Ok v -> Atp.Dagcbor.encode_string v
    | Error e -> failwith e
  in
  Bytes.of_string
    (cbor (obj [ ("op", int 1); ("t", str ("#" ^ kind)) ]) ^ cbor body)

let logs state workflows socket =
  let module W = Httpz_websocket in
  let mutex = Eio.Mutex.create () in
  let with_write_lock f =
    Eio.Mutex.lock mutex;
    match f () with
    | () -> Eio.Mutex.unlock mutex
    | exception exn ->
        Eio.Mutex.unlock mutex;
        raise exn
  in
  let ws =
    W.create ~role:Server ~max_message:(1024 * 1024) ~with_write_lock
      ~read:(fun bytes ~off ~len ->
        Eio.Time.with_timeout_exn state.system#clock 90. (fun () ->
            Body.Socket.read socket bytes ~off ~len))
      ~write:(fun bytes ~off ~len ->
        Eio.Time.with_timeout_exn state.system#clock 10. (fun () ->
            Body.Socket.write_sub socket bytes ~off ~len))
      ()
  in
  Eio.Fiber.first
    (fun () ->
      Eio.Fiber.first
        (fun () ->
          while W.receive ws ~f:(fun _ _ ~off:_ ~len:_ -> ()) do
            ()
          done)
        (fun () ->
          let empty = Bytes.empty in
          while true do
            Eio.Time.sleep state.system#clock 30.;
            W.ping ws empty ~off:0 ~len:0
          done))
    (fun () ->
      let subscriptions = List.map (fun p -> (p, ref [])) workflows in
      let rec send () =
        List.iter
          (fun ((p : Runner.t), sent) ->
            let snapshot = p.events in
            let rec added acc events =
              if events == !sent then acc
              else
                match events with
                | [] -> acc
                | event :: rest -> added (event :: acc) rest
            in
            List.iter
              (fun event ->
                let bytes = frame event in
                W.send ws Binary bytes ~off:0 ~len:(Bytes.length bytes))
              (added [] snapshot);
            sent := snapshot)
          subscriptions;
        if
          List.for_all
            (fun ((run : Runner.t), sent) ->
              Runner.terminal run && !sent == run.events)
            subscriptions
        then (
          W.close ws ();
          (* The receiving fiber completes when the peer acknowledges Close.
             Give it time to finish before the outer race closes the socket. *)
          Eio.Time.sleep state.system#clock 5.)
        else (
          Eio.Time.sleep state.system#clock 0.05;
          send ())
      in
      send ())

let upgrade state req respond =
  if Req.meth req <> Method.Get then invalid "logs require GET";
  let repo = did (query_required req "repo") in
  let p = find state ~repo (query_required req "pipeline") in
  let workflows = Engine.select_workflows p (query_list req "workflows") in
  let fields = ref [] in
  Headers.iter
    (fun _ name value ->
      fields := (Req.globalize name, Req.globalize value) :: !fields)
    (Req.headers req);
  let fields =
    if Req.connection_upgrade req then ("Connection", "Upgrade") :: !fields
    else !fields
  in
  match
    Httpz_websocket.Handshake.accept ~meth:"GET"
      ~http_1_1:(Req.version req = Httpz.Version.Http_1_1)
      fields
  with
  | Error e -> invalid e
  | Ok fields ->
      let headers =
        Headers.of_list
          (List.filter
             (fun (name, _) -> name <> "Connection" && name <> "Upgrade")
             fields)
      in
      Resp.upgrade respond ~protocol:"websocket" ~headers (fun socket ->
          try logs state workflows socket with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn ->
              (* The upgrade has already committed the HTTP response. *)
              Printf.eprintf "spindle log stream: %s\n%!"
                (Printexc.to_string exn))

(* Browsers on the Tangled site call XRPC methods directly. A configured
   origin, or any origin for ["*"], receives CORS fields, as the reference
   spindle's CORS_ORIGINS gives them. *)
let cors state req =
  let allowed = state.config.cors_origins in
  if allowed = [] then []
  else
    let vary = ("Vary", "Origin") in
    match fields req "origin" with
    | [ _ ] when List.mem "*" allowed ->
        [ vary; ("Access-Control-Allow-Origin", "*") ]
    | [ origin ] when List.mem origin allowed ->
        [ vary; ("Access-Control-Allow-Origin", origin) ]
    | _ -> [ vary ]

let allowed_methods ~post =
  if post then "POST, OPTIONS" else "GET, HEAD, OPTIONS"

(* A preflight is answered here, before authentication, since browsers send
   it without credentials. *)
let preflight state req respond ~post =
  let fields = cors state req in
  let fields =
    if List.mem_assoc "Access-Control-Allow-Origin" fields then
      fields
      @ [
          ("Access-Control-Allow-Methods", "GET, POST");
          ("Access-Control-Allow-Headers", "Authorization, Content-Type");
          ("Access-Control-Max-Age", "86400");
        ]
    else fields
  in
  Resp.empty respond ~status:Status.No_content
    ~headers:(Headers.of_list (("Allow", allowed_methods ~post) :: fields))
    ()

let respond_json respond ?(status = Status.Success) ?(headers = Headers.empty)
    json =
  let text = encode json in
  Resp.v respond ~status ~headers ~content_type:(This "application/json")
    (Body.String text)

let workflows state =
  List.map
    (fun (job : Job.t) -> Engine.definition state.config.hostname job.name)
    state.config.jobs

(* [static config repo] is the local clone of the static repository [repo].
   It is read with Git, since it has no knot. *)
let static config repo =
  match config.repo with
  | Some (did, source)
    when did = repo
         && (not (Filename.is_relative source))
         && Sys.file_exists source ->
      Some
        (fun args ->
          let p =
            Unix.open_process_args_in "git"
              (Array.of_list ("git" :: "-C" :: source :: args))
          in
          let out = In_channel.input_all p in
          ignore (Unix.close_process_in p);
          out)
  | _ -> None

(* Repository workflows are read from the knot, or from a static repository's
   clone. A missing directory has none. *)
let read_files config catalog network ~did:repo ~commit =
  let dir = Workflows.directory in
  match (config.repo, static config repo) with
  | Some (did, _), None when did = repo -> []
  | _, Some git ->
      git [ "ls-tree"; "-z"; commit; dir ^ "/" ]
      |> String.split_on_char '\000'
      |> List.filter_map (fun line ->
          match String.index_opt line '\t' with
          | Some i
            when String.starts_with ~prefix:"100644 blob" line
                 || String.starts_with ~prefix:"100755 blob" line ->
              let path = String.sub line (i + 1) (String.length line - i - 1) in
              let name = Filename.basename path in
              Some (name, git [ "cat-file"; "blob"; commit ^ ":" ^ path ])
          | _ -> None)
  | _ ->
      let knot = Catalog.knot_origin catalog repo in
      let xrpc nsid params =
        Network.query (knot ^ "/xrpc/" ^ nsid) (("repo", repo) :: params)
      in
      let tree =
        Network.json network
          (xrpc "org.tangled.git.getTree" [ ("ref", commit); ("path", dir) ])
      in
      if field "error" tree <> None then (
        Printf.eprintf "spindle: no workflows in %s at %s: %s\n%!" repo commit
          (encode tree);
        [])
      else
        let files =
          list (required "entries" tree)
          |> List.filter (fun e ->
              let mode = get "mode" e in
              let mode =
                String.sub mode
                  (String.length mode - min 6 (String.length mode))
                  (min 6 (String.length mode))
              in
              mode = "100644" || mode = "100755")
          |> List.map (get "name")
        in
        if List.length files > 32 then
          invalid "more than 32 files under .tangled/workflows";
        List.filter_map
          (fun name ->
            match
              Network.fetch_status ~limit:262144 network
                (xrpc "org.tangled.git.getBlob"
                   [ ("ref", commit); ("path", dir ^ "/" ^ name) ])
            with
            | 200, contents -> Some (name, contents)
            | status, _ ->
                Printf.eprintf "spindle: %s/%s at %s: HTTP %d\n%!" dir name
                  commit status;
                None)
          files

let resolve_commit state repo ref_ =
  if
    String.length ref_ >= 40
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         ref_
  then Some ref_
  else
    match static state.config repo with
    | Some git -> (
        match
          String.trim
            (git [ "rev-parse"; "--verify"; "-q"; "--"; ref_ ^ "^{commit}" ])
        with
        | "" -> None
        | commit -> Some commit)
    | None -> (
        let knot = Catalog.knot_origin state.engine.catalog repo in
        let commits =
          Network.json state.network
            (Network.query
               (knot ^ "/xrpc/org.tangled.git.listCommits")
               [ ("repo", repo); ("ref", ref_); ("limit", "1") ])
        in
        match field "commits" commits with
        | Some (Jsont.Array (c :: _, _)) -> Some (get "hash" c)
        | _ -> None)

let repository_definitions state repo ref_ =
  match resolve_commit state repo ref_ with
  | None -> []
  | Some commit ->
      Workflows.definitions ~repo ~commit
        (read_files state.config state.engine.catalog state.network ~did:repo
           ~commit)

let ci_output ids =
  obj [ ("ci", obj [ ("pipelines", arr (List.map str ids)) ]) ]

(* Only refs/heads and refs/tags trigger CI. A deleted ref has nothing to run,
   and the skip-ci and ci-skip push options suppress the run. *)
let runnable_push event =
  let ref_ = get "ref" event in
  (String.starts_with ~prefix:"refs/heads/" ref_
  || String.starts_with ~prefix:"refs/tags/" ref_)
  && (not (zero_sha (get "newSha" event)))
  && not
       (List.exists
          (fun o -> List.mem o [ "skip-ci"; "ci-skip" ])
          (strings "pushOptions" event))

(* A repository's token names the user acting through it in [act.sub]. Only
   the repository's own signature makes that claim credible. *)
let attribution req ~actor repo =
  if actor <> repo then actor
  else Option.value ~default:actor (Auth.controller (bearer req))

let notify state req ~issuer body =
  let repo = did (get "repo" body) in
  let event = required "event" body in
  if get "repo" event <> repo then invalid "event names another repository";
  let engine = state.engine in
  let created =
    match get "$type" event with
    | t when t = Engine.push ->
        (* Only the knot named by the repository's DID document reports its
           pushes. *)
        if issuer <> Catalog.knot_did engine.catalog repo then
          raise Auth.Rejected;
        let actor = did (get "actor" event) in
        let new_sha = sha (get "newSha" event) in
        let ref_ = get "ref" event in
        let changes_ref =
          (String.starts_with ~prefix:"refs/heads/" ref_
          || String.starts_with ~prefix:"refs/tags/" ref_)
          && not (zero_sha new_sha)
        in
        (* A banned pusher's push runs nothing and notifies no one. *)
        if Settings.banned state.settings actor then None
        else (
          (if changes_ref then
             match Catalog.managed engine.catalog repo with
             | Some r when Catalog.admitted engine.catalog r.owner ->
                 Hooks.push state.hooks r ~actor ~ref_
                   ~before:(get "oldSha" event) ~after:new_sha
             | _ -> ());
          if not (runnable_push event) then None
          else
            let managed = Engine.managed engine repo in
            let ref_ = get "ref" event in
            let default_ref =
              match Catalog.default_branch engine.catalog managed with
              | Some branch -> ref_ = "refs/heads/" ^ branch
              | None -> false
            in
            Engine.create engine
              ~dedup:
                (String.concat "/"
                   [ "push"; repo; ref_; get "oldSha" event; new_sha ])
              ~automatic:true ~default_ref ~actor
              (obj [ ("repo", str repo); ("trigger", event) ]))
    | t when t = Engine.manual ->
        let fields =
          [ ("repo", str repo); ("trigger", event) ]
          @
          match field "workflows" event with
          | Some names -> [ ("workflows", names) ]
          | None -> []
        in
        Engine.create engine ~automatic:false ~principal:issuer
          ~actor:(attribution req ~actor:issuer repo)
          (obj fields)
    | _ -> invalid "unsupported event type"
  in
  ci_output (Option.to_list created)

let served state req =
  ignore (Engine.managed state.engine (did (query_required req "repo")))

let require_admin state actor =
  if not (Catalog.admin state.engine.catalog actor) then
    reject Status.Unauthorized "AccessControl"
      ("DID does not have sufficient access permissions for this operation: "
     ^ actor)

(* Secrets and webhooks are the settings of a served repository, which its
   owner and collaborators may change. Tangled also signs these requests with
   the repository's own key. *)
let settings_repo state actor repo =
  let repo = did repo in
  match Catalog.managed state.engine.catalog repo with
  | None -> reject Status.Not_found "RepoNotFound" "failed to access repository"
  | Some r ->
      if not (Catalog.permitted state.engine.catalog r actor) then
        reject Status.Forbidden "AccessControl"
          ("DID does not have sufficient access permissions for this \
            operation: " ^ actor);
      if actor = r.did && Settings.banned state.settings r.owner then
        reject Status.Forbidden "AccessControl"
          "This repo is banned on this spindle.";
      r

(* A ban names a repository and an owner alike. Everything kept for them is
   removed, as the reference spindle wipes it. *)
let wipe state subject =
  let repos =
    List.sort_uniq compare (subject :: Settings.repos_of state.settings subject)
  in
  List.iter
    (fun repo ->
      Engine.wipe state.engine repo;
      Settings.wipe state.settings repo;
      Hooks.wipe state.hooks repo)
    repos

let usage state ~scope ~subject =
  let repos = Store.list state.engine.store "repo-owner" in
  let webhooks owner_of =
    let hooks = Hooks.counts state.hooks in
    List.fold_left
      (fun acc (repo, n) ->
        let key = owner_of repo in
        (key, n + Option.value ~default:0 (List.assoc_opt key acc))
        :: List.remove_assoc key acc)
      [] hooks
  in
  let rows scope_name dids webhooks =
    List.concat_map
      (fun d ->
        List.filter_map
          (fun (resource, used) ->
            if used = 0 then None
            else
              Some
                (obj
                   [
                     ("scope", str scope_name);
                     ("did", str d);
                     ("resource", str resource);
                     ("used", int used);
                   ]))
          [
            ("workflows", Engine.running state.engine d);
            ("webhooks", Option.value ~default:0 (List.assoc_opt d webhooks));
            ("ci_minutes_monthly", Settings.minutes state.settings d);
          ])
      dids
  in
  let repo_dids = List.map fst repos in
  let owners = List.sort_uniq compare (List.map snd repos) in
  let owner_of repo = Option.value ~default:repo (List.assoc_opt repo repos) in
  let keep d = match subject with None -> true | Some s -> s = d in
  obj
    [
      ( "usages",
        arr
          ((if scope = None || scope = Some "repo" then
              rows "repo" (List.filter keep repo_dids) (webhooks Fun.id)
            else [])
          @
          if scope = None || scope = Some "user" then
            rows "user" (List.filter keep owners) (webhooks owner_of)
          else []) );
    ]

let procedures =
  [
    "org.tangled.event.notifyEvent";
    "org.tangled.ci.cancelPipeline";
    "org.tangled.ci.approvePipeline";
    "org.tangled.ci.retryPipeline";
    "org.tangled.temp.spindle.allowList.add";
    "org.tangled.temp.spindle.allowList.remove";
    "org.tangled.secret.addSecret";
    "org.tangled.secret.removeSecret";
    "org.tangled.webhook.createWebhook";
    "org.tangled.webhook.updateWebhook";
    "org.tangled.webhook.deleteWebhook";
    "org.tangled.webhook.retryWebhookDelivery";
    "org.tangled.temp.spindle.moderation.ban";
    "org.tangled.temp.spindle.moderation.unban";
    "org.tangled.temp.spindle.quota.set";
    "org.tangled.temp.spindle.quota.unset";
    "org.tangled.temp.server.createInviteCode";
    "org.tangled.temp.server.redeemInviteCode";
  ]

let handle state name req respond =
  let post = List.mem name procedures in
  let cors =
    if name = "_did" then Headers.empty else Headers.of_list (cors state req)
  in
  let respond_json respond ?status ?(headers = Headers.empty) json =
    respond_json respond ?status ~headers:(headers @ cors) json
  in
  try
    if Req.meth req = Method.Options then preflight state req respond ~post
    else if name = "org.tangled.ci.subscribePipelineLogs" then
      upgrade state req respond
    else
      let meth = Req.meth req in
      if
        (post && meth <> Method.Post)
        || ((not post) && meth <> Method.Get && meth <> Method.Head)
      then reject Status.Method_not_allowed "InvalidRequest" "wrong HTTP method";
      let body () = decode (Req.globalize (Req.body req)) in
      let catalog = state.engine.catalog in
      let result =
        match name with
        | "_health" ->
            snd
              (Health.report state.health
                 ~now:(Eio.Time.now state.system#clock))
        | "_ready" ->
            let ready, report =
              Health.report state.health ~now:(Eio.Time.now state.system#clock)
            in
            if not ready then raise (Not_ready report);
            report
        | "_did" ->
            let id = Catalog.self catalog in
            obj
              [
                ("@context", arr [ str "https://www.w3.org/ns/did/v1" ]);
                ("id", str id);
                ( "service",
                  arr
                    [
                      obj
                        [
                          ("id", str "#tangled_spindle");
                          ("type", str "TangledSpindle");
                          ( "serviceEndpoint",
                            str
                              ((if state.config.allow_http then "http://"
                                else "https://")
                              ^ state.config.hostname) );
                        ];
                    ] );
              ]
        | "org.tangled.spindle.describeSpindle" ->
            obj
              [
                ("did", str (Catalog.self catalog));
                ("version", str "oxmono");
                ("capabilities", arr [ str "tangled1.0" ]);
              ]
        | "org.tangled.ci.getPipeline" ->
            let repo = did (query_required req "repo") in
            Engine.view state.engine
              (find state ~repo (query_required req "pipeline"))
        | "org.tangled.ci.queryPipelines" -> query_pipelines state req
        | "org.tangled.ci.listWorkflowDefinitions" ->
            let repo = did (query_required req "repo") in
            served state req;
            let ref_ = Option.value ~default:"HEAD" (query req "ref") in
            obj
              [
                ( "workflows",
                  arr (workflows state @ repository_definitions state repo ref_)
                );
              ]
        | "org.tangled.ci.getWorkflowDefinition" -> (
            served state req;
            let name = query_required req "workflow" in
            let repo = did (query_required req "repo") in
            match
              List.find_opt
                (fun (job : Job.t) -> job.name = name)
                state.config.jobs
            with
            | Some job ->
                obj
                  [
                    ( "workflow",
                      Engine.definition state.config.hostname job.name );
                  ]
            | None -> (
                match
                  List.find_opt
                    (fun d -> get "id" d = name)
                    (repository_definitions state repo "HEAD")
                with
                | Some d -> obj [ ("workflow", d) ]
                | None ->
                    reject Status.Not_found "WorkflowNotFound"
                      "workflow not found"))
        | "org.tangled.event.notifyEvent" ->
            let issuer = authorize state name req in
            notify state req ~issuer (body ())
        | "org.tangled.ci.cancelPipeline" ->
            let actor = authorize state name req in
            let body = body () in
            let repo = did (get "repo" body) in
            let p = find state ~repo (get "pipeline" body) in
            Engine.cancel state.engine ~actor ~repo ~id:p.id
              ~names:(strings "workflows" body);
            obj []
        | "org.tangled.ci.approvePipeline" ->
            let actor = authorize state name req in
            let body = body () in
            let repo = did (get "repo" body) in
            let p = find state ~repo (get "pipeline" body) in
            if
              not
                (List.exists
                   (fun (r : Runner.t) -> r.status = "blocked")
                   p.workflows)
            then
              reject Status.Bad_request "PipelineNotBlocked"
                "pipeline has no workflow waiting for approval";
            Engine.approve state.engine ~actor ~repo ~id:p.id;
            obj []
        | "org.tangled.ci.retryPipeline" -> (
            let actor = authorize state name req in
            let body = body () in
            let repo = did (get "repo" body) in
            let p = find state ~repo (get "pipeline" body) in
            match
              Engine.retry state.engine ~principal:actor
                ~actor:(attribution req ~actor repo)
                ~repo ~id:p.id ~names:(strings "workflows" body)
            with
            | Some id -> obj [ ("pipeline", str id) ]
            | None ->
                reject Status.Bad_request "PipelineNotRetryable"
                  "no workflow matches the original trigger")
        | "org.tangled.temp.spindle.allowList.list" ->
            require_admin state (authorize state name req);
            obj [ ("dids", arr (List.map str (Catalog.allowed catalog))) ]
        | "org.tangled.temp.spindle.allowList.add" ->
            require_admin state (authorize state name req);
            Catalog.allow catalog (did (get "did" (body ())));
            obj []
        | "org.tangled.temp.spindle.allowList.remove" ->
            require_admin state (authorize state name req);
            Catalog.disallow catalog (did (get "did" (body ())));
            obj []
        | "org.tangled.secret.listSecrets" ->
            let actor = authorize state name req in
            let r = settings_repo state actor (query_required req "repo") in
            Settings.list_secrets state.settings r.did
        | "org.tangled.secret.addSecret" ->
            let actor = authorize state name req in
            let body = body () in
            let key = get "key" body and value = get "value" body in
            Settings.check_secret ~key ~value:(Some value);
            let r = settings_repo state actor (get "repo" body) in
            Settings.add_secret state.settings ~repo:r.did
              ~by:(attribution req ~actor r.did)
              key value;
            obj []
        | "org.tangled.secret.removeSecret" ->
            let actor = authorize state name req in
            let body = body () in
            let key = get "key" body in
            Settings.check_secret ~key ~value:None;
            let r = settings_repo state actor (get "repo" body) in
            Settings.remove_secret state.settings ~repo:r.did key;
            obj []
        | "org.tangled.webhook.listWebhooks" ->
            let actor = authorize state name req in
            let r = settings_repo state actor (query_required req "repo") in
            Hooks.list state.hooks ~repo:r.did
        | "org.tangled.webhook.listWebhookDeliveries" ->
            let actor = authorize state name req in
            let r = settings_repo state actor (query_required req "repo") in
            let limit =
              match Option.bind (query req "limit") int_of_string_opt with
              | Some n when n >= 1 && n <= 100 -> n
              | _ -> 5
            in
            Hooks.deliveries state.hooks ~repo:r.did
              ~ids:(query_list req "webhooks")
              ~limit
        | "org.tangled.webhook.createWebhook" ->
            let actor = authorize state name req in
            let body = body () in
            let r = settings_repo state actor (get "repo" body) in
            Hooks.create state.hooks ~repo:r body
        | "org.tangled.webhook.updateWebhook" ->
            let actor = authorize state name req in
            let body = body () in
            let r = settings_repo state actor (get "repo" body) in
            Hooks.update state.hooks ~repo:r.did body;
            obj []
        | "org.tangled.webhook.deleteWebhook" ->
            let actor = authorize state name req in
            let body = body () in
            let r = settings_repo state actor (get "repo" body) in
            Hooks.delete state.hooks ~repo:r.did (get "webhook" body);
            obj []
        | "org.tangled.webhook.retryWebhookDelivery" ->
            let actor = authorize state name req in
            let body = body () in
            let r = settings_repo state actor (get "repo" body) in
            if not (Catalog.admitted catalog r.owner) then
              reject Status.Forbidden "OwnerPaused"
                "this owner isn't admitted on this spindle right now, so \
                 nothing runs or is sent for their repos";
            Hooks.retry state.hooks ~repo:r.did body;
            obj []
        | "org.tangled.temp.spindle.moderation.listBans" ->
            require_admin state (authorize state name req);
            Settings.bans state.settings
        | "org.tangled.temp.spindle.moderation.getBan" -> (
            require_admin state (authorize state name req);
            let subject = did (query_required req "did") in
            match
              List.find_opt
                (fun b -> get "did" b = subject)
                (list (required "bans" (Settings.bans state.settings)))
            with
            | Some b -> obj [ ("ban", b) ]
            | None -> reject Status.Not_found "BanNotFound" "ban not found")
        | "org.tangled.temp.spindle.moderation.ban" ->
            require_admin state (authorize state name req);
            let subject = get "did" (body ()) in
            if not (Atp.Did.is_valid subject) then
              reject Status.Bad_request "InvalidDid"
                ("invalid DID: " ^ subject);
            Settings.ban state.settings subject;
            wipe state subject;
            obj []
        | "org.tangled.temp.spindle.moderation.unban" ->
            require_admin state (authorize state name req);
            let subject = get "did" (body ()) in
            if not (Atp.Did.is_valid subject) then
              reject Status.Bad_request "InvalidDid"
                ("invalid DID: " ^ subject);
            Settings.unban state.settings subject;
            obj []
        | "org.tangled.temp.spindle.quota.list" ->
            require_admin state (authorize state name req);
            Settings.limits state.settings
        | "org.tangled.temp.spindle.quota.get" -> (
            require_admin state (authorize state name req);
            let subject = did (query_required req "did") in
            let resource = query_required req "resource" in
            match
              List.find_opt
                (fun l -> get "did" l = subject && get "resource" l = resource)
                (list (required "limits" (Settings.limits state.settings)))
            with
            | Some l -> obj [ ("limit", l) ]
            | None ->
                reject Status.Not_found "LimitNotFound"
                  "no override set for this did and resource")
        | "org.tangled.temp.spindle.quota.usage" ->
            require_admin state (authorize state name req);
            let scope = query req "scope" in
            (match scope with
            | None | Some ("user" | "repo") -> ()
            | Some s ->
                reject Status.Bad_request "Generic" ("invalid scope: " ^ s));
            usage state ~scope ~subject:(Option.map did (query req "did"))
        | "org.tangled.temp.spindle.quota.set" ->
            require_admin state (authorize state name req);
            let body = body () in
            let limit =
              match (field "limit" body, field "unlimited" body) with
              | Some n, (None | Some (Jsont.Bool (false, _))) ->
                  Some (int_of_float (number n))
              | None, Some (Jsont.Bool (true, _)) -> None
              | None, _ ->
                  reject Status.Bad_request "Generic"
                    "must specify either limit or unlimited"
              | Some _, Some _ ->
                  reject Status.Bad_request "Generic"
                    "cannot specify both limit and unlimited"
            in
            Settings.set_limit state.settings (get "did" body)
              (get "resource" body) limit;
            obj []
        | "org.tangled.temp.spindle.quota.unset" ->
            require_admin state (authorize state name req);
            let body = body () in
            Settings.unset_limit state.settings (get "did" body)
              (get "resource" body);
            obj []
        | "org.tangled.temp.server.createInviteCode" ->
            let actor = authorize state name req in
            require_admin state actor;
            let body = body () in
            let uses =
              match field "useCount" body with
              | Some n -> int_of_float (number n)
              | None -> 1
            in
            Settings.invite state.settings ~by:actor ~uses
              ~for_account:(Option.map string (field "forAccount" body))
        | "org.tangled.temp.server.redeemInviteCode" ->
            let actor = authorize state name req in
            Settings.redeem state.settings (get "code" (body ())) ~actor;
            Catalog.allow catalog actor;
            obj []
        | _ -> reject Status.Not_found "MethodNotFound" "unknown XRPC method"
      in
      respond_json respond result
  with
  | Not_ready report ->
      respond_json respond ~status:Status.Service_unavailable report
  | Invalid message ->
      respond_json respond ~status:Status.Bad_request
        (obj [ ("error", str "InvalidRequest"); ("message", str message) ])
  | Auth.Rejected ->
      respond_json respond ~status:Status.Unauthorized
        ~headers:(Headers.of_list [ ("WWW-Authenticate", "Bearer") ])
        (obj
           [
             ("error", str "AuthRequired");
             ( "message",
               str
                 "fresh service-auth token and repository write access required"
             );
           ])
  | Engine.Capacity ->
      respond_json respond ~status:Status.Service_unavailable
        (obj
           [
             ("error", str "CapacityExceeded");
             ("message", str "job queue is full");
           ])
  | Xrpc_error.E (status, code, message) ->
      let headers =
        match status with
        | Status.Method_not_allowed ->
            Headers.of_list [ ("Allow", allowed_methods ~post) ]
        | Status.Unauthorized ->
            Headers.of_list [ ("WWW-Authenticate", "Bearer") ]
        | _ -> Headers.empty
      in
      respond_json respond ~status ~headers
        (obj [ ("error", str code); ("message", str message) ])
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Io _ as exn ->
      (* An unreachable knot, PDS or DID host. The knot retries a 5xx push. *)
      Printf.eprintf "spindle XRPC %s: %s\n%!" name (Printexc.to_string exn);
      respond_json respond ~status:Status.Service_unavailable
        (obj
           [
             ("error", str "UpstreamUnavailable");
             ("message", str "a repository's knot or identity is unreachable");
           ])
  | exn ->
      Printf.eprintf "spindle XRPC %s: %s\n%!" name (Printexc.to_string exn);
      respond_json respond ~status:Status.Internal_server_error
        (obj
           [
             ("error", str "InternalError");
             ("message", str "request could not be completed");
           ])

let run ?(addr = "127.0.0.1") ?(operations = Operations.default) system config =
  ignore (did config.owner);
  ignore (did (Catalog.web_did config.hostname));
  Option.iter
    (fun url ->
      let uri = Httpz_uri.of_string_exn url in
      (match Httpz_uri.scheme uri with
      | This "wss" -> ()
      | This "ws" when config.allow_http -> ()
      | _ -> invalid "Jetstream requires WSS, or --allow-http for local WS");
      if Httpz_uri.has_userinfo uri || Httpz_uri.has_fragment uri then
        invalid "invalid Jetstream URL")
    config.jetstream;
  let state_dir =
    if Filename.is_relative config.state_dir then
      Filename.concat (Sys.getcwd ()) config.state_dir
    else config.state_dir
  in
  Eio.Switch.run @@ fun sw ->
  let directory = Eio.Path.(system#fs / state_dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 directory;
  let lock =
    Unix.openfile
      Eio.Path.(native_exn (directory / ".lock"))
      [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ]
      0o600
  in
  Fun.protect ~finally:(fun () -> Unix.close lock) @@ fun () ->
  Unix.lockf lock Unix.F_TLOCK 0;
  let network =
    Network.v ~allow_http:config.allow_http ~plc:config.plc system
  in
  let store = Store.open_ ~sw directory in
  Store.set_limits store operations;
  let static =
    Option.map
      (fun (repo, source) ->
        {
          Catalog.did = did repo;
          owner = config.owner;
          name = "";
          knot = "";
          source;
          created = "";
        })
      config.repo
  in
  let settings =
    Settings.v ~store
      ~now:(fun () -> Eio.Time.now system#clock)
      ~defaults:config.quotas
  in
  let catalog =
    Catalog.v ~store ~settings ~network ~owner:config.owner
      ~admins:config.admins ~hostname:config.hostname
      ~admission:config.admission ~static
      ~ttl:(float_of_int operations.reconcile_seconds)
      ~now:(fun () -> Eio.Time.now system#clock)
      ()
  in
  let hooks =
    Hooks.v ~sw ~store ~settings ~allow_private:config.private_webhooks system
  in
  Catalog.on_rename catalog (fun r ~old_name ->
      if Catalog.admitted catalog r.owner then Hooks.renamed hooks r ~old_name);
  let engine =
    Engine.v ~store ~catalog ~settings ~hostname:config.hostname
      ~jobs:config.jobs ~timeout:config.workflow_timeout
      ~executor:config.executor
      ~read_workflows:(read_files config catalog network)
      ~system ~directory ~sw ()
  in
  Engine.migrate engine config.repo;
  Engine.load engine;
  let health = Health.v ~store ~enabled:(config.jetstream <> None) in
  let state = { config; engine; network; system; health; settings; hooks } in
  Eio.Fiber.fork ~sw (fun () ->
      while true do
        let now = Eio.Time.now system#clock in
        (try
           let pipelines, receipts =
             Lock.protect engine.lock (fun () ->
                 Store.prune store ~now operations)
           in
           Store.put store "health" "maintenance"
             (encode
                (obj
                   [
                     ("lastSuccessAt", Jsont.Json.number now);
                     ("pipelinesRemoved", int pipelines);
                     ("receiptsRemoved", int receipts);
                   ]))
         with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
            Printf.eprintf "spindle retention: %s\n%!" (Printexc.to_string exn);
            Store.put store "health" "maintenance"
              (encode
                 (obj
                    [
                      ("lastFailureAt", Jsont.Json.number now);
                      ("error", str (Printexc.to_string exn));
                    ])));
        Eio.Time.sleep system#clock
          (float_of_int operations.maintenance_seconds)
      done);
  Option.iter
    (fun jetstream ->
      Eio.Fiber.fork ~sw (fun () ->
          Observer.run ~engine ~network ~hooks ~jetstream ~health
            ~policy:operations))
    config.jetstream;
  let open Route in
  let handler name env req respond = env (Req.globalize name) req respond in
  let site =
    Site.of_routes
      [
        get (s "xrpc" / str) handler;
        post (s "xrpc" / str) handler;
        route Method.Options (s "xrpc" / str) handler;
        get
          (s ".well-known" / s "did.json")
          (fun env req respond -> env "_did" req respond);
        get root (fun env req respond -> env "_health" req respond);
        get (s "readyz") (fun env req respond -> env "_ready" req respond);
      ]
  in
  let ip = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string addr) in
  Proffer_httpz.run ~sw
    ~addr:(`Tcp (ip, config.port))
    system ~env:(handle state) site
