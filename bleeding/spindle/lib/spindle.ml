(* SPDX-License-Identifier: ISC *)
module Job = Job
module Service_auth = Auth
module Operations = Operations
module Catalog = Catalog
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
}

type state = {
  config : config;
  engine : Engine.t;
  network : Network.t;
  system : Eio_unix.Stdenv.base;
  health : Health.t;
}

exception Http_error of Status.status * string * string
exception Not_ready of Jsont.json

let reject status code message = raise (Http_error (status, code, message))
let optional = Runner.optional

let authorize state meth req =
  let values = ref [] in
  Headers.iter
    (fun _ name value ->
      if String.lowercase_ascii (Req.globalize name) = "authorization" then
        values := Req.globalize value :: !values)
    (Req.headers req);
  let token =
    match !values with
    | [ s ] when String.starts_with ~prefix:"Bearer " s ->
        String.sub s 7 (String.length s - 7)
    | _ -> raise Auth.Rejected
  in
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

let respond_json respond ?(status = Status.Success) ?(headers = Headers.empty)
    json =
  let text = encode json in
  Resp.v respond ~status ~headers ~content_type:(This "application/json")
    (Body.String text)

let workflows state =
  List.map
    (fun (job : Job.t) -> Engine.definition state.config.hostname job.name)
    state.config.jobs

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

let notify state ~issuer body =
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
            (obj [ ("repo", str repo); ("trigger", event) ])
    | t when t = Engine.manual ->
        let fields =
          [ ("repo", str repo); ("trigger", event) ]
          @
          match field "workflows" event with
          | Some names -> [ ("workflows", names) ]
          | None -> []
        in
        Engine.create engine ~automatic:false ~actor:issuer (obj fields)
    | _ -> invalid "unsupported event type"
  in
  ci_output (Option.to_list created)

let served state req =
  ignore (Engine.managed state.engine (did (query_required req "repo")))

let require_admin state actor =
  if not (Catalog.admin state.engine.catalog actor) then raise Auth.Rejected

let procedures =
  [
    "org.tangled.event.notifyEvent";
    "org.tangled.ci.cancelPipeline";
    "org.tangled.ci.approvePipeline";
    "org.tangled.ci.retryPipeline";
    "org.tangled.temp.spindle.allowList.add";
    "org.tangled.temp.spindle.allowList.remove";
  ]

let handle state name req respond =
  try
    if name = "org.tangled.ci.subscribePipelineLogs" then
      upgrade state req respond
    else
      let post = List.mem name procedures in
      if
        (post && Req.meth req <> Method.Post)
        || (not post)
           && Req.meth req <> Method.Get
           && Req.meth req <> Method.Head
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
            served state req;
            obj [ ("workflows", arr (workflows state)) ]
        | "org.tangled.ci.getWorkflowDefinition" -> (
            served state req;
            let name = query_required req "workflow" in
            match
              List.find_opt
                (fun (job : Job.t) -> job.name = name)
                state.config.jobs
            with
            | None ->
                reject Status.Not_found "WorkflowNotFound" "workflow not found"
            | Some job ->
                obj
                  [
                    ( "workflow",
                      Engine.definition state.config.hostname job.name );
                  ])
        | "org.tangled.event.notifyEvent" ->
            let issuer = authorize state name req in
            notify state ~issuer (body ())
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
              Engine.retry state.engine ~actor ~repo ~id:p.id
                ~names:(strings "workflows" body)
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
  | Http_error (status, code, message) ->
      let headers =
        if status <> Status.Method_not_allowed then Headers.empty
        else
          Headers.of_list
            [
              ( "Allow",
                if List.mem name procedures then "POST"
                else "GET, HEAD" );
            ]
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
        })
      config.repo
  in
  let catalog =
    Catalog.v ~store ~network ~owner:config.owner ~admins:config.admins
      ~hostname:config.hostname ~admission:config.admission ~static
      ~ttl:(float_of_int operations.reconcile_seconds)
      ~now:(fun () -> Eio.Time.now system#clock)
      ()
  in
  let engine =
    Engine.v ~store ~catalog ~hostname:config.hostname ~jobs:config.jobs ~system
      ~directory ~sw
  in
  Engine.migrate engine config.repo;
  Engine.load engine;
  let health = Health.v ~store ~enabled:(config.jetstream <> None) in
  let state = { config; engine; network; system; health } in
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
          Observer.run ~engine ~network ~jetstream ~health ~policy:operations))
    config.jetstream;
  let open Route in
  let handler name env req respond = env (Req.globalize name) req respond in
  let site =
    Site.of_routes
      [
        get (s "xrpc" / str) handler;
        post (s "xrpc" / str) handler;
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
