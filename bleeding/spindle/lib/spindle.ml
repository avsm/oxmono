(* SPDX-License-Identifier: ISC *)
module Job = Job
module Service_auth = Auth
module Operations = Operations
open Json
open Proffer
module Status = Httpz.Res
module Method = Httpz.Method

type config = {
  hostname : string;
  owner : string;
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
      ~audience:("did:web:" ^ state.config.hostname)
      ~meth ~now token
  in
  actor

let check_repo state repo = ignore (Engine.managed state.engine repo)

let find state id =
  match Engine.find state.engine id with
  | Some p -> p
  | None -> reject Status.Not_found "PipelineNotFound" "pipeline not found"

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
  let kinds = query_list req "kinds" in
  if
    List.exists
      (fun k -> not (List.mem k [ "manual"; "push"; "pull_request" ]))
      kinds
  then invalid "invalid trigger kind";
  let commits = query_list req "commits" in
  Engine.query state.engine ~repo ~limit ~cursor ~kinds ~commits

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
    W.create ~role:Server ~max_message:4096 ~with_write_lock
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
        then W.close ws ()
        else (
          Eio.Time.sleep state.system#clock 0.05;
          send ())
      in
      send ())

let upgrade state req respond =
  if Req.meth req <> Method.Get then invalid "logs require GET";
  let p = find state (query_required req "pipeline") in
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
      Resp.upgrade respond ~protocol:"websocket" ~headers (logs state workflows)

let respond_json respond ?(status = Status.Success) ?(headers = Headers.empty)
    json =
  let text = encode json in
  Resp.v respond ~status ~headers ~content_type:(This "application/json")
    (Body.String text)

let handle state name req respond =
  try
    if name = "sh.tangled.ci.subscribePipelineLogs" then
      upgrade state req respond
    else
      let post =
        name = "sh.tangled.ci.triggerPipeline"
        || name = "sh.tangled.ci.cancelPipeline"
      in
      if
        (post && Req.meth req <> Method.Post)
        || (not post)
           && Req.meth req <> Method.Get
           && Req.meth req <> Method.Head
      then reject Status.Method_not_allowed "InvalidRequest" "wrong HTTP method";
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
            let id = "did:web:" ^ state.config.hostname in
            obj
              [
                ("@context", arr [ str "https://www.w3.org/ns/did/v1" ]);
                ("id", str id);
                ( "service",
                  arr
                    [
                      obj
                        [
                          ("id", str (id ^ "#tangled_spindle"));
                          ("type", str "TangledSpindle");
                          ( "serviceEndpoint",
                            str
                              ((if state.config.allow_http then "http://"
                                else "https://")
                              ^ state.config.hostname) );
                        ];
                    ] );
              ]
        | "sh.tangled.owner" -> obj [ ("owner", str state.config.owner) ]
        | "sh.tangled.ci.getPipeline" ->
            Engine.view (find state (query_required req "pipeline"))
        | "sh.tangled.ci.queryPipelines" -> query_pipelines state req
        | "sh.tangled.ci.describeWorkflowDefinition" ->
            check_repo state (query_required req "repo");
            ignore (sha (query_required req "sha"));
            Option.iter
              (fun repo -> ignore (Catalog.verified state.engine.catalog repo))
              (query req "sourceRepo");
            obj
              [
                ("derived", bool false);
                ( "workflows",
                  arr
                    (List.map
                       (fun (job : Job.t) -> str job.name)
                       state.config.jobs) );
              ]
        | "sh.tangled.ci.triggerPipeline" -> (
            let actor = authorize state name req in
            let request = decode (Req.globalize (Req.body req)) in
            match
              Engine.create state.engine ~automatic:false ~actor request
            with
            | None -> invalid "no workflows match this event"
            | Some id ->
                obj
                  [
                    ( "pipeline",
                      str
                        ("at://did:web:" ^ state.config.hostname
                       ^ "/sh.tangled.pipeline/" ^ id) );
                  ])
        | "sh.tangled.ci.cancelPipeline" ->
            let actor = authorize state name req in
            let body = decode (Req.globalize (Req.body req)) in
            ignore (find state (get "pipeline" body));
            Engine.cancel state.engine ~actor ~repo:(get "repo" body)
              ~id:(get "pipeline" body) ~names:(strings "workflows" body);
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
  | Catalog.Pending ->
      respond_json respond ~status:Status.Service_unavailable
        ~headers:(Headers.of_list [ ("Retry-After", "2") ])
        (obj
           [
             ("error", str "CatalogPending");
             ("message", str "repository discovery is refreshing");
           ])
  | Http_error (status, code, message) ->
      let headers =
        if status <> Status.Method_not_allowed then Headers.empty
        else
          Headers.of_list
            [
              ( "Allow",
                if
                  name = "sh.tangled.ci.triggerPipeline"
                  || name = "sh.tangled.ci.cancelPipeline"
                then "POST"
                else "GET, HEAD" );
            ]
      in
      respond_json respond ~status ~headers
        (obj [ ("error", str code); ("message", str message) ])
  | Eio.Cancel.Cancelled _ as exn -> raise exn
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
  ignore (did ("did:web:" ^ config.hostname));
  if config.repo = None && config.jetstream = None then
    invalid "configure Jetstream or a static repository";
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
          rkey = "";
          knot = "";
          source;
        })
      config.repo
  in
  let catalog =
    Catalog.v ~store ~network ~owner:config.owner ~hostname:config.hostname
      ~static
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
