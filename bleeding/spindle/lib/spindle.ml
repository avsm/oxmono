(* SPDX-License-Identifier: ISC *)
module Job = Job
module Service_auth = Auth
open Json
open Proffer
module Status = Httpz.Res
module Method = Httpz.Method

type config = {
  hostname : string;
  owner : string;
  repo : string;
  source : string;
  plc : string;
  state_dir : string;
  port : int;
  job : Job.t;
}

type pipeline = {
  id : string;
  request : Jsont.json;
  metadata : Jsont.json;
  commit : string;
  created : string;
  mutable status : string;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
}

type state = {
  config : config;
  system : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  directory : Eio.Fs.dir_ty Eio.Path.t;
  read : string -> string;
  pipelines : (string, pipeline) Hashtbl.t;
  slots : Eio.Semaphore.t;
  persistence : Eio.Mutex.t;
  mutable last_tid : int64;
}

exception Cancelled
exception Http_error of Status.status * string * string

let now () = Ptime.to_rfc3339 ~tz_offset_s:0 (Ptime_clock.now ())
let terminal p = p.status <> "pending" && p.status <> "running"
let optional k f = function None -> [] | Some v -> [k, f v]
let reject status code message = raise (Http_error (status, code, message))

let view state p =
  obj (["id", str p.id; "repo", str state.config.repo;
        "trigger", required "trigger" p.request; "commit", str p.commit;
        "createdAt", str p.created;
        "workflows", arr [obj (
          ["id", str state.config.job.name; "name", str state.config.job.name;
           "status", str p.status]
          @ optional "startedAt" str p.started
          @ optional "finishedAt" str p.finished
          @ optional "error" str p.error)]])

let persist state p =
  Eio.Mutex.use_rw ~protect:true state.persistence @@ fun () ->
  let data = obj ["pipeline", view state p; "request", p.request;
    "metadata", p.metadata; "events", arr (List.rev p.events)] in
  let open Eio.Path in
  let path = state.directory / (p.id ^ ".json") in
  let temporary = state.directory / (p.id ^ ".tmp") in
  save ~create:(`Or_truncate 0o600) temporary (encode data);
  rename temporary path

let emit state p ~step ~kind fields =
  let event = obj (["type", str kind; "time", str (now ());
    "workflow", str state.config.job.name; "step", int step] @ fields) in
  let size = String.length (encode event) in
  if p.log_bytes + size > 1024 * 1024 then failwith "job log exceeds 1 MiB";
  p.log_bytes <- p.log_bytes + size;
  p.events <- event :: p.events

let control state p ~step ~status command =
  emit state p ~step ~kind:"control" ["content", str command;
    "command", str command; "status", str status; "kind", str "user"];
  persist state p

let data state p ~step ~stream content =
  emit state p ~step ~kind:"data" ["content", str content;
    "stream", str stream]

let command state p ~cwd ~step argv =
  Eio.Switch.run @@ fun sw ->
  let mgr = state.system#process_mgr in
  let output, output_write = Eio.Process.pipe ~sw mgr in
  let errors, errors_write = Eio.Process.pipe ~sw mgr in
  let env = Eio.Process.Env.of_bindings [
    "PATH", "/usr/bin:/bin"; "LANG", "C"; "LC_ALL", "C";
    "GIT_CONFIG_NOSYSTEM", "1"; "GIT_CONFIG_GLOBAL", "/dev/null";
    "GIT_TERMINAL_PROMPT", "0"; "GIT_ALLOW_PROTOCOL", "file:https:http";
    "TANGLED_REPO", state.config.repo; "TANGLED_COMMIT_SHA", p.commit;
    "TANGLED_PIPELINE_ID", p.id;
    "SPINDLE_REQUEST", encode p.metadata] in
  let process = Eio.Process.spawn ~sw mgr ~cwd ~env
      ~stdin:(Eio.Flow.string_source "") ~stdout:output_write
      ~stderr:errors_write argv in
  Eio.Flow.close output_write;
  Eio.Flow.close errors_write;
  let read stream flow () =
    let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
    let rec loop () =
      match Eio.Buf_read.line reader with
      | line -> data state p ~step ~stream (line ^ "\n"); loop ()
      | exception End_of_file -> () in
    loop () in
  Eio.Fiber.all [read "stdout" output; read "stderr" errors;
    (fun () -> Eio.Process.await_exn process)]

let git args = "git" :: "-c" :: "core.hooksPath=/dev/null" :: args

let execute state p =
  let open Eio.Path in
  let work = state.directory / (p.id ^ ".work") in
  Eio.Semaphore.acquire state.slots;
  Fun.protect ~finally:(fun () -> Eio.Semaphore.release state.slots)
  @@ fun () ->
  if not p.cancelled then (
    p.status <- "running";
    p.started <- Some (now ());
    persist state p;
    let result = try
      if p.cancelled then raise Cancelled;
      Eio.Time.with_timeout_exn state.system#clock 60. @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      p.cancel <- Some (fun () -> Eio.Switch.fail sw Cancelled);
      control state p ~step:0 ~status:"start" "checkout";
      command state p ~cwd:state.directory ~step:0
        (git ["clone"; "--no-checkout"; "--no-hardlinks"; "--";
          state.config.source; native_exn work]);
      command state p ~cwd:work ~step:0
        (git ["checkout"; "--detach"; p.commit]);
      control state p ~step:0 ~status:"end" "checkout";
      List.iteri (fun index action ->
        let step = index + 1 in
        let name = match action with
          | Job.Metadata -> "spindle request metadata"
          | Job.Command argv -> String.concat " " argv in
        control state p ~step ~status:"start" name;
        (match action with
         | Job.Metadata ->
             let text = encode p.metadata in
             data state p ~step ~stream:"stdout" (text ^ "\n");
             Printf.printf "%s\n%!" text
         | Job.Command argv -> command state p ~cwd:work ~step argv);
        control state p ~step ~status:"end" name) state.config.job.steps;
      Ok ()
    with exn -> Error exn in
    Eio.Cancel.protect (fun () ->
      p.cancel <- None;
      p.finished <- Some (now ());
      (match result with
       | Ok () -> p.status <- "success"
       | Error exn ->
           p.status <- (if p.cancelled then "cancelled" else
             match exn with Eio.Time.Timeout -> "timeout" | _ -> "failed");
           p.error <- Some (Printexc.to_string exn));
      persist state p;
      (* Only the generated TID workspace is removed. *)
      Eio.Process.run state.system#process_mgr
        ["rm"; "-rf"; "--"; native_exn work]))

let authorize state meth req =
  let values = ref [] in
  Headers.iter (fun _ name value ->
    if String.lowercase_ascii (Req.globalize name) = "authorization" then
      values := Req.globalize value :: !values) (Req.headers req);
  let token = match !values with
    | [s] when String.starts_with ~prefix:"Bearer " s ->
        String.sub s 7 (String.length s - 7)
    | _ -> raise Auth.Rejected in
  Auth.authenticate ~read:state.read ~plc:state.config.plc
    ~actor:state.config.owner ~audience:("did:web:" ^ state.config.hostname)
    ~meth ~now:(Eio.Time.now state.system#clock) token

let check_repo state repo =
  ignore (did repo);
  if repo <> state.config.repo then
    reject Status.Forbidden "Forbidden" "repository is not configured"

let check_workflows state names =
  if List.exists (fun name -> name <> state.config.job.name) names then
    invalid "unknown workflow"

let find state id =
  if not (Atp.Tid.is_valid id) then invalid "pipeline must be a TID";
  match Hashtbl.find_opt state.pipelines id with
  | Some p -> p
  | None -> reject Status.Not_found "PipelineNotFound" "pipeline not found"

let query req k =
  match List.filter (fun (name, _) -> name = k) (Req.query req) with
  | [] -> None | [_, value] -> Some value
  | _ -> invalid ("duplicate query parameter " ^ k)

let query_required req k =
  match query req k with Some v -> v | None -> invalid (k ^ " is required")

let query_list req k =
  List.filter_map (fun (name, value) -> if name = k then Some value else None)
    (Req.query req)

let create state actor request =
  check_repo state (get "repo" request);
  check_workflows state (strings "workflows" request);
  let trigger = required "trigger" request in
  if get "$type" trigger <> "sh.tangled.ci.trigger#manual" then
    invalid "this spindle currently accepts manual triggers only";
  let commit = sha (get "sha" trigger) in
  (match field "ref" trigger with None -> () | Some v -> ignore (string v));
  (match field "sourceRepo" trigger with
   | None -> () | Some v -> check_repo state (string v));
  (match field "inputs" trigger with None -> () | Some inputs ->
    List.iter (fun pair -> ignore (get "key" pair); ignore (get "value" pair))
      (list inputs));
  if Hashtbl.length state.pipelines >= 1000 then
    reject Status.Service_unavailable "CapacityExceeded" "history is full";
  let active = Hashtbl.fold (fun _ p n -> if terminal p then n else n + 1)
      state.pipelines 0 in
  if active >= 32 then
    reject Status.Service_unavailable "CapacityExceeded" "job queue is full";
  let timestamp = Int64.of_float (Eio.Time.now state.system#clock *. 1e6) in
  let timestamp = Int64.max timestamp (Int64.succ state.last_tid) in
  state.last_tid <- timestamp;
  let id = Atp.Tid.to_string (Atp.Tid.of_timestamp_us ~clockid:0 timestamp) in
  let created = now () in
  let metadata = obj ["pipeline", str id; "actor", str actor;
    "spindle", str state.config.hostname; "receivedAt", str created;
    "request", request] in
  let p = { id; request; metadata; commit; created; status = "pending";
    started = None; finished = None; error = None; events = []; log_bytes = 0;
    cancel = None; cancelled = false } in
  Hashtbl.add state.pipelines id p;
  (try persist state p with exn ->
    Hashtbl.remove state.pipelines id; raise exn);
  Eio.Fiber.fork ~sw:state.sw (fun () ->
    try execute state p with exn ->
      Eio.Cancel.protect (fun () ->
        p.status <- "failed"; p.finished <- Some (now ());
        p.error <- Some (Printexc.to_string exn); persist state p));
  obj ["pipeline", str ("at://did:web:" ^ state.config.hostname ^
    "/sh.tangled.pipeline/" ^ id)]

let query_pipelines state req =
  check_repo state (query_required req "repo");
  let limit = match query req "limit" with None -> 50 | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 1 && n <= 250 -> n
     | _ -> invalid "limit must be between 1 and 250") in
  let cursor = query req "cursor" in
  Option.iter (fun id -> if not (Atp.Tid.is_valid id) then
    invalid "invalid cursor") cursor;
  let kinds = query_list req "kinds" in
  if List.exists (fun k -> not (List.mem k ["manual"; "push";
      "pull_request"])) kinds then invalid "invalid trigger kind";
  let commits = query_list req "commits" in
  let all = Hashtbl.fold (fun _ p acc ->
    if (kinds = [] || List.mem "manual" kinds) &&
       (commits = [] || List.mem p.commit commits) then p :: acc else acc)
      state.pipelines []
    |> List.sort (fun a b -> String.compare b.id a.id) in
  let seen = Hashtbl.create 16 in
  let all = if commits = [] then all else List.filter (fun p ->
    if Hashtbl.mem seen p.commit then false
    else (Hashtbl.add seen p.commit (); true)) all in
  let total = List.length all in
  let remaining = List.filter (fun p -> match cursor with
    | None -> true | Some cursor -> String.compare p.id cursor < 0) all in
  let page = List.filteri (fun i _ -> i < limit) remaining in
  let next = if List.length remaining > limit then
      optional "cursor" str (Option.map (fun p -> p.id)
        (List.nth_opt page (limit - 1))) else [] in
  obj (["total", int total; "pipelines", arr (List.map (view state) page)]
    @ next)

let frame event =
  let kind = get "type" event in
  let body = obj (List.remove_assoc "type" (members event)) in
  let cbor j = match Atp.Dagcbor.of_json j with
    | Ok v -> Atp.Dagcbor.encode_string v | Error e -> failwith e in
  Bytes.of_string (cbor (obj ["op", int 1; "t", str ("#" ^ kind)]) ^
    cbor body)

let logs state p socket =
  let module W = Httpz_websocket in
  let mutex = Eio.Mutex.create () in
  let with_write_lock f =
    Eio.Mutex.lock mutex;
    match f () with
    | () -> Eio.Mutex.unlock mutex
    | exception exn -> Eio.Mutex.unlock mutex; raise exn in
  let ws = W.create ~role:Server ~max_message:4096 ~with_write_lock
      ~read:(Body.Socket.read socket)
      ~write:(Body.Socket.write_sub socket) () in
  Eio.Time.with_timeout_exn state.system#clock 75. @@ fun () ->
  Eio.Fiber.first
    (fun () -> while W.receive ws ~f:(fun _ _ ~off:_ ~len:_ -> ()) do () done)
    (fun () ->
      let rec send sent =
        let snapshot = List.rev p.events in
        List.iteri (fun i event -> if i >= sent then (
          let bytes = frame event in
          W.send ws Binary bytes ~off:0 ~len:(Bytes.length bytes))) snapshot;
        if terminal p && List.length p.events = List.length snapshot then
          W.close ws ()
        else (
          Eio.Time.sleep state.system#clock 0.05;
          send (List.length snapshot)) in
      send 0)

let upgrade state req respond =
  if Req.meth req <> Method.Get then invalid "logs require GET";
  let p = find state (query_required req "pipeline") in
  check_workflows state (query_list req "workflows");
  let fields = ref [] in
  Headers.iter (fun _ name value ->
    fields := (Req.globalize name, Req.globalize value) :: !fields)
    (Req.headers req);
  let fields = if Req.connection_upgrade req then
      ("Connection", "Upgrade") :: !fields else !fields in
  match Httpz_websocket.Handshake.accept ~meth:"GET"
      ~http_1_1:(Req.version req = Httpz.Version.Http_1_1) fields with
  | Error e -> invalid e
  | Ok fields ->
      let headers = Headers.of_list (List.filter (fun (name, _) ->
        name <> "Connection" && name <> "Upgrade") fields) in
      Resp.upgrade respond ~protocol:"websocket" ~headers (logs state p)

let respond_json respond ?(status = Status.Success)
    ?(headers = Headers.empty) json =
  let text = encode json in
  Resp.v respond ~status ~headers
    ~content_type:(This "application/json") (Body.String text)

let handle state name req respond =
  try
    if name = "sh.tangled.ci.subscribePipelineLogs" then
      upgrade state req respond
    else (
      let post = name = "sh.tangled.ci.triggerPipeline" ||
        name = "sh.tangled.ci.cancelPipeline" in
      if (post && Req.meth req <> Method.Post) ||
         ((not post) && Req.meth req <> Method.Get &&
          Req.meth req <> Method.Head) then
        reject Status.Method_not_allowed "InvalidRequest" "wrong HTTP method";
      let result = match name with
        | "_health" -> obj ["status", str "ok"]
        | "sh.tangled.owner" -> obj ["owner", str state.config.owner]
        | "sh.tangled.ci.getPipeline" ->
            view state (find state (query_required req "pipeline"))
        | "sh.tangled.ci.queryPipelines" -> query_pipelines state req
        | "sh.tangled.ci.describeWorkflowDefinition" ->
            check_repo state (query_required req "repo");
            ignore (sha (query_required req "sha"));
            Option.iter (check_repo state) (query req "sourceRepo");
            obj ["derived", bool false;
              "workflows", arr [str state.config.job.name]]
        | "sh.tangled.ci.triggerPipeline" ->
            let actor = authorize state name req in
            create state actor (decode (Req.globalize (Req.body req)))
        | "sh.tangled.ci.cancelPipeline" ->
            ignore (authorize state name req);
            let body = decode (Req.globalize (Req.body req)) in
            check_repo state (get "repo" body);
            check_workflows state (strings "workflows" body);
            let p = find state (get "pipeline" body) in
            if not (terminal p) then (
              p.cancelled <- true;
              match p.cancel with
              | Some cancel -> cancel ()
              | None -> p.status <- "cancelled";
                  p.finished <- Some (now ()); persist state p);
            obj []
        | _ -> reject Status.Not_found "MethodNotFound" "unknown XRPC method"
      in
      respond_json respond result)
  with
  | Invalid message -> respond_json respond ~status:Status.Bad_request
      (obj ["error", str "InvalidRequest"; "message", str message])
  | Auth.Rejected -> respond_json respond ~status:Status.Unauthorized
      ~headers:(Headers.of_list ["WWW-Authenticate", "Bearer"])
      (obj ["error", str "AuthRequired";
        "message", str "valid owner service-auth token required"])
  | Http_error (status, code, message) ->
      let headers = if status <> Status.Method_not_allowed then Headers.empty
        else Headers.of_list ["Allow",
          (if name = "sh.tangled.ci.triggerPipeline" ||
              name = "sh.tangled.ci.cancelPipeline" then "POST"
           else "GET, HEAD")] in
      respond_json respond ~status ~headers
        (obj ["error", str code; "message", str message])

let load state =
  let open Eio.Path in
  let files = read_dir state.directory
    |> List.filter (fun name -> Filename.check_suffix name ".json") in
  if List.length files > 1000 then failwith "spindle history exceeds 1000 jobs";
  List.iter (fun file ->
    let id = Filename.remove_extension file in
    if not (Atp.Tid.is_valid id) then failwith "invalid state filename";
    let data = decode (load (state.directory / file)) in
    let v = required "pipeline" data in
    check_repo state (get "repo" v);
    let workflow = match list (required "workflows" v) with
      | [w] when get "name" w = state.config.job.name -> w
      | _ -> failwith "stored workflow differs from configuration" in
    let events = list (required "events" data) in
    let p = { id; request = required "request" data;
      metadata = required "metadata" data; commit = get "commit" v;
      created = get "createdAt" v; status = get "status" workflow;
      started = Option.map string (field "startedAt" workflow);
      finished = Option.map string (field "finishedAt" workflow);
      error = Option.map string (field "error" workflow);
      events = List.rev events;
      log_bytes = List.fold_left (fun n j -> n + String.length (encode j))
        0 events; cancel = None; cancelled = false } in
    if not (terminal p) then (
      p.status <- "failed"; p.error <- Some "spindle restarted during job";
      p.finished <- Some (now ()); persist state p);
    state.last_tid <- Int64.max state.last_tid
      (Atp.Tid.timestamp_us (Atp.Tid.of_string id));
    Hashtbl.add state.pipelines id p;
    let work = state.directory / (id ^ ".work") in
    Eio.Process.run state.system#process_mgr
      ["rm"; "-rf"; "--"; native_exn work]) files

let run ?(addr = "127.0.0.1") system config =
  ignore (did config.owner); ignore (did config.repo);
  if not (String.starts_with ~prefix:"did:plc:" config.owner) then
    invalid "the initial spindle requires a PLC owner DID";
  let state_dir = if Filename.is_relative config.state_dir then
      Filename.concat (Sys.getcwd ()) config.state_dir else config.state_dir in
  Eio.Switch.run @@ fun sw ->
  let directory = Eio.Path.(system#fs / state_dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 directory;
  let lock = Unix.openfile
      Eio.Path.(native_exn (directory / ".lock"))
      [Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close lock) @@ fun () ->
  Unix.lockf lock Unix.F_TLOCK 0;
  let fetch = Fetch_httpz.v system#net ~clock:system#mono_clock
      ~max_response:65536 () |> Fetch.restrict ~under:[config.plc] in
  let state = { config; system; sw; directory;
    read = (fun url -> Fetch.read ~limit:65536 fetch url);
    pipelines = Hashtbl.create 32; slots = Eio.Semaphore.make 2;
    persistence = Eio.Mutex.create ();
    last_tid = 0L } in
  load state;
  let open Route in
  let handler name env req respond = env (Req.globalize name) req respond in
  let site = Site.of_routes [
    get (s "xrpc" / str) handler;
    post (s "xrpc" / str) handler;
    get root (fun env req respond -> env "_health" req respond)] in
  let ip = Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string addr) in
  Proffer_httpz.run ~sw ~addr:(`Tcp (ip, config.port)) system
    ~env:(handle state) site
