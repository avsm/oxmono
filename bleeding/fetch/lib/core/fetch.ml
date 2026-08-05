(* [Middleware] defines the shared types and the extension API, since the
   modules below cannot depend on this one. Everything it holds is
   public, so it is included whole rather than re-exported item by item.
   Its [Url] is the whole internal module, so the shadowing is the
   identity. *)

include Middleware

module Header = Header
module Form = Form
module Credential = Credential
module Retry = Retry
module Middleware = Middleware
module Trace = Eio.Private.Trace

let stream ?length flow =
  Stream { length; flow = (flow :> Eio.Flow.source_ty Eio.Resource.t) }

let parse_url s =
  match Url.of_string s with
  | Ok u -> u
  | Error msg -> raise (err (Invalid_url (Printf.sprintf "%s (%S)" msg s)))

let make_request ?headers ?(body = Empty) ?(sensitive = []) meth url =
  let headers = match headers with Some h -> h | None -> Http.Header.init () in
  { meth; url; headers; body;
    sensitive = List.map String.lowercase_ascii sensitive }

(* {2 Responses} *)

let header h r = Header.get h (headers r)

(* {2 Narrowing} *)

let deny reason =
  Trace.log ("fetch: deny " ^ reason);
  raise (err (Denied reason))

let meth_equal a b = Http.Method.compare a b = 0

let restrict ?under ?methods ?filter inner =
  let under =
    Option.map (Scope.list ~what:"prefix" ~caller:"Fetch.restrict") under
  in
  let check (req : request) =
    (match under with
     | Some ps when not (List.exists (fun s -> Scope.matches s req.url) ps) ->
       deny (Fmt.str "url %a not permitted" Url.pp req.url)
     | _ -> ());
    (match methods with
     | Some ms when not (List.exists (meth_equal req.meth) ms) ->
       deny (Fmt.str "method %s not permitted" (Http.Method.to_string req.meth))
     | _ -> ());
    (match filter with
     | Some f ->
       (match f req with
        | `Allow -> ()
        | `Reject reason -> deny reason)
     | None -> ())
  in
  let inner = handler inner in
  of_handler (fun ~sw req -> check req; inner ~sw req)

let gate_methods ~mode allowed inner =
  let inner = handler inner in
  fun ~sw (req : request) ->
    if List.exists (meth_equal req.meth) allowed then inner ~sw req
    else
      deny (Fmt.str "method %s not permitted by %s"
              (Http.Method.to_string req.meth) mode)

let read_only inner =
  (* RFC 9110 s9.2.1's safe methods, minus TRACE as it reflects credentials. *)
  of_handler (gate_methods ~mode:"a read-only client" [ `GET; `HEAD; `OPTIONS ] inner)

(* {2 Appending} *)

let in_scope scope url =
  match scope with
  | None -> true
  | Some ps -> List.exists (fun s -> Scope.matches s url) ps

let with_headers ?scope ?(mode = `Set) bs inner =
  let scope = Option.map (Scope.list ~caller:"Fetch.with_headers") scope in
  let bindings = Header.to_list bs in
  List.iter (fun (name, _) ->
      if List.mem (String.lowercase_ascii name) sensitive_headers then
        invalid_arg
          (Printf.sprintf
             "Fetch.with_headers: %S is a credential header; use \
              with_credentials (scoped) instead" name))
    bindings;
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      let req =
        if not (in_scope scope req.url) then req
        else (
          let headers =
            List.fold_left (fun acc (name, value) ->
                match mode with
                | `Set -> Http.Header.replace acc name value
                | `Add -> Http.Header.add acc name value
                | `If_absent -> Http.Header.add_unless_exists acc name value
              ) req.headers bindings
          in
          { req with headers }
        )
      in
      inner ~sw req)

let check_credential = function
  | Credential.Bearer _ -> ()
  | Credential.Header (name, _) ->
    if not (is_token name) then
      invalid_arg
        (Printf.sprintf "Fetch.with_credentials: header name %S is not a token"
           name);
    let lname = String.lowercase_ascii name in
    if List.mem lname reserved_headers then
      invalid_arg
        (Printf.sprintf
           "Fetch.with_credentials: header %S is the backend's to set" name);
    if String.equal lname "cookie" then
      invalid_arg
        "Fetch.with_credentials: cookies belong to a jar; see Fetch_cookies"
  | Credential.Query params ->
    List.iter (fun (name, _) ->
        if name = "" then
          invalid_arg "Fetch.with_credentials: a parameter name cannot be empty")
      params

let attach_credential (req : request) = function
  | Credential.Bearer token ->
    { req with
      headers =
        Http.Header.replace req.headers "Authorization" ("Bearer " ^ token ()) }
  | Credential.Header (name, fn) ->
    let lname = String.lowercase_ascii name in
    let sensitive =
      if List.mem lname req.sensitive then req.sensitive
      else lname :: req.sensitive
    in
    { req with
      headers = Http.Header.replace req.headers name (fn req); sensitive }
  | Credential.Query params ->
    { req with url = Url.set_query_params req.url params }

let with_credentials ~scope ?(allow_insecure = false) creds inner =
  let scope = Scope.list ~caller:"Fetch.with_credentials" scope in
  List.iter check_credential creds;
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      let req =
        match creds with
        | [] -> req
        | _ when not (in_scope (Some scope) req.url) -> req
        | _ ->
          if Url.scheme req.url = `Http && not allow_insecure then
            deny (Fmt.str
                    "refusing to send credentials over plaintext http (%s)"
                    (Url.origin req.url));
          List.fold_left attach_credential req creds
      in
      inner ~sw req)

(* Best-effort to discard a small unwanted body (3xx, retried 429/5xx) so
   the backend can reuse the connection. If the body is unexpectedly
   large then stop and the backend cleans up when the switch finishes.
   The bytes read are never looked at, so all fibers (and domains) share
   one scratch buffer; interleaved writes into it are harmless. *)
let drain_buf = Cstruct.create 4096

let drain_body body =
  let limit = 32 * 1024 in
  let rec go n =
    if n < limit then
      match Eio.Flow.single_read body drain_buf with
      | got -> go (n + got)
      | exception End_of_file -> ()
  in
  go 0

(* {2 Flow control} *)

type limit_entry = {
  sem : Eio.Semaphore.t option;
  mutable next_start : Mtime.t option;
  mutable in_flight : int;  (* fibers between checkout and checkin *)
}

let mtime_add_s t s =
  match Mtime.Span.of_float_ns (s *. 1e9) with
  | None -> t
  | Some span -> Option.value (Mtime.add_span t span) ~default:t

let span_to_s span = Mtime.Span.to_float_ns span /. 1e9

let with_limits ~clock ?scope ?min_interval ?max_concurrent inner =
  let scope = Option.map (Scope.list ~caller:"Fetch.with_limits") scope in
  (match max_concurrent with
   | Some n when n < 1 ->
     invalid_arg "Fetch.with_limits: max_concurrent must be at least 1"
   | _ -> ());
  (match min_interval with
   | Some s when not (Float.is_finite s) || s < 0. ->
     invalid_arg "Fetch.with_limits: min_interval must be non-negative"
   | Some s when Mtime.Span.of_float_ns (s *. 1e9) = None ->
     invalid_arg "Fetch.with_limits: min_interval is too large to represent"
   | _ -> ());
  let mutex = Eio.Mutex.create () in
  let entries : (string, limit_entry) Hashtbl.t = Hashtbl.create 8 in
  let sweep_threshold = 64 in
  let sweep () =
    if Hashtbl.length entries > sweep_threshold then (
      let now = Eio.Time.Mono.now clock in
      Hashtbl.filter_map_inplace
        (fun _ e ->
           let idle =
             e.in_flight = 0
             && (match e.next_start with
                 | None -> true
                 | Some t -> not (Mtime.is_later t ~than:now))
           in
           if idle then None else Some e)
        entries)
  in
  let checkout origin =
    Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
    sweep ();
    let e =
      match Hashtbl.find_opt entries origin with
      | Some e -> e
      | None ->
        let e = { sem = Option.map Eio.Semaphore.make max_concurrent;
                  next_start = None; in_flight = 0 } in
        Hashtbl.replace entries origin e;
        e
    in
    e.in_flight <- e.in_flight + 1;
    e
  in
  let checkin e =
    Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
    e.in_flight <- e.in_flight - 1
  in
  let pace e =
    match min_interval with
    | None -> ()
    | Some interval ->
      let start =
        Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
        let now = Eio.Time.Mono.now clock in
        let start =
          match e.next_start with
          | Some t when Mtime.is_later t ~than:now -> t
          | _ -> now
        in
        (* Reserve the next slot, so concurrent fibers queue behind us
           rather than stampeding when the interval elapses. *)
        e.next_start <- Some (mtime_add_s start interval);
        start
      in
      let now = Eio.Time.Mono.now clock in
      if Mtime.is_later start ~than:now then
        Trace.log (Fmt.str "fetch: rate-limit wait %.3gs"
                     (span_to_s (Mtime.span start now)));
      Eio.Time.Mono.sleep_until clock start
  in
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      if not (in_scope scope req.url) then inner ~sw req
      else (
        let origin = Url.origin req.url in
        let e = checkout origin in
        Fun.protect ~finally:(fun () -> checkin e) @@ fun () ->
        match e.sem with
        | None -> pace e; inner ~sw req
        | Some sem ->
          if Eio.Semaphore.get_value sem = 0 then
            Trace.log (Fmt.str "fetch: waiting for an in-flight slot (%s)"
                         origin);
          Eio.Semaphore.acquire sem;
          Fun.protect ~finally:(fun () -> Eio.Semaphore.release sem)
            (fun () -> pace e; inner ~sw req)))

let retry_after hs =
  match Header.get Header.retry_after hs with
  | Some (`Seconds s) when s >= 0 -> Some (float_of_int s)
  | _ -> None

(* [rand] returns a uniform draw from [0, 1]. *)
let retry_delay (config : Retry.config) ~attempt ~retry_after ~rand =
  match retry_after with
  | Some s -> Float.min s config.backoff_max
  | None ->
    let d = config.backoff_factor *. (2. ** float_of_int (attempt - 1)) in
    let d = Float.min d config.backoff_max in
    if config.jitter && d > 0. then rand () *. d else d

let with_retry ~clock ~random ?(config = Retry.default) inner =
  let random = (random :> Eio.Flow.source_ty Eio.Resource.t) in
  (* A uniform multiplier in [0, 1]: a random non-negative int, scaled. *)
  let rand () =
    let buf = Cstruct.create 8 in
    Eio.Flow.read_exact random buf;
    float_of_int (Int64.to_int (Cstruct.LE.get_uint64 buf 0) land max_int)
    /. float_of_int max_int
  in
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      let can_retry =
        body_replayable req.body
        && List.exists (meth_equal req.meth) config.Retry.allowed_methods
      in
      let user_says ex =
        match config.Retry.retry_exception with
        | Some f -> f ex
        | None -> false
      in
      let retryable_exn ex =
        match ex with
        | Eio.Cancel.Cancelled _ -> false
        | Eio.Io (E (Denied _ | Tls_failure _ | Body_not_replayable
                    | Invalid_url _ | Invalid_request _), _) -> false
        | Eio.Io (E (Connection_failure _), _) -> true
        | ex -> user_says ex
      in
      let exn_reason ex =
        match ex with
        | Eio.Io (e, _) -> Fmt.str "%a" Eio.Exn.pp_err e
        | ex -> Printexc.to_string ex
      in
      let retryable_resp resp =
        List.mem (status resp) config.Retry.status_forcelist
        || (match config.Retry.retry_response with
            | Some f -> f req resp
            | None -> false)
      in
      let sleep_before ~attempt ~retry_after ~reason =
        let d = retry_delay config ~attempt ~retry_after ~rand in
        Trace.log (Fmt.str "fetch: retry %d/%d in %.3gs (%s)"
                     attempt config.Retry.max_retries d reason);
        if d > 0. then Eio.Time.Mono.sleep clock d
      in
      let rec attempt n =
        match inner ~sw req with
        | resp ->
          if can_retry && n < config.Retry.max_retries && retryable_resp resp then (
            let retry_after =
              if config.Retry.respect_retry_after then
                retry_after (headers resp)
              else None
            in
            drain_body (body resp);
            sleep_before ~attempt:(n + 1) ~retry_after
              ~reason:(Fmt.str "status %d" (status resp));
            attempt (n + 1)
          ) else resp
        | exception ex ->
          if can_retry && n < config.Retry.max_retries && retryable_exn ex then (
            sleep_before ~attempt:(n + 1) ~retry_after:None
              ~reason:(exn_reason ex);
            attempt (n + 1)
          ) else (
            let bt = Printexc.get_raw_backtrace () in
            Printexc.raise_with_backtrace ex bt
          )
      in
      attempt 0)

(* {2 Redirects} *)

let is_redirect = function
  | 301 | 302 | 303 | 307 | 308 -> true
  | _ -> false

let credentials_may_follow ~from ~target =
  Url.same_origin from target
  || (Url.scheme from = `Http
      && Url.scheme target = `Https
      && String.equal (Url.host from) (Url.host target)
      && Url.port from = Url.default_port `Http
      && Url.port target = Url.default_port `Https)

(* RFC 9110 s15. says: 303 always fetches the target with GET
   and historical practice converts 301/302 on POST. *)
let converts_to_get ~status meth =
  match status, meth with
  | 303, m -> not (meth_equal m `HEAD)
  | (301 | 302), `POST -> true
  | _ -> false

let redirect_request ~status ~target (req : request) =
  let headers =
    if credentials_may_follow ~from:req.url ~target then req.headers
    else (
      let stripped =
        sensitive_headers @ List.map String.lowercase_ascii req.sensitive
      in
      if List.exists (Http.Header.mem req.headers) stripped then
        Trace.log "fetch: dropping credential headers on cross-origin redirect";
      List.fold_left Http.Header.remove req.headers stripped)
  in
  if converts_to_get ~status req.meth then
    { meth = `GET;
      url = target;
      body = Empty;
      headers =
        List.fold_left Http.Header.remove headers
          [ "content-type"; "content-encoding"; "content-language";
            "content-location"; "content-digest"; "repr-digest" ];
      sensitive = req.sensitive;
    }
  else (
    if not (body_replayable req.body) then raise (err Body_not_replayable);
    { req with url = target; headers }
  )

let fetch ~sw ?headers:hs ?body:b ?(redirects = 10) ?(allow_downgrade = false)
    ?sensitive t meth url =
  let u = parse_url url in
  let hs = Option.map Header.to_http hs in
  let req = make_request ?headers:hs ?body:b ?sensitive meth u in
  let resolve_target (req : request) location =
    match Url.resolve ~base:req.url location with
    | Ok target ->
      let from_scheme = Url.scheme req.url in
      let to_scheme = Url.scheme target in
      (match from_scheme, to_scheme with
       | `Https, `Http when not allow_downgrade ->
         deny (Fmt.str "redirect would downgrade https to http (%a)" Url.pp target)
       | _ -> ());
      target
    | Error msg ->
      raise (err (Protocol_error (Fmt.str "invalid redirect Location %S: %s" location msg)))
  in
  let hops = ref 0 in
  let rec go req remaining =
    let resp = request ~sw t req in
    if not (is_redirect (status resp)) then resp
    else (
      (* Browsers act on the first Location line when a server sends several.
         Unfortunately Header.get returns the last one. *)
      match Http.Header.get_multi (headers resp) "location" with
      | [] -> resp
      | location :: _ ->
        drain_body (body resp);
        if remaining <= 0 then raise (err Too_many_redirects);
        let target = resolve_target req location in
        if String.equal (Url.to_string target) (Url.to_string req.url)
           && (meth_equal req.meth `GET
               || not (converts_to_get ~status:(status resp) req.meth))
        then (
          Trace.log "fetch: Location resolves to the same request and would loop";
          raise (err Too_many_redirects));
        incr hops;
        go (redirect_request ~status:(status resp) ~target req) (remaining - 1)
    )
  in
  if redirects <= 0 then request ~sw t req
  else
    try go req redirects
    with Eio.Io _ as ex when !hops > 0 ->
      let bt = Printexc.get_raw_backtrace () in
      Eio.Exn.reraise_with_context ex bt
        "fetching %a (%d redirect%s followed)" Url.pp u !hops
        (if !hops = 1 then "" else "s")

let get ~sw ?headers ?redirects t url = fetch ~sw ?headers ?redirects t `GET url
let head ~sw ?headers ?redirects t url = fetch ~sw ?headers ?redirects t `HEAD url
let delete ~sw ?headers ?redirects t url = fetch ~sw ?headers ?redirects t `DELETE url
let options ~sw ?headers ?redirects t url = fetch ~sw ?headers ?redirects t `OPTIONS url

let post ~sw ?headers ?redirects ~body t url = fetch ~sw ?headers ~body ?redirects t `POST url
let put ~sw ?headers ?redirects ~body t url = fetch ~sw ?headers ~body ?redirects t `PUT url
let patch ~sw ?headers ?redirects ~body t url = fetch ~sw ?headers ~body ?redirects t `PATCH url

let with_response ?headers ?body ?redirects ?allow_downgrade ?sensitive t meth url fn =
  Eio.Switch.run @@ fun sw ->
  fn (fetch ~sw ?headers ?body ?redirects ?allow_downgrade ?sensitive t meth url)

let read ?(limit = 16 * 1024 * 1024) t url =
  Eio.Switch.run @@ fun sw ->
  let resp = fetch ~sw t `GET url in
  let buf = Eio.Buf_read.of_flow ~max_size:limit (body resp) in
  Eio.Buf_read.take_all buf
