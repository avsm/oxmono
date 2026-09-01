include Middleware

module Header = Header
module Form = Form
module Credential = Credential
module Retry = Retry
module Redirect = Redirect
module Middleware = Middleware
module Trace = Eio.Private.Trace

let stream ?length flow =
  (match length with
   | Some length when Int64.compare length 0L < 0 ->
     invalid_arg (Printf.sprintf "Fetch.stream: length %Ld is negative" length)
   | None | Some _ -> ());
  Stream { length; flow = (flow :> Eio.Flow.source_ty Eio.Resource.t) }

exception Idle_timeout of float

module Idle_source = struct
  type t = {
    source : Eio.Flow.source_ty Eio.Resource.t;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    seconds : float;
  }

  let single_read t (buffer @ local) =
    (* Either branch of [Fiber.first] may remain suspended while the other
       runs, so its captured descriptor must be heap-resident. *)
    let buffer = Cstruct.globalize buffer in
    match
      Eio.Fiber.first
        (fun () -> `Read (Eio.Flow.single_read t.source buffer))
        (fun () ->
          Eio.Time.sleep t.clock t.seconds;
          `Timeout)
    with
    | `Read count -> count
    | `Timeout -> raise (Idle_timeout t.seconds)

  let handler =
    Eio.Flow.Pi.source
      (module struct
        type nonrec t = t

        let read_methods = []
        let single_read = single_read
      end)
end

let with_idle_timeout ~clock ~seconds source =
  if not (Float.is_finite seconds && seconds >= 0.) then
    invalid_arg
      "Fetch.with_idle_timeout: seconds must be finite and non-negative";
  Eio.Resource.T
    ( Idle_source.
        {
          source = (source :> Eio.Flow.source_ty Eio.Resource.t);
          clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t);
          seconds;
        },
      Idle_source.handler )

let parse_url s =
  match Url.of_string s with
  | Ok u -> u
  | Error msg -> raise (err (Invalid_url (Printf.sprintf "%s (%S)" msg s)))

let make_request ?headers ?(body = Empty) ?(sensitive = []) meth url =
  let headers = Option.value headers ~default:(Http.Header.init ()) in
  { meth; url; headers; body;
    sensitive = List.map String.lowercase_ascii sensitive;
    sensitive_query = [] }

let header h r = Header.get h (headers r)

let deny reason =
  Trace.log ("fetch: deny " ^ reason);
  raise (err (Denied reason))

let meth_equal a b = Http.Method.compare a b = 0

let in_scope scope url =
  match scope with
  | None -> true
  | Some ps -> List.exists (fun s -> Scope.matches s url) ps

let restrict ?under ?methods ?filter inner =
  let under =
    Option.map (Scope.list ~what:"prefix" ~caller:"Fetch.restrict") under
  in
  let check (req : request) =
    if not (in_scope under req.url) then
      deny (Fmt.str "url %a not permitted" (pp_url req) req.url);
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

let read_only inner =
  (* TRACE is safe in RFC 9110 §9.2.1, but can reflect credentials. *)
  let allowed = [ `GET; `HEAD; `OPTIONS ] in
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      if List.exists (meth_equal req.meth) allowed then inner ~sw req
      else
        deny (Fmt.str "method %s not permitted by a read-only client"
                (Http.Method.to_string req.meth)))

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
  | Credential.Bearer _ | Credential.Basic _ -> ()
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
    (* Reusing the codec's encoder, rather than concatenating "Bearer " and
       the token directly, is what rejects a token that is not a b64token
       (RFC 6750 s2.1): a space in it would otherwise authenticate as a
       different, truncated token than it names. *)
    let name, value =
      try Header.pair Header.authorization (`Bearer (token ()))
      with Invalid_argument _ ->
        deny "Bearer credential is not an RFC 6750 b64token"
    in
    { req with headers = Http.Header.replace req.headers name value }
  | Credential.Basic credentials ->
    let name, value =
      try Header.pair Header.authorization (`Basic (credentials ()))
      with Invalid_argument _ ->
        deny "Basic credentials must be printable ASCII with no colon in the user-id"
    in
    { req with headers = Http.Header.replace req.headers name value }
  | Credential.Header (name, fn) ->
    let lname = String.lowercase_ascii name in
    let sensitive =
      if List.mem lname req.sensitive then req.sensitive
      else lname :: req.sensitive
    in
    { req with
      headers = Http.Header.replace req.headers name (fn req); sensitive }
  | Credential.Query params ->
    (* The names are what a trace redacts: the values are in the URL from
       here on, where every printer would otherwise reach them. *)
    let sensitive_query =
      List.fold_left
        (fun acc (name, _) ->
           if List.mem name acc then acc else name :: acc)
        req.sensitive_query params
    in
    { req with url = Url.set_query_params req.url params; sensitive_query }

let redirect_scope_key = Eio.Fiber.create_key ()

let redirect_scope () =
  Option.value (Eio.Fiber.get redirect_scope_key) ~default:[]

let with_credentials ~scope ?(allow_insecure = false) ?(extend = false) creds
    inner =
  let scopes = Scope.list ~caller:"Fetch.with_credentials" scope in
  let scope = List.map Scope.to_string scopes in
  List.iter check_credential creds;
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      let caller_url = req.url in
      (* Classification is independent of attachment: even an out-of-scope
         caller value must not escape on a later redirect. *)
      let sensitive = List.fold_left (fun names -> function
        | Credential.Header (name, _) ->
            let name = String.lowercase_ascii name in
            if List.mem name names then names else name :: names
        | _ -> names) req.sensitive creds in
      let req = { req with sensitive } in
      let redirect_scope = if extend then redirect_scope () else [] in
      let extended =
        List.exists
             (fun origin -> Url.same_origin origin req.url)
             redirect_scope
      in
      let req =
        if
          creds = []
          || not
               (extended
                || List.exists (fun s -> Scope.matches s req.url) scopes)
        then req
        else (
          if Url.scheme req.url = `Http && not allow_insecure then
            deny (Fmt.str
                    "refusing to send credentials over plaintext http (%s)"
                    (Url.origin req.url));
          List.fold_left attach_credential req creds)
      in
      let response = inner ~sw req in
      let scope =
        if not extend then scope
        else
          List.fold_left
            (fun scope url ->
              let origin = Url.origin url in
              if List.mem origin scope then scope else scope @ [ origin ])
            scope redirect_scope
      in
      let scope =
        List.fold_left
          (fun scope entry ->
            if List.mem entry scope then scope else scope @ [ entry ])
          (Middleware.scope response) scope
      in
      Middleware.Pi.with_metadata ~url:caller_url ~scope ~sensitive response)

type limit_entry = {
  sem : Eio.Semaphore.t option;
  pace_mutex : Eio.Mutex.t;
  mutable next_start : Mtime.t option;
  mutable in_flight : int;
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
                  pace_mutex = Eio.Mutex.create (); next_start = None; in_flight = 0 } in
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
      (* Only the front waiter owns a prospective start. Cancellation releases
         the mutex without reserving time for a request that never started. *)
      Eio.Mutex.lock e.pace_mutex;
      Fun.protect ~finally:(fun () -> Eio.Mutex.unlock e.pace_mutex) @@ fun () ->
      let now = Eio.Time.Mono.now clock in
      let start = match e.next_start with
        | Some t when Mtime.is_later t ~than:now -> t
        | _ -> now
      in
      if Mtime.is_later start ~than:now then
        Trace.log (Fmt.str "fetch: rate-limit wait %.3gs"
                     (span_to_s (Mtime.span start now)));
      Eio.Time.Mono.sleep_until clock start;
      e.next_start <- Some (mtime_add_s (Eio.Time.Mono.now clock) interval)
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

(* [Retry-After] comes in two forms. The delta is self-contained; the
   HTTP-date has to be read against a wall clock, and without one there is
   nothing to subtract it from, so the caller's backoff applies instead. A
   date already past means no wait, not a negative one. *)
let retry_after ?wall hs =
  match Header.get Header.retry_after hs with
  | Some (`Seconds s) when s >= 0 -> Some (float_of_int s)
  | Some (`Date date) ->
    (match wall with
     | None -> None
     | Some clock ->
       let now = Eio.Time.now clock in
       let buf = Bytes.of_string date in
       let i16 = Httpz.Buf_read.i16 in
       let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (Bytes.length buf)) in
       let #(status, at) = Httpz.Date.parse ~now buf span in
       (match status with
        | Httpz.Date.Valid ->
          Some
            (Float.max 0.
               (Stdlib_upstream_compatible.Float_u.to_float at -. now))
        | Httpz.Date.Invalid -> None))
  | _ -> None

let retry_delay (config : Retry.config) ~attempt ~retry_after ~rand =
  match retry_after with
  | Some s -> Float.min s config.backoff_max
  | None ->
    let d = config.backoff_factor *. (2. ** float_of_int (attempt - 1)) in
    let d = Float.min d config.backoff_max in
    if config.jitter && d > 0. then rand () *. d else d

let with_retry ~clock ~random ?wall ?(config = Retry.default) inner =
  Retry.validate config;
  let random = (random :> Eio.Flow.source_ty Eio.Resource.t) in
  let wall =
    Option.map (fun c -> (c :> float Eio.Time.clock_ty Eio.Resource.t)) wall
  in
  let rand () =
    let buf = Cstruct.create 8 in
    Eio.Flow.read_exact random buf;
    float_of_int (Int64.to_int (Cstruct.LE.get_uint64 buf 0) land max_int)
    /. float_of_int max_int
  in
  let inner = handler inner in
  of_handler (fun ~sw (req : request) ->
      let can_retry =
        config.Retry.max_retries > 0
        && body_replayable req.body
        && List.exists (meth_equal req.meth) config.Retry.allowed_methods
        && (match config.Retry.retry_request with
            | Some f -> f req
            | None -> true)
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
                    | Invalid_url _ | Invalid_request _
                    | Decode_failure _), _) ->
          false
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
          (match (if can_retry && n < config.Retry.max_retries && retryable_resp resp then (
            let retry_after =
              let status = status resp in
              if
                config.Retry.respect_retry_after
                && ((status >= 300 && status <= 399) || status = 429 || status = 503)
              then
                retry_after ?wall (headers resp)
              else None
            in
            close resp;
            sleep_before ~attempt:(n + 1) ~retry_after
              ~reason:(Fmt.str "status %d" (status resp));
            attempt (n + 1)
          ) else resp) with
          | result -> result
          | exception ex ->
              let bt = Printexc.get_raw_backtrace () in
              close resp;
              Printexc.raise_with_backtrace ex bt)
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

(* RFC 9110 §15.4: 303 uses GET, as do 301 and 302 after POST by
   historical convention. *)
let converts_to_get ~status meth =
  match status, meth with
  | 303, m -> not (meth_equal m `HEAD)
  | (301 | 302), `POST -> true
  | _ -> false

let redirect_request ~status ~target (req : request) =
  let same_origin = Url.same_origin req.url target in
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
  (* Event-stream positions are scoped to the resource's origin. A redirect
     may still be followed, but a different origin must start without the
     opaque cursor minted by its predecessor. *)
  let headers =
    if same_origin then headers else Http.Header.remove headers "last-event-id"
  in
  if converts_to_get ~status req.meth then
    { meth = `GET;
      url = target;
      body = Empty;
      headers =
        List.fold_left Http.Header.remove headers
          [ "content-type"; "content-encoding"; "content-language";
            "content-location"; "content-digest"; "repr-digest"; "digest";
            "last-modified" ];
      sensitive = req.sensitive;
      sensitive_query = req.sensitive_query;
    }
  else (
    if not (body_replayable req.body) then raise (err Body_not_replayable);
    { req with url = target; headers }
  )

let fetch ~sw ?headers:hs ?body:b ?redirects ?allow_downgrade ?redirect
    ?sensitive t meth url =
  let redirect = Option.value redirect ~default:Redirect.default in
  let max_hops =
    match redirects with
    | Some hops when hops < 0 ->
      invalid_arg "Fetch.fetch: redirects must be non-negative"
    | Some hops -> hops
    | None ->
        if redirect.max_hops < 0 then
          invalid_arg "Fetch.fetch: redirect max_hops must be non-negative";
        redirect.max_hops
  in
  let allow_downgrade =
    Option.value allow_downgrade ~default:redirect.allow_downgrade
  in
  let u = parse_url url in
  let hs = Option.map Header.to_http hs in
  let req = make_request ?headers:hs ?body:b ?sensitive meth u in
  let resolve_target (req : request) location =
    match Url.resolve ~base:req.url location with
    | Ok target -> target
    | Error msg ->
      raise (err (Protocol_error (Fmt.str "invalid redirect Location %S: %s" location msg)))
  in
  let hops = ref 0 in
  let rec go req redirect_scope remaining =
    let resp =
      match redirect_scope with
      | [] -> request ~sw t req
      | _ ->
          Eio.Fiber.with_binding redirect_scope_key redirect_scope (fun () ->
            request ~sw t req)
    in
    let req = { req with sensitive = Middleware.sensitive resp } in
    match (if not (is_redirect (status resp)) then resp
    else (
      (* Location has no list syntax. Choosing either value from a duplicate
         field makes security policy depend on a first/last-wins differential,
         so expose the redirect response without following it. *)
      match Http.Header.get_multi (headers resp) "location" with
      | [] -> resp
      | _ :: _ :: _ ->
        Trace.log "fetch: not following a response with multiple Location fields";
        resp
      | [ location ] ->
        let target = resolve_target req location in
        match redirect.on_hop ~from:req.url ~to_:target resp with
        | Redirect.Stop -> resp
        | (Redirect.Follow | Redirect.Follow_within_scope) as decision ->
            close resp;
            if remaining <= 0 then raise (err Too_many_redirects);
            (match Url.scheme req.url, Url.scheme target with
            | `Https, `Http when not allow_downgrade ->
                deny
                  (Fmt.str "redirect would downgrade https to http (%a)"
                     Url.pp target)
            | _ -> ());
            if
              String.equal (Url.to_string target) (Url.to_string req.url)
              && (meth_equal req.meth `GET
                  || not
                       (converts_to_get ~status:(status resp) req.meth))
            then begin
              Trace.log
                "fetch: Location resolves to the same request and would loop";
              raise (err Too_many_redirects)
            end;
            let next_scope =
              match decision with
              | Redirect.Follow -> redirect_scope
              | Redirect.Follow_within_scope -> target :: redirect_scope
              | Redirect.Stop -> assert false
            in
            incr hops;
            go
              (redirect_request ~status:(status resp) ~target req)
              next_scope (remaining - 1)
    )) with
    | response -> response
    | exception ex ->
        let bt = Printexc.get_raw_backtrace () in
        close resp;
        Printexc.raise_with_backtrace ex bt
  in
  if max_hops <= 0 then request ~sw t req
  else
    try go req [] max_hops
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

let with_response ?headers ?body ?redirects ?allow_downgrade ?redirect
    ?sensitive t meth url fn =
  Eio.Switch.run @@ fun sw ->
  let response = fetch ~sw ?headers ?body ?redirects ?allow_downgrade ?redirect
      ?sensitive t meth url in
  Fun.protect ~finally:(fun () -> close response) (fun () -> fn response)

let read_bounded ~limit flow =
  if limit < 0 then invalid_arg "Fetch: byte limit must be non-negative";
  let out = Buffer.create (min limit 65536) in
  let scratch = Cstruct.create 65536 in
  let rec loop remaining =
    (* One byte of EOF lookahead is separate from the payload quota. *)
    let size = if remaining >= 65536 then 65536 else remaining + 1 in
    match Eio.Flow.single_read flow (Cstruct.sub scratch 0 size) with
    | count ->
        if count > remaining then raise Eio.Buf_read.Buffer_limit_exceeded;
        Buffer.add_string out (Cstruct.to_string ~len:count scratch);
        loop (remaining - count)
    | exception End_of_file -> Buffer.contents out
  in
  loop limit

let read ?(limit = 16 * 1024 * 1024) t url =
  if limit < 0 then invalid_arg "Fetch.read: limit must be non-negative";
  with_response t `GET url (fun resp -> read_bounded ~limit (body resp))

(* Typed bodies. *)

module Media = Httpz.Media
module Json = Httpz.Json
module Markdown = Media_cmarkit

let has_header hs name =
  List.exists
    (fun (n, _) -> String.equal (String.lowercase_ascii n) name)
    (Header.to_list hs)

let with_accept value = function
  | Some hs when has_header hs "accept" -> hs
  | Some hs -> Header.append hs Header.[ raw "Accept" value ]
  | None -> Header.[ raw "Accept" value ]

let encode codec x =
  ( Header.[ raw "Content-Type" (Media.content_type codec) ],
    Middleware.String (Media.encode codec x) )

let decode_failure r media error =
  Eio.Exn.add_context
    (Middleware.err (Decode_failure { media; error }))
    "reading %s" (Middleware.url r)

let content_type r = Http.Header.get (headers r) "content-type"

(* A reader over the body flow. The slice buffer is reused, which the reader
   contract allows, since a slice need only stay valid until the next read. *)
let reader_of_flow ~limit flow =
  let cs = Cstruct.create 65536 in
  let buf = Bytes.create 65536 in
  let total = ref 0 in
  Bytesrw.Bytes.Reader.make @@ fun () ->
  match Eio.Flow.single_read flow cs with
  | n ->
    total := !total + n;
    if !total > limit then raise Eio.Buf_read.Buffer_limit_exceeded;
    Cstruct.blit_to_bytes cs 0 buf 0 n;
    Bytesrw.Bytes.Slice.make buf ~first:0 ~length:n
  | exception End_of_file -> Bytesrw.Bytes.Slice.eod

let default_limit = 16 * 1024 * 1024

let decode ?(limit = default_limit) codec r =
  if limit < 0 then invalid_arg "Fetch.decode: limit must be non-negative";
  let ct = content_type r in
  let media = Media.media_type codec in
  if not (Media.accepts codec ct) then
    raise (decode_failure r media (Media.Unsupported ct));
  match Media.decode_reader codec (reader_of_flow ~limit (body r)) with
  | Ok x -> x
  | Error e -> raise (decode_failure r media e)
  | exception Eio.Buf_read.Buffer_limit_exceeded ->
      raise (decode_failure r media (Media.Too_large limit))

let is_success r =
  let s = status r in
  s >= 200 && s < 300

let get_as ~sw ?headers ?redirects t codec url =
  let headers =
    with_accept (Media.accept_header [ Media.media_type codec ]) headers
  in
  let r = fetch ~sw ~headers ?redirects t `GET url in
  if is_success r then
    Fun.protect ~finally:(fun () -> close r) (fun () -> Ok (decode codec r))
  else Error r

let read_as ?(limit = default_limit) t codec url =
  if limit < 0 then invalid_arg "Fetch.read_as: limit must be non-negative";
  Eio.Switch.run @@ fun sw ->
  let hs = with_accept (Media.accept_header [ Media.media_type codec ]) None in
  let r = fetch ~sw ~headers:hs t `GET url in
  Fun.protect ~finally:(fun () -> close r) @@ fun () ->
  if is_success r then Ok (decode ~limit codec r)
  else begin
    let buffered =
      read_bounded ~limit (body r)
    in
    let trailers = trailers r in
    let url =
      match Url.of_string (Middleware.url r) with
      | Ok u -> u
      | Error _ -> assert false
    in
    Error
      (Middleware.Pi.response ~status:(status r) ~headers:(headers r)
         ~version:(version r) ~body:(Eio.Flow.string_source buffered)
         ~close:(fun () -> ()) ~scope:(scope r) ~sensitive:(Middleware.sensitive r)
         ~trailers:(fun () -> trailers) ~url ())
  end

exception Rejected of response

let expect = function Ok x -> x | Error r -> raise (Rejected r)

let decode_seq ?(max_line = 1024 * 1024) sq r =
  if max_line < 0 || max_line > max_int - 2 then
    invalid_arg "Fetch.decode_seq: max_line must be between 0 and max_int - 2";
  let ct = content_type r in
  let media = Media.seq_media_type sq in
  if not (Media.seq_accepts sq ct) then
    raise (decode_failure r media (Media.Unsupported ct));
  let buf = Eio.Buf_read.of_flow ~max_size:(max_line + 2) (body r) in
  let rec next () =
    if Eio.Buf_read.at_end_of_input buf then Seq.Nil
    else
      let line =
        match Eio.Buf_read.line buf with
        | line -> line
        | exception Eio.Buf_read.Buffer_limit_exceeded ->
            raise (decode_failure r media (Media.Too_large max_line))
      in
      if String.length line > max_line then
        raise (decode_failure r media (Media.Too_large max_line));
      if String.trim line = "" then next ()
      else
        match Media.decode_item sq line with
        | Ok x -> Seq.Cons (x, next)
        | Error e -> raise (decode_failure r media e)
  in
  next

module Sse = struct
  type event = {
    name : string;
    data : string;
    id : string option;
    retry : int option;
  }

  let media_type = "text/event-stream"

  type decoder = {
    response : response;
    flow : Eio.Flow.source_ty Eio.Resource.t;
    input : Cstruct.t;
    line : Buffer.t;
    data : Buffer.t;
    max_event : int;
    mutable input_pos : int;
    mutable input_len : int;
    mutable at_start : bool;
    mutable skip_lf : bool;
    mutable block_bytes : int;
    mutable has_data : bool;
    mutable name : string option;
    mutable id : string option;
    mutable retry : int option;
    mutable last_id : string option;
    mutable reconnect_ms : int option;
    mutable on_block : unit -> unit;
  }

  let invalid_max_event caller max_event =
    if max_event < 1 then
      invalid_arg (caller ^ ": max_event must be at least 1")

  let make_decoder ?(max_event = 1024 * 1024) response =
    invalid_max_event "Fetch.Sse.decode" max_event;
    let ct = content_type response in
    if
      not
        (match ct with
        | Some ct -> Media.matches ~range:media_type ct
        | None -> false)
    then
      raise
        (decode_failure response media_type (Media.Unsupported ct));
    { response;
      flow = body response;
      input = Cstruct.create 65536;
      line = Buffer.create 128;
      data = Buffer.create 256;
      max_event;
      input_pos = 0;
      input_len = 0;
      at_start = true;
      skip_lf = false;
      block_bytes = 0;
      has_data = false;
      name = None;
      id = None;
      retry = None;
      last_id = None;
      reconnect_ms = None;
      on_block = (fun () -> ()) }

  let ensure_input t =
    if t.input_pos < t.input_len then true
    else
      match Eio.Flow.single_read t.flow t.input with
      | n ->
          t.input_pos <- 0;
          t.input_len <- n;
          true
      | exception End_of_file -> false

  let peek t = Cstruct.get_char t.input t.input_pos

  let consume t =
    let c = peek t in
    t.input_pos <- t.input_pos + 1;
    c

  let bump t n =
    if n > t.max_event - t.block_bytes then
      raise
        (decode_failure t.response media_type
           (Media.Too_large t.max_event));
    t.block_bytes <- t.block_bytes + n

  let prepare_line t =
    if t.skip_lf then begin
      t.skip_lf <- false;
      if ensure_input t && Char.equal (peek t) '\n' then
        ignore (consume t)
    end

  (* WHATWG "parsing an event stream" removes one leading U+FEFF, and only
     the first. It holds neither CR nor LF, so it cannot straddle a line:
     taking it off the first line covers a BOM split across reads too. *)
  let strip_bom t line =
    if not t.at_start then line
    else begin
      t.at_start <- false;
      if String.starts_with ~prefix:"\xef\xbb\xbf" line then
        String.sub line 3 (String.length line - 3)
      else line
    end

  let read_line t =
    prepare_line t;
    Buffer.clear t.line;
    let finish newline_bytes =
      let line = strip_bom t (Buffer.contents t.line) in
      (* The empty line is the boundary between blocks, rather than part of
         either block. Count both bytes of CRLF on every field line. *)
      if line <> "" then bump t newline_bytes;
      `Line line
    in
    let rec loop () =
      if not (ensure_input t) then `Eof
      else
        match consume t with
        | '\n' -> finish 1
        | '\r' ->
            let nonempty = Buffer.length t.line > 0 in
            let lf =
              if t.input_pos < t.input_len then
                if Char.equal (peek t) '\n' then begin
                  ignore (consume t);
                  true
                end
                else false
              else if nonempty then
                if ensure_input t && Char.equal (peek t) '\n' then begin
                  ignore (consume t);
                  true
                end
                else false
              else begin
                (* A blank lone CR dispatches immediately. If a split LF
                   follows, [prepare_line] discards it before the next line. *)
                t.skip_lf <- true;
                false
              end
            in
            finish (if lf then 2 else 1)
        | c ->
            bump t 1;
            Buffer.add_char t.line c;
            loop ()
    in
    loop ()

  let field line =
    match String.index_opt line ':' with
    | None -> #(line, "")
    | Some colon ->
        let name = String.sub line 0 colon in
        let first = colon + 1 in
        let first =
          if first < String.length line && Char.equal line.[first] ' ' then
            first + 1
          else first
        in
        #(name, String.sub line first (String.length line - first))

  let decimal s =
    s <> ""
    && String.for_all (function '0' .. '9' -> true | _ -> false) s

  let add_field t line =
    if line <> "" && not (Char.equal line.[0] ':') then
      let #(name, value) = field line in
      match name with
      | "data" ->
          t.has_data <- true;
          Buffer.add_string t.data value;
          Buffer.add_char t.data '\n'
      | "event" -> t.name <- Some value
      | "id" when not (String.contains value '\000') -> t.id <- Some value
      | "retry" when decimal value ->
          t.retry <- int_of_string_opt value
      | _ -> ()

  let reset_block t =
    t.block_bytes <- 0;
    t.has_data <- false;
    t.name <- None;
    t.id <- None;
    t.retry <- None;
    Buffer.clear t.data

  let dispatch t =
    Option.iter (fun id -> t.last_id <- Some id) t.id;
    Option.iter (fun retry -> t.reconnect_ms <- Some retry) t.retry;
    t.on_block ();
    let event =
      if not t.has_data then None
      else
        let data = Buffer.contents t.data in
        let data = String.sub data 0 (String.length data - 1) in
        let name =
          match t.name with
          | None | Some "" -> "message"
          | Some name -> name
        in
        Some { name; data; id = t.last_id; retry = t.retry }
    in
    reset_block t;
    event

  let rec next_event t () =
    match read_line t with
    | `Eof -> Seq.Nil
    | `Line "" ->
        (match dispatch t with
        | Some event -> Seq.Cons (event, next_event t)
        | None -> next_event t ())
    | `Line line ->
        add_field t line;
        next_event t ()

  let decode ?max_event response =
    let decoder = make_decoder ?max_event response in
    next_event decoder

  let with_last_event_id id headers =
    match id with
    | None -> headers
    | Some id ->
        let headers =
          Header.to_http headers |> fun hs -> Http.Header.remove hs "Last-Event-ID"
        in
        let headers =
          if Middleware.is_field_value id then
            Http.Header.replace headers "Last-Event-ID" id
          else headers
        in
        Header.of_http headers

  let connect_decoder ~sw ?headers ?last_event_id ?max_event client url =
    let headers = with_accept media_type headers in
    let headers = with_last_event_id last_event_id headers in
    let response = fetch ~sw ~headers client `GET url in
    if is_success response then
      (match make_decoder ?max_event response with
       | decoder -> Ok decoder
       | exception ex -> Middleware.close response; raise ex)
    else Error response

  let connect ~sw ?headers ?last_event_id ?max_event client url =
    match
      connect_decoder ~sw ?headers ?last_event_id ?max_event client url
    with
    | Ok decoder -> Ok (next_event decoder)
    | Error response -> Error response

  type subscription = {
    stream : [ `Event of event | `End ] Eio.Stream.t;
    last_id : string option Atomic.t;
    result : (unit, exn) result Eio.Promise.t;
    result_u : (unit, exn) result Eio.Promise.u;
    cancel : Eio.Cancel.t option Atomic.t;
    closed : bool Atomic.t;
    ended : bool Atomic.t;
    capacity : int;
  }

  exception Closed

  let events t = t.stream
  let last_event_id t = Atomic.get t.last_id
  let result t = t.result

  (* The daemon is the stream's only producer, so an add guarded by the room
     check cannot block. Waiting for room instead would wedge the owning
     switch for good: [Switch.await_idle] waits on daemons through a path
     that cannot be cancelled, so a consumer that stops draining a full
     stream would keep the scope alive forever. *)
  let finish t ~protect result =
    ignore (Eio.Promise.try_resolve t.result_u result);
    if Atomic.compare_and_set t.ended false true then begin
      let add () =
        if Eio.Stream.length t.stream < t.capacity then
          Eio.Stream.add t.stream `End
      in
      if protect then Eio.Cancel.protect add else add ()
    end

  let close t =
    if Atomic.compare_and_set t.closed false true then
      match Atomic.get t.cancel with
      | None -> ()
      | Some cancel -> Eio.Cancel.cancel cancel Closed

  let default_retryable = function
    | Eio.Io
        (E (Connection_failure _ | Protocol_error _ | Too_many_redirects), _)
      ->
        true
    | Rejected response ->
        status response = 429
        || (status response >= 500 && status response <= 599)
    | _ -> false

  let check_backoff name value =
    if not (Float.is_finite value) || value < 0. then
      invalid_arg ("Fetch.Sse.subscribe: " ^ name ^ " must be non-negative")

  let subscribe ~sw ~clock ?headers ?last_event_id
      ?(max_event = 1024 * 1024) ?(backoff_initial = 1.)
      ?(backoff_max = 60.) ?(capacity = 64)
      ?(retryable = default_retryable) client url =
    invalid_max_event "Fetch.Sse.subscribe" max_event;
    check_backoff "backoff_initial" backoff_initial;
    check_backoff "backoff_max" backoff_max;
    if backoff_initial <= 0. || backoff_max < backoff_initial then
      invalid_arg
        "Fetch.Sse.subscribe: backoff must satisfy 0 < initial <= max";
    if capacity < 1 then
      invalid_arg "Fetch.Sse.subscribe: capacity must be at least 1";
    let result, result_u = Eio.Promise.create () in
    let t =
      { stream = Eio.Stream.create capacity;
        last_id = Atomic.make last_event_id;
        result;
        result_u;
        cancel = Atomic.make None;
        closed = Atomic.make false;
        ended = Atomic.make false;
        capacity }
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Cancel.sub @@ fun cancel ->
      Atomic.set t.cancel (Some cancel);
      if Atomic.get t.closed then Eio.Cancel.cancel cancel Closed;
      let reconnect_ms = ref None in
      let next_backoff delay =
        Float.min backoff_max
          (if delay = 0. then backoff_initial else delay *. 2.)
      in
      (* A server's [retry] field is advice from the other end of an
         untrusted connection: honour it only between a floor that keeps a
         hostile or broken server from turning the subscription into a
         reconnection loop, and the caller's own backoff ceiling. *)
      let sleep delay =
        let delay =
          match !reconnect_ms with
          | Some milliseconds ->
              Float.min backoff_max
                (Float.max 0.1 (float_of_int milliseconds /. 1000.))
          | None -> delay
        in
        if delay > 0. then Eio.Time.Mono.sleep clock delay
      in
      let sync (decoder : decoder) =
        Option.iter (fun id -> Atomic.set t.last_id (Some id)) decoder.last_id;
        Option.iter (fun retry -> reconnect_ms := Some retry)
          decoder.reconnect_ms
      in
      let rec loop delay =
        if Atomic.get t.closed then raise Closed;
        let delivered = ref false in
        let decoder = ref None in
        let rec consume seq =
          match seq () with
          | Seq.Nil -> ()
          | Seq.Cons (event, rest) ->
              delivered := true;
              Option.iter sync !decoder;
              Eio.Stream.add t.stream (`Event event);
              if Atomic.get t.closed then raise Closed;
              consume rest
        in
        match
          connect_decoder ~sw ?headers
            ?last_event_id:(Atomic.get t.last_id) ~max_event client url
        with
        | exception ex ->
            if retryable ex then begin
              sleep delay;
              loop (next_backoff delay)
            end
            else raise ex
        | Error response ->
            let ex = Rejected response in
            Middleware.close response;
            if retryable ex then begin
              sleep delay;
              loop (next_backoff delay)
            end
            else raise ex
        | Ok connected ->
            decoder := Some connected;
            connected.on_block <- (fun () -> sync connected);
            (match Fun.protect
                ~finally:(fun () -> Middleware.close connected.response)
                (fun () -> consume (next_event connected)) with
            | () ->
                sync connected;
                let delay = if !delivered then backoff_initial else delay in
                sleep delay;
                loop (next_backoff delay)
            | exception ex ->
                sync connected;
                if retryable ex then begin
                  let delay =
                    if !delivered then backoff_initial else delay
                  in
                  sleep delay;
                  loop (next_backoff delay)
                end
                else raise ex)
      in
      (match loop backoff_initial with
      | () -> assert false
      | exception (Eio.Cancel.Cancelled _ | Closed) ->
          Atomic.set t.cancel None;
          finish t ~protect:true (Ok ())
      | exception ex ->
          Atomic.set t.cancel None;
          finish t ~protect:false (Error ex));
      `Stop_daemon);
    t
end

let encode_seq sq items =
  let pending = ref items in
  let next () =
    match !pending () with
    | Seq.Nil -> None
    | Seq.Cons (x, rest) ->
      pending := rest;
      Some (Media.encode_item sq x)
  in
  let module S = struct
    type t = { mutable chunk : string; mutable off : int }

    let read_methods = []

    let rec single_read t cs =
      if t.off < String.length t.chunk then begin
        let n = min (Cstruct.length cs) (String.length t.chunk - t.off) in
        Cstruct.blit_from_string t.chunk t.off cs 0 n;
        t.off <- t.off + n;
        n
      end
      else
        match next () with
        | None -> raise End_of_file
        | Some s ->
          t.chunk <- s;
          t.off <- 0;
          single_read t cs
  end in
  let flow =
    Eio.Resource.T ({ S.chunk = ""; off = 0 }, Eio.Flow.Pi.source (module S))
  in
  ( Header.[ raw "Content-Type" (Media.seq_content_type sq) ],
    Middleware.Stream { length = None; flow } )
