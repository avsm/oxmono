let src = Logs.Src.create "matrix.client" ~doc:"Matrix client HTTP"

module Log = (val Logs.src_log src : Logs.LOG)

type well_known_policy = Query | Do_not_query

module Url = struct
  module Base = Fetch.Middleware.Url
  include Base

  (* [Httpz_uri] deliberately accepts an empty port, but an authority whose
     colon is followed by non-digits is not a port at all.  At the time of
     writing, the dependency's scanner can reinterpret that suffix as the
     first path segment.  Check the original spelling before it can acquire a
     different meaning. *)
  let check_port_syntax value =
    let length = String.length value in
    let authority_start =
      match String.index_opt value ':' with
      | Some colon
        when colon + 2 < length
             && value.[colon + 1] = '/'
             && value.[colon + 2] = '/' ->
          Some (colon + 3)
      | Some _ | None -> None
    in
    match authority_start with
    | None -> Ok ()
    | Some authority_start -> (
        let rec find_authority_end index =
          if index = length then length
          else
            match value.[index] with
            | '/' | '?' | '#' -> index
            | _ -> find_authority_end (index + 1)
        in
        let authority_end = find_authority_end authority_start in
        let authority =
          String.sub value authority_start (authority_end - authority_start)
        in
        let host_port =
          match String.rindex_opt authority '@' with
          | None -> authority
          | Some at ->
              String.sub authority (at + 1) (String.length authority - at - 1)
        in
        let port =
          if String.starts_with ~prefix:"[" host_port then
            match String.index_opt host_port ']' with
            | None -> None
            | Some close ->
                let suffix =
                  String.sub host_port (close + 1)
                    (String.length host_port - close - 1)
                in
                if suffix = "" then None
                else if suffix.[0] = ':' then
                  Some (String.sub suffix 1 (String.length suffix - 1))
                else Some suffix
          else
            match String.rindex_opt host_port ':' with
            | None -> None
            | Some colon ->
                Some
                  (String.sub host_port (colon + 1)
                     (String.length host_port - colon - 1))
        in
        match port with
        | None -> Ok ()
        | Some "" -> Error "invalid empty port"
        | Some port
          when String.for_all (function '0' .. '9' -> true | _ -> false) port ->
            Ok ()
        | Some port ->
            Error
              (Printf.sprintf "invalid port %S (must contain decimal digits)"
                 port))

  let of_string value =
    match Base.of_string value with
    | Error _ as error -> error
    | Ok url -> Result.map (fun () -> url) (check_port_syntax value)

  let of_uri uri =
    let path = Uriz.path uri in
    if path <> "" && path.[0] <> '/' then
      Error "URL has a malformed authority or path"
    else of_string (Uriz.to_string uri)

  let to_uri url = Uriz.of_string_exn (effective_string url)

  let require_homeserver url =
    if has_query url then Error "homeserver URL must not contain a query"
    else if has_fragment url then
      Error "homeserver URL must not contain a fragment"
    else Ok url

  let homeserver_string value =
    match of_string value with
    | Error _ as error -> error
    | Ok url -> require_homeserver url

  let homeserver uri =
    match of_uri uri with
    | Error _ as error -> error
    | Ok url -> require_homeserver url

  let append_path base ~path ?query () =
    if path = "" || path.[0] <> '/' then
      Error "endpoint path must begin with '/'"
    else if String.exists (function '?' | '#' -> true | _ -> false) path then
      Error "endpoint path must not contain a query or fragment delimiter"
    else if has_query base || has_fragment base then
      Error "base URL must not contain a query or fragment"
    else
      let base = to_string base in
      let base =
        if String.ends_with ~suffix:"/" base then
          String.sub base 0 (String.length base - 1)
        else base
      in
      let value = base ^ path in
      let value =
        match query with
        | None | Some [] -> value
        | Some params ->
            Uriz.to_string
              (Uriz.with_query_params (Uriz.of_string_exn value) params)
      in
      of_string value
end

type config = {
  homeserver : Uriz.t;
  homeserver_url : Url.t;
  user_agent : string option;
  well_known_policy : well_known_policy;
}

type session = {
  user_id : Matrix_proto.Id.User_id.t;
  access_token : string;
  device_id : Matrix_proto.Id.Device_id.t;
  refresh_token : string option;
}

type refreshed_tokens = { access_token : string; refresh_token : string option }

type refreshed_tokens_with_expiry = {
  refreshed_tokens : refreshed_tokens;
  expires_at : Ptime.t option;
}

type refresh_decision =
  | Retry_with of string
  | Refresh_failed of Error.t
  | No_refresh

type sync_presence = [ `Online | `Offline | `Unavailable ]

type presence_cell = {
  mutable value : sync_presence;
  mutable next_wakeup : int;
  mutable wakeups : (int * (unit -> unit)) list;
  mutex : Eio.Mutex.t;
}

let create_presence_cell () =
  {
    value = `Online;
    next_wakeup = 0;
    wakeups = [];
    mutex = Eio.Mutex.create ();
  }

(* Server discovery is deliberately cached here rather than in [Server]: a
   [Client.t] is the unit of credentials, and [Client] cannot depend on the
   endpoint types in [Server] without introducing a module cycle.  The cache
   therefore stores only the successfully decoded response bodies.  It is a
   small mutable cell shared by copies of the same client; concurrent misses
   may race and issue duplicate requests, which is intentional best-effort
   behaviour. *)
module Server_metadata_cache = struct
  type t = {
    mutable versions : string option;
    mutable capabilities : string option;
    mutable oauth_metadata : (float * float option * string) option;
  }

  let create () =
    { versions = None; capabilities = None; oauth_metadata = None }

  let get t = function
    | `Versions -> t.versions
    | `Capabilities -> t.capabilities

  let set t kind body =
    match kind with
    | `Versions -> t.versions <- Some body
    | `Capabilities -> t.capabilities <- Some body

  let clear t =
    t.versions <- None;
    t.capabilities <- None

  let get_oauth_metadata t =
    Option.map
      (fun (fetched_at, _, body) -> (fetched_at, body))
      t.oauth_metadata

  let set_oauth_metadata t ts body = t.oauth_metadata <- Some (ts, None, body)
  let get_oauth_metadata_with_expiry t = t.oauth_metadata

  let set_oauth_metadata_with_expiry t fetched_at expires_at body =
    t.oauth_metadata <- Some (fetched_at, expires_at, body)

  let invalidate_oauth_metadata t = t.oauth_metadata <- None
end

type refresh_state = {
  mutable current : session;
  refresh : session -> (refreshed_tokens_with_expiry, Error.t) result;
  spawn_refresh : ((unit -> unit) -> unit) option;
  on_session_update :
    (session -> Ptime.t option -> (unit, Error.t) result) option;
  mutable expires_at : Ptime.t option;
  early_refresh : Ptime.Span.t;
  now : unit -> Ptime.t;
  mutex : Eio.Mutex.t;
  mutable generation : int;
  mutable last_outcome : (int * refresh_decision) option;
  mutable in_flight :
    (string * (refresh_decision Eio.Promise.t * refresh_decision Eio.Promise.u))
    option;
}

type t = {
  base : Fetch.plain; (* Origin-restricted client carrying no credentials. *)
  fetch : Fetch.plain;
      (* [base] with the session's bearer token attached, when logged in. *)
  origin : string; (* "scheme://host[:port]" of the homeserver. *)
  insecure_origin : bool; (* [true] when the homeserver scheme is http. *)
  homeserver_url : Url.t;
  random : Random.t;
  config : config;
  session : session option;
  auto_refresh : refresh_state option;
  server_metadata_cache : Server_metadata_cache.t;
  sync_presence : presence_cell;
  request_timeout : (Eio.Time.Timeout.t * unit ref) option;
}

(* The timeout is bound to the whole logical operation, rather than to an
   individual transport attempt.  Fiber-local state lets the small raw
   helpers be safely used by the higher-level helpers without restarting the
   timeout when they call one another (or when an automatic refresh replays a
   request). *)
let request_timeout_active = Eio.Fiber.create_key ()
let new_request_timeout_marker () = ref ()

(* Validated here rather than in {!create}, so that building a client cannot
   fail. *)
let config ~homeserver ?user_agent ?(well_known_policy = Query) () =
  let homeserver_url =
    match Url.homeserver homeserver with
    | Ok url -> url
    | Error reason -> invalid_arg ("Matrix_client.Client.config: " ^ reason)
  in
  let homeserver =
    if Url.path_segments homeserver_url = [] then
      Uriz.of_string_exn (Url.origin homeserver_url)
    else Url.to_uri homeserver_url
  in
  { homeserver; homeserver_url; user_agent; well_known_policy }

let create ~(config : config) ~fetch ~random =
  let homeserver_url = config.homeserver_url in
  let origin = Url.origin homeserver_url in
  let insecure_origin = Url.scheme homeserver_url = `Http in
  let base = Fetch.restrict ~under:[ origin ] fetch in
  let base =
    match config.user_agent with
    | None -> base
    | Some ua ->
        Fetch.with_headers ~mode:`If_absent
          Fetch.Header.[ (user_agent, ua) ]
          base
  in
  {
    base;
    fetch = base;
    origin;
    insecure_origin;
    homeserver_url;
    random;
    config;
    session = None;
    auto_refresh = None;
    server_metadata_cache = Server_metadata_cache.create ();
    sync_presence = create_presence_cell ();
    request_timeout = None;
  }

let with_request_timeout ~mono_clock seconds t =
  if Float.is_nan seconds || Float.is_infinite seconds || seconds <= 0. then
    invalid_arg
      "Matrix_client.Client.with_request_timeout: seconds must be finite and \
       positive";
  let marker = new_request_timeout_marker () in
  {
    t with
    request_timeout = Some (Eio.Time.Timeout.seconds mono_clock seconds, marker);
  }

let without_session t =
  {
    t with
    fetch = t.base;
    session = None;
    auto_refresh = None;
    server_metadata_cache = Server_metadata_cache.create ();
  }

let fetch_with_token t access_token =
  let fetch =
    Fetch.with_credentials ~scope:[ t.origin ] ~allow_insecure:t.insecure_origin
      [ Fetch.Credential.Bearer (fun () -> access_token) ]
      t.base
  in
  fetch

let with_access_token t access_token =
  let t = without_session t in
  { t with fetch = fetch_with_token t access_token }

let with_session t (session : session) =
  let authenticated = without_session t in
  let authenticated =
    {
      authenticated with
      fetch = fetch_with_token authenticated session.access_token;
    }
  in
  { authenticated with session = Some session }

let with_auto_refresh ?spawn_refresh ?on_session_update ~refresh t =
  let initial =
    match t.auto_refresh with
    | Some state -> Some state.current
    | None -> t.session
  in
  let initial =
    match initial with
    | Some session -> session
    | None -> invalid_arg "Matrix_client.Client.with_auto_refresh: no session"
  in
  let authenticated = with_session t initial in
  let state =
    {
      current = initial;
      refresh =
        (fun session ->
          Result.map
            (fun refreshed_tokens -> { refreshed_tokens; expires_at = None })
            (refresh session));
      spawn_refresh;
      on_session_update =
        Option.map (fun hook -> fun session _ -> hook session) on_session_update;
      expires_at = None;
      early_refresh = Ptime.Span.zero;
      now = (fun () -> Ptime_clock.now ());
      mutex = Eio.Mutex.create ();
      generation = 0;
      last_outcome = None;
      in_flight = None;
    }
  in
  { authenticated with auto_refresh = Some state }

let with_auto_refresh_expiry ?spawn_refresh ?on_session_update ?expires_at
    ?(early_refresh = Ptime.Span.of_int_s 60)
    ?(now = fun () -> Ptime_clock.now ()) ~refresh t =
  let initial =
    match t.auto_refresh with
    | Some state -> Some state.current
    | None -> t.session
  in
  let initial =
    match initial with
    | Some session -> session
    | None ->
        invalid_arg "Matrix_client.Client.with_auto_refresh_expiry: no session"
  in
  if Ptime.Span.compare early_refresh Ptime.Span.zero < 0 then
    invalid_arg
      "Matrix_client.Client.with_auto_refresh_expiry: negative early_refresh";
  let authenticated = with_session t initial in
  let state =
    {
      current = initial;
      refresh;
      spawn_refresh;
      on_session_update;
      expires_at;
      early_refresh;
      now;
      mutex = Eio.Mutex.create ();
      generation = 0;
      last_outcome = None;
      in_flight = None;
    }
  in
  { authenticated with auto_refresh = Some state }

let session t =
  match t.auto_refresh with
  | Some state -> Eio.Mutex.use_ro state.mutex (fun () -> Some state.current)
  | None -> t.session

let sync_presence t =
  Eio.Mutex.use_ro t.sync_presence.mutex (fun () -> t.sync_presence.value)

let set_sync_presence t value =
  let wakeups =
    Eio.Mutex.use_rw ~protect:true t.sync_presence.mutex (fun () ->
        if t.sync_presence.value = value then []
        else begin
          t.sync_presence.value <- value;
          List.map snd t.sync_presence.wakeups
        end)
  in
  List.iter
    (fun wake ->
      try wake () with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Log.warn (fun m ->
              m "sync-presence wakeup failed: %s" (Printexc.to_string exn)))
    wakeups

let register_presence_wakeup t wake =
  let id =
    Eio.Mutex.use_rw ~protect:true t.sync_presence.mutex (fun () ->
        let id = t.sync_presence.next_wakeup in
        t.sync_presence.next_wakeup <- id + 1;
        t.sync_presence.wakeups <- (id, wake) :: t.sync_presence.wakeups;
        id)
  in
  let removed = ref false in
  fun () ->
    Eio.Mutex.use_rw ~protect:true t.sync_presence.mutex (fun () ->
        if not !removed then begin
          removed := true;
          t.sync_presence.wakeups <-
            List.filter
              (fun (candidate, _) -> candidate <> id)
              t.sync_presence.wakeups
        end)

let homeserver t = t.config.homeserver
let homeserver_url t = t.homeserver_url

let same_origin t uri =
  match Url.of_uri uri with
  | Ok url -> Url.same_origin t.homeserver_url url
  | Error _ -> false

let well_known_policy t = t.config.well_known_policy
let random t = t.random
let server_metadata_cache t = t.server_metadata_cache
let api_base = "/_matrix/client/v3"

let make_url t path query =
  match Url.append_path t.homeserver_url ~path ?query () with
  | Ok url -> url
  | Error reason ->
      invalid_arg ("Matrix_client.Client: invalid request URL: " ^ reason)

let api_url t path query = make_url t (api_base ^ path) query
let absolute_url t path query = Url.to_string (make_url t path query)
let endpoint_uri t ~path ?query () = Url.to_uri (make_url t path query)

(* A query can contain opaque cursors, filters, login hints, or credentials.
   Keep it on the wire but out of this layer's logs and returned transport
   diagnostics.  The input has already passed [Url.of_string]. *)
let diagnostic_url_string url =
  match Url.of_string url with
  | Ok url ->
      Uriz.to_string
        (Uriz.with_userinfo
           (Uriz.with_fragment (Uriz.with_query (Url.to_uri url) Null) Null)
           Null)
  | Error _ -> "<invalid URL>"

(* Used when an internal callback converts an Eio I/O failure into a
   best-effort result. Fetch contexts can contain the complete request URL,
   including query credentials. Keep only the typed cause before attaching our
   safe operation label; there is no re-raise at this boundary. *)
let contextual_io exn label =
  match exn with
  | Eio.Io (error, _) -> Eio.Exn.add_context (Eio.Exn.create error) label
  | _ -> Eio.Exn.add_context exn label

(* Header sets. [Accept] is only asserted on the JSON endpoints so that the
   raw media endpoints are free to answer with any representation. *)

let json_accept = Fetch.Header.[ (accept, [ pref "application/json" ]) ]

let json_headers =
  Fetch.Header.append json_accept
    Fetch.Header.[ (content_type, media "application/json") ]

let content_type_header = Fetch.Header.text "Content-Type"
let cache_control_header = Fetch.Header.text "Cache-Control"
let expires_header = Fetch.Header.expires

(* Responses are read fully inside {!Fetch.with_response}, which closes the
   body when the callback returns. *)
let max_response_body = 64 * 1024 * 1024

let read_body response =
  Io_context.with_context "reading HTTP response body" (fun () ->
      let reader =
        Eio.Buf_read.of_flow ~max_size:max_response_body (Fetch.body response)
      in
      Eio.Buf_read.take_all reader)

(* Keep the debug-log policy in one place. Matrix responses can carry private
   crypto material in arbitrarily nested objects and arrays, so member names
   are matched at every depth. Redacting the value while retaining its member
   name and the surrounding shape keeps useful diagnostics without exposing
   credentials, sessions, pickles or recovery material. The broad [key] and
   [keys] entries are intentional: a debug log must not become a secret
   exfiltration path when a new endpoint chooses a shorter field name. *)
module Redaction = struct
  module Members = Set.Make (String)

  let sensitive_members =
    Members.of_list
      [
        (* Credentials and login proofs. *)
        "password";
        "new_password";
        "access_token";
        "refresh_token";
        "token";
        "client_secret";
        "response";
        "id_access_token";
        (* E2EE sessions, key material and secret-storage values. *)
        "session_key";
        "session_data";
        "recovery_key";
        "secret";
        "secrets";
        "key";
        "keys";
        "pickle";
        "pickle_key";
        "device_pickle";
        "dehydrated_pickle";
        "dehydrated_pickle_key";
        "private_key";
        "master_key";
        "self_signing_key";
        "user_signing_key";
        "backup_key";
        "room_key";
        "seed";
        "seeds";
        "decryption_key";
        "iv";
        "mac";
      ]

  let is_sensitive_member name = Members.mem name sensitive_members

  let rec json : Jsont.json -> Jsont.json = function
    | Jsont.Object (mems, meta) ->
        Jsont.Object
          ( List.map
              (fun (((name, _) as n), v) ->
                if is_sensitive_member name then
                  (n, Jsont.String ("<redacted>", Jsont.Meta.none))
                else (n, json v))
              mems,
            meta )
    | Jsont.Array (l, meta) -> Jsont.Array (List.map json l, meta)
    | (Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _) as v -> v
end

let redact_json = Redaction.json

(* Run HTTPz's bounded generic decoder first.  Besides checking every value in
   the document, doing this before the duplicate-member token pass prevents an
   attacker from making that pass retain an unbounded nesting stack.  The
   token pass then rejects ambiguity before the final typed Jsont decoder can
   apply its last-member-wins behaviour.  Keep every untrusted response-body
   decode on this path. *)
let decode_json jsont body =
  let decode codec =
    match Fetch.Json.decode_string' codec body with
    | Ok value -> Ok value
    | Error error -> Error (Jsont.Error.to_string error)
  in
  match decode Matrix_proto.Json.Codec.json with
  | Error _ as error -> error
  | Ok _ -> (
      match Matrix_proto.Json.Codec.validate_text body with
      | Error message -> Error message
      | Ok () -> decode jsont)

(* A body logged for debugging, with every secret member redacted. A body
   that is not JSON, such as a media upload, is not logged at all rather
   than logged verbatim, since there is no member name to redact by. *)
let redacted_body body =
  match decode_json Matrix_proto.Json.Codec.json body with
  | Ok json -> (
      match
        Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json
          (redact_json json)
      with
      | Ok s -> s
      | Error _ -> "<unloggable>")
  | Error _ -> "<non-JSON body, not logged>"

(* [log_body] is [false] for the raw endpoints, whose successful bodies are
   arbitrary binary. An error body is Matrix's JSON either way. *)
let handle_response ?(log_body = true) status body =
  Log.debug (fun m -> m "Response: status=%d" status);
  if log_body then
    Log.debug (fun m -> m "Response body: %s" (redacted_body body));
  if status >= 200 && status < 300 then Ok body
  else
    (* Already logged above at [Debug]. A non-2xx status is routine here, not
       a fault of this layer: [M_NOT_FOUND] from a state or account data
       lookup is how a caller tests whether something is set, and callers
       that consider their own error unexpected log it themselves. Warning
       here on every such lookup would cry wolf on the normal case. *)
    begin match decode_json Error.matrix_error_jsont body with
    | Ok matrix_err -> Error (Error.Matrix_error matrix_err)
    | Error _ -> Error (Error.Http_error { status; body })
    end

(* [run what f] runs [f], distinguishing a local capability refusal from a
   transport failure. Cancellation is never swallowed. *)
let run what f =
  try f () with
  | Eio.Io (Fetch.E (Fetch.Denied _), _) ->
      (* A policy wrapper may construct its reason from the complete request
         URL. Use our already-redacted operation label at this public result
         boundary so an opaque query or userinfo value cannot escape. *)
      let message = what ^ " is not permitted by HTTP policy" in
      Log.warn (fun m -> m "%s" message);
      Error (Error.Policy_denied message)
  | Eio.Io (Fetch.E (Fetch.Tls_failure reason), _) ->
      Log.err (fun m -> m "%s failed TLS: %s" what reason);
      Error (Error.Tls_error reason)
  | Eio.Io (error, _) ->
      (* HTTPz's context correctly includes the exact request URL.  It is too
         detailed for a result value, however: query values may be secrets.
         Retain the typed cause and add our query-free operation label. *)
      let cause = Eio.Exn.create error in
      let msg = Fmt.str "%s: %a" what Eio.Exn.pp cause in
      Log.err (fun m -> m "%s" msg);
      Error (Error.Network_error msg)
  | Eio.Buf_read.Buffer_limit_exceeded ->
      let msg = Fmt.str "response body exceeds %d bytes" max_response_body in
      Log.err (fun m -> m "%s failed: %s" what msg);
      Error (Error.Network_error msg)

let run_with_request_timeout t f =
  match (Eio.Fiber.get request_timeout_active, t.request_timeout) with
  | Some active, Some (_, marker) when active == marker -> f ()
  | _ -> (
      match t.request_timeout with
      | None -> f ()
      | Some (timeout, marker) -> (
          match
            Eio.Time.Timeout.run timeout (fun () ->
                Ok (Eio.Fiber.with_binding request_timeout_active marker f))
          with
          | Ok result -> result
          | Error `Timeout -> Error (Error.Network_error "request timed out")))

let request_once ~fetch ~name ~meth ~headers ~body url =
  let what = Fmt.str "%s %s" name (diagnostic_url_string url) in
  run what @@ fun () ->
  Log.debug (fun m -> m "%s" what);
  let status, resp_body =
    Fetch.with_response ~headers ~body fetch meth url @@ fun response ->
    (Fetch.status response, read_body response)
  in
  handle_response status resp_body

let is_unknown_token = function
  | Error.Matrix_error { errcode = Error.M_UNKNOWN_TOKEN; _ } -> true
  | _ -> false

let current_session t = session t

let current_access_token t =
  Option.map (fun (s : session) -> s.access_token) (current_session t)

let expiry_due state =
  match state.expires_at with
  | None -> false
  | Some expires_at -> (
      match Ptime.add_span (state.now ()) state.early_refresh with
      | None -> true
      | Some deadline -> Ptime.compare expires_at deadline <= 0)

let apply_refresh state old_session promise resolver =
  let updated = ref None in
  let result =
    try
      match state.refresh old_session with
      | Error error -> Refresh_failed error
      | Ok { refreshed_tokens; expires_at } ->
          let new_session =
            {
              old_session with
              access_token = refreshed_tokens.access_token;
              refresh_token =
                (match refreshed_tokens.refresh_token with
                | Some token -> Some token
                | None -> old_session.refresh_token);
            }
          in
          (* The session pointer, bearer token and deadline are rotated in one
             critical section. The persistence hook runs afterwards, so it can
             never observe a half-updated pair. *)
          Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
              state.current <- new_session;
              state.expires_at <- expires_at);
          updated := Some (new_session, expires_at);
          Retry_with refreshed_tokens.access_token
    with
    | Eio.Cancel.Cancelled _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
            state.in_flight <- None);
        Eio.Promise.resolve resolver
          (Refresh_failed (Error.Network_error "token refresh cancelled"));
        Printexc.raise_with_backtrace exn bt
    | Eio.Io _ as exn ->
        let contextual = contextual_io exn "refreshing access token" in
        let msg = Fmt.str "%a" Eio.Exn.pp contextual in
        Log.warn (fun m -> m "automatic token refresh failed: %s" msg);
        Refresh_failed (Error.Network_error msg)
    | exn ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
            state.in_flight <- None);
        Eio.Promise.resolve resolver
          (Refresh_failed (Error.Network_error "token refresh failed"));
        Printexc.raise_with_backtrace exn bt
  in
  Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
      state.generation <- state.generation + 1;
      state.last_outcome <- Some (state.generation, result);
      state.in_flight <- None);
  Eio.Promise.resolve resolver result;
  (* Resolve the operation before invoking user persistence code. This keeps a
     reentrant hook from waiting on the promise owned by the same refresh
     leader. The session and deadline were already committed together above;
     persistence errors remain best-effort as in [with_auto_refresh]. *)
  Option.iter
    (fun (new_session, expires_at) ->
      Option.iter
        (fun hook ->
          try
            match hook new_session expires_at with
            | Ok () -> ()
            | Error error ->
                Log.warn (fun m ->
                    m "session update persistence failed: %s"
                      (Error.to_string error))
          with
          | Eio.Cancel.Cancelled _ as exn -> (
              let bt = Printexc.get_raw_backtrace () in
              match state.spawn_refresh with
              | None -> Printexc.raise_with_backtrace exn bt
              | Some _ ->
                  (* A runtime-owned refresh has already committed and
                     resolved its shared result.  Do not let cancellation in
                     best-effort persistence fail the client's owning switch;
                     the inline API still propagates caller cancellation. *)
                  Log.warn (fun m ->
                      m "session update persistence was cancelled: %s"
                        (Printexc.to_string exn)))
          | Eio.Io _ as exn ->
              let contextual =
                contextual_io exn "persisting refreshed session"
              in
              Logs.warn (fun m ->
                  m "session update persistence failed: %a" Eio.Exn.pp
                    contextual))
        state.on_session_update)
    !updated;
  result

let start_refresh state old_session promise resolver =
  match state.spawn_refresh with
  | None -> apply_refresh state old_session promise resolver
  | Some spawn ->
      (* The runner owns the whole refresh state transition, including token
         commit and persistence. A cancelled waiter therefore cannot abandon a
         successful rotation. Eio supplies a runner attached to the client's
         long-lived switch; result-only callers retain the inline default. *)
      (try
         spawn (fun () ->
             ignore (apply_refresh state old_session promise resolver))
       with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          let result =
            Refresh_failed (Error.Network_error "token refresh cancelled")
          in
          Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
              state.generation <- state.generation + 1;
              state.last_outcome <- Some (state.generation, result);
              state.in_flight <- None);
          Eio.Promise.resolve resolver result;
          Printexc.raise_with_backtrace exn bt
      | Eio.Io _ as exn ->
          let contextual = contextual_io exn "starting token refresh" in
          let msg = Fmt.str "%a" Eio.Exn.pp contextual in
          let result = Refresh_failed (Error.Network_error msg) in
          Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
              state.generation <- state.generation + 1;
              state.last_outcome <- Some (state.generation, result);
              state.in_flight <- None);
          Eio.Promise.resolve resolver result
      | exn ->
          let bt = Printexc.get_raw_backtrace () in
          let result =
            Refresh_failed (Error.Network_error "token refresh failed")
          in
          Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
              state.generation <- state.generation + 1;
              state.last_outcome <- Some (state.generation, result);
              state.in_flight <- None);
          Eio.Promise.resolve resolver result;
          Printexc.raise_with_backtrace exn bt);
      Eio.Promise.await promise

let refresh_if_due t =
  match t.auto_refresh with
  | None -> No_refresh
  | Some state when not (expiry_due state) -> No_refresh
  | Some state -> (
      let action =
        Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
            if not (expiry_due state) then `No_refresh
            else
              match state.current.refresh_token with
              | None -> `No_refresh
              | Some _ -> (
                  match state.in_flight with
                  | Some (_, (promise, _)) -> `Wait promise
                  | None ->
                      let promise, resolver = Eio.Promise.create () in
                      state.in_flight <-
                        Some (state.current.access_token, (promise, resolver));
                      `Lead (state.current, promise, resolver)))
      in
      match action with
      | `No_refresh -> No_refresh
      | `Wait promise -> Eio.Promise.await promise
      | `Lead (old_session, promise, resolver) ->
          start_refresh state old_session promise resolver)

let refresh_before_request t =
  match refresh_if_due t with
  | Refresh_failed error -> Error error
  | No_refresh | Retry_with _ -> Ok ()

let authenticated_fetch t token =
  match t.auto_refresh with
  | Some _ -> fetch_with_token t token
  | None -> t.fetch

let access_token_and_generation t =
  match t.auto_refresh with
  | Some state ->
      Eio.Mutex.use_ro state.mutex (fun () ->
          (Some state.current.access_token, state.generation))
  | None -> (Option.map (fun (s : session) -> s.access_token) t.session, 0)

(* Return the access token to use after an M_UNKNOWN_TOKEN.  [generation] is
   captured before the initial request. Keeping the completed outcome for one
   generation closes the small race where a concurrent request reaches this
   function just after the leader has cleared [in_flight]. *)
let refresh_after_unknown t failed_token generation =
  match t.auto_refresh with
  | None -> No_refresh
  | Some state -> (
      let action =
        Eio.Mutex.use_rw ~protect:true state.mutex (fun () ->
            let current = state.current in
            if current.access_token <> failed_token then
              `Use current.access_token
            else
              match state.last_outcome with
              | Some (outcome_generation, outcome)
                when outcome_generation > generation ->
                  `Outcome outcome
              | _ -> (
                  match current.refresh_token with
                  | None -> `No_refresh
                  | Some _ -> (
                      match state.in_flight with
                      | Some (_, (promise, _)) -> `Wait promise
                      | None ->
                          let promise, resolver = Eio.Promise.create () in
                          state.in_flight <-
                            Some (failed_token, (promise, resolver));
                          `Lead (current, promise, resolver))))
      in
      match action with
      | `Use token -> Retry_with token
      | `No_refresh -> No_refresh
      | `Outcome outcome -> outcome
      | `Wait promise -> Eio.Promise.await promise
      | `Lead (old_session, promise, resolver) ->
          start_refresh state old_session promise resolver)

let request_authenticated t ~name ~meth ~headers ~body url =
  run_with_request_timeout t (fun () ->
      match refresh_before_request t with
      | Error error -> Error error
      | Ok () -> (
          match access_token_and_generation t with
          | None, _ ->
              request_once ~fetch:t.fetch ~name ~meth ~headers ~body url
          | Some token, generation -> (
              let result =
                request_once
                  ~fetch:(authenticated_fetch t token)
                  ~name ~meth ~headers ~body url
              in
              match result with
              | Error error when is_unknown_token error -> (
                  match refresh_after_unknown t token generation with
                  | Retry_with new_token ->
                      request_once
                        ~fetch:(fetch_with_token t new_token)
                        ~name ~meth ~headers ~body url
                  | No_refresh -> Error error
                  | Refresh_failed refresh_error -> Error refresh_error)
              | result -> result)))

module Http = struct
  type raw_response = { status : int; headers : Http.Header.t; body : string }

  let request_raw_once ~fetch ~name ~meth ~headers ~body url =
    let what = Fmt.str "%s %s" name (diagnostic_url_string url) in
    run what @@ fun () ->
    Log.debug (fun m -> m "%s" what);
    Fetch.with_response ?headers ?body fetch meth url @@ fun response ->
    Ok
      {
        status = Fetch.status response;
        headers = Fetch.headers response;
        body = read_body response;
      }

  let get t ~path ?query () =
    let url = Url.to_string (api_url t path query) in
    request_authenticated t ~name:"GET" ~meth:`GET ~headers:json_accept
      ~body:Fetch.Empty url

  let get_absolute t ~path ?query () =
    let url = absolute_url t path query in
    request_authenticated t ~name:"GET" ~meth:`GET ~headers:json_accept
      ~body:Fetch.Empty url

  let post t ~path ?query ~body () =
    let url = Url.to_string (api_url t path query) in
    Log.debug (fun m -> m "Request body: %s" (redacted_body body));
    request_authenticated t ~name:"POST" ~meth:`POST ~headers:json_headers
      ~body:(Fetch.String body) url

  let post_absolute t ~path ?query ~body () =
    let url = absolute_url t path query in
    Log.debug (fun m -> m "Request body: %s" (redacted_body body));
    request_authenticated t ~name:"POST" ~meth:`POST ~headers:json_headers
      ~body:(Fetch.String body) url

  let post_absolute_unauthenticated t ~path ?query ~body () =
    run_with_request_timeout t (fun () ->
        let url = absolute_url t path query in
        Log.debug (fun m -> m "Request body: %s" (redacted_body body));
        match
          request_raw_once ~fetch:t.base ~name:"POST (unauth)" ~meth:`POST
            ~headers:(Some json_headers) ~body:(Some (Fetch.String body)) url
        with
        | Error _ as error -> error
        | Ok response -> handle_response response.status response.body)

  let put t ~path ?query ~body () =
    let url = Url.to_string (api_url t path query) in
    Log.debug (fun m -> m "Request body: %s" (redacted_body body));
    request_authenticated t ~name:"PUT" ~meth:`PUT ~headers:json_headers
      ~body:(Fetch.String body) url

  let put_absolute t ~path ?query ~body () =
    let url = absolute_url t path query in
    Log.debug (fun m -> m "Request body: %s" (redacted_body body));
    request_authenticated t ~name:"PUT" ~meth:`PUT ~headers:json_headers
      ~body:(Fetch.String body) url

  let delete t ~path ?query ?body () =
    let url = Url.to_string (api_url t path query) in
    let headers, body =
      match body with
      | Some b ->
          Log.debug (fun m -> m "Request body: %s" (redacted_body b));
          (json_headers, Fetch.String b)
      | None -> (json_accept, Fetch.Empty)
    in
    request_authenticated t ~name:"DELETE" ~meth:`DELETE ~headers ~body url

  let delete_absolute t ~path ?query ?body () =
    let url = absolute_url t path query in
    let headers, body =
      match body with
      | Some b ->
          Log.debug (fun m -> m "Request body: %s" (redacted_body b));
          (json_headers, Fetch.String b)
      | None -> (json_accept, Fetch.Empty)
    in
    request_authenticated t ~name:"DELETE" ~meth:`DELETE ~headers ~body url

  let post_unauthenticated t ~path ?query ~body () =
    run_with_request_timeout t (fun () ->
        let url = Url.to_string (api_url t path query) in
        Log.debug (fun m -> m "Request body: %s" (redacted_body body));
        request_once ~fetch:t.base ~name:"POST (unauth)" ~meth:`POST
          ~headers:json_headers ~body:(Fetch.String body) url)

  let unauthenticated_name meth =
    let name =
      match meth with
      | `GET -> "GET (unauth)"
      | `POST -> "POST (unauth)"
      | `PUT -> "PUT (unauth)"
      | `DELETE -> "DELETE (unauth)"
      | `HEAD -> "HEAD (unauth)"
      | `PATCH -> "PATCH (unauth)"
      | `OPTIONS -> "OPTIONS (unauth)"
      | `TRACE -> "TRACE (unauth)"
      | `CONNECT -> "CONNECT (unauth)"
      | `Other meth_name -> meth_name ^ " (unauth)"
    in
    name

  let request_url_unauthenticated t ~meth ~url ?headers ?body () =
    run_with_request_timeout t (fun () ->
        let url = Url.to_string url in
        let name = unauthenticated_name meth in
        request_raw_once ~fetch:t.base ~name ~meth ~headers
          ~body:(Option.map (fun body -> Fetch.String body) body)
          url)

  let request_unauthenticated t ~meth ~path ?query ?headers ?body () =
    request_url_unauthenticated t ~meth ~url:(make_url t path query) ?headers
      ?body ()

  let get_url_with_cache_headers_with fetch url =
    let url = Url.to_string url in
    let what = Fmt.str "GET %s" (diagnostic_url_string url) in
    run what @@ fun () ->
    Log.debug (fun m -> m "%s" what);
    let status, content_type, cache_control, expires, body =
      Fetch.with_response ~body:Fetch.Empty fetch `GET url @@ fun response ->
      ( Fetch.status response,
        Fetch.header content_type_header response,
        Fetch.header cache_control_header response,
        Fetch.header expires_header response,
        read_body response )
    in
    match handle_response ~log_body:false status body with
    | Ok body -> Ok (body, content_type, cache_control, expires)
    | Error _ as e -> e

  let get_url_with_cache_control_with fetch url =
    Result.map
      (fun (body, content_type, cache_control, _expires) ->
        (body, content_type, cache_control))
      (get_url_with_cache_headers_with fetch url)

  let get_bytes_with_cache_control_with fetch t ~path ?query () =
    get_url_with_cache_control_with fetch (make_url t path query)

  let get_bytes_with_cache_headers_with fetch t ~path ?query () =
    get_url_with_cache_headers_with fetch (make_url t path query)

  let get_bytes_with fetch t ~path ?query () =
    Result.map
      (fun (body, content_type, _) -> (body, content_type))
      (get_bytes_with_cache_control_with fetch t ~path ?query ())

  let get_url_with_cache_control t ~url () =
    run_with_request_timeout t (fun () ->
        match refresh_before_request t with
        | Error error -> Error error
        | Ok () -> (
            let token, generation = access_token_and_generation t in
            let fetch =
              match token with
              | None -> t.fetch
              | Some token -> authenticated_fetch t token
            in
            let result = get_url_with_cache_control_with fetch url in
            match (token, result) with
            | Some token, Error error when is_unknown_token error -> (
                match refresh_after_unknown t token generation with
                | Retry_with new_token ->
                    get_url_with_cache_control_with
                      (fetch_with_token t new_token)
                      url
                | No_refresh -> Error error
                | Refresh_failed refresh_error -> Error refresh_error)
            | _ -> result))

  let get_url_with_cache_headers t ~url () =
    run_with_request_timeout t (fun () ->
        match refresh_before_request t with
        | Error error -> Error error
        | Ok () -> (
            let token, generation = access_token_and_generation t in
            let fetch =
              match token with
              | None -> t.fetch
              | Some token -> authenticated_fetch t token
            in
            let result = get_url_with_cache_headers_with fetch url in
            match (token, result) with
            | Some token, Error error when is_unknown_token error -> (
                match refresh_after_unknown t token generation with
                | Retry_with new_token ->
                    get_url_with_cache_headers_with
                      (fetch_with_token t new_token)
                      url
                | No_refresh -> Error error
                | Refresh_failed refresh_error -> Error refresh_error)
            | _ -> result))

  let get_bytes_with_cache_control t ~path ?query () =
    get_url_with_cache_control t ~url:(make_url t path query) ()

  let get_bytes_with_cache_headers t ~path ?query () =
    get_url_with_cache_headers t ~url:(make_url t path query) ()

  let get_bytes t ~path ?query () =
    Result.map
      (fun (body, content_type, _) -> (body, content_type))
      (get_bytes_with_cache_control t ~path ?query ())

  let get_bytes_unauthenticated t ~path ?query () =
    run_with_request_timeout t (fun () ->
        get_bytes_with t.base t ~path ?query ())

  let get_stream_with fetch t ~path ?query
      ~(on_response : content_type:string option -> _ Eio.Flow.source -> unit)
      () =
    let url = Url.to_string (make_url t path query) in
    let what = Fmt.str "GET %s" (diagnostic_url_string url) in
    run what @@ fun () ->
    Log.debug (fun m -> m "%s" what);
    Fetch.with_response ~body:Fetch.Empty fetch `GET url @@ fun response ->
    let status = Fetch.status response in
    let content_type = Fetch.header content_type_header response in
    if status >= 200 && status < 300 then begin
      Log.debug (fun m -> m "Response: status=%d" status);
      Io_context.with_context "consuming HTTP response body" (fun () ->
          on_response ~content_type (Fetch.body response));
      Ok ()
    end
    else begin
      (* Error responses are deliberately buffered, so that the normal
           Matrix/HTTP error mapping can inspect their JSON body. *)
      let body = read_body response in
      handle_response ~log_body:false status body |> Result.map (fun _ -> ())
    end

  let get_stream t ~path ?query ~on_response () =
    run_with_request_timeout t (fun () ->
        match refresh_before_request t with
        | Error error -> Error error
        | Ok () -> (
            let token, generation = access_token_and_generation t in
            let fetch =
              match token with
              | None -> t.fetch
              | Some token -> authenticated_fetch t token
            in
            let result = get_stream_with fetch t ~path ?query ~on_response () in
            match (token, result) with
            | Some token, Error error when is_unknown_token error -> (
                match refresh_after_unknown t token generation with
                | Retry_with new_token ->
                    get_stream_with
                      (fetch_with_token t new_token)
                      t ~path ?query ~on_response ()
                | No_refresh -> Error error
                | Refresh_failed refresh_error -> Error refresh_error)
            | _ -> result))

  let get_stream_unauthenticated t ~path ?query ~on_response () =
    run_with_request_timeout t (fun () ->
        get_stream_with t.base t ~path ?query ~on_response ())

  let post_empty t ~path ?query () =
    let url = Url.to_string (make_url t path query) in
    request_authenticated t ~name:"POST" ~meth:`POST ~headers:json_accept
      ~body:Fetch.Empty url

  let send_url_bytes t ~name ~meth ~url ~content_type ~body () =
    let url = Url.to_string url in
    (* [content_type] is sent verbatim rather than through the typed codec, so
       that a caller's parameters (a charset, a multipart boundary) survive. *)
    let cell = Fetch.Header.raw "Content-Type" content_type in
    request_authenticated t ~name ~meth
      ~headers:(Fetch.Header.append json_accept Fetch.Header.[ cell ])
      ~body:(Fetch.String body) url

  let post_url_bytes t ~url ~content_type ~body () =
    send_url_bytes t ~name:"POST" ~meth:`POST ~url ~content_type ~body ()

  let send_bytes t ~name ~meth ~path ?query ~content_type ~body () =
    send_url_bytes t ~name ~meth ~url:(make_url t path query) ~content_type
      ~body ()

  let post_bytes t ~path ?query ~content_type ~body () =
    send_bytes t ~name:"POST" ~meth:`POST ~path ?query ~content_type ~body ()

  let put_bytes t ~path ?query ~content_type ~body () =
    send_bytes t ~name:"PUT" ~meth:`PUT ~path ?query ~content_type ~body ()

  let post_stream t ~path ?query ~content_type ?length ~body () =
    Option.iter
      (fun length ->
        if length < 0L then
          invalid_arg "Matrix_client.Client.Http.post_stream: negative length")
      length;
    run_with_request_timeout t (fun () ->
        let url = Url.to_string (make_url t path query) in
        let cell = Fetch.Header.raw "Content-Type" content_type in
        let fetch =
          match current_access_token t with
          | None -> t.fetch
          | Some token -> authenticated_fetch t token
        in
        request_once ~fetch ~name:"POST" ~meth:`POST
          ~headers:(Fetch.Header.append json_accept Fetch.Header.[ cell ])
          ~body:(Fetch.stream ?length body)
          url)

  let decode_response jsont body =
    match decode_json jsont body with
    | Ok v -> Ok v
    | Error e ->
        Log.err (fun m -> m "JSON decode error: %s" e);
        Error (Error.Json_error e)

  let encode_body jsont value =
    match Jsont_bytesrw.encode_string jsont value with
    | Ok s -> Ok s
    | Error e ->
        Log.err (fun m -> m "JSON encode error: %s" e);
        Error (Error.Json_error e)
end
