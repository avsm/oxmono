open Result.Syntax

let src = Logs.Src.create "matrix.oauth" ~doc:"Matrix OAuth 2.0 authentication"

module Log = (val Logs.src_log src : Logs.LOG)
module String_map = Map.MakePortable (String)

(* Nothing below logs a token, a code, a code verifier or a refresh token.
   URLs are logged without their userinfo, query, or fragment for the same
   reason. *)
let redact uri =
  Uriz.to_string
    (Uriz.with_userinfo
       (Uriz.with_query (Uriz.with_fragment uri Null) Null) Null)

let now () = Ptime_clock.now ()

type oauth_error = {
  error : string;
  error_description : string option;
  error_uri : string option;
}

let oauth_error_jsont =
  Jsont.Object.(
    map ~kind:"oauth_error" (fun error error_description error_uri ->
        { error; error_description; error_uri })
    |> mem "error" Matrix_proto.Json.Codec.string ~enc:(fun e -> e.error)
    |> opt_mem "error_description" Matrix_proto.Json.Codec.string ~enc:(fun e ->
        e.error_description)
    |> opt_mem "error_uri" Matrix_proto.Json.Codec.string ~enc:(fun e ->
        e.error_uri)
    |> finish)

let pp_oauth_error fmt e =
  match e.error_description with
  | None -> Format.fprintf fmt "%s" e.error
  | Some d -> Format.fprintf fmt "%s: %s" e.error d

let oauth_error_of_json body =
  match Client.Http.decode_response oauth_error_jsont body with
  | Ok e -> Some e
  | Error _ -> None

let oauth_error_of_error = function
  | Error.Http_error { body; _ } -> oauth_error_of_json body
  | Error.Matrix_error _ | Error.Network_error _ | Error.Policy_denied _
  | Error.Tls_error _ | Error.Json_error _ | Error.No_session | Error.No_content
    ->
      None

let scope_api = "urn:matrix:client:api:*"
let scope_device_prefix = "urn:matrix:client:device:"

(* MSC2967 is still emitted by matrix-rust-sdk (including the pinned
   deployment reference used by this library), and is accepted by deployed
   matrix-authentication-service installations.  Keep the spellings explicit
   rather than silently combining tokens from the two dialects: an
   authorisation server can reject the resulting scope as a whole. *)
let scope_api_unstable = "urn:matrix:org.matrix.msc2967.client:api:*"

let scope_device_prefix_unstable =
  "urn:matrix:org.matrix.msc2967.client:device:"

let scope_device id =
  scope_device_prefix ^ Matrix_proto.Id.Device_id.to_string id

let scope_device_unstable id =
  scope_device_prefix_unstable ^ Matrix_proto.Id.Device_id.to_string id

(* The spec's scope is a space-separated list of tokens. *)
let split_scope scope =
  String.split_on_char ' ' scope |> List.filter (( <> ) "")

let device_id_of_scope scope =
  let prefixes = [ scope_device_prefix; scope_device_prefix_unstable ] in
  let is_device tok prefix =
    String.length tok > String.length prefix
    && String.equal (String.sub tok 0 (String.length prefix)) prefix
  in
  let devices =
    List.filter
      (fun tok -> List.exists (is_device tok) prefixes)
      (split_scope scope)
  in
  match devices with
  | [ tok ] ->
      let prefix = List.find (is_device tok) prefixes in
      let id =
        String.sub tok (String.length prefix)
          (String.length tok - String.length prefix)
      in
      Result.to_option (Matrix_proto.Id.Device_id.of_string id)
  (* "There MUST be exactly one [device] token in the requested scope", so
     zero or several (including one from each dialect) is not a device
     allocation to trust. *)
  | _ -> None

(* Ten characters of [A-Z0-9]: unreserved per RFC 3986, and the length the
   spec calls sufficient for per-user uniqueness. *)
let device_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
let device_id_length = 10

let generate_device_id ~random =
  let n = String.length device_alphabet in
  (* Rejection sampling: bytes at or above [limit] would make the first
     [256 mod n] letters likelier than the rest. *)
  let limit = 256 - (256 mod n) in
  let buf = Buffer.create device_id_length in
  let rec fill rounds =
    if Buffer.length buf < device_id_length then
      if rounds = 0 then
        (* A source that keeps handing back rejected bytes must not spin for
           ever. Sixteen rounds of 16 bytes make this unreachable with any
           real CSPRNG. *)
        while Buffer.length buf < device_id_length do
          Buffer.add_char buf device_alphabet.[0]
        done
      else begin
        String.iter
          (fun c ->
            let b = Char.code c in
            if b < limit && Buffer.length buf < device_id_length then
              Buffer.add_char buf device_alphabet.[b mod n])
          (Random.generate random 16);
        fill (rounds - 1)
      end
  in
  fill 16;
  Matrix_proto.Id.Device_id.of_string_exn (Buffer.contents buf)

type account_action =
  | Profile
  | Devices_list
  | Device_view
  | Device_delete
  | Account_deactivate
  | Cross_signing_reset
  | Other_action of string

let account_action_to_string = function
  | Profile -> "org.matrix.profile"
  | Devices_list -> "org.matrix.devices_list"
  | Device_view -> "org.matrix.device_view"
  | Device_delete -> "org.matrix.device_delete"
  | Account_deactivate -> "org.matrix.account_deactivate"
  | Cross_signing_reset -> "org.matrix.cross_signing_reset"
  | Other_action s -> s

let account_action_spellings = function
  | Profile -> [ "org.matrix.profile" ]
  | Devices_list -> [ "org.matrix.devices_list"; "org.matrix.sessions_list" ]
  | Device_view -> [ "org.matrix.device_view"; "org.matrix.session_view" ]
  | Device_delete -> [ "org.matrix.device_delete"; "org.matrix.session_end" ]
  | Account_deactivate ->
      [ "org.matrix.account_deactivate"; "org.matrix.deactivateaccount" ]
  | Cross_signing_reset -> [ "org.matrix.cross_signing_reset" ]
  | Other_action s -> [ s ]

let account_action_of_string = function
  | "org.matrix.profile" -> Profile
  | "org.matrix.devices_list" -> Devices_list
  | "org.matrix.sessions_list" -> Devices_list
  | "org.matrix.device_view" -> Device_view
  | "org.matrix.session_view" -> Device_view
  | "org.matrix.device_delete" -> Device_delete
  | "org.matrix.session_end" -> Device_delete
  | "org.matrix.account_deactivate" -> Account_deactivate
  | "org.matrix.deactivateaccount" -> Account_deactivate
  | "org.matrix.cross_signing_reset" -> Cross_signing_reset
  | s -> Other_action s

(* An action that names one device, and so takes the [device_id] parameter. *)
let action_takes_device_id = function
  | Device_view | Device_delete -> true
  | Profile | Devices_list | Account_deactivate | Cross_signing_reset
  | Other_action _ ->
      false

(* The authorisation server is normally a different origin from the
   homeserver, and [Client.t] is restricted to the homeserver's. Requests
   therefore go through a caller-supplied [Fetch.t] when there is one, and
   fall back to the client only for a same-origin URL. *)
let max_response_body = 4 * 1024 * 1024

let read_body response =
  Io_context.with_context "reading OAuth HTTP response body" (fun () ->
      let reader =
        Eio.Buf_read.of_flow ~max_size:max_response_body (Fetch.body response)
      in
      Eio.Buf_read.take_all reader)

let run what f =
  try f () with
  | Eio.Io (Fetch.E (Fetch.Denied _), _) ->
      (* Fetch policy reasons can contain the complete request URL. [what] is
         the equivalent operation label with userinfo/query/fragment removed. *)
      let message = what ^ " is not permitted by HTTP policy" in
      Log.warn (fun m -> m "%s" message);
      Error (Error.Policy_denied message)
  | Eio.Io (Fetch.E (Fetch.Tls_failure reason), _) ->
      Log.err (fun m -> m "%s failed TLS: %s" what reason);
      Error (Error.Tls_error reason)
  | Eio.Io (error, _) ->
      (* Fetch context contains the exact URL, including a query supplied by
         OAuth metadata.  Keep the typed cause, but pair it only with [what],
         whose URL is deliberately query-free. *)
      let cause = Eio.Exn.create error in
      let msg = Fmt.str "%s: %a" what Eio.Exn.pp cause in
      Log.err (fun m -> m "%s" msg);
      Error (Error.Network_error msg)
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error
        (Error.Network_error
           (Fmt.str "response body exceeds %d bytes" max_response_body))

(* The authorisation server's endpoints answer errors in the OAuth shape of
   RFC 6749 §5.2, not the Matrix one, so the body is handed back verbatim
   inside {!Error.Http_error} for {!oauth_error_of_error} to read. *)
let handle_status status body =
  if status >= 200 && status < 300 then Ok body
  else begin
    (match oauth_error_of_json body with
    | Some e ->
        Log.warn (fun m -> m "OAuth error %d: %a" status pp_oauth_error e)
    | None -> Log.warn (fun m -> m "HTTP error %d" status));
    Error (Error.Http_error { status; body })
  end

let json_accept = Fetch.Header.[ (accept, [ pref "application/json" ]) ]
let form_content_type = "application/x-www-form-urlencoded"

let url_of_uri uri =
  match Client.Url.of_uri uri with
  | Ok url -> Ok url
  | Error reason ->
      Error
        (Error.Json_error
           (Fmt.str "invalid HTTP URL %S: %s" (redact uri) reason))

let direct http ~name ~meth ~headers ~body url =
  let what = Fmt.str "%s %s" name (redact (Client.Url.to_uri url)) in
  run what @@ fun () ->
  Log.debug (fun m -> m "%s" what);
  let status, body =
    Fetch.with_response ~headers ~body http meth (Client.Url.to_string url)
    @@ fun response -> (Fetch.status response, read_body response)
  in
  handle_status status body

let direct_get_with_cache_control http url =
  let what = Fmt.str "GET %s" (redact (Client.Url.to_uri url)) in
  run what @@ fun () ->
  Log.debug (fun m -> m "%s" what);
  let status, cache_control, expires, body =
    Fetch.with_response ~headers:json_accept ~body:Fetch.Empty http `GET
      (Client.Url.to_string url)
    @@ fun response ->
    ( Fetch.status response,
      Fetch.header (Fetch.Header.text "Cache-Control") response,
      Fetch.header Fetch.Header.expires response,
      read_body response )
  in
  match handle_status status body with
  | Ok body -> Ok (body, cache_control, expires)
  | Error _ as e -> e

let off_origin uri =
  Error.Policy_denied
    (Fmt.str
       "%s is not on the homeserver origin; pass ?http to reach the \
        authorisation server"
       (redact uri))

let http_get_with_cache_control ?http client uri =
  let* url = url_of_uri uri in
  match http with
  | Some h -> direct_get_with_cache_control h url
  | None ->
      if Client.Url.same_origin url (Client.homeserver_url client) then
        let+ body, _content_type, cache_control, expires =
          Client.Http.get_url_with_cache_headers client ~url ()
        in
        (body, cache_control, expires)
      else Error (off_origin uri)

let http_get ?http client uri =
  let+ body, _cache_control, _expires =
    http_get_with_cache_control ?http client uri
  in
  body

let http_post ?http client ~content_type ~body uri =
  let* url = url_of_uri uri in
  match http with
  | Some h ->
      let cell = Fetch.Header.raw "Content-Type" content_type in
      let headers = Fetch.Header.append json_accept Fetch.Header.[ cell ] in
      direct h ~name:"POST" ~meth:`POST ~headers ~body:(Fetch.String body) url
  | None ->
      if Client.Url.same_origin url (Client.homeserver_url client) then
        Client.Http.post_url_bytes client ~url ~content_type ~body ()
      else Error (off_origin uri)

(* A form body built by [Fetch.Form.urlencoded], which percent-encodes both
   names and values and keeps the order given. *)
let form_body params =
  match Fetch.Form.urlencoded params with
  | _headers, Fetch.String s -> Ok s
  | _headers, (Fetch.Empty | Fetch.Stream _) ->
      (* This is an invariant of the current Fetch implementation, but keep
         the boundary typed if that implementation changes.  In particular,
         do not replace this with hand-rolled percent encoding. *)
      Error
        (Error.Json_error
           "Fetch.Form.urlencoded returned a non-string request body")

let post_form ?http client ~params uri =
  let* body = form_body params in
  http_post ?http client ~content_type:form_content_type ~body uri

(* RFC 8414 endpoints must pass the HTTP URL policy, beyond URI syntax. *)
let endpoint_uri =
  Jsont.of_of_string ~kind:"absolute URL" ~enc:Uriz.to_string (fun s ->
      match Client.Url.of_string s with
      | Ok url -> Ok (Client.Url.to_uri url)
      | Error reason -> Error (Printf.sprintf "%S: %s" s reason))

module Metadata = struct
  type t = {
    issuer : Uriz.t;
    authorization_endpoint : Uriz.t;
    token_endpoint : Uriz.t;
    registration_endpoint : Uriz.t option;
    revocation_endpoint : Uriz.t option;
    device_authorization_endpoint : Uriz.t option;
    account_management_uri : Uriz.t option;
    account_management_actions_supported : string list;
    response_types_supported : string list;
    grant_types_supported : string list;
    response_modes_supported : string list;
    code_challenge_methods_supported : string list;
    prompt_values_supported : string list;
    scopes_supported : string list;
  }

  let jsont =
    Jsont.Object.(
      map ~kind:"oauth_server_metadata"
        (fun
          issuer
          authorization_endpoint
          token_endpoint
          registration_endpoint
          revocation_endpoint
          device_authorization_endpoint
          account_management_uri
          account_management_actions_supported
          response_types_supported
          grant_types_supported
          response_modes_supported
          code_challenge_methods_supported
          prompt_values_supported
          scopes_supported
        ->
          {
            issuer;
            authorization_endpoint;
            token_endpoint;
            registration_endpoint;
            revocation_endpoint;
            device_authorization_endpoint;
            account_management_uri;
            account_management_actions_supported;
            response_types_supported;
            grant_types_supported;
            response_modes_supported;
            code_challenge_methods_supported;
            prompt_values_supported;
            scopes_supported;
          })
      |> mem "issuer" endpoint_uri ~enc:(fun t -> t.issuer)
      |> mem "authorization_endpoint" endpoint_uri ~enc:(fun t ->
          t.authorization_endpoint)
      |> mem "token_endpoint" endpoint_uri ~enc:(fun t -> t.token_endpoint)
      |> opt_mem "registration_endpoint" endpoint_uri ~enc:(fun t ->
          t.registration_endpoint)
      |> opt_mem "revocation_endpoint" endpoint_uri ~enc:(fun t ->
          t.revocation_endpoint)
      |> opt_mem "device_authorization_endpoint" endpoint_uri ~enc:(fun t ->
          t.device_authorization_endpoint)
      |> opt_mem "account_management_uri" endpoint_uri ~enc:(fun t ->
          t.account_management_uri)
      |> mem "account_management_actions_supported"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.account_management_actions_supported)
      |> mem "response_types_supported"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.response_types_supported)
      |> mem "grant_types_supported" (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> []) ~enc:(fun t -> t.grant_types_supported)
      (* RFC 8414 §2 makes [query] and [fragment] the default when the
         server says nothing. *)
      |> mem "response_modes_supported"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [ "query"; "fragment" ]) ~enc:(fun t ->
             t.response_modes_supported)
      |> mem "code_challenge_methods_supported"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.code_challenge_methods_supported)
      |> mem "prompt_values_supported"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.prompt_values_supported)
      |> mem "scopes_supported" (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> []) ~enc:(fun t -> t.scopes_supported)
      |> skip_unknown |> finish)

  let v1_path = "/_matrix/client/v1/auth_metadata"

  let unstable_path =
    "/_matrix/client/unstable/org.matrix.msc2965/auth_metadata"

  let openid_configuration_path = "/.well-known/openid-configuration"
  let supports list value = List.mem value list
  let supports_response_type t v = supports t.response_types_supported v
  let supports_grant_type t v = supports t.grant_types_supported v
  let supports_response_mode t v = supports t.response_modes_supported v

  let supports_code_challenge_method t v =
    supports t.code_challenge_methods_supported v

  let supports_prompt t v = supports t.prompt_values_supported v

  let supports_account_action t action =
    List.exists
      (fun spelling -> supports t.account_management_actions_supported spelling)
      (account_action_spellings action)

  let validate_urls ?(allow_insecure = false) t =
    let require ok what =
      if ok then Ok () else Error (Error.Json_error ("OAuth 2.0: " ^ what))
    in
    let validate_url ~issuer label uri =
      match Client.Url.of_uri uri with
      | Error reason ->
          Error
            (Error.Json_error
               (Printf.sprintf "OAuth 2.0: %s is invalid: %s" label reason))
      | Ok url ->
          let acceptable_scheme =
            Client.Url.scheme url = `Https
            || (allow_insecure && Client.Url.scheme url = `Http)
          in
          let* () =
            require acceptable_scheme
              (Printf.sprintf "%s must be an %s absolute URL" label
                 (if allow_insecure then "http or https" else "https"))
          in
          if issuer then
            require
              ((not (Client.Url.has_query url))
              && not (Client.Url.has_fragment url))
              "the issuer URL must not contain a query or fragment"
          else Ok ()
    in
    let* () = validate_url ~issuer:true "the issuer" t.issuer in
    let* () =
      validate_url ~issuer:false "the authorization endpoint"
        t.authorization_endpoint
    in
    let* () =
      validate_url ~issuer:false "the token endpoint" t.token_endpoint
    in
    let optional_url label = function
      | None -> Ok ()
      | Some uri -> validate_url ~issuer:false label uri
    in
    let* () =
      optional_url "the registration endpoint" t.registration_endpoint
    in
    let* () = optional_url "the revocation endpoint" t.revocation_endpoint in
    let* () =
      optional_url "the device authorization endpoint"
        t.device_authorization_endpoint
    in
    let* () =
      optional_url "the account management URI" t.account_management_uri
    in
    Ok ()

  let validate_loopback ?allow_insecure t =
    let require ok what =
      if ok then Ok () else Error (Error.Json_error ("OAuth 2.0: " ^ what))
    in
    let* () = validate_urls ?allow_insecure t in
    let* () =
      require
        (supports_response_type t "code")
        "the authorisation server does not support the \"code\" response type"
    in
    let* () =
      require
        (supports_grant_type t "authorization_code")
        "the authorisation server does not support the \"authorization_code\" \
         grant"
    in
    let* () =
      require
        (supports_grant_type t "refresh_token")
        "the authorisation server does not support the \"refresh_token\" grant"
    in
    let* () =
      require
        (supports_response_mode t "query")
        "the authorisation server does not support the \"query\" response mode"
    in
    require
      (supports_code_challenge_method t "S256")
      "the authorisation server does not support the \"S256\" PKCE method"

  let validate ?allow_insecure t =
    let require ok what =
      if ok then Ok () else Error (Error.Json_error ("OAuth 2.0: " ^ what))
    in
    let* () = validate_loopback ?allow_insecure t in
    require
      (supports_response_mode t "fragment")
      "the authorisation server does not support the \"fragment\" response mode"

  let validate_device ?allow_insecure t =
    let require ok what =
      if ok then Ok () else Error (Error.Json_error ("OAuth 2.0: " ^ what))
    in
    (* RFC 8628 does not use an authorization response, redirect mode or PKCE.
       Check only the common URL rules and the capabilities the Matrix device
       flow actually consumes.  The full Matrix metadata conformance check is
       [validate], but applying it here would couple device login to unrelated
       browser redirect modes. *)
    let* () = validate_urls ?allow_insecure t in
    let* () =
      require
        (Option.is_some t.device_authorization_endpoint)
        "the authorisation server advertises no device_authorization_endpoint"
    in
    let* () =
      require
        (supports_grant_type t "urn:ietf:params:oauth:grant-type:device_code")
        "the authorisation server does not support the \"device_code\" grant"
    in
    require
      (supports_grant_type t "refresh_token")
      "the authorisation server does not support the \"refresh_token\" grant"

  (* A 404, or the Matrix M_UNRECOGNIZED the spec pairs it with, means the
     homeserver does not serve that endpoint, so the next candidate is
     tried. Anything else is a real failure. *)
  let unsupported = function
    | Error.Http_error { status = 404; _ }
    | Error.Matrix_error { errcode = Error.M_UNRECOGNIZED; _ }
    | Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ } ->
        true
    | _ -> false

  let decode body = Client.Http.decode_response jsont body
  let cache_fallback_seconds = 86_400.
  let cache_monotonic_now () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9

  type cache_policy = Do_not_store | Store_for of float

  let http_date_timestamp value =
    let buf = Bytes.of_string value in
    let length = Bytes.length buf in
    if length > 32767 then None else
    let span = Httpz.Span.make ~off:#0S
        ~len:(Stdlib_stable.Int16_u.of_int length) in
    match Httpz.Date.parse buf span with
    | #(Httpz.Date.Valid, timestamp) ->
        Some (Stdlib_upstream_compatible.Float_u.to_float timestamp)
    | #(Httpz.Date.Invalid, _) -> None

  let expires_lifetime = function
    | None -> None
    | Some value -> (
        match http_date_timestamp value with
        | None -> None
        | Some timestamp ->
            Some (Float.max 0. (timestamp -. Unix.gettimeofday ())))

  let cache_policy cache_control expires =
    let expires_or_fallback () =
      match expires_lifetime expires with
      | Some lifetime -> Store_for (min cache_fallback_seconds lifetime)
      | None -> Store_for cache_fallback_seconds
    in
    match cache_control with
    | None -> expires_or_fallback ()
    | Some value -> (
        match Fetch.Header.decode Fetch.Header.cache_control value with
        | None ->
            (* A syntactically invalid field is treated as immediately stale;
               callers must not accidentally retain a response under an
               untrusted cache directive. *)
            Store_for 0.
        | Some directives -> (
            if directives.no_store then Do_not_store
            else if directives.no_cache then Store_for 0.
            else if
              List.exists
                (fun (name, _value) -> String.equal name "max-age")
                directives.extension
            then Store_for 0.
            else
              match directives.max_age with
              | Some seconds ->
                  Store_for
                    (min cache_fallback_seconds
                       (Float.max 0. (float_of_int seconds)))
              | None -> expires_or_fallback ()))

  let cache_success ?(now = cache_monotonic_now) client body cache_control
      expires = function
    | Ok _ as result ->
        let fetched_at = now () in
        (match cache_policy cache_control expires with
        | Do_not_store ->
            (* A successful response with [no-store] must not leave an older
               discovery document available through a later cache lookup. *)
            Client.Server_metadata_cache.invalidate_oauth_metadata
              (Client.server_metadata_cache client)
        | Store_for seconds ->
            Client.Server_metadata_cache.set_oauth_metadata_with_expiry
              (Client.server_metadata_cache client)
              fetched_at
              (Some (fetched_at +. seconds))
              body);
        result
    | Error _ as result -> result

  (* RFC 8414 §3 appends the well-known path to the issuer's own path, and
     issuers are conventionally written with a trailing slash. *)
  let issuer_configuration_url issuer =
    let path =
      let p = Uriz.path issuer in
      let n = String.length p in
      if n > 0 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p
    in
    Uriz.with_path issuer (path ^ openid_configuration_path)

  (* The MSC2965 shape that predates the stable endpoint. The released spec
     dropped it, but servers deployed before Matrix 1.15 are reachable only
     this way. *)
  let fetch_from_well_known ?http ~now client =
    let* well_known = Server.get_well_known client in
    match
      Option.bind well_known (fun (wk : Server.well_known) -> wk.authentication)
    with
    | None ->
        Error
          (Error.Http_error
             {
               status = 404;
               body =
                 "no OAuth 2.0 authorisation server: neither /auth_metadata \
                  nor m.authentication in the well-known";
             })
    | Some auth ->
        let url = issuer_configuration_url (Uriz.of_string_exn auth.issuer) in
        Log.info (fun m -> m "OAuth discovery via issuer %s" auth.issuer);
        let* body, cache_control, expires =
          http_get_with_cache_control ?http client url
        in
        cache_success ~now client body cache_control expires (decode body)

  let fetch ?http ?(now = cache_monotonic_now) client =
    let try_path path =
      match Client.Http.get_bytes_with_cache_headers client ~path () with
      | Ok (body, _content_type, cache_control, expires) ->
          Some
            (cache_success ~now client body cache_control expires (decode body))
      | Error e when unsupported e ->
          Log.debug (fun m -> m "%s not served here" path);
          None
      | Error e -> Some (Error e)
    in
    match try_path v1_path with
    | Some r -> r
    | None -> (
        match try_path unstable_path with
        | Some r -> r
        | None -> fetch_from_well_known ?http ~now client)

  let fetch_cached ?http ?(now = cache_monotonic_now) client =
    match
      Client.Server_metadata_cache.get_oauth_metadata
        (Client.server_metadata_cache client)
    with
    | None -> fetch ?http ~now client
    | Some (fetched_at, body) -> (
        let cached = decode body in
        let fresh =
          match
            Client.Server_metadata_cache.get_oauth_metadata_with_expiry
              (Client.server_metadata_cache client)
          with
          | Some (_, Some expires_at, _) -> now () < expires_at
          | Some (_, None, _) ->
              (* [set_oauth_metadata] predates the monotonic cache clock and
                 uses [0.] in its test/adaptor hook to mean an old entry. *)
              fetched_at > 0.
              && Float.max 0. (now () -. fetched_at) < cache_fallback_seconds
          | None -> false
        in
        if fresh then cached
        else
          (* The Rust SDK returns stale metadata and refreshes it in a spawned
             task.  The result API has no scheduler to own such a task, so do
             the refresh synchronously but retain the same stale-on-failure
             outcome. *)
          match fetch ?http ~now client with
          | Ok _ as refreshed -> refreshed
          | Error _ -> cached)

  let invalidate_cache client =
    Client.Server_metadata_cache.invalidate_oauth_metadata
      (Client.server_metadata_cache client)

  let account_management_url t ?action ?device_id () =
    match t.account_management_uri with
    | None -> None
    | Some uri ->
        let params =
          match action with
          | None -> []
          | Some a ->
              (* Prefer the stable spelling where it is advertised, but use a
                 deployed MSC4191 alias when that is the only spelling the
                 metadata offers.  For an unsupported action retain the
                 historical canonical value, so this helper remains useful
                 for callers that intentionally send an extension. *)
              let action_name =
                match
                  List.find_opt
                    (fun spelling ->
                      supports t.account_management_actions_supported spelling)
                    (account_action_spellings a)
                with
                | Some spelling -> spelling
                | None -> account_action_to_string a
              in
              let action_param = [ ("action", action_name) ] in
              (* The spec says actions that do not name a device ignore the
                 parameter; do not send noise. *)
              if action_takes_device_id a then
                match device_id with
                | None -> action_param
                | Some d ->
                    action_param
                    @ [ ("device_id", Matrix_proto.Id.Device_id.to_string d) ]
              else action_param
        in
        Some (Uriz.add_query_params uri params)
end

module Registration = struct
  type localized = { value : string; translations : (string * string) list }

  let plain value = { value; translations = [] }

  type client_metadata = {
    client_uri : string;
    client_name : localized option;
    logo_uri : localized option;
    tos_uri : localized option;
    policy_uri : localized option;
    contacts : string list;
    redirect_uris : string list;
    response_types : string list;
    grant_types : string list;
    token_endpoint_auth_method : string;
    application_type : string;
    sector_identifier_uri : string option;
    software_id : string option;
    software_version : string option;
    software_statement : string option;
    jwks_uri : string option;
    jwks : Jsont.json option;
  }

  let rec json_member name = function
    | [] -> None
    | ((member_name, _), value) :: rest ->
        if String.equal member_name name then Some value
        else json_member name rest

  let v ?client_name ?logo_uri ?tos_uri ?policy_uri ?(contacts = [])
      ?(response_types = [ "code" ])
      ?(grant_types = [ "authorization_code"; "refresh_token" ])
      ?(token_endpoint_auth_method = "none") ?(application_type = "native")
      ?sector_identifier_uri ?software_id ?software_version ?software_statement
      ?jwks_uri ?jwks ~client_uri ~redirect_uris () =
    if Option.is_some jwks && Option.is_some jwks_uri then
      invalid_arg
        "OAuth registration metadata cannot contain both jwks and jwks_uri";
    let valid_jwks = function
      | Jsont.Object (members, _) -> (
          match json_member "keys" members with
          | Some (Jsont.Array _) -> true
          | _ -> false)
      | _ -> false
    in
    Option.iter
      (fun jwks ->
        if not (valid_jwks jwks) then
          invalid_arg
            "OAuth registration metadata jwks must contain an array-valued \
             keys member")
      jwks;
    {
      client_uri;
      client_name;
      logo_uri;
      tos_uri;
      policy_uri;
      contacts;
      redirect_uris;
      response_types;
      grant_types;
      token_endpoint_auth_method;
      application_type;
      sector_identifier_uri;
      software_id;
      software_version;
      software_statement;
      jwks_uri;
      jwks;
    }

  (* RFC 8252 §7.3: a registered loopback redirect URI carries no port, and
     the server accepts whatever port the client binds at authorisation
     time. *)
  let loopback_redirect_uri = "http://127.0.0.1/callback"
  let mem name json = Jsont.Json.mem (Jsont.Json.name name) json
  let jstring s = Jsont.Json.string s
  let jlist l = Jsont.Json.list (List.map jstring l)

  (* RFC 7591 §2.2: a human-readable value may be repeated as
     [<field>#<language-tag>]. *)
  let localized_members name = function
    | None -> []
    | Some { value; translations } ->
        mem name (jstring value)
        :: List.map
             (fun (tag, v) -> mem (name ^ "#" ^ tag) (jstring v))
             translations

  let to_json m =
    if Option.is_some m.jwks && Option.is_some m.jwks_uri then
      invalid_arg
        "OAuth registration metadata cannot contain both jwks and jwks_uri";
    Option.iter
      (function
        | Jsont.Object (members, _) -> (
            match json_member "keys" members with
            | Some (Jsont.Array _) -> ()
            | _ ->
                invalid_arg
                  "OAuth registration metadata jwks must contain an \
                   array-valued keys member")
        | _ ->
            invalid_arg
              "OAuth registration metadata jwks must contain an array-valued \
               keys member")
      m.jwks;
    let members =
      [ mem "client_uri" (jstring m.client_uri) ]
      @ localized_members "client_name" m.client_name
      @ localized_members "logo_uri" m.logo_uri
      @ localized_members "tos_uri" m.tos_uri
      @ localized_members "policy_uri" m.policy_uri
      @ (if m.contacts = [] then [] else [ mem "contacts" (jlist m.contacts) ])
      @ [
          mem "redirect_uris" (jlist m.redirect_uris);
          mem "response_types" (jlist m.response_types);
          mem "grant_types" (jlist m.grant_types);
          mem "token_endpoint_auth_method"
            (jstring m.token_endpoint_auth_method);
          mem "application_type" (jstring m.application_type);
        ]
      @ (match m.sector_identifier_uri with
        | None -> []
        | Some uri -> [ mem "sector_identifier_uri" (jstring uri) ])
      @ (match m.software_id with
        | None -> []
        | Some id -> [ mem "software_id" (jstring id) ])
      @ (match m.software_version with
        | None -> []
        | Some version -> [ mem "software_version" (jstring version) ])
      @ (match m.software_statement with
        | None -> []
        | Some statement -> [ mem "software_statement" (jstring statement) ])
      @ (match m.jwks_uri with
        | None -> []
        | Some uri -> [ mem "jwks_uri" (jstring uri) ])
      @ match m.jwks with None -> [] | Some jwks -> [ mem "jwks" jwks ]
    in
    Jsont.Json.object' members

  type response = {
    client_id : string;
    client_id_issued_at : Ptime.t option;
    registered : (string * Jsont.json) list;
  }

  (* RFC 7591 §3.2.1 writes [client_id_issued_at] as seconds since the
     epoch. *)
  let posix_seconds =
    Jsont.map ~kind:"posix time"
      ~dec:(fun s ->
        match Ptime.of_float_s (float_of_int s) with
        | Some t -> t
        | None ->
            Jsont.Error.msg Jsont.Meta.none
              (Printf.sprintf "%d is not a representable POSIX time" s))
      ~enc:(fun t -> int_of_float (Ptime.to_float_s t))
      Matrix_proto.Json.Codec.int

  let response_jsont =
    Jsont.Object.(
      map ~kind:"client_registration_response"
        (fun client_id client_id_issued_at registered ->
          {
            client_id;
            client_id_issued_at;
            registered = String_map.bindings registered;
          })
      |> mem "client_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.client_id)
      |> opt_mem "client_id_issued_at" posix_seconds ~enc:(fun t ->
          t.client_id_issued_at)
      (* The server echoes the metadata it actually accepted, dropping what
         it does not support. Keep it so a caller can see what was
         registered. *)
      |> keep_unknown
           (Matrix_proto.Json.Codec.string_map_mems Matrix_proto.Json.Codec.json)
           ~enc:(fun t -> String_map.of_seq (List.to_seq t.registered))
      |> finish)

  let register ?http client (metadata : Metadata.t) cm =
    match metadata.registration_endpoint with
    | None ->
        Error
          (Error.Json_error
             "OAuth 2.0: the authorisation server advertises no \
              registration_endpoint")
    | Some url ->
        let* body =
          Client.Http.encode_body Matrix_proto.Json.Codec.json (to_json cm)
        in
        let* reply =
          http_post ?http client ~content_type:"application/json" ~body url
        in
        let+ r = Client.Http.decode_response response_jsont reply in
        Log.info (fun m -> m "Registered OAuth client %s" r.client_id);
        r
end

module Pkce = struct
  type t = { verifier : string; challenge : string }

  let challenge_method = "S256"

  let b64url s =
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

  let challenge_of_verifier verifier =
    b64url Digestif.SHA256.(digest_string verifier |> to_raw_string)

  (* Base64url of n bytes is ceil(4n/3) characters, all unreserved. 32 bytes
     give the minimum 43; 96 give the maximum 128. *)
  let min_bytes = 32
  let max_bytes = 96

  let create ?(bytes = min_bytes) ~random () =
    if bytes < min_bytes || bytes > max_bytes then
      invalid_arg
        (Printf.sprintf
           "Matrix_client.Oauth.Pkce.create: %d bytes is outside %d..%d" bytes
           min_bytes max_bytes);
    let verifier = b64url (Random.generate random bytes) in
    { verifier; challenge = challenge_of_verifier verifier }

  (* RFC 7636 §4.1: 43..128 characters of the RFC 3986 unreserved set. *)
  let is_unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
    | _ -> false

  let of_verifier verifier =
    let n = String.length verifier in
    if n < 43 || n > 128 then
      Error
        (Error.Json_error
           (Printf.sprintf
              "PKCE code verifier is %d characters; RFC 7636 requires 43 to 128"
              n))
    else if not (String.for_all is_unreserved verifier) then
      Error
        (Error.Json_error
           "PKCE code verifier has characters outside the unreserved set")
    else Ok { verifier; challenge = challenge_of_verifier verifier }
end

module Authorization = struct
  (* [scopes_supported] is optional in RFC 8414 and is commonly omitted by
     Matrix authorisation servers.  Only switch dialect when metadata gives a
     complete pair: checking the API token alone could otherwise make us send
     an unstable API scope with a stable device scope (or vice versa).  Stable
     wins when both pairs are advertised.  With no complete signal, preserve
     the long-standing stable default. *)
  let advertised_device_prefix scopes prefix =
    List.exists
      (fun scope ->
        String.equal scope prefix
        || String.equal scope (prefix ^ "*")
        || String.starts_with ~prefix scope)
      scopes

  let default_scope (m : Metadata.t) device_id =
    let scopes = m.scopes_supported in
    let stable =
      List.mem scope_api scopes
      && advertised_device_prefix scopes scope_device_prefix
    in
    let unstable =
      List.mem scope_api_unstable scopes
      && advertised_device_prefix scopes scope_device_prefix_unstable
    in
    if stable then [ scope_api; scope_device device_id ]
    else if unstable then
      [ scope_api_unstable; scope_device_unstable device_id ]
    else [ scope_api; scope_device device_id ]

  let build_url ?scope ?prompt ?login_hint ?(response_mode = "query")
      (m : Metadata.t) ~client_id ~redirect_uri ~device_id ~state
      ~(pkce : Pkce.t) () =
    let scope =
      match scope with Some s -> s | None -> default_scope m device_id
    in
    let params =
      [
        ("response_type", "code");
        ("client_id", client_id);
        ("redirect_uri", redirect_uri);
        ("scope", String.concat " " scope);
        ("state", state);
        ("response_mode", response_mode);
        ("code_challenge", pkce.challenge);
        ("code_challenge_method", Pkce.challenge_method);
      ]
      @ (match prompt with None -> [] | Some p -> [ ("prompt", p) ])
      @ match login_hint with None -> [] | Some h -> [ ("login_hint", h) ]
    in
    Uriz.add_query_params m.authorization_endpoint params

  type request = {
    url : Uriz.t;
    state : string;
    pkce : Pkce.t;
    device_id : Matrix_proto.Id.Device_id.t;
    redirect_uri : string;
    scope : string;
  }

  let request ?scope ?prompt ?login_hint ?response_mode ?device_id ?state ?pkce
      ~random m ~client_id ~redirect_uri () =
    let device_id =
      match device_id with Some d -> d | None -> generate_device_id ~random
    in
    let state =
      match state with
      | Some s -> s
      (* Opaque and unguessable; the spec likens it to a transaction id. *)
      | None -> Base64.encode_string ~pad:false (Random.generate random 16)
    in
    let pkce = match pkce with Some p -> p | None -> Pkce.create ~random () in
    let scope_list =
      match scope with Some s -> s | None -> default_scope m device_id
    in
    let url =
      build_url ~scope:scope_list ?prompt ?login_hint ?response_mode m
        ~client_id ~redirect_uri ~device_id ~state ~pkce ()
    in
    {
      url;
      state;
      pkce;
      device_id;
      redirect_uri;
      scope = String.concat " " scope_list;
    }

  type redirect = { code : string; state : string }
  type redirect_error = Denied of oauth_error | Malformed of string

  let pp_redirect_error fmt = function
    | Denied e -> Format.fprintf fmt "authorisation failed: %a" pp_oauth_error e
    | Malformed s -> Format.fprintf fmt "malformed redirect: %s" s

  let parse_redirect uri =
    (* [response_mode=fragment] puts the parameters after the '#', where
       they are encoded exactly as a query string. Both are read. *)
    let fragment_params =
      match Uriz.fragment uri with
      | Null | This "" -> []
      | This f -> Uriz.query_params ~plus_as_space:true (Uriz.make ~query:f ())
    in
    let params = Uriz.query_params ~plus_as_space:true uri @ fragment_params in
    let response_parameters =
      [ "state"; "code"; "error"; "error_description"; "error_uri" ]
    in
    let values name =
      List.filter_map
        (fun (key, value) ->
          if String.equal key name then Some (Option.value ~default:"" value)
          else None)
        params
    in
    match
      List.find_opt
        (fun name -> List.length (values name) > 1)
        response_parameters
    with
    | Some name -> Error (Malformed ("duplicate " ^ name ^ " parameter"))
    | None -> (
        let get name =
          match values name with [ v ] when v <> "" -> Some v | _ -> None
        in
        let state = get "state" in
        match get "error" with
        | Some error ->
            Error
              (Denied
                 {
                   error;
                   error_description = get "error_description";
                   error_uri = get "error_uri";
                 })
        | None -> (
            match (get "code", state) with
            | Some code, Some state -> Ok { code; state }
            | None, _ -> Error (Malformed "no code parameter")
            | Some _, None -> Error (Malformed "no state parameter")))
end

module Token = struct
  type t = {
    access_token : string;
    token_type : string;
    refresh_token : string option;
    scope : string option;
    expires_at : Ptime.t option;
  }

  let expires_in_of t =
    match t.expires_at with
    | None -> None
    | Some at ->
        let left = Ptime.Span.to_float_s (Ptime.diff at (now ())) in
        Some (if left <= 0. then 0 else int_of_float left)

  let jsont =
    Jsont.Object.(
      map ~kind:"oauth_tokens"
        (fun access_token token_type expires_in refresh_token scope ->
          {
            access_token;
            token_type;
            refresh_token;
            scope;
            expires_at =
              Option.bind expires_in (fun s ->
                  Ptime.add_span (now ()) (Ptime.Span.of_int_s s));
          })
      |> mem "access_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.access_token)
      |> mem "token_type" Matrix_proto.Json.Codec.string
           ~dec_absent:(fun () -> "Bearer")
           ~enc:(fun t -> t.token_type)
      |> opt_mem "expires_in" Matrix_proto.Json.Codec.int ~enc:expires_in_of
      |> opt_mem "refresh_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.refresh_token)
      |> opt_mem "scope" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.scope)
      |> skip_unknown |> finish)

  let is_expired ?now:instant t =
    match t.expires_at with
    | None -> false
    | Some at ->
        let instant = match instant with Some i -> i | None -> now () in
        Ptime.compare instant at >= 0

  let post_tokens ?http client (m : Metadata.t) ~params =
    let* body = post_form ?http client ~params m.token_endpoint in
    Client.Http.decode_response jsont body

  let exchange ?http client m ~client_id ~redirect_uri ~code ~(pkce : Pkce.t) ()
      =
    post_tokens ?http client m
      ~params:
        [
          ("grant_type", "authorization_code");
          ("code", code);
          ("redirect_uri", redirect_uri);
          ("client_id", client_id);
          ("code_verifier", pkce.verifier);
        ]

  let refresh ?http client m ~client_id ~refresh_token () =
    post_tokens ?http client m
      ~params:
        [
          ("grant_type", "refresh_token");
          ("refresh_token", refresh_token);
          ("client_id", client_id);
        ]

  type token_type_hint = [ `Access_token | `Refresh_token ]

  let token_type_hint_to_string = function
    | `Access_token -> "access_token"
    | `Refresh_token -> "refresh_token"

  let revoke ?http ?token_type_hint client (m : Metadata.t) ~client_id ~token ()
      =
    match m.revocation_endpoint with
    | None ->
        Error
          (Error.Json_error
             "OAuth 2.0: the authorisation server advertises no \
              revocation_endpoint")
    | Some url ->
        let params =
          [ ("token", token) ]
          @ (match token_type_hint with
            | None -> []
            | Some h -> [ ("token_type_hint", token_type_hint_to_string h) ])
          @ [ ("client_id", client_id) ]
        in
        let+ _body = post_form ?http client ~params url in
        ()

  let logout ?http client m ~client_id t =
    let* () =
      revoke ?http ~token_type_hint:`Access_token client m ~client_id
        ~token:t.access_token ()
    in
    match t.refresh_token with
    | None -> Ok ()
    | Some token ->
        revoke ?http ~token_type_hint:`Refresh_token client m ~client_id ~token
          ()

  let finish_login client ~device_id t =
    let+ user_id =
      Auth.whoami (Client.with_access_token client t.access_token)
    in
    Log.info (fun m ->
        m "OAuth login complete for %s on device %a"
          (Matrix_proto.Id.User_id.to_string user_id)
          Matrix_proto.Id.Device_id.pp device_id);
    {
      Client.user_id;
      access_token = t.access_token;
      device_id;
      refresh_token = t.refresh_token;
    }
end

module Device_authorization = struct
  type t = {
    device_code : string;
    user_code : string;
    verification_uri : Uriz.t;
    verification_uri_complete : Uriz.t option;
    expires_in : int;
    interval : int;
  }

  type poll_error =
    | Authorization_pending
    | Slow_down
    | Access_denied
    | Expired_token
    | OAuth_error of oauth_error
    | Transport_error of Error.t

  let nonempty ~what =
    Jsont.map
      ~dec:(fun s ->
        if s = "" then Jsont.Error.msg Jsont.Meta.none (what ^ " is empty")
        else s)
      ~enc:Fun.id Matrix_proto.Json.Codec.string

  let positive ~what =
    Jsont.map
      ~dec:(fun n ->
        if n > 0 then n
        else Jsont.Error.msg Jsont.Meta.none (what ^ " must be positive"))
      ~enc:Fun.id Matrix_proto.Json.Codec.int

  let jsont : t Jsont.t =
    Jsont.Object.(
      map
        (fun
          device_code
          user_code
          verification_uri
          verification_uri_complete
          expires_in
          interval
        ->
          {
            device_code;
            user_code;
            verification_uri;
            verification_uri_complete;
            expires_in;
            interval;
          })
      |> mem "device_code" (nonempty ~what:"device_code") ~enc:(fun t ->
          t.device_code)
      |> mem "user_code" (nonempty ~what:"user_code") ~enc:(fun t ->
          t.user_code)
      |> mem "verification_uri" endpoint_uri ~enc:(fun t -> t.verification_uri)
      |> opt_mem "verification_uri_complete" endpoint_uri ~enc:(fun t ->
          t.verification_uri_complete)
      |> mem "expires_in" (positive ~what:"expires_in") ~enc:(fun t ->
          t.expires_in)
      |> mem "interval" (positive ~what:"interval")
           ~dec_absent:(fun () -> 5) ~enc:(fun t ->
          t.interval)
      |> finish)

  let request ?http ?scope client (m : Metadata.t) ~client_id ~device_id () =
    match m.device_authorization_endpoint with
    | None ->
        Error
          (Error.Json_error
             "OAuth 2.0: the authorisation server advertises no \
              device_authorization_endpoint")
    | Some endpoint ->
        let scope =
          match scope with
          | Some s -> s
          | None -> Authorization.default_scope m device_id
        in
        let* body =
          post_form ?http client
            ~params:
              [ ("client_id", client_id); ("scope", String.concat " " scope) ]
            endpoint
        in
        Client.Http.decode_response jsont body

  let classify = function
    | Error.Http_error _ as e -> (
        match oauth_error_of_error e with
        | Some { error = "authorization_pending"; _ } -> Authorization_pending
        | Some { error = "slow_down"; _ } -> Slow_down
        | Some { error = "access_denied"; _ } -> Access_denied
        | Some { error = "expired_token"; _ } -> Expired_token
        | Some e -> OAuth_error e
        | None -> Transport_error e)
    | e -> Transport_error e

  let poll ?http client (m : Metadata.t) ~client_id ~device_code () :
      (Token.t, poll_error) result =
    match
      post_form ?http client
        ~params:
          [
            ("grant_type", "urn:ietf:params:oauth:grant-type:device_code");
            ("device_code", device_code);
            ("client_id", client_id);
          ]
        m.token_endpoint
    with
    | Ok body -> (
        match Client.Http.decode_response Token.jsont body with
        | Ok tokens -> Ok tokens
        | Error e -> Error (Transport_error e))
    | Error e -> Error (classify e)
end
