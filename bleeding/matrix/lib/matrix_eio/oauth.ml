module O = Matrix_client.Oauth

let unwrap context fn =
  Error.with_context context (fun () -> Error.unwrap (fn ()))

let src = Logs.Src.create "matrix.oauth" ~doc:"Matrix OAuth 2.0 authentication"

module Log = (val Logs.src_log src : Logs.LOG)

type err =
  | State_mismatch
  | Denied of O.oauth_error
  | Expired
  | OAuth_error of O.oauth_error
  | Malformed_redirect of string
  | Not_registered
  | Timeout

type session_invalid_reason = Invalid_grant
type Eio.Exn.err += E of err

let pp_err fmt = function
  | State_mismatch ->
      Format.fprintf fmt
        "OAuth state mismatch: the redirect does not belong to this \
         authorisation request"
  | Denied e ->
      Format.fprintf fmt "OAuth authorisation denied: %a" O.pp_oauth_error e
  | Expired -> Format.fprintf fmt "OAuth device authorisation expired"
  | OAuth_error e ->
      Format.fprintf fmt "OAuth device authorisation failed: %a"
        O.pp_oauth_error e
  | Malformed_redirect s -> Format.fprintf fmt "Malformed OAuth redirect: %s" s
  | Not_registered ->
      Format.fprintf fmt
        "No OAuth client_id, and the authorisation server has no registration \
         endpoint"
  | Timeout ->
      Format.fprintf fmt "OAuth authorisation timed out waiting for the browser"

let () =
  Eio.Exn.register_pp (fun fmt -> function
    | E e ->
        pp_err fmt e;
        true
    | _ -> false)

let fail e = raise (Eio.Exn.create (E e))
let default_timeout = 300.

let is_invalid_grant error =
  match O.oauth_error_of_error error with
  | Some { error = "invalid_grant"; _ } -> true
  | _ -> false

let notify_session_invalid on_session_invalid reason =
  Option.iter
    (fun callback ->
      try callback reason with
      | Eio.Cancel.Cancelled _ as exn ->
          let bt = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn bt
      | exn ->
          Log.warn (fun m ->
              m "OAuth session-invalid callback failed: %s"
                (Printexc.to_string exn)))
    on_session_invalid

type oauth_error = O.oauth_error = {
  error : string;
  error_description : string option;
  error_uri : string option;
}

type account_action = O.account_action =
  | Profile
  | Devices_list
  | Device_view
  | Device_delete
  | Account_deactivate
  | Cross_signing_reset
  | Other_action of string

let pp_oauth_error = O.pp_oauth_error
let oauth_error_of_json = O.oauth_error_of_json
let oauth_error_of_error = O.oauth_error_of_error
let scope_api = O.scope_api
let scope_device_prefix = O.scope_device_prefix
let scope_api_unstable = O.scope_api_unstable
let scope_device_prefix_unstable = O.scope_device_prefix_unstable
let scope_device = O.scope_device
let scope_device_unstable = O.scope_device_unstable
let device_id_of_scope = O.device_id_of_scope
let generate_device_id = O.generate_device_id
let account_action_to_string = O.account_action_to_string
let account_action_of_string = O.account_action_of_string

module Metadata = struct
  type t = O.Metadata.t = {
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

  let jsont = O.Metadata.jsont
  let v1_path = O.Metadata.v1_path
  let unstable_path = O.Metadata.unstable_path
  let openid_configuration_path = O.Metadata.openid_configuration_path

  let fetch ?http client =
    unwrap "fetching Matrix OAuth metadata" (fun () ->
        O.Metadata.fetch ?http (Client.base client))

  let fetch_cached ?http client =
    unwrap "fetching cached Matrix OAuth metadata" (fun () ->
        O.Metadata.fetch_cached ?http (Client.base client))

  let invalidate_cache client = O.Metadata.invalidate_cache (Client.base client)

  let validate ?allow_insecure m =
    unwrap "validating Matrix OAuth metadata" (fun () ->
        O.Metadata.validate ?allow_insecure m)

  let validate_loopback ?allow_insecure m =
    unwrap "validating Matrix OAuth loopback metadata" (fun () ->
        O.Metadata.validate_loopback ?allow_insecure m)

  let validate_device ?allow_insecure m =
    unwrap "validating Matrix OAuth device metadata" (fun () ->
        O.Metadata.validate_device ?allow_insecure m)

  let supports_response_type = O.Metadata.supports_response_type
  let supports_grant_type = O.Metadata.supports_grant_type
  let supports_response_mode = O.Metadata.supports_response_mode
  let supports_code_challenge_method = O.Metadata.supports_code_challenge_method
  let supports_prompt = O.Metadata.supports_prompt
  let supports_account_action = O.Metadata.supports_account_action
  let account_management_url = O.Metadata.account_management_url
end

module Registration = struct
  type localized = O.Registration.localized = {
    value : string;
    translations : (string * string) list;
  }

  type client_metadata = O.Registration.client_metadata = {
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

  type response = O.Registration.response = {
    client_id : string;
    client_id_issued_at : Ptime.t option;
    registered : (string * Jsont.json) list;
  }

  let plain = O.Registration.plain
  let v = O.Registration.v
  let loopback_redirect_uri = O.Registration.loopback_redirect_uri
  let to_json = O.Registration.to_json

  let register ?http client metadata cm =
    unwrap "registering a Matrix OAuth client" (fun () ->
        O.Registration.register ?http (Client.base client) metadata cm)
end

module Pkce = O.Pkce

module Authorization = struct
  type request = O.Authorization.request = {
    url : Uriz.t;
    state : string;
    pkce : Pkce.t;
    device_id : Matrix_proto.Id.Device_id.t;
    redirect_uri : string;
    scope : string;
  }

  type redirect = O.Authorization.redirect = { code : string; state : string }

  type redirect_error = O.Authorization.redirect_error =
    | Denied of oauth_error
    | Malformed of string

  let build_url = O.Authorization.build_url
  let request = O.Authorization.request
  let pp_redirect_error = O.Authorization.pp_redirect_error

  let parse_redirect uri =
    match O.Authorization.parse_redirect uri with
    | Ok r -> r
    | Error (O.Authorization.Denied e) -> fail (Denied e)
    | Error (O.Authorization.Malformed s) -> fail (Malformed_redirect s)
end

module Token = struct
  type t = O.Token.t = {
    access_token : string;
    token_type : string;
    refresh_token : string option;
    scope : string option;
    expires_at : Ptime.t option;
  }

  type token_type_hint = O.Token.token_type_hint

  let jsont = O.Token.jsont
  let is_expired = O.Token.is_expired

  let exchange ?http client metadata ~client_id ~redirect_uri ~code ~pkce () =
    unwrap "exchanging a Matrix OAuth authorization code" (fun () ->
        O.Token.exchange ?http (Client.base client) metadata ~client_id
          ~redirect_uri ~code ~pkce ())

  let refresh ?http client metadata ~client_id ~refresh_token () =
    unwrap "refreshing a Matrix OAuth token" (fun () ->
        O.Token.refresh ?http (Client.base client) metadata ~client_id
          ~refresh_token ())

  let revoke ?http ?token_type_hint client metadata ~client_id ~token () =
    unwrap "revoking a Matrix OAuth token" (fun () ->
        O.Token.revoke ?http ?token_type_hint (Client.base client) metadata
          ~client_id ~token ())

  let logout ?http client metadata ~client_id tokens =
    unwrap "logging out a Matrix OAuth session" (fun () ->
        O.Token.logout ?http (Client.base client) metadata ~client_id tokens)

  let finish_login client ~device_id tokens =
    unwrap "finishing a Matrix OAuth login" (fun () ->
        O.Token.finish_login (Client.base client) ~device_id tokens)
end

(* Install the refresh hook on the result-returning client underneath the Eio
   wrapper.  Refresh discovery deliberately starts from an unauthenticated
   client: an expired bearer must not make metadata discovery recurse through
   this hook.  The original transport is passed explicitly because the issuer
   named by the freshly fetched metadata is normally off the homeserver's
   origin. *)
let install_auto_refresh ?store ?allow_insecure ?on_session_update ?expires_at
    ?early_refresh ?now ?on_session_invalid ~track_expiry client ~client_id =
  let ( let* ) = Result.bind in
  let base = Client.base client in
  let unauthenticated = Matrix_client.Client.without_session base in
  let prepare (session : Matrix_client.Client.session) =
    match session.refresh_token with
    | None -> Error Matrix_client.Error.No_session
    | Some refresh_token ->
        let http = Client.http client in
        let* metadata = O.Metadata.fetch ~http unauthenticated in
        let* () = O.Metadata.validate ?allow_insecure metadata in
        Ok
          (fun () ->
            match
              O.Token.refresh ~http unauthenticated metadata ~client_id
                ~refresh_token ()
            with
            | Error error ->
                if is_invalid_grant error then
                  notify_session_invalid on_session_invalid Invalid_grant;
                Error error
            | Ok tokens ->
                Ok
                  Matrix_client.Client.
                    {
                      refreshed_tokens =
                        {
                          access_token = tokens.access_token;
                          refresh_token = tokens.refresh_token;
                        };
                      expires_at =
                        (if track_expiry then tokens.expires_at else None);
                    })
  in
  Client.with_prepared_auto_refresh_expiry ?store ?on_session_update ?expires_at
    ?early_refresh ?now ~prepare client

let with_auto_refresh_expiry ?store ?allow_insecure ?on_session_update
    ?expires_at ?early_refresh ?now ?on_session_invalid client ~client_id =
  install_auto_refresh ?store ?allow_insecure ?on_session_update ?expires_at
    ?early_refresh ?now ?on_session_invalid ~track_expiry:true client ~client_id

let with_auto_refresh ?store ?allow_insecure ?on_session_update
    ?on_session_invalid client ~client_id =
  let on_session_update =
    Option.map (fun callback session _ -> callback session) on_session_update
  in
  install_auto_refresh ?store ?allow_insecure ?on_session_update
    ?on_session_invalid ~track_expiry:false client ~client_id

module Device_authorization = struct
  type t = O.Device_authorization.t = {
    device_code : string;
    user_code : string;
    verification_uri : Uriz.t;
    verification_uri_complete : Uriz.t option;
    expires_in : int;
    interval : int;
  }

  type poll_error = O.Device_authorization.poll_error =
    | Authorization_pending
    | Slow_down
    | Access_denied
    | Expired_token
    | OAuth_error of oauth_error
    | Transport_error of Matrix_client.Error.t

  let jsont = O.Device_authorization.jsont

  let request ?http ?scope client metadata ~client_id ~device_id () =
    unwrap "requesting Matrix OAuth device authorization" (fun () ->
        O.Device_authorization.request ?http ?scope (Client.base client)
          metadata ~client_id ~device_id ())

  let poll ?http client metadata ~client_id ~device_code () =
    Error.with_context "polling Matrix OAuth device authorization" (fun () ->
        O.Device_authorization.poll ?http (Client.base client) metadata
          ~client_id ~device_code ())
end

module Loopback = struct
  type t = {
    port : int Atomic.t;
    path : string;
    callback : Uriz.t Eio.Promise.t;
    stopped : bool Atomic.t;
  }

  let done_page =
    "<!doctype html>\n\
     <html lang=\"en\">\n\
     <head><meta charset=\"utf-8\"><title>Signed in</title></head>\n\
     <body style=\"font-family:system-ui,sans-serif;margin:4rem \
     auto;max-width:32rem\">\n\
     <h1>Signed in</h1>\n\
     <p>Authorisation is complete. You can close this tab and return to your \
     terminal.</p>\n\
     </body>\n\
     </html>\n"

  let invalid_path () =
    invalid_arg
      "Matrix_eio.Oauth.Loopback.create: path must be a canonical absolute \
       route without a query, fragment, empty, or dot segment"

  let route_path path =
    let segments = Matrix_client.Client.Url.path_segments in
    let url =
      match Matrix_client.Client.Url.of_string ("http://127.0.0.1" ^ path) with
      | Ok url
        when String.equal (Matrix_client.Client.Url.path_and_query url) path ->
          url
      | Ok _ | Error _ -> invalid_path ()
    in
    let segments = segments url in
    match segments with
    | [] -> invalid_path ()
    | first :: rest ->
        List.fold_left
          (fun acc segment ->
            let append = Proffer.Route.( / ) in
            append acc (Proffer.Route.s segment))
          (Proffer.Route.s first) rest

  let create ?(path = "/callback") ~sw ~env net =
    let segments = String.split_on_char '/' path in
    if
      String.length path < 2
      || (not (String.is_valid_utf_8 path))
      || path.[0] <> '/'
      || String.exists (function '?' | '#' -> true | _ -> false) path
      ||
      match segments with
      | "" :: rest ->
          List.exists
            (fun segment -> segment = "" || segment = "." || segment = "..")
            rest
      | _ -> true
    then invalid_path ();
    let port_ready, port_resolver = Eio.Promise.create () in
    let callback, callback_resolver = Eio.Promise.create () in
    let stop, stop_resolver = Eio.Promise.create () in
    let t = { port = Atomic.make 0; path; callback;
              stopped = Atomic.make false } in
    let runtime =
      object
        method net = net
        method clock = Eio.Stdenv.clock env
        method mono_clock = Eio.Stdenv.mono_clock env
      end
    in
    let route = route_path path in
    let handler (t, callback_resolver, stop_resolver)
        (req : Proffer.Req.t @ local) (respond @ local) =
      if not (Proffer.Method.equal (Proffer.Req.meth req) Httpz.Method.Get) then
        Proffer.Resp.v respond ~status:Httpz.Res.Method_not_allowed
          ~headers:[ Proffer.Headers.h Httpz.Header_name.Allow "GET" ]
          ~content_type:(This "text/plain; charset=utf-8")
          (Proffer.Body.String "Method Not Allowed\n")
      else begin
        let uri = Uriz.of_string_exn
            (Printf.sprintf "http://127.0.0.1:%d%s" (Atomic.get t.port) path) in
        let uri = Uriz.with_query_params uri (Proffer.Req.query req) in
        match O.Authorization.parse_redirect uri with
        | Error (O.Authorization.Malformed _) ->
            Proffer.Resp.text respond ~status:Httpz.Res.Bad_request
              "Malformed OAuth redirect\n"
        | Ok _ | Error (O.Authorization.Denied _) ->
            if Atomic.compare_and_set t.stopped false true then begin
              Eio.Promise.resolve callback_resolver uri;
              Eio.Promise.resolve stop_resolver ()
            end;
            Proffer.Resp.html respond ~cache:Proffer.Cache_control.no_store
              done_page
      end
    in
    let site = Proffer.Site.of_routes [ Proffer.Route.get route handler ] in
    let config =
      {
        Proffer_httpz.default_config with
        first_byte_timeout = Duration.of_ms 500;
        request_timeout = Duration.of_ms 500;
        idle_timeout = Duration.of_sec 1;
      }
    in
    let listening = ref false in
    Eio.Fiber.fork ~sw (fun () ->
        try
          Eio.Switch.run @@ fun server_sw ->
          Proffer_httpz.run ~sw:server_sw ~config ~port:0
            ~on_listening:(fun addr ->
              match addr with
              | `Tcp (_, port) ->
                  listening := true;
                  Atomic.set t.port port;
                  Log.debug (fun m ->
                      m "OAuth loopback listener on 127.0.0.1:%d%s" port path);
                  Eio.Promise.resolve port_resolver (Ok ())
              | `Unix _ ->
                  (* [port:0] requests a TCP loopback listener.  Treat a
                     different address as a startup failure and let the
                     enclosing [Switch.run] unwind, which resolves
                     [port_ready] through the error path below rather than
                     leaving the caller waiting forever. *)
                  raise
                    (Error.err
                       (Error.Network
                          "OAuth loopback listener unexpectedly bound a Unix \
                           socket")))
            ~stop runtime ~env:(t, callback_resolver, stop_resolver) site
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          if !listening then Printexc.raise_with_backtrace exn bt
          else Eio.Promise.resolve port_resolver (Error (exn, bt)));
    Error.with_context "starting the OAuth loopback listener" (fun () ->
        match Eio.Promise.await port_ready with
        | Ok () -> ()
        | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt);
    t

  let port t = Atomic.get t.port
  let redirect_uri t =
    Printf.sprintf "http://127.0.0.1:%d%s" (Atomic.get t.port) t.path
  let wait t = Eio.Promise.await t.callback
end

let default_client_metadata =
  O.Registration.v ~client_uri:"https://github.com/ocaml-matrix/ocaml-matrix"
    ~client_name:(O.Registration.plain "ocaml-matrix")
    ~redirect_uris:[ O.Registration.loopback_redirect_uri ]
    ~application_type:"native" ()

(* RFC 8252 §7.3 permits a native client to register a loopback URI with the
   port omitted, but a dynamic registration made for this concrete flow can
   be stricter: register the exact endpoint that was already bound. Keep the
   exported template portless so callers can use it for portable metadata. *)
let browser_client_metadata ~redirect_uri =
  { default_client_metadata with redirect_uris = [ redirect_uri ] }

let default_device_client_metadata =
  O.Registration.v ~client_uri:"https://github.com/ocaml-matrix/ocaml-matrix"
    ~client_name:(O.Registration.plain "ocaml-matrix")
    ~redirect_uris:[ O.Registration.loopback_redirect_uri ]
    ~grant_types:
      [
        "authorization_code";
        "refresh_token";
        "urn:ietf:params:oauth:grant-type:device_code";
      ]
    ~application_type:"native" ()

let browser_opener ~sw ~env url =
  let mgr = Eio.Stdenv.process_mgr env in
  let try_command argv =
    match Eio.Process.spawn ~sw mgr argv with
    | _child -> true
    | exception Eio.Io _ -> false
  in
  let opened = try_command [ "xdg-open"; url ] || try_command [ "open"; url ] in
  if not opened then
    Log.info (fun m -> m "No desktop URL handler; open the URL by hand")

type browser_login = {
  session : Matrix_client.Client.session;
  client_id : string;
}

type browser_login_with_expiry = {
  session : Matrix_client.Client.session;
  client_id : string;
  expires_at : Ptime.t option;
}

let login_with_browser_full_expiry ~env ?http ?client_id ?client_metadata
    ?prompt ?login_hint ?scope ?path ?allow_insecure
    ?(timeout = default_timeout) client ~open_url () =
  Error.with_context "performing a Matrix OAuth browser login" (fun () ->
      let base = Client.base client in
      let http =
        match http with
        | Some h -> h
        | None ->
            Fetch_httpz.std
              ~retry:
                (Matrix_client.Http_retry.default
                   ~homeserver:(Client.homeserver client))
              env
      in
      let metadata =
        unwrap "fetching Matrix OAuth metadata" (fun () ->
            O.Metadata.fetch ~http base)
      in
      unwrap "validating Matrix OAuth loopback metadata" (fun () ->
          O.Metadata.validate_loopback ?allow_insecure metadata);
      (* The listener is bound for the length of the flow and no longer. *)
      Eio.Switch.run @@ fun sw ->
      let listener = Loopback.create ?path ~sw ~env (Eio.Stdenv.net env) in
      let redirect_uri = Loopback.redirect_uri listener in
      let client_id =
        match client_id with
        | Some id -> id
        | None ->
            if metadata.registration_endpoint = None then fail Not_registered;
            let cm =
              match client_metadata with
              | Some cm -> cm
              | None -> browser_client_metadata ~redirect_uri
            in
            (unwrap "registering a Matrix OAuth client" (fun () ->
                 O.Registration.register ~http base metadata cm))
              .client_id
      in
      let request =
        O.Authorization.request
          ~random:(Matrix_client.Client.random base)
          ?prompt ?login_hint ?scope metadata ~client_id ~redirect_uri ()
      in
      Error.with_context "opening the Matrix OAuth authorization URL" (fun () ->
          open_url (Uriz.to_string request.url));
      let redirect =
        try
          Eio.Time.with_timeout_exn env#clock timeout (fun () ->
              Loopback.wait listener)
        with Eio.Time.Timeout -> fail Timeout
      in
      let callback = Authorization.parse_redirect redirect in
      (* RFC 6749 §10.12: the state ties the redirect to this request. *)
      if not (String.equal callback.state request.state) then
        fail State_mismatch;
      let tokens =
        unwrap "exchanging a Matrix OAuth authorization code" (fun () ->
            O.Token.exchange ~http base metadata ~client_id ~redirect_uri
              ~code:callback.code ~pkce:request.pkce ())
      in
      (* Prefer the device id the server echoed in the granted scope; fall back
     to the requested one when it echoed none. *)
      let device_id =
        match Option.bind tokens.scope O.device_id_of_scope with
        | Some d -> d
        | None -> request.device_id
      in
      let session =
        unwrap "finishing a Matrix OAuth login" (fun () ->
            O.Token.finish_login base ~device_id tokens)
      in
      ({ session; client_id; expires_at = tokens.expires_at }
        : browser_login_with_expiry))

let login_with_browser_full ~env ?http ?client_id ?client_metadata ?prompt
    ?login_hint ?scope ?path ?allow_insecure ?timeout client ~open_url () =
  let login =
    login_with_browser_full_expiry ~env ?http ?client_id ?client_metadata
      ?prompt ?login_hint ?scope ?path ?allow_insecure ?timeout client ~open_url
      ()
  in
  ({ session = login.session; client_id = login.client_id } : browser_login)

let login_with_browser ~env ?http ?client_id ?client_metadata ?prompt
    ?login_hint ?scope ?path ?allow_insecure ?timeout client ~open_url () =
  (login_with_browser_full ~env ?http ?client_id ?client_metadata ?prompt
     ?login_hint ?scope ?path ?allow_insecure ?timeout client ~open_url ())
    .session

type device_login = browser_login
type device_login_with_expiry = browser_login_with_expiry

let login_with_device_expiry ~env ?http ?client_id ?client_metadata ?scope
    ?device_id ?allow_insecure ?timeout client ~show () =
  Error.with_context "performing a Matrix OAuth device login" (fun () ->
      let timeout =
        match timeout with
        | None -> None
        | Some seconds when Float.is_finite seconds && seconds > 0. ->
            Some seconds
        | Some _ -> fail Timeout
      in
      let base = Client.base client in
      let http =
        match http with
        | Some h -> h
        | None ->
            Fetch_httpz.std
              ~retry:
                (Matrix_client.Http_retry.default
                   ~homeserver:(Client.homeserver client))
              env
      in
      let metadata =
        unwrap "fetching Matrix OAuth metadata" (fun () ->
            O.Metadata.fetch ~http base)
      in
      unwrap "validating Matrix OAuth device metadata" (fun () ->
          O.Metadata.validate_device ?allow_insecure metadata);
      let client_id =
        match client_id with
        | Some id -> id
        | None ->
            if metadata.registration_endpoint = None then fail Not_registered;
            let cm =
              Option.value client_metadata
                ~default:default_device_client_metadata
            in
            (unwrap "registering a Matrix OAuth device client" (fun () ->
                 O.Registration.register ~http base metadata cm))
              .client_id
      in
      let requested_device_id =
        Option.value device_id
          ~default:
            (O.generate_device_id ~random:(Matrix_client.Client.random base))
      in
      let authorization =
        unwrap "requesting Matrix OAuth device authorization" (fun () ->
            O.Device_authorization.request ~http ?scope base metadata ~client_id
              ~device_id:requested_device_id ())
      in
      (* The server's expiry starts when it issued the response, so account for
     time spent presenting the codes before the first poll. *)
      let started = Eio.Time.Mono.now env#mono_clock in
      show authorization;
      let add_seconds at seconds =
        match Mtime.Span.of_float_ns (seconds *. 1e9) with
        | None -> at
        | Some span -> Option.value (Mtime.add_span at span) ~default:at
      in
      let deadline =
        let expiry = add_seconds started (float authorization.expires_in) in
        match timeout with
        | None -> expiry
        | Some seconds ->
            let caller = add_seconds started (Float.max 0. seconds) in
            if Mtime.compare caller expiry < 0 then caller else expiry
      in
      let remaining () =
        let now = Eio.Time.Mono.now env#mono_clock in
        if Mtime.compare now deadline >= 0 then None
        else Some (Mtime.Span.to_float_ns (Mtime.span now deadline) /. 1e9)
      in
      let wait interval =
        match remaining () with
        | None -> fail Timeout
        | Some left ->
            let delay = Float.min (float interval) left in
            let target = add_seconds (Eio.Time.Mono.now env#mono_clock) delay in
            Eio.Time.Mono.sleep_until env#mono_clock target;
            if remaining () = None then fail Timeout
      in
      let rec poll interval =
        wait interval;
        let left =
          match remaining () with Some left -> left | None -> fail Timeout
        in
        let result =
          try
            Eio.Time.Timeout.run_exn
              (Eio.Time.Timeout.seconds env#mono_clock left) (fun () ->
                Error.with_context "polling Matrix OAuth device authorization"
                  (fun () ->
                    O.Device_authorization.poll ~http base metadata ~client_id
                      ~device_code:authorization.device_code ()))
          with Eio.Time.Timeout -> fail Timeout
        in
        match result with
        | Ok tokens -> tokens
        | Error Device_authorization.Authorization_pending -> poll interval
        | Error Device_authorization.Slow_down -> poll (interval + 5)
        | Error Device_authorization.Access_denied ->
            fail
              (Denied
                 {
                   error = "access_denied";
                   error_description = None;
                   error_uri = None;
                 })
        | Error Device_authorization.Expired_token -> fail Expired
        | Error (Device_authorization.OAuth_error e) -> fail (OAuth_error e)
        | Error (Device_authorization.Transport_error e) ->
            Error.raise_client_error
              ~context:"polling Matrix OAuth device authorization" e
      in
      let tokens = poll authorization.interval in
      let device_id =
        match Option.bind tokens.scope O.device_id_of_scope with
        | Some d -> d
        | None -> requested_device_id
      in
      let session = Token.finish_login client ~device_id tokens in
      ({ session; client_id; expires_at = tokens.expires_at }
        : device_login_with_expiry))

let login_with_device ~env ?http ?client_id ?client_metadata ?scope ?device_id
    ?allow_insecure ?timeout client ~show () =
  let login =
    login_with_device_expiry ~env ?http ?client_id ?client_metadata ?scope
      ?device_id ?allow_insecure ?timeout client ~show ()
  in
  ({ session = login.session; client_id = login.client_id } : device_login)
