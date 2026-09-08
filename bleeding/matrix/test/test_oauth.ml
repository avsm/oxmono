(** Tests for {!Matrix_client.Oauth} and {!Matrix_eio.Oauth}.

    The mock tests pin the request that actually leaves the library — the
    discovery fallback order, the RFC 7591 registration document, the
    authorisation URL's query, and the form-encoded bodies of the token and
    revocation endpoints — and decode canned replies into the module's records.
    The loopback listener test runs under [Eio_main.run] and talks to a real
    socket.

    The [Fetch_mock] harness is the one [test_matrix_client.ml] uses, copied
    rather than shared so the files stay independent. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Oauth = Matrix_client.Oauth
module Metadata = Oauth.Metadata
module Registration = Oauth.Registration
module Pkce = Oauth.Pkce
module Authorization = Oauth.Authorization
module Token = Oauth.Token
module Device_authorization = Oauth.Device_authorization
module Qr_login = Matrix_client.Qr_login
module Msc4108 = Qr_login.Msc4108
module Id = Matrix_proto.Id

(* Long enough that every [Random.generate] in a single test succeeds; the
   content is fixed, which is what makes the generated values assertable. *)
let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\000')
  end

type recorded = {
  meth : string;
  url : string;
  headers : Http.Header.t;
  body : string option;
}

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

(* Read a form-encoded request body back as an ordered assoc list. *)
let form_of body =
  List.map
    (fun (key, value) -> (key, Option.value ~default:"" value))
    (Uriz.query_params ~plus_as_space:true (Uriz.make ~query:body ()))

let record log (req : Fetch.Middleware.request) =
  log :=
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      headers = req.headers;
      body = body_of_request req;
    }
    :: !log

(* A client that answers a scripted sequence of [(status, body)] pairs, one
   per request, in order. *)
let mock_seq replies =
  let log = ref [] in
  let remaining = ref replies in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        match !remaining with
        | [] -> Alcotest.fail "more requests than scripted replies"
        | (status, body) :: rest ->
            remaining := rest;
            Fetch_mock.respond ~status body req)
  in
  (log, client)

(* Like [mock_seq], but gives every response the same Cache-Control header.
   This is sufficient for discovery tests because only successful metadata
   responses are retained. *)
let mock_seq_cache_control ?expires cache_control replies =
  let log = ref [] in
  let remaining = ref replies in
  let headers =
    match (cache_control, expires) with
    | None, None -> None
    | _ ->
        Some
          (Http.Header.of_list
             (List.filter_map
                (fun (name, value) ->
                  Option.map (fun value -> (name, value)) value)
                [ ("cache-control", cache_control); ("expires", expires) ]))
  in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        match !remaining with
        | [] -> Alcotest.fail "more requests than scripted replies"
        | (status, body) :: rest -> (
            remaining := rest;
            match headers with
            | None -> Fetch_mock.respond ~status body req
            | Some headers -> Fetch_mock.respond ~status ~headers body req))
  in
  (log, client)

let default_homeserver = "https://hs.example"

let client_of ?(homeserver = default_homeserver) fetch =
  let config = Client.config ~homeserver:(Uriz.of_string_exn homeserver) () in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let did s = Result.get_ok (Id.Device_id.of_string s)
let uid s = Result.get_ok (Id.User_id.of_string s)
let requests log = List.rev !log
let urls log = List.map (fun r -> r.url) (requests log)

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one request, got %d" (List.length rs)

let header r name = Http.Header.get r.headers name
let check_string = Alcotest.(check string)
let check_str_opt = Alcotest.(check (option string))
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_strings = Alcotest.(check (list string))
let run f () = Eio_mock.Backend.run f

(* [contains haystack needle]: no Astring dependency in this tree. *)
let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.sub haystack i n = needle || at (i + 1))
  in
  at 0

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

let get_error = function
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error e -> e

(* The metadata document is the spec's own example, verbatim from
   [data/api/client-server/oauth_server_metadata.yaml]. *)

let metadata_json =
  {|{
  "issuer": "https://auth.example/",
  "authorization_endpoint": "https://auth.example/oauth2/auth",
  "token_endpoint": "https://auth.example/oauth2/token",
  "registration_endpoint": "https://auth.example/oauth2/clients/register",
  "device_authorization_endpoint": "https://auth.example/oauth2/device",
  "revocation_endpoint": "https://auth.example/oauth2/revoke",
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "response_modes_supported": ["query", "fragment"],
  "code_challenge_methods_supported": ["S256"],
  "prompt_values_supported": ["create"],
  "account_management_uri": "https://auth.example/manage",
  "account_management_actions_supported": [
    "org.matrix.profile",
    "org.matrix.devices_list",
    "org.matrix.device_view",
    "org.matrix.device_delete",
    "org.matrix.account_deactivate",
    "org.matrix.cross_signing_reset"
  ]
}|}

let unrecognized =
  {|{"errcode":"M_UNRECOGNIZED","error":"Legacy authentication is in use on this homeserver."}|}

let well_known_json =
  {|{"m.homeserver":{"base_url":"https://hs.example"},
     "m.authentication":{"issuer":"https://auth.example/","account":"https://auth.example/manage"}}|}

let metadata () = ok (Client.Http.decode_response Metadata.jsont metadata_json)

(* Metadata discovery and its fallback order. *)

let test_metadata_v1 () =
  let log, fetch = mock_seq [ (200, metadata_json) ] in
  let m = ok (Metadata.fetch (client_of fetch)) in
  check_strings "one request, to the stable path"
    [ "https://hs.example/_matrix/client/v1/auth_metadata" ]
    (urls log);
  check_string "issuer" "https://auth.example/" (Uriz.to_string m.issuer);
  check_string "authorization endpoint" "https://auth.example/oauth2/auth"
    (Uriz.to_string m.authorization_endpoint);
  check_string "token endpoint" "https://auth.example/oauth2/token"
    (Uriz.to_string m.token_endpoint);
  check_str_opt "registration endpoint"
    (Some "https://auth.example/oauth2/clients/register")
    (Option.map Uriz.to_string m.registration_endpoint);
  check_str_opt "revocation endpoint"
    (Some "https://auth.example/oauth2/revoke")
    (Option.map Uriz.to_string m.revocation_endpoint);
  check_str_opt "device authorization endpoint"
    (Some "https://auth.example/oauth2/device")
    (Option.map Uriz.to_string m.device_authorization_endpoint);
  check_str_opt "account management uri" (Some "https://auth.example/manage")
    (Option.map Uriz.to_string m.account_management_uri);
  check_strings "response modes" [ "query"; "fragment" ]
    m.response_modes_supported;
  check_strings "prompt values" [ "create" ] m.prompt_values_supported;
  check_bool "device_delete supported" true
    (Metadata.supports_account_action m Oauth.Device_delete)

let test_metadata_url_decode_validation () =
  let json_string value =
    Result.get_ok
      (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.string value)
  in
  let document ~issuer ~authorization_endpoint ~token_endpoint =
    Printf.sprintf
      {|{"issuer":%s,"authorization_endpoint":%s,"token_endpoint":%s}|}
      (json_string issuer)
      (json_string authorization_endpoint)
      (json_string token_endpoint)
  in
  let valid = "https://auth.example/" in
  let rejects =
    [
      document ~issuer:"relative" ~authorization_endpoint:valid
        ~token_endpoint:valid;
      document ~issuer:valid
        ~authorization_endpoint:"ftp://auth.example/authorize"
        ~token_endpoint:valid;
      document ~issuer:valid
        ~authorization_endpoint:"https://user@auth.example/authorize"
        ~token_endpoint:valid;
      document ~issuer:valid ~authorization_endpoint:valid
        ~token_endpoint:"https://auth.example:not-a-port/token";
    ]
  in
  List.iter
    (fun body ->
      match Client.Http.decode_response Metadata.jsont body with
      | Error (Error.Json_error _) -> ()
      | Error error ->
          Alcotest.failf "unexpected metadata URL error: %s"
            (Error.to_string error)
      | Ok _ -> Alcotest.fail "invalid OAuth metadata URL was accepted")
    rejects;
  let canonical =
    ok
      (Client.Http.decode_response Metadata.jsont
         (document ~issuer:"https://bücher.example/"
            ~authorization_endpoint:"https://bücher.example/authorize"
            ~token_endpoint:"https://bücher.example/token"))
  in
  check_string "metadata host is canonical"
    "https://xn--bcher-kva.example/token"
    (Uriz.to_string canonical.token_endpoint)

let test_metadata_cache () =
  let log, fetch =
    mock_seq [ (200, metadata_json); (200, metadata_json); (500, "{}") ]
  in
  let c = client_of fetch in
  ignore (Metadata.fetch_cached c);
  ignore (Metadata.fetch_cached c);
  check_int "cache hit avoids request" 1 (List.length !log);
  ignore (Metadata.fetch c);
  check_int "forced refresh" 2 (List.length !log);
  Metadata.invalidate_cache c;
  ignore (Metadata.fetch_cached c);
  check_int "invalidation refreshes" 3 (List.length !log)

let test_metadata_cache_failure_and_scope () =
  let log, fetch =
    mock_seq [ (200, metadata_json); (500, "{}"); (200, metadata_json) ]
  in
  let c = client_of fetch in
  ignore (Metadata.fetch_cached c);
  ignore (Metadata.fetch c);
  (* A failed forced refresh leaves the previous good value available. *)
  ignore (Metadata.fetch_cached c);
  check_int "failed refresh preserves cache" 2 (List.length !log);
  let c' =
    Client.with_session c
      {
        Client.user_id = uid "@alice:hs.example";
        access_token = "token";
        device_id = did "DEVICE1234";
        refresh_token = None;
      }
  in
  ignore (Metadata.fetch_cached c');
  check_int "credential client has independent cache" 3 (List.length !log)

let test_metadata_cache_expiry () =
  let log, fetch = mock_seq [ (200, metadata_json) ] in
  let client = client_of fetch in
  Client.Server_metadata_cache.set_oauth_metadata
    (Client.server_metadata_cache client)
    0. metadata_json;
  ignore (ok (Metadata.fetch_cached client));
  check_int "an expired entry is refreshed" 1 (List.length !log);
  let log, fetch = mock_seq [ (500, "{}") ] in
  let client = client_of fetch in
  Client.Server_metadata_cache.set_oauth_metadata
    (Client.server_metadata_cache client)
    0. metadata_json;
  let cached = ok (Metadata.fetch_cached client) in
  check_string "failed refresh returns stale metadata" "https://auth.example/"
    (Uriz.to_string cached.issuer);
  check_int "the failed refresh was attempted" 1 (List.length !log)

let test_metadata_cache_control_lifetime () =
  (* max-age=0 is an exact boundary: the entry is stale by the next lookup. *)
  let log, fetch =
    mock_seq_cache_control (Some "max-age=0")
      [ (200, metadata_json); (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "max-age controls revalidation" 2 (List.length !log);

  (* An absent or unrelated Cache-Control uses the compatibility fallback,
     retaining the old 24-hour behaviour. A malformed/negative max-age fails
     safe to immediate revalidation instead. *)
  let check_fallback value label =
    let log, fetch =
      mock_seq_cache_control (Some value)
        [ (200, metadata_json); (200, metadata_json) ]
    in
    let client = client_of fetch in
    ignore (ok (Metadata.fetch_cached client));
    ignore (ok (Metadata.fetch_cached client));
    check_int label 1 (List.length !log)
  in
  let log, fetch = mock_seq [ (200, metadata_json); (200, metadata_json) ] in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "missing Cache-Control uses fallback" 1 (List.length !log);
  check_fallback "public" "unrelated directive uses fallback";
  let check_revalidates value label =
    let log, fetch =
      mock_seq_cache_control (Some value)
        [ (200, metadata_json); (200, metadata_json) ]
    in
    let client = client_of fetch in
    ignore (ok (Metadata.fetch_cached client));
    ignore (ok (Metadata.fetch_cached client));
    check_int label 2 (List.length !log)
  in
  check_revalidates "max-age=garbage" "malformed max-age revalidates";
  check_revalidates "max-age=-1" "negative max-age revalidates";
  (* A valid but extreme lifetime is bounded by the compatibility ceiling. *)
  let log, fetch =
    mock_seq_cache_control (Some "max-age=999999999") [ (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  let cache =
    Client.Server_metadata_cache.get_oauth_metadata_with_expiry
      (Client.server_metadata_cache client)
  in
  (match cache with
  | Some (fetched_at, Some expires_at, _) ->
      check_bool "large max-age is capped" true
        (expires_at <= fetched_at +. 86_400.)
  | _ -> Alcotest.fail "metadata cache lost the bounded expiry");
  check_int "large max-age request" 1 (List.length !log)

let test_metadata_cache_control_directives () =
  let check_revalidates value label =
    let log, fetch =
      mock_seq_cache_control (Some value)
        [ (200, metadata_json); (200, metadata_json) ]
    in
    let client = client_of fetch in
    ignore (ok (Metadata.fetch_cached client));
    ignore (ok (Metadata.fetch_cached client));
    check_int label 2 (List.length !log)
  in
  check_revalidates "no-cache" "no-cache revalidates";
  check_revalidates "no-store" "no-store does not retain metadata";
  check_revalidates " Public, MAX-AGE = 0 "
    "mixed-case and whitespace directives parse"

let test_metadata_cache_control_typed () =
  (* Quoted delta-seconds and extension directives are parsed by Fetch's
     shared Cache-Control codec; commas inside quoted extension values must not
     split the field. *)
  let log, fetch =
    mock_seq_cache_control
      (Some "max-age=\"60\", x-matrix=\"a,b\", private=\"token\"")
      [ (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "quoted max-age and extensions are accepted" 1 (List.length !log);

  (* An unparseable max-age fails safe to immediate expiry, including when the
     shared codec preserves it as an extension. *)
  let log, fetch =
    mock_seq_cache_control (Some "max-age=\"not-a-number\"")
      [ (200, metadata_json); (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "malformed quoted max-age revalidates" 2 (List.length !log)

let test_metadata_cache_expires () =
  let future =
    Httpz.Date.format
      (Stdlib_upstream_compatible.Float_u.of_float
         (Unix.gettimeofday () +. 60.))
  in
  let past =
    Httpz.Date.format
      (Stdlib_upstream_compatible.Float_u.of_float
         (Unix.gettimeofday () -. 60.))
  in
  let log, fetch =
    mock_seq_cache_control ~expires:future None [ (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "future Expires keeps metadata fresh" 1 (List.length !log);

  let log, fetch =
    mock_seq_cache_control ~expires:past None
      [ (200, metadata_json); (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "past Expires revalidates" 2 (List.length !log);

  (* A valid Cache-Control lifetime wins over Expires, even when the latter is
     far in the future. *)
  let log, fetch =
    mock_seq_cache_control ~expires:future (Some "max-age=0")
      [ (200, metadata_json); (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "Cache-Control takes precedence over Expires" 2 (List.length !log)

let test_metadata_cache_monotonic_clock () =
  let clock = ref 100. in
  let now () = !clock in
  let log, fetch =
    mock_seq_cache_control (Some "max-age=10")
      [ (200, metadata_json); (200, metadata_json) ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached ~now client));
  (* A wall-clock-style rollback must not age the entry prematurely. *)
  clock := 90.;
  ignore (ok (Metadata.fetch_cached ~now client));
  check_int "monotonic rollback keeps cache fresh" 1 (List.length !log);
  clock := 111.;
  ignore (ok (Metadata.fetch_cached ~now client));
  check_int "monotonic age eventually expires" 2 (List.length !log)

let test_metadata_cache_control_well_known () =
  (* The issuer is off-origin, so this exercises the direct [Fetch] path that
     must preserve the metadata response headers just like Client.Http. *)
  let log, fetch =
    mock_seq_cache_control (Some "max-age=0")
      [
        (404, unrecognized);
        (404, unrecognized);
        (200, well_known_json);
        (200, metadata_json);
        (404, unrecognized);
        (404, unrecognized);
        (200, well_known_json);
        (200, metadata_json);
      ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached ~http:fetch client));
  ignore (ok (Metadata.fetch_cached ~http:fetch client));
  check_int "well-known metadata honours max-age" 8 (List.length !log)

let test_metadata_cache_control_unstable () =
  let log, fetch =
    mock_seq_cache_control (Some "max-age=0")
      [
        (404, unrecognized);
        (200, metadata_json);
        (404, unrecognized);
        (200, metadata_json);
      ]
  in
  let client = client_of fetch in
  ignore (ok (Metadata.fetch_cached client));
  ignore (ok (Metadata.fetch_cached client));
  check_int "unstable metadata honours max-age" 4 (List.length !log)

let test_metadata_unstable_fallback () =
  (* 404 M_UNRECOGNIZED on the stable path, MSC2965 name serves it. *)
  let log, fetch = mock_seq [ (404, unrecognized); (200, metadata_json) ] in
  let m = ok (Metadata.fetch (client_of fetch)) in
  check_strings "stable path first, then the MSC2965 one"
    [
      "https://hs.example/_matrix/client/v1/auth_metadata";
      "https://hs.example/_matrix/client/unstable/org.matrix.msc2965/auth_metadata";
    ]
    (urls log);
  check_string "issuer" "https://auth.example/" (Uriz.to_string m.issuer)

let test_metadata_well_known_fallback () =
  (* Neither endpoint exists; the well-known names an issuer, and OpenID
     Connect discovery against it is off-origin, so it needs ?http. *)
  let log, fetch =
    mock_seq
      [
        (404, unrecognized);
        (404, unrecognized);
        (200, well_known_json);
        (200, metadata_json);
      ]
  in
  let client = client_of fetch in
  let m = ok (Metadata.fetch ~http:fetch client) in
  check_strings "v1, unstable, well-known, then openid-configuration"
    [
      "https://hs.example/_matrix/client/v1/auth_metadata";
      "https://hs.example/_matrix/client/unstable/org.matrix.msc2965/auth_metadata";
      "https://hs.example/.well-known/matrix/client";
      "https://auth.example/.well-known/openid-configuration";
    ]
    (urls log);
  check_string "issuer" "https://auth.example/" (Uriz.to_string m.issuer);
  ignore (ok (Metadata.fetch_cached ~http:fetch client));
  check_int "the well-known result is cached" 4 (List.length !log)

let test_metadata_well_known_needs_http () =
  (* Without ?http the origin-restricted client cannot reach the issuer,
     and the library says so instead of trying. *)
  let _log, fetch =
    mock_seq
      [ (404, unrecognized); (404, unrecognized); (200, well_known_json) ]
  in
  match get_error (Metadata.fetch (client_of fetch)) with
  | Error.Policy_denied msg ->
      check_bool "names ?http" true (contains msg "?http")
  | e -> Alcotest.failf "expected Policy_denied, got %s" (Error.to_string e)

let test_metadata_none_anywhere () =
  let _log, fetch =
    mock_seq
      [
        (404, unrecognized);
        (404, unrecognized);
        (404, {|{"errcode":"M_NOT_FOUND"}|});
      ]
  in
  match get_error (Metadata.fetch (client_of fetch)) with
  | Error.Http_error { status = 404; _ } -> ()
  | e -> Alcotest.failf "expected 404, got %s" (Error.to_string e)

let test_metadata_validate () =
  check_bool "the spec's example validates" true
    (Result.is_ok (Metadata.validate (metadata ())));
  let no_s256 =
    {
      (metadata ()) with
      Metadata.code_challenge_methods_supported = [ "plain" ];
    }
  in
  check_bool "no S256 is rejected" true
    (Result.is_error (Metadata.validate no_s256));
  let no_refresh =
    {
      (metadata ()) with
      Metadata.grant_types_supported = [ "authorization_code" ];
    }
  in
  check_bool "no refresh_token grant is rejected" true
    (Result.is_error (Metadata.validate no_refresh));
  let no_query =
    { (metadata ()) with Metadata.response_modes_supported = [ "fragment" ] }
  in
  check_bool "no query response mode is rejected" true
    (Result.is_error (Metadata.validate no_query));
  let no_fragment =
    { (metadata ()) with Metadata.response_modes_supported = [ "query" ] }
  in
  check_bool "strict validation requires fragment response mode" true
    (Result.is_error (Metadata.validate no_fragment));
  check_bool "native loopback needs query but not fragment" true
    (Result.is_ok (Metadata.validate_loopback no_fragment));
  check_bool "native loopback rejects fragment-only metadata" true
    (Result.is_error (Metadata.validate_loopback no_query));
  let insecure =
    {
      (metadata ()) with
      Metadata.issuer = Uriz.of_string_exn "http://auth.example/";
      authorization_endpoint =
        Uriz.of_string_exn "http://auth.example/oauth2/auth";
      token_endpoint = Uriz.of_string_exn "http://auth.example/oauth2/token";
      registration_endpoint = None;
      revocation_endpoint = None;
      device_authorization_endpoint = None;
      account_management_uri = None;
    }
  in
  check_bool "http metadata is rejected by default" true
    (Result.is_error (Metadata.validate insecure));
  check_bool "http metadata has an explicit development escape hatch" true
    (Result.is_ok (Metadata.validate ~allow_insecure:true insecure));
  let queried_issuer =
    {
      (metadata ()) with
      Metadata.issuer = Uriz.of_string_exn "https://auth.example/?tenant=wrong";
    }
  in
  check_bool "issuer query is rejected" true
    (Result.is_error (Metadata.validate queried_issuer));
  let empty_query_issuer =
    {
      (metadata ()) with
      Metadata.issuer = Uriz.of_string_exn "https://auth.example/?";
    }
  in
  check_bool "even an empty issuer query is rejected" true
    (Result.is_error (Metadata.validate empty_query_issuer));
  let insecure_optional =
    {
      (metadata ()) with
      Metadata.revocation_endpoint =
        Some (Uriz.of_string_exn "http://auth.example/revoke");
    }
  in
  check_bool "optional endpoints are validated too" true
    (Result.is_error (Metadata.validate insecure_optional));

  let device_metadata =
    {
      (metadata ()) with
      Metadata.grant_types_supported =
        [ "refresh_token"; "urn:ietf:params:oauth:grant-type:device_code" ];
      response_types_supported = [];
      response_modes_supported = [];
      code_challenge_methods_supported = [];
    }
  in
  check_bool "device metadata needs no browser-only capabilities" true
    (Result.is_ok (Metadata.validate_device device_metadata));
  check_bool "device metadata requires its endpoint" true
    (Result.is_error
       (Metadata.validate_device
          { device_metadata with Metadata.device_authorization_endpoint = None }));
  check_bool "device metadata requires its grant advertisement" true
    (Result.is_error
       (Metadata.validate_device
          {
            device_metadata with
            Metadata.grant_types_supported = [ "refresh_token" ];
          }));
  check_bool "device metadata requires refresh-token support" true
    (Result.is_error
       (Metadata.validate_device
          {
            device_metadata with
            Metadata.grant_types_supported =
              [ "urn:ietf:params:oauth:grant-type:device_code" ];
          }))

let test_account_management_url () =
  let m = metadata () in
  let url a d =
    Option.map Uriz.to_string
      (Metadata.account_management_url m ?action:a ?device_id:d ())
  in
  check_str_opt "bare" (Some "https://auth.example/manage") (url None None);
  check_str_opt "profile"
    (Some "https://auth.example/manage?action=org.matrix.profile")
    (url (Some Oauth.Profile) None);
  check_str_opt "device_delete carries the device id"
    (Some
       "https://auth.example/manage?action=org.matrix.device_delete&device_id=ABCDEFGHIJ")
    (url (Some Oauth.Device_delete) (Some (did "ABCDEFGHIJ")));
  (* An action that does not name a device ignores the parameter. *)
  check_str_opt "profile drops the device id"
    (Some "https://auth.example/manage?action=org.matrix.profile")
    (url (Some Oauth.Profile) (Some (did "ABCDEFGHIJ")));
  check_str_opt "no URL advertised" None
    (Metadata.account_management_url
       { (metadata ()) with Metadata.account_management_uri = None }
       ()
    |> Option.map Uriz.to_string)

let test_scope_dialects () =
  let id = did "AAABBBCCCD" in
  check_string "unstable API scope" "urn:matrix:org.matrix.msc2967.client:api:*"
    Oauth.scope_api_unstable;
  check_string "unstable device scope"
    "urn:matrix:org.matrix.msc2967.client:device:AAABBBCCCD"
    (Oauth.scope_device_unstable id);
  check_bool "unstable device scope parses" true
    (Oauth.device_id_of_scope
       (Oauth.scope_api_unstable ^ " " ^ Oauth.scope_device_unstable id)
    = Some id);
  check_bool "stable and unstable device scopes are ambiguous" true
    (Oauth.device_id_of_scope
       (Oauth.scope_device id ^ " " ^ Oauth.scope_device_unstable id)
    = None)

let test_scope_dialect_selection () =
  let pkce = ok (Pkce.of_verifier (String.make 43 'a')) in
  let id = did "AAABBBCCCD" in
  let build m =
    let q =
      List.map
        (fun (k, value) -> (k, Option.value ~default:"" value))
        (Uriz.query_params ~plus_as_space:true
           (Authorization.build_url m ~client_id:"cid"
              ~redirect_uri:"http://127.0.0.1:1/callback" ~device_id:id
              ~state:"s" ~pkce ()))
    in
    Option.get (List.assoc_opt "scope" q)
  in
  let unstable =
    {
      (metadata ()) with
      Metadata.scopes_supported =
        [ Oauth.scope_api_unstable; Oauth.scope_device_prefix_unstable ];
    }
  in
  check_string "complete unstable metadata selects unstable pair"
    (Oauth.scope_api_unstable ^ " " ^ Oauth.scope_device_unstable id)
    (build unstable);
  let both =
    {
      unstable with
      Metadata.scopes_supported =
        [
          Oauth.scope_api;
          Oauth.scope_device_prefix;
          Oauth.scope_api_unstable;
          Oauth.scope_device_prefix_unstable;
        ];
    }
  in
  check_string "stable pair wins when both are advertised"
    (Oauth.scope_api ^ " " ^ Oauth.scope_device id)
    (build both);
  let incomplete =
    {
      (metadata ()) with
      Metadata.scopes_supported = [ Oauth.scope_api_unstable ];
    }
  in
  check_string "incomplete metadata retains stable default"
    (Oauth.scope_api ^ " " ^ Oauth.scope_device id)
    (build incomplete)

let test_account_action_aliases () =
  check_bool "sessions_list parses as Devices_list" true
    (Oauth.account_action_of_string "org.matrix.sessions_list"
    = Oauth.Devices_list);
  check_bool "session_view parses as Device_view" true
    (Oauth.account_action_of_string "org.matrix.session_view"
    = Oauth.Device_view);
  check_bool "session_end parses as Device_delete" true
    (Oauth.account_action_of_string "org.matrix.session_end"
    = Oauth.Device_delete);
  check_bool "deactivateaccount parses as Account_deactivate" true
    (Oauth.account_action_of_string "org.matrix.deactivateaccount"
    = Oauth.Account_deactivate);
  let alias_metadata aliases =
    {
      (metadata ()) with
      Metadata.account_management_actions_supported = aliases;
    }
  in
  check_bool "alias is supported" true
    (Metadata.supports_account_action
       (alias_metadata [ "org.matrix.sessions_list" ])
       Oauth.Devices_list);
  let url m action =
    Metadata.account_management_url m ~action () |> Option.map Uriz.to_string
  in
  check_str_opt "URL uses deployed alias"
    (Some "https://auth.example/manage?action=org.matrix.sessions_list")
    (url (alias_metadata [ "org.matrix.sessions_list" ]) Oauth.Devices_list);
  check_str_opt "URL prefers canonical spelling"
    (Some "https://auth.example/manage?action=org.matrix.devices_list")
    (url
       (alias_metadata
          [ "org.matrix.sessions_list"; "org.matrix.devices_list" ])
       Oauth.Devices_list)

(* Dynamic client registration. *)

let registration_reply =
  {|{"client_id":"s6BhdRkqt3","client_id_issued_at":1700000000,
     "client_uri":"https://example.com/","application_type":"native"}|}

let test_registration () =
  let log, fetch = mock_seq [ (201, registration_reply) ] in
  let cm =
    Registration.v ~client_uri:"https://example.com/"
      ~client_name:
        {
          Registration.value = "My App";
          translations = [ ("fr", "Mon application") ];
        }
      ~redirect_uris:[ Registration.loopback_redirect_uri ]
      ~sector_identifier_uri:"https://example.com/sector"
      ~software_id:"org.example.matrix-client" ~software_version:"1.2.3"
      ~software_statement:"eyJhbGciOiJSUzI1NiJ9.statement"
      ~jwks_uri:"https://example.com/.well-known/jwks.json" ()
  in
  let r =
    ok (Registration.register ~http:fetch (client_of fetch) (metadata ()) cm)
  in
  let req = one_request log in
  check_string "method" "POST" req.meth;
  check_string "url" "https://auth.example/oauth2/clients/register" req.url;
  check_str_opt "content type" (Some "application/json")
    (header req "content-type");
  let body = Option.get req.body in
  let json =
    ok (Client.Http.decode_response Matrix_proto.Json.Codec.json body)
  in
  let members =
    match json with
    | Jsont.Object (ms, _) -> List.map (fun ((n, _), v) -> (n, v)) ms
    | _ -> Alcotest.fail "registration body is not a JSON object"
  in
  let str name =
    match List.assoc_opt name members with
    | Some (Jsont.String (s, _)) -> Some s
    | _ -> None
  in
  let strs name =
    match List.assoc_opt name members with
    | Some (Jsont.Array (l, _)) ->
        Some (List.map (function Jsont.String (s, _) -> s | _ -> "?") l)
    | _ -> None
  in
  check_str_opt "client_uri" (Some "https://example.com/") (str "client_uri");
  check_str_opt "client_name" (Some "My App") (str "client_name");
  check_str_opt "localised client_name" (Some "Mon application")
    (str "client_name#fr");
  check_str_opt "token_endpoint_auth_method" (Some "none")
    (str "token_endpoint_auth_method");
  check_str_opt "application_type" (Some "native") (str "application_type");
  check_str_opt "sector_identifier_uri" (Some "https://example.com/sector")
    (str "sector_identifier_uri");
  check_str_opt "software_id" (Some "org.example.matrix-client")
    (str "software_id");
  check_str_opt "software_version" (Some "1.2.3") (str "software_version");
  check_str_opt "software_statement" (Some "eyJhbGciOiJSUzI1NiJ9.statement")
    (str "software_statement");
  check_str_opt "jwks_uri" (Some "https://example.com/.well-known/jwks.json")
    (str "jwks_uri");
  Alcotest.(check (option (list string)))
    "redirect_uris" (Some [ "http://127.0.0.1/callback" ])
    (strs "redirect_uris");
  Alcotest.(check (option (list string)))
    "response_types" (Some [ "code" ]) (strs "response_types");
  Alcotest.(check (option (list string)))
    "grant_types"
    (Some [ "authorization_code"; "refresh_token" ])
    (strs "grant_types");
  check_string "client_id" "s6BhdRkqt3" r.client_id;
  check_str_opt "client_id_issued_at" (Some "2023-11-14T22:13:20Z")
    (Option.map (Ptime.to_rfc3339 ~tz_offset_s:0) r.client_id_issued_at);
  check_bool "the echoed metadata is kept" true
    (List.mem_assoc "application_type" r.registered)

let test_registration_without_endpoint () =
  let _log, fetch = mock_seq [] in
  let m = { (metadata ()) with Metadata.registration_endpoint = None } in
  let cm =
    Registration.v ~client_uri:"https://example.com/" ~redirect_uris:[] ()
  in
  match
    get_error (Registration.register ~http:fetch (client_of fetch) m cm)
  with
  | Error.Json_error _ -> ()
  | e -> Alcotest.failf "expected Json_error, got %s" (Error.to_string e)

let test_registration_jwks_validation () =
  let jwks =
    ok
      (Client.Http.decode_response Matrix_proto.Json.Codec.json {|{"keys":[]}|})
  in
  let inline =
    Registration.v ~client_uri:"https://example.com/"
      ~redirect_uris:[ Registration.loopback_redirect_uri ]
      ~jwks ()
  in
  let inline_json = Registration.to_json inline in
  (match inline_json with
  | Jsont.Object (members, _) ->
      check_bool "inline jwks is encoded" true
        (List.exists (fun ((name, _), _) -> String.equal name "jwks") members)
  | _ -> Alcotest.fail "registration metadata was not encoded as an object");
  let raised_both =
    try
      ignore
        (Registration.v ~client_uri:"https://example.com/"
           ~redirect_uris:[ Registration.loopback_redirect_uri ]
           ~jwks ~jwks_uri:"https://example.com/jwks" ());
      false
    with Invalid_argument _ -> true
  in
  check_bool "jwks and jwks_uri are mutually exclusive" true raised_both;
  let raised_shape =
    try
      ignore
        (Registration.v ~client_uri:"https://example.com/"
           ~redirect_uris:[ Registration.loopback_redirect_uri ]
           ~jwks:(Jsont.Json.string "not-a-jwks")
           ());
      false
    with Invalid_argument _ -> true
  in
  check_bool "jwks must be an object" true raised_shape

let test_device_registration_metadata () =
  check_strings "browser defaults remain unchanged"
    [ "authorization_code"; "refresh_token" ]
    Matrix_eio.Oauth.default_client_metadata.grant_types;
  check_bool "device registration advertises RFC 8628" true
    (List.mem "urn:ietf:params:oauth:grant-type:device_code"
       Matrix_eio.Oauth.default_device_client_metadata.grant_types)

let test_browser_registration_uses_bound_port () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (200, metadata_json);
        (201, registration_reply);
        (200, {|{"access_token":"at","token_type":"Bearer","expires_in":600}|});
        (200, {|{"user_id":"@alice:hs.example"}|});
      ]
  in
  let client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn default_homeserver)
      ~fetch ()
  in
  let authorization_url = ref None in
  let open_url url =
    authorization_url := Some url;
    let authorization = Uriz.of_string_exn url in
    let redirect_uri =
      match
        Uriz.find_query ~plus_as_space:true authorization "redirect_uri"
      with
      | This uri -> uri
      | _ -> Alcotest.fail "authorisation URL had no redirect_uri"
    in
    let state =
      match Uriz.find_query ~plus_as_space:true authorization "state" with
      | This state -> state
      | _ -> Alcotest.fail "authorisation URL had no state"
    in
    let callback =
      Uriz.add_query_params
        (Uriz.of_string_exn redirect_uri)
        [ ("code", "browser-code"); ("state", state) ]
    in
    let callback_target = Uriz.encoded_path_and_query callback in
    let port =
      match Uriz.port callback with
      | This port -> port
      | Null -> Alcotest.fail "redirect URI had no bound port"
    in
    (* Deliver the callback synchronously. The listener has already been
       bound, and TCP keeps the request queued until [wait] accepts it. *)
    Eio.Switch.run @@ fun callback_sw ->
    let flow =
      Eio.Net.connect ~sw:callback_sw (Eio.Stdenv.net env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    Eio.Flow.copy_string
      (Printf.sprintf
         "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
         callback_target)
      flow;
    Eio.Flow.shutdown flow `Send;
    ignore Eio.Buf_read.(parse_exn ~max_size:65536 take_all flow)
  in
  let result =
    Matrix_eio.Oauth.login_with_browser_full_expiry ~env ~http:fetch client
      ~open_url ()
  in
  check_string "registered client id" "s6BhdRkqt3" result.client_id;
  check_bool "browser login retains token expiry" true
    (Option.is_some result.expires_at);
  let requests = requests log in
  let request_at_url url =
    match
      List.find_opt (fun request -> String.equal request.url url) requests
    with
    | Some request -> request
    | None -> Alcotest.failf "no request to %s" url
  in
  let registration =
    request_at_url "https://auth.example/oauth2/clients/register"
  in
  let registration_json =
    ok
      (Client.Http.decode_response Matrix_proto.Json.Codec.json
         (Option.get registration.body))
  in
  let registered_redirect_uri =
    match registration_json with
    | Jsont.Object (members, _) -> (
        match
          List.assoc_opt "redirect_uris"
            (List.map (fun ((n, _), v) -> (n, v)) members)
        with
        | Some (Jsont.Array ([ Jsont.String (uri, _) ], _)) -> uri
        | _ -> Alcotest.fail "registration body had no single redirect URI")
    | _ -> Alcotest.fail "registration body was not a JSON object"
  in
  let authorization = Uriz.of_string_exn (Option.get !authorization_url) in
  let authorization_redirect_uri =
    match Uriz.find_query ~plus_as_space:true authorization "redirect_uri" with
    | This uri -> uri
    | _ -> Alcotest.fail "authorisation request had no redirect URI"
  in
  check_string "registration and authorisation use the exact bound URI"
    authorization_redirect_uri registered_redirect_uri;
  check_bool "redirect URI has an ephemeral port" true
    (Uriz.port (Uriz.of_string_exn registered_redirect_uri) <> Null);
  let exchange = request_at_url "https://auth.example/oauth2/token" in
  let exchange_redirect_uri =
    match Option.map (fun body -> form_of body) exchange.body with
    | Some fields -> (
        match List.assoc_opt "redirect_uri" fields with
        | Some uri -> uri
        | None -> Alcotest.fail "token exchange had no redirect URI")
    | None -> Alcotest.fail "token exchange had no body"
  in
  check_string "token exchange uses the same bound URI"
    authorization_redirect_uri exchange_redirect_uri

(* PKCE. *)

(* RFC 7636 appendix B, the worked example. *)
let rfc7636_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
let rfc7636_challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

let test_pkce_known_answer () =
  let p = ok (Pkce.of_verifier rfc7636_verifier) in
  check_string "verifier round-trips" rfc7636_verifier p.verifier;
  check_string "challenge" rfc7636_challenge p.challenge;
  check_string "method" "S256" Pkce.challenge_method

let test_pkce_generated () =
  Eio_mock.Backend.run @@ fun () ->
  let random = Matrix_client.Random.of_env mock_env in
  let p = Pkce.create ~random () in
  check_int "43 characters, the RFC 7636 minimum" 43 (String.length p.verifier);
  check_bool "unreserved alphabet only" true
    (String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
         | _ -> false)
       p.verifier);
  check_string "challenge matches the verifier"
    (ok (Pkce.of_verifier p.verifier)).challenge p.challenge;
  check_int "128 characters at the top of the range" 128
    (String.length (Pkce.create ~bytes:96 ~random ()).verifier)

let test_pkce_rejects_bad_verifiers () =
  check_bool "too short" true (Result.is_error (Pkce.of_verifier "abc"));
  check_bool "too long" true
    (Result.is_error (Pkce.of_verifier (String.make 129 'a')));
  check_bool "reserved character" true
    (Result.is_error (Pkce.of_verifier (String.make 42 'a' ^ "/")));
  check_bool "43 unreserved characters" true
    (Result.is_ok (Pkce.of_verifier (String.make 43 'a')))

(* The authorisation request. *)

let query_of uri =
  List.map
    (fun (k, value) -> (k, Option.value ~default:"" value))
    (Uriz.query_params ~plus_as_space:true uri)

let test_authorization_url () =
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let device_id = did "AAABBBCCCD" in
  let url =
    Authorization.build_url (metadata ()) ~client_id:"s6BhdRkqt3"
      ~redirect_uri:"http://127.0.0.1:41234/callback" ~device_id
      ~state:"opaquestate" ~pkce ()
  in
  check_string "endpoint" "https://auth.example/oauth2/auth"
    (Uriz.to_string (Uriz.with_query url Null));
  let q = query_of url in
  let p name = List.assoc_opt name q in
  check_str_opt "response_type" (Some "code") (p "response_type");
  check_str_opt "client_id" (Some "s6BhdRkqt3") (p "client_id");
  check_str_opt "redirect_uri" (Some "http://127.0.0.1:41234/callback")
    (p "redirect_uri");
  check_str_opt "scope"
    (Some "urn:matrix:client:api:* urn:matrix:client:device:AAABBBCCCD")
    (p "scope");
  check_str_opt "state" (Some "opaquestate") (p "state");
  check_str_opt "response_mode defaults to query" (Some "query")
    (p "response_mode");
  check_str_opt "code_challenge" (Some rfc7636_challenge) (p "code_challenge");
  check_str_opt "code_challenge_method" (Some "S256")
    (p "code_challenge_method");
  check_str_opt "no prompt by default" None (p "prompt");
  (* The device scope is the one the server allocates the device from. *)
  check_bool "device id recoverable from the scope" true
    (Oauth.device_id_of_scope (Option.get (p "scope")) = Some device_id)

let test_authorization_url_options () =
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let url =
    Authorization.build_url ~prompt:"create"
      ~login_hint:"mxid:@alice:example.org" ~response_mode:"fragment"
      (metadata ()) ~client_id:"cid" ~redirect_uri:"https://app.example/cb"
      ~device_id:(did "AAABBBCCCD") ~state:"s" ~pkce ()
  in
  let q = query_of url in
  check_str_opt "prompt" (Some "create") (List.assoc_opt "prompt" q);
  check_str_opt "login_hint" (Some "mxid:@alice:example.org")
    (List.assoc_opt "login_hint" q);
  check_str_opt "response_mode" (Some "fragment")
    (List.assoc_opt "response_mode" q)

let test_authorization_request_generates () =
  Eio_mock.Backend.run @@ fun () ->
  let random = Matrix_client.Random.of_env mock_env in
  let r =
    Authorization.request ~random (metadata ()) ~client_id:"cid"
      ~redirect_uri:"http://127.0.0.1:1/callback" ()
  in
  check_int "device id is ten characters" 10
    (String.length (Id.Device_id.to_string r.device_id));
  check_bool "device id is uppercase alphanumeric" true
    (String.for_all
       (function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)
       (Id.Device_id.to_string r.device_id));
  check_string "scope"
    ("urn:matrix:client:api:* urn:matrix:client:device:"
    ^ Id.Device_id.to_string r.device_id)
    r.scope;
  check_bool "state is not empty" true (r.state <> "");
  let q = query_of r.url in
  check_str_opt "state is in the URL" (Some r.state) (List.assoc_opt "state" q);
  check_str_opt "challenge is in the URL" (Some r.pkce.challenge)
    (List.assoc_opt "code_challenge" q)

(* RFC 8628 device authorisation. *)

let device_reply =
  {|{"device_code":"device-secret","user_code":"ABCD-EFGH",
     "verification_uri":"https://auth.example/verify",
     "verification_uri_complete":"https://auth.example/verify?user_code=ABCD-EFGH",
     "expires_in":600}|}

let test_device_codec () =
  let d =
    ok (Client.Http.decode_response Device_authorization.jsont device_reply)
  in
  check_string "device code" "device-secret" d.device_code;
  check_string "user code" "ABCD-EFGH" d.user_code;
  check_string "verification URI" "https://auth.example/verify"
    (Uriz.to_string d.verification_uri);
  check_str_opt "complete verification URI"
    (Some "https://auth.example/verify?user_code=ABCD-EFGH")
    (Option.map Uriz.to_string d.verification_uri_complete);
  check_int "expiry" 600 d.expires_in;
  check_int "RFC default interval" 5 d.interval;
  let rejects json =
    check_bool "invalid device response rejected" true
      (Result.is_error
         (Client.Http.decode_response Device_authorization.jsont json))
  in
  rejects
    {|{"device_code":"","user_code":"u","verification_uri":"https://a/","expires_in":1}|};
  rejects
    {|{"device_code":"d","user_code":"u","verification_uri":"/verify","expires_in":1}|};
  rejects
    {|{"device_code":"d","user_code":"u","verification_uri":"https://a/","expires_in":0}|};
  rejects
    {|{"device_code":"d","user_code":"u","verification_uri":"https://a/","expires_in":1,"interval":0}|}

let test_device_request () =
  let log, fetch = mock_seq [ (200, device_reply) ] in
  let id = did "AAABBBCCCD" in
  let d =
    ok
      (Device_authorization.request ~http:fetch (client_of fetch) (metadata ())
         ~client_id:"client" ~device_id:id ())
  in
  let req = one_request log in
  check_string "method" "POST" req.meth;
  check_string "device endpoint" "https://auth.example/oauth2/device" req.url;
  check_str_opt "form content type" (Some "application/x-www-form-urlencoded")
    (header req "content-type");
  Alcotest.(check (list (pair string string)))
    "exact form fields"
    [
      ("client_id", "client");
      ("scope", "urn:matrix:client:api:* urn:matrix:client:device:AAABBBCCCD");
    ]
    (form_of (Option.get req.body));
  check_string "device code" "device-secret" d.device_code

let test_device_request_unstable_scope () =
  let log, fetch = mock_seq [ (200, device_reply) ] in
  let m =
    {
      (metadata ()) with
      Metadata.scopes_supported =
        [ Oauth.scope_api_unstable; Oauth.scope_device_prefix_unstable ];
    }
  in
  let _ =
    ok
      (Device_authorization.request ~http:fetch (client_of fetch) m
         ~client_id:"client" ~device_id:(did "AAABBBCCCD") ())
  in
  check_string "unstable scope pair is selected"
    (Oauth.scope_api_unstable ^ " "
    ^ Oauth.scope_device_unstable (did "AAABBBCCCD"))
    (List.assoc "scope" (form_of (Option.get (one_request log).body)))

let test_device_poll () =
  let log, fetch =
    mock_seq
      [
        ( 200,
          {|{"access_token":"at","token_type":"Bearer","scope":"urn:matrix:client:api:* urn:matrix:client:device:AAABBBCCCD"}|}
        );
      ]
  in
  let _ =
    match
      Device_authorization.poll ~http:fetch (client_of fetch) (metadata ())
        ~client_id:"client" ~device_code:"device-secret" ()
    with
    | Ok tokens -> tokens
    | Error _ -> Alcotest.fail "successful device poll was rejected"
  in
  let req = one_request log in
  check_string "token endpoint" "https://auth.example/oauth2/token" req.url;
  Alcotest.(check (list (pair string string)))
    "exact device grant fields"
    [
      ("grant_type", "urn:ietf:params:oauth:grant-type:device_code");
      ("device_code", "device-secret");
      ("client_id", "client");
    ]
    (form_of (Option.get req.body));
  let classify code =
    let _log, fetch =
      mock_seq [ (400, Printf.sprintf {|{"error":"%s"}|} code) ]
    in
    match
      Device_authorization.poll ~http:fetch (client_of fetch) (metadata ())
        ~client_id:"client" ~device_code:"device-secret" ()
    with
    | Ok _ -> Alcotest.failf "expected %s to be an error" code
    | Error e -> e
  in
  let check code expected =
    match classify code with
    | expected' when expected' = expected -> ()
    | _ -> Alcotest.failf "wrong classification for %s" code
  in
  check "authorization_pending" Device_authorization.Authorization_pending;
  check "slow_down" Device_authorization.Slow_down;
  check "access_denied" Device_authorization.Access_denied;
  check "expired_token" Device_authorization.Expired_token;
  match classify "server_specific" with
  | Device_authorization.OAuth_error { error; _ } ->
      check_string "unknown OAuth error retained" "server_specific" error
  | Device_authorization.Transport_error _ ->
      Alcotest.fail "unknown OAuth error was treated as transport failure"
  | _ -> Alcotest.fail "unknown OAuth error was classified as a standard error"

(* The callback. *)

let test_parse_redirect_query () =
  let uri =
    Uriz.of_string_exn "http://127.0.0.1:41234/callback?state=abc&code=xyz"
  in
  let r = Result.get_ok (Authorization.parse_redirect uri) in
  check_string "code" "xyz" r.code;
  check_string "state" "abc" r.state

let test_parse_redirect_fragment () =
  (* [response_mode=fragment] encodes the same parameters after the '#'. *)
  let uri = Uriz.of_string_exn "https://app.example/cb#state=abc&code=xyz" in
  let r = Result.get_ok (Authorization.parse_redirect uri) in
  check_string "code" "xyz" r.code;
  check_string "state" "abc" r.state

let test_parse_redirect_escaping () =
  List.iter
    (fun delimiter ->
      let uri =
        Uriz.of_string_exn
          ("https://app.example/cb" ^ delimiter
         ^ "state=a%26code%3Dinjected%2B+%25%2526&code=x,y%2Cz%3D%23")
      in
      let r = Result.get_ok (Authorization.parse_redirect uri) in
      check_string "state decoded once" "a&code=injected+ %%26" r.state;
      check_string "commas and delimiters stay in one value" "x,y,z=#" r.code)
    [ "?"; "#" ]

let test_parse_redirect_denied () =
  let uri =
    Uriz.of_string_exn
      "https://app.example/cb#state=abc&error=access_denied&error_description=The+resource+owner+or+authorization+server+denied+the+request.&error_uri=https%3A%2F%2Ferrors.example.com%2F"
  in
  match Authorization.parse_redirect uri with
  | Ok _ -> Alcotest.fail "expected the denial to be reported"
  | Error (Authorization.Denied e) ->
      check_string "error" "access_denied" e.error;
      check_str_opt "description"
        (Some "The resource owner or authorization server denied the request.")
        e.error_description;
      check_str_opt "uri" (Some "https://errors.example.com/") e.error_uri
  | Error (Authorization.Malformed s) ->
      Alcotest.failf "expected Denied, got Malformed %s" s

let test_parse_redirect_malformed () =
  let malformed s =
    match Authorization.parse_redirect (Uriz.of_string_exn s) with
    | Error (Authorization.Malformed m) -> m
    | Ok _ -> Alcotest.failf "expected Malformed for %s" s
    | Error (Authorization.Denied _) ->
        Alcotest.failf "expected Malformed for %s" s
  in
  check_string "no parameters at all" "no code parameter"
    (malformed "http://127.0.0.1/callback");
  check_string "code without state" "no state parameter"
    (malformed "http://127.0.0.1/callback?code=xyz");
  check_string "state without code" "no code parameter"
    (malformed "http://127.0.0.1/callback?state=abc");
  check_string "duplicate state" "duplicate state parameter"
    (malformed "http://127.0.0.1/callback?code=xyz&state=abc&state=def");
  check_string "duplicate code across response modes" "duplicate code parameter"
    (malformed "http://127.0.0.1/callback?code=xyz&state=abc#code=other");
  check_string "encoded duplicate key" "duplicate state parameter"
    (malformed "https://app.example/cb?code=xyz&state=abc&%73tate=def");
  check_string "valueless duplicate key" "duplicate code parameter"
    (malformed "https://app.example/cb#state=abc&code&code=xyz")

(* Token exchange, refresh and revocation. *)

let token_reply =
  {|{"access_token":"2YotnFZFEjr1zCsicMWpAA","token_type":"Bearer",
     "expires_in":299,"refresh_token":"tGz3JOkF0XG5Qx2TlKWIA",
     "scope":"urn:matrix:client:api:* urn:matrix:client:device:AAABBBCCCD"}|}

let test_exchange () =
  let log, fetch = mock_seq [ (200, token_reply) ] in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let t =
    ok
      (Token.exchange ~http:fetch (client_of fetch) (metadata ())
         ~client_id:"s6BhdRkqt3" ~redirect_uri:"http://127.0.0.1:41234/callback"
         ~code:"iuB7Eiz9" ~pkce ())
  in
  let req = one_request log in
  check_string "method" "POST" req.meth;
  check_string "url" "https://auth.example/oauth2/token" req.url;
  check_str_opt "content type" (Some "application/x-www-form-urlencoded")
    (header req "content-type");
  let body = Option.get req.body in
  check_bool "the body is form-encoded, grant type first" true
    (String.starts_with ~prefix:"grant_type=authorization_code&" body);
  let f = form_of body in
  Alcotest.(check (list (pair string string)))
    "form fields, in order"
    [
      ("grant_type", "authorization_code");
      ("code", "iuB7Eiz9");
      ("redirect_uri", "http://127.0.0.1:41234/callback");
      ("client_id", "s6BhdRkqt3");
      ("code_verifier", rfc7636_verifier);
    ]
    f;
  check_string "access token" "2YotnFZFEjr1zCsicMWpAA" t.access_token;
  check_string "token type" "Bearer" t.token_type;
  check_bool "an expiry was computed from expires_in" true (t.expires_at <> None);
  check_bool "and it has not passed yet" false (Token.is_expired t);
  check_str_opt "refresh token" (Some "tGz3JOkF0XG5Qx2TlKWIA") t.refresh_token;
  check_bool "device id from the granted scope" true
    (Oauth.device_id_of_scope (Option.get t.scope) = Some (did "AAABBBCCCD"))

let test_exchange_error_is_readable () =
  let _log, fetch =
    mock_seq
      [
        ( 400,
          {|{"error":"invalid_grant","error_description":"code has expired"}|}
        );
      ]
  in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let e =
    get_error
      (Token.exchange ~http:fetch (client_of fetch) (metadata ())
         ~client_id:"cid" ~redirect_uri:"http://127.0.0.1:1/cb" ~code:"c" ~pkce
         ())
  in
  match Oauth.oauth_error_of_error e with
  | Some oe ->
      check_string "error code" "invalid_grant" oe.error;
      check_str_opt "description" (Some "code has expired") oe.error_description
  | None -> Alcotest.failf "expected an OAuth error, got %s" (Error.to_string e)

let test_refresh () =
  let log, fetch = mock_seq [ (200, token_reply) ] in
  let t =
    ok
      (Token.refresh ~http:fetch (client_of fetch) (metadata ())
         ~client_id:"s6BhdRkqt3" ~refresh_token:"old-refresh" ())
  in
  let req = one_request log in
  check_string "url" "https://auth.example/oauth2/token" req.url;
  Alcotest.(check (list (pair string string)))
    "form fields"
    [
      ("grant_type", "refresh_token");
      ("refresh_token", "old-refresh");
      ("client_id", "s6BhdRkqt3");
    ]
    (form_of (Option.get req.body));
  check_str_opt "the reply rotates the refresh token"
    (Some "tGz3JOkF0XG5Qx2TlKWIA") t.refresh_token;
  check_bool "refresh preserves the returned expiry" true
    (Option.is_some t.expires_at)

let test_revoke () =
  let log, fetch = mock_seq [ (200, "") ] in
  ok
    (Token.revoke ~http:fetch ~token_type_hint:`Access_token (client_of fetch)
       (metadata ()) ~client_id:"s6BhdRkqt3" ~token:"mat_secret" ());
  let req = one_request log in
  check_string "method" "POST" req.meth;
  check_string "url" "https://auth.example/oauth2/revoke" req.url;
  check_str_opt "content type" (Some "application/x-www-form-urlencoded")
    (header req "content-type");
  Alcotest.(check (list (pair string string)))
    "form fields"
    [
      ("token", "mat_secret");
      ("token_type_hint", "access_token");
      ("client_id", "s6BhdRkqt3");
    ]
    (form_of (Option.get req.body))

let test_logout_revokes_both () =
  let log, fetch = mock_seq [ (200, ""); (200, "") ] in
  let tokens =
    {
      Token.access_token = "at";
      token_type = "Bearer";
      refresh_token = Some "rt";
      scope = None;
      expires_at = None;
    }
  in
  ok
    (Token.logout ~http:fetch (client_of fetch) (metadata ()) ~client_id:"cid"
       tokens);
  let hints =
    List.map
      (fun r -> List.assoc "token_type_hint" (form_of (Option.get r.body)))
      (requests log)
  in
  check_strings "access token then refresh token"
    [ "access_token"; "refresh_token" ]
    hints

let test_revoke_without_endpoint () =
  let _log, fetch = mock_seq [] in
  let m = { (metadata ()) with Metadata.revocation_endpoint = None } in
  match
    get_error
      (Token.revoke ~http:fetch (client_of fetch) m ~client_id:"c" ~token:"t" ())
  with
  | Error.Json_error _ -> ()
  | e -> Alcotest.failf "expected Json_error, got %s" (Error.to_string e)

(* An authorisation server on the homeserver's own origin needs no separate
   HTTP client; the request goes through the Matrix one. *)
let test_same_origin_needs_no_http () =
  let log, fetch = mock_seq [ (200, token_reply) ] in
  let m =
    {
      (metadata ()) with
      Metadata.token_endpoint =
        Uriz.of_string_exn
          "https://HS.EXAMPLE:443/oauth2/token?aud=a&aud=b&return=x%2Fy";
    }
  in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let _ =
    ok
      (Token.exchange (client_of fetch) m ~client_id:"cid"
         ~redirect_uri:"http://127.0.0.1:1/cb" ~code:"c" ~pkce ())
  in
  check_strings "went to the homeserver origin"
    [ "https://hs.example/oauth2/token?aud=a&aud=b&return=x%2Fy" ]
    (urls log)

let test_transport_error_redacts_endpoint_query () =
  let marker = "matrix-oauth-query-secret" in
  let userinfo_marker = "matrix-oauth-userinfo-secret" in
  let fetch =
    Fetch_mock.client (fun _req ->
        raise (Fetch.err (Fetch.Protocol_error "truncated")))
  in
  let m =
    {
      (metadata ()) with
      Metadata.token_endpoint =
        Uriz.of_string_exn
          ("https://auth.example/oauth2/token?opaque=" ^ marker);
    }
  in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  (match
     get_error
       (Token.exchange ~http:fetch (client_of fetch) m ~client_id:"cid"
          ~redirect_uri:"http://127.0.0.1:1/cb" ~code:"c" ~pkce ())
   with
  | Error.Network_error msg ->
      check_bool "operation remains useful" true
        (contains msg "POST https://auth.example/oauth2/token");
      check_bool "query value is absent" false (contains msg marker);
      check_bool "typed cause remains useful" true (contains msg "truncated")
  | e -> Alcotest.failf "expected Network_error, got %s" (Error.to_string e));
  let m =
    {
      (metadata ()) with
      Metadata.token_endpoint =
        Uriz.of_string_exn
          ("https://user:" ^ userinfo_marker ^ "@auth.example/oauth2/token");
    }
  in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  match
    get_error
      (Token.exchange ~http:fetch (client_of fetch) m ~client_id:"cid"
         ~redirect_uri:"http://127.0.0.1:1/cb" ~code:"c" ~pkce ())
  with
  | Error.Json_error msg ->
      check_bool "userinfo value is absent" false (contains msg userinfo_marker)
  | e -> Alcotest.failf "expected Json_error, got %s" (Error.to_string e)

let test_off_origin_without_http_is_refused () =
  let log, fetch = mock_seq [] in
  let pkce = ok (Pkce.of_verifier rfc7636_verifier) in
  let e =
    get_error
      (Token.exchange (client_of fetch) (metadata ()) ~client_id:"cid"
         ~redirect_uri:"http://127.0.0.1:1/cb" ~code:"c" ~pkce ())
  in
  check_strings "no request was attempted" [] (urls log);
  match e with
  | Error.Policy_denied _ -> ()
  | e -> Alcotest.failf "expected Policy_denied, got %s" (Error.to_string e)

(* Building a session. *)

let test_finish_login () =
  let log, fetch = mock_seq [ (200, {|{"user_id":"@alice:example.org"}|}) ] in
  let client = client_of fetch in
  let provisional = Client.with_access_token client "2YotnFZFEjr1zCsicMWpAA" in
  check_bool "an access-token client carries no session" false
    (Option.is_some (Client.session provisional));
  let tokens =
    {
      Token.access_token = "2YotnFZFEjr1zCsicMWpAA";
      token_type = "Bearer";
      refresh_token = Some "tGz3JOkF0XG5Qx2TlKWIA";
      scope = Some "urn:matrix:client:api:* urn:matrix:client:device:AAABBBCCCD";
      expires_at = None;
    }
  in
  let session =
    ok (Token.finish_login client ~device_id:(did "AAABBBCCCD") tokens)
  in
  let req = one_request log in
  check_string "whoami" "https://hs.example/_matrix/client/v3/account/whoami"
    req.url;
  check_str_opt "the new token is used" (Some "Bearer 2YotnFZFEjr1zCsicMWpAA")
    (header req "authorization");
  check_string "user id" "@alice:example.org"
    (Id.User_id.to_string session.user_id);
  check_string "device id" "AAABBBCCCD"
    (Id.Device_id.to_string session.device_id);
  check_str_opt "refresh token is carried into the session"
    (Some "tGz3JOkF0XG5Qx2TlKWIA") session.refresh_token

(* The loopback redirect listener needs a real socket, so it runs under
   [Eio_main.run] rather than the mock backend. *)

let test_loopback () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listener = Matrix_eio.Oauth.Loopback.create ~sw ~env net in
  let port = Matrix_eio.Oauth.Loopback.port listener in
  check_bool "an ephemeral port was bound" true (port > 0);
  check_string "redirect uri carries the bound port"
    (Printf.sprintf "http://127.0.0.1:%d/callback" port)
    (Matrix_eio.Oauth.Loopback.redirect_uri listener);
  let received = ref None and response = ref "" in
  Eio.Fiber.both
    (fun () -> received := Some (Matrix_eio.Oauth.Loopback.wait listener))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow =
        Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      in
      Eio.Flow.copy_string
        "GET /callback?code=abc&state=s1 HTTP/1.1\r\n\
         Host: 127.0.0.1\r\n\
         Connection: close\r\n\
         \r\n"
        flow;
      Eio.Flow.shutdown flow `Send;
      response := Eio.Buf_read.(parse_exn ~max_size:65536 take_all flow));
  let uri = Option.get !received in
  let r = Result.get_ok (Authorization.parse_redirect uri) in
  check_bool "callback URI carries the listening port" true
    (Uriz.port uri = This port);
  check_string "code" "abc" r.code;
  check_string "state" "s1" r.state;
  check_bool "served a 200" true
    (String.starts_with ~prefix:"HTTP/1.1 200 OK" !response);
  check_bool "typed HTML content type" true
    (contains !response "Content-Type: text/html; charset=utf-8");
  check_bool "no-store cache policy" true
    (contains !response "Cache-Control: no-store");
  check_bool "and a page telling the user to close this tab" true
    (contains !response "close this tab")

let test_login_timeout () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let _, fetch = mock_seq [ (200, metadata_json) ] in
  let client =
    Matrix_eio.Client.create ~sw ~env
      ~homeserver:(Uriz.of_string_exn default_homeserver)
      ~fetch ()
  in
  try
    ignore
      (Matrix_eio.Oauth.login_with_browser ~env ~http:fetch ~client_id:"cid"
         ~timeout:0. client
         ~open_url:(fun _ -> ())
         ());
    Alcotest.fail "OAuth login did not time out"
  with Eio.Io (Matrix_eio.Oauth.E Matrix_eio.Oauth.Timeout, _) -> ()

let test_loopback_rejects_and_continues () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listener = Matrix_eio.Oauth.Loopback.create ~sw ~env net in
  let request raw =
    Eio.Switch.run @@ fun request_sw ->
    let flow =
      Eio.Net.connect ~sw:request_sw net
        (`Tcp
           (Eio.Net.Ipaddr.V4.loopback, Matrix_eio.Oauth.Loopback.port listener))
    in
    Eio.Flow.copy_string raw flow;
    Eio.Flow.shutdown flow `Send;
    Eio.Buf_read.(parse_exn ~max_size:65536 take_all flow)
  in
  let large = String.make 40000 'x' in
  let callback = ref None in
  Eio.Fiber.both
    (fun () ->
      let valid = Matrix_eio.Oauth.Loopback.wait listener in
      callback := Some valid)
    (fun () ->
      (* A peer disappearing before it completes a request must not cancel the
         listener or consume the one callback. *)
      ( Eio.Switch.run @@ fun request_sw ->
        let flow =
          Eio.Net.connect ~sw:request_sw net
            (`Tcp
               ( Eio.Net.Ipaddr.V4.loopback,
                 Matrix_eio.Oauth.Loopback.port listener ))
        in
        Eio.Flow.copy_string "GET /callback HTTP/1.1\r\n" flow );
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      let response =
        request
          "GET /favicon.ico HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
      in
      check_bool "wrong path is 404" true (contains response "404 Not Found");
      let response =
        request
          "POST /callback HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
      in
      check_bool "POST is 405" true (contains response "405 Method Not Allowed");
      check_bool "POST advertises GET" true (contains response "Allow: GET");
      let response =
        request
          "HEAD /callback HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
      in
      check_bool "HEAD is 405" true (contains response "405 Method Not Allowed");
      let response =
        request
          ("GET /callback HTTP/1.1\r\nHost: 127.0.0.1\r\nX-Pad: " ^ large
         ^ "\r\nConnection: close\r\n\r\n")
      in
      check_bool "oversized head is rejected" true
        (contains response "431 Request Header Fields Too Large");
      let response =
        Eio.Switch.run @@ fun request_sw ->
        let flow =
          Eio.Net.connect ~sw:request_sw net
            (`Tcp
               ( Eio.Net.Ipaddr.V4.loopback,
                 Matrix_eio.Oauth.Loopback.port listener ))
        in
        Eio.Flow.copy_string "GET /callback HTTP/1.1\r\nHost: 127.0.0.1\r\n"
          flow;
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.7;
        Eio.Flow.shutdown flow `Send;
        Eio.Buf_read.(parse_exn ~max_size:65536 take_all flow)
      in
      check_bool "slow head times out" true
        (contains response "408 Request Timeout");
      let response =
        request
          "GET /callback?code=missing-state HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
      in
      check_bool "malformed OAuth query is rejected" true
        (contains response "400 Bad Request");
      let response =
        request
          "GET /callback?code=abc&state=s1 HTTP/1.1\r\n\
           Host: 127.0.0.1\r\n\
           Connection: close\r\n\
           \r\n"
      in
      check_bool "callback response is HTML" true
        (contains response "Content-Type: text/html; charset=utf-8");
      check_bool "callback response is not stored" true
        (contains response "Cache-Control: no-store"));
  let callback = Option.get !callback in
  check_string "callback query survives bad requests" "abc"
    (match Uriz.find_query ~plus_as_space:true callback "code" with
    | This code -> code
    | Null -> Alcotest.fail "callback had no code");
  let rec wait_until_closed attempts =
    if attempts = 0 then Alcotest.fail "loopback listener remained open"
    else
      match
        Eio.Switch.run @@ fun request_sw ->
        let flow =
          Eio.Net.connect ~sw:request_sw net
            (`Tcp
               ( Eio.Net.Ipaddr.V4.loopback,
                 Matrix_eio.Oauth.Loopback.port listener ))
        in
        Eio.Flow.shutdown flow `All
      with
      | () ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
          wait_until_closed (attempts - 1)
      | exception Eio.Io _ -> ()
  in
  wait_until_closed 20

let test_loopback_path_validation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let rejects path =
    match Matrix_eio.Oauth.Loopback.create ~path ~sw ~env net with
    | _ -> Alcotest.failf "accepted invalid callback path %S" path
    | exception Invalid_argument _ -> ()
  in
  List.iter rejects
    [
      "";
      "/";
      "callback";
      "/a?query";
      "/a#fragment";
      "/a//b";
      "/a/";
      "/./a";
      "/a/../b";
      "/raw space";
      "/" ^ String.make 1 (Char.chr 255);
    ];
  let listener =
    Matrix_eio.Oauth.Loopback.create ~path:"/oauth%20callback" ~sw ~env net
  in
  check_string "canonical encoded callback path"
    (Printf.sprintf "http://127.0.0.1:%d/oauth%%20callback"
       (Matrix_eio.Oauth.Loopback.port listener))
    (Matrix_eio.Oauth.Loopback.redirect_uri listener);
  Eio.Fiber.both
    (fun () ->
      let callback = Matrix_eio.Oauth.Loopback.wait listener in
      check_string "decoded custom callback code" "ok"
        (match Uriz.find_query ~plus_as_space:true callback "code" with
        | This code -> code
        | Null -> Alcotest.fail "callback had no code"))
    (fun () ->
      Eio.Switch.run @@ fun request_sw ->
      let flow =
        Eio.Net.connect ~sw:request_sw net
          (`Tcp
             ( Eio.Net.Ipaddr.V4.loopback,
               Matrix_eio.Oauth.Loopback.port listener ))
      in
      Eio.Flow.copy_string
        "GET /oauth%20callback?code=ok&state=s HTTP/1.1\r\n\
         Host: 127.0.0.1\r\n\
         Connection: close\r\n\
         \r\n"
        flow;
      Eio.Flow.shutdown flow `Send;
      ignore Eio.Buf_read.(parse_exn ~max_size:65536 take_all flow))

let eio_client ~env ~sw fetch =
  Matrix_eio.Client.create ~sw ~env
    ~homeserver:(Uriz.of_string_exn default_homeserver)
    ~fetch ()

let eio_session () : Client.session =
  {
    user_id = uid "@alice:hs.example";
    access_token = "at";
    device_id = did "DEVICEID";
    refresh_token = Some "rt";
  }

let test_logout_session_routing () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch = mock_seq [ (200, "") ] in
  let client =
    eio_client ~env ~sw fetch |> fun c ->
    Matrix_eio.Client.with_session c (eio_session ())
  in
  let matrix_auth : Matrix_client.Session.Auth.t =
    {
      access_token = "at";
      device_id = did "DEVICEID";
      refresh_token = Some "rt";
      access_token_expires_at = None;
      method_ = Matrix_client.Session.Auth.Matrix;
    }
  in
  Matrix_eio.Auth.logout_session client matrix_auth;
  check_strings "Matrix logout only calls /logout"
    [ "https://hs.example/_matrix/client/v3/logout" ]
    (urls log);
  let log, fetch = mock_seq [ (200, metadata_json); (200, ""); (200, "") ] in
  let client =
    eio_client ~env ~sw fetch |> fun c ->
    Matrix_eio.Client.with_session c (eio_session ())
  in
  let oauth_auth : Matrix_client.Session.Auth.t =
    {
      access_token = "at";
      device_id = did "DEVICEID";
      refresh_token = Some "rt";
      access_token_expires_at = None;
      method_ = Matrix_client.Session.Auth.OAuth { client_id = "cid" };
    }
  in
  Matrix_eio.Auth.logout_session client oauth_auth;
  check_strings "OAuth logout discovers and revokes, never /logout"
    [
      "https://hs.example/_matrix/client/v1/auth_metadata";
      "https://auth.example/oauth2/revoke";
      "https://auth.example/oauth2/revoke";
    ]
    (urls log)

(* Automatic refresh is deliberately exercised through the Eio wrapper.  The
   metadata request must be unauthenticated (an expired bearer must not recurse
   into the refresh hook), while the issuer request needs the original,
   unrestricted transport. *)

let unknown_token = {|{"errcode":"M_UNKNOWN_TOKEN","error":"expired"}|}

exception Cancel_initiating_refresh

let auto_client ~env ~sw fetch ?store ?on_session_update ?on_session_invalid ()
    =
  let client =
    eio_client ~env ~sw fetch |> fun c ->
    Matrix_eio.Client.with_session c (eio_session ())
  in
  Matrix_eio.Oauth.with_auto_refresh ?store ?on_session_update
    ?on_session_invalid client ~client_id:"cid"

let test_auto_refresh_replays_with_rotated_token () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (401, unknown_token);
        (200, metadata_json);
        (200, token_reply);
        (200, {|{"user_id":"@alice:hs.example"}|});
      ]
  in
  let persisted = ref None in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_update:(fun session ->
        persisted := Some session;
        Ok ())
      ()
  in
  let user = Matrix_eio.Auth.whoami client in
  check_string "whoami after refresh" "@alice:hs.example"
    (Id.User_id.to_string user);
  let rs = requests log in
  check_int "initial request, discovery, refresh and one replay" 4
    (List.length rs);
  check_str_opt "expired bearer on initial request" (Some "Bearer at")
    (header (List.nth rs 0) "authorization");
  check_str_opt "metadata is unauthenticated" None
    (header (List.nth rs 1) "authorization");
  check_str_opt "token endpoint is unauthenticated" None
    (header (List.nth rs 2) "authorization");
  check_str_opt "replay uses the rotated bearer"
    (Some "Bearer 2YotnFZFEjr1zCsicMWpAA")
    (header (List.nth rs 3) "authorization");
  check_string "refresh grant uses the current refresh token" "rt"
    (List.assoc "refresh_token" (form_of (Option.get (List.nth rs 2).body)));
  check_string "refresh grant has the adapter client id" "cid"
    (List.assoc "client_id" (form_of (Option.get (List.nth rs 2).body)));
  let current = Option.get (Matrix_eio.Client.session client) in
  check_string "rotated access token is visible in the session"
    "2YotnFZFEjr1zCsicMWpAA" current.access_token;
  check_str_opt "rotated refresh token is visible in the session"
    (Some "tGz3JOkF0XG5Qx2TlKWIA") current.refresh_token;
  let saved = Option.get !persisted in
  check_string "persistence sees the rotated access token" current.access_token
    saved.access_token

let test_auto_refresh_shared_profile () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let directory = Filename.temp_dir "matrix-oauth-shared-" "" in
  let root = Eio.Path.(Eio.Stdenv.fs env / directory) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () ->
      let module P = Matrix_client.Profile_store in
      let module S = Matrix_client.Session in
      let store = P.create_at ~root ~profile:"test" in
      let initial = eio_session () in
      let stored =
        S.Session_file.
          {
            server =
              {
                homeserver = Uriz.of_string_exn "https://hs.example";
                user_id = initial.user_id;
              };
            auth =
              {
                access_token = initial.access_token;
                refresh_token = initial.refresh_token;
                device_id = initial.device_id;
                access_token_expires_at = None;
                method_ = OAuth { client_id = "cid" };
              };
            sync = { next_batch = None; filter_id = None };
            metadata =
              {
                created_at = Ptime.epoch;
                last_used_at = Ptime.epoch;
                client_name = "test";
              };
          }
      in
      ignore (Result.get_ok (P.save_session store stored));
      let log, fetch =
        mock_seq
          [
            (401, unknown_token);
            (200, metadata_json);
            (200, token_reply);
            (200, {|{"user_id":"@alice:hs.example"}|});
            (401, unknown_token);
            (200, {|{"user_id":"@alice:hs.example"}|});
          ]
      in
      let callback (session : Client.session) =
        let saved = Option.get (Result.get_ok (P.load_session store)) in
        check_string "persisted before notification" session.Client.access_token
          saved.auth.access_token;
        Ok ()
      in
      let first =
        auto_client ~env ~sw fetch ~store ~on_session_update:callback ()
      in
      let second =
        auto_client ~env ~sw fetch ~store ~on_session_update:callback ()
      in
      ignore (Matrix_eio.Auth.whoami first);
      ignore (Matrix_eio.Auth.whoami second);
      let rs = requests log in
      check_int "only one discovery and exchange" 6 (List.length rs);
      check_str_opt "second client uses persisted rotation"
        (Some "Bearer 2YotnFZFEjr1zCsicMWpAA")
        (header (List.nth rs 5) "authorization"))

let with_refresh_profile env fn =
  let directory = Filename.temp_dir "matrix-oauth-prepare-" "" in
  let root = Eio.Path.(Eio.Stdenv.fs env / directory) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree root)
    (fun () ->
      let module P = Matrix_client.Profile_store in
      let module S = Matrix_client.Session in
      let store = P.create_at ~root ~profile:"test" in
      let initial = eio_session () in
      let session =
        S.Session_file.
          {
            server =
              {
                homeserver = Uriz.of_string_exn "https://hs.example";
                user_id = initial.user_id;
              };
            auth =
              {
                access_token = initial.access_token;
                refresh_token = initial.refresh_token;
                device_id = initial.device_id;
                access_token_expires_at = None;
                method_ = OAuth { client_id = "cid" };
              };
            sync = { next_batch = None; filter_id = None };
            metadata =
              {
                created_at = Ptime.epoch;
                last_used_at = Ptime.epoch;
                client_name = "test";
              };
          }
      in
      ignore (Result.get_ok (P.save_session store session));
      fn store)

let persisted_auto_client ~env ~sw ~store ~proactive fetch =
  let client =
    eio_client ~env ~sw fetch |> fun c ->
    Matrix_eio.Client.with_session c (eio_session ())
  in
  if proactive then
    Matrix_eio.Oauth.with_auto_refresh_expiry ~store ~expires_at:Ptime.epoch
      client ~client_id:"cid"
  else Matrix_eio.Oauth.with_auto_refresh ~store client ~client_id:"cid"

let test_persisted_preparation_retry ~proactive ~invalid () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_refresh_profile env @@ fun store ->
  let module P = Matrix_client.Profile_store in
  let prefix = if proactive then [] else [ (401, unknown_token) ] in
  let first =
    if invalid then
      ( 200,
        {|{"issuer":"http://auth.example/","authorization_endpoint":"http://auth.example/auth","token_endpoint":"http://auth.example/token","response_types_supported":["code"],"grant_types_supported":["refresh_token"],"code_challenge_methods_supported":["S256"]}|}
      )
    else (500, {|{"errcode":"M_UNKNOWN","error":"discovery unavailable"}|})
  in
  let log, fetch =
    mock_seq
      (prefix @ [ first ] @ prefix
      @ [
          (200, metadata_json);
          (200, token_reply);
          (200, {|{"user_id":"@alice:hs.example"}|});
        ])
  in
  let client = persisted_auto_client ~env ~sw ~store ~proactive fetch in
  let before = Eio.Path.load Eio.Path.(P.dir store / "session.json") in
  Alcotest.(check bool)
    "first request failed" true
    (try
       ignore (Matrix_eio.Auth.whoami client);
       false
     with Eio.Io _ -> true);
  Alcotest.(check bool)
    "preparation did not mark the token consumed" false
    (Eio.Path.is_file Eio.Path.(P.dir store / ".refresh_pending.json"));
  check_string "credentials unchanged" before
    (Eio.Path.load Eio.Path.(P.dir store / "session.json"));
  ignore (Matrix_eio.Auth.whoami client);
  let rs = requests log in
  check_int "preparation was retried"
    (if proactive then 4 else 6)
    (List.length rs);
  let posts = List.filter (fun request -> request.meth = "POST") rs in
  check_int "one token exchange" 1 (List.length posts);
  check_string "unconsumed token reused" "rt"
    (List.assoc "refresh_token" (form_of (Option.get (List.hd posts).body)));
  check_string "rotation persisted" "2YotnFZFEjr1zCsicMWpAA"
    (Option.get (Result.get_ok (P.load_session store))).auth.access_token

let test_persisted_exchange_failure ~proactive () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_refresh_profile env @@ fun store ->
  let prefix = if proactive then [] else [ (401, unknown_token) ] in
  let log, fetch =
    mock_seq
      (prefix
      @ [ (200, metadata_json); (500, {|{"error":"temporarily_unavailable"}|}) ]
      @ prefix)
  in
  let client = persisted_auto_client ~env ~sw ~store ~proactive fetch in
  for _attempt = 1 to 2 do
    Alcotest.(check bool)
      "uncertain exchange refused" true
      (try
         ignore (Matrix_eio.Auth.whoami client);
         false
       with Eio.Io _ -> true)
  done;
  Alcotest.(check bool)
    "uncertain token remains marked" true
    (Eio.Path.is_file
       Eio.Path.(
         Matrix_client.Profile_store.dir store / ".refresh_pending.json"));
  check_int "no second discovery or exchange"
    (if proactive then 2 else 4)
    (List.length (requests log))

let token_reply_without_refresh =
  {|{"access_token":"new-access","token_type":"Bearer","expires_in":299}|}

let test_auto_refresh_preserves_omitted_refresh_token () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (401, unknown_token);
        (200, metadata_json);
        (200, token_reply_without_refresh);
        (200, {|{"user_id":"@alice:hs.example"}|});
      ]
  in
  let client = auto_client ~env ~sw fetch () in
  ignore (Matrix_eio.Auth.whoami client);
  let current = Option.get (Matrix_eio.Client.session client) in
  check_string "new access token" "new-access" current.access_token;
  check_str_opt "omitted refresh token retains the old one" (Some "rt")
    current.refresh_token;
  check_int "exactly one refresh and replay" 4 (List.length (requests log))

let test_auto_refresh_replays_at_most_once () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (401, unknown_token);
        (200, metadata_json);
        (200, token_reply);
        (401, unknown_token);
      ]
  in
  let client = auto_client ~env ~sw fetch () in
  (try
     ignore (Matrix_eio.Auth.whoami client);
     Alcotest.fail "the failed replay should be returned"
   with Eio.Io _ -> ());
  check_strings "no second refresh after a failed replay"
    [
      "https://hs.example/_matrix/client/v3/account/whoami";
      "https://hs.example/_matrix/client/v1/auth_metadata";
      "https://auth.example/oauth2/token";
      "https://hs.example/_matrix/client/v3/account/whoami";
    ]
    (urls log)

let invalid_grant =
  {|{"error":"invalid_grant","error_description":"refresh token expired"}|}

let test_auto_refresh_notifies_invalid_grant_once () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [ (401, unknown_token); (200, metadata_json); (400, invalid_grant) ]
  in
  let notifications = ref [] in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_invalid:(fun reason ->
        notifications := reason :: !notifications)
      ()
  in
  (try
     ignore (Matrix_eio.Auth.whoami client);
     Alcotest.fail "invalid_grant refresh should fail"
   with Eio.Io _ -> ());
  check_int "one invalid-grant notification" 1 (List.length !notifications);
  match !notifications with
  | [ Matrix_eio.Oauth.Invalid_grant ] -> ()
  | _ -> Alcotest.fail "wrong invalid-grant notification"

let test_auto_refresh_invalid_grant_notification_isolated () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [ (401, unknown_token); (200, metadata_json); (400, invalid_grant) ]
  in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_invalid:(fun _ -> failwith "notification callback")
      ()
  in
  (try
     ignore (Matrix_eio.Auth.whoami client);
     Alcotest.fail "invalid_grant refresh should fail"
   with Eio.Io _ -> ());
  check_int "callback failure does not add requests" 3
    (List.length (requests log))

let test_auto_refresh_detached_persistence_cancellation_isolated () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (401, unknown_token);
        (200, metadata_json);
        (200, token_reply_without_refresh);
        (200, {|{"user_id":"@alice:hs.example"}|});
      ]
  in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_update:(fun _ ->
        raise (Eio.Cancel.Cancelled Cancel_initiating_refresh))
      ()
  in
  let user = Matrix_eio.Auth.whoami client in
  check_string "persistence cancellation does not fail owner switch"
    "@alice:hs.example"
    (Id.User_id.to_string user);
  check_int "refresh still replays exactly once" 4 (List.length (requests log))

let test_auto_refresh_notifies_shared_invalid_grant_once () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log = ref [] in
  let initial_requests = ref 0 in
  let token_started, token_started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        log := req :: !log;
        if String.ends_with ~suffix:"/account/whoami" url then begin
          incr initial_requests;
          if !initial_requests = 2 then Eio.Promise.resolve release_u ();
          Fetch_mock.respond ~status:401 unknown_token req
        end
        else if String.ends_with ~suffix:"/auth_metadata" url then
          Fetch_mock.respond metadata_json req
        else if String.ends_with ~suffix:"/oauth2/token" url then begin
          Eio.Promise.resolve token_started_u ();
          Eio.Promise.await release;
          Fetch_mock.respond ~status:400 invalid_grant req
        end
        else Alcotest.failf "unexpected URL %s" url)
  in
  let notifications = ref 0 in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_invalid:(fun Matrix_eio.Oauth.Invalid_grant ->
        incr notifications)
      ()
  in
  let first = ref false and second = ref false in
  Eio.Fiber.both
    (fun () ->
      try ignore (Matrix_eio.Auth.whoami client)
      with Eio.Io _ -> first := true)
    (fun () ->
      Eio.Promise.await token_started;
      try ignore (Matrix_eio.Auth.whoami client)
      with Eio.Io _ -> second := true);
  (* The second initial request releases the token endpoint, so both callers
     observe the same failed refresh operation. *)
  check_bool "first waiter failed" true !first;
  check_bool "second waiter failed" true !second;
  check_int "shared invalid_grant is reported once" 1 !notifications;
  check_int "two initial requests plus metadata and token" 4 (List.length !log)

let test_auto_refresh_does_not_notify_transient_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch =
    mock_seq
      [
        (401, unknown_token);
        (200, metadata_json);
        (503, {|{"error":"temporarily_unavailable"}|});
      ]
  in
  let notifications = ref 0 in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_invalid:(fun _ -> incr notifications)
      ()
  in
  (try
     ignore (Matrix_eio.Auth.whoami client);
     Alcotest.fail "transient refresh should fail"
   with Eio.Io _ -> ());
  check_int "transient failure does not notify invalid session" 0 !notifications;
  check_int "transient refresh request count" 3 (List.length (requests log))

let test_auto_refresh_survives_initiator_cancellation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let token_started, token_started_u = Eio.Promise.create () in
  let release_token, release_token_u = Eio.Promise.create () in
  let committed, committed_u = Eio.Promise.create () in
  let token_requests = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        let authorization =
          header
            { meth = ""; url; headers = req.headers; body = None }
            "authorization"
        in
        if String.ends_with ~suffix:"/account/whoami" url then
          match authorization with
          | Some "Bearer new-access" ->
              Fetch_mock.respond {|{"user_id":"@alice:hs.example"}|} req
          | _ -> Fetch_mock.respond ~status:401 unknown_token req
        else if String.ends_with ~suffix:"/auth_metadata" url then
          Fetch_mock.respond metadata_json req
        else if String.ends_with ~suffix:"/oauth2/token" url then begin
          incr token_requests;
          Eio.Promise.resolve token_started_u ();
          Eio.Promise.await release_token;
          Fetch_mock.respond token_reply_without_refresh req
        end
        else Alcotest.failf "unexpected URL %s" url)
  in
  let client =
    auto_client ~env ~sw fetch
      ~on_session_update:(fun session ->
        Eio.Promise.resolve committed_u session;
        Ok ())
      ()
  in
  let cancelled =
    try
      Eio.Cancel.sub (fun cc ->
          Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.await token_started;
              Eio.Cancel.cancel cc Cancel_initiating_refresh);
          ignore (Matrix_eio.Auth.whoami client);
          false)
    with Eio.Cancel.Cancelled Cancel_initiating_refresh -> true
  in
  check_bool "initiating request was cancelled" true cancelled;
  Eio.Promise.resolve release_token_u ();
  let saved = Eio.Promise.await committed in
  check_string "detached refresh committed the access token" "new-access"
    saved.access_token;
  check_int "cancelled initiator caused one token request" 1 !token_requests;
  let user = Matrix_eio.Auth.whoami client in
  check_string "next request uses committed token" "@alice:hs.example"
    (Id.User_id.to_string user)

let test_auto_refresh_is_opt_in () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let log, fetch = mock_seq [ (401, unknown_token) ] in
  let client =
    eio_client ~env ~sw fetch |> fun c ->
    Matrix_eio.Client.with_session c (eio_session ())
  in
  (try
     ignore (Matrix_eio.Auth.whoami client);
     Alcotest.fail "an unconfigured client must not refresh"
   with Eio.Io _ -> ());
  check_strings "opt-in client makes no discovery or token request"
    [ "https://hs.example/_matrix/client/v3/account/whoami" ]
    (urls log)

let qr_vector =
  "SU9fRUxFTUVOVF9NU0M0Mzg4AwG0yzZ1QVpQ1jlnoxWX3d5jrWRFfELxjS2gN7pz9y+3PAAaMDFIWDlLMDBRMUg2S1BENDdFRzRHMVQzWEcAJGh0dHBzOi8vc3luYXBzZS1vaWRjLmxhYi5lbGVtZW50LmRldg"

let qr_ok = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "expected QR value, got %a" Qr_login.pp_codec_error error

let test_qr_msc4388_vector () =
  let value = qr_ok (Qr_login.of_base64 qr_vector) in
  check_bool "reciprocate intent" true (value.intent = Qr_login.Reciprocate);
  check_string "public key" "tMs2dUFaUNY5Z6MVl93eY61kRXxC8Y0toDe6c/cvtzw"
    (Matrix_client.Crypto_key.Curve25519.Public.to_base64 value.public_key);
  check_string "rendezvous ID" "01HX9K00Q1H6KPD47EG4G1T3XG" value.rendezvous_id;
  check_string "base URL" "https://synapse-oidc.lab.element.dev"
    (Uriz.to_string value.base_url);
  check_string "byte-identical Base64 round trip" qr_vector
    (qr_ok (Qr_login.to_base64 value))

let replace_byte value index byte =
  let copy = Bytes.of_string value in
  Bytes.set copy index (Char.chr byte);
  Bytes.unsafe_to_string copy

let is_qr_error expected = function
  | Error error when expected error -> true
  | Ok _ | Error _ -> false

let test_qr_msc4388_rejects_malformed () =
  let raw = Result.get_ok (Matrix_proto.Base64.decode qr_vector) in
  check_bool "truncated" true
    (is_qr_error
       (function Qr_login.Not_enough_data -> true | _ -> false)
       (Qr_login.of_bytes (String.sub raw 0 24)));
  check_bool "prefix" true
    (is_qr_error
       (function Qr_login.Invalid_prefix -> true | _ -> false)
       (Qr_login.of_bytes (replace_byte raw 0 0)));
  check_bool "type" true
    (is_qr_error
       (function Qr_login.Invalid_type 2 -> true | _ -> false)
       (Qr_login.of_bytes
          (replace_byte raw (String.length "IO_ELEMENT_MSC4388") 2)));
  check_bool "intent" true
    (is_qr_error
       (function Qr_login.Invalid_intent 9 -> true | _ -> false)
       (Qr_login.of_bytes
          (replace_byte raw (String.length "IO_ELEMENT_MSC4388" + 1) 9)));
  let public_key = (qr_ok (Qr_login.of_base64 qr_vector)).public_key in
  check_bool "invalid UTF-8" true
    (is_qr_error
       (function Qr_login.Invalid_utf8 _ -> true | _ -> false)
       (Qr_login.make ~intent:Qr_login.Login ~public_key ~rendezvous_id:"\255"
          ~base_url:(Uriz.of_string_exn default_homeserver)));
  check_bool "relative base URL" true
    (is_qr_error
       (function Qr_login.Invalid_base_url _ -> true | _ -> false)
       (Qr_login.make ~intent:Qr_login.Login ~public_key ~rendezvous_id:"id"
          ~base_url:(Uriz.of_string_exn "/relative")));
  List.iter
    (fun base_url ->
      check_bool
        ("invalid base URL " ^ base_url)
        true
        (match Uriz.of_string base_url with
        | Null -> true
        | This base_url ->
            is_qr_error
              (function Qr_login.Invalid_base_url _ -> true | _ -> false)
              (Qr_login.make ~intent:Qr_login.Login ~public_key
                 ~rendezvous_id:"id" ~base_url)))
    [
      "https://user@hs.example";
      "https://hs.example?tenant=one";
      "https://hs.example#fragment";
      "https://hs.example:not-a-port";
    ];
  let prefixed =
    qr_ok
      (Qr_login.make ~intent:Qr_login.Login ~public_key ~rendezvous_id:"id"
         ~base_url:(Uriz.of_string_exn "https://hs.example/matrix/"))
  in
  check_string "base-path QR URL is retained" "https://hs.example/matrix/"
    (Uriz.to_string prefixed.base_url);
  let prefixed =
    qr_ok (Qr_login.of_bytes (qr_ok (Qr_login.to_bytes prefixed)))
  in
  check_string "base-path QR URL survives wire round trip"
    "https://hs.example/matrix/"
    (Uriz.to_string prefixed.base_url);
  check_bool "oversized ID" true
    (is_qr_error
       (function Qr_login.Field_too_long _ -> true | _ -> false)
       (Qr_login.make ~intent:Qr_login.Login ~public_key
          ~rendezvous_id:(String.make 0x10000 'x')
          ~base_url:(Uriz.of_string_exn default_homeserver)))

let qr_msc4108_data_base64 =
  "TUFUUklYAgPYhmhqshl7eA4wCp1KIUdIBwDXkp85qzG55RQ3AkjtawBHaHR0cHM6Ly9yZW5kZXp2b3VzLmxhYi5lbGVtZW50LmRldi9lOGRhNjM1NS01NTBiLTRhMzItYTE5My0xNjE5ZDk4MzA2Njg"

let qr_msc4108_reciprocate_base64 =
  "TUFUUklYAgTYhmhqshl7eA4wCp1KIUdIBwDXkp85qzG55RQ3AkjtawBHaHR0cHM6Ly9yZW5kZXp2b3VzLmxhYi5lbGVtZW50LmRldi9lOGRhNjM1NS01NTBiLTRhMzItYTE5My0xNjE5ZDk4MzA2NjgACm1hdHJpeC5vcmc"

let qr_msc4108_synapse_base64 =
  "TUFUUklYAgS0yzZ1QVpQ1jlnoxWX3d5jrWRFfELxjS2gN7pz9y+3PABaaHR0cHM6Ly9zeW5hcHNlLW9pZGMubGFiLmVsZW1lbnQuZGV2L19zeW5hcHNlL2NsaWVudC9yZW5kZXp2b3VzLzAxSFg5SzAwUTFINktQRDQ3RUc0RzFUM1hHACVodHRwczovL3N5bmFwc2Utb2lkYy5sYWIuZWxlbWVudC5kZXYv"

let msc4108_ok = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "expected MSC4108 value, got %a" Msc4108.pp_codec_error
        error

let test_qr_msc4108_vectors () =
  let value = msc4108_ok (Msc4108.of_base64 qr_msc4108_data_base64) in
  check_bool "login intent" true
    (match value.intent with Msc4108.Login -> true | _ -> false);
  check_string "login public key" "2IZoarIZe3gOMAqdSiFHSAcA15KfOasxueUUNwJI7Ws"
    (Matrix_client.Crypto_key.Curve25519.Public.to_base64 value.public_key);
  check_string "login rendezvous URL"
    "https://rendezvous.lab.element.dev/e8da6355-550b-4a32-a193-1619d9830668"
    (Uriz.to_string value.rendezvous_url);
  check_string "login byte-identical Base64 round trip" qr_msc4108_data_base64
    (msc4108_ok (Msc4108.to_base64 value));
  let reciprocate =
    msc4108_ok (Msc4108.of_base64 qr_msc4108_reciprocate_base64)
  in
  check_bool "reciprocate intent" true
    (match reciprocate.intent with
    | Msc4108.Reciprocate uri -> String.equal (Uriz.to_string uri) "matrix.org"
    | Msc4108.Login -> false);
  check_string "reciprocate byte-identical Base64 round trip"
    qr_msc4108_reciprocate_base64
    (msc4108_ok (Msc4108.to_base64 reciprocate));
  let synapse = msc4108_ok (Msc4108.of_base64 qr_msc4108_synapse_base64) in
  check_string "synapse public key"
    "tMs2dUFaUNY5Z6MVl93eY61kRXxC8Y0toDe6c/cvtzw"
    (Matrix_client.Crypto_key.Curve25519.Public.to_base64 synapse.public_key);
  check_string "synapse rendezvous URL"
    "https://synapse-oidc.lab.element.dev/_synapse/client/rendezvous/01HX9K00Q1H6KPD47EG4G1T3XG"
    (Uriz.to_string synapse.rendezvous_url);
  check_string "synapse server name" "https://synapse-oidc.lab.element.dev/"
    (match synapse.intent with
    | Msc4108.Reciprocate uri -> Uriz.to_string uri
    | Msc4108.Login -> "");
  check_string "synapse byte-identical Base64 round trip"
    qr_msc4108_synapse_base64
    (msc4108_ok (Msc4108.to_base64 synapse))

let test_qr_msc4108_rejects_malformed () =
  let raw = Result.get_ok (Matrix_proto.Base64.decode qr_msc4108_data_base64) in
  let is_error expected = function
    | Error error when expected error -> true
    | Ok _ | Error _ -> false
  in
  check_bool "truncated" true
    (is_error
       (function Msc4108.Not_enough_data -> true | _ -> false)
       (Msc4108.of_bytes (String.sub raw 0 (String.length raw - 1))));
  check_bool "prefix" true
    (is_error
       (function Msc4108.Invalid_prefix -> true | _ -> false)
       (Msc4108.of_bytes (replace_byte raw 0 0)));
  check_bool "version" true
    (is_error
       (function Msc4108.Invalid_version 1 -> true | _ -> false)
       (Msc4108.of_bytes (replace_byte raw 6 1)));
  check_bool "intent" true
    (is_error
       (function Msc4108.Invalid_intent 9 -> true | _ -> false)
       (Msc4108.of_bytes (replace_byte raw 7 9)));
  check_bool "trailing bytes" true
    (is_error
       (function Msc4108.Trailing_data -> true | _ -> false)
       (Msc4108.of_bytes (raw ^ "x")));
  let invalid_utf8 = replace_byte raw 42 0xff in
  check_bool "invalid rendezvous UTF-8" true
    (is_error
       (function Msc4108.Invalid_utf8 "rendezvous URL" -> true | _ -> false)
       (Msc4108.of_bytes invalid_utf8));
  check_bool "rendezvous control byte" true
    (is_error
       (function Msc4108.Invalid_url _ -> true | _ -> false)
       (Msc4108.of_bytes (replace_byte raw 42 (Char.code '\n'))));
  check_bool "invalid Base64" true
    (is_error
       (function Msc4108.Invalid_base64 _ -> true | _ -> false)
       (Msc4108.of_base64 "!"));
  let reciprocate_raw =
    Result.get_ok (Matrix_proto.Base64.decode qr_msc4108_reciprocate_base64)
  in
  check_bool "invalid server-name UTF-8" true
    (is_error
       (function Msc4108.Invalid_utf8 "server name" -> true | _ -> false)
       (Msc4108.of_bytes
          (replace_byte reciprocate_raw
             (String.length reciprocate_raw - String.length "matrix.org")
             0xff)));
  let public_key = (msc4108_ok (Msc4108.of_bytes raw)).public_key in
  check_bool "relative rendezvous URL" true
    (is_error
       (function Msc4108.Invalid_url _ -> true | _ -> false)
       (Msc4108.make ~intent:Msc4108.Login ~public_key
          ~rendezvous_url:(Uriz.of_string_exn "/relative")));
  List.iter
    (fun rendezvous_url ->
      check_bool
        ("invalid rendezvous URL " ^ rendezvous_url)
        true
        (match Uriz.of_string rendezvous_url with
        | Null -> true
        | This rendezvous_url ->
            is_error
              (function Msc4108.Invalid_url _ -> true | _ -> false)
              (Msc4108.make ~intent:Msc4108.Login ~public_key ~rendezvous_url)))
    [
      "https://user@rendezvous.example/id";
      "https://rendezvous.example/id#fragment";
      "https://rendezvous.example:not-a-port/id";
    ];
  check_bool "relative reciprocate server name" true
    (is_error
       (function Msc4108.Invalid_url _ -> true | _ -> false)
       (Msc4108.make
          ~intent:(Msc4108.Reciprocate (Uriz.of_string_exn "/relative"))
          ~public_key
          ~rendezvous_url:
            (Uriz.of_string_exn "https://rendezvous.example/id")));
  check_bool "malformed bare server name" true
    (is_error
       (function Msc4108.Invalid_url _ -> true | _ -> false)
       (Msc4108.make
          ~intent:(Msc4108.Reciprocate (Uriz.of_string_exn "user@example.org"))
          ~public_key
          ~rendezvous_url:
            (Uriz.of_string_exn "https://rendezvous.example/id")));
  check_bool "oversized rendezvous URL" true
    (is_error
       (function Msc4108.Field_too_long "rendezvous URL" -> true | _ -> false)
       (Msc4108.make ~intent:Msc4108.Login ~public_key
          ~rendezvous_url:
            (Uriz.of_string_exn ("https://a/" ^ String.make 0xffff 'x'))))

let rv_etag ?(weak = false) tag : Fetch.Header.etag = { weak; tag }
let rv_content_type ?(params = []) media = Fetch.Header.media ~params media

let rv_response ?etag ?content_type ?expires_at status body :
    Msc4108.Rendezvous.response =
  { status; etag; content_type; body; expires_at }

type rv_call = {
  rv_method : Msc4108.Rendezvous.method_;
  rv_uri : Uriz.t;
  rv_headers : (string * string) list;
  rv_body : string option;
}

let fake_rendezvous responses ?(now = Ptime.epoch) () =
  let calls = ref [] in
  let responses = ref responses in
  let now = ref now in
  let sleeps = ref 0 in
  let request ~method_ ~uri ~headers ~body =
    calls :=
      {
        rv_method = method_;
        rv_uri = uri;
        rv_headers = Fetch.Header.to_list headers;
        rv_body = body;
      }
      :: !calls;
    match !responses with
    | response :: rest ->
        responses := rest;
        response
    | [] -> Alcotest.fail "fake rendezvous received an unexpected request"
  in
  let transport =
    {
      Msc4108.Rendezvous.request;
      now = (fun () -> !now);
      sleep = (fun _ -> incr sleeps);
    }
  in
  (transport, calls, now, sleeps)

let rv_call_header name call = List.assoc_opt name call.rv_headers

let test_qr_msc4108_rendezvous_pipeline () =
  let rendezvous_server =
    Uriz.of_string_exn
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4108/rendezvous"
  in
  let rendezvous_url = Uriz.of_string_exn "https://hs.example/rendezvous/abc" in
  let responses =
    [
      Ok
        (rv_response ~etag:(rv_etag ~weak:true "e0") 201
           {|{"url":"https://hs.example/rendezvous/abc"}|});
      Ok (rv_response ~etag:(rv_etag "e1") 204 "");
      Ok (rv_response ~etag:(rv_etag "e1") 304 "");
      Ok
        (rv_response ~etag:(rv_etag "e2")
           ~content_type:
             (rv_content_type ~params:[ ("charset", "utf-8") ] "text/plain")
           200 "message");
      Ok (rv_response 204 "");
    ]
  in
  let transport, calls, _, sleeps = fake_rendezvous responses () in
  let channel =
    match Msc4108.Rendezvous.create transport ~rendezvous_server () with
    | Ok channel -> channel
    | Error _ -> Alcotest.fail "rendezvous creation failed"
  in
  check_string "created URL"
    (Uriz.to_string rendezvous_url)
    (Uriz.to_string (Msc4108.Rendezvous.rendezvous_url channel));
  (match Msc4108.Rendezvous.send channel "hello" with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "rendezvous send failed");
  check_string "received body" "message"
    (match Msc4108.Rendezvous.receive channel with
    | Ok body -> body
    | Error _ -> Alcotest.fail "rendezvous receive failed");
  check_int "304 sleeps once" 1 !sleeps;
  (match Msc4108.Rendezvous.close channel with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "rendezvous close failed");
  check_bool "closed" true (Msc4108.Rendezvous.status channel = `Closed);
  let calls = List.rev !calls in
  check_int "request count" 5 (List.length calls);
  match calls with
  | [ create; send; get; _get_message; _close ] ->
      check_bool "create is POST" true (create.rv_method = `POST);
      check_string "create content type" "text/plain"
        (Option.get (rv_call_header "Content-Type" create));
      check_string "create has an empty body" "" (Option.get create.rv_body);
      check_bool "create has no bearer" true
        (rv_call_header "Authorization" create = None);
      check_bool "send is PUT" true (send.rv_method = `PUT);
      check_string "send etag" "W/\"e0\""
        (Option.get (rv_call_header "If-Match" send));
      check_string "send content type" "text/plain"
        (Option.get (rv_call_header "Content-Type" send));
      check_string "send body" "hello" (Option.get send.rv_body);
      check_string "receive etag" "\"e1\""
        (Option.get (rv_call_header "If-None-Match" get));
      check_bool "receive has no bearer" true
        (rv_call_header "Authorization" get = None)
  | _ -> Alcotest.fail "unexpected rendezvous request sequence"

let test_qr_msc4108_rendezvous_terminal_and_expiry () =
  let rendezvous_url = Uriz.of_string_exn "https://hs.example/rendezvous/abc" in
  let expired = Option.get (Ptime.of_float_s 1.) in
  let transport, calls, now, _ =
    fake_rendezvous
      [
        Ok (rv_response ~etag:(rv_etag "e0") 200 "");
        Ok (rv_response 404 "gone");
      ]
      ~now:Ptime.epoch ()
  in
  let channel =
    match
      Msc4108.Rendezvous.accept transport ~rendezvous_url ~expires_at:expired ()
    with
    | Ok (channel, _) -> channel
    | Error _ -> Alcotest.fail "rendezvous accept failed"
  in
  now := expired;
  check_bool "expired send" true
    (match Msc4108.Rendezvous.send channel "late" with
    | Error Msc4108.Rendezvous.Expired -> true
    | _ -> false);
  check_bool "expiry avoids network" true (List.length !calls = 1);
  (* A fresh channel treats a server-side terminal response as closed even
     when that terminal response has no ETag, and cleanup remains idempotent. *)
  let transport, calls, _, _ =
    fake_rendezvous
      [
        Ok (rv_response ~etag:(rv_etag "e0") 200 "");
        Ok (rv_response 404 "gone");
      ]
      ()
  in
  let channel =
    match Msc4108.Rendezvous.accept transport ~rendezvous_url () with
    | Ok (channel, _) -> channel
    | Error _ -> Alcotest.fail "rendezvous accept failed"
  in
  check_bool "terminal receive returns HTTP error" true
    (match Msc4108.Rendezvous.receive channel with
    | Error (Msc4108.Rendezvous.Http_error { status = 404; body = "gone" }) ->
        true
    | _ -> false);
  check_bool "terminal response closes channel" true
    (Msc4108.Rendezvous.status channel = `Closed);
  check_bool "terminal close is idempotent" true
    (Msc4108.Rendezvous.close channel = Ok ());
  check_int "terminal request count" 2 (List.length !calls)

let test_qr_msc4108_rejects_expired_creation () =
  let expired = Option.get (Ptime.of_float_s 1.) in
  let transport, _, _, _ =
    fake_rendezvous
      [
        Ok
          (rv_response ~etag:(rv_etag "e0") ~expires_at:expired 201
             {|{"url":"https://hs.example/rendezvous/abc"}|});
      ]
      ~now:expired ()
  in
  check_bool "expired create rejected" true
    (match
       Msc4108.Rendezvous.create transport
         ~rendezvous_server:(Uriz.of_string_exn "https://hs.example/rendezvous")
         ()
     with
    | Error Msc4108.Rendezvous.Expired -> true
    | _ -> false);
  let transport, _, _, _ =
    fake_rendezvous
      [ Ok (rv_response ~etag:(rv_etag "e0") ~expires_at:expired 200 "") ]
      ~now:expired ()
  in
  check_bool "expired accept rejected" true
    (match
       Msc4108.Rendezvous.accept transport
         ~rendezvous_url:
           (Uriz.of_string_exn "https://hs.example/rendezvous/abc")
         ()
     with
    | Error Msc4108.Rendezvous.Expired -> true
    | _ -> false)

let test_qr_msc4108_client_transport () =
  let log = ref [] in
  let replies =
    ref
      [
        ( 201,
          [ ("etag", "\"e0\"") ],
          {|{"url":"https://HS.EXAMPLE:443/rendezvous/abc?via=one&via=two"}|} );
        (204, [ ("etag", "\"e1\"") ], "");
        (204, [], "");
      ]
  in
  let fetch =
    Fetch_mock.client (fun request ->
        record log request;
        match !replies with
        | (status, headers, body) :: rest ->
            replies := rest;
            Fetch_mock.respond ~status
              ~headers:(Http.Header.of_list headers)
              body request
        | [] -> Alcotest.fail "more requests than scripted replies")
  in
  let client = client_of fetch in
  let transport =
    Msc4108.Rendezvous.transport_of_client ~sleep:(fun _ -> ()) client
  in
  let url =
    Uriz.of_string_exn "https://hs.example/rendezvous/abc?via=one&via=two"
  in
  let channel =
    match
      Msc4108.Rendezvous.create transport
        ~rendezvous_server:
          (Uriz.of_string_exn
             "https://hs.example/_matrix/client/unstable/org.matrix.msc4108/rendezvous")
        ()
    with
    | Ok channel -> channel
    | Error _ -> Alcotest.fail "client rendezvous creation failed"
  in
  check_string "client-created URL" (Uriz.to_string url)
    (Uriz.to_string (Msc4108.Rendezvous.rendezvous_url channel));
  (match Msc4108.Rendezvous.send channel "payload" with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "client rendezvous send failed");
  (match Msc4108.Rendezvous.close channel with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "client rendezvous close failed");
  List.iter
    (fun request ->
      check_bool "client transport omits bearer" true
        (header request "authorization" = None))
    (requests log);
  let calls = requests log in
  check_int "client transport request count" 3 (List.length calls);
  (match calls with
  | [ create; send; close ] ->
      check_string "client create path"
        "https://hs.example/_matrix/client/unstable/org.matrix.msc4108/rendezvous"
        create.url;
      check_string "client send method" "PUT" send.meth;
      check_string "client send URL keeps repeated query" (Uriz.to_string url)
        send.url;
      check_string "client send If-Match" "\"e0\""
        (Option.get (header send "if-match"));
      check_string "client send content type" "text/plain"
        (Option.get (header send "content-type"));
      check_string "client close method" "DELETE" close.meth
  | _ -> Alcotest.fail "unexpected client transport requests");
  let before = List.length (requests log) in
  (match
     Msc4108.Rendezvous.accept transport
       ~rendezvous_url:
         (Uriz.of_string_exn "https://evil.example/rendezvous/abc")
       ()
   with
  | Error (Msc4108.Rendezvous.Transport_error (Error.Policy_denied _)) -> ()
  | _ -> Alcotest.fail "foreign rendezvous URL was not denied");
  check_int "foreign URL makes no request" before (List.length (requests log))

let client_rendezvous_create ?expires_at ?(now = Ptime.epoch) headers =
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun request ->
        record log request;
        Fetch_mock.respond ~status:201
          ~headers:(Http.Header.of_list headers)
          {|{"url":"https://hs.example/rendezvous/typed"}|} request)
  in
  let clock = ref now in
  let transport =
    Msc4108.Rendezvous.transport_of_client
      ~now:(fun () -> !clock)
      ~sleep:(fun _ -> ())
      (client_of fetch)
  in
  let result =
    Msc4108.Rendezvous.create transport ?expires_at
      ~rendezvous_server:
        (Uriz.of_string_exn
           "https://hs.example/_matrix/client/unstable/org.matrix.msc4108/rendezvous")
      ()
  in
  (result, clock, log)

let test_qr_msc4108_typed_etags () =
  let missing result =
    match result with
    | Error Msc4108.Rendezvous.Missing_etag -> true
    | _ -> false
  in
  List.iter
    (fun value ->
      let result, _, _ = client_rendezvous_create [ ("etag", value) ] in
      check_bool ("reject ETag " ^ String.escaped value) true (missing result))
    [
      "unquoted";
      "\"unterminated";
      "unterminated\"";
      "w/\"lowercase-weak\"";
      "W/unquoted";
      "\"contains space\"";
      "\"\"";
      "W/\"\"";
    ];
  let result, _, _ =
    client_rendezvous_create [ ("etag", "W/\"opaque\\validator\"") ]
  in
  check_bool "weak ETag with legal opaque bytes is retained" true
    (Result.is_ok result)

let test_qr_msc4108_http_dates () =
  let expiry_seconds = 784_111_777. in
  let expiry = Option.get (Ptime.of_float_s expiry_seconds) in
  let before = Option.get (Ptime.of_float_s (expiry_seconds -. 1.)) in
  let rollback = Option.get (Ptime.of_float_s (expiry_seconds -. 10.)) in
  let caller_later = Option.get (Ptime.of_float_s (expiry_seconds +. 60.)) in
  List.iter
    (fun value ->
      let result, clock, _ =
        client_rendezvous_create ~expires_at:caller_later ~now:before
          [ ("etag", "\"date\""); ("expires", value) ]
      in
      let channel =
        match result with
        | Ok channel -> channel
        | Error _ -> Alcotest.failf "HTTP-date was not accepted: %s" value
      in
      clock := rollback;
      check_bool "clock rollback does not expire the channel" true
        (Msc4108.Rendezvous.status channel = `Active);
      clock := expiry;
      check_bool "server expiry wins and is inclusive" true
        (Msc4108.Rendezvous.status channel = `Expired))
    [
      "Sun, 06 Nov 1994 08:49:37 GMT";
      "Sunday, 06-Nov-94 08:49:37 GMT";
      "Sun Nov  6 08:49:37 1994";
    ];
  let server_later =
    Httpz.Date.format
      (Stdlib_upstream_compatible.Float_u.of_float (expiry_seconds +. 60.))
  in
  let result, clock, _ =
    client_rendezvous_create ~expires_at:expiry ~now:before
      [ ("etag", "\"caller\""); ("expires", server_later) ]
  in
  let channel =
    match result with
    | Ok channel -> channel
    | Error _ -> Alcotest.fail "caller/server expiry setup failed"
  in
  clock := expiry;
  check_bool "caller expiry wins when earlier" true
    (Msc4108.Rendezvous.status channel = `Expired);
  let result, _, _ =
    client_rendezvous_create ~now:expiry
      [
        ("etag", "\"malformed-date\"");
        ("expires", "Sun, 32 Nov 1994 08:49:37 GMT");
      ]
  in
  check_bool "malformed Expires is ignored" true (Result.is_ok result);
  let result, _, _ =
    client_rendezvous_create ~now:expiry
      [
        ("etag", "\"already-expired\"");
        ("expires", "Sun, 06 Nov 1994 08:49:37 GMT");
      ]
  in
  check_bool "already expired response is rejected" true
    (match result with Error Msc4108.Rendezvous.Expired -> true | _ -> false)

let test_qr_msc4108_typed_media_type () =
  let log = ref [] in
  let replies =
    ref
      [
        (200, [ ("etag", "\"initial\"") ], "");
        ( 200,
          [
            ("etag", "W/\"message\"");
            ("content-type", "TEXT/PLAIN; Charset=\"UTF-8\"");
          ],
          "payload" );
      ]
  in
  let fetch =
    Fetch_mock.client (fun request ->
        record log request;
        match !replies with
        | (status, headers, body) :: rest ->
            replies := rest;
            Fetch_mock.respond ~status
              ~headers:(Http.Header.of_list headers)
              body request
        | [] -> Alcotest.fail "unexpected typed-media request")
  in
  let transport =
    Msc4108.Rendezvous.transport_of_client
      ~sleep:(fun _ -> ())
      (client_of fetch)
  in
  let channel, _ =
    match
      Msc4108.Rendezvous.accept transport
        ~rendezvous_url:
          (Uriz.of_string_exn "https://hs.example/rendezvous/typed")
        ()
    with
    | Ok value -> value
    | Error _ -> Alcotest.fail "typed-media accept failed"
  in
  check_string "parameterized text/plain body" "payload"
    (match Msc4108.Rendezvous.receive channel with
    | Ok body -> body
    | Error _ -> Alcotest.fail "parameterized text/plain was rejected");
  let calls = requests log in
  check_string "typed If-None-Match encoding" "\"initial\""
    (Option.get (header (List.nth calls 1) "if-none-match"));
  let transport, _, _, _ =
    fake_rendezvous
      [
        Ok (rv_response ~etag:(rv_etag "initial") 200 "");
        Ok
          (rv_response ~etag:(rv_etag "message")
             ~content_type:(rv_content_type "text/html")
             200 "payload");
      ]
      ()
  in
  let channel, _ =
    Result.get_ok
      (Msc4108.Rendezvous.accept transport
         ~rendezvous_url:
           (Uriz.of_string_exn "https://hs.example/rendezvous/typed")
         ())
  in
  check_bool "non-text media type is rejected" true
    (match Msc4108.Rendezvous.receive channel with
    | Error (Msc4108.Rendezvous.Invalid_content_type "text/html") -> true
    | _ -> false)

let test_qr_rendezvous_probe () =
  let log, fetch =
    mock_seq
      [
        (200, {|{"create_available":true}|});
        (200, {|{"create_available":false}|});
        (404, "{}");
        (403, "{}");
        (500, "{}");
      ]
  in
  let client = client_of fetch in
  check_bool "available" true (ok (Qr_login.rendezvous_server_supported client));
  check_bool "server disabled" false
    (ok (Qr_login.rendezvous_server_supported client));
  check_bool "404 is capability false" false
    (ok (Qr_login.rendezvous_server_supported client));
  check_bool "403 is capability false" false
    (ok (Qr_login.rendezvous_server_supported client));
  (match Qr_login.rendezvous_server_supported client with
  | Error (Error.Http_error { status = 500; _ }) -> ()
  | Error error ->
      Alcotest.failf "expected HTTP 500, got %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "expected HTTP 500");
  List.iter
    (fun request ->
      check_string "discovery path"
        (default_homeserver ^ Qr_login.rendezvous_path)
        request.url;
      check_bool "probe has no bearer token" true
        (Option.is_none (header request "authorization")))
    (requests log)

let test_qr_rendezvous_paths () =
  check_string "MSC4388 discovery path"
    "/_matrix/client/unstable/io.element.msc4388/rendezvous"
    Qr_login.rendezvous_path;
  check_string "MSC4108 creation path"
    "/_matrix/client/unstable/org.matrix.msc4108/rendezvous"
    Msc4108.rendezvous_path;
  check_bool "discovery and creation paths differ" true
    (not (String.equal Qr_login.rendezvous_path Msc4108.rendezvous_path))

let test_qr_rendezvous_matrix_errors_and_malformed () =
  let _, fetch =
    mock_seq
      [
        (403, {|{"errcode":"M_FORBIDDEN","error":"no"}|});
        (404, {|{"errcode":"M_NOT_FOUND","error":"gone"}|});
        (200, "{}");
      ]
  in
  let client = client_of fetch in
  check_bool "M_FORBIDDEN is capability false" false
    (ok (Qr_login.rendezvous_server_supported client));
  check_bool "M_NOT_FOUND is capability false" false
    (ok (Qr_login.rendezvous_server_supported client));
  match Qr_login.rendezvous_server_supported client with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "expected JSON error, got %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "missing create_available was accepted"

let test_qr_secure_channel_boundary () =
  let value = qr_ok (Qr_login.of_base64 qr_vector) in
  check_bool "same-side QR is invalid" true
    (Qr_login.establish_secure_channel ~expected_intent:Qr_login.Reciprocate
       value
    = Error Qr_login.Invalid_channel_intent);
  check_bool "MSC4388 secure channel is explicitly unsupported" true
    (Qr_login.establish_secure_channel ~expected_intent:Qr_login.Login value
    = Error Qr_login.Unsupported_qr_code_type)

let () =
  Alcotest.run "matrix-oauth"
    [
      ( "metadata discovery",
        [
          Alcotest.test_case "stable /auth_metadata" `Quick
            (run test_metadata_v1);
          Alcotest.test_case "metadata URL validation" `Quick
            (run test_metadata_url_decode_validation);
          Alcotest.test_case "cached metadata" `Quick (run test_metadata_cache);
          Alcotest.test_case "cache failure and scope" `Quick
            (run test_metadata_cache_failure_and_scope);
          Alcotest.test_case "cache expiry" `Quick
            (run test_metadata_cache_expiry);
          Alcotest.test_case "Cache-Control lifetime" `Quick
            (run test_metadata_cache_control_lifetime);
          Alcotest.test_case "Cache-Control directives" `Quick
            (run test_metadata_cache_control_directives);
          Alcotest.test_case "typed Cache-Control" `Quick
            (run test_metadata_cache_control_typed);
          Alcotest.test_case "Expires" `Quick (run test_metadata_cache_expires);
          Alcotest.test_case "monotonic cache clock" `Quick
            (run test_metadata_cache_monotonic_clock);
          Alcotest.test_case "well-known Cache-Control lifetime" `Quick
            (run test_metadata_cache_control_well_known);
          Alcotest.test_case "unstable Cache-Control lifetime" `Quick
            (run test_metadata_cache_control_unstable);
          Alcotest.test_case "falls back to the MSC2965 path" `Quick
            (run test_metadata_unstable_fallback);
          Alcotest.test_case "falls back to the well-known issuer" `Quick
            (run test_metadata_well_known_fallback);
          Alcotest.test_case "the issuer step needs ?http" `Quick
            (run test_metadata_well_known_needs_http);
          Alcotest.test_case "no OAuth API anywhere" `Quick
            (run test_metadata_none_anywhere);
          Alcotest.test_case "validate" `Quick (run test_metadata_validate);
          Alcotest.test_case "account management URL" `Quick
            (run test_account_management_url);
          Alcotest.test_case "stable and unstable scope dialects" `Quick
            (run test_scope_dialects);
          Alcotest.test_case "compatible default scope dialect" `Quick
            (run test_scope_dialect_selection);
          Alcotest.test_case "MSC4191 account action aliases" `Quick
            (run test_account_action_aliases);
        ] );
      ( "registration",
        [
          Alcotest.test_case "the RFC 7591 document" `Quick
            (run test_registration);
          Alcotest.test_case "no registration endpoint" `Quick
            (run test_registration_without_endpoint);
          Alcotest.test_case "JWKS metadata validation" `Quick
            (run test_registration_jwks_validation);
          Alcotest.test_case "device registration metadata" `Quick
            test_device_registration_metadata;
          Alcotest.test_case "browser registration uses bound port" `Quick
            test_browser_registration_uses_bound_port;
        ] );
      ( "pkce",
        [
          Alcotest.test_case "RFC 7636 appendix B" `Quick test_pkce_known_answer;
          Alcotest.test_case "generated verifiers" `Quick test_pkce_generated;
          Alcotest.test_case "rejects bad verifiers" `Quick
            test_pkce_rejects_bad_verifiers;
        ] );
      ( "authorisation request",
        [
          Alcotest.test_case "URL parameters" `Quick test_authorization_url;
          Alcotest.test_case "prompt, login_hint, response_mode" `Quick
            test_authorization_url_options;
          Alcotest.test_case "generated device id, state and PKCE" `Quick
            test_authorization_request_generates;
        ] );
      ( "device authorisation",
        [
          Alcotest.test_case "response codec" `Quick (run test_device_codec);
          Alcotest.test_case "exact request body" `Quick
            (run test_device_request);
          Alcotest.test_case "unstable scope request" `Quick
            (run test_device_request_unstable_scope);
          Alcotest.test_case "token poll and error classes" `Quick
            (run test_device_poll);
        ] );
      ( "callback",
        [
          Alcotest.test_case "query response mode" `Quick
            test_parse_redirect_query;
          Alcotest.test_case "fragment response mode" `Quick
            test_parse_redirect_fragment;
          Alcotest.test_case "redirect escaping" `Quick
            test_parse_redirect_escaping;
          Alcotest.test_case "user denied" `Quick test_parse_redirect_denied;
          Alcotest.test_case "malformed" `Quick test_parse_redirect_malformed;
        ] );
      ( "browser flow",
        [ Alcotest.test_case "timeout" `Quick test_login_timeout ] );
      ( "tokens",
        [
          Alcotest.test_case "authorisation code exchange" `Quick
            (run test_exchange);
          Alcotest.test_case "an OAuth error survives" `Quick
            (run test_exchange_error_is_readable);
          Alcotest.test_case "refresh" `Quick (run test_refresh);
          Alcotest.test_case "revoke" `Quick (run test_revoke);
          Alcotest.test_case "logout revokes both tokens" `Quick
            (run test_logout_revokes_both);
          Alcotest.test_case "no revocation endpoint" `Quick
            (run test_revoke_without_endpoint);
          Alcotest.test_case "same-origin needs no ?http" `Quick
            (run test_same_origin_needs_no_http);
          Alcotest.test_case "transport errors redact endpoint queries" `Quick
            (run test_transport_error_redacts_endpoint_query);
          Alcotest.test_case "off-origin without ?http is refused" `Quick
            (run test_off_origin_without_http_is_refused);
          Alcotest.test_case "finish_login calls whoami" `Quick
            (run test_finish_login);
        ] );
      ( "loopback listener",
        [
          Alcotest.test_case "serves one callback" `Quick test_loopback;
          Alcotest.test_case "rejects bad requests and continues" `Quick
            test_loopback_rejects_and_continues;
          Alcotest.test_case "validates custom callback paths" `Quick
            test_loopback_path_validation;
        ] );
      ( "stored logout",
        [
          Alcotest.test_case "routes by authentication provenance" `Quick
            test_logout_session_routing;
        ] );
      ( "automatic refresh",
        [
          Alcotest.test_case "replays with rotated token" `Quick
            test_auto_refresh_replays_with_rotated_token;
          Alcotest.test_case "preserves omitted refresh token" `Quick
            test_auto_refresh_preserves_omitted_refresh_token;
          Alcotest.test_case "replays at most once" `Quick
            test_auto_refresh_replays_at_most_once;
          Alcotest.test_case "notifies invalid_grant" `Quick
            test_auto_refresh_notifies_invalid_grant_once;
          Alcotest.test_case "isolates invalid_grant callback failures" `Quick
            test_auto_refresh_invalid_grant_notification_isolated;
          Alcotest.test_case "shared profile persists and reuses rotation"
            `Quick test_auto_refresh_shared_profile;
          Alcotest.test_case "isolates detached persistence cancellation" `Quick
            test_auto_refresh_detached_persistence_cancellation_isolated;
          Alcotest.test_case "shares invalid_grant notification" `Quick
            test_auto_refresh_notifies_shared_invalid_grant_once;
          Alcotest.test_case "does not notify transient refresh failures" `Quick
            test_auto_refresh_does_not_notify_transient_failure;
          Alcotest.test_case "survives initiating request cancellation" `Quick
            test_auto_refresh_survives_initiator_cancellation;
          Alcotest.test_case "is opt-in" `Quick test_auto_refresh_is_opt_in;
        ] );
      ( "persisted refresh",
        [
          Alcotest.test_case "reactive discovery retry" `Quick
            (test_persisted_preparation_retry ~proactive:false ~invalid:false);
          Alcotest.test_case "proactive discovery retry" `Quick
            (test_persisted_preparation_retry ~proactive:true ~invalid:false);
          Alcotest.test_case "reactive metadata validation retry" `Quick
            (test_persisted_preparation_retry ~proactive:false ~invalid:true);
          Alcotest.test_case "proactive metadata validation retry" `Quick
            (test_persisted_preparation_retry ~proactive:true ~invalid:true);
          Alcotest.test_case "reactive uncertain exchange" `Quick
            (test_persisted_exchange_failure ~proactive:false);
          Alcotest.test_case "proactive uncertain exchange" `Quick
            (test_persisted_exchange_failure ~proactive:true);
        ] );
      ( "MSC4388 QR boundary",
        [
          Alcotest.test_case "Rust payload vector" `Quick test_qr_msc4388_vector;
          Alcotest.test_case "malformed payloads" `Quick
            test_qr_msc4388_rejects_malformed;
          Alcotest.test_case "rendezvous capability probe" `Quick
            (run test_qr_rendezvous_probe);
          Alcotest.test_case "Matrix errors and malformed response" `Quick
            (run test_qr_rendezvous_matrix_errors_and_malformed);
          Alcotest.test_case "typed unsupported channel" `Quick
            test_qr_secure_channel_boundary;
        ] );
      ( "MSC4108 QR codec",
        [
          Alcotest.test_case "Rust payload vectors" `Quick
            test_qr_msc4108_vectors;
          Alcotest.test_case "malformed payloads" `Quick
            test_qr_msc4108_rejects_malformed;
          Alcotest.test_case "rendezvous request pipeline" `Quick
            test_qr_msc4108_rendezvous_pipeline;
          Alcotest.test_case "uses distinct rendezvous endpoint" `Quick
            test_qr_rendezvous_paths;
          Alcotest.test_case "rendezvous terminal and expiry" `Quick
            test_qr_msc4108_rendezvous_terminal_and_expiry;
          Alcotest.test_case "rejects expired creation" `Quick
            test_qr_msc4108_rejects_expired_creation;
          Alcotest.test_case "rendezvous client transport" `Quick
            (run test_qr_msc4108_client_transport);
          Alcotest.test_case "typed ETags" `Quick
            (run test_qr_msc4108_typed_etags);
          Alcotest.test_case "HTTP-date expiry" `Quick
            (run test_qr_msc4108_http_dates);
          Alcotest.test_case "typed media type" `Quick
            (run test_qr_msc4108_typed_media_type);
        ] );
    ]
