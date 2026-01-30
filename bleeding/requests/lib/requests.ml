(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OCaml HTTP client library with streaming support *)

let src = Logs.Src.create "requests" ~doc:"HTTP Client Library"
module Log = (val Logs.src_log src : Logs.LOG)

module Method = Method
module Mime = Mime
module Headers = Headers
module Http_date = Http_date
module Http_version = Http_version
module Auth = Auth
module Proxy = Proxy
module Proxy_tunnel = Proxy_tunnel
module Timeout = Timeout
module Body = Body
module Response = Response
module One = One
module Http_client = Http_client
module Status = Status
module Error = Error
module Retry = Retry
module Cache_control = Cache_control
module Response_limits = Response_limits
module Expect_continue = Expect_continue
module Version = Version
module Link = Link
module Timing = Timing
module Header_name = Header_name
module Header_parsing = Header_parsing
module Websocket = Websocket
module Signature = Signature

(** Minimum TLS version configuration - re-exported from Tls_config. *)
type tls_version = Tls_config.tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)

(* Main API - Session functionality with connection pooling *)

(** Protocol hint for endpoints - remembers ALPN negotiation results. *)
type protocol_hint = H1 | H2

type t = T : {
  sw : Eio.Switch.t;
  clock : [> float Eio.Time.clock_ty] Eio.Resource.t;
  net : [> [> `Generic] Eio.Net.ty] Eio.Resource.t;
  http_pool : unit Conpool.t;
      (** HTTP/1.x pool - exclusive access, no protocol state *)
  https_pool : unit Conpool.t;
      (** HTTPS pool - exclusive access, no protocol state *)
  h2_pool : H2_conpool_handler.h2_state Conpool.t;
      (** HTTP/2 pool - shared access with H2 client state *)
  protocol_hints : (string, protocol_hint) Hashtbl.t;
      (** Maps "host:port" to protocol hint from ALPN *)
  protocol_hints_mutex : Eio.Mutex.t;
  cookie_jar : Cookeio_jar.t;
  cookie_mutex : Eio.Mutex.t;
  default_headers : Headers.t;
  auth : Auth.t option;
  timeout : Timeout.t;
  follow_redirects : bool;
  max_redirects : int;
  verify_tls : bool;
  tls_config : Tls.Config.client option;
  retry : Retry.config option;
  persist_cookies : bool;
  xdg : Xdge.t option;
  auto_decompress : bool;
  expect_100_continue : Expect_continue.t;  (** 100-continue configuration *)
  base_url : string option;  (** Per Recommendation #11: Base URL for relative paths *)
  xsrf_cookie_name : string option;  (** Per Recommendation #24: XSRF cookie name *)
  xsrf_header_name : string;  (** Per Recommendation #24: XSRF header name *)
  proxy : Proxy.config option;  (** HTTP/HTTPS proxy configuration *)
  allow_insecure_auth : bool;  (** Allow auth over HTTP for dev/testing *)
  nonce_counter : Auth.Nonce_counter.t;  (** Digest auth nonce count tracker *)

  (* Statistics - mutable but NOTE: when sessions are derived via record update
     syntax ({t with field = value}), these are copied not shared. Each derived
     session has independent statistics. Use the same session object to track
     cumulative stats. *)
  mutable requests_made : int;
  mutable total_time : float;
  mutable retries_count : int;
} -> t

let create
    ~sw
    ?http_pool
    ?https_pool
    ?cookie_jar
    ?(default_headers = Headers.empty)
    ?auth
    ?(timeout = Timeout.default)
    ?(follow_redirects = true)
    ?(max_redirects = 10)
    ?(verify_tls = true)
    ?tls_config
    ?(min_tls_version = TLS_1_2)
    ?(max_connections_per_host = 10)
    ?(connection_idle_timeout = 60.0)
    ?(connection_lifetime = 300.0)
    ?retry
    ?(persist_cookies = false)
    ?xdg
    ?(auto_decompress = true)
    ?(expect_100_continue = `Threshold Expect_continue.default_threshold)
    ?base_url
    ?(xsrf_cookie_name = Some "XSRF-TOKEN")  (* Per Recommendation #24 *)
    ?(xsrf_header_name = "X-XSRF-TOKEN")
    ?proxy
    ?(allow_insecure_auth = false)
    env =

  Mirage_crypto_rng_unix.use_default (); (* avsm: is this bad to do twice? very common footgun to forget to initialise *)
  let clock = env#clock in
  let net = env#net in

  let xdg = match xdg, persist_cookies with
    | Some x, _ -> Some x
    | None, true -> Some (Xdge.create env#fs "requests")
    | None, false -> None
  in

  (* Create TLS config for HTTPS pool if needed
     Per Recommendation #6: Enforce minimum TLS version *)
  let tls_config = Tls_config.create_client_opt
    ?existing_config:tls_config
    ~verify_tls
    ~min_tls_version
    ~host:"session-init"
    ()
  in

  (* Create connection pools if not provided *)
  let pool_config = Conpool.Config.make
    ~max_connections_per_endpoint:max_connections_per_host
    ~max_idle_time:connection_idle_timeout
    ~max_connection_lifetime:connection_lifetime
    ()
  in

  (* HTTP pool - plain TCP connections *)
  let http_pool = match http_pool with
    | Some p -> p
    | None ->
        Conpool.create_basic ~sw ~net ~clock ~config:pool_config ()
  in

  (* HTTPS pool - TLS-wrapped connections *)
  let https_pool = match https_pool with
    | Some p -> p
    | None ->
        Conpool.create_basic ~sw ~net ~clock ?tls:tls_config ~config:pool_config ()
  in

  (* HTTP/2 pool - shared connections with H2 state *)
  let h2_pool =
    Conpool.create ~sw ~net ~clock ?tls:tls_config ~config:pool_config
      ~protocol:H2_conpool_handler.h2_protocol ()
  in

  (* Protocol hints - remember ALPN negotiation results *)
  let protocol_hints = Hashtbl.create 32 in

  Log.info (fun m -> m "Created Requests session with connection pools (max_per_host=%d, TLS=%b)"
    max_connections_per_host (Option.is_some tls_config));

  let cookie_jar = match cookie_jar, persist_cookies, xdg with
    | Some jar, _, _ -> jar
    | None, true, Some xdg_ctx ->
        let data_dir = Xdge.data_dir xdg_ctx in
        let cookie_file = Eio.Path.(data_dir / "cookies.txt") in
        Cookeio_jar.load ~clock cookie_file
    | None, _, _ ->
        Cookeio_jar.create ()
  in

  (* Build expect_100_continue configuration from polymorphic variant *)
  let expect_100_timeout = Timeout.expect_100_continue timeout |> Option.value ~default:1.0 in
  let expect_100_config = Expect_continue.of_config ~timeout:expect_100_timeout expect_100_continue in

  (* Normalize base_url: remove trailing slash for consistent path joining *)
  let base_url = Option.map (fun url ->
    if String.length url > 0 && url.[String.length url - 1] = '/' then
      String.sub url 0 (String.length url - 1)
    else url
  ) base_url in

  T {
    sw;
    clock;
    net;
    http_pool;
    https_pool;
    h2_pool;
    protocol_hints;
    protocol_hints_mutex = Eio.Mutex.create ();
    cookie_jar;
    cookie_mutex = Eio.Mutex.create ();
    default_headers;
    auth;
    timeout;
    follow_redirects;
    max_redirects;
    verify_tls;
    tls_config;
    retry;
    persist_cookies;
    xdg;
    auto_decompress;
    expect_100_continue = expect_100_config;
    base_url;
    xsrf_cookie_name;
    xsrf_header_name;
    proxy;
    allow_insecure_auth;
    nonce_counter = Auth.Nonce_counter.create ();
    requests_made = 0;
    total_time = 0.0;
    retries_count = 0;
  }

let set_default_header (T t) key value =
  T { t with default_headers = Headers.set (Header_name.of_string key) value t.default_headers }

let remove_default_header (T t) key =
  T { t with default_headers = Headers.remove (Header_name.of_string key) t.default_headers }

let set_auth (T t) auth =
  Log.debug (fun m -> m "Setting authentication method");
  T { t with auth = Some auth }

let clear_auth (T t) =
  Log.debug (fun m -> m "Clearing authentication");
  T { t with auth = None }

let set_timeout (T t) timeout =
  Log.debug (fun m -> m "Setting timeout: %a" Timeout.pp timeout);
  T { t with timeout }

let set_retry (T t) config =
  Log.debug (fun m -> m "Setting retry config: max_retries=%d" config.Retry.max_retries);
  T { t with retry = Some config }

let cookies (T t) = t.cookie_jar
let clear_cookies (T t) = Cookeio_jar.clear t.cookie_jar

let set_proxy (T t) config =
  Log.debug (fun m -> m "Setting proxy: %s:%d" config.Proxy.host config.Proxy.port);
  T { t with proxy = Some config }

let clear_proxy (T t) =
  Log.debug (fun m -> m "Clearing proxy configuration");
  T { t with proxy = None }

let proxy (T t) = t.proxy

(* Redirect handling - delegated to shared Redirect module *)

(** {1 URL Resolution and Path Templating}

    Per Recommendation #11: Base URL support with RFC 3986 resolution.
    Per Recommendation #29: Path parameter templating. *)

(** Check if a URL is relative (no scheme) *)
let is_relative_url url =
  let uri = Uri.of_string url in
  Option.is_none (Uri.scheme uri)

(** Resolve a URL against a base URL per RFC 3986 Section 5.
    If the URL is already absolute, return it unchanged.
    If base_url is None, return the original URL. *)
let resolve_url ?base_url url =
  match base_url with
  | None -> url
  | Some base ->
      if is_relative_url url then begin
        let base_uri = Uri.of_string base in
        let rel_uri = Uri.of_string url in
        let scheme = Uri.scheme base_uri |> Option.value ~default:"https" in
        let resolved = Uri.resolve scheme base_uri rel_uri in
        Log.debug (fun m -> m "Resolved relative URL %s against base %s -> %s"
          url base (Uri.to_string resolved));
        Uri.to_string resolved
      end else
        url  (* Already absolute *)

(** Substitute path parameters in a URL template.
    Per Recommendation #29 and RFC 6570 (simplified).
    Template: "/users/{id}/posts/{post_id}"
    Params: [("id", "123"); ("post_id", "456")]
    Result: "/users/123/posts/456"
    Values are automatically URL-encoded. *)
let substitute_path_params url params =
  List.fold_left (fun url (key, value) ->
    let pattern = "{" ^ key ^ "}" in
    let encoded_value = Uri.pct_encode value in
    let rec replace s =
      match String.split_on_char '{' s with
      | [] -> ""
      | [single] -> single
      | before :: rest ->
          let rest_str = String.concat "{" rest in
          if String.length rest_str >= String.length key + 1 &&
             String.sub rest_str 0 (String.length key) = key &&
             rest_str.[String.length key] = '}' then
            before ^ encoded_value ^ String.sub rest_str (String.length key + 1)
              (String.length rest_str - String.length key - 1)
          else
            before ^ "{" ^ replace rest_str
    in
    if String.length pattern > 0 then replace url else url
  ) url params

(** {1 XSRF Token Handling}

    Per Recommendation #24: Automatically inject XSRF tokens from cookies. *)

(** Extract XSRF token from cookies and add to headers if:
    1. xsrf_cookie_name is configured
    2. The cookie exists
    3. The request is same-origin (security) *)
let apply_xsrf_token ~cookie_jar ~clock ~xsrf_cookie_name ~xsrf_header_name ~url headers =
  match xsrf_cookie_name with
  | None -> headers  (* XSRF handling disabled *)
  | Some cookie_name ->
      let uri = Uri.of_string url in
      let domain = Uri.host uri |> Option.value ~default:"" in
      let path = Uri.path uri in
      let is_secure = Uri.scheme uri = Some "https" in

      (* Get cookies for this domain *)
      let cookies = Cookeio_jar.get_cookies cookie_jar ~clock
        ~domain ~path ~is_secure in

      (* Find the XSRF token cookie *)
      let xsrf_value = List.find_map (fun cookie ->
        if Cookeio.name cookie = cookie_name then
          Some (Cookeio.value cookie)
        else
          None
      ) cookies in

      match xsrf_value with
      | Some token ->
          Log.debug (fun m -> m "Adding XSRF token header: %s" xsrf_header_name);
          (* XSRF header name is configurable, use string variant *)
          Headers.set_string xsrf_header_name token headers
      | None -> headers

(* Internal request function using connection pools *)
let make_request_internal (T t) ?headers ?body ?auth ?timeout ?follow_redirects ?max_redirects
    ?(path_params=[]) ~method_ url =
  let start_time = Unix.gettimeofday () in
  let method_str = Method.to_string method_ in

  (* Per Recommendation #29: Substitute path parameters first *)
  let url = if path_params = [] then url else substitute_path_params url path_params in

  (* Per Recommendation #11: Resolve relative URLs against base_url *)
  let url = resolve_url ?base_url:t.base_url url in

  Log.info (fun m -> m "Making %s request to %s" method_str url);

  (* Merge headers *)
  let headers = match headers with
    | Some h -> Headers.merge t.default_headers h
    | None -> t.default_headers
  in

  (* Add default User-Agent if not already set - per RFC 9110 Section 10.1.5 *)
  let headers =
    if not (Headers.mem `User_agent headers) then
      Headers.set `User_agent Version.user_agent headers
    else
      headers
  in

  (* Per Recommendation #24: Apply XSRF token from cookies *)
  let headers = Eio.Mutex.use_ro t.cookie_mutex (fun () ->
    apply_xsrf_token
      ~cookie_jar:t.cookie_jar
      ~clock:t.clock
      ~xsrf_cookie_name:t.xsrf_cookie_name
      ~xsrf_header_name:t.xsrf_header_name
      ~url
      headers
  ) in

  (* Use provided auth or default *)
  let auth = match auth with
    | Some a -> Some a
    | None -> t.auth
  in

  (* Apply auth with HTTPS validation per RFC 7617/6750 *)
  let headers = match auth with
    | Some a ->
        Log.debug (fun m -> m "Applying authentication");
        Auth.apply_secure ~allow_insecure_auth:t.allow_insecure_auth ~url a headers
    | None -> headers
  in

  (* Add content type from body *)
  let base_headers = match body with
    | Some b -> (match Body.content_type b with
        | Some mime -> Headers.content_type mime headers
        | None -> headers)
    | None -> headers
  in

  (* Add Accept-Encoding header for auto-decompression if not already set *)
  let base_headers =
    if t.auto_decompress && not (Headers.mem `Accept_encoding base_headers) then
      Headers.set `Accept_encoding "gzip, deflate" base_headers
    else
      base_headers
  in

  (* Get request body, defaulting to empty *)
  let request_body = Option.value ~default:Body.empty body in

  (* Helper to extract and store cookies from response headers *)
  let extract_cookies_from_headers resp_headers url_str =
    let uri = Uri.of_string url_str in
    let cookie_domain = Uri.host uri |> Option.value ~default:"" in
    let cookie_path = Uri.path uri in
    Eio.Mutex.use_rw ~protect:true t.cookie_mutex (fun () ->
      match Headers.get_all `Set_cookie resp_headers with
      | [] -> ()
      | cookie_headers ->
          Log.debug (fun m -> m "Received %d Set-Cookie headers" (List.length cookie_headers));
          List.iter (fun cookie_str ->
            let now = fun () -> Ptime.of_float_s (Eio.Time.now t.clock) |> Option.get in
            match Cookeio.of_set_cookie_header ~now ~domain:cookie_domain ~path:cookie_path cookie_str with
            | Ok cookie ->
                Log.debug (fun m -> m "Storing cookie: %s" (Cookeio.name cookie));
                Cookeio_jar.add_cookie t.cookie_jar cookie
            | Error msg ->
                Log.warn (fun m -> m "Failed to parse cookie: %s (%s)" cookie_str msg)
          ) cookie_headers
    )
  in

  (* Track the original URL for cross-origin redirect detection *)
  let original_uri = Uri.of_string url in

  let response =

        (* Execute request with redirect handling
           headers_for_request: the headers to use for this specific request (may have auth stripped)
           ~method_: the HTTP method for this request (may be changed by 303 redirect)
           ~body: the request body (may be stripped by 303 redirect) *)
        let rec make_with_redirects ~headers_for_request ~method_ ~body url_to_fetch redirects_left =
          let uri_to_fetch = Uri.of_string url_to_fetch in

          (* Parse the redirect URL to get correct host and port *)
          let redirect_host = match Uri.host uri_to_fetch with
            | Some h -> h
            | None -> raise (Error.err (Error.Invalid_redirect { url = url_to_fetch; reason = "URL must contain a host" }))
          in
          let redirect_port = match Uri.scheme uri_to_fetch, Uri.port uri_to_fetch with
            | Some "https", None -> 443
            | Some "https", Some p -> p
            | Some "http", None -> 80
            | Some "http", Some p -> p
            | _, Some p -> p
            | _ -> 80
          in

          (* Create endpoint for this specific URL *)
          let redirect_endpoint = Conpool.Endpoint.make ~host:redirect_host ~port:redirect_port in

          (* Determine if we need TLS based on this URL's scheme *)
          let redirect_is_https = match Uri.scheme uri_to_fetch with
            | Some "https" -> true
            | _ -> false
          in

          (* Choose the appropriate connection pool for this URL *)
          let redirect_pool = if redirect_is_https then t.https_pool else t.http_pool in

          (* Get cookies for this specific URL *)
          let fetch_domain = redirect_host in
          let fetch_path = Uri.path uri_to_fetch in
          let fetch_is_secure = redirect_is_https in
          let headers_with_cookies =
            Eio.Mutex.use_ro t.cookie_mutex (fun () ->
              let cookies = Cookeio_jar.get_cookies t.cookie_jar ~clock:t.clock
                ~domain:fetch_domain ~path:fetch_path ~is_secure:fetch_is_secure in
              match cookies with
              | [] ->
                  Log.debug (fun m -> m "No cookies found for %s%s" fetch_domain fetch_path);
                  headers_for_request
              | cookies ->
                  let cookie_header = Cookeio.make_cookie_header cookies in
                  Log.debug (fun m -> m "Adding %d cookies for %s%s: Cookie: %s"
                    (List.length cookies) fetch_domain fetch_path cookie_header);
                  Headers.set `Cookie cookie_header headers_for_request
            )
          in

          (* Log the request being made at Info level *)
          Log.info (fun m -> m "");
          Log.info (fun m -> m "=== Request to %s ===" url_to_fetch);
          Log.info (fun m -> m "> %s %s HTTP/1.1" method_str (Uri.to_string uri_to_fetch));
          Log.info (fun m -> m "> Request Headers:");
          Headers.to_list headers_with_cookies |> List.iter (fun (k, v) ->
            Log.info (fun m -> m ">   %s: %s" k v)
          );
          Log.info (fun m -> m "");

          (* Determine if we should use proxy for this URL *)
          let use_proxy = match t.proxy with
            | None -> false
            | Some proxy -> not (Proxy.should_bypass proxy url_to_fetch)
          in

          (* Helper to make endpoint key for protocol hints *)
          let endpoint_key = Printf.sprintf "%s:%d" redirect_host redirect_port in

          (* Get protocol hint for this endpoint *)
          let protocol_hint =
            Eio.Mutex.use_ro t.protocol_hints_mutex (fun () ->
              Hashtbl.find_opt t.protocol_hints endpoint_key)
          in

          let make_request_fn () =
            match use_proxy, redirect_is_https, t.proxy, protocol_hint with
            | false, true, _, Some H2 ->
                (* Known HTTP/2 - use h2_pool with shared connections *)
                Log.debug (fun m -> m "Using HTTP/2 for %s (from protocol hint)" url_to_fetch);
                Eio.Switch.run (fun conn_sw ->
                  let h2_conn = Conpool.connection ~sw:conn_sw t.h2_pool redirect_endpoint in
                  match H2_conpool_handler.request
                    ~state:h2_conn.Conpool.state
                    ~uri:uri_to_fetch
                    ~headers:headers_with_cookies
                    ~body
                    ~method_
                    ~auto_decompress:t.auto_decompress
                    ()
                  with
                  | Ok resp -> (resp.H2_adapter.status, resp.H2_adapter.headers, resp.H2_adapter.body)
                  | Error msg -> raise (Error.err (Error.Invalid_request { reason = "HTTP/2 error: " ^ msg }))
                )

            | false, true, _, Some H1 ->
                (* Known HTTP/1.x - use https_pool *)
                Log.debug (fun m -> m "Using HTTP/1.1 for %s (from protocol hint)" url_to_fetch);
                Eio.Switch.run (fun conn_sw ->
                  let conn_info = Conpool.connection ~sw:conn_sw redirect_pool redirect_endpoint in
                  Http_client.make_request_100_continue_decompress
                    ~expect_100:t.expect_100_continue
                    ~clock:t.clock
                    ~sw:t.sw
                    ~method_ ~uri:uri_to_fetch
                    ~headers:headers_with_cookies ~body
                    ~auto_decompress:t.auto_decompress conn_info.Conpool.flow
                )

            | false, _, _, _ ->
                (* Unknown protocol or non-HTTPS - use ALPN detection *)
                Eio.Switch.run (fun conn_sw ->
                  let conn_info = Conpool.connection ~sw:conn_sw redirect_pool redirect_endpoint in
                  (* Check ALPN negotiated protocol *)
                  let is_h2 = match conn_info.Conpool.tls_epoch with
                    | Some epoch -> epoch.Tls.Core.alpn_protocol = Some "h2"
                    | None -> false
                  in

                  (* Update protocol hint for future requests *)
                  if redirect_is_https then begin
                    let hint = if is_h2 then H2 else H1 in
                    Eio.Mutex.use_rw ~protect:true t.protocol_hints_mutex (fun () ->
                      Hashtbl.replace t.protocol_hints endpoint_key hint);
                    Log.debug (fun m -> m "Learned protocol for %s: %s"
                      endpoint_key (if is_h2 then "H2" else "H1"))
                  end;

                  if is_h2 then begin
                    (* First H2 connection - use H2_adapter for this request
                       (subsequent requests will use h2_pool) *)
                    Log.debug (fun m -> m "Using HTTP/2 for %s (ALPN negotiated)" url_to_fetch);
                    match H2_adapter.request
                      ~sw:conn_sw
                      ~flow:conn_info.Conpool.flow
                      ~uri:uri_to_fetch
                      ~headers:headers_with_cookies
                      ~body
                      ~method_
                      ~auto_decompress:t.auto_decompress
                      ()
                    with
                    | Ok resp -> (resp.H2_adapter.status, resp.H2_adapter.headers, resp.H2_adapter.body)
                    | Error msg -> raise (Error.err (Error.Invalid_request { reason = "HTTP/2 error: " ^ msg }))
                  end else begin
                    (* Use HTTP/1.x client *)
                    Http_client.make_request_100_continue_decompress
                      ~expect_100:t.expect_100_continue
                      ~clock:t.clock
                      ~sw:t.sw
                      ~method_ ~uri:uri_to_fetch
                      ~headers:headers_with_cookies ~body
                      ~auto_decompress:t.auto_decompress conn_info.Conpool.flow
                  end
                )

            | true, false, Some proxy, _ ->
                (* HTTP via proxy - connect to proxy and use absolute-URI form *)
                Log.debug (fun m -> m "Routing HTTP request via proxy %s:%d"
                  proxy.Proxy.host proxy.Proxy.port);
                let proxy_endpoint = Conpool.Endpoint.make
                  ~host:proxy.Proxy.host ~port:proxy.Proxy.port in
                (* Convert Auth.t to header value string *)
                let proxy_auth = match proxy.Proxy.auth with
                  | Some auth ->
                      let auth_headers = Auth.apply auth Headers.empty in
                      Headers.get `Authorization auth_headers
                  | None -> None
                in
                Conpool.with_connection t.http_pool proxy_endpoint (fun conn ->
                  (* Write request using absolute-URI form *)
                  Http_write.write_and_flush conn.Conpool.flow (fun w ->
                    Http_write.request_via_proxy w ~sw:t.sw ~method_ ~uri:uri_to_fetch
                      ~headers:headers_with_cookies ~body
                      ~proxy_auth
                  );
                  (* Read response *)
                  let limits = Response_limits.default in
                  let buf_read = Http_read.of_flow ~max_size:65536 conn.Conpool.flow in
                  let _version, status, resp_headers, body_str =
                    Http_read.response ~limits ~method_ buf_read in
                  (* Handle decompression if enabled *)
                  let body_str = match t.auto_decompress, Headers.get `Content_encoding resp_headers with
                    | true, Some encoding ->
                        Http_client.decompress_body ~limits ~content_encoding:encoding body_str
                    | _ -> body_str
                  in
                  (status, resp_headers, body_str)
                )

            | true, true, Some proxy, _ ->
                (* HTTPS via proxy - establish CONNECT tunnel then TLS *)
                Log.debug (fun m -> m "Routing HTTPS request via proxy %s:%d (CONNECT tunnel)"
                  proxy.Proxy.host proxy.Proxy.port);
                (* Establish TLS tunnel through proxy *)
                let tunnel_flow = Proxy_tunnel.connect_with_tls
                  ~sw:t.sw ~net:t.net ~clock:t.clock
                  ~proxy
                  ~target_host:redirect_host
                  ~target_port:redirect_port
                  ?tls_config:t.tls_config
                  ()
                in
                (* Send request through tunnel using normal format (not absolute-URI) *)
                Http_client.make_request_100_continue_decompress
                  ~expect_100:t.expect_100_continue
                  ~clock:t.clock
                  ~sw:t.sw
                  ~method_ ~uri:uri_to_fetch
                  ~headers:headers_with_cookies ~body
                  ~auto_decompress:t.auto_decompress tunnel_flow

            | true, _, None, _ ->
                (* Should not happen due to use_proxy check *)
                Conpool.with_connection redirect_pool redirect_endpoint (fun conn ->
                  Http_client.make_request_100_continue_decompress
                    ~expect_100:t.expect_100_continue
                    ~clock:t.clock
                    ~sw:t.sw
                    ~method_ ~uri:uri_to_fetch
                    ~headers:headers_with_cookies ~body
                    ~auto_decompress:t.auto_decompress conn.Conpool.flow
                )
          in

          (* Apply timeout if specified *)
          let status, resp_headers, response_body_str =
            let timeout_val = Option.value timeout ~default:t.timeout in
            match Timeout.total timeout_val with
            | Some seconds ->
                Log.debug (fun m -> m "Setting timeout: %.2f seconds" seconds);
                Eio.Time.with_timeout_exn t.clock seconds make_request_fn
            | None -> make_request_fn ()
          in

          (* Log response headers at Info level *)
          Log.info (fun m -> m "< HTTP/1.1 %d" status);
          Log.info (fun m -> m "< Response Headers:");
          Headers.to_list resp_headers |> List.iter (fun (k, v) ->
            Log.info (fun m -> m "<   %s: %s" k v)
          );
          Log.info (fun m -> m "");

          (* Extract and store cookies from this response (including redirect responses) *)
          extract_cookies_from_headers resp_headers url_to_fetch;

          (* Handle redirects if enabled *)
          let follow = Option.value follow_redirects ~default:t.follow_redirects in
          let max_redir = Option.value max_redirects ~default:t.max_redirects in

          if follow && (status >= 300 && status < 400) then begin
            if redirects_left <= 0 then begin
              Log.err (fun m -> m "Too many redirects (%d) for %s" max_redir url);
              raise (Error.err (Error.Too_many_redirects { url; count = max_redir; max = max_redir }))
            end;

            match Headers.get `Location resp_headers with
            | None ->
                Log.debug (fun m -> m "Redirect response missing Location header");
                (status, resp_headers, response_body_str, url_to_fetch)
            | Some location ->
                (* Validate redirect URL scheme - Per Recommendation #5 *)
                let _ = Redirect.validate_url location in

                (* Resolve relative redirects against the current URL *)
                let location_uri = Uri.of_string location in
                let absolute_location =
                  match Uri.host location_uri with
                  | Some _ -> location  (* Already absolute *)
                  | None ->
                      (* Relative redirect - resolve against current URL *)
                      let base_uri = uri_to_fetch in
                      let scheme = Option.value (Uri.scheme base_uri) ~default:"http" in
                      let resolved = Uri.resolve scheme base_uri location_uri in
                      Uri.to_string resolved
                in
                Log.info (fun m -> m "Following redirect to %s (%d remaining)" absolute_location redirects_left);
                (* Strip sensitive headers on cross-origin redirects (security)
                   Per Recommendation #1: Strip auth headers to prevent credential leakage *)
                let redirect_uri = Uri.of_string absolute_location in
                let headers_for_redirect =
                  if Redirect.same_origin original_uri redirect_uri then
                    headers_for_request
                  else begin
                    Log.debug (fun m -> m "Cross-origin redirect detected: stripping sensitive headers");
                    Redirect.strip_sensitive_headers headers_for_request
                  end
                in
                (* RFC 9110 Section 15.4.4: For 303 See Other, change method to GET
                   "A user agent can perform a retrieval request targeting that URI
                   (a GET or HEAD request if using HTTP)" *)
                let redirect_method, redirect_body =
                  if status = 303 then begin
                    match method_ with
                    | `POST | `PUT | `DELETE | `PATCH ->
                        Log.debug (fun m -> m "303 redirect: changing %s to GET and stripping body"
                          (Method.to_string method_));
                        (`GET, Body.empty)
                    | _ -> (method_, body)
                  end else
                    (method_, body)
                in
                make_with_redirects ~headers_for_request:headers_for_redirect
                  ~method_:redirect_method ~body:redirect_body
                  absolute_location (redirects_left - 1)
          end else
            (status, resp_headers, response_body_str, url_to_fetch)
        in

        let max_redir = Option.value max_redirects ~default:t.max_redirects in

        (* Apply HTTP Message Signature if configured (RFC 9421) *)
        let signed_headers = match auth with
          | Some a when Auth.is_signature a ->
              Auth.apply_signature ~clock:t.clock ~method_ ~uri:original_uri ~headers:base_headers a
          | _ -> base_headers
        in

        let final_status, final_headers, final_body_str, final_url =
          make_with_redirects ~headers_for_request:signed_headers
            ~method_ ~body:request_body url max_redir
        in

        let elapsed = Unix.gettimeofday () -. start_time in
        Log.info (fun m -> m "Request completed in %.3f seconds" elapsed);

        (* Create a flow from the body string *)
        let body_flow = Eio.Flow.string_source final_body_str in

        Response.Private.make
          ~sw:t.sw
          ~status:final_status
          ~headers:final_headers
          ~body:body_flow
          ~url:final_url
          ~elapsed
  in

  (* Cookies are extracted and stored during the redirect loop for each response,
     including the final response, so no additional extraction needed here *)

  (* Update statistics *)
  t.requests_made <- t.requests_made + 1;
  t.total_time <- t.total_time +. (Unix.gettimeofday () -. start_time);
  Log.info (fun m -> m "Request completed with status %d" (Response.status_code response));

  (* Save cookies to disk if persistence is enabled *)
  (match t.persist_cookies, t.xdg with
   | true, Some xdg_ctx ->
       let data_dir = Xdge.data_dir xdg_ctx in
       let cookie_file = Eio.Path.(data_dir / "cookies.txt") in
       Eio.Mutex.use_rw ~protect:true t.cookie_mutex (fun () ->
         Cookeio_jar.save cookie_file t.cookie_jar;
         Log.debug (fun m -> m "Saved cookies to %a" Eio.Path.pp cookie_file)
       )
   | _ -> ());

  response

(* Helper to handle Digest authentication challenges (401 and 407).
   Per RFC 7235: 401 uses WWW-Authenticate/Authorization headers,
   407 uses Proxy-Authenticate/Proxy-Authorization headers. *)
let handle_digest_auth (T t as wrapped_t) ~headers ~body ~auth ~timeout ~follow_redirects ~max_redirects ~method_ ~url response =
  let status = Response.status_code response in
  let auth_to_use = match auth with Some a -> a | None -> Option.value t.auth ~default:Auth.none in
  (* Handle both 401 Unauthorized and 407 Proxy Authentication Required *)
  let is_auth_challenge = (status = 401 || status = 407) && Auth.is_digest auth_to_use in
  if is_auth_challenge then begin
    match Auth.get_digest_credentials auth_to_use with
    | Some (username, password) ->
        (* RFC 7235: 401 uses WWW-Authenticate, 407 uses Proxy-Authenticate *)
        let challenge_header : Header_name.t = if status = 401 then `Www_authenticate else `Proxy_authenticate in
        let auth_header_name : Header_name.t = if status = 401 then `Authorization else `Proxy_authorization in
        (match Response.header challenge_header response with
         | Some www_auth ->
             (match Auth.parse_www_authenticate www_auth with
              | Some challenge ->
                  Log.info (fun m -> m "Received %s challenge (status %d), retrying with authentication"
                    (if status = 401 then "Digest" else "Proxy Digest") status);
                  let uri = Uri.of_string url in
                  let uri_path = Uri.path uri in
                  let uri_path = if uri_path = "" then "/" else uri_path in
                  (* Apply digest auth to headers with nonce counter for replay protection *)
                  let base_headers = Option.value headers ~default:Headers.empty in
                  (* Build the Authorization/Proxy-Authorization value manually *)
                  let auth_value = Auth.apply_digest
                    ~nonce_counter:t.nonce_counter
                    ~username ~password
                    ~method_:(Method.to_string method_)
                    ~uri:uri_path
                    ~challenge
                    Headers.empty
                  in
                  (* Get the auth value and set it on the correct header name *)
                  let auth_headers = match Headers.get `Authorization auth_value with
                    | Some v -> Headers.set auth_header_name v base_headers
                    | None -> base_headers
                  in
                  (* Retry with Digest auth - use Auth.none to prevent double-application *)
                  make_request_internal wrapped_t ~headers:auth_headers ?body ~auth:Auth.none ?timeout
                    ?follow_redirects ?max_redirects ~method_ url
              | None ->
                  Log.warn (fun m -> m "Could not parse Digest challenge from %s" (Header_name.to_string challenge_header));
                  response)
         | None ->
             Log.warn (fun m -> m "%d response has no %s header" status (Header_name.to_string challenge_header));
             response)
    | None -> response
  end else
    response

(* Public request function - executes synchronously with retry support *)
let request (T t as wrapped_t) ?headers ?body ?auth ?timeout ?follow_redirects ?max_redirects
    ?(path_params=[]) ~method_ url =
  (* Helper to wrap response with Digest auth handling *)
  let with_digest_handling response =
    handle_digest_auth wrapped_t ~headers ~body ~auth ~timeout ~follow_redirects ~max_redirects ~method_ ~url response
  in
  match t.retry with
  | None ->
      (* No retry configured, execute directly *)
      let response = make_request_internal wrapped_t ?headers ?body ?auth ?timeout
        ?follow_redirects ?max_redirects ~path_params ~method_ url in
      with_digest_handling response
  | Some retry_config ->
      (* Wrap in retry logic *)
      (* Check if an Eio.Io exception is retryable using the new error types *)
      let should_retry_exn = function
        | Eio.Io (Error.E e, _) -> Error.is_retryable e
        | Eio.Time.Timeout -> true
        | _ -> false
      in

      let rec attempt_with_status_retry attempt =
        if attempt > 1 then
          Log.info (fun m -> m "Retry attempt %d/%d for %s %s"
            attempt (retry_config.Retry.max_retries + 1)
            (Method.to_string method_) url);

        try
          let response = make_request_internal wrapped_t ?headers ?body ?auth ?timeout
            ?follow_redirects ?max_redirects ~path_params ~method_ url in
          (* Handle Digest auth challenge if applicable *)
          let response = with_digest_handling response in
          let status = Response.status_code response in

          (* Check if this status code should be retried *)
          if attempt <= retry_config.Retry.max_retries &&
             Retry.should_retry ~config:retry_config ~method_ ~status
          then begin
            (* Per Recommendation #4: Use Retry-After header when available *)
            let delay =
              if retry_config.respect_retry_after && (status = 429 || status = 503) then
                match Response.header `Retry_after response with
                | Some value ->
                    Retry.parse_retry_after value
                    |> Option.value ~default:(Retry.calculate_backoff ~config:retry_config ~attempt)
                | None -> Retry.calculate_backoff ~config:retry_config ~attempt
              else
                Retry.calculate_backoff ~config:retry_config ~attempt
            in
            Log.warn (fun m -> m "Request returned status %d (attempt %d/%d). Retrying in %.2f seconds..."
              status attempt (retry_config.Retry.max_retries + 1) delay);
            Eio.Time.sleep t.clock delay;
            t.retries_count <- t.retries_count + 1;
            attempt_with_status_retry (attempt + 1)
          end else
            response
        with exn when attempt <= retry_config.Retry.max_retries && should_retry_exn exn ->
          let delay = Retry.calculate_backoff ~config:retry_config ~attempt in
          Log.warn (fun m -> m "Request failed (attempt %d/%d): %s. Retrying in %.2f seconds..."
            attempt (retry_config.Retry.max_retries + 1) (Printexc.to_string exn) delay);
          Eio.Time.sleep t.clock delay;
          t.retries_count <- t.retries_count + 1;
          attempt_with_status_retry (attempt + 1)
      in
      attempt_with_status_retry 1

(* Convenience methods *)
let get t ?headers ?auth ?timeout ?params ?(path_params=[]) url =
  let url = match params with
    | Some p ->
        let uri = Uri.of_string url in
        let uri = List.fold_left (fun u (k, v) -> Uri.add_query_param' u (k, v)) uri p in
        Uri.to_string uri
    | None -> url
  in
  request t ?headers ?auth ?timeout ~path_params ~method_:`GET url

let post t ?headers ?body ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?body ?auth ?timeout ~path_params ~method_:`POST url

let put t ?headers ?body ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?body ?auth ?timeout ~path_params ~method_:`PUT url

let patch t ?headers ?body ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?body ?auth ?timeout ~path_params ~method_:`PATCH url

let delete t ?headers ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?auth ?timeout ~path_params ~method_:`DELETE url

let head t ?headers ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?auth ?timeout ~path_params ~method_:`HEAD url

let options t ?headers ?auth ?timeout ?(path_params=[]) url =
  request t ?headers ?auth ?timeout ~path_params ~method_:`OPTIONS url

(* Cmdliner integration module *)
module Cmd = struct
  open Cmdliner

  (** Source tracking for configuration values.
      Tracks where each configuration value came from for debugging
      and transparency. *)
  type source =
    | Default                (** Value from hardcoded default *)
    | Env of string          (** Value from environment variable (stores var name) *)
    | Cmdline                (** Value from command-line argument *)

  (** Wrapper for values with source tracking *)
  type 'a with_source = {
    value : 'a;
    source : source;
  }

  (** Proxy configuration from command line and environment *)
  type proxy_config = {
    proxy_url : string with_source option;  (** Proxy URL (from HTTP_PROXY/HTTPS_PROXY/etc) *)
    no_proxy : string with_source option;   (** NO_PROXY patterns *)
  }

  type config = {
    xdg : Xdge.t * Xdge.Cmd.t;
    persist_cookies : bool with_source;
    verify_tls : bool with_source;
    timeout : float option with_source;
    max_retries : int with_source;
    retry_backoff : float with_source;
    follow_redirects : bool with_source;
    max_redirects : int with_source;
    user_agent : string option with_source;
    verbose_http : bool with_source;
    proxy : proxy_config;
  }

  (** Helper to check environment variable and track source *)
  let check_env_bool ~app_name ~suffix ~default =
    let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
    match Sys.getenv_opt env_var with
    | Some v when String.lowercase_ascii v = "1" || String.lowercase_ascii v = "true" ->
        { value = true; source = Env env_var }
    | Some v when String.lowercase_ascii v = "0" || String.lowercase_ascii v = "false" ->
        { value = false; source = Env env_var }
    | Some _ | None -> { value = default; source = Default }

  let check_env_string ~app_name ~suffix =
    let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
    match Sys.getenv_opt env_var with
    | Some v when v <> "" -> Some { value = v; source = Env env_var }
    | Some _ | None -> None

  let check_env_float ~app_name ~suffix ~default =
    let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
    match Sys.getenv_opt env_var with
    | Some v ->
        (try { value = float_of_string v; source = Env env_var }
         with _ -> { value = default; source = Default })
    | None -> { value = default; source = Default }

  let check_env_int ~app_name ~suffix ~default =
    let env_var = String.uppercase_ascii app_name ^ "_" ^ suffix in
    match Sys.getenv_opt env_var with
    | Some v ->
        (try { value = int_of_string v; source = Env env_var }
         with _ -> { value = default; source = Default })
    | None -> { value = default; source = Default }

  (** Parse proxy configuration from environment.
      Follows standard HTTP_PROXY/HTTPS_PROXY/ALL_PROXY/NO_PROXY conventions. *)
  let proxy_from_env () =
    let proxy_url =
      (* Check in order of preference *)
      match Sys.getenv_opt "HTTP_PROXY" with
      | Some v when v <> "" -> Some { value = v; source = Env "HTTP_PROXY" }
      | _ ->
          match Sys.getenv_opt "http_proxy" with
          | Some v when v <> "" -> Some { value = v; source = Env "http_proxy" }
          | _ ->
              match Sys.getenv_opt "HTTPS_PROXY" with
              | Some v when v <> "" -> Some { value = v; source = Env "HTTPS_PROXY" }
              | _ ->
                  match Sys.getenv_opt "https_proxy" with
                  | Some v when v <> "" -> Some { value = v; source = Env "https_proxy" }
                  | _ ->
                      match Sys.getenv_opt "ALL_PROXY" with
                      | Some v when v <> "" -> Some { value = v; source = Env "ALL_PROXY" }
                      | _ ->
                          match Sys.getenv_opt "all_proxy" with
                          | Some v when v <> "" -> Some { value = v; source = Env "all_proxy" }
                          | _ -> None
    in
    let no_proxy =
      match Sys.getenv_opt "NO_PROXY" with
      | Some v when v <> "" -> Some { value = v; source = Env "NO_PROXY" }
      | _ ->
          match Sys.getenv_opt "no_proxy" with
          | Some v when v <> "" -> Some { value = v; source = Env "no_proxy" }
          | _ -> None
    in
    { proxy_url; no_proxy }

  let create config env sw =
    let xdg, _xdg_cmd = config.xdg in
    let retry = if config.max_retries.value > 0 then
      Some (Retry.create_config
        ~max_retries:config.max_retries.value
        ~backoff_factor:config.retry_backoff.value ())
    else None in

    let timeout = match config.timeout.value with
      | Some t -> Timeout.create ~total:t ()
      | None -> Timeout.default in

    (* Build proxy config if URL is set *)
    let proxy = match config.proxy.proxy_url with
      | Some { value = url; _ } ->
          let no_proxy = match config.proxy.no_proxy with
            | Some { value = np; _ } ->
                np |> String.split_on_char ','
                   |> List.map String.trim
                   |> List.filter (fun s -> s <> "")
            | None -> []
          in
          (* Parse proxy URL to extract components *)
          let uri = Uri.of_string url in
          let host = Uri.host uri |> Option.value ~default:"localhost" in
          let port = Uri.port uri |> Option.value ~default:8080 in
          let auth = match Uri.userinfo uri with
            | Some info ->
                (match String.index_opt info ':' with
                 | Some idx ->
                     let username = String.sub info 0 idx in
                     let password = String.sub info (idx + 1) (String.length info - idx - 1) in
                     Some (Auth.basic ~username ~password)
                 | None -> Some (Auth.basic ~username:info ~password:""))
            | None -> None
          in
          Some (Proxy.http ~port ?auth ~no_proxy host)
      | None -> None
    in

    let req = create ~sw
      ~xdg
      ~persist_cookies:config.persist_cookies.value
      ~verify_tls:config.verify_tls.value
      ~timeout
      ?retry
      ~follow_redirects:config.follow_redirects.value
      ~max_redirects:config.max_redirects.value
      ?proxy
      env in

    (* Set user agent if provided *)
    let req = match config.user_agent.value with
      | Some ua -> set_default_header req "User-Agent" ua
      | None -> req
    in

    req

  (* Individual terms - parameterized by app_name
     These terms return with_source wrapped values to track provenance *)

  let persist_cookies_term app_name =
    let doc = "Persist cookies to disk between sessions" in
    let env_name = String.uppercase_ascii app_name ^ "_PERSIST_COOKIES" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & flag & info ["persist-cookies"] ~env:env_info ~doc) in
    Term.(const (fun cmdline ->
      if cmdline then
        { value = true; source = Cmdline }
      else
        check_env_bool ~app_name ~suffix:"PERSIST_COOKIES" ~default:false
    ) $ cmdline_arg)

  let verify_tls_term app_name =
    let doc = "Skip TLS certificate verification (insecure)" in
    let env_name = String.uppercase_ascii app_name ^ "_NO_VERIFY_TLS" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & flag & info ["no-verify-tls"] ~env:env_info ~doc) in
    Term.(const (fun no_verify ->
      if no_verify then
        { value = false; source = Cmdline }
      else
        let env_val = check_env_bool ~app_name ~suffix:"NO_VERIFY_TLS" ~default:false in
        { value = not env_val.value; source = env_val.source }
    ) $ cmdline_arg)

  let timeout_term app_name =
    let doc = "Request timeout in seconds" in
    let env_name = String.uppercase_ascii app_name ^ "_TIMEOUT" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & opt (some float) None & info ["timeout"] ~env:env_info ~docv:"SECONDS" ~doc) in
    Term.(const (fun cmdline ->
      match cmdline with
      | Some t -> { value = Some t; source = Cmdline }
      | None ->
          match check_env_string ~app_name ~suffix:"TIMEOUT" with
          | Some { value = v; source } ->
              (try { value = Some (float_of_string v); source }
               with _ -> { value = None; source = Default })
          | None -> { value = None; source = Default }
    ) $ cmdline_arg)

  let retries_term app_name =
    let doc = "Maximum number of request retries" in
    let env_name = String.uppercase_ascii app_name ^ "_MAX_RETRIES" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & opt (some int) None & info ["max-retries"] ~env:env_info ~docv:"N" ~doc) in
    Term.(const (fun cmdline ->
      match cmdline with
      | Some n -> { value = n; source = Cmdline }
      | None -> check_env_int ~app_name ~suffix:"MAX_RETRIES" ~default:3
    ) $ cmdline_arg)

  let retry_backoff_term app_name =
    let doc = "Retry backoff factor for exponential delay" in
    let env_name = String.uppercase_ascii app_name ^ "_RETRY_BACKOFF" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & opt (some float) None & info ["retry-backoff"] ~env:env_info ~docv:"FACTOR" ~doc) in
    Term.(const (fun cmdline ->
      match cmdline with
      | Some f -> { value = f; source = Cmdline }
      | None -> check_env_float ~app_name ~suffix:"RETRY_BACKOFF" ~default:0.3
    ) $ cmdline_arg)

  let follow_redirects_term app_name =
    let doc = "Don't follow HTTP redirects" in
    let env_name = String.uppercase_ascii app_name ^ "_NO_FOLLOW_REDIRECTS" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & flag & info ["no-follow-redirects"] ~env:env_info ~doc) in
    Term.(const (fun no_follow ->
      if no_follow then
        { value = false; source = Cmdline }
      else
        let env_val = check_env_bool ~app_name ~suffix:"NO_FOLLOW_REDIRECTS" ~default:false in
        { value = not env_val.value; source = env_val.source }
    ) $ cmdline_arg)

  let max_redirects_term app_name =
    let doc = "Maximum number of redirects to follow" in
    let env_name = String.uppercase_ascii app_name ^ "_MAX_REDIRECTS" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & opt (some int) None & info ["max-redirects"] ~env:env_info ~docv:"N" ~doc) in
    Term.(const (fun cmdline ->
      match cmdline with
      | Some n -> { value = n; source = Cmdline }
      | None -> check_env_int ~app_name ~suffix:"MAX_REDIRECTS" ~default:10
    ) $ cmdline_arg)

  let user_agent_term app_name =
    let doc = "User-Agent header to send with requests" in
    let env_name = String.uppercase_ascii app_name ^ "_USER_AGENT" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & opt (some string) None & info ["user-agent"] ~env:env_info ~docv:"STRING" ~doc) in
    Term.(const (fun cmdline ->
      match cmdline with
      | Some ua -> { value = Some ua; source = Cmdline }
      | None ->
          match check_env_string ~app_name ~suffix:"USER_AGENT" with
          | Some { value; source } -> { value = Some value; source }
          | None -> { value = None; source = Default }
    ) $ cmdline_arg)

  let verbose_http_term app_name =
    let doc = "Enable verbose HTTP-level logging (hexdumps, TLS details)" in
    let env_name = String.uppercase_ascii app_name ^ "_VERBOSE_HTTP" in
    let env_info = Cmdliner.Cmd.Env.info env_name in
    let cmdline_arg = Arg.(value & flag & info ["verbose-http"] ~env:env_info ~doc) in
    Term.(const (fun cmdline ->
      if cmdline then
        { value = true; source = Cmdline }
      else
        check_env_bool ~app_name ~suffix:"VERBOSE_HTTP" ~default:false
    ) $ cmdline_arg)

  let proxy_term _app_name =
    let doc = "HTTP/HTTPS proxy URL (e.g., http://proxy:8080)" in
    let cmdline_arg = Arg.(value & opt (some string) None & info ["proxy"] ~docv:"URL" ~doc) in
    let no_proxy_doc = "Comma-separated list of hosts to bypass proxy" in
    let no_proxy_arg = Arg.(value & opt (some string) None & info ["no-proxy"] ~docv:"HOSTS" ~doc:no_proxy_doc) in
    Term.(const (fun cmdline_proxy cmdline_no_proxy ->
      let proxy_url = match cmdline_proxy with
        | Some url -> Some { value = url; source = Cmdline }
        | None -> (proxy_from_env ()).proxy_url
      in
      let no_proxy = match cmdline_no_proxy with
        | Some np -> Some { value = np; source = Cmdline }
        | None -> (proxy_from_env ()).no_proxy
      in
      { proxy_url; no_proxy }
    ) $ cmdline_arg $ no_proxy_arg)

  (* Combined terms *)

  let config_term app_name fs =
    let xdg_term = Xdge.Cmd.term app_name fs
      ~dirs:[`Config; `Data; `Cache] () in
    Term.(const (fun xdg persist verify timeout retries backoff follow max_redir ua verbose proxy ->
      { xdg; persist_cookies = persist; verify_tls = verify;
        timeout; max_retries = retries; retry_backoff = backoff;
        follow_redirects = follow; max_redirects = max_redir;
        user_agent = ua; verbose_http = verbose; proxy })
      $ xdg_term
      $ persist_cookies_term app_name
      $ verify_tls_term app_name
      $ timeout_term app_name
      $ retries_term app_name
      $ retry_backoff_term app_name
      $ follow_redirects_term app_name
      $ max_redirects_term app_name
      $ user_agent_term app_name
      $ verbose_http_term app_name
      $ proxy_term app_name)

  let requests_term app_name eio_env sw =
    let config_t = config_term app_name eio_env#fs in
    Term.(const (fun config -> create config eio_env sw) $ config_t)

  let minimal_term app_name fs =
    let xdg_term = Xdge.Cmd.term app_name fs
      ~dirs:[`Data; `Cache] () in
    Term.(const (fun (xdg, _xdg_cmd) persist -> (xdg, persist.value))
      $ xdg_term
      $ persist_cookies_term app_name)

  let env_docs app_name =
    let app_upper = String.uppercase_ascii app_name in
    Printf.sprintf
      "## ENVIRONMENT\n\n\
       The following environment variables affect %s:\n\n\
       ### XDG Directories\n\n\
       **%s_CONFIG_DIR**\n\
       :   Override configuration directory location\n\n\
       **%s_DATA_DIR**\n\
       :   Override data directory location (for cookies)\n\n\
       **%s_CACHE_DIR**\n\
       :   Override cache directory location\n\n\
       **XDG_CONFIG_HOME**\n\
       :   Base directory for user configuration files (default: ~/.config)\n\n\
       **XDG_DATA_HOME**\n\
       :   Base directory for user data files (default: ~/.local/share)\n\n\
       **XDG_CACHE_HOME**\n\
       :   Base directory for user cache files (default: ~/.cache)\n\n\
       ### HTTP Settings\n\n\
       **%s_PERSIST_COOKIES**\n\
       :   Set to '1' to persist cookies by default\n\n\
       **%s_NO_VERIFY_TLS**\n\
       :   Set to '1' to disable TLS verification (insecure)\n\n\
       **%s_TIMEOUT**\n\
       :   Default request timeout in seconds\n\n\
       **%s_MAX_RETRIES**\n\
       :   Maximum number of retries (default: 3)\n\n\
       **%s_RETRY_BACKOFF**\n\
       :   Retry backoff factor (default: 0.3)\n\n\
       **%s_NO_FOLLOW_REDIRECTS**\n\
       :   Set to '1' to disable redirect following\n\n\
       **%s_MAX_REDIRECTS**\n\
       :   Maximum redirects to follow (default: 10)\n\n\
       **%s_USER_AGENT**\n\
       :   User-Agent header to send with requests\n\n\
       **%s_VERBOSE_HTTP**\n\
       :   Set to '1' to enable verbose HTTP-level logging\n\n\
       ### Proxy Configuration\n\n\
       **HTTP_PROXY** / **http_proxy**\n\
       :   HTTP proxy URL (e.g., http://proxy:8080 or http://user:pass@proxy:8080)\n\n\
       **HTTPS_PROXY** / **https_proxy**\n\
       :   HTTPS proxy URL (used for HTTPS requests)\n\n\
       **ALL_PROXY** / **all_proxy**\n\
       :   Fallback proxy URL for all protocols\n\n\
       **NO_PROXY** / **no_proxy**\n\
       :   Comma-separated list of hosts to bypass proxy (e.g., localhost,*.example.com)\
      "
      app_name app_upper app_upper app_upper
      app_upper app_upper app_upper app_upper
      app_upper app_upper app_upper app_upper app_upper

  (** Pretty-print source type *)
  let pp_source ppf = function
    | Default -> Format.fprintf ppf "default"
    | Env var -> Format.fprintf ppf "env(%s)" var
    | Cmdline -> Format.fprintf ppf "cmdline"

  (** Pretty-print a value with its source *)
  let pp_with_source pp_val ppf ws =
    Format.fprintf ppf "%a [%a]" pp_val ws.value pp_source ws.source

  let pp_config ?(show_sources = true) ppf config =
    let _xdg, xdg_cmd = config.xdg in
    let pp_bool = Format.pp_print_bool in
    let pp_float = Format.pp_print_float in
    let pp_int = Format.pp_print_int in
    let pp_string_opt = Format.pp_print_option Format.pp_print_string in
    let pp_float_opt = Format.pp_print_option Format.pp_print_float in

    let pp_val pp = if show_sources then pp_with_source pp else fun ppf ws -> pp ppf ws.value in

    Format.fprintf ppf "@[<v>Configuration:@,\
      @[<v 2>XDG:@,%a@]@,\
      persist_cookies: %a@,\
      verify_tls: %a@,\
      timeout: %a@,\
      max_retries: %a@,\
      retry_backoff: %a@,\
      follow_redirects: %a@,\
      max_redirects: %a@,\
      user_agent: %a@,\
      verbose_http: %a@,\
      @[<v 2>Proxy:@,\
        url: %a@,\
        no_proxy: %a@]@]"
      Xdge.Cmd.pp xdg_cmd
      (pp_val pp_bool) config.persist_cookies
      (pp_val pp_bool) config.verify_tls
      (pp_val pp_float_opt) config.timeout
      (pp_val pp_int) config.max_retries
      (pp_val pp_float) config.retry_backoff
      (pp_val pp_bool) config.follow_redirects
      (pp_val pp_int) config.max_redirects
      (pp_val pp_string_opt) config.user_agent
      (pp_val pp_bool) config.verbose_http
      (Format.pp_print_option (pp_with_source Format.pp_print_string))
        config.proxy.proxy_url
      (Format.pp_print_option (pp_with_source Format.pp_print_string))
        config.proxy.no_proxy

  (* Logging configuration *)
  let setup_log_sources ?(verbose_http = false) level =
    (* Helper to set TLS tracing level by finding the source by name *)
    let set_tls_tracing_level lvl =
      match List.find_opt (fun s -> Logs.Src.name s = "tls.tracing") (Logs.Src.list ()) with
      | Some tls_src -> Logs.Src.set_level tls_src (Some lvl)
      | None -> () (* TLS not loaded yet, ignore *)
    in
    match level with
    | Some Logs.Debug ->
        (* Enable debug logging for application-level requests modules *)
        Logs.Src.set_level src (Some Logs.Debug);
        Logs.Src.set_level Auth.src (Some Logs.Debug);
        Logs.Src.set_level Body.src (Some Logs.Debug);
        Logs.Src.set_level Response.src (Some Logs.Debug);
        Logs.Src.set_level Retry.src (Some Logs.Debug);
        Logs.Src.set_level Headers.src (Some Logs.Debug);
        Logs.Src.set_level Error.src (Some Logs.Debug);
        Logs.Src.set_level Method.src (Some Logs.Debug);
        Logs.Src.set_level Mime.src (Some Logs.Debug);
        Logs.Src.set_level Status.src (Some Logs.Debug);
        Logs.Src.set_level Timeout.src (Some Logs.Debug);
        (* Only enable HTTP-level debug if verbose_http is set *)
        if verbose_http then begin
          Logs.Src.set_level One.src (Some Logs.Debug);
          Logs.Src.set_level Http_client.src (Some Logs.Debug);
          Logs.Src.set_level Conpool.src (Some Logs.Debug);
          set_tls_tracing_level Logs.Debug
        end else begin
          Logs.Src.set_level One.src (Some Logs.Info);
          Logs.Src.set_level Http_client.src (Some Logs.Info);
          Logs.Src.set_level Conpool.src (Some Logs.Info);
          set_tls_tracing_level Logs.Warning
        end
    | Some Logs.Info ->
        (* Set info level for main modules *)
        Logs.Src.set_level src (Some Logs.Info);
        Logs.Src.set_level Response.src (Some Logs.Info);
        Logs.Src.set_level One.src (Some Logs.Info);
        set_tls_tracing_level Logs.Warning
    | _ ->
        (* Suppress TLS debug output by default *)
        set_tls_tracing_level Logs.Warning
end

(** {1 Supporting Types} *)

module Huri = Huri

