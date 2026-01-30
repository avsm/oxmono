(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.one" ~doc:"One-shot HTTP Requests"
module Log = (val Logs.src_log src : Logs.LOG)

(* Redirect handling - delegated to shared Redirect module *)

(* Helper to create TCP connection to host:port *)
let connect_tcp ~sw ~net ~host ~port =
  Log.debug (fun m -> m "Connecting to %s:%d" host port);
  (* Resolve hostname to IP address *)
  let addrs = Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port) in
  match addrs with
  | addr :: _ ->
      Log.debug (fun m -> m "Resolved %s, connecting..." host);
      Eio.Net.connect ~sw net addr
  | [] ->
      Log.err (fun m -> m "Failed to resolve hostname: %s" host);
      raise (Error.err (Error.Dns_resolution_failed { hostname = host }))

(** Minimum TLS version configuration - re-exported from Tls_config. *)
type tls_version = Tls_config.tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)

(** Negotiated protocol after TLS handshake *)
type negotiated_protocol =
  | Http1  (** HTTP/1.x (including plain HTTP) *)
  | Http2  (** HTTP/2 negotiated via ALPN *)

(* Track whether TLS tracing has been suppressed *)
let tls_tracing_suppressed = ref false

(* Suppress TLS tracing debug output (hexdumps) unless explicitly enabled *)
let suppress_tls_tracing () =
  if not !tls_tracing_suppressed then begin
    tls_tracing_suppressed := true;
    match List.find_opt (fun s -> Logs.Src.name s = "tls.tracing") (Logs.Src.list ()) with
    | Some tls_src ->
        (* Only suppress if currently at Debug level *)
        (match Logs.Src.level tls_src with
         | Some Logs.Debug -> Logs.Src.set_level tls_src (Some Logs.Warning)
         | _ -> ())
    | None -> ()
  end

(* Helper to wrap connection with TLS if needed.
   Returns the TLS flow and the negotiated protocol. *)
let wrap_tls flow ~host ~verify_tls ~tls_config ~min_tls_version =
  Log.debug (fun m -> m "Wrapping connection with TLS for %s (verify=%b)" host verify_tls);

  (* Get or create TLS config with minimum version enforcement *)
  let tls_cfg = match tls_config with
    | Some cfg -> cfg
    | None -> Tls_config.create_client ~verify_tls ~min_tls_version ~host ()
  in

  (* Get domain name for SNI *)
  let domain = match Domain_name.of_string host with
    | Ok dn -> (match Domain_name.host dn with
        | Ok d -> d
        | Error (`Msg msg) ->
            Log.err (fun m -> m "Invalid hostname for TLS: %s (%s)" host msg);
            raise (Error.err (Error.Tls_handshake_failed {
              host;
              reason = "Invalid hostname: " ^ msg
            })))
    | Error (`Msg msg) ->
        Log.err (fun m -> m "Invalid hostname for TLS: %s (%s)" host msg);
        raise (Error.err (Error.Tls_handshake_failed {
          host;
          reason = "Invalid hostname: " ^ msg
        }))
  in

  let tls_flow = Tls_eio.client_of_flow ~host:domain tls_cfg flow in
  (* Suppress TLS tracing after first connection creates the tls.tracing source *)
  suppress_tls_tracing ();

  (* Check negotiated ALPN protocol *)
  let protocol = match Tls_eio.epoch tls_flow with
    | Ok epoch ->
        (match epoch.Tls.Core.alpn_protocol with
         | Some "h2" ->
             Log.info (fun m -> m "ALPN negotiated HTTP/2 for %s" host);
             Http2
         | Some proto ->
             Log.debug (fun m -> m "ALPN negotiated %s for %s" proto host);
             Http1
         | None ->
             Log.debug (fun m -> m "No ALPN negotiated for %s, using HTTP/1.1" host);
             Http1)
    | Error () ->
        Log.debug (fun m -> m "Could not get TLS epoch for %s, using HTTP/1.1" host);
        Http1
  in

  ((tls_flow :> [`Close | `Flow | `R | `Shutdown | `W] Eio.Resource.t), protocol)

(* Parse URL and connect directly (no pooling).
   Returns the flow and the negotiated protocol. *)
let connect_to_url ~sw ~clock ~net ~url ~timeout ~verify_tls ~tls_config ~min_tls_version =
  let uri = Uri.of_string url in

  (* Extract host and port *)
  let host = match Uri.host uri with
    | Some h -> h
    | None -> raise (Error.err (Error.Invalid_url { url; reason = "URL must contain a host" }))
  in

  let is_https = Uri.scheme uri = Some "https" in
  let default_port = if is_https then 443 else 80 in
  let port = Option.value (Uri.port uri) ~default:default_port in

  (* Apply connection timeout if specified *)
  let connect_fn () =
    let tcp_flow = connect_tcp ~sw ~net ~host ~port in
    if is_https then
      wrap_tls tcp_flow ~host ~verify_tls ~tls_config ~min_tls_version
    else
      ((tcp_flow :> [`Close | `Flow | `R | `Shutdown | `W] Eio.Resource.t), Http1)
  in

  match Option.bind timeout Timeout.total with
  | Some seconds ->
      Log.debug (fun m -> m "Setting connection timeout: %.2f seconds" seconds);
      Eio.Time.with_timeout_exn clock seconds connect_fn
  | None -> connect_fn ()

(* Main request implementation - completely stateless *)
let request ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?(follow_redirects = true) ?(max_redirects = 10)
    ?(verify_tls = true) ?tls_config ?(auto_decompress = true)
    ?(min_tls_version = TLS_1_2)
    ?(expect_100_continue = `Threshold Expect_continue.default_threshold)
    ?(allow_insecure_auth = false)
    ?proxy
    ~method_ url =

  let start_time = Unix.gettimeofday () in
  let method_str = Method.to_string method_ in
  Log.debug (fun m -> m "[One] Executing %s request to %s" method_str url);

  (* Prepare headers *)
  let headers = Option.value headers ~default:Headers.empty in

  (* Add default User-Agent if not already set - per RFC 9110 Section 10.1.5 *)
  let headers =
    if not (Headers.mem `User_agent headers) then
      Headers.set `User_agent Version.user_agent headers
    else
      headers
  in

  (* Apply auth with secure transport validation per RFC 7617/6750 *)
  let headers = Option.fold ~none:headers auth ~some:(fun a ->
    Log.debug (fun m -> m "Applying authentication");
    Auth.apply_secure ~allow_insecure_auth ~url a headers)
  in

  (* Add content type from body *)
  let headers = Option.bind body Body.content_type
    |> Option.fold ~none:headers ~some:(fun mime -> Headers.content_type mime headers)
  in

  (* Add Accept-Encoding header for auto-decompression if not already set *)
  let headers =
    if auto_decompress && not (Headers.mem `Accept_encoding headers) then
      Headers.set `Accept_encoding "gzip, deflate" headers
    else
      headers
  in

  (* Get request body, defaulting to empty *)
  let request_body = Option.value ~default:Body.empty body in

  (* Track the original URL for cross-origin redirect detection *)
  let original_uri = Uri.of_string url in

  (* Execute request with redirects
     headers_for_request: the headers to use for this specific request (may have auth stripped) *)
  let rec make_with_redirects ~headers_for_request url_to_fetch redirects_left =
    let uri_to_fetch = Uri.of_string url_to_fetch in

    (* Determine if we should use proxy for this URL *)
    let use_proxy = match proxy with
      | None -> false
      | Some p -> not (Proxy.should_bypass p url_to_fetch)
    in

    let is_https = Uri.scheme uri_to_fetch = Some "https" in

    (* Build expect_100 config from polymorphic variant *)
    let expect_100_timeout = Option.bind timeout Timeout.expect_100_continue |> Option.value ~default:1.0 in
    let expect_100_config = Expect_continue.of_config ~timeout:expect_100_timeout expect_100_continue in

    (* Connect and make request based on proxy configuration *)
    let status, resp_headers, response_body_str =
      match use_proxy, is_https, proxy with
      | false, _, _ ->
          (* Direct connection *)
          let (flow, protocol) = connect_to_url ~sw ~clock ~net ~url:url_to_fetch
                       ~timeout ~verify_tls ~tls_config ~min_tls_version in
          (match protocol with
           | Http2 ->
               (* Use HTTP/2 client via H2_adapter (handles decompression) *)
               Log.debug (fun m -> m "[One] Using HTTP/2 for %s" url_to_fetch);
               (match H2_adapter.one_request
                  ~sw
                  ~flow
                  ~uri:uri_to_fetch
                  ~headers:headers_for_request
                  ~body:request_body
                  ~method_
                  ~auto_decompress
                  ()
               with
                | Ok resp -> (resp.H2_adapter.status, resp.H2_adapter.headers, resp.H2_adapter.body)
                | Error msg -> raise (Error.err (Error.Invalid_request { reason = "HTTP/2 error: " ^ msg })))
           | Http1 ->
               (* Use HTTP/1.x client *)
               Http_client.make_request_100_continue_decompress
                 ~expect_100:expect_100_config
                 ~clock
                 ~sw
                 ~method_ ~uri:uri_to_fetch
                 ~headers:headers_for_request ~body:request_body
                 ~auto_decompress flow)

      | true, false, Some p ->
          (* HTTP via proxy - use absolute-URI form *)
          Log.debug (fun m -> m "[One] Routing HTTP request via proxy %s:%d"
            p.Proxy.host p.Proxy.port);
          let flow = connect_tcp ~sw ~net ~host:p.Proxy.host ~port:p.Proxy.port in
          let flow = (flow :> [`Close | `Flow | `R | `Shutdown | `W] Eio.Resource.t) in
          (* Convert Auth.t to header value string *)
          let proxy_auth = match p.Proxy.auth with
            | Some auth ->
                let auth_headers = Auth.apply auth Headers.empty in
                Headers.get `Authorization auth_headers
            | None -> None
          in
          (* Write request using absolute-URI form *)
          Http_write.write_and_flush flow (fun w ->
            Http_write.request_via_proxy w ~sw ~method_ ~uri:uri_to_fetch
              ~headers:headers_for_request ~body:request_body
              ~proxy_auth
          );
          (* Read response *)
          let limits = Response_limits.default in
          let buf_read = Http_read.of_flow ~max_size:65536 flow in
          let _version, status, resp_headers, body_str =
            Http_read.response ~limits ~method_ buf_read in
          (* Handle decompression if enabled *)
          let body_str = match auto_decompress, Headers.get `Content_encoding resp_headers with
            | true, Some encoding ->
                Http_client.decompress_body ~limits ~content_encoding:encoding body_str
            | _ -> body_str
          in
          (status, resp_headers, body_str)

      | true, true, Some p ->
          (* HTTPS via proxy - establish CONNECT tunnel then TLS *)
          Log.debug (fun m -> m "[One] Routing HTTPS request via proxy %s:%d (CONNECT tunnel)"
            p.Proxy.host p.Proxy.port);
          let target_host = Uri.host uri_to_fetch |> Option.value ~default:"" in
          let target_port = Uri.port uri_to_fetch |> Option.value ~default:443 in
          (* Establish TLS tunnel through proxy *)
          let tunnel_flow = Proxy_tunnel.connect_with_tls
            ~sw ~net ~clock
            ~proxy:p
            ~target_host
            ~target_port
            ?tls_config
            ()
          in
          Http_client.make_request_100_continue_decompress
            ~expect_100:expect_100_config
            ~clock
            ~sw
            ~method_ ~uri:uri_to_fetch
            ~headers:headers_for_request ~body:request_body
            ~auto_decompress tunnel_flow

      | true, _, None ->
          (* Should not happen due to use_proxy check *)
          let (flow, _protocol) = connect_to_url ~sw ~clock ~net ~url:url_to_fetch
                       ~timeout ~verify_tls ~tls_config ~min_tls_version in
          (* Note: This fallback case always uses HTTP/1.x *)
          Http_client.make_request_100_continue_decompress
            ~expect_100:expect_100_config
            ~clock
            ~sw
            ~method_ ~uri:uri_to_fetch
            ~headers:headers_for_request ~body:request_body
            ~auto_decompress flow
    in

    Log.info (fun m -> m "Received response: status=%d" status);

    (* Handle redirects if enabled *)
    if follow_redirects && (status >= 300 && status < 400) then begin
      if redirects_left <= 0 then begin
        Log.err (fun m -> m "Too many redirects (%d) for %s" max_redirects url);
        raise (Error.err (Error.Too_many_redirects { url; count = max_redirects; max = max_redirects }))
      end;

      match Headers.get `Location resp_headers with
      | None ->
          Log.debug (fun m -> m "Redirect response missing Location header");
          (status, resp_headers, response_body_str, url_to_fetch)
      | Some location ->
          (* Validate redirect URL scheme - Per Recommendation #5 *)
          let _ = Redirect.validate_url location in

          Log.info (fun m -> m "Following redirect to %s (%d remaining)" location redirects_left);
          (* Strip sensitive headers on cross-origin redirects (security)
             Per Recommendation #1: Strip auth headers to prevent credential leakage *)
          let redirect_uri = Uri.of_string location in
          let headers_for_redirect =
            if Redirect.same_origin original_uri redirect_uri then
              headers_for_request
            else begin
              Log.debug (fun m -> m "Cross-origin redirect detected: stripping sensitive headers");
              Redirect.strip_sensitive_headers headers_for_request
            end
          in
          make_with_redirects ~headers_for_request:headers_for_redirect location (redirects_left - 1)
    end else
      (status, resp_headers, response_body_str, url_to_fetch)
  in

  let final_status, final_headers, final_body_str, final_url =
    make_with_redirects ~headers_for_request:headers url max_redirects
  in

  let elapsed = Unix.gettimeofday () -. start_time in
  Log.info (fun m -> m "Request completed in %.3f seconds" elapsed);

  (* Create a flow from the body string *)
  let body_flow = Eio.Flow.string_source final_body_str in

  Response.Private.make
    ~sw
    ~status:final_status
    ~headers:final_headers
    ~body:body_flow
    ~url:final_url
    ~elapsed

(* Convenience methods *)
let get ~sw ~clock ~net ?headers ?auth ?timeout
    ?follow_redirects ?max_redirects ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?auth ?timeout
    ?follow_redirects ?max_redirects ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy
    ~expect_100_continue:`Disabled  (* GET has no body *)
    ~method_:`GET url

let post ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy ~method_:`POST url

let put ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy ~method_:`PUT url

let delete ~sw ~clock ~net ?headers ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy
    ~expect_100_continue:`Disabled  (* DELETE typically has no body *)
    ~method_:`DELETE url

let head ~sw ~clock ~net ?headers ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy
    ~expect_100_continue:`Disabled  (* HEAD has no body *)
    ~method_:`HEAD url

let patch ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy url =
  request ~sw ~clock ~net ?headers ?body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy ~method_:`PATCH url

let upload ~sw ~clock ~net ?headers ?auth ?timeout ?method_ ?mime ?length
    ?on_progress ?verify_tls ?tls_config ?min_tls_version
    ?expect_100_continue
    ?allow_insecure_auth ?proxy ~source url =
  let method_ = Option.value method_ ~default:`POST in
  let mime = Option.value mime ~default:Mime.octet_stream in

  (* Wrap source with progress tracking if callback provided *)
  let tracked_source = match on_progress with
    | None -> source
    | Some callback ->
        (* For now, progress tracking is not implemented for uploads
           due to complexity of wrapping Eio.Flow.source.
           This would require creating a custom flow wrapper. *)
        let _ = callback in
        source
  in

  let body = Body.of_stream ?length mime tracked_source in
  request ~sw ~clock ~net ?headers ~body ?auth ?timeout
    ?verify_tls ?tls_config ?min_tls_version
    ?allow_insecure_auth ?proxy
    ?expect_100_continue ~method_ url

let download ~sw ~clock ~net ?headers ?auth ?timeout ?on_progress
    ?verify_tls ?tls_config ?min_tls_version ?allow_insecure_auth ?proxy url ~sink =
  let response = get ~sw ~clock ~net ?headers ?auth ?timeout
                   ?verify_tls ?tls_config ?min_tls_version
                   ?allow_insecure_auth ?proxy url in

  try
    (* Get content length for progress tracking *)
    let total = Response.content_length response in

    let body = Response.body response in

    (* Stream data to sink with optional progress *)
    match on_progress with
    | None ->
        (* No progress tracking, just copy directly *)
        Eio.Flow.copy body sink
    | Some progress_fn ->
        (* Copy with progress tracking *)
        (* We need to intercept the flow to track bytes *)
        (* For now, just do a simple copy - proper progress tracking needs flow wrapper *)
        progress_fn ~received:0L ~total;
        Eio.Flow.copy body sink;
        progress_fn ~received:(Option.value total ~default:0L) ~total;

    (* Response auto-closes with switch *)
    ()
  with e ->
    (* Response auto-closes with switch *)
    raise e
