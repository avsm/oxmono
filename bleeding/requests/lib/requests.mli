(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Requests - A modern HTTP/1.1 client library for OCaml

    Requests is an HTTP client library for OCaml inspired by Python's requests
    and urllib3 libraries. It provides a simple, intuitive API for making HTTP
    requests while handling complexities like TLS configuration, connection
    pooling, retries, and cookie management.

    The library implements {{:https://datatracker.ietf.org/doc/html/rfc9110}RFC 9110}
    (HTTP Semantics) and {{:https://datatracker.ietf.org/doc/html/rfc9112}RFC 9112}
    (HTTP/1.1).

    {2 Choosing an API}

    The library offers two APIs optimized for different use cases:

    {3 Session API (Requests.t)}

    Use {!Requests.create} when you need:
    - {b Cookie persistence} across requests (automatic session handling)
    - {b Connection pooling} for efficient reuse of TCP/TLS connections
    - {b Shared authentication} configured once for all requests
    - {b Automatic retry handling} with exponential backoff
    - {b Base URL support} for API clients ({{:https://datatracker.ietf.org/doc/html/rfc3986#section-5}RFC 3986 Section 5})

    {[
      open Eio_main

      let () = run @@ fun env ->
      Eio.Switch.run @@ fun sw ->

      (* Create a session with connection pooling *)
      let session = Requests.create ~sw env in

      (* Configure authentication once for all requests *)
      let session = Requests.set_auth session (Requests.Auth.bearer "your-token") in

      (* Make concurrent requests - connections are reused *)
      let (user, repos) = Eio.Fiber.both
        (fun () -> Requests.get session "https://api.github.com/user")
        (fun () -> Requests.get session "https://api.github.com/user/repos") in

      (* Process responses *)
      let user_data = Requests.Response.text user in
      let repos_data = Requests.Response.text repos in
      Printf.printf "User: %s\nRepos: %s\n" user_data repos_data

      (* Resources auto-cleanup when switch closes *)
    ]}

    {3 One-Shot API (Requests.One)}

    Use {!Requests.One} for stateless, single requests when you need:
    - {b Minimal overhead} for one-off requests
    - {b Fine-grained control} over TLS and connection settings
    - {b No session state} (no cookies, no connection reuse)
    - {b Direct streaming} without middleware

    {[
      open Eio_main

      let () = run @@ fun env ->
      Eio.Switch.run @@ fun sw ->

      (* Direct stateless request - no session needed *)
      let response = Requests.One.get ~sw
        ~clock:env#clock ~net:env#net
        "https://api.github.com" in

      Printf.printf "Status: %d\n" (Requests.Response.status_code response);

      (* POST with JSON body *)
      let json_body = Jsont.Object ([
        ("name", Jsont.String "Alice")
      ], Jsont.Meta.none) in

      let response = Requests.One.post ~sw
        ~clock:env#clock ~net:env#net
        ~headers:(Requests.Headers.empty
                  |> Requests.Headers.content_type Requests.Mime.json)
        ~body:(Requests.Body.json json_body)
        "https://api.example.com/users" in

      Printf.printf "Created: %d\n" (Requests.Response.status_code response)
    ]}

    {2 Features}

    - {b Simple API}: Intuitive functions for GET, POST, PUT, DELETE, etc.
    - {b Authentication}: Built-in support for Basic, Bearer, Digest, and OAuth
    - {b Streaming}: Upload and download large files efficiently
    - {b Retries}: Automatic retry with exponential backoff (see {!Retry})
    - {b Timeouts}: Configurable connection and read timeouts (see {!Timeout})
    - {b Cookie Management}: Automatic cookie handling with persistence (see {!Cookeio} and {!Cookeio_jar})
    - {b TLS/SSL}: Secure connections with certificate verification
    - {b Connection Pooling}: Efficient TCP/TLS connection reuse (see {!Conpool})
    - {b Error Handling}: Comprehensive error types and recovery (see {!module:Error})

    {2 Related Libraries}

    This library integrates with several companion libraries:

    {ul
    {- [Conpool] - TCP/IP connection pooling with retry logic and statistics}
    {- [Cookeio] - HTTP cookie parsing and validation per RFC 6265}
    {- [Cookeio_jar] - Cookie jar storage with XDG persistence}
    {- [Xdge] - XDG Base Directory support for cookie persistence paths}}

    {2 Error Handling}

    The Requests library uses exceptions as its primary error handling mechanism,
    following Eio's structured concurrency model. This approach ensures that
    errors are propagated cleanly through the switch hierarchy.

    {b Exception-Based Errors:}

    All request functions may raise [Eio.Io] exceptions with {!Error.E} payload:
    - [Error.Timeout]: Request exceeded timeout limit
    - [Error.Dns_resolution_failed], [Error.Tcp_connect_failed]: Network connection failed
    - [Error.Too_many_redirects]: Exceeded maximum redirect count
    - [Error.Http_error]: HTTP error response received (4xx/5xx status)
    - [Error.Tls_handshake_failed]: TLS/SSL connection error
    - [Error.Authentication_failed]: Authentication failed

    {b Note on HTTP Status Codes:}

    By default, the library does {b NOT} raise exceptions for HTTP error status
    codes (4xx, 5xx). The response is returned normally and you should check
    the status code explicitly:

    {[
      let resp = Requests.get req "https://api.example.com/data" in
      if Requests.Response.ok resp then
        (* Success: 2xx status *)
        let body = Requests.Response.body resp |> Eio.Flow.read_all in
        process_success body
      else
        (* Error: non-2xx status *)
        let status = Requests.Response.status_code resp in
        handle_error status
    ]}

    To automatically retry on certain HTTP status codes, configure retry behavior:

    {[
      let retry_config = Requests.Retry.create_config
        ~max_retries:3
        ~status_forcelist:[429; 500; 502; 503; 504]  (* Retry these codes *)
        () in
      let req = Requests.create ~sw ~retry:retry_config env in
    ]}

    {b Catching Exceptions:}

    {[
      try
        let resp = Requests.get req url in
        handle_success resp
      with
      | Requests.Error.Timeout ->
          (* Handle timeout specifically *)
          retry_with_longer_timeout ()
      | Requests.Error.ConnectionError msg ->
          (* Handle connection errors *)
          log_error "Connection failed: %s" msg
      | exn ->
          (* Handle other errors *)
          log_error "Unexpected error: %s" (Printexc.to_string exn)
    ]}

    The {!module:Error} module also provides a Result-based API for functional error
    handling, though the primary API uses exceptions for better integration
    with Eio's structured concurrency.

    {2 Common Use Cases}

    {b Working with JSON APIs:}
    {[
      let response = Requests.post req "https://api.example.com/data"
        ~body:(Requests.Body.json {|{"key": "value"}|}) in
      let body_text =
        Requests.Response.body response
        |> Eio.Flow.read_all in
      print_endline body_text
      (* Response auto-closes with switch *)
    ]}

    {b File uploads:}
    {[
      let body = Requests.Body.multipart [
        { name = "file"; filename = Some "document.pdf";
          content_type = Requests.Mime.pdf;
          content = `File (Eio.Path.(fs / "document.pdf")) };
        { name = "description"; filename = None;
          content_type = Requests.Mime.text_plain;
          content = `String "Important document" }
      ] in
      let response = Requests.post req "https://example.com/upload"
        ~body
      (* Response auto-closes with switch *)
    ]}

    {b Streaming downloads:}
    {[
      Requests.One.download ~sw ~client
        "https://example.com/large-file.zip"
        ~sink:(Eio.Path.(fs / "download.zip" |> sink))
    ]}

*)

(** {1 Main API}

    The main Requests API provides stateful HTTP clients with automatic cookie
    management and persistent configuration. Requests execute synchronously by default.
    Use Eio.Fiber.both or Eio.Fiber.all for concurrent execution.
*)

type t
(** A stateful HTTP client that maintains cookies, auth, configuration, and
    connection pools across requests. The clock and network resources are
    existentially quantified and hidden behind this abstract type. *)

(** {2 TLS Configuration} *)

type tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)
(** Minimum TLS version to require for HTTPS connections.
    Per Recommendation #6: Allow enforcing minimum TLS version for security. *)

(** {2 Creation and Configuration} *)

val create :
  sw:Eio.Switch.t ->
  ?http_pool:unit Conpool.t ->
  ?https_pool:unit Conpool.t ->
  ?cookie_jar:Cookeio_jar.t ->
  ?default_headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?follow_redirects:bool ->
  ?max_redirects:int ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?max_connections_per_host:int ->
  ?connection_idle_timeout:float ->
  ?connection_lifetime:float ->
  ?retry:Retry.config ->
  ?persist_cookies:bool ->
  ?xdg:Xdge.t ->
  ?auto_decompress:bool ->
  ?expect_100_continue:Expect_continue.config ->
  ?base_url:string ->
  ?xsrf_cookie_name:string option ->
  ?xsrf_header_name:string ->
  ?proxy:Proxy.config ->
  ?allow_insecure_auth:bool ->
  < clock: _ Eio.Time.clock; net: _ Eio.Net.t; fs: Eio.Fs.dir_ty Eio.Path.t; .. > ->
  t
(** Create a new requests instance with persistent state and connection pooling.
    All resources are bound to the provided switch and will be cleaned up automatically.

    @param sw Switch for resource management
    @param http_pool Optional pre-configured HTTP {!Conpool.t} connection pool (creates new if not provided)
    @param https_pool Optional pre-configured HTTPS {!Conpool.t} connection pool (creates new if not provided)
    @param cookie_jar {!Cookeio_jar.t} cookie storage (default: empty in-memory jar)
    @param default_headers Headers included in every request
    @param auth Default authentication
    @param timeout Default timeout configuration
    @param follow_redirects Whether to follow HTTP redirects (default: true)
    @param max_redirects Maximum redirects to follow (default: 10)
    @param verify_tls Whether to verify TLS certificates (default: true)
    @param tls_config Custom TLS configuration for HTTPS pool (default: system CA certs)
    @param min_tls_version Minimum TLS version to require (default: TLS_1_2)
    @param max_connections_per_host Maximum pooled connections per host:port (default: 10)
    @param connection_idle_timeout Max idle time before closing pooled connection (default: 60s)
    @param connection_lifetime Max lifetime of any pooled connection (default: 300s)
    @param retry Retry configuration for failed requests
    @param persist_cookies Whether to persist cookies to disk via {!Cookeio_jar} (default: false)
    @param xdg {!Xdge.t} XDG directory context for cookies (required if persist_cookies=true)
    @param auto_decompress Whether to automatically decompress gzip/deflate responses (default: true)
    @param expect_100_continue HTTP 100-continue configuration (default: [`Threshold 1_048_576L]).
           Use [`Disabled] to never send Expect: 100-continue,
           [`Always] to always send it for requests with bodies, or
           [`Threshold n] to send it only for bodies >= n bytes.
    @param base_url Base URL for relative paths (per Recommendation #11). Relative URLs are resolved against this.
    @param xsrf_cookie_name Cookie name to extract XSRF token from (default: Some "XSRF-TOKEN", per Recommendation #24). Set to None to disable.
    @param xsrf_header_name Header name to inject XSRF token into (default: "X-XSRF-TOKEN")
    @param proxy HTTP/HTTPS proxy configuration. When set, requests are routed through the proxy.
           HTTP requests use absolute-URI form (RFC 9112 Section 3.2.2).
           HTTPS requests use CONNECT tunneling (RFC 9110 Section 9.3.6).
    @param allow_insecure_auth Allow Basic/Bearer/Digest authentication over plaintext HTTP (default: false).
           Per {{:https://datatracker.ietf.org/doc/html/rfc7617#section-4}RFC 7617 Section 4} and
           {{:https://datatracker.ietf.org/doc/html/rfc6750#section-5.1}RFC 6750 Section 5.1},
           these auth schemes transmit credentials that SHOULD be protected by TLS.
           {b Only set to [true] for local development or testing environments.}
           Example for local dev server:
           {[
             let session = Requests.create ~sw ~allow_insecure_auth:true env in
             let session = Requests.set_auth session (Requests.Auth.basic ~username:"dev" ~password:"dev") in
             (* Can now make requests to http://localhost:8080 with Basic auth *)
           ]}
*)

(** {2 Configuration Management} *)

val set_default_header : t -> string -> string -> t
(** Add or update a default header. Returns a new session with the updated header.
    The original session's connection pools are shared. *)

val remove_default_header : t -> string -> t
(** Remove a default header. Returns a new session without the header. *)

val set_auth : t -> Auth.t -> t
(** Set default authentication. Returns a new session with auth configured. *)

val clear_auth : t -> t
(** Clear authentication. Returns a new session without auth. *)

val set_timeout : t -> Timeout.t -> t
(** Set default timeout. Returns a new session with the timeout configured. *)

val set_retry : t -> Retry.config -> t
(** Set retry configuration. Returns a new session with retry configured. *)

(** {2 Request Methods}

    All request methods execute synchronously. To make concurrent requests,
    you must explicitly use Eio.Fiber.both or Eio.Fiber.all.
    The response will auto-close when the parent switch closes.

    Example of concurrent requests using Fiber.both:
    {[
      let req = Requests.create ~sw env in

      (* Use Fiber.both for two concurrent requests *)
      let (r1, r2) = Eio.Fiber.both
        (fun () -> Requests.get req "https://api1.example.com")
        (fun () -> Requests.post req "https://api2.example.com" ~body)
      in

      (* Process responses *)
      let body1 = Response.body r1 |> Eio.Flow.read_all in
      let body2 = Response.body r2 |> Eio.Flow.read_all in
    ]}

    Example using Fiber.all for multiple requests:
    {[
      let req = Requests.create ~sw env in

      (* Use Fiber.all for multiple concurrent requests *)
      let urls = [
        "https://api1.example.com";
        "https://api2.example.com";
        "https://api3.example.com";
      ] in

      let responses = ref [] in
      Eio.Fiber.all [
        (fun () -> responses := Requests.get req (List.nth urls 0) :: !responses);
        (fun () -> responses := Requests.get req (List.nth urls 1) :: !responses);
        (fun () -> responses := Requests.get req (List.nth urls 2) :: !responses);
      ];

      (* Process all responses *)
      List.iter (fun r ->
        let body = Response.body r |> Eio.Flow.read_all in
        print_endline body
      ) !responses
    ]}

    Example using Promise for concurrent requests with individual control:
    {[
      let req = Requests.create ~sw env in

      (* Start requests in parallel using promises *)
      let p1, r1 = Eio.Promise.create () in
      let p2, r2 = Eio.Promise.create () in
      let p3, r3 = Eio.Promise.create () in

      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve r1 (Requests.get req "https://api1.example.com")
      );
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve r2 (Requests.post req "https://api2.example.com" ~body)
      );
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Promise.resolve r3 (Requests.get req "https://api3.example.com")
      );

      (* Wait for all promises and process *)
      let resp1 = Eio.Promise.await p1 in
      let resp2 = Eio.Promise.await p2 in
      let resp3 = Eio.Promise.await p3 in

      (* Process responses *)
      let body1 = Response.body resp1 |> Eio.Flow.read_all in
      let body2 = Response.body resp2 |> Eio.Flow.read_all in
      let body3 = Response.body resp3 |> Eio.Flow.read_all in
    ]}
*)

val request :
  t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?follow_redirects:bool ->
  ?max_redirects:int ->
  ?path_params:(string * string) list ->
  method_:Method.t ->
  string ->
  Response.t
(** Make a concurrent HTTP request.
    @param path_params List of (key, value) pairs for URL template substitution (per Recommendation #29).
    Example: [request ~path_params:[("id", "123")] ~method_:`GET "/users/{id}"] *)

val get :
  t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?params:(string * string) list ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent GET request.
    @param params Query parameters to append to URL
    @param path_params Path template substitutions (e.g., ["/users/{id}"] with [("id", "123")]) *)

val post :
  t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent POST request *)

val put :
  t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent PUT request *)

val patch :
  t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent PATCH request *)

val delete :
  t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent DELETE request *)

val head :
  t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent HEAD request *)

val options :
  t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?path_params:(string * string) list ->
  string ->
  Response.t
(** Concurrent OPTIONS request *)

(** {2 Cookie Management} *)

val cookies : t -> Cookeio_jar.t
(** Get the cookie jar for direct manipulation *)

val clear_cookies : t -> unit
(** Clear all cookies *)

(** {2 Proxy Configuration} *)

val set_proxy : t -> Proxy.config -> t
(** Set HTTP/HTTPS proxy configuration. Returns a new session with proxy configured.
    When set, requests are routed through the proxy:
    - HTTP requests use absolute-URI form (RFC 9112 Section 3.2.2)
    - HTTPS requests use CONNECT tunneling (RFC 9110 Section 9.3.6)

    Example:
    {[
      let proxy = Proxy.http ~port:8080 "proxy.example.com" in
      let session = Requests.set_proxy session proxy
    ]} *)

val clear_proxy : t -> t
(** Remove proxy configuration. Returns a new session without proxy. *)

val proxy : t -> Proxy.config option
(** Get the current proxy configuration, if any. *)

(** {1 Cmdliner Integration} *)

module Cmd : sig
  (** Cmdliner integration for Requests configuration.

      This module provides command-line argument handling for configuring
      HTTP requests, including XDG directory paths, timeouts, retries,
      proxy settings, and other parameters.

      {2 Source Tracking}

      Configuration values include source tracking to indicate where
      each value came from (command line, environment variable, or default).
      This enables transparent debugging and helps users understand
      how their configuration was resolved.

      {[
        let config = ... in
        if show_sources then
          Format.printf "%a@." (Cmd.pp_config ~show_sources:true) config
      ]} *)

  (** {2 Source Tracking Types} *)

  (** Source of a configuration value.
      Tracks where each configuration value originated from for debugging
      and transparency. *)
  type source =
    | Default                (** Value from hardcoded default *)
    | Env of string          (** Value from environment variable (stores var name) *)
    | Cmdline                (** Value from command-line argument *)

  (** Wrapper for values with source tracking *)
  type 'a with_source = {
    value : 'a;              (** The actual configuration value *)
    source : source;         (** Where the value came from *)
  }

  (** Proxy configuration from command line and environment *)
  type proxy_config = {
    proxy_url : string with_source option;  (** Proxy URL (from HTTP_PROXY/HTTPS_PROXY/etc) *)
    no_proxy : string with_source option;   (** NO_PROXY patterns *)
  }

  (** {2 Configuration Type} *)

  (** Configuration from command line and environment.
      All values include source tracking for debugging. *)
  type config = {
    xdg : Xdge.t * Xdge.Cmd.t;       (** XDG paths and their sources *)
    persist_cookies : bool with_source;  (** Whether to persist cookies *)
    verify_tls : bool with_source;       (** Whether to verify TLS certificates *)
    timeout : float option with_source;  (** Request timeout in seconds *)
    max_retries : int with_source;       (** Maximum number of retries *)
    retry_backoff : float with_source;   (** Retry backoff factor *)
    follow_redirects : bool with_source; (** Whether to follow redirects *)
    max_redirects : int with_source;     (** Maximum number of redirects *)
    user_agent : string option with_source;  (** User-Agent header *)
    verbose_http : bool with_source;     (** Enable verbose HTTP-level logging *)
    proxy : proxy_config;                (** Proxy configuration *)
  }

  val create : config -> < clock: _ Eio.Time.clock; net: _ Eio.Net.t; fs: Eio.Fs.dir_ty Eio.Path.t; .. > -> Eio.Switch.t -> t
  (** [create config env sw] creates a requests instance from command-line configuration.
      Proxy configuration from the config is applied automatically. *)

  (** {2 Individual Terms}

      Each term returns a value with source tracking to indicate whether
      the value came from the command line, environment, or default.
      Source precedence: Cmdline > Env > Default *)

  val persist_cookies_term : string -> bool with_source Cmdliner.Term.t
  (** Term for [--persist-cookies] flag with app-specific env var.
      Env var: [{APP_NAME}_PERSIST_COOKIES] *)

  val verify_tls_term : string -> bool with_source Cmdliner.Term.t
  (** Term for [--no-verify-tls] flag with app-specific env var.
      Env var: [{APP_NAME}_NO_VERIFY_TLS] *)

  val timeout_term : string -> float option with_source Cmdliner.Term.t
  (** Term for [--timeout SECONDS] option with app-specific env var.
      Env var: [{APP_NAME}_TIMEOUT] *)

  val retries_term : string -> int with_source Cmdliner.Term.t
  (** Term for [--max-retries N] option with app-specific env var.
      Env var: [{APP_NAME}_MAX_RETRIES] *)

  val retry_backoff_term : string -> float with_source Cmdliner.Term.t
  (** Term for [--retry-backoff FACTOR] option with app-specific env var.
      Env var: [{APP_NAME}_RETRY_BACKOFF] *)

  val follow_redirects_term : string -> bool with_source Cmdliner.Term.t
  (** Term for [--no-follow-redirects] flag with app-specific env var.
      Env var: [{APP_NAME}_NO_FOLLOW_REDIRECTS] *)

  val max_redirects_term : string -> int with_source Cmdliner.Term.t
  (** Term for [--max-redirects N] option with app-specific env var.
      Env var: [{APP_NAME}_MAX_REDIRECTS] *)

  val user_agent_term : string -> string option with_source Cmdliner.Term.t
  (** Term for [--user-agent STRING] option with app-specific env var.
      Env var: [{APP_NAME}_USER_AGENT] *)

  val verbose_http_term : string -> bool with_source Cmdliner.Term.t
  (** Term for [--verbose-http] flag with app-specific env var.

      Enables verbose HTTP-level logging including hexdumps, TLS details,
      and low-level protocol information. Typically used in conjunction
      with debug-level logging.
      Env var: [{APP_NAME}_VERBOSE_HTTP] *)

  val proxy_term : string -> proxy_config Cmdliner.Term.t
  (** Term for [--proxy URL] and [--no-proxy HOSTS] options.

      Provides cmdliner integration for proxy configuration with proper
      source tracking. Environment variables are checked in order:
      HTTP_PROXY, http_proxy, HTTPS_PROXY, https_proxy, ALL_PROXY, all_proxy.

      {b Generated Flags:}
      - [--proxy URL]: HTTP/HTTPS proxy URL (e.g., http://proxy:8080)
      - [--no-proxy HOSTS]: Comma-separated list of hosts to bypass proxy

      {b Environment Variables:}
      - [HTTP_PROXY] / [http_proxy]: HTTP proxy URL
      - [HTTPS_PROXY] / [https_proxy]: HTTPS proxy URL
      - [ALL_PROXY] / [all_proxy]: Fallback proxy URL for all protocols
      - [NO_PROXY] / [no_proxy]: Hosts to bypass proxy *)

  (** {2 Combined Terms} *)

  val config_term : string -> Eio.Fs.dir_ty Eio.Path.t -> config Cmdliner.Term.t
  (** [config_term app_name fs] creates a complete configuration term.

      This combines all individual terms plus XDG configuration into
      a single term that can be used to configure requests. All values
      include source tracking.

      {b Generated Flags:}
      - [--config-dir DIR]: Configuration directory
      - [--data-dir DIR]: Data directory
      - [--cache-dir DIR]: Cache directory
      - [--persist-cookies]: Enable cookie persistence
      - [--no-verify-tls]: Disable TLS verification
      - [--timeout SECONDS]: Request timeout
      - [--max-retries N]: Maximum retries
      - [--retry-backoff FACTOR]: Retry backoff multiplier
      - [--no-follow-redirects]: Disable redirect following
      - [--max-redirects N]: Maximum redirects to follow
      - [--user-agent STRING]: User-Agent header
      - [--verbose-http]: Enable verbose HTTP-level logging
      - [--proxy URL]: HTTP/HTTPS proxy URL
      - [--no-proxy HOSTS]: Hosts to bypass proxy

      {b Example:}
      {[
        let open Cmdliner in
        let config_t = Requests.Cmd.config_term "myapp" env#fs in
        let main config =
          Eio.Switch.run @@ fun sw ->
          let req = Requests.Cmd.create config env sw in
          (* Use requests *)
        in
        let cmd = Cmd.v info Term.(const main $ config_t) in
        Cmd.eval cmd
      ]} *)

  val requests_term : string -> < clock: _ Eio.Time.clock; net: _ Eio.Net.t; fs: Eio.Fs.dir_ty Eio.Path.t; .. > -> Eio.Switch.t -> t Cmdliner.Term.t
  (** [requests_term app_name env sw] creates a term that directly produces a requests instance.

      This is a convenience function that combines configuration parsing
      with requests creation.

      {b Example:}
      {[
        let open Cmdliner in
        let main req =
          (* Use requests directly *)
          let resp = Requests.get req "https://example.com" in
          (* ... *)
        in
        Eio.Switch.run @@ fun sw ->
        let req_t = Requests.Cmd.requests_term "myapp" env sw in
        let cmd = Cmd.v info Term.(const main $ req_t) in
        Cmd.eval cmd
      ]} *)

  val minimal_term : string -> Eio.Fs.dir_ty Eio.Path.t -> (Xdge.t * bool) Cmdliner.Term.t
  (** [minimal_term app_name fs] creates a minimal configuration term.

      This only provides:
      - [--cache-dir DIR]: Cache directory for responses
      - [--persist-cookies]: Cookie persistence flag

      Returns the XDG context and persist_cookies boolean (without source tracking
      for simplified usage).

      {b Example:}
      {[
        let open Cmdliner in
        let minimal_t = Requests.Cmd.minimal_term "myapp" env#fs in
        let main (xdg, persist) =
          Eio.Switch.run @@ fun sw ->
          let req = Requests.create ~sw ~xdg ~persist_cookies:persist env in
          (* Use requests *)
        in
        let cmd = Cmd.v info Term.(const main $ minimal_t) in
        Cmd.eval cmd
      ]} *)

  (** {2 Documentation and Pretty-Printing} *)

  val env_docs : string -> string
  (** [env_docs app_name] generates environment variable documentation.

      Returns formatted documentation for all environment variables that
      affect requests configuration, including XDG variables and proxy settings.

      {b Included Variables:}
      - [${APP_NAME}_CONFIG_DIR]: Configuration directory
      - [${APP_NAME}_DATA_DIR]: Data directory
      - [${APP_NAME}_CACHE_DIR]: Cache directory
      - [${APP_NAME}_STATE_DIR]: State directory
      - [XDG_CONFIG_HOME], [XDG_DATA_HOME], [XDG_CACHE_HOME], [XDG_STATE_HOME]
      - [HTTP_PROXY], [HTTPS_PROXY], [ALL_PROXY]: Proxy URLs
      - [NO_PROXY]: Hosts to bypass proxy

      {b Example:}
      {[
        let env_info = Cmdliner.Cmd.Env.info
          ~docs:Cmdliner.Manpage.s_environment
          ~doc:(Requests.Cmd.env_docs "myapp")
          ()
      ]} *)

  val pp_source : Format.formatter -> source -> unit
  (** Pretty print a source type.
      Output format: "default", "env(VAR_NAME)", or "cmdline" *)

  val pp_with_source : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a with_source -> unit
  (** [pp_with_source pp_val ppf ws] pretty prints a value with its source.
      Output format: "value [source]"

      {b Example:}
      {[
        let pp_bool_with_source = Cmd.pp_with_source Format.pp_print_bool in
        Format.printf "%a@." pp_bool_with_source config.verify_tls
        (* Output: true [env(MYAPP_NO_VERIFY_TLS)] *)
      ]} *)

  val pp_config : ?show_sources:bool -> Format.formatter -> config -> unit
  (** [pp_config ?show_sources ppf config] pretty prints configuration for debugging.

      @param show_sources If true (default), shows the source of each value
             (e.g., "default", "env(VAR_NAME)", "cmdline"). If false, only
             shows the values without source annotations.

      {b Example:}
      {[
        (* Show full config with sources *)
        Format.printf "%a@." (Cmd.pp_config ~show_sources:true) config;

        (* Show config without sources for cleaner output *)
        Format.printf "%a@." (Cmd.pp_config ~show_sources:false) config;
      ]} *)

  (** {2 Logging Configuration} *)

  val setup_log_sources : ?verbose_http:bool -> Logs.level option -> unit
  (** [setup_log_sources ~verbose_http level] configures Requests library log sources.

      This helper function configures all Requests logging sources based on
      the specified log level and verbose_http flag. It's designed to work
      with Logs_cli and provides fine-grained control over HTTP-level logging.

      {b Log Level Behavior:}
      - [Some Debug]: Enables debug logging for all application-level modules
        (Auth, Body, Response, Retry, Headers, Error, Method, Mime, Status, Timeout).
        If [verbose_http] is true, also enables debug logging for protocol-level
        modules (One, Http_client, Conpool, and TLS tracing). If [verbose_http]
        is false, TLS tracing is set to Warning level to suppress hexdumps.
      - [Some Info]: Enables info logging for main modules (src, Response, One).
        TLS tracing is set to Warning level.
      - [None] or other levels: TLS tracing is set to Warning level to suppress
        verbose protocol output.

      {b Example with Logs_cli:}
      {[
        let setup_logging =
          let open Cmdliner.Term in
          const (fun style level verbose_http ->
            Fmt_tty.setup_std_outputs ?style_renderer:style ();
            Logs.set_level level;
            Logs.set_reporter (Logs_fmt.reporter ());
            Requests.Cmd.setup_log_sources ~verbose_http level)
          $ Fmt_cli.style_renderer ()
          $ Logs_cli.level ()
          $ Requests.Cmd.verbose_http_term "myapp"
      ]} *)
end

(** Retry policies and backoff strategies *)
module Retry = Retry

(** {1 One-Shot API}

    The {!One} module provides direct control over HTTP requests without
    session state. Each request opens a new TCP connection that is closed
    when the switch closes.

    Use {!One} for:
    - Single, stateless requests without session overhead
    - Fine-grained control over TLS configuration per request
    - Direct streaming uploads and downloads
    - Situations where connection pooling is not needed

    See the module documentation for examples and full API.
*)

(** One-shot HTTP client for stateless requests.

    Provides {!One.get}, {!One.post}, {!One.put}, {!One.patch}, {!One.delete},
    {!One.head}, {!One.upload}, and {!One.download} functions.

    Example:
    {[
      let response = Requests.One.get ~sw
        ~clock:env#clock ~net:env#net
        "https://example.com" in
      Printf.printf "Status: %d\n" (Requests.Response.status_code response)
    ]} *)
module One = One

(** Low-level HTTP client over pooled connections *)
module Http_client = Http_client

(** {1 Core Types}

    These modules define the fundamental types used throughout the library.
*)

(** HTTP response handling *)
module Response = Response

(** Request body construction and encoding *)
module Body = Body

(** HTTP headers manipulation *)
module Headers = Headers

(** Authentication schemes (Basic, Bearer, OAuth, etc.) *)
module Auth = Auth

(** HTTP/HTTPS proxy configuration *)
module Proxy = Proxy

(** HTTPS proxy tunneling via CONNECT *)
module Proxy_tunnel = Proxy_tunnel

(** Error types and exception handling *)
module Error = Error

(** {1 Supporting Types} *)

(** Efficient URI serialization for Buf_write.

    For all URI operations (parsing, manipulation, etc.), use the external
    [uri] opam library directly. This module only provides {!Huri.write} for
    efficient serialization to [Eio.Buf_write] without intermediate strings. *)
module Huri = Huri

(** HTTP status codes and reason phrases *)
module Status = Status

(** HTTP request methods (GET, POST, etc.) *)
module Method = Method

(** HTTP protocol version types and ALPN support *)
module Http_version = Http_version

(** MIME types for content negotiation *)
module Mime = Mime

(** Timeout configuration for requests *)
module Timeout = Timeout

(** HTTP Cache-Control header parsing (RFC 9111) *)
module Cache_control = Cache_control

(** HTTP response size limits for DoS prevention *)
module Response_limits = Response_limits

(** HTTP 100-Continue configuration for large uploads *)
module Expect_continue = Expect_continue

(** HTTP Link header parsing (RFC 8288) for pagination and API discovery *)
module Link = Link

(** HTTP request timing metrics for performance analysis *)
module Timing = Timing

(** HTTP header name types and utilities *)
module Header_name = Header_name

(** HTTP header value parsing for complex headers (RFC 9110)
    @see <https://www.rfc-editor.org/rfc/rfc9110> *)
module Header_parsing = Header_parsing

(** WebSocket handshake support (RFC 6455)
    @see <https://www.rfc-editor.org/rfc/rfc6455> *)
module Websocket = Websocket

(** HTTP Message Signatures (RFC 9421) *)
module Signature = Signature

(** {2 Logging} *)

(** Log source for the requests library.
    Use [Logs.Src.set_level src] to control logging verbosity.
    Example: [Logs.Src.set_level Requests.src (Some Logs.Debug)] *)
val src : Logs.Src.t

