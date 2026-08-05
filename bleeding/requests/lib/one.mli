(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** One-shot HTTP/1.1 client for stateless requests

    The One module provides a stateless HTTP client for single requests without
    session state like cookies, connection pooling, or persistent configuration.
    Each request opens a new TCP connection (with TLS for https://) that is
    closed when the Eio switch closes.

    Implements {{:https://datatracker.ietf.org/doc/html/rfc9112}RFC 9112} (HTTP/1.1)
    with {{:https://datatracker.ietf.org/doc/html/rfc9110}RFC 9110} semantics.

    For stateful requests with automatic cookie handling, connection pooling,
    and persistent configuration, use the main {!Requests} module instead.

    {2 Examples}

    {[
      open Eio_main

      let () = run @@ fun env ->
      Switch.run @@ fun sw ->

      (* Simple GET request *)
      let response = One.get ~sw
        ~clock:env#clock ~net:env#net
        "https://example.com" in
      Printf.printf "Status: %d\n" (Response.status_code response);
      Response.close response;

      (* POST with JSON body *)
      let response = One.post ~sw
        ~clock:env#clock ~net:env#net
        ~body:(Body.json {|{"key": "value"}|})
        ~headers:(Headers.empty |> Headers.content_type Mime.json)
        "https://api.example.com/data" in
      Response.close response;

      (* Download file with streaming *)
      One.download ~sw
        ~clock:env#clock ~net:env#net
        "https://example.com/large-file.zip"
        ~sink:(Eio.Path.(fs / "download.zip" |> sink))
    ]}
*)

(** Log source for one-shot request operations *)
val src : Logs.Src.t

(** {1 TLS Configuration} *)

(** Minimum TLS version configuration.
    Per security recommendations, allows enforcing minimum TLS version. *)
type tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)

(** {1 HTTP Request Methods}

    All functions are stateless - they open a new TCP connection for each request
    and close it when the switch closes. No connection pooling or reuse. *)

val request :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?follow_redirects:bool ->
  ?max_redirects:int ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?auto_decompress:bool ->
  ?min_tls_version:tls_version ->
  ?expect_100_continue:Expect_continue.config ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  method_:Method.t ->
  string ->
  Response.t
(** [request ~sw ~clock ~net ?headers ?body ?auth ?timeout ?follow_redirects
    ?max_redirects ?verify_tls ?tls_config ?auto_decompress ?min_tls_version
    ?expect_100_continue ?allow_insecure_auth ~method_ url]
    makes a single HTTP request without connection pooling.

    Each call opens a new TCP connection (with TLS if https://), makes the
    request, and closes the connection when the switch closes.

    @param sw Switch for resource management (response/connection bound to this)
    @param clock Clock for timeouts
    @param net Network interface for TCP connections
    @param headers Request headers (default: empty)
    @param body Request body (default: none)
    @param auth Authentication to apply (default: none)
    @param timeout Request timeout (default: 30s connect, 60s read)
    @param follow_redirects Whether to follow HTTP redirects (default: true)
    @param max_redirects Maximum redirects to follow (default: 10)
    @param verify_tls Whether to verify TLS certificates (default: true)
    @param tls_config Custom TLS configuration (default: system CA certs)
    @param auto_decompress Whether to automatically decompress gzip/deflate responses (default: true)
    @param min_tls_version Minimum TLS version to accept (default: TLS_1_2)
    @param expect_100_continue HTTP 100-continue configuration (default: [`Threshold 1_048_576L]).
           Use [`Disabled] to never send, [`Always] for all bodies, or [`Threshold n] for bodies >= n bytes.
    @param allow_insecure_auth Allow Basic/Bearer/Digest auth over HTTP (default: false).
           Per RFC 7617 Section 4 and RFC 6750 Section 5.1, these auth methods
           MUST be used over TLS. Set to [true] only for testing environments.
    @param proxy HTTP/HTTPS proxy configuration. When set, requests are routed through the proxy.
           HTTP requests use absolute-URI form (RFC 9112 Section 3.2.2).
           HTTPS requests use CONNECT tunneling (RFC 9110 Section 9.3.6).
    @param method_ HTTP method (GET, POST, etc.)
    @param url URL to request
*)

val get :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?follow_redirects:bool ->
  ?max_redirects:int ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** GET request. See {!request} for parameter details. *)

val post :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?expect_100_continue:Expect_continue.config ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** POST request with 100-continue support. See {!request} for parameter details. *)

val put :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?expect_100_continue:Expect_continue.config ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** PUT request with 100-continue support. See {!request} for parameter details. *)

val delete :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** DELETE request. See {!request} for parameter details. *)

val head :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** HEAD request. See {!request} for parameter details. *)

val patch :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?body:Body.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?expect_100_continue:Expect_continue.config ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  Response.t
(** PATCH request with 100-continue support. See {!request} for parameter details. *)

val upload :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?method_:Method.t ->
  ?mime:Mime.t ->
  ?length:int64 ->
  ?on_progress:(sent:int64 -> total:int64 option -> unit) ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?expect_100_continue:Expect_continue.config ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  source:Eio.Flow.source_ty Eio.Resource.t ->
  string ->
  Response.t
(** Upload from stream with 100-continue support (default: [`Threshold 1MB]).
    See {!request} for parameter details. *)

val download :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:_ Eio.Net.t ->
  ?headers:Headers.t ->
  ?auth:Auth.t ->
  ?timeout:Timeout.t ->
  ?on_progress:(received:int64 -> total:int64 option -> unit) ->
  ?verify_tls:bool ->
  ?tls_config:Tls.Config.client ->
  ?min_tls_version:tls_version ->
  ?allow_insecure_auth:bool ->
  ?proxy:Proxy.config ->
  string ->
  sink:Eio.Flow.sink_ty Eio.Resource.t ->
  unit
(** Download to stream. See {!request} for parameter details. *)
