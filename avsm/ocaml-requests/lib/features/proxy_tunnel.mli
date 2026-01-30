(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP CONNECT Tunneling for HTTPS via Proxy

    Per RFC 9110 Section 9.3.6:
    The CONNECT method requests that the recipient establish a tunnel
    to the destination origin server and, if successful, thereafter restrict
    its behavior to blind forwarding of packets in both directions.

    {2 Usage}

    Establish an HTTPS tunnel through an HTTP proxy:
    {[
      let tunnel_flow = Proxy_tunnel.connect
        ~sw ~net
        ~proxy:(Proxy.http "proxy.example.com")
        ~target_host:"api.example.com"
        ~target_port:443
        ()
      in
      (* Now wrap tunnel_flow with TLS and send HTTPS requests *)
    ]} *)

(** Log source for tunnel operations *)
val src : Logs.Src.t

(** {1 Tunnel Establishment} *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  proxy:Proxy.config ->
  target_host:string ->
  target_port:int ->
  unit ->
  [`Close | `Flow | `R | `Shutdown | `W] Eio.Resource.t
(** [connect ~sw ~net ~proxy ~target_host ~target_port ()] establishes
    an HTTP tunnel through [proxy] to [target_host:target_port].

    This performs the following steps per RFC 9110 Section 9.3.6:
    1. Opens a TCP connection to the proxy server
    2. Sends a CONNECT request with the target host:port
    3. Includes Proxy-Authorization header if proxy has auth configured
    4. Waits for a 2xx response from the proxy
    5. Returns the raw connection for the caller to wrap with TLS

    @param sw Eio switch for resource management
    @param net Eio network capability
    @param proxy Proxy configuration including host, port, and optional auth
    @param target_host Destination server hostname
    @param target_port Destination server port (typically 443 for HTTPS)
    @raise Error.Proxy_error if the CONNECT request fails
    @raise Error.Tcp_connect_failed if cannot connect to proxy *)

val connect_with_tls :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  proxy:Proxy.config ->
  target_host:string ->
  target_port:int ->
  ?tls_config:Tls.Config.client ->
  unit ->
  Eio.Flow.two_way_ty Eio.Resource.t
(** [connect_with_tls ~sw ~net ~clock ~proxy ~target_host ~target_port ?tls_config ()]
    establishes an HTTPS tunnel and performs TLS handshake.

    This is a convenience function that combines {!connect} with TLS wrapping:
    1. Establishes the tunnel via {!connect}
    2. Performs TLS handshake with the target host through the tunnel
    3. Returns the TLS-wrapped connection ready for HTTPS requests

    @param sw Eio switch for resource management
    @param net Eio network capability
    @param clock Eio clock for TLS operations
    @param proxy Proxy configuration
    @param target_host Destination server hostname (used for SNI)
    @param target_port Destination server port
    @param tls_config Optional custom TLS configuration. If not provided,
           uses default configuration from system CA certificates.
    @raise Error.Proxy_error if tunnel establishment fails
    @raise Error.Tls_handshake_failed if TLS handshake fails *)

(** {1 Low-level Functions} *)

val write_connect_request :
  Eio.Buf_write.t ->
  proxy:Proxy.config ->
  target_host:string ->
  target_port:int ->
  unit
(** [write_connect_request w ~proxy ~target_host ~target_port] writes
    a CONNECT request to the buffer.

    Format per RFC 9110 Section 9.3.6:
    {v
    CONNECT host:port HTTP/1.1
    Host: host:port
    Proxy-Authorization: Basic ... (if auth configured)

    v}

    This is exposed for testing and custom tunnel implementations. *)

val parse_connect_response :
  Eio.Buf_read.t ->
  proxy:Proxy.config ->
  target:string ->
  unit
(** [parse_connect_response r ~proxy ~target] reads and validates
    the CONNECT response from the proxy.

    @raise Error.Proxy_error if the response status is not 2xx *)
