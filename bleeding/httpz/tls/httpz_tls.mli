(** TLS flows for Httpz clients and servers.

    This library supplies the Eio TLS plumbing shared by [Fetch_httpz] and
    [Proffer_httpz]. It uses the pure-OCaml [tls-eio] stack. *)

type flow =
  [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
(** A closeable bidirectional flow before or after TLS is applied. *)

type client = Httpz.Uriz.t -> flow -> flow
(** A [client] upgrades a connected flow for the HTTPS origin named by the
    URI. *)

type server = flow -> flow
(** A [server] accepts TLS on a newly connected flow. *)

exception Error of string
(** [Error message] reports TLS setup, peer-name, trust-store, or handshake
    failure. Cancellation is never converted to [Error]. *)

val close : ?timeout:float -> clock:_ Eio.Time.Mono.t -> flow -> unit
(** [close ~clock flow] attempts a TLS close notification for at most
    [timeout] seconds, then closes [flow] in a cancellation-protected cleanup.
    Shutdown, timeout, peer, and close errors are contained. [timeout] defaults
    to one second and must be finite and non-negative. *)

val client :
  authenticator:X509.Authenticator.t @ portable -> client @ portable
  @@ portable
(** [client ~authenticator] verifies each peer with [authenticator]. DNS names
    are checked as DNS subjectAltNames and sent as SNI. IPv4 and IPv6 literals
    are checked as IP subjectAltNames and are not sent as DNS SNI.

    A fresh TLS configuration and a fresh explicit Unix getentropy generator
    are made for each peer; client handshakes do not consult or mutate Mirage
    Crypto's process-global generator. *)

val system : client @@ portable
(** [system] is {!val-client} using the operating system's trust anchors. The
    trust store is loaded once when this library is initialized, and the
    resulting portable authenticator is reused by every connection. *)

val server : Tls.Config.server -> server
(** [server config] accepts a TLS connection using [config]. For Httpz HTTP/1.1
    servers, configure ALPN with either [["http/1.1"]] or no protocols. The
    ordinary server path installs Mirage Crypto's Unix generator on first use
    if the application has not already selected one. *)
