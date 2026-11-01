(** The unauthenticated MSC4108 rendezvous channel.

    This module only implements the HTTP rendezvous layer. It deliberately does
    not provide the secure channel or interpret the bytes exchanged over it. *)

type method_ = [ `GET | `POST | `PUT | `DELETE ]

type response = {
  status : int;
  etag : Fetch.Header.etag option;
  content_type : Fetch.Header.media_type option;
  body : string;
  expires_at : Ptime.t option;
}
(** A parsed rendezvous response. Entity tags retain their weak/strong bit and
    media types retain legal parameters. A custom transport should parse an HTTP
    [Expires] value into [expires_at]. *)

type transport = {
  request :
    method_:method_ ->
    uri:Uriz.t ->
    headers:Fetch.Header.headers ->
    body:string option ->
    (response, Error.t) result;
  now : unit -> Ptime.t;
  sleep : float -> unit;
}
(** Injectable I/O, clock and sleeper capabilities. [request] must remain
    unauthenticated; [sleep] is used only between unchanged [304] polls and may
    propagate cancellation. *)

type error =
  | Transport_error of Error.t
  | Http_error of { status : int; body : string }
  | Invalid_response of string
  | Invalid_url of string
  | Missing_etag
  | Invalid_content_type of string
  | Empty_message
  | Expired
  | Closed
      (** Failures retain transport and non-success response bodies. [Expired]
          and [Closed] are terminal channel states. *)

type t

val transport_of_client :
  sleep:(float -> unit) -> ?now:(unit -> Ptime.t) -> Client.t -> transport
(** The Client adapter is origin-restricted and unauthenticated. It decodes
    typed ETag and media-type headers and parses all RFC 9110 HTTP-date forms in
    [Expires]. Malformed values are ignored; caller-provided expiry still
    applies. *)

val create :
  transport ->
  rendezvous_server:Uriz.t ->
  ?expires_at:Ptime.t ->
  unit ->
  (t, error) result
(** [create transport ~rendezvous_server ()] creates an outbound session with an
    unauthenticated [POST]. The response must contain a non-empty ETag and an
    absolute HTTP(S) [url]. An already expired response is rejected. *)

val accept :
  transport ->
  rendezvous_url:Uriz.t ->
  ?expires_at:Ptime.t ->
  unit ->
  (t * string, error) result
(** [accept transport ~rendezvous_url ()] opens an inbound session with an
    unconditional [GET], returning its initial (possibly empty) opaque body. The
    response must be successful and contain a non-empty ETag. *)

val rendezvous_url : t -> Uriz.t
val status : t -> [ `Active | `Closed | `Expired ]

val send : t -> string -> (unit, error) result
(** [send t message] conditionally replaces the current rendezvous body using
    [If-Match] and [Content-Type: text/plain], then advances to the returned
    ETag. Calls on terminal channels do no I/O. Callers must serialize channel
    operations. *)

val receive : t -> (string, error) result
(** [receive t] polls with [If-None-Match] until it obtains a non-empty
    [text/plain] body. A [304] advances any returned ETag, sleeps for one second
    through the transport and tries again. HTTP 404/410 closes the channel. *)

val close : t -> (unit, error) result
(** [close t] conditionally deletes an active rendezvous. It is idempotent for
    an already closed or expired channel; a failed deletion remains retryable.
*)
