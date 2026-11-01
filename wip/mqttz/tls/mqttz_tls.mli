(** MQTT over TLS with certificate and peer-name verification. *)

val connect :
  ?authenticator:X509.Authenticator.t @ portable ->
  sw:Eio.Switch.t -> net:_ Eio.Net.t -> clock:_ Eio.Time.Mono.t ->
  config:Mqttz_eio.config -> host:string -> port:int -> unit -> Mqttz_eio.t
(** [connect ...] verifies [host] as a DNS name or IP address. The default
    authenticator uses system trust anchors. TLS and MQTT handshakes share
    the configured operation timeout. *)
