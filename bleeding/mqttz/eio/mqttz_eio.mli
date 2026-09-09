(** A client for one clean MQTT session. Operations may run in concurrent fibers
    on one domain. Calls requiring acknowledgements are serialized. Reconnection
    and persistent sessions are application responsibilities. *)

type config = {
  client_id : string;
  version : Mqttz.Protocol_version.t;
  keep_alive : int;
  credentials : Mqttz.Credentials.t option;
  will : Mqttz.Will.t option;
  max_packet_size : int;
  message_capacity : int;
  operation_timeout : float;
}

val default_config : client_id:string -> config
val validate_config : config -> unit

type message = {
  topic : string;
  payload : Mqttz.Slice.t;
  qos : Mqttz.Qos.t;
  retain : bool;
  properties : Mqttz.V5.Property.t list;
}
(** Received payloads own their backing buffer through the view and remain valid
    after the next receive. They require no defensive copy. *)

exception Closed
exception Protocol_error of string
exception Rejected of string

type t

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.Mono.t ->
  config:config ->
  host:string ->
  port:int ->
  unit ->
  t
(** [connect ...] opens a plaintext TCP connection and completes CONNECT. The
    connection and background fibers belong to [sw]. *)

val of_flow :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  config:config ->
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
  t
(** [of_flow ... flow] completes CONNECT over an existing flow, taking ownership
    of it. This supports TLS and test flows via Bytesrw adapters. The adapters
    can copy at the flow boundary. *)

val is_connected : t -> bool

val publish :
  ?qos:Mqttz.Qos.t ->
  ?retain:bool ->
  ?properties:Mqttz.V5.Property.t list ->
  t ->
  topic:string ->
  Mqttz.Slice.t ->
  unit
(** [publish t ~topic payload] borrows [payload] until it returns. QoS 1 waits
    for PUBACK. QoS 2 waits for PUBCOMP. Negative acknowledgements raise
    [Rejected]. An interrupted exchange closes the connection. *)

val subscribe : ?qos:Mqttz.Qos.t -> t -> string list -> unit
(** [subscribe t filters] waits for SUBACK and rejects failed grants. *)

val unsubscribe : t -> string list -> unit
(** [unsubscribe t filters] waits for UNSUBACK. *)

val receive : t -> message
(** [receive t] waits for a message or raises when the connection closes. If the
    bounded message queue fills, the connection closes rather than losing
    messages silently or blocking acknowledgement processing. *)

val disconnect : t -> unit
(** [disconnect t] sends DISCONNECT before closing. It is idempotent. *)

val close : t -> unit
(** [close t] closes immediately without DISCONNECT, allowing the broker to
    publish the Will. It is idempotent. *)
