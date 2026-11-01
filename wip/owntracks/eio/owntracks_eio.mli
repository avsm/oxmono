(** OwnTracks messages on an existing mqttz connection. *)

val receive : Mqttz_eio.t -> (Owntracks.Mqtt.t, string) result
(** [receive client] waits for a message and decodes its payload directly.
    Malformed or unsupported OwnTracks messages return [Error]. Transport
    failures raise mqttz exceptions. *)

val publish :
  ?qos:Mqttz.Qos.t ->
  ?retain:bool ->
  Mqttz_eio.t ->
  topic:string ->
  Owntracks.Message.t ->
  unit
(** [publish client ~topic message] encodes and publishes [message]. Invalid
    JSON values raise [Invalid_argument]. mqttz controls QoS and retention. *)
