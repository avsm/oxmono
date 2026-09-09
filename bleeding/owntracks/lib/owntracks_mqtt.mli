type t
(** OwnTracks envelopes over mqttz's borrowed payloads. *)

val topic : t -> string
val user : t -> string option
val device : t -> string option
val message : t -> Owntracks_message.t

val parse_topic : string -> (string * string) option
(** [parse_topic topic] extracts the user and device from
    [owntracks/user/device] or a subtopic. Custom prefixes return [None]. *)

val of_mqtt : topic:string -> payload:Mqttz.Slice.t -> (t, string) result
(** [of_mqtt ~topic ~payload] decodes without rewriting the payload. The MQTT
    topic takes precedence over a location's embedded [topic] field. *)

val default_topic : string
val user_topic : string -> string

val device_topic : user:string -> device:string -> string
(** [device_topic ~user ~device] builds a topic. Empty levels and MQTT wildcards
    raise [Invalid_argument], as for [user_topic]. *)

val pp : Format.formatter -> t -> unit
