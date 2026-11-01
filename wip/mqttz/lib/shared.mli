module Qos : sig
  type t = [ `At_most_once | `At_least_once | `Exactly_once ]

  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Protocol_version : sig
  type t = [ `V3_1_1 | `V5_0 ]

  val to_int : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Credentials : sig
  type t =
    [ `Username of string
    | `Password of string
    | `Username_password of string * string ]
  (** Passwords contain arbitrary binary data. A password without a username is
      supported only by MQTT 5.0. *)

  val pp : Format.formatter -> t -> unit
  (** [pp formatter credentials] redacts passwords. *)
end

module Will : sig
  type t

  val create : topic:string -> payload:string -> qos:Qos.t -> retain:bool -> t
  val topic : t -> string
  val payload : t -> string
  val qos : t -> Qos.t
  val retain : t -> bool
  val pp : Format.formatter -> t -> unit
end

module Packet_id : sig
  type t = int
  (** Packet identifiers are 1..65535. The codec checks this range. *)

  val pp : Format.formatter -> t -> unit
end

module Topic : sig
  module Name : sig
    type t = string

    val validate : t -> bool
    [@@zero_alloc]
    (** [validate name] checks UTF-8, length and absence of wildcards. *)

    val pp : Format.formatter -> t -> unit
  end

  module Filter : sig
    type t = string

    val validate : t -> bool
    [@@zero_alloc]
    (** [validate filter] checks MQTT UTF-8 and wildcard placement. *)

    val validate_shared : t -> bool
    [@@zero_alloc]
    (** [validate_shared filter] also checks MQTT 5 shared-subscription syntax.
    *)

    val matches : filter:t -> topic:Name.t -> bool
    [@@zero_alloc]
    (** [matches ~filter ~topic] checks matching, including the [$] namespace
        rule. A [$share/group/] prefix is interpreted as MQTT 5 syntax. *)

    val pp : Format.formatter -> t -> unit
  end
end

val pp_semi : Format.formatter -> unit -> unit

module Packet_type : sig
  type t =
    [ `RESERVED
    | `CONNECT
    | `CONNACK
    | `PUBLISH
    | `PUBACK
    | `PUBREC
    | `PUBREL
    | `PUBCOMP
    | `SUBSCRIBE
    | `SUBACK
    | `UNSUBSCRIBE
    | `UNSUBACK
    | `PINGREQ
    | `PINGRESP
    | `DISCONNECT
    | `AUTH ]

  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end
