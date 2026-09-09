(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** MQTT v5.0 Protocol Implementation

    @see <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html>
      OASIS Standard *)

(** {1 Reason Codes}
    @see <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901031>
      Section 2.4 *)

module Reason_code : sig
  type t =
    [ `Success
    | `Normal_disconnection
    | `Granted_qos_0
    | `Granted_qos_1
    | `Granted_qos_2
    | `Disconnect_with_will
    | `No_matching_subscribers
    | `No_subscription_existed
    | `Continue_authentication
    | `Re_authenticate
    | `Unspecified_error
    | `Malformed_packet
    | `Protocol_error
    | `Implementation_specific_error
    | `Unsupported_protocol_version
    | `Client_identifier_not_valid
    | `Bad_user_name_or_password
    | `Not_authorized
    | `Server_unavailable
    | `Server_busy
    | `Banned
    | `Server_shutting_down
    | `Bad_authentication_method
    | `Keep_alive_timeout
    | `Session_taken_over
    | `Topic_filter_invalid
    | `Topic_name_invalid
    | `Packet_identifier_in_use
    | `Packet_identifier_not_found
    | `Receive_maximum_exceeded
    | `Topic_alias_invalid
    | `Packet_too_large
    | `Message_rate_too_high
    | `Quota_exceeded
    | `Administrative_action
    | `Payload_format_invalid
    | `Retain_not_supported
    | `Qos_not_supported
    | `Use_another_server
    | `Server_moved
    | `Shared_subscriptions_not_supported
    | `Connection_rate_exceeded
    | `Maximum_connect_time
    | `Subscription_identifiers_not_supported
    | `Wildcard_subscriptions_not_supported ]

  val pp : Format.formatter -> t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
end

(** {1 Properties}
    @see <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901027>
      Section 2.2.2 *)

module Property : sig
  type t =
    | Payload_format_indicator of int
    | Message_expiry_interval of int32
    | Content_type of string
    | Response_topic of string
    | Correlation_data of string
    | Subscription_identifier of int
    | Session_expiry_interval of int32
    | Assigned_client_identifier of string
    | Server_keep_alive of int
    | Authentication_method of string
    | Authentication_data of string
    | Request_problem_information of int
    | Will_delay_interval of int32
    | Request_response_information of int
    | Response_information of string
    | Server_reference of string
    | Reason_string of string
    | Receive_maximum of int
    | Topic_alias_maximum of int
    | Topic_alias of int
    | Maximum_qos of Shared.Qos.t
    | Retain_available of bool
    | User_property of string * string
    | Maximum_packet_size of int32
    | Wildcard_subscription_available of bool
    | Subscription_identifier_available of bool
    | Shared_subscription_available of bool

  val pp : Format.formatter -> t -> unit
end

(** {1 Subscription Options}
    @see <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901169>
      Section 3.8.3.1 *)

module Subscription_options : sig
  type t = {
    qos : Shared.Qos.t;
    no_local : bool;
    retain_as_published : bool;
    retain_handling : int;
  }

  val pp : Format.formatter -> t -> unit
  val default : Shared.Qos.t -> t
end

(** {1 Subscription} *)

module Subscription : sig
  type t = { filter : Shared.Topic.Filter.t; options : Subscription_options.t }

  val pp : Format.formatter -> t -> unit
end

(** {1 Will Properties} *)

module Will_properties : sig
  type t = {
    will_topic : string;
    will_payload : string;
    will_qos : Shared.Qos.t;
    will_retain : bool;
    will_properties : Property.t list;
  }

  val pp : Format.formatter -> t -> unit
end

(** {1 Packets}
    @see <https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901019>
      Section 2.1 *)

module Packet : sig
  (** {2 Connect} *)

  module Connect : sig
    type t = {
      clean_start : bool;
      keep_alive : int;
      client_id : string;
      credentials : Shared.Credentials.t option;
      will : Will_properties.t option;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Connack} *)

  module Connack : sig
    type t = {
      session_present : bool;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Publish} *)

  module Publish : sig
    type t = {
      dup : bool;
      qos : Shared.Qos.t;
      retain : bool;
      topic : Shared.Topic.Name.t;
      packet_id : Shared.Packet_id.t option;
      payload : Slice.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Puback} *)

  module Puback : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Pubrec} *)

  module Pubrec : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Pubrel} *)

  module Pubrel : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Pubcomp} *)

  module Pubcomp : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Subscribe} *)

  module Subscribe : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      topics : Subscription.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Suback} *)

  module Suback : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      reason_codes : Reason_code.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Unsubscribe} *)

  module Unsubscribe : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      topics : Shared.Topic.Filter.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Unsuback} *)

  module Unsuback : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      reason_codes : Reason_code.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Disconnect} *)

  module Disconnect : sig
    type t = { reason_code : Reason_code.t; properties : Property.t list }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Auth} *)

  module Auth : sig
    type t = { reason_code : Reason_code.t; properties : Property.t list }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Packet Type} *)

  type t =
    | Connect of Connect.t
    | Connack of Connack.t
    | Publish of Publish.t
    | Puback of Puback.t
    | Pubrec of Pubrec.t
    | Pubrel of Pubrel.t
    | Pubcomp of Pubcomp.t
    | Subscribe of Subscribe.t
    | Suback of Suback.t
    | Unsubscribe of Unsubscribe.t
    | Unsuback of Unsuback.t
    | Pingreq
    | Pingresp
    | Disconnect of Disconnect.t
    | Auth of Auth.t

  val pp : Format.formatter -> t -> unit

  val validate : t -> unit
  (** [validate packet] raises [Invalid_argument] for invalid packet fields. *)

  val decode : ?max_size:int -> Slice.t -> (t, string) result
  (** [decode frame] decodes exactly one frame. PUBLISH payloads borrow
      [frame]'s bytes, which must remain unchanged while those views are used.
      Metadata strings are copied. The default limit is 16 MiB. *)

  val encode : t -> Slice.t list
  (** [encode packet] is its wire representation. PUBLISH payloads are borrowed
      and kept separate from the allocated header. Invalid fields raise
      [Invalid_argument]. Keep all bytes unchanged until writing ends. *)

  val to_bytes : t -> bytes
  (** [to_bytes packet] copies the complete encoding into an owned buffer. *)
end
