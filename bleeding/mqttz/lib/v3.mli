(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** MQTT v3.1.1 Protocol Implementation

    @see <http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html>
      OASIS Standard *)

(** {1 Return Codes}
    @see <http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718035>
      Section 3.2.2.3 *)

module Return_code : sig
  type t =
    [ `Accepted
    | `Unacceptable_protocol_version
    | `Identifier_rejected
    | `Server_unavailable
    | `Bad_username_or_password
    | `Not_authorized ]

  val pp : Format.formatter -> t -> unit
  val to_int : t -> int
  val of_int : int -> t
  val to_string : t -> string
end

(** {1 SUBACK Codes}
    @see <http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718071>
      Section 3.9.3 *)

module Suback_code : sig
  type t = [ `Granted_qos_0 | `Granted_qos_1 | `Granted_qos_2 | `Failure ]

  val pp : Format.formatter -> t -> unit
  val to_int : t -> int
  val of_int : int -> t
end

(** {1 Subscription} *)

module Subscription : sig
  type t = { filter : Shared.Topic.Filter.t; qos : Shared.Qos.t }

  val pp : Format.formatter -> t -> unit
end

(** {1 Packets}
    @see <http://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html#_Toc398718027>
      Section 2.2 *)

module Packet : sig
  (** {2 Connect} *)

  module Connect : sig
    type t = {
      clean_session : bool;
      keep_alive : int;
      client_id : string;
      credentials : Shared.Credentials.t option;
      will : Shared.Will.t option;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Connack} *)

  module Connack : sig
    type t = { session_present : bool; return_code : Return_code.t }

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
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Subscribe} *)

  module Subscribe : sig
    type t = { packet_id : Shared.Packet_id.t; topics : Subscription.t list }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Suback} *)

  module Suback : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      return_codes : Suback_code.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Unsubscribe} *)

  module Unsubscribe : sig
    type t = {
      packet_id : Shared.Packet_id.t;
      topics : Shared.Topic.Filter.t list;
    }

    val pp : Format.formatter -> t -> unit
  end

  (** {2 Packet Type} *)

  type t =
    | Connect of Connect.t
    | Connack of Connack.t
    | Publish of Publish.t
    | Puback of Shared.Packet_id.t
    | Pubrec of Shared.Packet_id.t
    | Pubrel of Shared.Packet_id.t
    | Pubcomp of Shared.Packet_id.t
    | Subscribe of Subscribe.t
    | Suback of Suback.t
    | Unsubscribe of Unsubscribe.t
    | Unsuback of Shared.Packet_id.t
    | Pingreq
    | Pingresp
    | Disconnect

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
