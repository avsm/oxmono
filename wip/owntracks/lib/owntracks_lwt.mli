(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** LWT (Last Will and Testament) message type for OwnTracks.

    @canonical Owntracks.Lwt

    Published automatically by the MQTT broker when a client disconnects
    unexpectedly. This allows subscribers to know when a device has gone
    offline. *)

type t : immutable_data
(** The type for LWT messages. *)

(** {1 Constructors} *)

val v : tst:int -> t
(** [v ~tst] creates an LWT message with the given timestamp. *)

(** {1 Accessors} *)

val tst : t -> int
(** [tst lwt] is the timestamp at which the client first connected. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** [jsont] is a JSON codec for LWT messages. Expects the ["_type"] field to be
    ["lwt"]. *)

val jsont_bare : t Jsont.t
(** [jsont_bare] is a JSON codec that doesn't require the ["_type"] field. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf lwt] pretty-prints an LWT message. *)
