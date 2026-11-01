@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Threads.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-3} RFC 8621 Section
     3} defines the Thread object, which is a set of Emails the server considers
    to be one conversation. A Thread is entirely server set and has no [/set]
    method.

    @canonical Jmap.Proto.Thread *)

(** {1 Properties} *)

type property = [ `Id | `Email_ids ]
(** The type for the properties a [Thread/get] may ask for. *)

val property_to_string : [< property ] -> string
(** [property_to_string p] is the wire name of [p], such as ["emailIds"]. *)

val property_of_string : string -> property option
(** [property_of_string s] is the property whose wire name is [s], or [None] if
    there is none. The comparison is by octet. *)

(** {1 Threads} *)

type t = {
  id : Proto_id.t option;  (** The server assigned id of the Thread. *)
  email_ids : Proto_id.t list option;
      (** The ids of the Emails of the Thread, sorted by their [receivedAt]. *)
}
(** The type for Thread objects. A property is [None] when the [Thread/get] did
    not ask for it. *)

val id : t -> Proto_id.t option
(** [id t] is the id of the Thread [t], or [None] if the [Thread/get] did not
    ask for it. *)

val jsont : t Jsont.t
(** [jsont] is the codec for a Thread. *)
