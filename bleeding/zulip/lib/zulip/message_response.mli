@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Responses from sending Zulip messages.

    A response identifies the new message and may include the visibility policy
    automatically assigned to a newly created topic. Unrecognized object members
    are preserved. *)

type t
(** The type for successful send-message responses. *)

val id : t -> Id.Message.t
(** [id response] is the identifier of the sent message. *)

val automatic_new_visibility_policy : t -> Topic_visibility.t option
(** [automatic_new_visibility_policy response] is the visibility policy
    automatically assigned to the new topic, or [None] if the response omits it.
    Unknown integer policies remain representable. *)

val raw : t -> Jsont.json
(** [raw response] is the complete response object, including unrecognized
    members. *)

val jsont : t Jsont.t
(** [jsont] is a codec for send-message response objects. The [id] member is
    required and [automatic_new_visibility_policy] is optional. Unrecognized
    members are preserved. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf response] writes the sent message identifier to [ppf]. *)
