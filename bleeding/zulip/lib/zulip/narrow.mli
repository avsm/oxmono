@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Narrow filters for Zulip message queries.

    A narrow is a conjunction of filter clauses supplied to message query
    endpoints. Each clause encodes an operator, an operand, and whether the
    operator is negated. *)

type t
(** The type for a single narrow filter clause. *)

val stream : string -> t
(** [stream name] is a clause selecting messages in the channel named [name]. *)

val stream_id : Id.Channel.t -> t
(** [stream_id id] is a clause selecting messages in the channel identified by
    [id]. *)

val topic : string -> t
(** [topic name] is a clause selecting channel messages with topic [name]. *)

val channel : string -> t
(** [channel name] is {!stream} applied to [name]. *)

val sender : string -> t
(** [sender email] is a clause selecting messages sent by the user with [email].
*)

val sender_id : Id.User.t -> t
(** [sender_id id] is a clause selecting messages sent by the user identified by
    [id]. *)

type is_operand =
  [ `Alerted  (** The message contains an alert word. *)
  | `Dm  (** The message is a direct message. *)
  | `Mentioned  (** The current user is mentioned. *)
  | `Private  (** The message is a direct message. *)
  | `Resolved  (** The message belongs to a resolved topic. *)
  | `Starred  (** The message is starred. *)
  | `Unread  (** The message is unread. *) ]
(** The type for message properties accepted by an [is] clause. [`Dm] and
    [`Private] retain their distinct wire spellings. *)

val is : is_operand -> t
(** [is property] is a clause selecting messages with [property]. *)

type has_operand =
  [ `Attachment  (** The message has a file attachment. *)
  | `Image  (** The message contains an image. *)
  | `Link  (** The message contains a link. *)
  | `Reaction  (** The message has an emoji reaction. *) ]
(** The type for content properties accepted by a [has] clause. *)

val has : has_operand -> t
(** [has property] is a clause selecting messages with [property]. *)

val search : string -> t
(** [search query] is a clause selecting messages that match the full-text
    search expression [query]. *)

val id : Id.Message.t -> t
(** [id message_id] is a clause selecting the message identified by
    [message_id]. *)

val near : Id.Message.t -> t
(** [near message_id] is a clause centering a result window on the message
    identified by [message_id]. *)

val dm : string list -> t
(** [dm emails] is a clause selecting direct-message conversations whose
    participants have exactly the email addresses in [emails]. *)

val dm_including : string -> t
(** [dm_including email] is a clause selecting direct-message conversations that
    include the user with [email]. *)

val not_ : t -> t
(** [not_ clause] is [clause] with its negation state reversed. Applying [not_]
    twice restores the original clause. *)

val jsont : t Jsont.t
(** [jsont] is a codec for narrow clause objects. A missing [negated] member
    decodes as [false]. Integer operands must be exactly representable JSON
    integers. *)

val list_jsont : t list Jsont.t
(** [list_jsont] is a codec for lists of narrow clauses. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf clause] writes [clause] as an operator and operand prefixed with [-]
    when it is negated. *)
