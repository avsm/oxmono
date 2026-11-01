(** Stable Zulip conversations.

    A channel room identifies a channel across all topics while retaining the
    topic of its source destination for replies. A direct room identifies a
    direct-message recipient set. *)

type id =
  | Channel of Zulip.Id.Channel.t
  | Direct of Zulip.Id.Recipient.t
      (** The type for conversation identifiers. *)

type t
(** The type for conversations and their reply destinations. *)

val of_destination : Context.t -> Zulip.Message.destination -> t
(** [of_destination context destination] is the room that sends replies through
    [context] to [destination]. *)

val id : t -> id
(** [id room] is the stable conversation identifier of [room]. *)

val key : t -> string
(** [key room] is a process-independent string identifying the conversation of
    [room]. Channel keys do not include the topic. *)

val destination : t -> Zulip.Message.destination
(** [destination room] is the reply destination retained by [room]. *)

val topic : t -> string option
(** [topic room] is the retained channel topic, or [None] for a direct room. *)

val is_direct : t -> bool
(** [is_direct room] is [true] if [room] is a direct conversation. *)

val participants : t -> Zulip.Id.User.t list
(** [participants room] is the direct-message participant list. It is empty for
    a channel room. *)

val send_text : t -> string -> Sent.t
(** [send_text room content] enqueues [content] for the retained destination of
    [room]. It can block until the context send queue admits the request. The
    returned handle describes delivery after admission. *)

val equal_id : id -> id -> bool
(** [equal_id a b] is [true] if [a] and [b] identify the same conversation. *)

val compare_id : id -> id -> int
(** [compare_id a b] orders channel identifiers before direct-recipient
    identifiers and orders identifiers of the same kind numerically. *)

val pp_id : Format.formatter -> id -> unit
(** [pp_id ppf id] prints [id] with its conversation kind on [ppf]. *)
