@@ portable

(** The content of the moderation, encryption and extensible event types.

    These are the event types built on the extensible-events proposals, plus the
    two encryption types and the moderation policy rules. Each module exposes
    the record its codec reads.

    @see <https://spec.matrix.org/v1.11/client-server-api/#events> Events *)

(** {1 Moderation} *)

module Recommendation : sig
  (** What to do with an entity a moderation policy rule matches. *)

  type t =
    | Ban  (** The only value the specification defines. *)
    | Unknown of string  (** A recommendation this type does not name. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t
  (** [of_string s] is the recommendation [s] names, or [Unknown s]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same recommendation. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Policy_rule_content : sig
  (** The content of [m.policy.rule.user], [m.policy.rule.room] and
      [m.policy.rule.server], a published moderation rule of the kind a ban list
      distributes.

      @see <https://spec.matrix.org/v1.11/client-server-api/#moderation-policy-lists>
        Moderation policy lists *)

  type t = {
    entity : string;
        (** A glob over user ids, room ids or server names, according to the
            event type. *)
    reason : string;  (** Shown to moderators. It may be empty. *)
    recommendation : Recommendation.t;
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Marked_unread_content : sig
  (** The content of [m.marked_unread], room account data recording that the
      user marked the room unread by hand. No message count reflects it.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mmarked_unread>
        m.marked_unread *)

  type t = { unread : bool }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent [unread] member decodes to
      [false]. *)
end

(** {1 Encryption} *)

module Encryption_algorithm : sig
  (** The algorithm an [m.room.encrypted] event was encrypted under. *)

  type t =
    | Olm_v1_curve25519_aes_sha2  (** To-device, per recipient device. *)
    | Megolm_v1_aes_sha2  (** Room messages, per sending session. *)
    | Unknown of string  (** An algorithm this type does not name. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t
  (** [of_string s] is the algorithm [s] names, or [Unknown s]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same algorithm. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Encrypted_content : sig
  (** The content of [m.room.encrypted], a ciphertext that is either a Megolm
      room message or an Olm to-device message.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroomencrypted>
        m.room.encrypted *)

  type t = {
    algorithm : Encryption_algorithm.t;
    sender_key : string;
        (** The sender's Curve25519 identity key, unpadded base64. *)
    ciphertext : Jsont.json;
        (** A string under Megolm. Under Olm it is an object keyed by recipient
            device key, which is why it is left untyped. *)
    session_id : string option;  (** Megolm only. Which session to use. *)
    device_id : string option;  (** Megolm only. The sending device. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

(** {1 Reactions, polls and live location} *)

module Reaction_content : sig
  (** The content of [m.reaction], an emoji reaction to another event.

      The relation is of kind {!Matrix_event_core.Rel_type.Annotation} and its
      [key] carries the reaction itself.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mreaction>
        m.reaction *)

  type t = { relates_to : Matrix_event_core.Relates_to.t }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Beacon_info_content : sig
  (** The content of [m.beacon_info], state announcing that a user has started
      sharing their live location. The positions follow as {!Beacon_content}
      events.

      @see <https://github.com/matrix-org/matrix-spec-proposals/pull/3672>
        MSC3672 Sharing ephemeral streams of location data *)

  type t = {
    description : string option;  (** What is being shared, for display. *)
    live : bool;  (** Sharing is under way. [false] ends it early. *)
    timeout : int64;  (** Milliseconds the share lasts. *)
    timestamp : Matrix_event_core.Timestamp.t option;
        (** Start time in milliseconds. Absent in early MSC3488 events. *)
    asset_type : string option;
        (** What the location describes, such as ["m.self"]. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent [live] member decodes to
      [true]. *)
end

module Beacon_content : sig
  (** The content of [m.beacon], one position in a live location share. *)

  type location = {
    uri : string;  (** A [geo:] URI. *)
    description : string option;
  }
  (** The type for one reported position. *)

  type t = {
    location : location;
    timestamp : Matrix_event_core.Timestamp.t;
        (** When the position was taken. *)
    relates_to : Matrix_event_core.Relates_to.t;
        (** A reference to the {!Beacon_info_content} event this position
            belongs to. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Poll_start_content : sig
  (** The content of [m.poll.start], which opens a poll.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mpollstart>
        m.poll.start *)

  type poll_answer = {
    id : string;  (** What a response cites, at most 255 bytes. *)
    text : string;
  }
  (** The type for one answer a respondent may pick. *)

  (** The type for how a poll reports its tally. *)
  type poll_kind =
    | Disclosed  (** The running tally is shown. *)
    | Undisclosed  (** The tally is revealed only once the poll ends. *)

  type poll_start = {
    question : string;
    kind : poll_kind;
    max_selections : int;  (** How many answers one user may pick. *)
    answers : poll_answer list;  (** At most twenty. *)
  }
  (** The type for the poll definition. *)

  type t = {
    poll_start : poll_start;
    text : string;
        (** A rendering of the poll for clients that cannot show one. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. An absent [kind] decodes to
      [Disclosed] and an absent [max_selections] to [1]. *)
end

module Poll_response_content : sig
  (** The content of [m.poll.response], one user's answers. A later response
      from the same user replaces the earlier one, and responses after the poll
      ends are ignored. *)

  type t = {
    relates_to : Matrix_event_core.Relates_to.t;
        (** A reference to the poll's start event. *)
    answers : string list;
        (** Answer ids. More than [max_selections] of them is truncated, and ids
            the poll does not offer make the response spoilt. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Poll_end_content : sig
  (** The content of [m.poll.end], which closes a poll. Only the poll's sender,
      or a user who may redact, can end it. *)

  type t = {
    relates_to : Matrix_event_core.Relates_to.t;
        (** A reference to the poll's start event. *)
    text : string;  (** A rendering of the final result. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end
