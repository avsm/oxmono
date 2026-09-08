@@ portable

(** Zulip messages received from the server.

    A message has a channel or direct-message destination. Decoding validates
    documented structured members and preserves unrecognized object members. *)

type destination =
  | Channel of {
      channel_id : Id.Channel.t;
      channel_name : string;
      topic : string;
    }
  | Direct of { recipient_id : Id.Recipient.t; participants : Id.User.t list }
      (** The type for message destinations. Channel destinations contain the
          channel identifier, channel display name, and topic. Direct
          destinations contain the recipient-set identifier and participant
          identifiers. *)

type t = private {
  id : Id.Message.t;
  sender_id : Id.User.t;
  sender_email : string;
  sender_full_name : string;
  timestamp : float;
  content : string;
  content_type : string;
  destination : destination;
  flags : Message_flag.t list;
  raw : Jsont.json;
}
(** The type for received messages. [timestamp] is Unix time in seconds. [raw]
    is the complete message object. *)

val id : t -> Id.Message.t
(** [id message] is the identifier of [message]. *)

val sender_id : t -> Id.User.t
(** [sender_id message] is the identifier of the sender of [message]. *)

val sender_email : t -> string
(** [sender_email message] is the API email address of the sender. *)

val sender_full_name : t -> string
(** [sender_full_name message] is the display name of the sender. *)

val timestamp : t -> float
(** [timestamp message] is the send time of [message] as finite Unix time in
    seconds. *)

val content : t -> string
(** [content message] is the message body. Its representation is described by
    {!val-content_type}. *)

val content_type : t -> string
(** [content_type message] is the media type of {!val-content}. *)

val destination : t -> destination
(** [destination message] is the channel or direct-message destination of
    [message]. *)

val flags : t -> Message_flag.t list
(** [flags message] is the list of flags attached to [message]. A missing wire
    member decodes as an empty list. Unknown flags remain representable. *)

val raw : t -> Jsont.json
(** [raw message] is the complete JSON object for [message], including
    unrecognized members. *)

type direct_participant = private {
  user_id : Id.User.t;
  email : string option;
  full_name : string option;
  is_mirror_dummy : bool option;
  raw : Jsont.json;
}
(** The type for an entry in a direct message's [display_recipient] list. [raw]
    retains unrecognized object members. *)

type edit = private {
  topic : string option;
  previous_topic : string option;
  channel_id : Id.Channel.t option;
  previous_channel_id : Id.Channel.t option;
  content : string option;
  rendered_content : string option;
  previous_content : string option;
  previous_rendered_content : string option;
  user_id : Id.User.t option;
  content_html_diff : string option;
  timestamp : int option;
  raw : Jsont.json;
}
(** The type for a message edit history entry. [timestamp] is Unix time in
    seconds. [user_id] is [None] when its wire member is absent or null. [raw]
    retains unrecognized object members. *)

type reaction = private {
  emoji_name : string;
  emoji_code : string;
  reaction_type : string;
  user_id : Id.User.t;
  raw : Jsont.json;
}
(** The type for an emoji reaction on a message. [raw] retains unrecognized
    object members. *)

type topic_link = private { text : string; url : string; raw : Jsont.json }
(** The type for a link detected in a message topic. [raw] retains unrecognized
    object members. *)

type submessage = private {
  id : int;
  message_id : Id.Message.t;
  sender_id : Id.User.t;
  msg_type : string;
  content : string;
  raw : Jsont.json;
}
(** The type for application-defined data attached to a message. [raw] retains
    unrecognized object members. *)

val client : t -> string option
(** [client message] is the name of the client that sent [message], or [None] if
    the member is absent. *)

val avatar_url : t -> string option
(** [avatar_url message] is the sender's avatar URL, or [None] if the member is
    absent or null. *)

val is_me_message : t -> bool option
(** [is_me_message message] is the server's emote-message state, or [None] if
    the member is absent. *)

val last_edit_timestamp : t -> int option
(** [last_edit_timestamp message] is the most recent edit time as Unix time in
    seconds, or [None] if the member is absent. *)

val last_moved_timestamp : t -> int option
(** [last_moved_timestamp message] is the most recent move time as Unix time in
    seconds, or [None] if the member is absent. *)

val sender_realm : t -> string option
(** [sender_realm message] is the sender's organization string from the
    [sender_realm_str] member, or [None] if it is absent. *)

val edit_history : t -> edit list option
(** [edit_history message] is the edit history, or [None] if the member is
    absent. A present empty list remains distinct from absence. *)

val reactions : t -> reaction list option
(** [reactions message] is the reaction list, or [None] if the member is absent.
    A present empty list remains distinct from absence. *)

val topic_links : t -> topic_link list option
(** [topic_links message] is the topic link list, or [None] if the member is
    absent. A present empty list remains distinct from absence. *)

val submessages : t -> submessage list option
(** [submessages message] is the attached submessage list, or [None] if the
    member is absent. A present empty list remains distinct from absence. *)

val direct_participants : t -> direct_participant list option
(** [direct_participants message] is the structured [display_recipient] list for
    a direct [message]. It is [None] for a channel message. A present empty list
    remains distinct from absence. *)

val direct_participant_jsont : direct_participant Jsont.t
(** [direct_participant_jsont] is a codec for direct-message participant
    objects. It preserves unrecognized members. *)

val edit_jsont : edit Jsont.t
(** [edit_jsont] is a codec for message edit objects. It preserves unrecognized
    members. *)

val reaction_jsont : reaction Jsont.t
(** [reaction_jsont] is a codec for message reaction objects. It preserves
    unrecognized members. *)

val topic_link_jsont : topic_link Jsont.t
(** [topic_link_jsont] is a codec for topic link objects. It preserves
    unrecognized members. *)

val submessage_jsont : submessage Jsont.t
(** [submessage_jsont] is a codec for submessage objects. It preserves
    unrecognized members. *)

val jsont : t Jsont.t
(** [jsont] is a codec for received message objects. Channel messages require
    [stream_id], a string [display_recipient], and [subject]. Direct messages
    require [recipient_id] and an array [display_recipient]. The accepted direct
    type spellings are ["private"] and ["direct"]. The channel spelling is
    ["stream"]. Decoding rejects nonfinite timestamps and malformed documented
    optional members. Encoding uses ["private"] for direct messages and
    ["stream"] for channel messages. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf message] writes the message identifier, sender identifier, and
    destination kind to [ppf]. *)

val create :
  id:Id.Message.t ->
  sender_id:Id.User.t ->
  sender_email:string ->
  sender_full_name:string ->
  timestamp:float ->
  content:string ->
  destination:destination ->
  ?content_type:string ->
  ?flags:Message_flag.t list ->
  unit ->
  (t, Jsont.Error.t) result
(** [create ~id ~sender_id ~sender_email ~sender_full_name ~timestamp ~content
     ~destination ()] is a synthetic message with a JSON object consistent with
    its fields. [timestamp] is Unix time in seconds and must be finite.
    [content_type] defaults to ["text/x-markdown"]. [flags] defaults to the
    empty list. The result is [Error error] if the message cannot be encoded or
    validated. *)
