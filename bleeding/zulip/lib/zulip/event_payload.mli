@@ portable

(** Typed payloads from Zulip event queues.

    Supported event families decode into private records. Each record retains
    the complete payload object in [raw]. Unsupported and future families remain
    available through {!t.constructor-Unknown}. *)

type op =
  | Add
  | Remove
  | Update
  | Create
  | Delete
  | Peer_add
  | Peer_remove
  | Add_members
  | Remove_members
  | Add_subgroups
  | Remove_subgroups
  | Other_op of string
      (** The type for event operations. [Other_op value] preserves an unknown
          wire spelling [value]. Known operations that are invalid for a
          particular event family are rejected by {!decode}. *)

type change = [ `Add | `Remove | `Other of string ]
(** The type for reaction and message-flag changes. [`Other value] preserves an
    unknown wire spelling [value]. *)

val change_to_string : change -> string
(** [change_to_string change] is the wire spelling of [change]. *)

type error = private {
  event_type : Event_type.t;
  path : string list;
  message : string;
  cause : Jsont.Error.t option;
}
(** The type for malformed known event payloads. [event_type] identifies the
    payload family. [path] lists containing object members from outermost to
    innermost. [message] describes the violation. [cause] retains the
    location-aware codec error when one caused the violation. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] writes the event family, member path, and diagnostic
    message of [error] to [ppf]. *)

val error_to_string : error -> string
(** [error_to_string error] is the diagnostic produced by {!pp_error}. *)

type message = private {
  message : Message.t;
  flags : Message_flag.t list;
  raw : Jsont.json;
}
(** The payload of a [message] event. [flags] comes from the top-level event
    member when present and otherwise from [message]. *)

type message_edit = private {
  message_id : Id.Message.t;
  message_ids : Id.Message.t list;
  user_id : Id.User.t option;
  edit_timestamp : float option;
  content : string option;
  rendered_content : string option;
  channel_id : Id.Channel.t option;
  new_channel_id : Id.Channel.t option;
  topic : string option;
  raw : Jsont.json;
}
(** The payload of an [update_message] event. [edit_timestamp] is Unix time in
    seconds. [channel_id], [new_channel_id], and [topic] correspond to the wire
    members [stream_id], [new_stream_id], and [subject]. [message_ids] defaults
    to the singleton [message_id] when its member is absent or empty. *)

type message_delete = private {
  message_ids : Id.Message.t list;
  message_type : Message_type.t option;
  raw : Jsont.json;
}
(** The payload of a [delete_message] event. A singular [message_id] is used
    when [message_ids] is absent or empty. At least one identifier is required.
*)

type reaction = private {
  op : change;
  message_id : Id.Message.t;
  user_id : Id.User.t;
  emoji_name : string;
  emoji_code : string option;
  reaction_type : string option;
  raw : Jsont.json;
}
(** The payload of a [reaction] event. The message, user, operation, and emoji
    name are required. Unknown operation spellings remain in [op]. *)

type message_flags = private {
  op : change;
  flag : Message_flag.t;
  message_ids : Id.Message.t list;
  all : bool option;
  raw : Jsont.json;
}
(** The payload of an [update_message_flags] event. [message_ids] is decoded
    from the required [messages] member. Unknown operation and flag spellings
    remain representable. *)

type realm_user = private {
  op : op;
  user_id : Id.User.t;
  full_name : string option;
  email : string option;
  new_email : string option;
  is_bot : bool option;
  added_user : User.t option;
  person : Jsont.json;
  raw : Jsont.json;
}
(** The payload of a [realm_user] event. [person] is the complete required user
    change object. [added_user] is its decoded user for [Add] operations and is
    [None] for other operations. *)

type channel = private {
  op : op;
  channel_ids : Id.Channel.t list;
  channels : Channel.t list;
  channel_id : Id.Channel.t option;
  property : string option;
  value : Jsont.json option;
  raw : Jsont.json;
}
(** The payload of a [stream] event. [channel_ids] combines the listed channel
    identifiers with a singular [stream_id] when present. Identifiers from
    [stream_ids] take precedence over identifiers extracted from [streams].
    [channels] contains decoded [streams] only for [Create] operations. [value]
    preserves a present JSON null as [Some json]. *)

type subscription = private {
  op : op;
  channel_ids : Id.Channel.t list;
  user_ids : Id.User.t list;
  subscriptions : Channel.Subscription.t list;
  property : string option;
  value : Jsont.json option;
  raw : Jsont.json;
}
(** The payload of a [subscription] event. [channel_ids] combines listed and
    singular channel identifiers. Identifiers from [stream_ids] take precedence
    over identifiers extracted from [subscriptions]. [subscriptions] contains
    decoded objects only for [Add] operations. [value] preserves a present JSON
    null as [Some json]. *)

type user_status = private {
  user_id : Id.User.t;
  away : bool option;
  status_text : string option;
  emoji_name : string option;
  emoji_code : string option;
  reaction_type : string option;
  raw : Jsont.json;
}
(** The payload of a [user_status] event. Only [user_id] is required. *)

type topic = private {
  channel_id : Id.Channel.t;
  topic_name : string;
  last_updated : float;
  visibility_policy : Topic_visibility.t;
  raw : Jsont.json;
}
(** The payload of a [user_topic] event. [last_updated] is Unix time in seconds.
    Unknown integer visibility policies remain representable. *)

type user_group = private {
  op : op;
  group_id : Id.User_group.t;
  name : string option;
  user_ids : Id.User.t list;
  direct_subgroup_ids : Id.User_group.t list;
  data : Jsont.json option;
  raw : Jsont.json;
}
(** The payload of a [user_group] event. Add operations may provide the group
    identifier, name, members, and direct subgroups inside [group]. Other forms
    use top-level identifiers and lists. Missing lists decode as empty lists.
    [data] preserves a present JSON null as [Some json]. *)

type presence = private {
  user_id : Id.User.t option;
  email : string option;
  server_timestamp : float option;
  active_timestamp : float option;
  idle_timestamp : float option;
  presence : Jsont.json option;
  presences : Jsont.json option;
  raw : Jsont.json;
}
(** The payload of a [presence] event. Timestamps are Unix time in seconds.
    [active_timestamp] and [idle_timestamp] are read from the singular
    [presence] object. The event must contain either [user_id] or [presences].
    Raw presence values, including JSON null, are retained when present. *)

type unknown = private { event_type : Event_type.t; raw : Jsont.json }
(** The payload of an unsupported or future event family. [raw] is retained
    without family-specific validation and need not be a JSON object. *)

type t =
  | Message of message
  | Message_edit of message_edit
  | Message_delete of message_delete
  | Reaction of reaction
  | Message_flags of message_flags
  | Realm_user of realm_user
  | Channel of channel
  | Subscription of subscription
  | User_status of user_status
  | Topic of topic
  | User_group of user_group
  | Presence of presence
  | Unknown of unknown  (** The type for typed event payloads. *)

val op_of_string : string -> op
(** [op_of_string value] is the operation encoded by [value]. Unknown values
    produce [Other_op value]. *)

val op_to_string : op -> string
(** [op_to_string op] is the wire spelling of [op]. *)

val raw : t -> Jsont.json
(** [raw payload] is the complete JSON value supplied when [payload] was
    decoded. *)

val decode : Event_type.t -> Jsont.json -> (t, error) result
(** [decode event_type json] is the typed payload for [json]. Malformed payloads
    of supported [event_type] values produce [Error error]. Unsupported and
    future event types produce [Unknown] without validating [json]. *)

val of_event : Event.t -> (t, error) result
(** [of_event event] is the result of decoding {!Event.val-data} using
    {!Event.val-type_}. *)
