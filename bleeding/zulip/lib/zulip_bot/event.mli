(** Events presented to bot handlers.

    Protocol events are projected into typed handler records. Supported events
    with invalid payloads become malformed events. Unsupported valid events
    retain their raw JSON as custom events. *)

type envelope = private {
  room : Room.t;
  sender : Zulip.Id.User.t;
  event_id : Zulip.Id.Event.t option;
}
(** The type for routing shared by events that have a room and sender.
    [event_id] is absent for messages created by an external adapter. *)

type message = private {
  envelope : envelope;
  message : Zulip.Message.t;
  flags : Zulip.Message_flag.t list;
  body : string;
}
(** The type for message events. [message] is the source protocol message.
    [flags] are typed message flags. [body] is the text presented to handlers
    and may differ from the source content after adapter normalization. *)

type command = private {
  message : message;
  name : string;
  args : string;
  argv : string list;
}
(** The type for parsed command events. [args] is the trimmed text after the
    command name. [argv] splits [args] on ASCII whitespace without quoting or
    escape processing. *)

type edit = private {
  envelope : envelope;
  message_id : Zulip.Id.Message.t;
  raw : Jsont.json;
}
(** The type for message-edit events with a resolved source room and sender.
    [raw] is the original protocol payload. *)

type reaction = private {
  envelope : envelope;
  message_id : Zulip.Id.Message.t;
  raw : Jsont.json;
}
(** The type for reaction events with a resolved source room and sender. [raw]
    is the original protocol payload. *)

type delete = private {
  room : Room.t option;
  event_id : Zulip.Id.Event.t;
  message_ids : Zulip.Id.Message.t list;
  raw : Jsont.json;
}
(** The type for message-deletion events. [room] is present when the destination
    of at least one deleted message is cached. [raw] is the original protocol
    payload. *)

type custom = {
  envelope : envelope option;
  event_type : string;
  raw : Jsont.json;
}
(** The type for supported adapter extensions and valid protocol events without
    a dedicated handler record. [envelope] is optional adapter-supplied routing.
*)

type sync =
  | Connecting
  | Live
  | Recovering of Zulip_eio.Error.t
  | Stopped  (** The type for collector state notifications. *)

type malformed = private {
  event_type : string;
  raw : Jsont.json;
  error : Zulip.Event_payload.error;
}
(** The type for known protocol events whose payload could not be decoded.
    [error] preserves the structured payload error. *)

type t =
  | Message of message
  | Command of command
  | Edit of edit
  | Reaction of reaction
  | Delete of delete
  | Custom of custom
  | Malformed of malformed
  | Sync of sync  (** The type for events dispatched to bot handlers. *)

val envelope : t -> envelope option
(** [envelope event] is the routing envelope of [event], if it has one. Deletion
    events do not expose an envelope. *)

val room : t -> Room.t option
(** [room event] is the conversation associated with [event], if known. *)

val sender : t -> Zulip.Id.User.t option
(** [sender event] is the sender in the envelope of [event], if present. *)

val room_key : t -> string option
(** [room_key event] is the stable conversation key of [event], if its room is
    known. *)

val reply : envelope -> string -> Sent.t
(** [reply envelope content] enqueues [content] for the reply destination in
    [envelope]. It can block until the context send queue admits the request. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf event] prints the kind of [event] on [ppf]. *)

val argv : string -> string list
(** [argv text] is [text] split on spaces, tabs, carriage returns, and newlines.
    Empty words are omitted. Quotes and escapes have no special meaning. *)

val command : prefix:string -> message -> command option
(** [command ~prefix message] is the command parsed from [message] when its
    trimmed body starts with [prefix] and contains a command name. It is [None]
    otherwise. An empty [prefix] accepts the first word of every nonempty body.
*)

val of_zulip : Context.t -> Zulip.Event.t -> t option
(** [of_zulip context event] is the handler event projected from [event]. It
    updates the context's identity, bot classification, and bounded history of
    1024 message-destination observations as required by the decoded payload.
    Heartbeats produce [None]. Malformed supported payloads produce [Malformed].
    Valid unsupported payloads and supported events lacking routing data produce
    [Custom].

    Edit and reaction projection can fetch a message when its destination is
    absent from the bounded cache. A failed fetch produces [Custom]. *)

val of_message :
  Context.t -> ?flags:Zulip.Message_flag.t list -> Zulip.Message.t -> message
(** [of_message context ~flags message] is an adapter message with no protocol
    event identifier. [flags] defaults to the flags in [message]. It records the
    message destination in [context] for later edit, reaction, and deletion
    routing. *)

val payload :
  custom -> (Zulip.Event_payload.t, Zulip.Event_payload.error) result
(** [payload custom] is the supported protocol payload decoded from [custom].
    Future event kinds retain their raw payload through [Unknown]. *)

val with_body : message -> body:string -> message
(** [with_body message ~body] is [message] with [body] as its handler-visible
    text. It retains the source message, flags, and routing. Adapters use it to
    normalize input while keeping the original message available. *)
