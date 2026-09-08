(** Messages, reactions, receipts, and personal flags.

    Message content supplied to mutation endpoints is Zulip-flavored Markdown.
    Endpoint functions return {!Error.t} for local validation, Zulip API, HTTP,
    JSON, transport, and timeout failures. Eio cancellation propagates. *)

type anchor =
  | Newest
  | Oldest
  | First_unread
  | Date of string
      (** The first message at or after an ISO 8601 date or datetime. *)
  | Message_id of Zulip.Id.Message.t
      (** The type for positions around which Zulip selects messages. *)

type propagate_mode =
  | Change_one
  | Change_later
  | Change_all
      (** The type for selecting which messages in a topic an edit affects. *)

type emoji_type =
  | Unicode_emoji
  | Realm_emoji
  | Zulip_extra_emoji  (** The type for Zulip emoji namespaces. *)

type page = {
  messages : Zulip.Message.t list;
  anchor : Zulip.Id.Message.t option;
      (** The message identifier selected by Zulip as the anchor, if any. *)
  found_oldest : bool;
      (** Whether the result reaches the oldest message matching the narrow. *)
  found_newest : bool;
      (** Whether the result reaches the newest message matching the narrow. *)
  found_anchor : bool;
      (** Whether the requested anchor exists and matches the narrow. *)
  history_limited : bool;
      (** Whether history access restrictions limited the result. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for message-page responses. *)

val send_detailed :
  Client.t ->
  type_:[ `Channel | `Direct ] ->
  to_:string ->
  ?topic:string ->
  ?queue_id:string ->
  ?local_id:string ->
  ?read_by_sender:bool ->
  content:string ->
  unit ->
  (Zulip.Message_response.t, Error.t) result
(** [send_detailed client ~type_ ~to_ ~topic ~queue_id ~local_id ~read_by_sender
     ~content ()] is Zulip's structured response after sending [content]. [to_]
    is a channel name or identifier for [`Channel]. It is the JSON-encoded
    recipient list accepted by Zulip for [`Direct]. [topic] is required for a
    channel message and forbidden for a direct message. [read_by_sender]
    defaults to [true]. [queue_id] and [local_id] are omitted by default and
    must be supplied together for local echo. Invalid combinations return
    {!Error.t.constructor-Invalid_request} without making a request. *)

val send :
  Client.t ->
  type_:[ `Channel | `Direct ] ->
  to_:string ->
  ?topic:string ->
  ?queue_id:string ->
  ?local_id:string ->
  ?read_by_sender:bool ->
  content:string ->
  unit ->
  (Zulip.Id.Message.t, Error.t) result
(** [send client ~type_ ~to_ ~topic ~queue_id ~local_id ~read_by_sender ~content
     ()] is the identifier of the sent message. Its addressing, local-echo, and
    validation rules are those of {!send_detailed}. *)

val send_channel :
  Client.t ->
  channel:string ->
  topic:string ->
  ?queue_id:string ->
  ?local_id:string ->
  ?read_by_sender:bool ->
  content:string ->
  unit ->
  (Zulip.Id.Message.t, Error.t) result
(** [send_channel client ~channel ~topic ~queue_id ~local_id ~read_by_sender
     ~content ()] is the identifier of the message sent to [channel] under
    [topic]. [read_by_sender] defaults to [true]. [queue_id] and [local_id] are
    omitted by default and must be supplied together for local echo. *)

val send_channel_id :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  ?queue_id:string ->
  ?local_id:string ->
  ?read_by_sender:bool ->
  content:string ->
  unit ->
  (Zulip.Id.Message.t, Error.t) result
(** [send_channel_id client ~channel_id ~topic ~queue_id ~local_id
     ~read_by_sender ~content ()] is the identifier of the message sent to
    [channel_id] under [topic]. [read_by_sender] defaults to [true]. [queue_id]
    and [local_id] are omitted by default and must be supplied together for
    local echo. *)

val send_direct :
  Client.t ->
  recipients:Zulip.Id.User.t list ->
  ?queue_id:string ->
  ?local_id:string ->
  ?read_by_sender:bool ->
  content:string ->
  unit ->
  (Zulip.Id.Message.t, Error.t) result
(** [send_direct client ~recipients ~queue_id ~local_id ~read_by_sender ~content
     ()] is the identifier of the direct message sent to [recipients].
    [read_by_sender] defaults to [true]. [queue_id] and [local_id] are omitted
    by default and must be supplied together for local echo. *)

type raw_content_result = {
  raw_content : string;  (** The original Zulip-flavored Markdown content. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed raw-content responses. *)

val get_raw_detailed :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  ?apply_markdown:bool ->
  ?allow_empty_topic_name:bool ->
  unit ->
  (raw_content_result, Error.t) result
(** [get_raw_detailed client ~message_id ~apply_markdown ~allow_empty_topic_name
     ()] is the detailed response containing the original Markdown for
    [message_id]. [apply_markdown] defaults to [true] and controls whether the
    message content retained in [raw] is rendered HTML. [allow_empty_topic_name]
    is omitted by default, which uses Zulip's default of replacing an empty
    topic with the organization's display name. *)

val get_raw :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  ?apply_markdown:bool ->
  ?allow_empty_topic_name:bool ->
  unit ->
  (string, Error.t) result
(** [get_raw client ~message_id ~apply_markdown ~allow_empty_topic_name ()] is
    the original Zulip-flavored Markdown for [message_id]. The optional
    arguments affect other fields in Zulip's response and have the defaults
    described by {!get_raw_detailed}. *)

val get :
  Client.t -> message_id:Zulip.Id.Message.t -> (Zulip.Message.t, Error.t) result
(** [get client ~message_id] is the complete message identified by [message_id],
    with rendered HTML content. It returns
    {!Error.t.constructor-Invalid_request} if Zulip does not include the
    requested message in its response. *)

val get_messages :
  Client.t ->
  ?anchor:anchor ->
  ?num_before:int ->
  ?num_after:int ->
  ?narrow:Zulip.Narrow.t list ->
  ?include_anchor:bool ->
  ?client_gravatar:bool ->
  ?apply_markdown:bool ->
  ?use_first_unread_anchor:bool ->
  ?message_ids:Zulip.Id.Message.t list ->
  ?allow_empty_topic_name:bool ->
  unit ->
  (page, Error.t) result
(** [get_messages client ~anchor ~num_before ~num_after ~narrow ~include_anchor
     ~client_gravatar ~apply_markdown ~use_first_unread_anchor ~message_ids
     ~allow_empty_topic_name ()] is a page of messages selected by the supplied
    range or identifiers. A range query requires nonnegative [num_before] and
    [num_after]. Its [anchor] defaults to [Newest] when neither [anchor] nor
    [use_first_unread_anchor] is supplied. Supplying [use_first_unread_anchor]
    without [anchor] uses Zulip's legacy anchor selection. Modern callers select
    [First_unread] explicitly. The value carried by [Date] is an ISO 8601 date
    or datetime interpreted by Zulip. [include_anchor] defaults on the server to
    [true]. A [message_ids] query cannot include [anchor], either count,
    [include_anchor], or [use_first_unread_anchor]. Supplying [anchor] with
    [use_first_unread_anchor] is also invalid. These invalid combinations return
    {!Error.t.constructor-Invalid_request} without making a request. Omitting
    [narrow] selects the authenticated user's combined feed. [client_gravatar]
    and [apply_markdown] default on the server to [true].
    [allow_empty_topic_name] defaults on the server to replacing an empty topic
    with the organization's display name. *)

type narrow_match = {
  message_id : Zulip.Id.Message.t;
  match_content : string;
      (** Content annotated with the narrow's search matches. *)
  match_topic : string;  (** Topic text annotated with search matches. *)
}
(** The type for a message that matches a narrow. *)

type narrow_match_result = {
  matches : narrow_match list;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed narrow-match responses. *)

val check_messages_match_narrow_detailed :
  Client.t ->
  message_ids:Zulip.Id.Message.t list ->
  narrow:Zulip.Narrow.t list ->
  (narrow_match_result, Error.t) result
(** [check_messages_match_narrow_detailed client ~message_ids ~narrow] is the
    detailed response describing which [message_ids] match [narrow]. *)

val check_messages_match_narrow :
  Client.t ->
  message_ids:Zulip.Id.Message.t list ->
  narrow:Zulip.Narrow.t list ->
  (narrow_match list, Error.t) result
(** [check_messages_match_narrow client ~message_ids ~narrow] is the subset of
    [message_ids] that match [narrow]. *)

type history = {
  edits : Zulip.Message.edit list;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for message edit-history responses. *)

val get_history :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  ?allow_empty_topic_name:bool ->
  unit ->
  (history, Error.t) result
(** [get_history client ~message_id ~allow_empty_topic_name ()] is the edit
    history for [message_id]. [allow_empty_topic_name] is omitted by default,
    which uses Zulip's default of replacing an empty topic with the
    organization's display name. Zulip may disable access to edit history for an
    organization. *)

type edit_result = {
  detached_uploads : Attachments.t list;
      (** Uploads no longer referenced after the edit. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed message-edit responses. *)

val edit_detailed :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  ?content:string ->
  ?topic:string ->
  ?channel_id:Zulip.Id.Channel.t ->
  ?propagate_mode:propagate_mode ->
  ?send_notification_to_old_thread:bool ->
  ?send_notification_to_new_thread:bool ->
  ?prev_content_sha256:string ->
  unit ->
  (edit_result, Error.t) result
(** [edit_detailed client ~message_id ~content ~topic ~channel_id
     ~propagate_mode ~send_notification_to_old_thread
     ~send_notification_to_new_thread ~prev_content_sha256 ()] is the detailed
    response after editing [message_id]. Omitted content, topic, and channel
    fields remain unchanged. [propagate_mode] defaults on the server to
    [Change_one]. Old-thread notifications default on the server to [false].
    New-thread notifications default on the server to [true].
    [prev_content_sha256] is omitted by default. When supplied, Zulip rejects
    the edit if it differs from the SHA-256 hash of the stored Markdown.
    Supplying none of [content], [topic], and [channel_id] returns
    {!Error.t.constructor-Invalid_request}. Zulip rejects simultaneous content
    and channel changes and enforces the organization's edit and move
    permissions. *)

val edit :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  ?content:string ->
  ?topic:string ->
  ?channel_id:Zulip.Id.Channel.t ->
  ?propagate_mode:propagate_mode ->
  ?send_notification_to_old_thread:bool ->
  ?send_notification_to_new_thread:bool ->
  ?prev_content_sha256:string ->
  unit ->
  (unit, Error.t) result
(** [edit client ~message_id ~content ~topic ~channel_id ~propagate_mode
     ~send_notification_to_old_thread ~send_notification_to_new_thread
     ~prev_content_sha256 ()] updates [message_id] and discards the detailed
    response. Its defaults and validation rules are those of {!edit_detailed}.
*)

val delete : Client.t -> message_id:Zulip.Id.Message.t -> (unit, Error.t) result
(** [delete client ~message_id] permanently deletes [message_id]. Zulip requires
    content access and enforces the organization's message deletion permissions.
*)

val update_flags :
  Client.t ->
  messages:Zulip.Id.Message.t list ->
  op:Zulip.Message_flag.op ->
  flag:Zulip.Message_flag.modifiable ->
  (unit, Error.t) result
(** [update_flags client ~messages ~op ~flag] applies [op] for [flag] to
    [messages] and discards Zulip's per-message result. *)

type flag_result = {
  messages : Zulip.Id.Message.t list;
      (** The message identifiers for which Zulip changed the flag. *)
  ignored_because_not_subscribed_channels : Zulip.Id.Channel.t list;
      (** Channels ignored because the authenticated user is not subscribed. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed message-flag responses. *)

val update_flags_detailed :
  Client.t ->
  messages:Zulip.Id.Message.t list ->
  op:Zulip.Message_flag.op ->
  flag:Zulip.Message_flag.modifiable ->
  (flag_result, Error.t) result
(** [update_flags_detailed client ~messages ~op ~flag] is the detailed response
    after applying [op] for [flag] to [messages]. *)

type narrow_flag_result = {
  processed_count : int;
      (** The number of messages examined in the bounded range. *)
  updated_count : int;  (** The number of messages whose flag changed. *)
  first_processed_id : Zulip.Id.Message.t option;
      (** The first examined message identifier, if a message was examined. *)
  last_processed_id : Zulip.Id.Message.t option;
      (** The last examined message identifier, if a message was examined. *)
  found_oldest : bool;
      (** Whether the range reached the oldest message matching the narrow. *)
  found_newest : bool;
      (** Whether the range reached the newest message matching the narrow. *)
  ignored_because_not_subscribed_channels : Zulip.Id.Channel.t list;
      (** Channels ignored because the authenticated user is not subscribed. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for flag updates over a narrow. *)

val update_flags_for_narrow :
  Client.t ->
  anchor:anchor ->
  num_before:int ->
  num_after:int ->
  narrow:Zulip.Narrow.t list ->
  ?include_anchor:bool ->
  op:Zulip.Message_flag.op ->
  flag:Zulip.Message_flag.modifiable ->
  unit ->
  (narrow_flag_result, Error.t) result
(** [update_flags_for_narrow client ~anchor ~num_before ~num_after ~narrow
     ~include_anchor ~op ~flag ()] is the detailed result of applying [op] for
    [flag] to the bounded range around [anchor] that matches [narrow].
    [include_anchor] defaults on the server to [true]. Zulip may reduce the
    requested bounds to limit transaction size. [Date] and negative bounds
    return {!Error.t.constructor-Invalid_request} without making a request. *)

val get_read_receipts :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  (Zulip.Id.User.t list, Error.t) result
(** [get_read_receipts client ~message_id] is the list of users who marked
    [message_id] as read and permit sharing that status. It excludes the message
    sender. *)

type read_receipts_result = {
  user_ids : Zulip.Id.User.t list;
      (** Users who marked the message as read and permit sharing that status.
      *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed read-receipt responses. *)

val get_read_receipts_detailed :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  (read_receipts_result, Error.t) result
(** [get_read_receipts_detailed client ~message_id] is the detailed read-receipt
    response for [message_id]. The user list excludes the message sender and
    users whose privacy settings prohibit sharing read status. *)

val report :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  report_type:string ->
  ?description:string ->
  unit ->
  (unit, Error.t) result
(** [report client ~message_id ~report_type ~description ()] reports
    [message_id] to the organization's configured moderation channel.
    [report_type] must be a key advertised by Zulip. [description] is omitted by
    default and is required when [report_type] is ["other"]. Zulip rejects
    reports when no moderation channel is configured. *)

val mark_all_as_read : Client.t -> (unit, Error.t) result
(** [mark_all_as_read client] marks the authenticated user's messages as read.
    Zulip may complete only one batch before a timeout while still returning a
    successful response, which this operation discards. Repeated use of
    {!update_flags_for_narrow} is the supported replacement. *)

val mark_channel_as_read :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (unit, Error.t) result
(** [mark_channel_as_read client ~channel_id] marks every message in
    [channel_id] as read for the authenticated user. This deprecated Zulip
    operation is replaced by {!update_flags_for_narrow}. *)

val mark_topic_as_read :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  (unit, Error.t) result
(** [mark_topic_as_read client ~channel_id ~topic] marks every message under
    [topic] in [channel_id] as read for the authenticated user. This deprecated
    Zulip operation is replaced by {!update_flags_for_narrow}. *)

val add_reaction :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  emoji_name:string ->
  ?emoji_code:string ->
  ?reaction_type:emoji_type ->
  unit ->
  (unit, Error.t) result
(** [add_reaction client ~message_id ~emoji_name ~emoji_code ~reaction_type ()]
    adds the authenticated user's reaction to [message_id]. [emoji_code] and
    [reaction_type] are omitted by default, which lets Zulip resolve
    [emoji_name]. They should both be copied from an existing reaction when
    matching its exact emoji version or namespace. *)

val remove_reaction :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  emoji_name:string ->
  ?emoji_code:string ->
  ?reaction_type:emoji_type ->
  unit ->
  (unit, Error.t) result
(** [remove_reaction client ~message_id ~emoji_name ~emoji_code ~reaction_type
     ()] removes the authenticated user's matching reaction from [message_id].
    [emoji_code] and [reaction_type] are omitted by default, which lets Zulip
    resolve [emoji_name]. They should both be copied from an existing reaction
    when matching its exact emoji version or namespace. *)

type render_result = {
  rendered : string;  (** The content rendered as HTML. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed Markdown-rendering responses. *)

val render_detailed :
  Client.t -> content:string -> (render_result, Error.t) result
(** [render_detailed client ~content] is the detailed response after rendering
    the Zulip-flavored Markdown [content] as HTML. *)

val render : Client.t -> content:string -> (string, Error.t) result
(** [render client ~content] is the HTML rendering of the Zulip-flavored
    Markdown [content]. *)

val move_topic :
  Client.t ->
  channel:string ->
  new_channel:string ->
  topic:string ->
  ?new_topic:string ->
  ?message_id:Zulip.Id.Message.t ->
  ?propagate_mode:propagate_mode ->
  ?notify_old_topic:bool ->
  ?notify_new_topic:bool ->
  unit ->
  (unit, Error.t) result
(** [move_topic client ~channel ~new_channel ~topic ~new_topic ~message_id
     ~propagate_mode ~notify_old_topic ~notify_new_topic ()] moves messages from
    [topic] in [channel] to [new_channel]. Omitting [new_topic] retains the
    topic name. [propagate_mode] defaults to [Change_all]. [notify_old_topic]
    and [notify_new_topic] default to [true]. [message_id] identifies the first
    affected message. It may be omitted for [Change_all], in which case the
    newest message in the topic is resolved first. Omitting it for another mode
    or moving an empty topic returns {!Error.t.constructor-Invalid_request}.
    Zulip enforces the organization's message-move permissions. *)
