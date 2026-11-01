@@ portable

(** Zulip channels and subscriptions.

    Zulip calls channels streams in some API paths and wire members. Channel
    values preserve unrecognized object members when decoded and re-encoded. *)

module Topics_policy : sig
  type t =
    | Inherit
    | Allow_empty_topic
    | Disable_empty_topic
    | Empty_topic_only
    | Other of string
        (** The type for channel topic policies. [Other value] preserves the
            unknown wire spelling [value]. [Empty_topic_only] is valid only when
            every existing message in the channel has the empty topic. *)

  val to_string : t -> string
  (** [to_string policy] is the wire spelling of [policy]. *)

  val of_string : string -> t
  (** [of_string value] is the topic policy encoded by [value]. Unknown values
      produce [Other value]. *)

  val jsont : t Jsont.t
  (** [jsont] is a string codec for topic policies. Unknown spellings decode as
      [Other value] and encode unchanged. *)
end

module Posting_policy : sig
  type t =
    | Everyone
    | Administrators
    | Full_members
    | Moderators
    | Other of int
        (** The type for legacy channel posting policies. The known constructors
            have wire values [1] through [4]. [Other n] preserves the unknown
            wire value [n]. *)

  val of_int : int -> t
  (** [of_int n] is the posting policy represented by [n]. Unknown values
      produce [Other n]. *)

  val to_int : t -> int
  (** [to_int policy] is the wire value of [policy]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] have equal wire values. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by their wire values. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf policy] writes the decimal wire value of [policy] to [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is a codec for exactly representable integer posting policies.
      Unknown integers decode as [Other n] and encode unchanged. *)
end

type t
(** The type for channels. *)

val create :
  name:string ->
  ?stream_id:Id.Channel.t ->
  ?description:string ->
  ?rendered_description:string ->
  ?invite_only:bool ->
  ?is_web_public:bool ->
  ?history_public_to_subscribers:bool ->
  ?is_default:bool ->
  ?message_retention_days:int option ->
  ?first_message_id:Id.Message.t ->
  ?date_created:float ->
  ?creator_id:Id.User.t ->
  ?stream_post_policy:Posting_policy.t ->
  ?is_archived:bool ->
  ?topics_policy:Topics_policy.t ->
  ?folder_id:Id.Channel_folder.t option ->
  ?is_recently_active:bool ->
  ?is_announcement_only:bool ->
  ?subscriber_count:int ->
  ?stream_weekly_traffic:int option ->
  ?can_add_subscribers_group:Group_setting.t ->
  ?can_remove_subscribers_group:Group_setting.t ->
  ?can_administer_channel_group:Group_setting.t ->
  ?can_delete_any_message_group:Group_setting.t ->
  ?can_delete_own_message_group:Group_setting.t ->
  ?can_move_messages_out_of_channel_group:Group_setting.t ->
  ?can_move_messages_within_channel_group:Group_setting.t ->
  ?can_send_message_group:Group_setting.t ->
  ?can_subscribe_group:Group_setting.t ->
  ?can_resolve_topics_group:Group_setting.t ->
  ?can_create_topic_group:Group_setting.t ->
  ?extensions:Jsont.json ->
  unit ->
  t
(** [create ~name ()] is a channel named [name]. [description] defaults to the
    empty string. [invite_only], [is_web_public], [is_default], and
    [is_archived] default to [false]. [history_public_to_subscribers] defaults
    to [true]. [stream_post_policy] defaults to [Posting_policy.Everyone]. All
    other optional arguments default to absence. Passing [None] explicitly for
    [message_retention_days], [folder_id], or [stream_weekly_traffic] records a
    present JSON null. [extensions] defaults to an empty object and supplies
    unrecognized object members.

    @raise Stdlib.exception-Invalid_argument
      if [extensions] is not a JSON object. *)

val name : t -> string
(** [name channel] is the channel's display name. *)

val stream_id : t -> Id.Channel.t option
(** [stream_id channel] is the channel identifier, or [None] if it is absent. *)

val description : t -> string
(** [description channel] is the channel description in Markdown. A missing wire
    member decodes as the empty string. *)

val rendered_description : t -> string option
(** [rendered_description channel] is the rendered channel description, or
    [None] if it is absent. *)

val invite_only : t -> bool
(** [invite_only channel] is [true] if the channel is private. A missing wire
    member decodes as [false]. *)

val is_web_public : t -> bool
(** [is_web_public channel] is [true] if channel history is publicly available
    without authentication. A missing wire member decodes as [false]. *)

val history_public_to_subscribers : t -> bool
(** [history_public_to_subscribers channel] is [true] if new subscribers may
    read messages sent before they subscribed. A missing wire member decodes as
    [true]. *)

val is_default : t -> bool
(** [is_default channel] is [true] if new users subscribe automatically. A
    missing wire member decodes as [false]. *)

val message_retention_days : t -> int option option
(** [message_retention_days channel] distinguishes an absent retention member as
    [None], JSON null as [Some None], and an explicit day count [days] as
    [Some (Some days)]. *)

val first_message_id : t -> Id.Message.t option
(** [first_message_id channel] is the identifier of the oldest message that is
    available, or [None] if the wire member is absent or null. *)

val date_created : t -> float option
(** [date_created channel] is the channel creation time as Unix time in seconds,
    or [None] if it is absent. *)

val creator_id : t -> Id.User.t option
(** [creator_id channel] is the creating user's identifier, or [None] if the
    wire member is absent or null. *)

val stream_post_policy : t -> Posting_policy.t
(** [stream_post_policy channel] is the channel's legacy posting policy. A
    missing wire member decodes as [Posting_policy.Everyone]. *)

val is_archived : t -> bool
(** [is_archived channel] is [true] if the channel is archived. A missing wire
    member decodes as [false]. *)

val topics_policy : t -> Topics_policy.t option
(** [topics_policy channel] is the channel's empty-topic policy, or [None] if it
    is absent. *)

val folder_id : t -> Id.Channel_folder.t option option
(** [folder_id channel] distinguishes an absent folder member as [None], JSON
    null as [Some None], and a folder identifier [id] as [Some (Some id)]. *)

val is_recently_active : t -> bool option
(** [is_recently_active channel] is the server's recent-activity state, or
    [None] if it is absent. *)

val is_announcement_only : t -> bool option
(** [is_announcement_only channel] is the server's announcement-only state, or
    [None] if it is absent. *)

val subscriber_count : t -> int option
(** [subscriber_count channel] is the number of subscribers, or [None] if it is
    absent. *)

val stream_weekly_traffic : t -> int option option
(** [stream_weekly_traffic channel] distinguishes an absent traffic member as
    [None], JSON null as [Some None], and an explicit count [count] as
    [Some (Some count)]. *)

val can_add_subscribers_group : t -> Group_setting.t option
(** [can_add_subscribers_group channel] is the group allowed to add subscribers,
    or [None] if it is absent. *)

val can_remove_subscribers_group : t -> Group_setting.t option
(** [can_remove_subscribers_group channel] is the group allowed to remove
    subscribers, or [None] if it is absent. *)

val can_administer_channel_group : t -> Group_setting.t option
(** [can_administer_channel_group channel] is the group allowed to administer
    the channel, or [None] if it is absent. *)

val can_delete_any_message_group : t -> Group_setting.t option
(** [can_delete_any_message_group channel] is the group allowed to delete any
    message, or [None] if it is absent. *)

val can_delete_own_message_group : t -> Group_setting.t option
(** [can_delete_own_message_group channel] is the group allowed to delete its
    own messages, or [None] if it is absent. *)

val can_move_messages_out_of_channel_group : t -> Group_setting.t option
(** [can_move_messages_out_of_channel_group channel] is the group allowed to
    move messages out of the channel, or [None] if it is absent. *)

val can_move_messages_within_channel_group : t -> Group_setting.t option
(** [can_move_messages_within_channel_group channel] is the group allowed to
    move messages within the channel, or [None] if it is absent. *)

val can_send_message_group : t -> Group_setting.t option
(** [can_send_message_group channel] is the group allowed to send messages, or
    [None] if it is absent. *)

val can_subscribe_group : t -> Group_setting.t option
(** [can_subscribe_group channel] is the group allowed to subscribe, or [None]
    if it is absent. *)

val can_resolve_topics_group : t -> Group_setting.t option
(** [can_resolve_topics_group channel] is the group allowed to resolve topics,
    or [None] if it is absent. *)

val can_create_topic_group : t -> Group_setting.t option
(** [can_create_topic_group channel] is the group allowed to create topics, or
    [None] if it is absent. *)

val extensions : t -> Jsont.json
(** [extensions channel] is an object containing the unrecognized wire members
    preserved for [channel]. *)

val raw : t -> Jsont.json
(** [raw channel] is the complete JSON object encoded from [channel], including
    its extension members.

    @raise Jsont.exception-Error if a member cannot be encoded. *)

module Subscription : sig
  type channel := t

  type t
  (** The type for a user's subscription to a channel. *)

  val channel : t -> channel
  (** [channel subscription] is the channel described by [subscription]. *)

  val color : t -> string option
  (** [color subscription] is the channel color, or [None] if it is absent. *)

  val is_muted : t -> bool
  (** [is_muted subscription] is [true] if the channel is muted. A missing wire
      member decodes as [false]. *)

  val in_home_view : t -> bool option
  (** [in_home_view subscription] is the home-view inclusion state, or [None] if
      it is absent. *)

  val pin_to_top : t -> bool
  (** [pin_to_top subscription] is [true] if the channel is pinned. A missing
      wire member decodes as [false]. *)

  val desktop_notifications : t -> bool option
  (** [desktop_notifications subscription] is the desktop notification override,
      or [None] if the wire member is absent or null. *)

  val audible_notifications : t -> bool option
  (** [audible_notifications subscription] is the audible notification override,
      or [None] if the wire member is absent or null. *)

  val push_notifications : t -> bool option
  (** [push_notifications subscription] is the push notification override, or
      [None] if the wire member is absent or null. *)

  val email_notifications : t -> bool option
  (** [email_notifications subscription] is the email notification override, or
      [None] if the wire member is absent or null. *)

  val wildcard_mentions_notify : t -> bool option
  (** [wildcard_mentions_notify subscription] is the wildcard-mention
      notification override, or [None] if the wire member is absent or null. *)

  val subscribers : t -> Id.User.t list option
  (** [subscribers subscription] is the complete subscriber list, or [None] if
      it is absent. *)

  val partial_subscribers : t -> Id.User.t list option
  (** [partial_subscribers subscription] is the partial subscriber list, or
      [None] if it is absent. *)

  val extensions : t -> Jsont.json
  (** [extensions subscription] is an object containing the unrecognized wire
      members preserved for [subscription]. *)

  val raw : t -> Jsont.json
  (** [raw subscription] is the complete JSON object encoded from
      [subscription], including its extension members.

      @raise Jsont.exception-Error if a member cannot be encoded. *)

  val jsont : t Jsont.t
  (** [jsont] is a codec for channel subscription objects. It preserves
      unrecognized members. *)
end

val jsont : t Jsont.t
(** [jsont] is a codec for channel objects. It preserves unrecognized members.
*)

val pp : Format.formatter -> t -> unit
(** [pp ppf channel] writes the channel name and optional identifier to [ppf].
*)
