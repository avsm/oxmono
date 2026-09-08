(** Channel administration, subscriptions, and topics.

    Identifiers use distinct channel, folder, message, and user types. Optional
    request arguments are omitted unless stated otherwise, allowing the
    connected Zulip server to apply version-appropriate defaults. Topic policy
    and visibility types preserve unrecognized future wire values. *)

type include_subscribers =
  | Exclude
  | Include
  | Partial
      (** The type for subscriber detail in subscription listings. [Exclude]
          omits subscriber lists. [Include] requests complete lists. [Partial]
          permits the server to return partial lists for large channels. *)

type retention =
  | Realm_default
  | Forever
  | Days of int
      (** The type for channel message retention. [Realm_default] uses the
          organization policy. [Forever] disables automatic expiry. [Days days]
          retains messages for [days], which must be positive. *)

type create_options = {
  name : string;
  description : string option;
  subscribers : Zulip.Id.User.t list;
  announce : bool option;
  invite_only : bool option;
  is_web_public : bool option;
  is_default_stream : bool option;
  history_public_to_subscribers : bool option;
  message_retention_days : retention option;
  folder_id : Zulip.Id.Channel_folder.t option;
  topics_policy : Zulip.Channel.Topics_policy.t option;
  can_add_subscribers_group : Zulip.Group_setting.t option;
  can_create_topic_group : Zulip.Group_setting.t option;
  can_delete_any_message_group : Zulip.Group_setting.t option;
  can_delete_own_message_group : Zulip.Group_setting.t option;
  can_remove_subscribers_group : Zulip.Group_setting.t option;
  can_administer_channel_group : Zulip.Group_setting.t option;
  can_move_messages_out_of_channel_group : Zulip.Group_setting.t option;
  can_move_messages_within_channel_group : Zulip.Group_setting.t option;
  can_send_message_group : Zulip.Group_setting.t option;
  can_subscribe_group : Zulip.Group_setting.t option;
  can_resolve_topics_group : Zulip.Group_setting.t option;
}
(** The type for channel creation requests. [name] names the channel and
    [subscribers] contains typed user identifiers to subscribe. [description] is
    Markdown. Optional Boolean, retention, folder, topic, and group-setting
    fields are omitted when [None], for which Zulip applies its server defaults.
*)

type subscription_request = {
  name : string;
  color : string option;
  description : string option;
}
(** The type for subscription requests. [description] configures a channel
    created by the request. [color] and [description] are omitted when [None].
*)

type subscription_result = {
  subscribed : (string * string list) list;
  already_subscribed : (string * string list) list;
  unauthorized : string list;
  new_subscription_messages_sent : bool option;
  raw : Jsont.json;
}
(** The type for subscription results. The association lists map user
    identifiers as returned by Zulip to channel names. Missing collection fields
    decode as empty. [new_subscription_messages_sent] is absent when the server
    omits it. [raw] preserves the complete response for future fields. *)

type subscription_update_result = {
  subscribed : (string * string list) list;
  already_subscribed : (string * string list) list;
  not_removed : string list;
  removed : string list;
  new_subscription_messages_sent : bool option;
  raw : Jsont.json;
}
(** The type for combined subscription-update results. Missing collection fields
    decode as empty. [new_subscription_messages_sent] is absent when the server
    omits it. [raw] preserves the complete response for future fields. *)

type unsubscribe_result = {
  not_removed : string list;
  removed : string list;
  raw : Jsont.json;
}
(** The type for unsubscribe results. Missing collection fields decode as empty.
    [raw] preserves the complete response for future fields. *)

type subscription_property =
  | Color of string
  | Is_muted of bool
  | In_home_view of bool
  | Pin_to_top of bool
  | Desktop_notifications of bool
  | Audible_notifications of bool
  | Push_notifications of bool
  | Email_notifications of bool
  | Wildcard_mentions_notify of bool
      (** The type for mutable personal channel-subscription properties. Each
          constructor carries the new property value. *)

type property_update = {
  channel_id : Zulip.Id.Channel.t;
  property : subscription_property;
}
(** The type for a personal subscription-property update on a typed channel
    identifier. *)

type mute_op =
  | Mute
  | Unmute  (** The type for the legacy topic mute operation. *)

module Topic : sig
  (** Summaries of accessible channel topics. *)

  type t
  (** The type for topic summaries. *)

  val name : t -> string
  (** [name topic] is the topic name. *)

  val max_id : t -> Zulip.Id.Message.t
  (** [max_id topic] is the typed identifier of the latest message in [topic].
  *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for topic summaries. *)
end

val list : Client.t -> (Zulip.Channel.t list, Error.t) result
(** [list client] is the default set of channels accessible to [client]. Zulip
    includes public and subscribed channels and excludes archived,
    web-public-only, default-only, owner-subscribed, and other
    metadata-accessible channels by default. Request and response-decoding
    failures are returned as [Error]. *)

val list_all :
  Client.t ->
  ?include_public:bool ->
  ?include_web_public:bool ->
  ?include_subscribed:bool ->
  ?exclude_archived:bool ->
  ?include_all_active:bool ->
  ?include_all:bool ->
  ?include_default:bool ->
  ?include_owner_subscribed:bool ->
  ?include_can_access_content:bool ->
  unit ->
  (Zulip.Channel.t list, Error.t) result
(** [list_all client ~include_public ~include_web_public ~include_subscribed
     ~exclude_archived ~include_all_active ~include_all ~include_default
     ~include_owner_subscribed ~include_can_access_content ()] is the requested
    set of channels accessible to [client]. Every selector is omitted by
    default. Zulip defaults [include_public], [include_subscribed], and
    [exclude_archived] to [true], and the other selectors to [false].
    [include_all_active] is a deprecated server option superseded by
    [include_all]. Request and response-decoding failures are returned as
    [Error]. *)

val get_id : Client.t -> name:string -> (Zulip.Id.Channel.t, Error.t) result
(** [get_id client ~name] is the typed identifier of the channel named [name].
    Request and response-decoding failures are returned as [Error]. *)

val get_by_id :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (Zulip.Channel.t, Error.t) result
(** [get_by_id client ~channel_id] is the channel identified by [channel_id].
    Request and response-decoding failures are returned as [Error]. *)

val create_options : name:string -> unit -> create_options
(** [create_options ~name ()] is a channel creation request named [name] with no
    subscribers and every optional setting omitted. *)

val create : Client.t -> create_options -> (Zulip.Id.Channel.t, Error.t) result
(** [create client options] creates the channel described by [options] and is
    its typed identifier. It returns [Error.Invalid_request] without making a
    request when [options.message_retention_days] is nonpositive. Encoding,
    request, and response-decoding failures are returned as [Error]. *)

val create_simple :
  Client.t ->
  name:string ->
  ?description:string ->
  ?invite_only:bool ->
  unit ->
  (Zulip.Id.Channel.t, Error.t) result
(** [create_simple client ~name ~description ~invite_only ()] creates a channel
    named [name] and is its typed identifier. [description] and [invite_only]
    are omitted by default. The channel has no initial subscribers, and Zulip
    applies server defaults to its other settings. Encoding, request, and
    response-decoding failures are returned as [Error]. *)

val update :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  ?description:string ->
  ?new_name:string ->
  ?is_private:bool ->
  ?is_web_public:bool ->
  ?history_public_to_subscribers:bool ->
  ?is_default_stream:bool ->
  ?message_retention_days:retention ->
  ?is_archived:bool ->
  ?folder_id:Zulip.Id.Channel_folder.t option ->
  ?topics_policy:Zulip.Channel.Topics_policy.t ->
  ?can_add_subscribers_group:Zulip.Group_setting.update ->
  ?can_create_topic_group:Zulip.Group_setting.update ->
  ?can_delete_any_message_group:Zulip.Group_setting.update ->
  ?can_delete_own_message_group:Zulip.Group_setting.update ->
  ?can_remove_subscribers_group:Zulip.Group_setting.update ->
  ?can_administer_channel_group:Zulip.Group_setting.update ->
  ?can_move_messages_out_of_channel_group:Zulip.Group_setting.update ->
  ?can_move_messages_within_channel_group:Zulip.Group_setting.update ->
  ?can_send_message_group:Zulip.Group_setting.update ->
  ?can_subscribe_group:Zulip.Group_setting.update ->
  ?can_resolve_topics_group:Zulip.Group_setting.update ->
  ?stream_post_policy:int ->
  unit ->
  (unit, Error.t) result
(** [update client ~channel_id ~description ~new_name ~is_private ~is_web_public
     ~history_public_to_subscribers ~is_default_stream ~message_retention_days
     ~is_archived ~folder_id ~topics_policy ~can_add_subscribers_group
     ~can_create_topic_group ~can_delete_any_message_group
     ~can_delete_own_message_group ~can_remove_subscribers_group
     ~can_administer_channel_group ~can_move_messages_out_of_channel_group
     ~can_move_messages_within_channel_group ~can_send_message_group
     ~can_subscribe_group ~can_resolve_topics_group ~stream_post_policy ()]
    applies the supplied changes to [channel_id]. Each optional change is
    omitted by default. [folder_id] set to [Some None] removes the channel from
    its folder. Group-setting updates can include an expected old value.
    [stream_post_policy] is the deprecated integer wire setting for older
    servers.

    It returns [Error.Invalid_request] without making a request when no change
    is supplied or retention days are nonpositive. Encoding and request failures
    are returned as [Error]. *)

val delete : Client.t -> channel_id:Zulip.Id.Channel.t -> (unit, Error.t) result
(** [delete client ~channel_id] archives [channel_id]. Request failures are
    returned as [Error]. *)

val archive :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (unit, Error.t) result
(** [archive client ~channel_id] archives [channel_id]. It is equivalent to
    {!delete}. Request failures are returned as [Error]. *)

val set_archived :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  archived:bool ->
  (unit, Error.t) result
(** [set_archived client ~channel_id ~archived] sets the archive state of
    [channel_id] to [archived]. Request failures are returned as [Error]. *)

val add_default :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (unit, Error.t) result
(** [add_default client ~channel_id] adds [channel_id] to the organization's
    default channels. Request failures are returned as [Error]. *)

val remove_default :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (unit, Error.t) result
(** [remove_default client ~channel_id] removes [channel_id] from the
    organization's default channels. Request failures are returned as [Error].
*)

val subscribe :
  Client.t ->
  subscriptions:subscription_request list ->
  ?principals:[ `Emails of string list | `User_ids of Zulip.Id.User.t list ] ->
  ?authorization_errors_fatal:bool ->
  ?announce:bool ->
  ?invite_only:bool ->
  ?is_web_public:bool ->
  ?is_default_stream:bool ->
  ?history_public_to_subscribers:bool ->
  ?message_retention_days:retention ->
  ?folder_id:Zulip.Id.Channel_folder.t ->
  ?topics_policy:Zulip.Channel.Topics_policy.t ->
  ?can_add_subscribers_group:Zulip.Group_setting.t ->
  ?can_create_topic_group:Zulip.Group_setting.t ->
  ?can_delete_any_message_group:Zulip.Group_setting.t ->
  ?can_delete_own_message_group:Zulip.Group_setting.t ->
  ?can_remove_subscribers_group:Zulip.Group_setting.t ->
  ?can_administer_channel_group:Zulip.Group_setting.t ->
  ?can_move_messages_out_of_channel_group:Zulip.Group_setting.t ->
  ?can_move_messages_within_channel_group:Zulip.Group_setting.t ->
  ?can_send_message_group:Zulip.Group_setting.t ->
  ?can_subscribe_group:Zulip.Group_setting.t ->
  ?can_resolve_topics_group:Zulip.Group_setting.t ->
  ?send_new_subscription_messages:bool ->
  unit ->
  (subscription_result, Error.t) result
(** [subscribe client ~subscriptions ~principals ~authorization_errors_fatal
     ~announce ~invite_only ~is_web_public ~is_default_stream
     ~history_public_to_subscribers ~message_retention_days ~folder_id
     ~topics_policy ~can_add_subscribers_group ~can_create_topic_group
     ~can_delete_any_message_group ~can_delete_own_message_group
     ~can_remove_subscribers_group ~can_administer_channel_group
     ~can_move_messages_out_of_channel_group
     ~can_move_messages_within_channel_group ~can_send_message_group
     ~can_subscribe_group ~can_resolve_topics_group
     ~send_new_subscription_messages ()] subscribes users to the named channels.
    Missing channels are created using the supplied initial settings.
    [principals] is omitted by default, for which the authenticated user is the
    target. All other optional arguments are omitted by default and Zulip
    applies its server defaults. Initial channel settings do not affect channels
    that already exist.

    It returns [Error.Invalid_request] without making a request when retention
    days are nonpositive. Encoding, request, and response-decoding failures are
    returned as [Error]. *)

val subscribe_simple :
  Client.t -> channels:string list -> (unit, Error.t) result
(** [subscribe_simple client ~channels] subscribes the authenticated user to
    [channels]. Missing channels are created with server-default settings.
    Encoding and request failures are returned as [Error]. *)

val unsubscribe :
  Client.t ->
  subscriptions:string list ->
  ?principals:[ `Emails of string list | `User_ids of Zulip.Id.User.t list ] ->
  unit ->
  (unsubscribe_result, Error.t) result
(** [unsubscribe client ~subscriptions ~principals ()] unsubscribes users from
    the named channels. [principals] is omitted by default, for which the
    authenticated user is the target. Encoding, request, and response-decoding
    failures are returned as [Error]. *)

val unsubscribe_simple :
  Client.t -> channels:string list -> (unit, Error.t) result
(** [unsubscribe_simple client ~channels] unsubscribes the authenticated user
    from [channels]. Encoding and request failures are returned as [Error]. *)

val update_subscriptions :
  Client.t ->
  ?add:subscription_request list ->
  ?remove:string list ->
  unit ->
  (subscription_update_result, Error.t) result
(** [update_subscriptions client ~add ~remove ()] applies both subscription
    changes to the authenticated user. [add] and [remove] default to empty
    lists. It returns [Error.Invalid_request] without making a request when both
    lists are empty. Encoding, request, and response-decoding failures are
    returned as [Error]. *)

val get_subscriptions :
  Client.t -> (Zulip.Channel.Subscription.t list, Error.t) result
(** [get_subscriptions client] is the authenticated user's subscriptions without
    requested subscriber lists. Request and response-decoding failures are
    returned as [Error]. *)

val get_subscriptions_with :
  Client.t ->
  ?include_subscribers:include_subscribers ->
  unit ->
  (Zulip.Channel.Subscription.t list, Error.t) result
(** [get_subscriptions_with client ~include_subscribers ()] is the authenticated
    user's subscriptions with the requested subscriber detail.
    [include_subscribers] is omitted by default, for which Zulip excludes
    subscriber lists. Request and response-decoding failures are returned as
    [Error]. *)

val get_user_channels :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  (Zulip.Id.Channel.t list, Error.t) result
(** [get_user_channels client ~user_id] is the typed identifiers of channels to
    which [user_id] is subscribed. Request and response-decoding failures are
    returned as [Error]. *)

val get_subscription_status :
  Client.t ->
  user_id:Zulip.Id.User.t ->
  channel_id:Zulip.Id.Channel.t ->
  (bool, Error.t) result
(** [get_subscription_status client ~user_id ~channel_id] is [true] if [user_id]
    is subscribed to [channel_id]. Request and response-decoding failures are
    returned as [Error]. *)

val update_subscription_properties :
  Client.t -> property_update list -> (unit, Error.t) result
(** [update_subscription_properties client updates] applies [updates] to the
    authenticated user's subscriptions. It returns [Error.Invalid_request]
    without making a request when [updates] is empty. Encoding and request
    failures are returned as [Error]. *)

val update_subscription_property :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  subscription_property ->
  (unit, Error.t) result
(** [update_subscription_property client ~channel_id property] applies
    [property] to the authenticated user's subscription to [channel_id].
    Encoding and request failures are returned as [Error]. *)

val update_subscription_settings :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  ?color:string ->
  ?is_muted:bool ->
  ?pin_to_top:bool ->
  ?desktop_notifications:bool ->
  ?audible_notifications:bool ->
  ?push_notifications:bool ->
  ?email_notifications:bool ->
  ?wildcard_mentions_notify:bool ->
  unit ->
  (unit, Error.t) result
(** [update_subscription_settings client ~channel_id ~color ~is_muted
     ~pin_to_top ~desktop_notifications ~audible_notifications
     ~push_notifications ~email_notifications ~wildcard_mentions_notify ()]
    applies the supplied personal settings to the subscription for [channel_id].
    Each setting is omitted by default. It returns [Error.Invalid_request]
    without making a request when no setting is supplied. Encoding and request
    failures are returned as [Error]. *)

val get_topics :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  ?allow_empty_topic_name:bool ->
  unit ->
  (Topic.t list, Error.t) result
(** [get_topics client ~channel_id ~allow_empty_topic_name ()] is the accessible
    topics in [channel_id], ordered with the most recently active first.
    [allow_empty_topic_name] is omitted by default, for which Zulip replaces an
    empty topic name with the organization's display name. Request and
    response-decoding failures are returned as [Error]. *)

val delete_topic :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  ([ `Complete | `Incomplete ], Error.t) result
(** [delete_topic client ~channel_id ~topic] requests deletion of all messages
    in [topic] from [channel_id]. [`Complete] means that all messages were
    deleted. [`Incomplete] means that the server stopped after deleting only
    part of the topic and the caller should issue another request to delete the
    remainder. The client does not retry this mutation automatically. Request
    and response-decoding failures are returned as [Error]. *)

val set_topic_visibility :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  visibility_policy:Zulip.Topic_visibility.t ->
  (unit, Error.t) result
(** [set_topic_visibility client ~channel_id ~topic ~visibility_policy] sets the
    authenticated user's visibility policy for [topic] in [channel_id]. The
    typed policy preserves unknown future integer values. Request failures are
    returned as [Error]. *)

val set_topic_mute :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  topic:string ->
  op:mute_op ->
  (unit, Error.t) result
(** [set_topic_mute client ~channel_id ~topic ~op] applies the legacy [op] to
    [topic]. [Mute] sets the visibility policy to muted. [Unmute] sets it to
    unmuted. Request failures are returned as [Error]. *)

val get_subscribers :
  Client.t ->
  channel_id:Zulip.Id.Channel.t ->
  (Zulip.Id.User.t list, Error.t) result
(** [get_subscribers client ~channel_id] is the typed identifiers of subscribers
    to [channel_id]. Request and response-decoding failures are returned as
    [Error]. *)

val get_subscribers_by_name :
  Client.t -> name:string -> (Zulip.Id.User.t list, Error.t) result
(** [get_subscribers_by_name client ~name] is the typed identifiers of
    subscribers to the channel named [name]. Failures to resolve the name and
    failures to request or decode subscribers are returned as [Error]. *)

val get_email_address :
  Client.t -> channel_id:Zulip.Id.Channel.t -> (string, Error.t) result
(** [get_email_address client ~channel_id] is the incoming email address for
    [channel_id] using Zulip's default email-gateway sender. Request and
    response-decoding failures are returned as [Error]. *)
