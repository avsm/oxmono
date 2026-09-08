(** Typed personal settings.

    Setting keys pair each wire parameter name with its OCaml value type and
    JSON codec. Endpoint and decoding failures are returned as {!Error.t}
    values. *)

type mark_read_policy = [ `Always | `Conversation_views | `Never ]

type channel_default_view =
  [ `Top_topic | `Channel_feed | `Topic_list | `Top_unread_topic ]

type color_scheme = [ `Automatic | `Dark | `Light ]
type home_view = [ `Recent | `Inbox | `All_messages ]
type emoji_set = [ `Google | `Twitter | `Text ]
type demote_inactive = [ `Automatic | `Always | `Never ]
type user_list_style = [ `Compact | `Status | `Avatar_and_status ]
type animate_image_previews = [ `Always | `On_hover | `Never ]
type unread_count_policy = [ `All | `Unmuted | `None ]
type desktop_icon_count = [ `All | `Dm_mentions_followed | `Dm_mentions | `None ]
type realm_name_policy = [ `Automatic | `Always | `Never ]
type automatic_topic_policy = [ `Participated | `Sent | `Started | `Never ]
type resolved_notice_policy = [ `Always | `Except_followed | `Never ]

type email_visibility =
  [ `Everyone | `Members | `Administrators | `Nobody | `Moderators ]
(** The policy types above enumerate the accepted values of their corresponding
    settings. Values outside these closed variants are JSON decoding errors. *)

type _ key =
  | Full_name : string key
  | Email : string key
  | Old_password : string key
  | New_password : string key
  | Twenty_four_hour_time : bool key
  | Web_mark_read_on_scroll_policy : mark_read_policy key
  | Web_channel_default_view : channel_default_view key
  | Starred_message_counts : bool key
  | Receives_typing_notifications : bool key
  | Web_suggest_update_timezone : bool key
  | Fluid_layout_width : bool key
  | High_contrast_mode : bool key
  | Web_font_size_px : int key
  | Web_line_height_percent : int key
  | Color_scheme : color_scheme key
  | Enable_drafts_synchronization : bool key
  | Translate_emoticons : bool key
  | Display_emoji_reaction_users : bool key
  | Default_language : string key
  | Web_home_view : home_view key
  | Web_escape_navigates_to_home_view : bool key
  | Left_side_userlist : bool key
  | Emojiset : emoji_set key
  | Demote_inactive_streams : demote_inactive key
  | User_list_style : user_list_style key
  | Web_animate_image_previews : animate_image_previews key
  | Web_stream_unreads_count_display_policy : unread_count_policy key
  | Hide_ai_features : bool key
  | Web_inbox_show_channel_folders : bool key
  | Web_left_sidebar_show_channel_folders : bool key
  | Web_left_sidebar_unreads_count_summary : bool key
  | Timezone : string key
  | Enable_stream_desktop_notifications : bool key
  | Enable_stream_email_notifications : bool key
  | Enable_stream_push_notifications : bool key
  | Enable_stream_audible_notifications : bool key
  | Notification_sound : string key
  | Enable_desktop_notifications : bool key
  | Enable_sounds : bool key
  | Email_notifications_batching_period_seconds : int key
  | Enable_offline_email_notifications : bool key
  | Enable_offline_push_notifications : bool key
  | Enable_online_push_notifications : bool key
  | Enable_followed_topic_desktop_notifications : bool key
  | Enable_followed_topic_email_notifications : bool key
  | Enable_followed_topic_push_notifications : bool key
  | Enable_followed_topic_audible_notifications : bool key
  | Enable_digest_emails : bool key
  | Enable_marketing_emails : bool key
  | Enable_login_emails : bool key
  | Message_content_in_email_notifications : bool key
  | Pm_content_in_desktop_notifications : bool key
  | Wildcard_mentions_notify : bool key
  | Enable_followed_topic_wildcard_mentions_notify : bool key
  | Desktop_icon_count_display : desktop_icon_count key
  | Realm_name_in_email_notifications_policy : realm_name_policy key
  | Automatically_follow_topics_policy : automatic_topic_policy key
  | Automatically_unmute_topics_in_muted_streams_policy :
      automatic_topic_policy key
  | Automatically_follow_topics_where_mentioned : bool key
  | Resolved_topic_notice_auto_read_policy : resolved_notice_policy key
  | Presence_enabled : bool key
  | Enter_sends : bool key
  | Send_private_typing_notifications : bool key
  | Send_stream_typing_notifications : bool key
  | Send_read_receipts : bool key
  | Allow_private_data_export : bool key
  | Email_address_visibility : email_visibility key
  | Web_navigate_to_sent_message : bool key
      (** ['a key] identifies a setting whose value has type ['a]. *)

type assignment =
  | Set : 'a key * 'a -> assignment
      (** The type for heterogeneous setting assignments. [Set (key, value)]
          binds a key only to a value of the key's associated type. *)

type target_users = {
  user_ids : Zulip.Id.User.t list;
  group_ids : Zulip.Id.User_group.t list;
  skip_if_already_edited : bool option;
}
(** The type for an administrator's bulk update targets. Empty identifier lists
    select no users or groups. [skip_if_already_edited] is omitted when [None].
*)

type update_result = {
  ignored_parameters_unsupported : string list;
  raw : Jsont.json;
}
(** The type for a settings update response. [ignored_parameters_unsupported]
    defaults to an empty list when absent. [raw] is the complete response
    object. *)

val name : 'a key -> string
(** [name key] is the settings API parameter name of [key]. *)

val codec : 'a key -> 'a Jsont.t
(** [codec key] is the JSON codec for values associated with [key]. *)

val get : Initial_state.t -> 'a key -> ('a option, Error.t) result
(** [get state key] is the value of [key] in [state], or [None] if the setting
    is absent. Malformed present values produce [Error error]. *)

val update :
  Client.t ->
  ?target_users:target_users ->
  assignment list ->
  (update_result, Error.t) result
(** [update client assignments] applies [assignments] to the current user and is
    the complete server response. [target_users] defaults to omission. When
    present, it requests an administrator bulk update for those users and
    groups. An empty assignment list with no [target_users] produces
    [Error (Error.Invalid_request _)] without making a request. Password values
    are placed only in the request body. *)
