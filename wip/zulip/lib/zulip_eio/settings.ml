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

type assignment = Set : 'a key * 'a -> assignment

type target_users = {
  user_ids : Zulip.Id.User.t list;
  group_ids : Zulip.Id.User_group.t list;
  skip_if_already_edited : bool option;
}

type update_result = {
  ignored_parameters_unsupported : string list;
  raw : Jsont.json;
}

let ( let* ) = Result.bind

let name : type a. a key -> string = function
  | Full_name -> "full_name"
  | Email -> "email"
  | Old_password -> "old_password"
  | New_password -> "new_password"
  | Twenty_four_hour_time -> "twenty_four_hour_time"
  | Web_mark_read_on_scroll_policy -> "web_mark_read_on_scroll_policy"
  | Web_channel_default_view -> "web_channel_default_view"
  | Starred_message_counts -> "starred_message_counts"
  | Receives_typing_notifications -> "receives_typing_notifications"
  | Web_suggest_update_timezone -> "web_suggest_update_timezone"
  | Fluid_layout_width -> "fluid_layout_width"
  | High_contrast_mode -> "high_contrast_mode"
  | Web_font_size_px -> "web_font_size_px"
  | Web_line_height_percent -> "web_line_height_percent"
  | Color_scheme -> "color_scheme"
  | Enable_drafts_synchronization -> "enable_drafts_synchronization"
  | Translate_emoticons -> "translate_emoticons"
  | Display_emoji_reaction_users -> "display_emoji_reaction_users"
  | Default_language -> "default_language"
  | Web_home_view -> "web_home_view"
  | Web_escape_navigates_to_home_view -> "web_escape_navigates_to_home_view"
  | Left_side_userlist -> "left_side_userlist"
  | Emojiset -> "emojiset"
  | Demote_inactive_streams -> "demote_inactive_streams"
  | User_list_style -> "user_list_style"
  | Web_animate_image_previews -> "web_animate_image_previews"
  | Web_stream_unreads_count_display_policy ->
      "web_stream_unreads_count_display_policy"
  | Hide_ai_features -> "hide_ai_features"
  | Web_inbox_show_channel_folders -> "web_inbox_show_channel_folders"
  | Web_left_sidebar_show_channel_folders ->
      "web_left_sidebar_show_channel_folders"
  | Web_left_sidebar_unreads_count_summary ->
      "web_left_sidebar_unreads_count_summary"
  | Timezone -> "timezone"
  | Enable_stream_desktop_notifications -> "enable_stream_desktop_notifications"
  | Enable_stream_email_notifications -> "enable_stream_email_notifications"
  | Enable_stream_push_notifications -> "enable_stream_push_notifications"
  | Enable_stream_audible_notifications -> "enable_stream_audible_notifications"
  | Notification_sound -> "notification_sound"
  | Enable_desktop_notifications -> "enable_desktop_notifications"
  | Enable_sounds -> "enable_sounds"
  | Email_notifications_batching_period_seconds ->
      "email_notifications_batching_period_seconds"
  | Enable_offline_email_notifications -> "enable_offline_email_notifications"
  | Enable_offline_push_notifications -> "enable_offline_push_notifications"
  | Enable_online_push_notifications -> "enable_online_push_notifications"
  | Enable_followed_topic_desktop_notifications ->
      "enable_followed_topic_desktop_notifications"
  | Enable_followed_topic_email_notifications ->
      "enable_followed_topic_email_notifications"
  | Enable_followed_topic_push_notifications ->
      "enable_followed_topic_push_notifications"
  | Enable_followed_topic_audible_notifications ->
      "enable_followed_topic_audible_notifications"
  | Enable_digest_emails -> "enable_digest_emails"
  | Enable_marketing_emails -> "enable_marketing_emails"
  | Enable_login_emails -> "enable_login_emails"
  | Message_content_in_email_notifications ->
      "message_content_in_email_notifications"
  | Pm_content_in_desktop_notifications -> "pm_content_in_desktop_notifications"
  | Wildcard_mentions_notify -> "wildcard_mentions_notify"
  | Enable_followed_topic_wildcard_mentions_notify ->
      "enable_followed_topic_wildcard_mentions_notify"
  | Desktop_icon_count_display -> "desktop_icon_count_display"
  | Realm_name_in_email_notifications_policy ->
      "realm_name_in_email_notifications_policy"
  | Automatically_follow_topics_policy -> "automatically_follow_topics_policy"
  | Automatically_unmute_topics_in_muted_streams_policy ->
      "automatically_unmute_topics_in_muted_streams_policy"
  | Automatically_follow_topics_where_mentioned ->
      "automatically_follow_topics_where_mentioned"
  | Resolved_topic_notice_auto_read_policy ->
      "resolved_topic_notice_auto_read_policy"
  | Presence_enabled -> "presence_enabled"
  | Enter_sends -> "enter_sends"
  | Send_private_typing_notifications -> "send_private_typing_notifications"
  | Send_stream_typing_notifications -> "send_stream_typing_notifications"
  | Send_read_receipts -> "send_read_receipts"
  | Allow_private_data_export -> "allow_private_data_export"
  | Email_address_visibility -> "email_address_visibility"
  | Web_navigate_to_sent_message -> "web_navigate_to_sent_message"

let enum_int kind decode encode =
  Jsont.map ~kind ~dec:decode ~enc:encode Jsont.int

let enum_string kind decode encode =
  Jsont.map ~kind ~dec:decode ~enc:encode Jsont.string

let bad kind value =
  Jsont.Error.msgf Jsont.Meta.none "unknown %s value %S" kind value

let mark_read_jsont =
  enum_int "mark-read policy"
    (function
      | 1 -> `Always
      | 2 -> `Conversation_views
      | 3 -> `Never
      | n -> bad "mark-read policy" (string_of_int n))
    (function `Always -> 1 | `Conversation_views -> 2 | `Never -> 3)

let channel_view_jsont =
  enum_int "channel default view"
    (function
      | 1 -> `Top_topic
      | 2 -> `Channel_feed
      | 3 -> `Topic_list
      | 4 -> `Top_unread_topic
      | n -> bad "channel default view" (string_of_int n))
    (function
      | `Top_topic -> 1
      | `Channel_feed -> 2
      | `Topic_list -> 3
      | `Top_unread_topic -> 4)

let color_jsont =
  enum_int "color scheme"
    (function
      | 1 -> `Automatic
      | 2 -> `Dark
      | 3 -> `Light
      | n -> bad "color scheme" (string_of_int n))
    (function `Automatic -> 1 | `Dark -> 2 | `Light -> 3)

let home_jsont =
  enum_string "home view"
    (function
      | "recent" -> `Recent
      | "inbox" -> `Inbox
      | "all_messages" -> `All_messages
      | s -> bad "home view" s)
    (function
      | `Recent -> "recent"
      | `Inbox -> "inbox"
      | `All_messages -> "all_messages")

let emoji_jsont =
  enum_string "emoji set"
    (function
      | "google" -> `Google
      | "twitter" -> `Twitter
      | "text" -> `Text
      | s -> bad "emoji set" s)
    (function `Google -> "google" | `Twitter -> "twitter" | `Text -> "text")

let demote_jsont =
  enum_int "inactive-channel policy"
    (function
      | 1 -> `Automatic
      | 2 -> `Always
      | 3 -> `Never
      | n -> bad "inactive-channel policy" (string_of_int n))
    (function `Automatic -> 1 | `Always -> 2 | `Never -> 3)

let user_list_jsont =
  enum_int "user-list style"
    (function
      | 1 -> `Compact
      | 2 -> `Status
      | 3 -> `Avatar_and_status
      | n -> bad "user-list style" (string_of_int n))
    (function `Compact -> 1 | `Status -> 2 | `Avatar_and_status -> 3)

let animate_jsont =
  enum_string "image animation policy"
    (function
      | "always" -> `Always
      | "on_hover" -> `On_hover
      | "never" -> `Never
      | s -> bad "image animation policy" s)
    (function
      | `Always -> "always" | `On_hover -> "on_hover" | `Never -> "never")

let unread_jsont =
  enum_int "unread-count policy"
    (function
      | 1 -> `All
      | 2 -> `Unmuted
      | 3 -> `None
      | n -> bad "unread-count policy" (string_of_int n))
    (function `All -> 1 | `Unmuted -> 2 | `None -> 3)

let desktop_count_jsont =
  enum_int "desktop icon count"
    (function
      | 1 -> `All
      | 2 -> `Dm_mentions_followed
      | 3 -> `Dm_mentions
      | 4 -> `None
      | n -> bad "desktop icon count" (string_of_int n))
    (function
      | `All -> 1 | `Dm_mentions_followed -> 2 | `Dm_mentions -> 3 | `None -> 4)

let realm_name_jsont =
  enum_int "realm-name policy"
    (function
      | 1 -> `Automatic
      | 2 -> `Always
      | 3 -> `Never
      | n -> bad "realm-name policy" (string_of_int n))
    (function `Automatic -> 1 | `Always -> 2 | `Never -> 3)

let auto_topic_jsont =
  enum_int "automatic topic policy"
    (function
      | 1 -> `Participated
      | 2 -> `Sent
      | 3 -> `Started
      | 4 -> `Never
      | n -> bad "automatic topic policy" (string_of_int n))
    (function `Participated -> 1 | `Sent -> 2 | `Started -> 3 | `Never -> 4)

let resolved_jsont =
  enum_string "resolved-topic notice policy"
    (function
      | "always" -> `Always
      | "except_followed" -> `Except_followed
      | "never" -> `Never
      | s -> bad "resolved-topic notice policy" s)
    (function
      | `Always -> "always"
      | `Except_followed -> "except_followed"
      | `Never -> "never")

let email_visibility_jsont =
  enum_int "email visibility"
    (function
      | 1 -> `Everyone
      | 2 -> `Members
      | 3 -> `Administrators
      | 4 -> `Nobody
      | 5 -> `Moderators
      | n -> bad "email visibility" (string_of_int n))
    (function
      | `Everyone -> 1
      | `Members -> 2
      | `Administrators -> 3
      | `Nobody -> 4
      | `Moderators -> 5)

let codec : type a. a key -> a Jsont.t = function
  | Full_name -> Jsont.string
  | Email -> Jsont.string
  | Old_password -> Jsont.string
  | New_password -> Jsont.string
  | Default_language -> Jsont.string
  | Timezone -> Jsont.string
  | Notification_sound -> Jsont.string
  | Web_font_size_px -> Jsont.int
  | Web_line_height_percent -> Jsont.int
  | Email_notifications_batching_period_seconds -> Jsont.int
  | Web_mark_read_on_scroll_policy -> mark_read_jsont
  | Web_channel_default_view -> channel_view_jsont
  | Color_scheme -> color_jsont
  | Web_home_view -> home_jsont
  | Emojiset -> emoji_jsont
  | Demote_inactive_streams -> demote_jsont
  | User_list_style -> user_list_jsont
  | Web_animate_image_previews -> animate_jsont
  | Web_stream_unreads_count_display_policy -> unread_jsont
  | Desktop_icon_count_display -> desktop_count_jsont
  | Realm_name_in_email_notifications_policy -> realm_name_jsont
  | Automatically_follow_topics_policy -> auto_topic_jsont
  | Automatically_unmute_topics_in_muted_streams_policy -> auto_topic_jsont
  | Resolved_topic_notice_auto_read_policy -> resolved_jsont
  | Email_address_visibility -> email_visibility_jsont
  | Twenty_four_hour_time -> Jsont.bool
  | Starred_message_counts -> Jsont.bool
  | Receives_typing_notifications -> Jsont.bool
  | Web_suggest_update_timezone -> Jsont.bool
  | Fluid_layout_width -> Jsont.bool
  | High_contrast_mode -> Jsont.bool
  | Enable_drafts_synchronization -> Jsont.bool
  | Translate_emoticons -> Jsont.bool
  | Display_emoji_reaction_users -> Jsont.bool
  | Web_escape_navigates_to_home_view -> Jsont.bool
  | Left_side_userlist -> Jsont.bool
  | Hide_ai_features -> Jsont.bool
  | Web_inbox_show_channel_folders -> Jsont.bool
  | Web_left_sidebar_show_channel_folders -> Jsont.bool
  | Web_left_sidebar_unreads_count_summary -> Jsont.bool
  | Enable_stream_desktop_notifications -> Jsont.bool
  | Enable_stream_email_notifications -> Jsont.bool
  | Enable_stream_push_notifications -> Jsont.bool
  | Enable_stream_audible_notifications -> Jsont.bool
  | Enable_desktop_notifications -> Jsont.bool
  | Enable_sounds -> Jsont.bool
  | Enable_offline_email_notifications -> Jsont.bool
  | Enable_offline_push_notifications -> Jsont.bool
  | Enable_online_push_notifications -> Jsont.bool
  | Enable_followed_topic_desktop_notifications -> Jsont.bool
  | Enable_followed_topic_email_notifications -> Jsont.bool
  | Enable_followed_topic_push_notifications -> Jsont.bool
  | Enable_followed_topic_audible_notifications -> Jsont.bool
  | Enable_digest_emails -> Jsont.bool
  | Enable_marketing_emails -> Jsont.bool
  | Enable_login_emails -> Jsont.bool
  | Message_content_in_email_notifications -> Jsont.bool
  | Pm_content_in_desktop_notifications -> Jsont.bool
  | Wildcard_mentions_notify -> Jsont.bool
  | Enable_followed_topic_wildcard_mentions_notify -> Jsont.bool
  | Automatically_follow_topics_where_mentioned -> Jsont.bool
  | Presence_enabled -> Jsont.bool
  | Enter_sends -> Jsont.bool
  | Send_private_typing_notifications -> Jsont.bool
  | Send_stream_typing_notifications -> Jsont.bool
  | Send_read_receipts -> Jsont.bool
  | Allow_private_data_export -> Jsont.bool
  | Web_navigate_to_sent_message -> Jsont.bool

let encode_value : type a. a key -> a -> string =
 fun key value ->
  match key with
  | Full_name -> value
  | Email -> value
  | Old_password -> value
  | New_password -> value
  | Default_language -> value
  | Timezone -> value
  | Notification_sound -> value
  | Web_font_size_px -> string_of_int value
  | Web_line_height_percent -> string_of_int value
  | Email_notifications_batching_period_seconds -> string_of_int value
  | Twenty_four_hour_time -> string_of_bool value
  | Starred_message_counts -> string_of_bool value
  | Receives_typing_notifications -> string_of_bool value
  | Web_suggest_update_timezone -> string_of_bool value
  | Fluid_layout_width -> string_of_bool value
  | High_contrast_mode -> string_of_bool value
  | Enable_drafts_synchronization -> string_of_bool value
  | Translate_emoticons -> string_of_bool value
  | Display_emoji_reaction_users -> string_of_bool value
  | Web_escape_navigates_to_home_view -> string_of_bool value
  | Left_side_userlist -> string_of_bool value
  | Hide_ai_features -> string_of_bool value
  | Web_inbox_show_channel_folders -> string_of_bool value
  | Web_left_sidebar_show_channel_folders -> string_of_bool value
  | Web_left_sidebar_unreads_count_summary -> string_of_bool value
  | Enable_stream_desktop_notifications -> string_of_bool value
  | Enable_stream_email_notifications -> string_of_bool value
  | Enable_stream_push_notifications -> string_of_bool value
  | Enable_stream_audible_notifications -> string_of_bool value
  | Enable_desktop_notifications -> string_of_bool value
  | Enable_sounds -> string_of_bool value
  | Enable_offline_email_notifications -> string_of_bool value
  | Enable_offline_push_notifications -> string_of_bool value
  | Enable_online_push_notifications -> string_of_bool value
  | Enable_followed_topic_desktop_notifications -> string_of_bool value
  | Enable_followed_topic_email_notifications -> string_of_bool value
  | Enable_followed_topic_push_notifications -> string_of_bool value
  | Enable_followed_topic_audible_notifications -> string_of_bool value
  | Enable_digest_emails -> string_of_bool value
  | Enable_marketing_emails -> string_of_bool value
  | Enable_login_emails -> string_of_bool value
  | Message_content_in_email_notifications -> string_of_bool value
  | Pm_content_in_desktop_notifications -> string_of_bool value
  | Wildcard_mentions_notify -> string_of_bool value
  | Enable_followed_topic_wildcard_mentions_notify -> string_of_bool value
  | Automatically_follow_topics_where_mentioned -> string_of_bool value
  | Presence_enabled -> string_of_bool value
  | Enter_sends -> string_of_bool value
  | Send_private_typing_notifications -> string_of_bool value
  | Send_stream_typing_notifications -> string_of_bool value
  | Send_read_receipts -> string_of_bool value
  | Allow_private_data_export -> string_of_bool value
  | Web_navigate_to_sent_message -> string_of_bool value
  | Web_mark_read_on_scroll_policy ->
      string_of_int
        (match value with
        | `Always -> 1
        | `Conversation_views -> 2
        | `Never -> 3)
  | Web_channel_default_view ->
      string_of_int
        (match value with
        | `Top_topic -> 1
        | `Channel_feed -> 2
        | `Topic_list -> 3
        | `Top_unread_topic -> 4)
  | Color_scheme ->
      string_of_int
        (match value with `Automatic -> 1 | `Dark -> 2 | `Light -> 3)
  | Web_home_view -> (
      match value with
      | `Recent -> "recent"
      | `Inbox -> "inbox"
      | `All_messages -> "all_messages")
  | Emojiset -> (
      match value with
      | `Google -> "google"
      | `Twitter -> "twitter"
      | `Text -> "text")
  | Demote_inactive_streams ->
      string_of_int
        (match value with `Automatic -> 1 | `Always -> 2 | `Never -> 3)
  | User_list_style ->
      string_of_int
        (match value with
        | `Compact -> 1
        | `Status -> 2
        | `Avatar_and_status -> 3)
  | Web_animate_image_previews -> (
      match value with
      | `Always -> "always"
      | `On_hover -> "on_hover"
      | `Never -> "never")
  | Web_stream_unreads_count_display_policy ->
      string_of_int (match value with `All -> 1 | `Unmuted -> 2 | `None -> 3)
  | Desktop_icon_count_display ->
      string_of_int
        (match value with
        | `All -> 1
        | `Dm_mentions_followed -> 2
        | `Dm_mentions -> 3
        | `None -> 4)
  | Realm_name_in_email_notifications_policy ->
      string_of_int
        (match value with `Automatic -> 1 | `Always -> 2 | `Never -> 3)
  | Automatically_follow_topics_policy ->
      string_of_int
        (match value with
        | `Participated -> 1
        | `Sent -> 2
        | `Started -> 3
        | `Never -> 4)
  | Automatically_unmute_topics_in_muted_streams_policy ->
      string_of_int
        (match value with
        | `Participated -> 1
        | `Sent -> 2
        | `Started -> 3
        | `Never -> 4)
  | Resolved_topic_notice_auto_read_policy -> (
      match value with
      | `Always -> "always"
      | `Except_followed -> "except_followed"
      | `Never -> "never")
  | Email_address_visibility ->
      string_of_int
        (match value with
        | `Everyone -> 1
        | `Members -> 2
        | `Administrators -> 3
        | `Nobody -> 4
        | `Moderators -> 5)

let target_users_jsont =
  Jsont.Object.map ~kind:"Zulip setting targets"
    (fun user_ids group_ids skip_if_already_edited ->
      { user_ids; group_ids; skip_if_already_edited })
  |> Jsont.Object.mem "user_ids"
       (Jsont.list Zulip.Id.User.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun t -> t.user_ids)
  |> Jsont.Object.mem "group_ids"
       (Jsont.list Zulip.Id.User_group.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun t -> t.group_ids)
  |> Jsont.Object.opt_mem "skip_if_already_edited" Jsont.bool ~enc:(fun t ->
      t.skip_if_already_edited)
  |> Jsont.Object.finish

let result_jsont =
  Jsont.map ~kind:"Zulip settings update result"
    ~enc:(fun result -> result.raw)
    ~dec:(fun raw ->
      let ignored =
        match raw with
        | Jsont.Object (members, _) -> (
            match
              Jsont.Json.find_mem "ignored_parameters_unsupported" members
            with
            | None -> []
            | Some (_, value) -> (
                match Jsont.Json.decode' (Jsont.list Jsont.string) value with
                | Ok v -> v
                | Error e -> raise (Jsont.Error e)))
        | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
      in
      { ignored_parameters_unsupported = ignored; raw })
    Jsont.json

let get state key = Initial_state.setting state (name key) (codec key)

let update client ?target_users assignments =
  if assignments = [] && Option.is_none target_users then
    Error (Error.Invalid_request "settings update has no changes")
  else
    let settings =
      List.map
        (fun (Set (key, value)) -> (name key, encode_value key value))
        assignments
    in
    let* targets =
      match target_users with
      | None -> Ok []
      | Some targets ->
          Codec.encode target_users_jsont targets
          |> Result.map (fun value -> [ ("target_users", value) ])
    in
    Client.request_typed client ~method_:`PATCH ~path:"/api/v1/settings"
      ~params:(targets @ settings) ~codec:result_jsont ()
