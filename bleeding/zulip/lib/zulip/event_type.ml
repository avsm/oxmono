(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Message
  | Heartbeat
  | Presence
  | Typing
  | Reaction
  | Subscription
  | Stream
  | Realm
  | Realm_user
  | Realm_emoji
  | Realm_linkifiers
  | User_group
  | User_status
  | Update_message
  | Delete_message
  | Update_message_flags
  | Restart
  | Alert_words
  | Attachment
  | Custom_profile_fields
  | Drafts
  | Muted_users
  | User_topic
  | User_settings
  | Typing_edit_message
  | Realm_bot
  | Realm_domains
  | Realm_export
  | Realm_playgrounds
  | Realm_user_settings_defaults
  | Channel_folder
  | Navigation_view
  | Reminders
  | Saved_snippets
  | Scheduled_messages
  | Web_reload_client
  | Other of string

let to_string = function
  | Message -> "message"
  | Heartbeat -> "heartbeat"
  | Presence -> "presence"
  | Typing -> "typing"
  | Reaction -> "reaction"
  | Subscription -> "subscription"
  | Stream -> "stream"
  | Realm -> "realm"
  | Realm_user -> "realm_user"
  | Realm_emoji -> "realm_emoji"
  | Realm_linkifiers -> "realm_linkifiers"
  | User_group -> "user_group"
  | User_status -> "user_status"
  | Update_message -> "update_message"
  | Delete_message -> "delete_message"
  | Update_message_flags -> "update_message_flags"
  | Restart -> "restart"
  | Alert_words -> "alert_words"
  | Attachment -> "attachment"
  | Custom_profile_fields -> "custom_profile_fields"
  | Drafts -> "drafts"
  | Muted_users -> "muted_users"
  | User_topic -> "user_topic"
  | User_settings -> "user_settings"
  | Typing_edit_message -> "typing_edit_message"
  | Realm_bot -> "realm_bot"
  | Realm_domains -> "realm_domains"
  | Realm_export -> "realm_export"
  | Realm_playgrounds -> "realm_playgrounds"
  | Realm_user_settings_defaults -> "realm_user_settings_defaults"
  | Channel_folder -> "channel_folder"
  | Navigation_view -> "navigation_view"
  | Reminders -> "reminders"
  | Saved_snippets -> "saved_snippets"
  | Scheduled_messages -> "scheduled_messages"
  | Web_reload_client -> "web_reload_client"
  | Other s -> s

let of_string = function
  | "message" -> Message
  | "heartbeat" -> Heartbeat
  | "presence" -> Presence
  | "typing" -> Typing
  | "reaction" -> Reaction
  | "subscription" -> Subscription
  | "stream" -> Stream
  | "realm" -> Realm
  | "realm_user" -> Realm_user
  | "realm_emoji" -> Realm_emoji
  | "realm_linkifiers" -> Realm_linkifiers
  | "user_group" -> User_group
  | "user_status" -> User_status
  | "update_message" -> Update_message
  | "delete_message" -> Delete_message
  | "update_message_flags" -> Update_message_flags
  | "restart" -> Restart
  | "alert_words" -> Alert_words
  | "attachment" -> Attachment
  | "custom_profile_fields" -> Custom_profile_fields
  | "drafts" -> Drafts
  | "muted_users" -> Muted_users
  | "user_topic" -> User_topic
  | "user_settings" -> User_settings
  | "typing_edit_message" -> Typing_edit_message
  | "realm_bot" -> Realm_bot
  | "realm_domains" -> Realm_domains
  | "realm_export" -> Realm_export
  | "realm_playgrounds" -> Realm_playgrounds
  | "realm_user_settings_defaults" -> Realm_user_settings_defaults
  | "channel_folder" -> Channel_folder
  | "navigation_view" -> Navigation_view
  | "reminders" -> Reminders
  | "saved_snippets" -> Saved_snippets
  | "scheduled_messages" -> Scheduled_messages
  | "web_reload_client" -> Web_reload_client
  | s -> Other s

let pp fmt t = Format.fprintf fmt "%s" (to_string t)

let jsont =
  Jsont.map ~kind:"Zulip event type" ~dec:of_string ~enc:to_string Jsont.string
