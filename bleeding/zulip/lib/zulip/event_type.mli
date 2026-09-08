@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Zulip event family names.

    Event family names are the strings in the [type] member of event queue
    objects. Unknown names remain representable for compatibility with newer
    servers. *)

(** The type for event family names. Constructors without payload support in
    {!Event_payload} decode as unknown payloads there. *)
type t =
  | Message  (** A new message was received. *)
  | Heartbeat  (** The event queue sent a keep-alive heartbeat. *)
  | Presence  (** A user's presence changed. *)
  | Typing  (** A user's typing state changed. *)
  | Reaction  (** An emoji reaction changed. *)
  | Subscription  (** A channel subscription changed. *)
  | Stream  (** A channel was created, deleted, or changed. *)
  | Realm  (** An organization setting changed. *)
  | Realm_user  (** An organization user changed. *)
  | Realm_emoji  (** A custom emoji changed. *)
  | Realm_linkifiers  (** An organization linkifier changed. *)
  | User_group  (** A user group changed. *)
  | User_status  (** A user's status changed. *)
  | Update_message  (** A message was edited or moved. *)
  | Delete_message  (** A message was deleted. *)
  | Update_message_flags  (** One or more message flags changed. *)
  | Restart  (** The server requested an event queue restart. *)
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
  | Other of string  (** The server supplied an unknown event family name. *)

val to_string : t -> string
(** [to_string event_type] is the wire spelling of [event_type]. *)

val of_string : string -> t
(** [of_string value] is the event type encoded by [value]. Unknown values
    produce [Other value]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf event_type] writes the wire spelling of [event_type] to [ppf]. *)

val jsont : t Jsont.t
(** [jsont] is a string codec for event family names. Unknown names decode as
    [Other value] and encode unchanged. *)
