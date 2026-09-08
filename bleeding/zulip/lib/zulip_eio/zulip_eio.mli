(** Zulip REST clients and live event queues over Fetch and Eio.

    {!Client} sends requests through an application-owned {!Transport}.
    {!Profile} resolves named XDG profiles. Endpoint modules return {!Error.t}
    and {!Event_queue} provides registration, polling and acknowledgement. *)

module Error = Error
(** Structured request and storage failures. *)

module Transport = Transport
(** Reusable HTTP capabilities. *)

module Auth = Auth
(** Zulip credentials and zuliprc imports. *)

module Profile = Profile
(** Named XDG credential profiles. *)

module Client = Client
(** Zulip REST requests. *)

module Event_queue = Event_queue
(** Event registration, polling, and acknowledgement. *)

module Initial_state = Initial_state
(** Typed registration snapshot access. *)

module Messages = Messages
(** Message operations. *)

module Channels = Channels
(** Channel, subscription, and topic operations. *)

module Users = Users
(** User and bot operations. *)

module User_group = User_group
(** User-group operations. *)

module Presence = Presence
(** Presence operations and codecs. *)

module Typing = Typing
(** Typing indicators. *)

module Server = Server
(** Server metadata. *)

module Bot_storage = Bot_storage
(** Server-backed bot key/value storage. *)

module Attachments = Attachments
(** Attachment operations. *)

module Drafts = Drafts
(** Draft operations. *)

module Saved_snippets = Saved_snippets
(** Saved-snippet operations. *)

module Scheduled_messages = Scheduled_messages
(** Scheduled-message operations. *)

module Reminders = Reminders
(** Reminder operations. *)

module Channel_folders = Channel_folders
(** Channel-folder operations. *)

module Settings = Settings
(** Organization settings. *)
