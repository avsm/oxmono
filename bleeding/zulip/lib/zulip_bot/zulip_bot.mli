(** Composable Zulip bot framework. *)

module Plugin_store = Plugin_store
(** Persistent plugin values. *)

module Context = Context
(** Shared bot runtime resources. *)

module Sent = Sent
(** Asynchronous message-send results. *)

module Room = Room
(** Stable Zulip conversations. *)

module Event = Event
(** Events presented to bot handlers. *)

module Bot = Bot
(** Bot specifications and execution. *)

module Webhook = Webhook
(** Outgoing-webhook validation and dispatch. *)
