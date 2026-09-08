(** Bots on [matrix-chat.ui]: a typed event stream, per-room ordered handlers, and
    plugins that compose.

    A bot is a {!Bot.type-spec}, which is handlers registered against
    {!Event.t}, built by functions that take a spec and return a new one, so a
    plugin is any [spec -> spec]. {!Bot.run} owns the concurrency. One fiber per
    room drains that room's events in order while rooms proceed concurrently, a
    handler that raises is logged and its room continues, and a send answers
    through a {!Sent.t} the handler may await or ignore. {!Context} logs in and
    keeps the session, the keys and the plugin values under a profile, and
    {!Main} is the command line around it. *)

module Logging = Logging
module Plugin_store = Plugin_store
module Context = Context
module Sent = Sent
module Room = Room
module Event = Event
module Args = Args
module Bot = Bot
module Main = Main
