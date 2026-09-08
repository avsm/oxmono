(** echo — a plugin that repeats what it is told.

    One {!Matrix_bot.Bot.on_message} handler and one send. *)

val plugin : ?reply_prefix:string -> Matrix_bot.Bot.plugin
(** [plugin spec] answers every message with [reply_prefix] before its body, as
    a notice. [reply_prefix] defaults to ["you said: "]. A message beginning
    with the bot's command prefix is a {!Matrix_bot.Event.Command} rather than a
    {!Matrix_bot.Event.Message} and is not echoed. *)
