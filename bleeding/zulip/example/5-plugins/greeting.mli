(** Configurable greeting commands. *)

val command : name:string -> greeting:string -> Zulip_bot.Bot.plugin
(** [command ~name ~greeting] is a plugin that registers [name] as a command
    replying with [greeting] and the supplied command argument. *)
