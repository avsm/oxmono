(** commands — a plugin of commands, and the generated help.

    A name, a usage and a doc that {!Matrix_bot.Bot.help} renders, an answer
    that is a reply, a reaction, a state event, and one command reserved to
    moderators. *)

val plugin : Matrix_bot.Bot.plugin
(** [plugin spec] registers [!ping], [!roll NdM], [!react], [!topic <text>] and
    the [!help] that lists them. [!topic] is [~admin:true], so a sender under
    the spec's admin level is refused before the handler runs. *)
