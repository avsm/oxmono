(** moderator — a plugin that redacts, warns and eventually removes.

    The strike counts are kept in {!Matrix_bot.Plugin_store} per room and per
    user, so a bot that is restarted does not forgive anybody. *)

val plugin : ?words:string list -> ?strikes:int -> Matrix_bot.Bot.plugin
(** [plugin spec] redacts a message containing one of [words], warns its sender,
    and kicks them on the [strikes]th. [words] defaults to
    [["badger"; "spoiler"]] and [strikes] to 3. The test is case-folded and
    diacritic-insensitive, and it is a substring test rather than a word test,
    which is deliberately blunt.

    The bot needs power level 50 in the room to redact and to kick, and strictly
    more than its target to kick. Without it the warning says what the
    homeserver refused. *)
