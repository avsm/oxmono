(** welcome — a plugin that narrates what happens to a room.

    Membership, display names and room state arrive as {!Matrix_ui.Presentation}
    values, so nothing here parses an [m.room.member] event. *)

val plugin : Matrix_bot.Bot.plugin
(** [plugin spec] greets an arrival, notes a departure, a ban and a kick, and
    reports a change of name, topic, avatar or display name, each as a notice.
    The bot says nothing about itself. *)
