(** logging — the library's log source.

    Every module of [matrix-chat.bot] reports through {!src}, so a program setting up
    [Logs] can raise or lower the bot library on its own. *)

val src : Logs.src
(** The source named ["matrix.bot"]. *)
