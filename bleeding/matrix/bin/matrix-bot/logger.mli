(** logger — a plugin that prints the room list and then every event.

    The typed event stream seen from outside, with no timeline items and no
    diffs, one line per {!Matrix_bot.Event.t}. With an event store under the
    context, what it paged in is still there after a restart. *)

val plugin : ?out:Format.formatter -> ?backfill:int -> Matrix_bot.Bot.plugin
(** [plugin spec] prints the room list once the sync loop is live, and prints
    every event of every room to [out], which defaults to
    {!Format.std_formatter}. [backfill] is how many pages of history to fetch
    from each room it joins, and defaults to 20. Zero fetches none. Back-filled
    history arrives as events like anything else, so it is printed too, oldest
    last. *)
