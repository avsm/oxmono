(** notify — one message, sent, waited for, and then the process is done.

    The cron shape, and the one thing here that is not a plugin: a bot that
    returns of its own accord. It joins the room it is given, sends, waits for
    the homeserver to acknowledge the event, and stops. *)

val send :
  Matrix_bot.Context.t ->
  room:Matrix_client.Directory.room_id_or_alias ->
  body:string ->
  ?timeout:float ->
  unit ->
  (Matrix_proto.Id.Event_id.t, string) result
(** [send ctx ~room ~body ()] runs a bot until [body] has been sent to [room]
    and is the event id the homeserver gave it. [timeout] is in seconds,
    defaults to 120, and covers the join, the room's encryption settling and the
    send together. The error is a message fit for a command line. *)

val spec :
  room:Matrix_client.Directory.room_id_or_alias ->
  body:string ->
  ?timeout:float ->
  ((Matrix_proto.Id.Event_id.t, string) result -> unit) ->
  Matrix_bot.Bot.spec
(** [spec ~room ~body report] is the one-shot bot {!send} runs. It is also
    useful to callers which already compose {!Matrix_bot.Bot.spec} values. The
    outcome is handed to [report] once, before the bot stops. *)
