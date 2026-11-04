(** Keeping a message per pipeline up to date in each bound room. *)

val run :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  api:Tangled.Api.t ->
  bot:Bot.t ->
  messages:Messages.t ->
  accounts:string list ->
  ?interval:float ->
  ?edit_interval:float ->
  ?bindings_interval:float ->
  ?appview:string ->
  unit ->
  'a
(** [run ~sw ~clock ~api ~bot ~messages ~accounts ()] reports, forever, every
    pipeline of the repositories that [accounts] bound to [bot]'s rooms, as
    {!Bindings.read} finds them every [bindings_interval] seconds, 60 by
    default.

    A pipeline's message is sent when it is first seen and edited in place as it
    changes, at most once every [edit_interval] seconds, 10 by default, so that
    a busy pipeline does not hit the homeserver's rate limits. The last edit
    always says how the pipeline ended. Pipelines are polled every [interval]
    seconds, 15 by default, and linked to on [appview]. Pipelines that were
    running when the poster stopped are picked up again from [messages]. A room
    the bot has not joined, or whose encryption it has not learnt, holds its
    messages back until it has. *)
