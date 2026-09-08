(** Command-line entry points for Zulip bots.

    Each entry point defines a Cmdliner command, evaluates it from [Sys.argv],
    and terminates the process with Cmdliner's exit status. *)

val run :
  name:string ->
  ?version:string ->
  ?doc:string ->
  Zulip_bot.Bot.spec ->
  unit ->
  unit
(** [run ~name ~version ~doc spec ()] evaluates and runs the command named
    [name] for [spec]. [version] defaults to no reported version. [doc] defaults
    to [Run a Zulip bot]. The command runs under Eio with a fresh switch,
    resolves the selected Zulip profile, optionally imports a zuliprc before
    connecting, and exits through Cmdliner.

    Temporary SIGINT and SIGTERM handlers request {!Zulip_bot.Bot.stop} during
    the bot run. The previous handlers are restored when the run finishes. *)

val run_once :
  name:string ->
  ?version:string ->
  ?doc:string ->
  (Zulip_bot.Context.t -> unit) ->
  unit ->
  unit
(** [run_once ~name ~version ~doc action ()] evaluates the command named [name]
    and calls [action] once with a connected context. [version] defaults to no
    reported version. [doc] defaults to [Run a Zulip bot]. The command uses the
    same profile resolution and optional zuliprc import as {!run}, runs under
    Eio with a fresh switch, and exits through Cmdliner. It does not install
    signal handlers. *)
