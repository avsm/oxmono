(** main — the command line around a bot or one-shot Matrix action.

    {!run_mode} evaluates a [cmdliner] term beside the profile options,
    connects, runs the selected mode, and exits with a status a shell can test.
*)

(** A command mode selected after command-line parsing and run inside the same
    authenticated context. [Run_bot spec] runs until the bot stops or receives a
    termination signal. [Run_once action] calls [action] once, returns its exit
    status, and installs no signal handler. *)
type mode =
  | Run_bot of Bot.spec
  | Run_once of (Context.t -> Cmdliner.Cmd.Exit.code)

val run_mode :
  name:string ->
  ?version:string ->
  ?doc:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?encrypt:bool ->
  ?persist_events:bool ->
  mode Cmdliner.Term.t ->
  'a
(** [run_mode ~name mode] evaluates a command whose own options select either a
    long-running bot or a one-shot action. Profile, login and logging options
    are added once around both alternatives. Encryption state is saved before a
    one-shot returns, raises or is cancelled; {!Bot.run} provides the same
    guarantee for a bot. *)

val run :
  name:string ->
  ?version:string ->
  ?doc:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?encrypt:bool ->
  ?persist_events:bool ->
  Bot.spec Cmdliner.Term.t ->
  'a
(** [run ~name spec] never returns. [name] is the program name in the usage and
    the manual page. [version] is what [--version] prints, and without it the
    program has no [--version]. [doc] is the one-line synopsis, and [man] the
    manual page sections after it. Without them the page carries neither.

    The term's own options are joined with [--homeserver], [--username],
    [--password-file], [--profile] and the log-level options. [encrypt] and
    [persist_events] are passed to {!Context.connect} and are both [true] by
    default.

    A failed login prints its reason and exits with status 77. A bot that stops
    itself exits 0, and so does a signal, after the encryption state is saved.
*)

val run_once :
  name:string ->
  ?version:string ->
  ?doc:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?encrypt:bool ->
  ?persist_events:bool ->
  (Context.t -> Cmdliner.Cmd.Exit.code) Cmdliner.Term.t ->
  'a
(** [run_once ~name action] is {!run_mode} specialized to [Run_once]. It
    evaluates a command with the standard profile, login and logging options,
    then calls [action] with the connected context. It exits with the action's
    status. Unlike {!run}, it installs no signal handlers and does not start a
    bot runtime. The encryption state is saved before the context's switch is
    torn down, including when [action] raises or is cancelled. [encrypt] and
    [persist_events] are passed to {!Context.connect} and are both [true] by
    default. *)

val run_once_with_context :
  Context.t -> (Context.t -> Cmdliner.Cmd.Exit.code) -> Cmdliner.Cmd.Exit.code
(** [run_once_with_context ctx action] runs [action] in an already-connected
    context and returns its shell status. It saves the encryption state before
    returning, including when [action] raises or is cancelled. The caller owns
    the context's switch; this function is useful when composing a one-shot
    operation into a larger Eio program. *)

val plugin_flag :
  name:string ->
  doc:string ->
  Bot.plugin Cmdliner.Term.t ->
  Bot.plugin Cmdliner.Term.t
(** [plugin_flag ~name ~doc plugin] is a [--name] flag that enables [plugin] and
    otherwise is the identity, for an executable that offers several. [plugin]
    is a term rather than a plugin so that a plugin with options of its own can
    read them. [Cmdliner.Term.const] is the rest. *)

val compose :
  Bot.plugin list Cmdliner.Term.t -> Bot.spec -> Bot.spec Cmdliner.Term.t
(** [compose plugins base] is [base] with every enabled plugin applied to it, in
    the order [plugins] lists them. *)
