(** matrix_cli — cmdliner terms for Matrix command-line clients.

    Each term parses one argument and nothing else, so a command combines the
    ones it needs and receives values that are already parsed. Most come in
    pairs. A [_term] makes the argument mandatory and a [_opt_term] yields
    [None] when it is absent.

    {[
    let term =
      Cmdliner.Term.(
        const run $ Matrix_cli.verbosity_term $ Matrix_cli.homeserver_term
        $ Matrix_cli.profile_term)
    ]} *)

(** {1 Connecting} *)

type http_policy = {
  retries : int option;
  rate_limit : float option;
  max_concurrent : int option;
  connect_timeout : float option;
  idle_timeout : float option;
}

val http_policy_default : http_policy
val http_policy_term : http_policy Cmdliner.Term.t

type http_options = {
  retry : Fetch.Retry.config option;
  min_interval : Duration.t option;
  max_concurrent : int option;
  connect_timeout : Duration.t option;
  idle_timeout : Duration.t option;
}

val http_options : ?homeserver:Uriz.t -> http_policy -> http_options
(** [http_options ~homeserver policy] is the Fetch configuration for [policy].
    [homeserver] selects the Matrix retry policy for that origin. Without it,
    [retry] is [None] unless [policy.retries] is supplied, and POST is disabled.

    @raise Invalid_argument
      if a rate or timeout is not finite and positive, if a timeout or
      reciprocal rate is outside the nonzero range of [Duration.t], or if the
      retry count is invalid. *)

val homeserver_term : Uriz.t Cmdliner.Term.t
(** [--homeserver URL] or [-s URL], falling back to [MATRIX_HOMESERVER]. *)

val homeserver_opt_term : Uriz.t option Cmdliner.Term.t
(** {!homeserver_term} made optional, for a command that can take the homeserver
    from a stored session instead. *)

val profile_term : string Cmdliner.Term.t
(** [--profile NAME] or [-P NAME], defaulting to ["default"]. A profile is a
    directory under [$XDG_DATA_HOME/matrix/profiles/], so one machine can hold
    several accounts. *)

(** {1 Authenticating} *)

val username_term : string Cmdliner.Term.t
(** [--username USER] or [-u USER], falling back to [MATRIX_USERNAME]. Accepts a
    localpart or a full [@user:server]. *)

val username_opt_term : string option Cmdliner.Term.t
(** {!username_term} made optional. *)

val password_env_var : string
(** ["MATRIX_PASSWORD"], the environment variable {!password_opt_term} reads. *)

val password_opt_term : string option Cmdliner.Term.t
(** [--password-file FILE], read whole with one trailing newline removed, and
    otherwise the value of {!password_env_var}. The file wins when both are
    given. There is no flag that takes the password itself, because a command
    line is readable by every process on the machine.

    The term is [None] when the file is absent and the variable is unset.
    Reading the file raises [Sys_error]. *)

val password_term : string Cmdliner.Term.t
(** {!password_opt_term} made mandatory. A command line that supplies neither
    the file nor the variable fails with {!exit_usage}. *)

type login_credentials = {
  homeserver : Uriz.t;
  username : string;
  password : string;
  profile : string;
}
(** What the password login flow needs. *)

val login_credentials_term : login_credentials Cmdliner.Term.t
(** {!homeserver_term}, {!username_term}, {!password_term} and {!profile_term}
    collected into one record. *)

(** {1 Naming a target} *)

val room_term : Matrix_proto.Id.Room_id.t Cmdliner.Term.t
(** [--room ROOM_ID] or [-r ROOM_ID], parsed with {!room_id_conv}. *)

val room_opt_term : Matrix_proto.Id.Room_id.t option Cmdliner.Term.t
(** {!room_term} made optional. *)

val recipient_term : Matrix_proto.Id.User_id.t Cmdliner.Term.t
(** [--to USER_ID] or [-t USER_ID], parsed with {!user_id_conv}, naming the
    other end of a direct message. *)

val recipient_opt_term : Matrix_proto.Id.User_id.t option Cmdliner.Term.t
(** {!recipient_term} made optional. *)

(** {1 Sending} *)

val message_term : string Cmdliner.Term.t
(** The message text, as the first positional argument. *)

val message_opt_term : string option Cmdliner.Term.t
(** {!message_term} made optional. *)

val encrypted_term : bool Cmdliner.Term.t
(** [--encrypted] or [-e], asking that a room being created be end-to-end
    encrypted. *)

(** {1 Logging} *)

val verbosity_term : unit Cmdliner.Term.t
(** [-v], repeatable, once for informational messages and twice for debug, and
    [--color] for the output style. Evaluating the term sets the [Logs] level
    and installs the global [Logs] reporter, so a command that wants logging
    must include it and a command that installs a reporter of its own must not.
*)

(** {1 Converters} *)

val user_id_conv : Matrix_proto.Id.User_id.t Cmdliner.Arg.conv
(** Parses a [@localpart:server] user id, rejecting anything else with the
    reason [Matrix_proto.Id.User_id.of_string] gave. *)

val room_id_conv : Matrix_proto.Id.Room_id.t Cmdliner.Arg.conv
(** Parses a [!opaque:server] room id, rejecting anything else with the reason
    [Matrix_proto.Id.Room_id.of_string] gave. *)

val uri_conv : Uriz.t Cmdliner.Arg.conv
(** Parses a URI reference and reports malformed input as an argument error. *)

(** {1 Exit codes}

    {!exit_ok} and {!exit_usage} are Cmdliner's own, so a command that uses them
    agrees with the codes Cmdliner returns for a parse error of its own. The
    other three are [sysexits.h] values. *)

val exit_ok : Cmdliner.Cmd.Exit.code
(** [0]. *)

val exit_usage : Cmdliner.Cmd.Exit.code
(** [124], the command line was wrong. *)

val exit_auth : Cmdliner.Cmd.Exit.code
(** [77], there is no session, or the homeserver refused the credentials. *)

val exit_network : Cmdliner.Cmd.Exit.code
(** [69], the homeserver could not be reached. *)

val exit_internal : Cmdliner.Cmd.Exit.code
(** [70], anything else. *)
