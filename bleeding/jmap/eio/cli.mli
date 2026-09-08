(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Command line configuration for a JMAP tool.

    A JMAP client needs a session URL, a credential and, usually, an account id.
    The [Cmdliner] terms below read them from the command line and the
    environment, and two functions turn the result into a {!Client.t}.

    A command line flag wins over an environment variable, which wins over the
    corresponding field of a selected profile, which wins over the default. The
    variables are [JMAP_SESSION_URL], [JMAP_API_KEY], [JMAP_API_KEY_FILE],
    [JMAP_AUTH], [JMAP_ACCOUNT_ID] and [JMAP_PROFILE], and {!env_docs} describes
    them for a manual page.

    {!val-main} is the whole of a small JMAP program apart from what it does
    with the account it is given. The generated command accepts either the URL
    and credential settings or [--profile NAME].
    {[
    let capabilities = Jmap.Proto.[ Capability.core; Capability.mail ]

    let () =
      Jmap_eio.Cli.main "inbox" ~doc:"List the inbox" @@ fun ctx ->
      let g, response =
        Jmap_eio.Client.chain_exn ctx.client ~capabilities
          (Jmap.Chain.mailbox_get ~account_id:ctx.account_id ())
      in
      List.iter print_mailbox (Jmap.Chain.parse_exn g response).list
    ]} *)

(** {1 Configuration} *)

(** The type for where a configuration value came from. *)
type source =
  | Default
  | Env of string  (** The environment variable the value was read from. *)
  | Profile of string  (** The named profile the value was read from. *)
  | Cmdline

(** The type for how the API key is presented to the server. *)
type auth_scheme = Auth.scheme =
  | Bearer  (** [Authorization: Bearer <api_key>]. *)
  | Basic  (** [Authorization: Basic], with [api_key] as [user:password]. *)

type config = {
  profile : string option;  (** The selected shared profile, when any. *)
  profile_source : source;
  session_url : string;
      (** The URL of the session resource, or empty until {!resolve} supplies
          the selected profile's URL. *)
  session_url_source : source;
  api_key : string;
      (** The secret itself, empty when [api_key_file] names the file holding it
          or until {!resolve} supplies the selected profile's secret. *)
  api_key_source : source;
  api_key_file : string option;
      (** The file holding the secret, when one was named. It is not read while
          the command line is parsed. {!connect} reads it through the
          environment's filesystem capability, so an unreadable file is a
          connection failure rather than a usage error. *)
  auth : auth_scheme;
      (** The scheme the secret is presented with. Before {!resolve}, this is
          {!Bearer} when a profile is the only configuration. *)
  auth_source : source;
  account_id : string option;  (** The account to act on, when one was named. *)
  account_id_source : source;
  allow_insecure : bool;
      (** Whether credentials may be sent over cleartext HTTP. *)
  debug : bool;  (** Whether {!val-debug} prints. *)
}
(** The type for the configuration of a JMAP command. *)

val pp_source : source Fmt.t
(** [pp_source ppf s] prints [s]. *)

val pp_config : config Fmt.t
(** [pp_config ppf cfg] prints the set fields of [cfg], each with the source it
    came from. The API key is masked and a key file is named rather than read.
    An unset profile, an empty session URL and an absent account id print
    nothing, and so does the authentication scheme when no key, key file or
    explicit scheme was given. *)

val terminal_text : string -> string
(** [terminal_text text] renders C0, DEL, and C1 control characters visibly.
    Apply it to server-supplied strings before printing them to a terminal.
    Printable UTF-8 is preserved. *)

(** {1 Terms} *)

val config_term : config Cmdliner.Term.t
(** [config_term] reads a whole {!type-config} from the command line and the
    environment. A selected [--profile] or [JMAP_PROFILE] supplies a missing
    session URL and credential when {!connect} has an Eio filesystem capability
    with which to load it. Explicit URL, key and auth settings override those
    profile fields.

    It reports a missing session URL or API key when there is no profile, an
    unsafe profile name, an unknown [JMAP_AUTH] and a basic credential that is
    not [user:password] as usage errors, so evaluating it fails the command with
    the exit code [Cmdliner.Cmd.Exit.cli_error] rather than raising. *)

val session_url_term : string option Cmdliner.Term.t
(** [session_url_term] reads [--url]. *)

val api_key_term : string option Cmdliner.Term.t
(** [api_key_term] reads [--api-key]. Its help warns that a command-line secret
    is visible in the process list and points to [--api-key-file] and
    [JMAP_API_KEY]. *)

val api_key_file_term : string option Cmdliner.Term.t
(** [api_key_file_term] reads [--api-key-file]. *)

val account_id_term : string option Cmdliner.Term.t
(** [account_id_term] reads [--account]. *)

val profile_term : string option Cmdliner.Term.t
(** [profile_term] reads [--profile] or [JMAP_PROFILE]. The profile is a name in
    the shared {!Profile.xdg_store}; a caller passes it to
    {!Profile.connect_name}. *)

val debug_term : bool Cmdliner.Term.t
(** [debug_term] reads [--debug]. *)

val allow_insecure_term : bool Cmdliner.Term.t
(** [allow_insecure_term] reads [--allow-insecure]. *)

val env_docs : string
(** [env_docs] describes the environment variables and their precedence, for a
    manual page. *)

(** {1 Clients} *)

val default_timeout : float
(** [default_timeout] is [60.], the exchange and download-idle timeout used by
    {!connect} and {!create_client} unless the caller supplies one. *)

val auth_value :
  ?fs:Eio.Fs.dir_ty Eio.Path.t -> config -> (Auth.t, string) result
(** [auth_value ~fs cfg] is {!Auth.of_scheme} over the scheme of [cfg] and
    either the API key of [cfg] or the key file it names. [fs] is the filesystem
    the key file is read through and defaults to absent, in which case
    {!Auth.bearer_from_file} falls back to the OCaml standard library.
    {!connect} passes [Eio.Stdenv.fs env]. A configuration containing only a
    profile name must first be passed to {!resolve}; {!connect} does that
    itself. A malformed manually constructed configuration is an error rather
    than an anonymous credential. *)

val resolve : Eio_unix.Stdenv.base -> config -> (config, string) result
(** [resolve env cfg] loads fields needed from the profile selected by [cfg], if
    any, and fills a missing session URL or credential from it. Explicit
    command-line or environment fields win. Values supplied by the profile have
    {!constructor-Profile} as their source. The returned configuration no longer
    needs the profile file to connect.

    It is an error when the shared store cannot be located, or the selected
    profile is missing, unsafe or malformed. Filesystem access uses
    [Eio.Stdenv.fs env], and Eio cancellation propagates. *)

val connect :
  sw:Eio.Switch.t ->
  ?timeout:float ->
  Eio_unix.Stdenv.base ->
  config ->
  (Client.t, Client.error) result
(** [connect ~sw ~timeout env cfg] fetches the session named by [cfg] over the
    default stack of {!Transport.v} and is a JMAP client for it, authenticated
    with {!auth_value} reading a key file through the filesystem capability of
    [env]. It first applies {!resolve}, so [cfg] may select a shared profile
    instead of containing a URL and credential itself.

    [sw] is the switch the client's background work belongs to, passed on to
    {!Client.connect}. A command wraps its body in [Eio.Switch.run]. [timeout]
    is the number of seconds allowed for each exchange and for an idle blob
    read, as in {!Client.connect}, and defaults to {!default_timeout}. Call
    {!Client.connect} directly when no deadline is wanted.

    Credentials travel over cleartext only when [cfg.allow_insecure] is [true].
    The command-line configuration sets it only for [--allow-insecure]. See the
    option of the same name on {!Client.connect}.

    A library or a test wants this form, where a failed connection is a value to
    report. {!create_client} is the same thing for a command that should die on
    it. *)

val create_client :
  sw:Eio.Switch.t ->
  ?timeout:float ->
  Eio_unix.Stdenv.base ->
  config ->
  Client.t
(** [create_client ~sw ~timeout env cfg] is {!connect}, printing the error with
    {!Client.pp_error} and exiting with status 1 on failure. *)

val account_id :
  ?capability:string -> config -> Client.t -> (Jmap.Proto.Id.t, string) result
(** [account_id ~capability cfg client] is the account id named by [cfg], or the
    primary account for [capability] when [cfg] names none. [capability]
    defaults to the mail capability. The error holds a message suitable for a
    command line tool, for an invalid account id or a session with no matching
    primary account. *)

(** {1 Commands} *)

type context = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  config : config;
  client : Client.t;
  account_id : Jmap.Proto.Id.t;  (** The account {!val-account_id} resolved. *)
}
(** The type for what {!val-main} hands the body of a command. [env] is the
    environment of the [Eio_main.run] the body runs under and [sw] is the switch
    [client] was connected on. *)

val main :
  ?doc:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?capability:string ->
  string ->
  (context -> unit) ->
  unit
(** [main name f] evaluates the command [name], whose command line is
    {!config_term}, and runs [f ctx] for the configuration it read. It never
    returns, since it ends by exiting with the status of the evaluation.

    [f] is called under [Eio_main.run] and an [Eio.Switch.run], with a client
    connected by {!connect} and the account id of {!val-account_id}. It is the
    body of a JMAP program with nothing of the frame around it left to write.

    [capability] is passed to {!val-account_id} and so chooses which primary
    account the context holds when the command line names none. It defaults to
    the mail capability; a contacts program passes
    [urn:ietf:params:jmap:contacts]. [doc] is the one line description of the
    command and defaults to none. [man] is the manual page and defaults to none.
    An ENVIRONMENT section holding {!env_docs} as preformatted text is appended
    to it either way.

    The status is [0] if [f] returns and [1] if the connection fails, if no
    account can be resolved, or if [f] raises one of the exceptions a JMAP
    program is expected to let through, which are {!Client.Jmap_client_error}
    from the [_exn] functions of {!Client}, {!Jmap.Chain.Parse_error} from
    {!Jmap.Chain.parse_exn} and {!Jmap.Chain.parse_all_exn}, {!Sync.Sync_error}
    from the [_exn] functions of {!Sync}, and [Failure]. Each is printed on
    stderr under a [name] prefix. A command line error exits with
    [Cmdliner.Cmd.Exit.cli_error] and any other exception with
    [Cmdliner.Cmd.Exit.internal_error]. *)

val main' :
  ?doc:string ->
  ?man:Cmdliner.Manpage.block list ->
  ?capability:string ->
  args:'a Cmdliner.Term.t ->
  string ->
  (context -> 'a -> unit) ->
  unit
(** [main' name ~args f] is {!val-main} for a command that has options of its
    own. [args] is the term of those options and its value is the second
    argument of [f]. It is evaluated beside {!config_term}, so the command
    accepts both. *)

(** {1 Debugging} *)

val debug : config -> ('a, Format.formatter, unit) format -> 'a
(** [debug cfg fmt] prints a message on stderr when [cfg] has debugging enabled,
    and prints nothing otherwise. *)
