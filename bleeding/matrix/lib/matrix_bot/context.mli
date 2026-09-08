(** context — what a bot runs with.

    A context bundles an Eio environment and switch, a logged-in client, and
    optionally the encryption machine, event store and plugin store that go with
    them. {!connect} builds one from a profile on disk, so a bot comes back as
    the same device with the same keys. {!v} bundles pieces a caller has already
    made. *)

type clock = float Eio.Time.clock_ty Eio.Std.r
(** The type for the clock a bot measures timeouts against. *)

type t
(** The type for contexts. *)

val v :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  client:Matrix_eio.Client.t ->
  ?clock:clock ->
  ?encryption:Matrix_eio.Encryption.t ->
  ?event_store:Matrix_ui.Event_store.t ->
  ?plugin_store:Plugin_store.t ->
  unit ->
  t
(** [v ~env ~sw ~client ()] is a context over already-built pieces. [clock]
    defaults to the environment's. [encryption] is the machine the runtime
    decrypts and encrypts with, and without it the bot reads and writes
    plaintext rooms only. [event_store] is where the event cache persists, and
    without it the cache starts empty on every run. [plugin_store] defaults to
    {!Plugin_store.memory}, which forgets on exit. *)

(** {1 Profiles} *)

(** The type for why {!connect} refused. *)
type error =
  | Missing_credential of { profile : string; needs : string }
      (** [needs] names the option the profile has no stored session to stand in
          for. *)
  | Session_unreadable of { profile : string; error : Matrix_client.Error.t }
      (** The profile's stored session file exists but does not parse. *)
  | Session_unwritable of Matrix_client.Error.t
      (** Logging in succeeded but the new session could not be stored. *)
  | Login_failed of string
      (** Logging in failed. The string is the homeserver's message. *)
  | Crypto_store of string
      (** The profile's crypto store could not be opened or created. The string
          is the underlying error's message. *)
  | Key_upload of string
      (** Uploading this device's keys failed. The string is the homeserver's
          message. *)
  | Event_store of { path : string; error : Matrix_ui.Event_store.Error.t }
      (** The event cache at [path] could not be opened. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints [e] as a sentence fit for a command line, without a
    trailing newline. *)

val error_to_string : error -> string
(** [error_to_string e] is {!pp_error} into a string. *)

val connect :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  ?homeserver:Uriz.t ->
  ?username:string ->
  ?password:string ->
  ?encrypt:bool ->
  ?persist_events:bool ->
  unit ->
  (t, error) result
(** [connect ~env ~sw ~profile ()] restores the session stored under
    [$XDG_DATA_HOME/matrix/profiles/<profile>/], or logs in with [homeserver],
    [username] and [password] and saves the session there. The three are needed
    only when there is no stored session, and their absence is
    {!Missing_credential}.

    It opens the crypto store and publishes this device's keys unless [encrypt]
    is [false], which it is not by default. It opens the profile's plugin store,
    and, when [persist_events] is [true], a SQLite event store beside it so a
    restarted bot resumes where it stopped. [persist_events] defaults to
    [false]. *)

val save : t -> unit
(** [save t] writes the encryption machine back to its store. {!Bot.run} calls
    it on the way out. A long-running bot may call it more often. It does
    nothing without an encryption machine, and reports a failure on
    {!Logging.src} rather than raising. *)

(** {1 Pieces} *)

val env : t -> Eio_unix.Stdenv.base
(** [env t] is the environment [t] was built with. *)

val switch : t -> Eio.Switch.t
(** [switch t] is the switch [t] was built with, which outlives the bot. *)

val clock : t -> clock
(** [clock t] is what a bot's timeouts are measured against. *)

val client : t -> Matrix_eio.Client.t
(** [client t] is the logged-in client every request goes through. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id t] is who the bot is logged in as. *)

val encryption : t -> Matrix_eio.Encryption.t option
(** [encryption t] is the encryption machine, or [None] when the bot runs
    without one. *)

val event_store : t -> Matrix_ui.Event_store.t option
(** [event_store t] is where the event cache persists, or [None] when it does
    not. *)

val plugin_store : t -> Plugin_store.t
(** [plugin_store t] is where plugins keep their values. *)

val profile_dir : t -> Eio.Fs.dir_ty Eio.Path.t option
(** [profile_dir t] is the profile directory, for a plugin that keeps a file of
    its own. It is [None] for a context built by {!v}. *)
