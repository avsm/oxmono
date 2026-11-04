(** Publisher and service credentials from an encrypted keychain.

    {1 NAME}
    Taposaur_secrets - named credentials for acquisition strategies.

    {1 SYNOPSIS}
    {[
      let secrets = Taposaur_secrets.open_ ~sw ~fs:env#fs () in
      match Taposaur_secrets.find secrets Elsevier_api_key with
      | Some key -> Sekrit.Secret.with_value key connect
      | None -> ()
    ]}

    {1 DESCRIPTION}
    Credentials live in a {!Sekrit} vault under the [taposaur/] prefix, one
    name per credential, written with [sekrit put NAME]. The vault and its
    master key are the user's default Sekrit paths, which
    [SEKRIT_STORE_DIR] and [SEKRIT_KEY_FILE] relocate. Opening without a
    vault succeeds with no credentials, so commands that need none still run.

    Each credential may also come from a process environment variable, which
    the keychain overrides when both exist. The variables remain for hosts
    whose secrets are injected by a service manager. Values never appear in
    errors, listings or audit records. Service addresses are not secrets and
    stay in flags and environment variables. *)

type credential =
  | Elsevier_api_key
  | Elsevier_inst_token
  | Springer_api_key
  | Wiley_tokens  (** Comma-separated tokens. *)
  | Core_api_token

val credentials : credential list
(** [credentials] lists every credential in the order of {!name}. *)

val name : credential -> string
(** [name c] is the vault name, such as [taposaur/elsevier/api-key]. *)

val variable : credential -> string
(** [variable c] is the environment fallback, such as [ELSEVIER_API_KEY]. *)

val of_name : string -> credential option
(** [of_name s] inverts {!name}. *)

type t
(** Opened credential sources. *)

type origin = Keychain | Environment
(** Where a present credential comes from. *)

val open_ : ?env:(string -> string option) -> sw:Eio.Switch.t ->
  fs:_ Eio.Path.t -> ?root:_ Eio.Path.t -> ?key_file:_ Eio.Path.t -> unit -> t
(** [open_ ~sw ~fs ()] opens the default vault when its directory exists,
    reading it through [fs]. [root] and [key_file] select another vault.
    [env] defaults to [Sys.getenv_opt] and supplies fallbacks. A vault that
    exists but cannot be opened raises [Sekrit.Error]. The handle expires
    with [sw]. *)

val keychain : t -> bool
(** [keychain t] is true when a vault was opened. *)

val find : t -> credential -> Sekrit.Secret.t option
(** [find t c] resolves the credential afresh from the vault, else from its
    environment variable. Blank values count as absent. *)

val origin : t -> credential -> origin option
(** [origin t c] reports where {!find} would take the value from. *)

val value : t -> credential -> (unit -> string) option
(** [value t c] is a closure that exposes the credential on each call, for
    service clients that take [api_key:(unit -> string)]. Rotation in the
    vault is seen on the next call. *)
