(** The Matrix account a poster posts as.

    The bot logs in once and keeps its session, device keys and encryption state
    in its state directory, so a restart is the same device. The first time a
    device runs, it is verified with the account's cross-signing keys: recovered
    from secret storage with a recovery key, or created, together with a
    recovery key, for an account that has none. Other people's clients then show
    its messages as sent by a verified device.

    It joins rooms it is invited to and sends only to rooms whose encryption it
    has learnt from a sync, so that nothing meant for an encrypted room goes out
    in the clear. *)

type t

type config = {
  user : string;  (** The bot's Matrix user ID, such as [@ci:example.org]. *)
  password : string;
      (** Its password, for the first login and for creating cross-signing keys.
      *)
  homeserver : string option;
      (** Its homeserver's client API, found from [user]'s server through
          [.well-known/matrix/client] when [None]. *)
  recovery_key : string option;
      (** The account's recovery key or passphrase, needed to verify the device
          of an account that already has cross-signing keys. *)
  allow_unencrypted : bool;  (** Also post to rooms without encryption. *)
}

val connect :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  state:Eio.Fs.dir_ty Eio.Path.t ->
  config ->
  t
(** [connect ~sw ~env ~state config] logs in, or resumes the session kept in
    [state], publishes the device's keys, verifies the device if it has not
    been, and starts syncing and sending on [sw].

    A recovery key created for the account is written to [state/recovery-key],
    readable only by its owner, and logged once. Keep it: a later device of the
    account needs it to verify itself. An account with cross-signing keys but no
    [recovery_key] runs unverified, with a warning.

    Raises [Eio.Io] if the login fails. *)

val user_id : t -> string
val device_id : t -> string

val ready : t -> room:string -> bool
(** [ready t ~room] holds when the bot has synced, has joined [room] and, unless
    [allow_unencrypted], knows [room] is encrypted. *)

val send : t -> room:string -> Render.t -> string
(** [send t ~room m] sends [m] to [room] as an [m.notice], and is its event ID.
    Raises [Failure] if it is not sent within two minutes or the server refuses
    it. *)

val edit : t -> room:string -> event:string -> Render.t -> unit
(** [edit t ~room ~event m] replaces the message [event] with [m]. It fails as
    {!send} does. *)
