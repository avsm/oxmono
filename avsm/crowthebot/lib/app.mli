(** Profile-aware command-line operations. Network access uses [env]'s Eio
    capabilities. Profile operations require a native filesystem for SQLite and
    a process lock. All clients and databases live under [sw]. *)

val init :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  admin:string ->
  homeserver:string ->
  unit

val login :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  username:string ->
  password_file:string option ->
  unit
(** [login ~env ~sw ~profile ~username ~password_file] logs in with a private
    password file, or prompts without echo when no file is supplied. *)

val join :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  room:string ->
  unit
(** [join ~env ~sw ~profile ~room] joins an ID or alias and records the returned
    room ID as enabled for group commands. Accepted direct invitations are
    tracked separately by [run]. *)

val run :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  api_key_file:string option ->
  unit
(** [run ~env ~sw ~profile ~api_key_file] runs until interrupted or the Matrix
    runtime stops. It restores the profile's encrypted Matrix device. *)

val verify :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  user:string option ->
  listen:bool ->
  room:string option ->
  recovery_key_file:string option ->
  unit
(** [verify ~env ~sw ~profile ~user ~listen ~room ~recovery_key_file] runs one
    terminal SAS verification on the saved device. [user] defaults to the admin.
    [listen] waits for that user's request. [room] is an optional room ID for
    in-room verification. A private recovery-key file imports Crow's existing
    cross-signing identity and signs its device. It never resets the identity.
    Stop [run] first because the profile lock is exclusive. *)

val people :
  env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> profile:string -> unit

val memory :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  command:string ->
  unit
(** [memory ... ~command] runs a local memory command as the profile admin. *)

val tools :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  day:string option ->
  after:int ->
  unit
(** [tools ... ~day ~after] prints up to 100 log entries after [after]. The day
    defaults to today in UTC. *)

val note :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  day:string option ->
  generate:bool ->
  api_key_file:string option ->
  unit
(** [note ... ~day ~generate] reads yesterday's note by default. [generate] uses
    the configured OpenRouter client to generate a missing or outdated note for
    a completed day. Local commands require the profile to be stopped. *)

val feeds :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  command:string ->
  unit
(** [feeds ... ~command] inspects, polls or removes feed state locally while the
    profile is stopped. Add subscriptions from Matrix to set their source. *)

val probe :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  api_key_file:string option ->
  unit
(** [probe ~env ~sw ~profile ~api_key_file] sends a synthetic prompt to the
    configured model. It does not connect to Matrix. *)

val complete :
  Eio_unix.Stdenv.base -> Config.t -> Openrouter.t -> Engine.complete
(** [complete env config client messages tools] runs one native model request
    with a 90-second deadline. Empty tool lists omit the wire tool fields. *)

val configure :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  (Secret_store.t -> unit) ->
  unit
(** [configure ~env ~sw ~profile action] runs an operator configuration action.
    The store is separate from the database and all runtime tool capabilities.
*)
