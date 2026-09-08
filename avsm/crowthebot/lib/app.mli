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
    room ID as enabled. Invitations alone never enable a room. *)

val run :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  profile:string ->
  api_key_file:string option ->
  unit
(** [run ~env ~sw ~profile ~api_key_file] runs until interrupted or the Matrix
    runtime stops. It restores the profile's encrypted Matrix device. *)

val people :
  env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> profile:string -> unit

val blogroll : env:Eio_unix.Stdenv.base -> query:string -> unit

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
