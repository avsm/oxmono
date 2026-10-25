(** Dooit configuration. Loading resolves paths but does not create stores,
    initialize Git or resolve referenced secrets. *)

type secret =
  | File of string
  | Inline of string
  | Reference of Sekrit.Reference.t

type backend = Git | Webdav | Local

type webdav = {
  url : string;
  subdir : string;
  collection : string;
  username : string;
  secret : secret;
}

type t = {
  path : string;
  root : string;
  backend : backend;
  git : Git.config;
  webdav : webdav option;
  jmap_profile : string option;
  jmap_account : string option;
  jmap_service : string option;
}

val backend_name : backend -> string

val parse : path:string -> default_root:string -> string -> t
(** [parse ~path ~default_root raw] validates TOML and resolves relative paths
    against the config directory. It retains named secret references unchanged. *)

val defaults :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  ?path:string ->
  ?root:string ->
  ?backend:backend ->
  unit ->
  t

val load :
  fs:Eio.Fs.dir_ty Eio.Path.t -> ?path:string -> ?root:string -> unit -> t
(** [load ~fs ?path ?root ()] reads the selected configuration. Explicit path
    and root overrides are relative to the current directory. Inline secrets
    require a private configuration file. Referenced secrets are not loaded. *)

val save_new : t -> unit
(** [save_new config] writes a private config, refusing an existing file. *)

val collection : url:string -> subdir:string -> string
val require_webdav : t -> webdav
val require_git : t -> Git.config

val password : webdav -> string
(** [password config] resolves the configured secret when constructing a
    connection, preserving spaces and applying the single-line password rule.
    This is synchronous. The returned string must not be logged. *)
