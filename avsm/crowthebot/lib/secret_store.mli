(** Operator-only named configuration. Never pass this capability to a tool. *)

type t

val validate_name : string -> unit

val with_dir : sw:Eio.Switch.t -> Eio.Fs.dir_ty Eio.Path.t -> (t -> 'a) -> 'a
(** [with_dir ~sw dir f] opens a private directory and locks configuration
    updates for the duration of [f]. Files must be owned by the current user
    with mode 0600. Directories must have mode 0700. Symlinks are rejected. *)

val with_xdg :
  sw:Eio.Switch.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  profile:string ->
  profile_dir:Eio.Fs.dir_ty Eio.Path.t ->
  (t -> 'a) ->
  'a
(** [with_xdg ~sw ~fs ~profile ~profile_dir f] opens
    [XDG_CONFIG_HOME/crowthebot/secrets/PROFILE]. Configuration must be outside
    the Matrix profiles directory, including when XDG paths contain symlinks. *)

val list : t -> tool:string -> (string * bool) list
val get : t -> tool:string -> name:string -> Jsont.json option
val selected : t -> tool:string -> (string * Jsont.json) option
val put : t -> tool:string -> name:string -> replace:bool -> Jsont.json -> unit
val remove : t -> tool:string -> name:string -> unit
val rename : t -> tool:string -> name:string -> into:string -> unit
val select : t -> tool:string -> name:string -> unit
