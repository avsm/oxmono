(** Named XDG profiles and private credential files.

    Configuration defaults to [~/.config/zulip/profiles/NAME.json]. Data
    defaults to [~/.local/share/zulip/profiles/NAME]. XDG directory variables
    override these bases. [ZULIP_CONFIG_DIR] and [ZULIP_DATA_DIR] take
    precedence over the corresponding XDG bases. The [zulip] subdirectory is
    appended to either base. Directory variables must be absolute. Filesystem
    operations may create the application's XDG directories. Cancellation
    propagates. *)

type t
(** The type for named credential profiles. *)

val create : name:string -> auth:Auth.t -> (t, Error.t) result
(** [create ~name ~auth] is a named credential profile. [name] must start with
    an ASCII letter or digit and contain at most 64 ASCII letters, digits, dots,
    hyphens or underscores. Invalid names return [Error.Invalid_request]. *)

val name : t -> string
(** [name profile] is its local profile name. *)

val auth : t -> Auth.t
(** [auth profile] is its credential set, including the secret API key. *)

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> (t, Error.t) result
(** [load ~fs name] is the profile read from the XDG configuration directory
    through [fs]. The directory must be private and the file must be a private
    regular file, with no group or other access. Files over 64 KiB and
    filesystem or XDG failures return [Error.Storage]. Invalid contents return
    [Error.Json] or [Error.Invalid_request]. *)

val save : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (unit, Error.t) result
(** [save ~fs profile] persists credentials in a private file, replacing any
    profile with the same name atomically. The profile directory is created with
    mode [0700] and new files with mode [0600]. Existing insecure directories
    and filesystem failures return [Error.Storage]. *)

val resolve :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  ?site:string ->
  ?email:string ->
  ?api_key:string ->
  string ->
  (t, Error.t) result
(** [resolve ~fs ~site ~email ~api_key name] is the credential profile assembled
    for [name]. Each optional argument defaults to its [ZULIP_SITE],
    [ZULIP_EMAIL], or [ZULIP_API_KEY] environment variable, then the stored
    field. An explicit empty argument is an override and fails credential
    validation. A missing file is allowed when other sources supply all
    credentials. A malformed existing file remains an error even when all fields
    are overridden. Resolution does not save credentials. *)

val import_zuliprc :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  name:string ->
  Eio.Fs.dir_ty Eio.Path.t ->
  (t, Error.t) result
(** [import_zuliprc ~fs ~name path] is the profile imported from [path] and
    saved under [name], replacing that profile if present. Import and storage
    failures are returned. *)

val data_dir :
  fs:Eio.Fs.dir_ty Eio.Path.t -> t -> (Eio.Fs.dir_ty Eio.Path.t, Error.t) result
(** [data_dir ~fs profile] is its private XDG data directory, created when
    needed. Existing directories must exclude group and other access. Invalid
    XDG paths, filesystem failures and insecure permissions return
    [Error.Storage]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf profile] prints its name, site and email. It never prints the API
    key. *)
