(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Shared JMAP connection profiles.

    A profile gives a name to a session URL and its credential. Profiles live
    under [$XDG_CONFIG_HOME/jmap/profiles], or [$HOME/.config/jmap/profiles]
    when [XDG_CONFIG_HOME] is unset, so every JMAP program can use the same
    login rather than growing an application-specific credential store.

    {!connect_name} is the short path from the name selected by a command line
    or user interface to a connected {!Client.t}:

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    match Profile.connect_name ~sw env "personal" with
    | Ok client ->
        let session = Client.session client in
        Fmt.pr "signed in as %s@." session.username
    | Error error -> Fmt.epr "%a@." Profile.pp_error error
    ]}

    A profile does not choose an account. One credential may expose several
    accounts, and different task clients may need different capability-specific
    primary accounts. After connecting, select an explicit account or use
    {!Jmap.Proto.Session.primary_account_for} on {!Client.session}.

    A profile file holds its secret in plain text. Stores created by {!save}
    have mode [0700] and files have mode [0600]; {!load} refuses a file with any
    group or other permission bits, a non-regular file, or a file larger than 64
    KiB. Replacing a profile is atomic.

    Secrets are ordinary immutable OCaml strings. The runtime may copy them and
    provides no reliable [mlock] or zero-after-use guarantee, so this module
    cannot promise erasure from process memory. Keep profile values scoped to
    the connection setup that needs them and apply the usual deployment policy
    for core dumps and process inspection. *)

(** {1 Profiles} *)

(** The credential recorded in a profile. *)
type credential =
  | Bearer of string  (** An RFC 6750 bearer token issued by the service. *)
  | Basic of { user : string; password : string }
      (** An RFC 7617 user and password. *)

type t
(** The type for a validated profile. *)

(** The failures of profile validation, storage and connection. *)
type error =
  | Invalid_profile of string
      (** A profile name or one of its fields is invalid or incomplete. *)
  | Storage_error of { path : string; message : string }
      (** A profile path could not be read or written safely. The message keeps
          the filesystem cause and identifies the attempted profile operation.
      *)
  | Connection_error of Client.error  (** The session could not be fetched. *)

val v : name:string -> session_url:string -> credential -> (t, error) result
(** [v ~name ~session_url credential] is a profile after validating the name,
    URL field and credential. Surrounding whitespace is removed from
    [session_url] and a basic [user]; secrets are kept exactly.

    A name is non-empty, is not ["."] or [".."], and contains only ASCII
    letters, digits, dots, hyphens and underscores. The session URL must fit on
    one line and contain no C0, DEL, or C1 control character. A bearer token
    must satisfy the syntax enforced by {!Auth.bearer}, and a basic user and
    password the syntax enforced by {!Auth.basic}, which admits printable ASCII
    alone, so no stored value can carry a control character. A basic user must
    be non-empty and contain no colon. Empty secrets are rejected. *)

val valid_name : string -> bool
(** [valid_name name] reports whether [name] can safely be a profile file name,
    on the terms of {!v}. *)

val name : t -> string
(** [name profile] is the name of [profile]. *)

val session_url : t -> string
(** [session_url profile] is its JMAP session-resource URL. *)

val credential : t -> credential
(** [credential profile] is its credential. This exposes the secret and should
    not be printed or retained longer than the profile needs to be used. *)

val auth : t -> Auth.t
(** [auth profile] is the credential as an {!Auth.t}. *)

val pp : t Fmt.t
(** [pp] prints a profile with its secret redacted. *)

(** {1 Stores} *)

type store
(** A directory reached through an Eio filesystem capability. *)

val of_directory : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> store
(** [of_directory ~fs directory] is a profile store at [directory], resolved
    through [fs]. It need not exist yet. This is useful for a confined
    application store or a test; most programs want {!xdg_store}. *)

val xdg_store : Eio_unix.Stdenv.base -> (store, error) result
(** [xdg_store env] is the shared store under [$XDG_CONFIG_HOME/jmap/profiles].
    An unset or relative [XDG_CONFIG_HOME] falls back to
    [$HOME/.config/jmap/profiles]. It is an error when neither variable names an
    absolute directory. *)

val directory : store -> string
(** [directory store] is the directory string used to construct [store]. *)

val pp_error : error Fmt.t
(** [pp_error] prints one line describing a profile failure. *)

val error_to_string : error -> string
(** [error_to_string error] is {!pp_error} as a string. *)

val load : store -> string -> (t, error) result
(** [load store name] reads [name] from [store]. The file consists of
    [key=value] lines for [url], [auth], [user] and [secret]. [auth] is [bearer]
    or [basic], and [user] is required only for basic authentication. Unknown
    lines are ignored so a newer writer can extend the format.

    A [secret] keeps the whitespace around it, the secret being whatever the
    line holds, while the other values are trimmed. A hand-edited
    [secret = token] therefore carries a leading space and is rejected as an
    invalid credential.

    The file must be regular, at most 64 KiB, and have no group or other
    permission bits. The store directory must have no group or other permission
    bits. Symlinks are checked at their target. Eio cancellation propagates;
    filesystem failures are returned as {!Storage_error}. *)

val list : store -> (t list, error) result
(** [list store] is every valid, readable profile in bytewise name order. A
    missing store is an empty list. Invalid names and individual profiles that
    {!load} would reject are omitted, which lets an optional profile picker
    remain usable. A store directory with group or other permission bits and a
    failure to inspect the directory are returned as {!Storage_error}. *)

val save : store -> t -> (unit, error) result
(** [save store profile] atomically creates or replaces [profile]. Missing
    directories are created with mode [0700], and the profile with mode [0600].
    An existing store directory with group or other permission bits is rejected
    without changing its mode. An encoded profile over 64 KiB is rejected before
    an existing file is touched. Eio cancellation propagates. *)

(** {1 Connecting} *)

val connect :
  sw:Eio.Switch.t ->
  ?transport:Transport.t ->
  ?timeout:float ->
  ?allow_insecure:bool ->
  Eio_unix.Stdenv.base ->
  t ->
  (Client.t, error) result
(** [connect ~sw env profile] fetches the session URL of [profile] with its
    credential and returns the connected client. [transport] defaults to the
    environment transport constructed by {!Client.connect_env}; an I/O failure
    during that construction is returned as a connection error. [timeout] and
    [allow_insecure] have the meanings they do for {!Client.connect}; there is
    no deadline by default, and cleartext credentials are refused by default. A
    transport failure retains its structured cause and identifies the profile by
    its validated name.

    @raise Invalid_argument
      when [timeout] is invalid or is used with a [transport] carrying no clock,
      on the terms of {!Client.connect}. *)

val connect_name :
  sw:Eio.Switch.t ->
  ?store:store ->
  ?transport:Transport.t ->
  ?timeout:float ->
  ?allow_insecure:bool ->
  Eio_unix.Stdenv.base ->
  string ->
  (Client.t, error) result
(** [connect_name ~sw env name] loads [name] from the shared XDG store and
    passes it to {!connect}. [store] selects a different store, for example one
    constructed with {!of_directory}. A successful result owns a fetched JMAP
    session, available with {!Client.session}. *)
