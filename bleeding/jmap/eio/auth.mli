(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How a JMAP client authenticates.

    {{:https://www.rfc-editor.org/rfc/rfc8620#section-8.2}RFC 8620 §8.2} leaves
    authentication to the HTTP layer. A client presents whatever credential the
    service documents and the service answers [401] when it is missing or stale.
    The two schemes services use are named here, the bearer token of
    {{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1} and the
    basic credential of {{:https://www.rfc-editor.org/rfc/rfc7617}RFC 7617},
    without exposing the HTTP client's own credential type, so that a program
    can pick a scheme before it has an Eio environment to build a client with.

    A secret is a constant, a file read on demand, or a thunk called for every
    request. {!Client.connect} turns it into the credentials it scopes to the
    origins the session names, which is the only place a secret leaves this
    module. Secrets are immutable OCaml strings and cannot be reliably locked or
    wiped after use; avoid retaining configuration records longer than the
    client needs them and control core dumps and process inspection at
    deployment. *)

type t
(** The type for credentials. A value of this type is a scheme and a secret, or
    the absence of both. *)

(** {1 Schemes} *)

val none : t
(** [none] sends no credential. It is the default of {!Client.connect} and what
    a service reached over an already authenticated channel, such as a unix
    socket or a reverse proxy that has logged the user in, wants. *)

val bearer : string -> t
(** [bearer token] sends [Authorization: Bearer token]
    ({{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1}).
    Hosted JMAP services usually issue an API key to be presented this way.

    @raise Invalid_argument if [token] is not an RFC 6750 [b64token]. *)

val basic : user:string -> password:string -> t
(** [basic ~user ~password] sends the [Authorization: Basic] credential of
    {{:https://www.rfc-editor.org/rfc/rfc7617#section-2}RFC 7617 §2}, which a
    self-hosted server such as Cyrus expects. The pair is joined with a colon
    and base64 encoded by {!Fetch.Credential.basic}.

    @raise Invalid_argument
      if either part is not printable ASCII or [user] contains a colon. *)

val bearer_from_file : ?fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [bearer_from_file ~fs path] is {!bearer} with the token read from the first
    line of [path], trimmed of surrounding whitespace. [fs] is the filesystem
    [path] is resolved against and is usually [Eio.Stdenv.fs env]. Without it
    the file is read with the OCaml standard library instead, which is the
    fallback for a caller that has to name the file before the event loop
    starts, such as a [Cmdliner] term.

    The file is read the first time a request needs the token and the value is
    then kept, so a token rotated on disk is picked up only after {!refresh}.
    Reading is deferred so that a program can name a key file it may not read
    until it connects. A read that fails, and a file whose first line is empty,
    exceeds 64 KiB, is not regular, or has nonzero group/other permission bits,
    is reported as a {!Client.constructor-Transport} error carrying
    [Fetch.Denied] on the request that needed it. A token that is not an RFC
    6750 [b64token] is denied the same way. Symlinks are checked at their
    target. *)

val basic_from_file : ?fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [basic_from_file ~fs path] is {!basic} with [user:password] read from the
    first line of [path], on the terms of {!bearer_from_file}. A line with no
    colon or with parts that do not satisfy {!basic} is denied as a malformed
    credential. *)

val read_secret_file :
  ?fs:Eio.Fs.dir_ty Eio.Path.t -> string -> (string, string) result
(** [read_secret_file ~fs path] reads at most 64 KiB from the first line of the
    regular file [path] and applies the emptiness and permission checks of
    {!bearer_from_file}. It is intended for applications that must put a
    file-backed secret into an editable form; clients should normally retain the
    lazy {!bearer_from_file} or {!basic_from_file} credential instead. *)

val refreshing : refresh:(unit -> string) -> t
(** [refreshing ~refresh] is a bearer credential whose token is [refresh ()],
    called once for every request rather than kept. It is the hook for an OAuth
    2.0 access token
    ({{:https://www.rfc-editor.org/rfc/rfc6749#section-1.4}RFC 6749 §1.4}),
    where [refresh] consults a token store and renews an expired token. It runs
    inside the request, so it may block on Eio effects. A returned value that is
    not an RFC 6750 bearer token denies that request with [Fetch.Denied] rather
    than escaping as [Invalid_argument]. An I/O exception from [refresh] gains
    credential-refresh context while retaining its original diagnostic and
    backtrace. Cancellation and other exceptions propagate unchanged. *)

(** {1 Settings} *)

(** The type for the scheme a secret is presented with. [Bearer] is the token of
    {{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1} and
    [Basic] is the [user:password] credential of
    {{:https://www.rfc-editor.org/rfc/rfc7617}RFC 7617}. *)
type scheme = Bearer | Basic

val scheme_of_string : name:string -> string -> (scheme, string) result
(** [scheme_of_string ~name s] is [Bearer] for ["bearer"], [Basic] for
    ["basic"], and an error for anything else. [name] is how the setting that
    supplied [s] is named in that error, such as ["JMAP_AUTH"] or ["--auth"]. *)

val of_scheme :
  ?fs:Eio.Fs.dir_ty Eio.Path.t ->
  key_name:string ->
  auth_name:string ->
  scheme ->
  [ `Key of string | `File of string ] ->
  (t, string) result
(** [of_scheme ~fs ~key_name ~auth_name scheme secret] presents [secret] with
    [scheme]. [`Key k] holds the secret itself and [`File path] names a file
    holding it, on the terms of {!bearer_from_file}. [fs] is passed to
    {!bearer_from_file} and defaults to absent.

    It is an error for [Bearer] with a [`Key k] that is not an RFC 6750
    [b64token], or for [Basic] with a [`Key k] that has no colon in it, a basic
    secret being a user and a password joined by one. [key_name] and [auth_name]
    are how the two settings are named in that error, such as ["JMAP_API_KEY"]
    and ["JMAP_AUTH=basic"] in the environment, or ["--api-key"] and
    ["--auth basic"] on a command line. A [`File path] is named rather than
    read, so its contents are checked only when a request reads it. A malformed
    file credential is then reported as [Fetch.Denied], not as an uncaught
    exception. *)

val of_env :
  ?prefix:string ->
  ?fs:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  (t option, string) result
(** [of_env ~prefix ~fs ()] is the credential the environment describes, as
    {!Cli.config_term} reads it. [JMAP_AUTH] selects the scheme, ["bearer"] or
    ["basic"], and defaults to ["bearer"]. [JMAP_API_KEY_FILE] names a file
    whose first line holds the secret. [JMAP_API_KEY] holds the secret itself.
    The file wins over the direct key, which is the precedence of the command
    line. The settings are read with {!scheme_of_string} and {!of_scheme}, so a
    malformed one is reported in the same words wherever a program takes it
    from.

    A variable set to the empty string counts as unset, so an exported but empty
    [JMAP_API_KEY] selects no credential and an empty [JMAP_AUTH] selects the
    default scheme. It is [Ok None] when neither [JMAP_API_KEY_FILE] nor
    [JMAP_API_KEY] is set, so that a caller can fall back to {!none} or to its
    own default. It is an error when a setting is malformed, which is an unknown
    [JMAP_AUTH], an invalid bearer [JMAP_API_KEY], or a basic [JMAP_API_KEY]
    with no colon in it.

    [prefix] is the environment variable prefix without its underscore and
    defaults to ["JMAP"], so a prefix of ["ORACLE"] reads [ORACLE_API_KEY] and
    its neighbours. [fs] is passed to {!bearer_from_file} and defaults to
    absent. *)

(** {1 Use} *)

val refresh : t -> unit
(** [refresh t] drops the secret [t] has cached, so that the next request reads
    the file again. It does nothing for a constant credential and nothing for
    {!refreshing}, neither of which answers from a cache. *)

val pp : t Fmt.t
(** [pp ppf t] prints the scheme of [t] and a redacted secret, which is the
    first four characters of the secret followed by ["***"], or ["***"] alone
    for a secret of four characters or fewer. A basic credential shows its user
    in full, the user name not being the secret.

    It reads nothing. A file credential prints as [bearer <file:PATH>] or
    [basic <file:PATH>] whether or not the file has been read, so that printing
    a credential is neither a cancellation point nor a way for an unreadable
    file to fail a log line. A {!refreshing} credential prints the last token
    its thunk produced, or [<refreshing>] if it has not run. *)

val to_credentials : t -> Fetch.Credential.t list
(** [to_credentials t] is [t] as the HTTP client's own credentials, ready for
    [Fetch.with_credentials]. It is the escape hatch for code that builds its
    own client stack rather than going through {!Client.connect}. The list is
    empty for {!none}. *)
