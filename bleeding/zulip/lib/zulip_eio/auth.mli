(** Zulip site credentials and standard zuliprc imports. *)

type t
(** The type for validated credentials for one Zulip site. *)

val create :
  site:string -> email:string -> api_key:string -> (t, Error.t) result
(** [create ~site ~email ~api_key] is a credential set. [site] accepts an HTTP
    or HTTPS URL, or a hostname with an optional port. A missing scheme becomes
    HTTPS. Trailing slashes are removed. Query strings, fragments, URL user
    information, invalid URLs, empty credentials and invalid Basic-auth text
    return [Error.Invalid_request]. Cleartext use is controlled by
    {!Client.create}. *)

val site : t -> string
(** [site auth] is the normalized site URL. *)

val email : t -> string
(** [email auth] is the API user's email address. *)

val api_key : t -> string
(** [api_key auth] is the secret API key. *)

val credential : t -> Fetch.Credential.t
(** [credential auth] is the validated Basic-auth credential for Fetch. *)

val of_zuliprc : string -> (t, Error.t) result
(** [of_zuliprc text] is the credential set in the [api] section of [text]. The
    [email], [key] and [site] fields are required. Whitespace is trimmed.
    Whole-line [#] and [;] comments and unknown sections or keys are ignored.
    Inline comments remain part of values. Interpolation is unsupported.
    Duplicate supported keys, invalid credentials and input over 64 KiB return
    [Error.Invalid_request]. *)

val load_zuliprc : Eio.Fs.dir_ty Eio.Path.t -> (t, Error.t) result
(** [load_zuliprc path] is the credential set read from [path] using
    {!of_zuliprc}. Filesystem failures and files over 64 KiB return
    [Error.Storage]. Cancellation propagates. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf auth] prints the site and email. It never prints the API key. *)
