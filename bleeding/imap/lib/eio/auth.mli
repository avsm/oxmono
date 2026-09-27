(** Password and bearer credentials. [Auto] prefers advertised SASL PLAIN
    over TLS, then CRAM-MD5, then LOGIN. On explicitly insecure transports it
    prefers CRAM-MD5 before LOGIN. An explicit SASL mechanism must be advertised;
    no authentication failure triggers fallback. TLS is required for PLAIN,
    OAUTHBEARER and LOGIN, including Auto's LOGIN fallback, unless
    [allow_insecure_transport] opts out for an isolated fixture. CRAM-MD5 does
    not protect subsequent mailbox traffic. *)
type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
type t

exception Invalid_credentials
(** Raised when a resolved secret is invalid, when a provider raises, or when
    the username cannot be used with the chosen mechanism. *)

val password : username:string -> password:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> unit -> t
(** [password ~username ~password ?mechanism ?allow_insecure_transport ()]
    holds a fixed password. [mechanism] defaults to [`Auto] and
    [allow_insecure_transport] to [false]. It raises [Invalid_argument] for
    an empty, non-UTF-8 or control-character username, a password containing
    NUL, [`Oauthbearer], or [`Cram_md5] with a username containing
    whitespace. *)
val refreshing : username:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t
(** [refreshing ~username ?mechanism ?allow_insecure_transport get] calls
    [get] for the password at each authentication. It checks [username] and
    [mechanism] as {!password} does. *)
val bearer : username:string -> token:string ->
  ?allow_insecure_transport:bool -> unit -> t
(** [bearer ~username ~token ?allow_insecure_transport ()] holds a fixed
    OAUTHBEARER token. [allow_insecure_transport] defaults to [false]. It
    raises [Invalid_argument] for an invalid username, or a token that is
    empty, longer than 32 KiB or not an RFC 6750 b64token. *)
val refreshing_bearer : username:string ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t
(** [refreshing_bearer ~username ?allow_insecure_transport get] calls [get]
    for the token at each authentication. *)
val username : t -> string
val mechanism : t -> mechanism
val allow_insecure_transport : t -> bool
val resolve_password : t -> string
(** [resolve_password t] is the current password. It raises
    {!Invalid_credentials} if it contains NUL or its provider raises. *)

val cram_md5_response : t -> string -> string
(** [cram_md5_response t] checks the username and resolves the password,
    raising {!Invalid_credentials} on failure. The resulting function maps a
    challenge to the RFC 2195 response, Base64 of
    [username SP lowercase-HMAC-MD5]. *)

val plain_response : t -> string
(** [plain_response t] is Base64 of RFC 4616 [NUL authcid NUL password]. It
    raises {!Invalid_credentials} for an empty or non-UTF-8 password. *)

val oauthbearer_response : t -> string
(** [oauthbearer_response t] is Base64 of the RFC 7628 GS2 header and Bearer
    authorization pair. *)
