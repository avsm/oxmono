(** Password and bearer credentials. [Auto] prefers advertised SASL PLAIN
    over TLS, then CRAM-MD5, then LOGIN. On explicitly insecure transports it
    prefers CRAM-MD5 before LOGIN. An explicit SASL mechanism must be advertised;
    no authentication failure triggers fallback. TLS is required for PLAIN,
    OAUTHBEARER and LOGIN, including Auto's LOGIN fallback, unless
    [allow_insecure_transport] opts out for an isolated fixture. CRAM-MD5 does
    not protect subsequent mailbox traffic. *)
type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
type t
val password : username:string -> password:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> unit -> t
val refreshing : username:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t
val bearer : username:string -> token:string ->
  ?allow_insecure_transport:bool -> unit -> t
val refreshing_bearer : username:string ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t
val username : t -> string
val mechanism : t -> mechanism
val allow_insecure_transport : t -> bool
val resolve_password : t -> string
val resolve_token : t -> string

(** RFC 2195 response, Base64 of [username SP lowercase-HMAC-MD5]. *)
val cram_md5_response : t -> string -> string
(** Base64 of RFC 4616 [NUL authcid NUL password]. *)
val plain_response : t -> string
(** Base64 of RFC 7628 GS2 header and Bearer authorization pair. *)
val oauthbearer_response : t -> string
