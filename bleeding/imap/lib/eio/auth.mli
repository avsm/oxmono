(** IMAP credentials and SASL responses, documented in [Imap_eio.Auth]. *)

type mechanism = [ `Auto | `Login | `Cram_md5 | `Plain | `Oauthbearer ]
type t

exception Invalid_credentials
(** Raised when a resolved secret or the username cannot be sent. *)

val password : username:string -> password:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> unit -> t @@ portable

val refreshing : username:string -> ?mechanism:mechanism ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t @@ portable

val bearer : username:string -> token:string ->
  ?allow_insecure_transport:bool -> unit -> t @@ portable

val refreshing_bearer : username:string ->
  ?allow_insecure_transport:bool -> (unit -> string) -> t @@ portable

val username : t -> string @@ portable
val mechanism : t -> mechanism @@ portable
val allow_insecure_transport : t -> bool @@ portable

val resolve_password : t -> string
(** [resolve_password t] is the current password, or raises
    {!Invalid_credentials}. *)

val cram_md5_response : t -> string -> string
(** [cram_md5_response t] resolves the credentials, raising
    {!Invalid_credentials}, and maps a challenge to its RFC 2195 response. *)

val plain_response : t -> string
(** [plain_response t] is the Base64 RFC 4616 response, or raises
    {!Invalid_credentials}. *)

val oauthbearer_response : t -> string
(** [oauthbearer_response t] is the Base64 RFC 7628 initial response. *)
