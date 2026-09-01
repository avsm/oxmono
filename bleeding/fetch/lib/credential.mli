(** Credentials attached by {!Fetch.with_credentials}.

    Credentials are evaluated for each in-scope exchange, including retries
    and redirects, without caching. Bearer and Basic values use the same
    validation as their constant constructors. An [Invalid_argument] from
    their callback or validation becomes {!Fetch.Denied}, without exposing
    the secret or original exception message. Other exceptions propagate. *)

type t =
  | Bearer of (unit -> string)
  (** [Bearer token] is a credential that sets [Authorization] to
      ["Bearer " ^ token ()]. If [token ()] is not an
      {{:https://www.rfc-editor.org/rfc/rfc6750#section-2.1}RFC 6750 §2.1
       b64token}, the request is denied with {!Fetch.Denied}. Use {!bearer}
      for a constant token so it is checked at construction time. *)
  | Basic of (unit -> string * string)
  (** [Basic credentials] sets [Authorization] to the RFC 7617 Basic
      encoding of [credentials ()]. Both parts must be printable ASCII,
      and the user-id must contain no colon; otherwise the request is denied.
      Empty parts and spaces are permitted; a password may contain colons.
      Use {!basic} for a constant pair checked at construction time. *)
  | Header of string * (Middleware.request -> string)
  (** [Header (name, value)] is a credential that sets [name] to [value request]. This
      form supports API-key headers and request signatures. *)
  | Query of (string * string) list
  (** [Query parameters] is a credential that binds [parameters] on the request URL. Each
      binding replaces a caller-supplied binding with the same name and leaves the rest of
      the query unchanged. *)

val bearer : string -> t
(** [bearer token] is a constant {!Bearer} credential. It checks [token]
    immediately and raises [Invalid_argument] without reproducing the token in
    the exception if it is not an RFC 6750 [b64token]. *)

val basic : user:string -> password:string -> t
(** [basic ~user ~password] is a constant {!Basic} credential. Invalid values
    raise [Invalid_argument] at construction without exposing either part. *)
