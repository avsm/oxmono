(** Matrix-aware HTTP retry policy.

    Matrix uses [POST] for both read-like requests and mutations. This module
    adds only the replay-safe key-query route to Fetch's normal idempotent
    method policy. *)

val retry_request : homeserver:Client.Url.t -> Fetch.Middleware.request -> bool
(** [retry_request ~homeserver request] is the request-level veto used by {!v}.
    It approves non-[POST] requests for Fetch's ordinary method check. A [POST]
    is approved only when its canonical origin and decoded path are exactly the
    key-query endpoint obtained by appending [[/_matrix/client/v3/keys/query]]
    to [homeserver], including a configured homeserver path prefix. A query or
    fragment does not match.

    Approval never makes a streamed body or a method absent from
    [Fetch.Retry.config.allowed_methods] retryable. In particular, key claim,
    key upload, sync and Matrix mutations are not approved. *)

val v : ?max_retries:int -> homeserver:Uriz.t -> unit -> Fetch.Retry.config
(** [v ~homeserver ()] is [Fetch.Retry.default] with [POST] added to its allowed
    methods and {!retry_request} installed as the request-level veto.
    [max_retries] overrides Fetch's retry count; the other backoff, status and
    [Retry-After] defaults are unchanged.

    Raises [Invalid_argument] if [homeserver] is not a valid HTTP(S) homeserver
    URL, or contains a query or fragment. *)

val default : homeserver:Uriz.t -> Fetch.Retry.config
(** [default ~homeserver] is [v ~homeserver ()]. *)
