(** This module configures retry behavior for {!Fetch.with_retry}. *)

type config =
  { max_retries : int
  (** [max_retries] is the number of attempts after the first. It defaults to 3. *)
  ; backoff_factor : float
  (** [backoff_factor] is the scale of exponential backoff. Retry [n] waits
      [backoff_factor * 2 ^ (n - 1)] seconds. It defaults to 0.5. *)
  ; backoff_max : float
  (** [backoff_max] is the maximum delay in seconds, including a server-supplied
      [Retry-After] delay. It defaults to 120. *)
  ; status_forcelist : int list
  (** [status_forcelist] is the list of response statuses to retry. It defaults to
      [[429; 500; 502; 503; 504]]. *)
  ; allowed_methods : Http.Method.t list
  (** [allowed_methods] is the list of methods that may be retried. The default contains
      the idempotent methods of
      {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.2} RFC 9110 §9.2.2}: GET,
      HEAD, OPTIONS, PUT, and DELETE. *)
  ; respect_retry_after : bool
  (** [respect_retry_after] is [true] when retries honour the [Retry-After] response
      header. It defaults to [true]. *)
  ; jitter : bool
  (** [jitter] is [true] when each computed backoff is spread between zero and its full
      delay, drawing from {!Fetch.with_retry}'s [random]. It does not affect a
      server-supplied [Retry-After] delay. It defaults to [true]. *)
  ; retry_exception : (exn -> bool) option
  (** [retry_exception] is the optional predicate selecting additional exceptions to
      retry, including exceptions raised by middleware. It is never called for
      [Eio.Cancel.Cancelled], or for [Denied], [Tls_failure], [Body_not_replayable],
      [Invalid_url] and [Invalid_request], which a re-issue cannot fix. It defaults to
      [None]. *)
  ; retry_response : (Middleware.request -> Middleware.response -> bool) option
  (** [retry_response] is the optional predicate selecting additional responses to retry.
      It defaults to [None]. *)
  ; retry_request : (Middleware.request -> bool) option
  (** [retry_request] is an optional narrowing predicate shared by every
      retry reason, including built-in statuses, connection failures, and
      the additive [retry_response] and [retry_exception] predicates. A
      [false] result disables retries and skips both additive predicates,
      but never rejects the initial exchange. It is evaluated once per
      request handled by {!Fetch.with_retry}, after the retry count, body,
      and method checks; each canonical redirect hop is evaluated
      separately. The request URL is already parsed and canonicalised at
      this boundary, so route policies can use
      {!Middleware.Url.path_segments}. Keep the predicate pure and fast,
      and avoid diagnostics that could expose credentials. If it raises,
      the exception propagates before the inner client is called. It is
      not called when [max_retries] is zero, the body is a [Stream], or the
      method is disallowed. It defaults to [None]. *)
  }

(** [default] is a policy with three retries, 0.5-second exponential backoff capped at 120
    seconds, jitter, [Retry-After] handling, and the status and method defaults described
    above. *)
val default : config

(** [max_retries_limit] is [100], the construction-time amplification cap. *)
val max_retries_limit : int

(** [validate config] is [()] when retry counts, delays, and statuses are
    valid. It raises [Invalid_argument] otherwise. This also validates records
    assembled directly rather than through {!v}. *)
val validate : config -> unit

(** [v ()] is {!default} with the given fields overridden. It rejects retry
    counts outside 0 through {!max_retries_limit}, non-finite or negative
    delays, and statuses outside 100 through 599. *)
val v
  :  ?max_retries:int
  -> ?backoff_factor:float
  -> ?backoff_max:float
  -> ?status_forcelist:int list
  -> ?allowed_methods:Http.Method.t list
  -> ?respect_retry_after:bool
  -> ?jitter:bool
  -> ?retry_exception:(exn -> bool)
  -> ?retry_response:(Middleware.request -> Middleware.response -> bool)
  -> ?retry_request:(Middleware.request -> bool)
  -> unit
  -> config
