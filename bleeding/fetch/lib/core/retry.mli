(** Configuration for {!Fetch.with_retry}. *)

type config = {
  max_retries : int;  (** Attempts after the first (default 3). *)
  backoff_factor : float;
      (** Attempt [n] waits [backoff_factor * 2 ^ (n - 1)] seconds
          (default 0.5). *)
  backoff_max : float;
      (** Cap on any delay, [Retry-After] included (default 120). *)
  status_forcelist : int list;
      (** Statuses to retry (default [[429; 500; 502; 503; 504]]). *)
  allowed_methods : Http.Method.t list;
      (** Methods that may be retried (default: the idempotent
          methods of
          {{:https://www.rfc-editor.org/rfc/rfc9110#section-9.2.2}RFC
          9110 §9.2.2}, namely GET, HEAD, OPTIONS, PUT and DELETE).
          Others never are. *)
  respect_retry_after : bool;
      (** Honour the [Retry-After] header (default [true]). *)
  jitter : bool;
      (** Spread each computed backoff uniformly between zero and its
          full delay (default [true]), drawing on
          {!Fetch.with_retry}'s [random]. Does not apply to a
          server-supplied [Retry-After]. *)
  retry_exception : (exn -> bool) option;
      (** Retry exceptions beyond connection failures, middleware's
          included. Never consulted for [Eio.Cancel.Cancelled], or for
          [Denied], [Tls_failure], [Body_not_replayable],
          [Invalid_url] and [Invalid_request], which a re-issue cannot
          fix. *)
  retry_response : (Middleware.request -> Middleware.response -> bool) option;
      (** Retry responses beyond [status_forcelist]. *)
}

val default : config
(** [default] is the configuration described above. *)

val v :
  ?max_retries:int ->
  ?backoff_factor:float ->
  ?backoff_max:float ->
  ?status_forcelist:int list ->
  ?allowed_methods:Http.Method.t list ->
  ?respect_retry_after:bool ->
  ?jitter:bool ->
  ?retry_exception:(exn -> bool) ->
  ?retry_response:(Middleware.request -> Middleware.response -> bool) ->
  unit -> config
(** [v ()] is {!default} with the given fields overridden. *)
