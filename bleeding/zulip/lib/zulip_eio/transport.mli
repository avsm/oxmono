(** Reusable HTTP capabilities for requests and independent long polls. *)

type t
(** A capability borrowing its Eio environment or injected Fetch handlers. *)

val v :
  ?https:Fetch_httpz.https ->
  ?retry:Fetch.Retry.config ->
  ?max_concurrent:int ->
  ?idle_timeout:float ->
  < net : _ Eio.Net.t
  ; clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  t
(** [v ~https ~retry ~max_concurrent ~idle_timeout env] is a transport using
    [env]. [https] defaults to the standard verified TLS connector. A supplied
    connector configures trust and client certificates. [max_concurrent]
    defaults to [8]. [idle_timeout] is the inactivity timeout in seconds and
    defaults to [30.]. Cookies are disabled.

    [retry] defaults to {!Fetch.Retry.v} with three retries. Its allowed methods
    are restricted to GET and HEAD. Mutating requests are never replayed by this
    transport. Each long poll has a separate concurrency allowance and no
    transport retries. Keep [env] alive for all requests.

    @raise Stdlib.exception-Invalid_argument
      if [max_concurrent] is nonpositive, [idle_timeout] is nonfinite,
      nonpositive, or outside the supported duration range, or if [retry] is
      invalid. *)

val of_fetch :
  ?clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  ?poll_fetch:Fetch.plain ->
  _ Fetch.t ->
  t
(** [of_fetch ~clock ~poll_fetch fetch] is a transport borrowing [fetch].
    [clock] defaults to absent. Without it, explicit request deadlines and event
    polling are unavailable. [poll_fetch] defaults to [fetch]. A separate
    handler permits polling while ordinary requests use a constrained connection
    pool. Retry, TLS, and resource-lifetime policies remain those of the
    supplied handlers. *)

val fetch : t -> Fetch.plain
(** [fetch transport] is its ordinary request capability. *)

val poll_fetch : t -> timeout:float -> Fetch.plain
(** [poll_fetch transport ~timeout] is its long-poll capability for a deadline
    of [timeout] seconds. A transport from {!v} allows five extra seconds of
    network inactivity. Injected handlers ignore this argument.

    @raise Stdlib.exception-Invalid_argument
      for a transport from {!v} if [timeout] plus five seconds cannot be
      represented as a duration. *)

val clock : t -> float Eio.Time.clock_ty Eio.Resource.t option
(** [clock transport] is its wall clock, or [None] for a clockless injection. *)
