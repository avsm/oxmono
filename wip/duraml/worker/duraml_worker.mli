(** Structured workers for Duraml queues.

    {1 NAME}
    Duraml_worker - concurrent handlers with lease renewal and graceful drain.

    {1 SYNOPSIS}
    {[
      Duraml_worker.run ~mono_clock:env#mono_clock ~queue:"extract"
        ~concurrency:4 ~lease:(Ptime.Span.of_int_s 60) store
        (fun ~sw claim ->
          let input = Duraml.Spec.payload (Duraml.Claim.spec claim) in
          Duraml.complete (Duraml.Succeeded (extract ~sw input)))
    ]}

    {1 DESCRIPTION}
    Each worker claims one job, runs its handler in a child switch, waits for
    that switch's fibers to finish, and settles the completion. A sibling fiber
    renews the lease every third of its duration using a monotonic clock.
    Lost ownership cancels the handler and joins cleanup before taking more
      work.
    Handlers must yield cooperatively. Blocking a domain prevents heartbeats.
    Bound network operations and subprocesses with application timeouts.

    Return expected failures and explicit retry schedules as completions.
    Unexpected handler exceptions, renewal I/O failures and settlement conflicts
    stop the pool and cancel sibling workers. An unacknowledged external effect
    may have completed, so unsettled claims are left to expire.

    {1 SHUTDOWN}
    Resolving [stop] prevents new handlers and starts a drain interval. Active
    handlers may finish and settle. On expiry they are cancelled and cleanup is
    joined. Cleanup itself may exceed the drain interval. A claim racing with
    stop may be left to expire without invoking its handler. Caller cancellation
    requests cancellation immediately and also joins cleanup.

    {1 LIMITS}
    Concurrency and retry budgets belong to this invocation, not to the store.
    Multiple pools may serve the same queue. There is no global concurrency
    quota or publisher rate limiter. Attempt counts include expired attempts
    left by crashed workers. Primitive consumers can implement other policies.
      *)

val run :
  mono_clock:_ Eio.Time.Mono.t -> queue:string ->
  ?concurrency:int -> ?lease:Ptime.Span.t -> ?max_attempts:int ->
  ?stop:unit Eio.Promise.t -> ?drain:Ptime.Span.t ->
  ?on_settled:(Duraml.Claim.t -> Duraml.settlement -> unit) ->
  _ Duraml.t ->
  (sw:Eio.Switch.t -> Duraml.Claim.t -> Duraml.completion) -> unit
(** [run ... store handler] blocks until stopped, cancelled or failed.
    [concurrency] defaults to 4 and must be in 1..1024. [lease] defaults to
    60 seconds and must be positive. [max_attempts] defaults to 5 and must be
    positive. A claim beyond this budget fails without invoking [handler].
    A retry on the final allowed attempt becomes an [attempt_limit] failure.
    Invalid completions are rejected by {!Duraml.complete} before this policy.

    [drain] defaults to 30 seconds and must be nonnegative. It applies only
    when [stop] is supplied. Durations use {!Ptime.Span.t}, while interval
    measurement uses [mono_clock].

    [on_settled claim result] runs after each settlement response, including a
    lost claim or conflict. Callback exceptions stop the pool. Callbacks are
    transient notifications. Durable audit consumers read {!Duraml.events}. *)
