(** Typed queues and structured workers.

    {1 NAME}
    Duraml_client - application-facing capabilities over a Duraml store.

    {1 SYNOPSIS}
    {[
      let queue = Queue.v store ~name:"extract" ~kind:"document" ~version:1
        ~request:Codec.utf8 ~result:Codec.utf8 in
      let submitted = Queue.submit ~key:"document:123:v1" queue "123" in
      Queue.start ~sw ~mono_clock:env#mono_clock ~concurrency:4 queue
        (fun ~sw request -> Completion.succeed (extract ~sw request));
      submitted
    ]}

    Open [Duraml_client] for this example. Construct [store] using either
    [Duraml_memory] or [Duraml_sqlite]. Handlers receive decoded requests and
    return typed completions. Clients do not acquire, renew or settle claims.

    {1 ARCHITECTURE}
    A queue capability binds a store, name, kind, version and two codecs.
    Submission encodes requests. Workers decode them and encode successful
    results. Job handles bind result decoding to a store-local identifier.
    All durable transitions use the core Duraml engine. Lease renewal, handler
    cancellation, attempt budgets and graceful drain use [Duraml_worker].

    One queue name must have one kind and schema version among its active
    workers. Binding is local, not a persisted schema registration. Use a new
    queue name when old and new workers must coexist. An incompatible stored
    specification stops the runner with {!Queue.Schema_mismatch}, leaving the
    claim to expire instead of discarding work intended for another version.
    Decode failures for a matching schema become permanent [invalid_payload]
    failures without invoking the handler. Codec exceptions are programming
    errors and propagate, cancelling the worker pool.

    {1 EFFECTS AND SUCCESSORS}
    {!Queue.stage} encodes work without publishing it. Attach staged submissions
    to {!Completion.succeed} or {!Completion.fail} for atomic publication with
    the current job's outcome. All successors must belong to the same store.
    A conflict rolls back the outcome and every successor and stops the pool.
    Calling {!Queue.submit} inside a handler publishes immediately in a separate
    transaction. No API makes arbitrary external effects exactly once.

    {1 LIFETIME}
    Handles borrow their underlying store and become unusable when it closes.
    Share capabilities between fibers in one Eio domain. {!Queue.run} blocks in
    the calling fiber. {!Queue.start} explicitly forks into a supplied switch.
    Worker failures propagate through that switch. Codec construction, binding
    and submission never start workers implicitly. *)

module Codec : sig
  type 'a t
  (** An application encoding. No JSON dependency is imposed. *)
  val v : encode:('a -> string) -> decode:(string -> ('a, string) result) ->
    'a t
  (** [v ~encode ~decode] constructs a codec. Encoding must be deterministic
      for keyed submissions and completion reconciliation. Bytes must be UTF-8
      and obey Duraml's 1 MiB payload/result budget. Decoding malformed data
      should return [Error]. Exceptions from either callback propagate.
      Version both request and result formats with the queue schema version. *)
  val utf8 : string t
  (** [utf8] preserves valid UTF-8 strings without further encoding. *)
end

module Submission : sig
  type t
  (** Encoded work staged for atomic successor publication. It captures the
      target store identity and specification, but owns no store connection.
      Reusing unkeyed staged work can create duplicates. *)
end

module Completion : sig
  type 'a t
  (** A typed handler outcome with optional staged successors. *)
  val succeed : ?followups:Submission.t list -> 'a -> 'a t
  (** [succeed ~followups value] completes with [value]. Successors are
    validated
      now. Result encoding and its byte budget are checked before settlement. *)
  val retry : at:Ptime.t -> reason:string -> 'a t
  (** [retry ~at ~reason] schedules another attempt without occupying a worker.
      The timestamp and reason are validated immediately. No successors are
      allowed. The worker may replace a final-attempt retry with failure. *)
  val fail : ?followups:Submission.t list -> code:string -> detail:string ->
    unit -> 'a t
  (** [fail ~code ~detail ()] records a permanent failure. Failure fields and
      successors obey {!Duraml.complete}'s budgets and validate immediately.
      Failure codes and retry reasons appear in durable audit events. *)
end

module Job : sig
  type 'a t
  (** A handle to a durable job and its typed result. *)
  type 'a status = Queued | Running | Done of 'a
    | Failed of {code : string; detail : string}
    | Cancelled | Invalid_result of string | Missing
  (** [Invalid_result] reports undecodable or inconsistent terminal data without
      altering the job. Decoder diagnostics may include sensitive input. *)
  val id : 'a t -> Duraml.Id.t
  (** [id job] is its store-local identifier, suitable for bookmarking. *)
  val inspect : 'a t -> 'a status
  (** [inspect job] reads current state and decodes successful results. A retry
      is [Queued] until reclaimed. Previous retry outcomes are not terminal. *)
  val snapshot : 'a t -> Duraml.job option
  (** [snapshot job] exposes detailed core metadata for diagnostics and audit.
    *)
  val cancel : 'a t -> bool
  (** [cancel job] revokes nonterminal work. External effects are not undone. *)
end

val attempt : unit -> (Duraml.Id.t * int) option
(** [attempt ()] is the job and attempt number whose handler is running in
    the calling fiber, or [None] outside a handler. The runner binds it for
    the handler's fiber and every fiber forked beneath it, so audit records
    and logs written by nested code can name the work they belong to. *)

module Queue : sig
  type ('request, 'result) t
  (** A typed capability borrowing a store. *)
  type 'a submission = Added of 'a Job.t | Existing of 'a Job.t
    | Conflict of Duraml.Id.t
  (** Submission outcomes preserve keyed deduplication and conflict detection.
      [Existing] includes terminal jobs. [Conflict] names a different existing
      specification with the same key. No new work is created in either case. *)
  exception Schema_mismatch of Duraml.Id.t
  (** A claimed job has a different kind or version than this runner. It remains
      running until expiry. No handler or settlement is attempted. *)
  val v : _ Duraml.t -> name:string -> kind:string -> version:int ->
    request:'request Codec.t -> result:'result Codec.t ->
    ('request, 'result) t
  (** [v store ...] validates the binding and reads the stable store identity.
      It may suspend for that read. It creates no jobs or workers and reserves
      no persistent queue schema. Both codecs must match all active workers. *)
  val name : (_, _) t -> string
  (** [name queue] is its store-local queue name. *)
  val submit : ?key:string -> ?not_before:Ptime.t -> ?lane:string ->
    ('request, 'result) t -> 'request -> 'result submission
  (** [submit queue request] encodes and enqueues a request. Keys and initial
      schedules are part of the immutable specification. Repeating the exact
      keyed submission reconciles a response lost through cancellation or I/O
      failure. Unkeyed retries of submission may create duplicate jobs.
      [lane] is the fair-share dispatch lane of {!Duraml.Spec.v}; it does
      not take part in reconciliation. *)
  val stage : ?key:string -> ?not_before:Ptime.t -> ?lane:string ->
    ('request, _) t -> 'request -> Submission.t
  (** [stage queue request] encodes and validates work without store I/O.
      Attach it to a completion to publish atomically in the same store.
      Multiple handles for the same stable store identity are permitted. A
      staged successor without a lane inherits the lane of the job that
      publishes it. *)
  val find : (_, 'result) t -> Duraml.Id.t -> 'result Job.t option
  (** [find queue id] restores a typed handle, including after reopening a
    store.
      Returns [None] for absent jobs or a different queue, kind or version. *)
  val run : mono_clock:_ Eio.Time.Mono.t -> ?concurrency:int ->
    ?lease:Ptime.Span.t -> ?max_attempts:int -> ?stop:unit Eio.Promise.t ->
    ?drain:Ptime.Span.t ->
    ?on_settled:('result Job.t -> Duraml.settlement -> unit) ->
    ?on_exn:(exn -> 'result Completion.t) ->
    ('request, 'result) t ->
    (sw:Eio.Switch.t -> 'request -> 'result Completion.t) -> unit
  (** [run queue handler] blocks until stopped, cancelled or failed. Each
    handler
      owns a child switch and must yield cooperatively. Defaults are concurrency
      4, lease 60 seconds, 5 attempts and drain 30 seconds. Resolving [stop]
      prevents new handlers, drains active ones, then cancels and joins cleanup.
      Cleanup may exceed the drain duration. Caller cancellation requests
      immediate cancellation and joins cleanup. Unexpected exceptions and
      settlement conflicts stop the pool. Unsettled claims expire normally.
      [on_settled] is a transient notification, not durable audit delivery.
      Its exceptions stop the pool. It also receives conflict/lost responses.
      [on_exn exn] is the completion of a handler that raised [exn], and
      supplying it keeps the pool running through storage failures, as
      {!Duraml_worker.run} describes. *)
  val start : sw:Eio.Switch.t -> mono_clock:_ Eio.Time.Mono.t ->
    ?concurrency:int -> ?lease:Ptime.Span.t -> ?max_attempts:int ->
    ?stop:unit Eio.Promise.t -> ?drain:Ptime.Span.t ->
    ?on_settled:('result Job.t -> Duraml.settlement -> unit) ->
    ?on_exn:(exn -> 'result Completion.t) ->
    ('request, 'result) t ->
    (sw:Eio.Switch.t -> 'request -> 'result Completion.t) -> unit
  (** [start ~sw queue handler] forks {!run} into [sw] and returns immediately.
      Validation and worker exceptions propagate through [sw]. To close [sw]
      normally, resolve [stop] or cancel its enclosing scope. The switch waits
      for worker and handler cleanup. *)
end
