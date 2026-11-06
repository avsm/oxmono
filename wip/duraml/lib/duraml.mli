(** Durable Eio job queues.

    {1 NAME}
    Duraml - leased jobs with interchangeable storage and atomic successors.

    Most applications should start with [Duraml_client] from [duraml.client].
    This module exposes explicit claims and the backend provider protocol.

    {1 SYNOPSIS}
    {[
      let spec = Duraml.Spec.v ~queue:"extract" ~kind:"pdf" ~version:1
        ~key:"paper:123:v1" ~payload:"123" () in
      let queued = Duraml.enqueue store spec in
      let claim = Duraml.take store ~queue:"extract"
        ~lease:(Ptime.Span.of_int_s 60) in
      let result = Duraml.settle store claim
        (Duraml.complete (Duraml.Succeeded "extracted")) in
      queued, result
    ]}

    {1 DESCRIPTION}
    A store contains named queues of immutable, versioned specifications.
    Applications encode payloads and execute handlers. Duraml records claims,
    outcomes, successors and lifecycle events. A key reserves one specification
    within a queue, including after completion. An identical enqueue returns
    the existing ID. A different specification with that key conflicts.

    {1 ARCHITECTURE}
    The core is an Eio resource interface and a shared transition engine.
    [duraml.memory] supplies an ephemeral backend. [duraml.sqlite] supplies a
    local, multi-process backend. [duraml.worker] supplies an optional worker
    pool. [duraml.mock] supplies scripted providers and fault injection.
    Construct a backend once and pass its resource to application code.
    Constructors take explicit lifetime, clock and randomness capabilities.
    Creating a store does not start workers.

    A queued job becomes running when claimed. A live claim can succeed, fail
    or schedule a retry. Success and failure are terminal. Retry returns the job
    to the queue. Cancellation is terminal. An expired running job can be
    reclaimed with a new attempt number and token. Only the current unexpired
    claim may change the job. Terminal completion and successor enqueues commit
    in the same transaction. A successor conflict rolls back both.

    {1 TIME}
    Timestamps use {!Ptime.t}, from the Unix epoch through [Ptime.max].
    Durations use {!Ptime.Span.t}. Leases must be positive. Backends convert
    their supplied Eio wall clock at the boundary. Workers use an Eio monotonic clock for
    heartbeat and drain intervals. Sleep precision is that of the Eio backend.
    Persisted deadlines and retry times use wall time, so clock adjustments can
    advance or delay eligibility. Processes sharing a store must use consistent
    wall clocks. No implicit system clock is read.

    {1 CONCURRENCY AND RECOVERY}
    Share a handle between fibers in one Eio domain. SQLite additionally permits
    independent handles in separate processes on the same host. Operations are
    atomic, but cancellation or I/O failure may hide a committed response.
    Repeating the same keyed enqueue or identical settlement reconciles it.
    An interrupted claim is recovered by lease expiry.

    Execution is at least once. A lease fences queue updates, not external
    network or filesystem effects. Applications must make those effects
    idempotent or publish them conditionally. No distributed rate limit,
    filesystem lock, automatic retention or workflow graph is provided.

    {1 ERRORS}
    Constructors and operations raise [Invalid_argument] for invalid inputs.
    Backend failures use [Eio.Io (Duraml.E error, context)], including
    damaged stored records. Eio cancellation
    propagates normally. Domain outcomes such as key conflicts and lost claims
    are returned as variants. Closing the owning switch invalidates its handle.

    {1 AUDIT}
    Events commit with transitions and have increasing, store-local cursors.
    Consumers deduplicate by store identity and event sequence and advance their
    external cursor only after delivery. Payloads and results are omitted from
    events. Caller-supplied reasons and failure codes must not contain secrets.
    Jobs, attempts, keys and events remain stored until an explicit future
    retention operation is introduced. *)

(** {1 Identifiers and specifications} *)
module Id : sig
  type t
  (** A positive job identifier, scoped to one store. *)
  val to_string : t -> string @@ portable
  (** [to_string id] is its decimal storage and display form. *)
  val of_string : string -> (t, string) result @@ portable
  (** [of_string s] parses the decimal form of {!to_string}: digits without
      sign, prefix, separator or leading zero, denoting a positive signed
      64-bit integer. Every identifier therefore has exactly one string. *)
  val compare : t -> t -> int @@ portable
  (** [compare a b] orders identifiers by allocation order within a store. *)
  val equal : t -> t -> bool @@ portable
  (** [equal a b] is [compare a b = 0]. *)
  val pp : Format.formatter -> t -> unit
  (** [pp ppf id] prints {!to_string}[ id]. *)
end
module Spec : sig
  type t
  (** An immutable job specification. Construct with {!v}. *)
  val v : queue:string -> kind:string -> version:int -> payload:string ->
    ?key:string -> ?not_before:Ptime.t -> ?lane:string -> unit -> t
    @@ portable
  (** [v ... ()] validates a specification. Names, keys and lanes must be
      NUL-free UTF-8 of at most 256 bytes, not empty or only Unicode white
      space. [version] must be positive. Payloads are UTF-8 of at most
      1 MiB. Omitted [not_before] means immediately eligible. [lane] names
      the fair-share lane the job is dispatched from. Omitted, the job is in
      the unnamed lane. Deduplication compares every field except the lane,
      including the initial schedule. *)
  val queue : t -> string @@ portable
  (** [queue s] is the named queue receiving [s]. *)
  val kind : t -> string @@ portable
  (** [kind s] identifies the application handler or payload format. *)
  val version : t -> int @@ portable
  (** [version s] is the application's payload schema version. *)
  val payload : t -> string @@ portable
  (** [payload s] is the application-encoded UTF-8 data. *)
  val key : t -> string option @@ portable
  (** [key s] is its optional, queue-scoped deduplication key. *)
  val not_before : t -> Ptime.t option @@ portable
  (** [not_before s] is the initial eligibility time, if supplied. *)
  val lane : t -> string @@ portable
  (** [lane s] is its dispatch lane, or [""] for the unnamed lane. A keyed
      repeat with a different lane reconciles with the stored job and keeps
      that job's lane. *)
end

(** {1 Outcomes and snapshots} *)
type outcome = Succeeded of string
  | Retry of {at : Ptime.t; reason : string}
  | Failed of {code : string; detail : string}
(** A handler result. Retry is explicitly scheduled by the application. *)
type completion
(** A validated outcome with atomic successor specifications. *)
val complete : ?followups:Spec.t list -> outcome -> completion @@ portable
(** [complete ~followups outcome] validates a result. Success data is limited
    to 1 MiB. Reasons and failure details are UTF-8 of at most 4096 bytes.
    Failure codes obey the name rules of {!Spec.v}. At most 100 successors
    and 4 MiB of successor payloads are allowed. Successors sharing a queue
    and key must be identical. Retry cannot have successors.
    Validation occurs here, before a completion reaches a store or worker. *)
module Completion : sig
  type t = completion
  val outcome : t -> outcome @@ portable
  (** [outcome c] is the validated handler result. *)
  val followups : t -> Spec.t list @@ portable
  (** [followups c] are the successors, in enqueue order. *)
end

type state = Queued | Running | Done | Failed_job | Cancelled
(** The current job state. [Done], [Failed_job] and [Cancelled] are terminal. *)
val state_name : state -> string @@ portable
(** [state_name s] is its stable lowercase storage name. *)
type job
(** An immutable observation of a job. Fetch again to observe later changes. *)
module Job : sig
  type t = job
  val id : t -> Id.t @@ portable
  (** [id j] identifies the job within its store. *)
  val spec : t -> Spec.t @@ portable
  (** [spec j] is the original specification, unchanged across retries. *)
  val state : t -> state @@ portable
  (** [state j] is the observed lifecycle state. *)
  val attempt : t -> int @@ portable
  (** [attempt j] is the claim count, initially zero. Reclaims increment it. *)
  val available_at : t -> Ptime.t @@ portable
  (** [available_at j] is the eligibility time when queued, or the current
      lease deadline when running. Terminal jobs retain the last value. *)
  val created_at : t -> Ptime.t @@ portable
  (** [created_at j] is the enqueue time. *)
  val updated_at : t -> Ptime.t @@ portable
  (** [updated_at j] is the most recent transition or renewal time. *)
  val outcome : t -> outcome option @@ portable
  (** [outcome j] is the last settled result. A later claim retains it. *)
  val parent : t -> Id.t option @@ portable
  (** [parent j] is the job whose completion published [j] as a successor,
      or [None] for work enqueued directly. A keyed successor that
      reconciled with an existing job keeps that job's original parent. *)
end
module Claim : sig
  type t
  (** An opaque authority for one attempt in one store. *)
  val id : t -> Id.t @@ portable
  (** [id c] is the claimed job. *)
  val spec : t -> Spec.t @@ portable
  (** [spec c] is its immutable specification. *)
  val attempt : t -> int @@ portable
  (** [attempt c] is the positive claim generation. *)
  val deadline : t -> Ptime.t @@ portable
  (** [deadline c] is the deadline at issuance. Renewal does not mutate this
      snapshot. {!Job.available_at} exposes the current deadline. *)
end

type enqueue_result = Inserted of Id.t | Existing of Id.t | Conflict of Id.t
(** A new job, an identical keyed specification, or a conflicting key owner. *)
type settlement = Applied | Already_applied | Lost_claim
  | Settlement_conflict | Followup_conflict of Id.t
(** [Already_applied] means this attempt committed the identical completion.
    [Settlement_conflict] means it committed a different one. [Lost_claim]
    means no live authority remains. [Followup_conflict id] identifies the
    existing job whose specification conflicts with a successor. *)
type waiting
(** A backend-specific observation used to avoid missed wakeups. *)
module Waiting : sig
  type t = waiting
  val revision : t -> int64 @@ portable
  (** [revision w] is an opaque equality token, not an audit cursor. *)
  val next_at : t -> Ptime.t option @@ portable
  (** [next_at w] is the earliest observed eligibility or expiry time. *)
end
type poll = Ready of Claim.t | Await of waiting
(** A claim or a wait observation. Pass observations only to their own store. *)

type query
(** A validated job-list filter. *)
val query : ?queue:string -> ?state:state -> ?after:Id.t -> ?parent:Id.t ->
  ?limit:int -> unit -> query
(** [query ... ()] combines optional filters with AND. [after] is an exclusive
    ID cursor. [parent] selects the successors of one job. [limit] defaults
    to 100 and must be in 1..1000. *)
module Query : sig
  type t = query
  val queue : t -> string option @@ portable
  (** [queue q] is the queue filter, or all queues. *)
  val state : t -> state option @@ portable
  (** [state q] is the state filter, or all states. *)
  val after : t -> Id.t option @@ portable
  (** [after q] is the exclusive pagination cursor. *)
  val parent : t -> Id.t option @@ portable
  (** [parent q] restricts results to successors of that job. *)
  val limit : t -> int @@ portable
  (** [limit q] is the maximum result count. *)
end

type event
(** An immutable lifecycle event. Lease renewals do not emit events. *)
module Event : sig
  type t = event
  val sequence : t -> int64 @@ portable
  (** [sequence e] is its positive, increasing store-local cursor. *)
  val time : t -> Ptime.t @@ portable
  (** [time e] is the transition's wall-clock timestamp. *)
  val kind : t -> string @@ portable
  (** [kind e] is [enqueued], [claimed], [lease_expired], [retry_scheduled],
      [succeeded], [failed] or [cancelled]. *)
  val job : t -> Id.t @@ portable
  (** [job e] is the affected job. *)
  val attempt : t -> int @@ portable
  (** [attempt e] is the generation at the transition, zero before the
      first claim. *)
  val detail : t -> string @@ portable
  (** [detail e] is the retry reason or failure code, otherwise empty. *)
end

type error = Closed | Storage of string | Conflict of string
(** A closed handle, a backend failure, or stored work contradicting a
    worker's completion. A [Storage] failure may be transient, and a busy
    database is one. A [Conflict] is deterministic. Text is diagnostic
    only. *)
type Eio.Exn.err += E of error
val err : error -> exn
(** [err e] wraps [e] in an Eio I/O exception. *)

(** {1 Store operations} *)
type 'a t = 'a Eio.Resource.t constraint 'a = [> `Job_queue]
(** An Eio queue capability. The manifest type supports resource composition. *)
type plain = [ `Job_queue ] Eio.Resource.t
(** A store exposing only the queue capability. *)
val identity : _ t -> string
(** [identity t] is the stable store identity. Combine it with event sequence
    numbers when deduplicating delivery to an external audit consumer. *)
val enqueue : _ t -> Spec.t -> enqueue_result
(** [enqueue t s] atomically inserts or reconciles [s]. Unkeyed enqueues always
    insert and cannot reconcile a lost response. Keys survive terminal
    states. *)
val try_take : _ t -> queue:string -> lease:Ptime.Span.t -> poll
(** [try_take t ~queue ~lease] claims the next eligible job or returns a
    wait observation. It does not wait for eligibility, but storage may suspend.
    Dispatch is fair across lanes and oldest-first within a lane. The lane
    of [queue] served least recently goes first, then the eligible job with
    the earliest creation time, then the lowest ID. A lane that gains work
    after holding none ranks as served just now, so new lanes join the back
    of the rotation instead of preempting lanes already waiting. A retry or
    an expired claim keeps its place behind no job younger than itself, and
    a small submission is interleaved with a large one rather than queued
    behind it. Each lane with work is served once per rotation, so a
    submitter able to create unlimited lanes can take a matching share of
    dispatch. Expired claims receive a new generation. The deadline must
    fit within [Ptime]'s timestamp range, or [Invalid_argument] is raised
    whether or not work is eligible. *)
val take : _ t -> queue:string -> lease:Ptime.Span.t -> Claim.t
(** [take t ~queue ~lease] polls and waits until eligible work is claimed or
    the fiber is cancelled. It is safe against enqueue/wait races. *)
val renew : _ t -> Claim.t -> lease:Ptime.Span.t -> bool
(** [renew t c ~lease] sets the deadline to current time plus [lease], which
    may shorten it. It is [false] after expiry, cancellation or takeover.
    The deadline must fit within [Ptime]'s timestamp range. *)
val settle : _ t -> Claim.t -> completion -> settlement
(** [settle t c result] commits the outcome and all successors atomically.
    Identical repeats return [Already_applied], even after expiry or a later
    retry attempt. Different repeats return [Settlement_conflict]. Successor
    conflicts roll back the entire operation. Retry the identical completion
    to reconcile a response lost through cancellation or I/O failure. *)
val cancel : _ t -> Id.t -> bool
(** [cancel t id] revokes queued or running work. It is [false] for missing
    or terminal jobs. External effects are not undone. A worker observes revocation
    at its next renewal. *)
val find : _ t -> Id.t -> job option
(** [find t id] is the current snapshot, or [None] if absent. *)
val list : _ t -> query -> job list
(** [list t q] is the matching snapshots in ascending ID order. Separate
    pages are separate observations, not a snapshot of the whole store. A
    page holds at most [limit q] jobs, and ends early after the job that
    brings its payloads and results to 16 MiB, so it always holds at least
    one matching job if there is one. Continue after the last job's ID until
    a page is empty. *)
val count : _ t -> query -> int
(** [count t q] is the number of jobs matching every filter of [q],
    ignoring its limit. *)
val events : ?after:int64 -> ?limit:int -> _ t -> event list
(** [events t] is ascending events. [after] defaults to zero and is an
    exclusive nonnegative cursor. [limit] defaults to 100 and must be
    1..1000. *)
val tail : ?before:int64 -> ?limit:int -> _ t -> event list
(** [tail t] is the newest events first, those with sequence below the
    exclusive positive cursor [before], or the newest of all when it is
    omitted. [limit] defaults to 100 and must be 1..1000. A viewer seeds
    itself from the tail and then follows {!events} from the newest
    sequence it saw. *)

(** {1 Provider interface} *)
module Pi : sig
  (** Implementing backends.

      Applications use the operations above. Providers implement the typed
      request protocol and wait primitive below. Both supplied backends run
      {!execute}, so leases, deduplication and settlement share one definition.
      Custom providers can use that engine or implement the same contract.

      Each request must observe one atomic storage view. Mutations commit all
      updates and events together. Exceptions roll back all changes, including
      allocations. [await] must recheck its observation before sleeping, wake
      when eligibility changes, and remain cancellable. Bounded polling is
      permitted. A renewal may change eligibility without adding an audit event.

      Restoration functions are trusted serialization hooks. They bypass the
      application authority boundary and must not be exposed to untrusted input.
      Claims are process values, not user-facing bearer tokens. *)
  type _ request =
    | Identity : string request
    | Enqueue : Spec.t -> enqueue_result request
    | Try_take : string * Ptime.Span.t -> poll request
    | Renew : Claim.t * Ptime.Span.t -> bool request
    | Settle : Claim.t * completion -> settlement request
    | Cancel : Id.t -> bool request
    | Find : Id.t -> job option request
    | Query : query -> job list request
    | Count : query -> int request
    | Events : int64 * int -> event list request
    | Tail : int64 option * int -> event list request
  (** The typed provider protocol. Use public operations for input
      validation. *)
  val operation : 'a request -> string
  (** [operation r] is its diagnostic name, without payload or credentials. *)
  module type BACKEND = sig
    type t
    (** Provider-owned state. *)
    val call : t -> 'a request -> 'a
    (** [call t r] executes one atomic request. *)
    val await : t -> waiting -> unit
    (** [await t w] waits for a change or deadline. Spurious wakeups are
        allowed. *)
  end
  val handler : (module BACKEND with type t = 'a) ->
    ('a, [ `Job_queue ]) Eio.Resource.handler
  (** [handler backend] packages a backend for [Eio.Resource.T]. *)
  val call : _ t -> 'a request -> 'a
  (** [call t r] dispatches a raw provider request without public validation. *)
  val await : _ t -> waiting -> unit
  (** [await t w] dispatches the provider's wait operation. *)
  val waiting : revision:int64 -> next_at:Ptime.t option -> waiting
  (** [waiting ~revision ~next_at] constructs an observation for a provider. *)
  val page_bytes : int
  (** [page_bytes] is the 16 MiB of payloads and results after which a
      {!list} page ends. *)
  val job_bytes : job -> int @@ portable
  (** [job_bytes j] is the size of [j]'s payload and outcome text, as a
      {!list} page counts it. *)
  val page : job list -> job list @@ portable
  (** [page jobs] is the longest prefix of [jobs] that ends at the first job
      bringing the total {!job_bytes} to {!page_bytes}, or all of them. *)

  type attempt
  (** A stored claim and its settlement receipt. *)
  type storage = {
    identity : string;
    allocate : unit -> Id.t;
    find : Id.t -> job option;
    find_key : queue:string -> key:string -> job option;
    put : job -> unit;
    eligible : queue:string -> now:Ptime.t -> job option;
    served : queue:string -> lane:string -> unit;
    next_at : queue:string -> Ptime.t option;
    query : query -> job list;
    count : query -> int;
    attempt : Id.t -> int -> attempt option;
    put_attempt : attempt -> unit;
    emit : time:Ptime.t -> kind:string -> job:Id.t -> attempt:int ->
      detail:string -> unit;
    revision : unit -> int64;
    events : after:int64 -> limit:int -> event list;
    tail : before:int64 option -> limit:int -> event list;
  }
  (** Transaction-local storage callbacks. They must not yield. [allocate]
      returns a fresh increasing ID. [put] and [put_attempt] upsert records.
      [eligible] returns a queued or running job with deadline at or before
      [now], from the lane of [queue] served least recently, with ties
      broken by lane name, ordered within the lane by creation time then
      ID. [put] ranks a lane that gains its first queued or running job as
      served just now, and may forget a lane left with none. [served ~queue
      ~lane] records that the lane was just dispatched from, so it sorts
      after every other lane of [queue]. [count] applies a query's filters
      without its limit. [tail] returns events in descending sequence order
      below an optional exclusive cursor. [next_at] returns the earliest
      deadline in [queue] regardless of current eligibility. [emit]
      allocates an increasing event sequence. [attempt] returning [None]
      for a running job's current generation is a storage failure. Reads must see preceding writes in the same
      transaction. [revision] is a wakeup token independent of audit sequence.
      Custom stores must preserve key uniqueness and retain settlement receipts.
      The record is intentionally concrete so providers can supply callbacks. *)
  exception Abort_followup of Id.t
  (** Requests transaction rollback on successor conflict. {!settle} converts
      this exception to [Followup_conflict] after the backend has rolled
      back, so a provider must let it escape unwrapped. *)
  val execute : now:Ptime.t -> token:string -> storage -> 'a request -> 'a
  (** [execute ~now ~token storage r] applies the shared transition engine.
      Sample [now] after acquiring the transaction lock. Supply a fresh random
      [token] for [Try_take]. Commit only on success, otherwise roll back,
      and let {!Abort_followup} escape unwrapped. *)
  val idle : now:Ptime.t -> lease:Ptime.Span.t -> storage -> queue:string ->
    waiting option
  (** [idle ~now ~lease storage ~queue] is the observation [Try_take] would
      return at [now] when [queue] has no eligible job, or [None] when it
      has one. It validates [lease] as [Try_take] does.
      It reads without writing, so a provider can answer an idle poll
      without a write lock or a fresh token, then run {!execute} only when
      work exists. *)
  val time_of_float_s : float -> Ptime.t
  (** [time_of_float_s seconds] converts an Eio Unix clock reading. Raises
      [Invalid_argument] for nonfinite, pre-epoch or out-of-range readings. *)
  val clock_deadline : Ptime.t -> float
  (** [clock_deadline t] rounds a timestamp upwards to an Eio clock reading.
      Use it for absolute sleeps so sub-clock-precision deadlines do not
      round into the past and cause an immediate-wakeup loop. *)
  val random_id : _ Eio.Flow.source -> string
  (** [random_id source] consumes 16 bytes and returns hexadecimal text.
      Production callers supply [env#secure_random]. It may suspend, so call
      it before entering a transaction. *)

  val restore_job : ?parent:Id.t -> id:Id.t -> spec:Spec.t -> state:state ->
    attempt:int -> available_at:Ptime.t -> created_at:Ptime.t ->
    updated_at:Ptime.t -> outcome:outcome option -> unit -> job @@ portable
  (** [restore_job ...] rebuilds a trusted persisted job snapshot. Records
      written before successor parents were stored have no [parent]. *)
  val restore_claim : identity:string -> id:Id.t -> spec:Spec.t ->
    attempt:int -> token:string -> deadline:Ptime.t -> Claim.t @@ portable
  (** [restore_claim ...] rebuilds trusted attempt authority. *)
  val restore_attempt : claim:Claim.t -> status:string ->
    completion:completion option -> attempt @@ portable
  (** [restore_attempt ...] rebuilds a receipt. Status is [running], [expired],
      [cancelled] or [settled]. Only settled attempts carry a completion. *)
  val restore_event : sequence:int64 -> time:Ptime.t -> kind:string ->
    job:Id.t -> attempt:int -> detail:string -> event @@ portable
  (** [restore_event ...] rebuilds a trusted persisted audit event. *)
  val attempt_claim : attempt -> Claim.t @@ portable
  (** [attempt_claim a] is the recorded claim, including renewed deadline. *)
  val claim_identity : Claim.t -> string @@ portable
  (** [claim_identity c] is the issuing store identity. *)
  val claim_token : Claim.t -> string @@ portable
  (** [claim_token c] is the private attempt nonce for backend serialization. *)
  val attempt_status : attempt -> string @@ portable
  (** [attempt_status a] is its persisted lifecycle status. *)
  val attempt_completion : attempt -> completion option @@ portable
  (** [attempt_completion a] is its settlement receipt, if settled. *)
end
