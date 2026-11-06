# Duraml

Duraml is an Eio job queue with interchangeable memory and SQLite backends.
Jobs contain versioned data. Applications provide handlers and own external
side effects. The queue provides at-least-once execution, lease fencing,
deduplication and atomic successor publication.

## Libraries

| Library | Purpose |
| --- | --- |
| `duraml.client` | Typed queues, job handles and switch-owned workers |
| `duraml.jsont` | Optional Jsont codecs for application payloads |
| `duraml` | Resource API, provider interface and shared transition engine |
| `duraml.memory` | Transactional in-memory backend |
| `duraml.sqlite` | Durable, multi-process SQLite backend |
| `duraml.worker` | Optional structured worker pool |
| `duraml.mock` | Scripted providers and request-boundary fault injection |

Specifications, completions, jobs, queries, events and wait observations have
abstract representations with read-only accessors. Only the provider interface
exposes trusted backend restoration hooks.

The core and memory backend do not link SQLite or a JSON library. The SQLite
backend uses Jsont for its internal record encoding, versioned by the
database schema version. Applications
can use any encoding for their UTF-8 payloads.

Start with [the typed client interface](client/duraml_client.mli).
For provider development and explicit claim handling, see
[the core interface](lib/duraml.mli),
[worker interface](worker/duraml_worker.mli) and
[mock interface](mock/duraml_mock.mli).

## Choose a backend

Construct the queue at the application's composition root. Constructors take
explicit clocks, randomness and lifetime capabilities. They do not read the
environment, choose paths or start workers implicitly.

```ocaml
let store =
  match backend with
  | `Memory ->
      Duraml_memory.create ~sw ~clock:env#clock
        ~random:env#secure_random ()
  | `Sqlite path ->
      Duraml_sqlite.open_ ~sw ~clock:env#clock
        ~random:env#secure_random Eio.Path.(env#fs / path)
```

Both branches return `Duraml.plain`, an `Eio.Resource.t`. Calling code uses
`Duraml_client.Queue` capabilities without a backend switch.
Handles become unusable when their owning switch closes. Share handles
between fibers in one Eio domain. SQLite also supports separate handles in
separate processes on the same host.

Production callers should supply `env#secure_random`. Store identities and
claim tokens consume 16 bytes per value. Deterministic sources are appropriate
only in tests.

## Bind, submit and handle typed work

Link `duraml.client` and bind the request and result formats once:

```ocaml
open Duraml_client

let documents = Queue.v store ~name:"documents" ~kind:"extract" ~version:1
  ~request:Codec.utf8 ~result:Codec.utf8 in
let submitted = Queue.submit ~key:"document:123:v1" documents "123" in
let stop, request_stop = Eio.Promise.create () in
Queue.start ~sw ~mono_clock:env#mono_clock ~stop ~concurrency:4 documents
  (fun ~sw input -> Completion.succeed (extract ~sw input));
submitted, request_stop
```

Submission returns `Added job`, `Existing job` or `Conflict id`. Inspect a
handle with `Job.inspect` and bookmark `Job.id`. After reopening the store,
`Queue.find` restores a handle and checks its queue and schema. Successful
results are decoded through the bound result codec. `Job.cancel` revokes work.

`Queue.start` forks into the supplied switch. Resolve the stop promise at
shutdown to drain active handlers and let the switch close. `Queue.run` runs
the same pool in the calling fiber. Both own acquisition, renewal and settlement.
Handlers receive decoded application values and their own child switch.
Return `Completion.retry ~at ~reason` for scheduled retries, or
`Completion.fail ~code ~detail ()` for permanent failure.

Use `Codec.v ~encode ~decode` for an application encoding, or link
`duraml.jsont` and use `Duraml_jsont.codec json`. Encoding must be deterministic
for keyed deduplication. Decoder errors become fixed `invalid_payload` failures.
Decoder exceptions propagate and stop the pool. Bind one kind and version per
queue name. Incompatible stored specifications stop the runner and remain
recoverable rather than being consumed by the wrong handler, whatever their
attempt count.

Pass `~on_exn` to keep a long-running pool alive. A handler exception then
becomes the completion `on_exn` returns, and transient storage failures,
such as a busy SQLite database, are retried or left to lease expiry instead
of stopping every worker. Settlement conflicts are deterministic and still
stop the pool, as `Duraml.E (Conflict _)`.

To build an atomic pipeline, encode a successor with `Queue.stage next request`
and return `Completion.succeed ~followups:[successor] result`. Staging performs
no store I/O. Successors must belong to the same store and publish only with
the parent's terminal outcome. Ordinary `Queue.submit` publishes immediately
in a separate transaction, even when called inside a handler.

## Advanced: explicit claims

```ocaml
let spec = Duraml.Spec.v
  ~queue:"documents" ~kind:"extract" ~version:1
  ~key:"document:123:extract:v1" ~payload:"123" () in
match Duraml.enqueue store spec with
| Inserted id | Existing id ->
    Printf.printf "Job %s\n" (Duraml.Id.to_string id)
| Conflict _ ->
    failwith "This key already names different work"
```

The key is scoped to the queue. It reserves the exact specification, including
kind, version, payload and initial schedule. Completion does not release the
key. Use a new key to deliberately repeat completed work. Unkeyed enqueues
always create new jobs and cannot reconcile an uncertain enqueue response.

```ocaml
Duraml_worker.run ~mono_clock:env#mono_clock
  ~queue:"documents" ~concurrency:4 ~lease:(Ptime.Span.of_int_s 60) ~max_attempts:5
  store (fun ~sw claim ->
    let payload = Duraml.Spec.payload (Duraml.Claim.spec claim) in
    let result = extract ~sw payload in
    Duraml.complete (Succeeded result))
```

`run` blocks until cancellation or an unexpected error. Each invocation serves
one queue. Run several invocations with `Eio.Fiber.all` for independent pools.
Concurrency is per invocation, not a distributed resource quota.

Handlers return expected failures and scheduled retries:

```ocaml
Duraml.complete (Retry { at = retry_at; reason = "upstream_busy" })
Duraml.complete (Failed { code = "invalid_document"; detail = explanation })
```

Retries do not occupy a worker while waiting. Applications choose retry times
and backoff. A runner's attempt budget includes expired attempts left by
crashed workers. Primitive queue consumers can implement other budgets.

Handlers may use their child switch for concurrent work. All child fibers
finish before the completion is settled. The runner renews its claim every
third of a lease using a monotonic clock. It cancels the handler if renewal
reports lost ownership. Renewal I/O failures and unexpected handler exceptions
stop the pool and cancel sibling workers. Their uncertain claims remain until
expiry. Expected application errors should be returned, not raised.
`Duraml.complete` validates outcomes and successor budgets at construction.
An invalid completion raises before it can reach the worker attempt policy.

For graceful shutdown, pass a `unit Eio.Promise.t` as `~stop`. Resolving it
stops new handlers and drains active work for up to `~drain` seconds (default
30). Remaining handlers are cancelled and their cleanup is joined. Claims
left by cancellation, including a claim racing with stop, expire normally.
Cancelling the caller cancels the pool immediately.

The runner does not impose a handler runtime limit. Applications should bound
HTTP calls, subprocesses and other operations using their Eio capabilities.

## Claims and successors

A successful `take` grants a time-limited claim. Only the current unexpired
claim can renew or settle a job. Expiry, cancellation and takeover revoke that
authority. A new claimant receives a new generation and token.

`cancel` revokes queued or running work. A running handler observes revocation
at its next renewal. Cancellation cannot undo external effects.

```ocaml
let next = Duraml.Spec.v
  ~queue:"index" ~kind:"index" ~version:1
  ~key:"document:123:index:v1" ~payload:"123" () in
Duraml.settle store claim
  (Duraml.complete ~followups:[next] (Succeeded "extracted"))
```

Completion and successors commit in one transaction. Any conflicting successor
key rolls back the whole settlement. Retry outcomes cannot create successors.
An identical repeated settlement returns `Already_applied`, including after a
retry has started another attempt. A differing repeated settlement returns
`Settlement_conflict`. A revoked or expired claim returns `Lost_claim`.

An interrupted backend operation can commit before the caller receives its
result. Retry keyed enqueues and identical settlements to reconcile this.
Interrupted claims remain recoverable through lease expiry.

Lease fencing protects queue state, not external effects. A paused worker can
continue an external write after another worker has reclaimed its job. Use
stable application idempotency keys, conditional writes or durable publication
receipts. Do not treat a claim as an exactly-once filesystem or network lock.

## Time, queries and events

Scheduling and persisted lease deadlines use `Ptime.t` timestamps at or after
the Unix epoch. Leases, polling intervals and drain periods use `Ptime.Span.t`.
For example, `Ptime.Span.of_int_s 60` constructs a sixty-second lease.
Consumers sharing a database must use the same wall-clock source. Clock
adjustments can delay or accelerate eligibility and expiry. Fencing still
rejects old generations, but cannot stop an already-running external effect.
Worker heartbeat intervals use a supplied monotonic clock.

Dispatch is fair across lanes and oldest-first within a lane. A
specification names its lane with `?lane`. Omitted, the job is in the
unnamed lane. An explicit lane must be a nonblank name.
Each claim takes the lane of the queue served least recently, ties broken by
name, and within it the eligible job with the earliest creation time, then
the lowest ID. A lane that gains work after holding none ranks as served
just now, so it joins the back of the rotation. A retry or an expired claim
keeps its place behind no job younger than itself, and a single request
submitted during a bulk batch runs after at most one job of each other
waiting lane. Every lane with work is served once per rotation, so let
untrusted submitters choose among a bounded set of lanes.
Successors without a lane inherit the lane of the job that published them.
Lanes schedule work but do not identify it: a keyed repeat in another lane
reconciles with the stored job and keeps the stored lane. IDs are scoped to
a store. Claims additionally carry the store identity. `Duraml.query` provides
queue/state filters and an exclusive job-ID cursor. A listing page ends early
after the job that brings its payloads and results to 16 MiB, so continue
from the last job's ID until a page is empty. Event cursors are separate
increasing sequence numbers.

```ocaml
let jobs = Duraml.list store (Duraml.query ~state:Queued ~limit:50 ()) in
let changes = Duraml.events ~after:last_sequence ~limit:100 store in
consume jobs changes
```

Lifecycle events commit with their state transitions. They include enqueue,
claim, expiry, retry, completion, failure and cancellation. Lease renewals do
not produce audit noise. Payloads and results are not copied into events.
Retry reasons and failure codes are supplied by the application and must not
contain secrets. Full payloads, outcomes and attempt records remain in storage.

Audit adapters should consume this event stream and use `Duraml.identity store`
plus event sequence for delivery deduplication. Commit an
external consumer cursor only after successful delivery. The worker's optional
`on_settled` callback is a notification, not durable audit delivery.

Duraml does not implicitly delete jobs, attempts, deduplication keys or events.
Retention and export are future explicit operations.

## Backend behaviour

Memory uses immutable maps and transaction-local updates under an Eio mutex.
An exception discards all updates. Waiting uses a revision check and a condition
variable, so an enqueue between an empty poll and waiting is not lost. This
revision also changes when a renewal shortens a lease, which wakes
consumers. It is independent of the lifecycle event cursor. An idle poll
reads no randomness. This backend is intended for tests and small ephemeral
queues. Selection scans the queued and running jobs of the queue. Listings
filtered by queue, or by queue and state, read only those jobs from their
cursor. Counts without a cursor or parent filter read per-queue, per-state
totals.

SQLite stores indexed jobs, payloads, attempts, events and store metadata.
Prepared statements are kept per handle and finalized when it closes. Writes
use
short `BEGIN IMMEDIATE` transactions via `sqlite3-eio`. It samples the wall
clock after acquiring the write transaction so lock contention cannot make a
stale timestamp renew an expired claim. An idle poll reads in a deferred
transaction and takes no write lock. No handler runs under a transaction.
SQLite errors, including a busy database after the five-second busy
timeout, are `Duraml.E (Storage _)` with SQLite's error text.

Schema version 5 stores exact UTC timestamps, the dispatch lane and creation
time of every job, a lanes table ranking each lane with queued or running
work, and each queue's latest rank. Dispatch indexes let a claim seek the
first ready lane in service order and that lane's oldest eligible job. Each
payload is written once to a payloads table, and job and attempt records
leave it out, so renewals and transitions rewrite only small records.
Versions 2, 3 and 4 upgrade in place on their first writable open. Read-only
opens require the current version. Experimental version 1 stores are refused
without modification.
The database uses WAL and FULL synchronization. New files are mode 0600, and
a database path that is a symbolic link is refused.
Unrelated databases and unknown schema versions are refused. Use local storage,
not a network filesystem. Consumers poll for cross-process changes, at a
configurable interval of 0.1 seconds by default. No daemon or notification
socket is required. A changed revision causes an immediate recheck.

SQLite backups must include committed WAL state, using its backup facilities
or a coordinated checkpoint. Memory has no persistence. Neither backend
provides application-level distributed locks or publisher rate limiting.

## Mocks and testing

Use the memory backend with `Eio_mock.Backend.run_full` for deterministic
worker tests. Its wall and monotonic clocks advance without real sleeps.
Pass an `Eio.Flow.string_source` of deterministic bytes as test randomness.

`Duraml_mock.make` accepts a polymorphic handler over `Duraml.Pi.request`,
allowing scripts to return correctly typed responses. `Duraml_mock.wrap`
intercepts operations before or after forwarding to a real backend. Raising
in an `after` observer models losing the response after a successful commit.
These wrappers do not simulate storage durability or power failure.

The same conformance suite runs against memory and SQLite. Additional tests
exercise real competing SQLite processes, SIGKILL during an open transaction,
claim recovery, committed successor durability and cross-process wakeups.
Worker tests cover renewal, concurrency, retry budgets and cancellation.

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check wip/duraml
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force wip/duraml
```

## Scope

Duraml does not contain HTTP clients, publisher rules, file validation, artifact
storage, review batches or a web UI. Those belong in applications such as
Taposaur. It also does not implement workflow DAGs, cron scheduling, distributed
rate-limit permits or automatic retention. Atomic successors support simple
pipelines without requiring a general workflow engine.
