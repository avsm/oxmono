(** Versioned SQLite telemetry, separate from the cache's authoritative index.
    Raw rows are pruned to 30 days and one million rows per event type at
    startup and every 1000 writes. Hourly aggregate counters survive raw-row
    pruning. Paths exclude queries. No request headers, credentials or client
    addresses are stored. *)

type transfer = {
  key : string;
  path : string;
  bytes : int;
  total : int option;
  age : float;
}

type t

val open_ : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> _ Eio.Path.t -> t

val request :
  t ->
  meth:string ->
  path:string ->
  object_key:string option ->
  range:bool ->
  status:int ->
  cache:string option ->
  bytes:int ->
  duration_us:int ->
  unit
(** [request t ...] records a completed backend event and updates its hourly
    aggregate atomically. [bytes] is the backend's transmitted body count, zero
    for HEAD. [duration_us] includes waiting, downloading and serving. *)

val download :
  t ->
  object_key:string ->
  status:int option ->
  bytes:int ->
  stored_bytes:int ->
  duration_us:int ->
  outcome:string ->
  unit
(** [download t ...] records an attempted upstream fetch. [bytes] includes
    received payload from failed attempts. [stored_bytes] counts newly installed
    blob bytes, zero when deduplicated. Outcomes are success, missing, error,
    limit or cancelled. Only the initiating request owns these costs. *)

val summary : sw:Eio.Switch.t -> _ Eio.Path.t -> string
(** [summary ~sw dir] is JSON containing hourly aggregates and schema metadata.
    It can read a running proxy's logger. *)

val export :
  sw:Eio.Switch.t ->
  _ Eio.Path.t ->
  table:[ `Requests | `Downloads ] ->
  after:int64 ->
  limit:int ->
  string
(** [export ~sw dir ~table ~after ~limit] is a JSON page ordered by row ID.
    [limit] must be between 1 and 10000. Continue after the last returned ID.
    Export before pruning if raw events need long-term retention. *)

val heartbeat :
  t ->
  session:string ->
  running:bool ->
  connections:int ->
  queued:int ->
  transfers:transfer list ->
  unit
(** [heartbeat t ...] replaces the current server gauges. [transfers] contains
    query-free paths and byte progress. The server publishes once a second and
    marks orderly stops. *)

val snapshot :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  mono_clock:_ Eio.Time.Mono.t ->
  _ Eio.Path.t ->
  string
(** [snapshot ~sw ~clock dir] reads totals, live gauges and the latest 200
    events of each kind in one read transaction. It also supports databases
    created before live gauges were introduced. *)
