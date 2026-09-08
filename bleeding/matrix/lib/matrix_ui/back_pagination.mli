(** A bounded, shared scheduler for room back-pagination.

    The scheduler is deliberately independent of any one UI consumer. It fills
    the shared {!Event_cache} and lets callers stop when their target is
    present. Forward pagination remains owned by focused views, as in the pinned
    Rust SDK. *)

type priority =
  | Low
  | Normal
  | High
      (** Scheduling priority, highest first and FIFO within one priority. *)

type stop_reason =
  | Reached_start
  | Stop_condition
  | Batch_limit
  | No_data
  | Failed of Matrix_client.Error.t
  | Stale
  | Cancelled
  | Forgotten
  | Closed  (** Why one queued run ended. *)

type run_result = {
  reason : stop_reason;
  events : Event_cache.event list;
  batches : int;
}
(** The events inserted by the run, oldest first. *)

type request = {
  room_id : Matrix_proto.Id.Room_id.t;
  priority : priority;
  batch_size : int;
  max_batches : int option;
  stop : Event_cache.event list -> reached_start:bool -> bool;
}
(** One bounded run. [stop] is called after each successful local chunk or
    server page with the newly resident events and whether that step reached the
    room start. [max_batches] counts both kinds of step. *)

type t
(** A scheduler whose workers live on the supplied switch. *)

type handle
(** A waitable, cancellable handle. Handles for the same room and priority share
    one underlying run. *)

val create :
  sw:Eio.Switch.t ->
  client:Matrix_client.Client.t ->
  event_cache:Event_cache.t ->
  ?max_concurrent:int ->
  unit ->
  t
(** [create ...] starts the bounded worker pool. [max_concurrent] defaults to
    three, matching the Rust SDK event-cache default, and must be positive. *)

val enqueue : t -> request -> handle
(** Enqueue a request. A request for a room already queued or running at the
    same priority coalesces onto that run. Invalid request bounds raise
    [Invalid_argument]. *)

val await : handle -> run_result
(** Wait for the shared run result. If the waiting fiber is cancelled, its
    handle is cancelled and the cancellation is re-raised. *)

val cancel : handle -> unit
(** Cancel this waiter. If it is the last waiter for a queued or running run,
    the underlying work is cancelled too. Idempotent. *)

val close : t -> unit
(** Close the scheduler, completing queued work as [Closed] and cancelling
    running work. Idempotent. *)
