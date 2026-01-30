open! Core
open Async_kernel

(** A mutable state machine used to implement polling state RPCs backed by a single bus

    This is similar to an [Mvar.t], except [unsubscribe]/[subscribe] causes us to drop any
    pending [take]s.

    This is similar to a [Bvar.t], except we store the latest broadcasted value, allowing
    [take] to return an immediately determined value when the bus is ahead of the client. *)

type 'a t

val create : unit -> 'a t

(** Immediately return the last untaken value, or return a [Deferred.t] that will get
    resolved the next time a value is written to the bus.

    If there are multiple calls to [take] before a value is written to the bus, they will
    all return the same [Deferred.t] *)
val take : 'a t -> 'a Deferred.t

(** Reset the state and unsubscribe from the bus. Any pending calls to [take] will remain
    undetermined indefinitely.

    This should be sufficient since the polling state RPC client should only maintain one
    in-flight RPC at a time, aborting any previous requests. Since garbage-collecting a
    [Deferred.t] also garbage-collects any registered callbacks, this should not cause a
    memory leak. *)
val unsubscribe : 'a t -> unit

(** Switch over to a new bus, unsubscribing to the previous bus if necessary. *)
val subscribe : 'a t -> ('a -> unit, [> read ]) Bus.t -> unit
