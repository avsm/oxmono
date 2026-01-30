open! Core
open! Async_kernel
open! Import

(** A [Buffered_output] processes log operations, batching writes before sending to the
    output, and also sequencing them with flushes and rotations. *)
type t

val create
  :  flush:(unit -> unit Deferred.t)
  -> rotate:(unit -> unit Deferred.t)
  -> write:(Message_event.t Queue.t -> unit Deferred.t)
  -> t

val flushed : t -> unit Deferred.t
val rotate : t -> unit Deferred.t
val write : t -> Message_event.t -> unit

(** If one of the operations raises, the processing loop stops and [background_error] is
    filled. *)
val background_error : t -> exn Deferred.t

module Private : sig
  val batch_size : int
end
