open! Core
open Async_kernel
open! Import

module type S = sig
  type t [@@deriving equal]

  val hash : t -> int
  val flushed : t -> unit Deferred.t
  val is_closed : t -> bool
  val flush_and_close : t -> unit Deferred.t
end

type 'a t

val create : (module S with type t = 'a) -> 'a t

(** [register t entry] causes [entry] to be closed when it's GC'd, and flushed (if not
    already closed) at shutdown. *)
val register : 'a t -> 'a -> unit

val live_entries_flushed : _ t -> unit Deferred.t
