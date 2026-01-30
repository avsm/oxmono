open! Core
open! Async

type t

val create
  :  implementations:'s Rpc.Implementations.t
  -> initial_connection_state:(Rpc.Connection.t -> 's)
  -> t

val shutdown : t -> unit
val client_connection : t -> Rpc.Connection.t Deferred.t
