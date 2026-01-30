open! Core
open! Async_kernel
open! Import
include module type of Async_log_kernel.Log

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  include module type of Async_log_kernel.Log.Private

  (** This function allows certain logging outputs that close at shutdown to close after
      all other logs created as part of shutdown handlers have been flushed. *)
  val register_logging_shutdown_handler : (unit -> unit Deferred.t) -> unit
end
