(** Run a compiled site against synthetic requests, with no sockets.

    The request goes through {!Proffer.Serve.handle}, the same code a socket
    backend calls, so a test sees the real dispatch, conditional and HEAD
    behaviour. *)

val request :
  ?headers:(string * string) list ->
  ?body:string ->
  'env Proffer.Compiled.t ->
  'env ->
  Proffer.Method.t ->
  string ->
  Proffer.Serve.outcome
(** [request compiled env meth target] dispatches one request. A [`Stream] body
    is run to completion into a buffer and comes back as [`String], with
    [content_length] set to what was collected. *)
