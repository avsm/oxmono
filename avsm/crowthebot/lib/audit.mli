(** Durable tool-call logging, independent of Matrix delivery. *)

val run :
  Store.t ->
  actor:string ->
  room:string ->
  event:string ->
  source:string ->
  call_id:string ->
  tool:string ->
  arguments:string ->
  (unit -> (string, string) result) ->
  string
(** [run store ... f] records the call before running [f]. [Ok] is a successful
    result and [Error] a rejected call. Exceptions and cancellation are recorded
    and propagated. Memory, cron and location payloads are omitted. Other
    payloads are bounded to 4096 bytes. A logging failure prevents effects or
    propagates to the caller. *)

val line : Store.tool_use -> string
