(** Durable tool-call logging, independent of Matrix delivery. *)

type summary = {
  log_id : int;
  tool : string;
  argument_bytes : int;
  result_bytes : int;
  status : string;
}

val summary_line : summary -> string
(** [summary_line summary] renders metadata without argument or result contents.
*)

val run :
  ?on_finish:(summary -> unit) ->
  ?max_result_bytes:int ->
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
    stored payloads are bounded to 4096 bytes. Returned results are bounded
    to [max_result_bytes], 4096 by default. A logging failure prevents effects or
    propagates to the caller. [on_finish] receives sizes before clipping or
    redaction and the recorded status. Exceptions have zero result bytes. *)

val line : Store.tool_use -> string
