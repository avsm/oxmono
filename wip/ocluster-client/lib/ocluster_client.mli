(** A client for an OCluster scheduler's submission service. *)

type submission
(** A capability to submit jobs to the scheduler's pools. *)

type ticket
(** A queued job, which may not have started. *)

type job
(** A job a worker has taken, or will take. *)

exception Error of string
(** [Error message] reports a failed RPC or a failed job. *)

val connect : sw:Eio.Switch.t -> _ Eio.Net.t -> string -> submission
(** [connect ~sw net path] connects to the submission service named by the
    capability file [path], which holds a [capnp://] URI with the scheduler's
    certificate fingerprint. The connection lasts until [sw] finishes. *)

val submit :
  submission ->
  pool:string ->
  ?urgent:bool ->
  cache_hint:string ->
  ?src:string * string list ->
  string ->
  ticket
(** [submit s ~pool ~cache_hint ~src spec] queues the OBuilder [spec], an
    S-expression, on [pool]. [src] is a Git repository URL and the commits a
    worker checks out before building. [cache_hint] groups jobs that share
    caches on a worker. [urgent] defaults to [false]. *)

val job : ticket -> job
(** [job ticket] is the job of [ticket]. Calls on it wait until a worker takes
    it. *)

val cancel_ticket : ticket -> unit
(** [cancel_ticket ticket] removes a queued job, or cancels a started one. *)

val log : job -> int64 -> string * int64
(** [log job start] is the log from offset [start] and the offset that follows
    it. It waits for more output, and is [("", start)] at the end of the log. *)

val stream_log : job -> (string -> unit) -> unit
(** [stream_log job f] passes each chunk of the job's log to [f] until the log
    ends. *)

val result : job -> (string, string) result
(** [result job] waits for [job] to finish. It is [Ok output] on success and
    [Error message] on failure. *)

val cancel : job -> unit
(** [cancel job] asks the worker to stop [job]. *)

val release : ticket -> unit
(** [release ticket] drops the reference to [ticket]. A job whose ticket is
    released is cancelled by the scheduler. *)
