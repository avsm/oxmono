(** A client for an OCluster scheduler's submission service. *)

type submission
(** A capability to submit jobs to the scheduler's pools. *)

type ticket
(** A queued job, which may not have started. *)

type job
(** A job a worker has taken, or will take. *)

exception Error of string
(** [Error message] reports a failed RPC or a failed job. *)

type scheduler
(** Where a submission service is, with a vat to reach it through. *)

val scheduler : sw:Eio.Switch.t -> _ Eio.Net.t -> string -> scheduler
(** [scheduler ~sw net path] reads the capability file [path], which holds a
    [capnp://] URI with the scheduler's certificate fingerprint. Its vat, and
    the connections it makes, last until [sw] finishes. Creating a vat generates
    a key, so a long-running client keeps one. *)

val submission : scheduler -> submission
(** [submission s] connects to the submission service, reusing the vat's
    connection while it lasts. Release it with {!release_submission}. *)

val connect : sw:Eio.Switch.t -> _ Eio.Net.t -> string -> submission
(** [connect ~sw net path] is [submission (scheduler ~sw net path)]. *)

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

type day10 = {
  opam_repository : string;
      (** The opam repository's Git URL, which names the worker's mirror of it.
          Empty is ocaml/opam-repository. *)
  opam_repository_commit : string;  (** The commit to build against. *)
  ocaml_version : string;  (** Such as [ocaml.5.4.1]. *)
  with_test : bool;  (** Whether to build and run the tests. *)
  dune_args : string list;  (** Such as [["@install"; "@runtest"]]. *)
  only_packages : string list;
      (** The project's packages to build, or all of them when empty. *)
  arch : string;
  os : string;
  os_family : string;
  os_distribution : string;
  os_version : string;
      (** The platform's opam variables. Empty ones are the worker's own. *)
}
(** What a [day10 build] job builds, as Mark Elvers's day10 branch of OCluster
    defines it. *)

val submit_day10 :
  submission ->
  pool:string ->
  ?urgent:bool ->
  cache_hint:string ->
  src:string * string list ->
  day10 ->
  ticket
(** [submit_day10 s ~pool ~cache_hint ~src d] submits a [day10 build] of the
    project checked out from [src], as {!submit} does an OBuilder spec. Only
    workers started with [--day10-cache] accept it. day10 solves and builds the
    project itself. *)

val job : ticket -> job
(** [job ticket] is the job of [ticket]. Calls on it wait until a worker takes
    it. *)

val started : job -> (unit, string) result
(** [started job] waits until a worker takes [job] from its pool's queue and
    starts it, or until the scheduler gives up on it, when it is [Error]. The
    scheduler tells a submitter no more about its place in the queue. *)

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

val release_job : job -> unit
(** [release_job job] drops the reference to [job]. *)

val release_submission : submission -> unit
(** [release_submission s] drops the reference to [s]. *)
