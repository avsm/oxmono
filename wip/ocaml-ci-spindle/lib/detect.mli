(** Running brief in a container on an OCluster pool.

    The [detect] workflow of {!Spindle_brief} gets its report from a job that an
    OCluster worker runs: a container built from a Debian image that fetches the
    latest brief release from GitHub, checks it against the release's checksums,
    copies the commit the worker checked out into it and runs [brief -json]
    there as an unprivileged user. The spindle's host neither checks out the
    repository nor runs brief. *)

val default_image : string
(** [default_image] is [debian:13-slim]. *)

val spec : ?image:string -> string -> string
(** [spec ~image refreshed] is the OBuilder spec of the job, from [image],
    {!default_image} by default. [refreshed] is text within the command that
    installs brief, such as a date, so that a worker's cached image is built
    again, with the release that is latest then, when it changes. *)

val report_of_log : string list -> (string, string) result
(** [report_of_log lines] is the JSON that brief wrote in the job's log [lines],
    which follows the operation that runs it, from its [{] line to its last [}]
    line, or why there is none. *)

val scan :
  ?image:string ->
  ?today:(unit -> string) ->
  pool:string ->
  clock:_ Eio.Time.Mono.t ->
  Ocluster_client.scheduler ->
  Spindle_brief.scan
(** [scan ~image ~today ~pool ~clock scheduler] submits {!spec} to [pool] with
    the commit as its source and is the report it writes. [today ()] is the
    [refreshed] text, the UTC date by default, so the image fetches the latest
    release again each day. The job's log streams into the step, up to brief's
    own output, and the workflow is pending while the job waits in the pool's
    queue. Cancelling the workflow cancels the job, given [clock] for the
    scheduler to answer within 30 seconds. It raises when the job fails or
    writes no report. *)
