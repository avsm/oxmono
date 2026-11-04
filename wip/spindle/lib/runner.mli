(** Execution and logs for one independently cancellable workflow. *)

type status =
  [ `Pending  (** Waiting for a slot, its limits or a builder's queue. *)
  | `Running
  | `Blocked  (** Waiting for a collaborator's approval. *)
  | `Success
  | `Failed
  | `Cancelled
  | `Timeout ]

val status_to_string : status -> string
(** [status_to_string s] is [s] as the lexicon spells it, such as ["pending"].
*)

val status_of_string : string -> status
(** [status_of_string s] is the status spelled [s], and [`Failed] for any other
    text. *)

type t = {
  job : Job.t;
  parent : string option;
      (** The workflow that spawned this one while it ran, if any. *)
  mutable status : status;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable log_seq : int64;
  log_lock : Eio.Mutex.t;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
  mutable queued : bool;
      (** Whether it waits in a builder's queue, shown as pending meanwhile. *)
}

type env = {
  system : Eio_unix.Stdenv.base;
  directory : Eio.Fs.dir_ty Eio.Path.t;
  slots : Eio.Semaphore.t;
  timeout : float;  (** Seconds a workflow may run. *)
  executor : Executor.t;
}

type input = {
  id : string;
  repo : string;
  source : string;
  commit : string;
  metadata : Jsont.json;
  variables : (string * string) list;  (** CI variables for every step. *)
  secrets : (string * string) list;  (** Secrets, hidden in the log. *)
  files : Executor.files;  (** The repository at [commit]. *)
  labels : unit -> string list;  (** As {!Executor.request} describes. *)
}

val now : unit -> string
(** [now ()] is the current time in RFC 3339, in UTC, as workflows record their
    start and end. *)

val terminal : t -> bool
(** [terminal t] holds once [t] has ended. A blocked workflow has not. *)

val v : ?parent:string -> Job.t -> t
(** [v ~parent job] is a pending workflow running [job], spawned by the workflow
    called [parent], if any. *)

val view : t -> Jsont.json
(** [view t] is [t] as an [org.tangled.ci.defs#workflow] status. *)

val snapshot : t -> Jsont.json
(** [snapshot t] is everything {!restore} needs to bring [t] back. *)

val interrupted : Jsont.json -> bool
(** [interrupted raw] holds when the workflow that snapshot [raw] holds was
    running, or waiting in a builder's queue, when it was taken. *)

val restore : ?resume:bool -> Jsont.json -> t
(** [restore ~resume raw] is the workflow [raw] holds. One {!interrupted} when
    the spindle stopped is failed, unless [resume], when it is pending again
    with an empty log, to run from the start. *)

val reset : t -> unit
(** [reset t] makes [t] pending again with an empty log. *)

val outcome : t -> Executor.outcome
(** [outcome t] is how finished workflow [t] ended, with its log as text. *)

val progress : t -> Executor.progress
(** [progress t] is how far [t] has got, with the step it is in. *)

val message : exn -> string
(** [message exn] is the error shown for a workflow that failed with [exn]: the
    text of a [Failure] or [Invalid_argument], or else the exception. *)

val execute :
  env ->
  input ->
  record:(string -> int64) ->
  persist:(unit -> unit) ->
  spawn:(env:(string * string) list -> string -> Job.step list -> unit) ->
  ?await:(unit -> t list) ->
  ?watch:(Executor.progress list option -> Executor.progress list) ->
  ?finished:(unit -> t list) ->
  ?queued_for:(float -> unit) ->
  ?interruptible:bool ->
  t ->
  unit
(** [execute env input ~record ~persist ~spawn workflow] waits for a worker slot
    and hands [workflow] to the executor of [env]. The executor's [spawn] calls
    [spawn], and its [await_spawned] calls [await], with the worker slot given
    up meanwhile. Its [spawned] calls [watch], with the slot given up when it
    waits. Its [finished] calls [finished]. Its [queued] tells [queued_for] how
    many seconds the workflow was queued. [await], [watch] and [finished] return
    [[]] by default. With [interruptible], a workflow whose fiber is cancelled
    other than by {!cancel}, as when the spindle stops, is left running and the
    cancellation is raised. Each transition calls [persist]. Each JSON-encoded
    event calls [record] before becoming visible to log subscribers. [record]
    returns the durable journal sequence for snapshot compaction. A cancelled
    workflow's error is ["User canceled the workflow"], as the reference spindle
    reports it, and a workflow that ran out of time has none. *)

val cancel : persist:(unit -> unit) -> t -> unit
(** [cancel ~persist t] cancels [t] unless it has ended: a running workflow's
    fiber is cancelled, and a waiting one ends at once, calling [persist]. *)
