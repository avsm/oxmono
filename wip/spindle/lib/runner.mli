(** Execution and logs for one independently cancellable workflow. *)

type t = {
  job : Job.t;
  parent : string option;
      (** The workflow that spawned this one while it ran, if any. *)
  mutable status : string;
  mutable started : string option;
  mutable finished : string option;
  mutable error : string option;
  mutable events : Jsont.json list;
  mutable log_bytes : int;
  mutable log_seq : int64;
  log_lock : Eio.Mutex.t;
  mutable cancel : (unit -> unit) option;
  mutable cancelled : bool;
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
}

val now : unit -> string

val pairs : (string * string) list -> Jsont.json
(** [pairs l] is [l] as a JSON object of strings. *)

val unpairs : Jsont.json -> (string * string) list
val optional : string -> ('a -> 'b) -> 'a option -> (string * 'b) list
val terminal : t -> bool
val v : ?parent:string -> Job.t -> t
val view : t -> Jsont.json
val snapshot : t -> Jsont.json
val restore : Jsont.json -> t

val outcome : t -> Executor.outcome
(** [outcome t] is how finished workflow [t] ended, with its log as text. *)

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
  t ->
  unit
(** [execute env input ~record ~persist ~spawn workflow] waits for a worker slot
    and hands [workflow] to the executor of [env]. The executor's [spawn] calls
    [spawn], and its [await_spawned] calls [await], with the worker slot given
    up meanwhile. [await] is [fun () -> []] by default. Each transition calls
    [persist]. Each JSON-encoded event calls [record] before becoming visible to
    log subscribers. [record] returns the durable journal sequence for snapshot
    compaction. A cancelled workflow's error is ["User canceled the workflow"],
    as the reference spindle reports it, and a workflow that ran out of time has
    none. *)

val cancel : persist:(unit -> unit) -> t -> unit
