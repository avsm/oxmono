(** Where workflows run.

    The spindle never runs a workflow's commands itself. It hands each workflow
    to an executor with everything needed to run it elsewhere, and records what
    the executor reports as the workflow's log. *)

type files = {
  list : string -> string list;
      (** [list dir] is the names of the regular files in directory [dir] of the
          commit, [""] for its root. It is [[]] for a missing directory. *)
  read : string -> string option;
      (** [read path] is the contents of the file [path] of the commit. *)
}
(** Read-only access to the repository at the commit being run, through its
    knot. Nothing is checked out on the spindle's host. *)

type request = {
  pipeline : string;  (** The pipeline's ID. *)
  workflow : string;  (** The workflow's name. *)
  repo : string;  (** The repository's DID. *)
  source : string;  (** Where to clone the repository from. *)
  commit : string;  (** The commit to check out. *)
  clone : bool;  (** Whether to check out [commit] before the steps. *)
  metadata : Jsont.json;
      (** The trigger, the actor and the event that started the pipeline. *)
  definition : Jsont.json option;
      (** The [org.tangled.ci.defs#workflow] of a repository workflow. *)
  yaml : string option;  (** The text of a repository workflow's file. *)
  env : (string * string) list;  (** The workflow's own environment. *)
  variables : (string * string) list;  (** The CI variables. *)
  secrets : (string * string) list;
      (** The repository's secrets. Empty unless the pipeline is trusted. *)
  steps : Job.step list;
  files : files;
  labels : unit -> string list;
      (** [labels ()] is the keys of the labels that the repository's owner or a
          collaborator put on the pull request that started the pipeline, as
          they are now. A pull request's author may label it too, and those
          labels are left out. It is [[]] for other triggers and when the knot
          cannot say. Each call asks the knot. *)
}
(** A step's environment is [env], then [variables], then [secrets], then the
    step's own [env], with later bindings winning. *)

type kind = [ `User | `System ]
(** A [`User] step comes from the workflow. A [`System] step is one the executor
    adds, such as a checkout, which viewers fold by default. *)

type outcome = {
  workflow : string;  (** The workflow's name. *)
  status : [ `Success | `Failed | `Cancelled | `Timeout ];
  error : string option;  (** Why it failed, when known. *)
  log : string;
      (** Its log as text: each step's name, prefixed by [## ], then its output.
      *)
}
(** How a finished workflow ended. *)

type progress = {
  workflow : string;  (** The workflow's name. *)
  status : [ `Pending | `Running | `Success | `Failed | `Cancelled | `Timeout ];
      (** [`Pending] until it has a worker slot, then [`Running] until it ends.
      *)
  step : string option;  (** The name of the step it is running, if any. *)
  error : string option;  (** Why it failed, when known. *)
}
(** How far a spawned workflow has got. *)

type context = {
  step : ?kind:kind -> ?command:string -> string -> (unit -> unit) -> unit;
      (** [step ~kind ~command name f] runs [f] as the next step of the log,
          called [name]. [kind] defaults to [`User]. [command], when given, is
          shown with the step's name. Secrets in [name] and [command] are
          hidden. The step fails if [f] raises, or if what it wrote last no
          longer fits the log. Steps run one at a time and do not nest. *)
  log : [ `Stdout | `Stderr ] -> string -> unit;
      (** [log stream text] adds [text] to the current step's log, with every
          secret replaced by [***]. The log holds one line per frame, so an
          unterminated line waits for its newline, a line longer than 16 KiB or
          the end of the step. It raises [Invalid_argument] outside a step. *)
  spawn : ?env:(string * string) list -> string -> Job.step list -> unit;
      (** [spawn ~env name steps] adds workflow [name] to the pipeline, as
          {!Job.spawned} describes it, and starts it with this executor. It runs
          alongside the current workflow, for instance one for each build a plan
          finds. [name] must be a valid workflow file name not already in the
          pipeline, which holds at most 50 workflows, unless this workflow
          spawned it before. A workflow that runs again after a restart spawns
          what it spawned before, and each earlier one stands, even if [steps]
          differ. *)
  await_spawned : unit -> outcome list;
      (** [await_spawned ()] waits until every workflow this one spawned, and
          every workflow those spawned, has finished, and is their outcomes in
          the order they were spawned. The workflow gives up its worker slot
          while it waits, and the wait does not count towards its minutes. Its
          time limit still applies. *)
  spawned : ?since:progress list -> unit -> progress list;
      (** [spawned ~since ()] is the progress of the workflows {!await_spawned}
          waits for, in the order they were spawned. With [since], it first
          waits until that progress differs from [since], giving up the worker
          slot as {!await_spawned} does. Without it, it does not wait. A step
          that starts or ends is a change. *)
  finished : unit -> outcome list;
      (** [finished ()] is how every other workflow of the pipeline that has
          finished ended, in the order they were added, as the spindle's store
          holds them, so it is the same after a restart. *)
  queued : 'a. (unit -> 'a) -> 'a;
      (** [queued f] is [f ()], during which the workflow is shown as pending,
          not running, as while it waits in a remote builder's queue for a
          worker. Its start time is when [f] returns, and the time [f] takes is
          not charged as minutes. It keeps its worker slot and its place in its
          repository's and owner's limits. *)
}

type t = context -> request -> unit
(** An executor runs a workflow and returns when it has succeeded. It fails the
    workflow by raising. Cancellation and the time limit cancel its fiber. *)
