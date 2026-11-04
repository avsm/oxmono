(** Durable dispatch and workflow orchestration. *)

type pipeline = {
  id : string;
  repo : string;
  owner : string;
  source : string;
  request : Jsont.json;
  metadata : Jsont.json;
  commit : string;
  created : string;
  trusted : bool;
      (** Whether its workflows receive the repository's secrets. *)
  variables : (string * string) list;  (** CI variables for its steps. *)
  mutable workflows : Runner.t list;
      (** An executor may add workflows while the pipeline runs. *)
}

type reader = did:string -> commit:string -> (string * string) list
(** [reader ~did ~commit] is each file in [.tangled/workflows] of repository
    [did] at [commit], with its contents. *)

type tree = did:string -> commit:string -> Executor.files
(** [tree ~did ~commit] reads repository [did] at [commit] for executors. *)

type t = private {
  store : Store.t;
  catalog : Catalog.t;
  settings : Settings.t;
  hostname : string;
  jobs : Job.t list;
  read_workflows : reader option;
  tree : tree option;
  runner : Runner.env;
  sw : Eio.Switch.t;
  pipelines : (string, pipeline) Hashtbl.t;
  lock : Eio.Mutex.t;
  mutable last_tid : int64;
  running : (string, int) Hashtbl.t;
  quota_changed : Eio.Condition.t;
  changed : Eio.Condition.t;
  durable : (string, string) Duraml_client.Queue.t option;
}

exception Capacity

val v :
  store:Store.t ->
  catalog:Catalog.t ->
  settings:Settings.t ->
  hostname:string ->
  jobs:Job.t list ->
  ?read_workflows:reader ->
  ?tree:tree ->
  ?timeout:float ->
  ?concurrency:int ->
  ?durable:Duraml.plain ->
  executor:Executor.t ->
  system:Eio_unix.Stdenv.base ->
  directory:Eio.Fs.dir_ty Eio.Path.t ->
  sw:Eio.Switch.t ->
  unit ->
  t
(** [v ... ~jobs ~executor ()] hands the OCaml [jobs], and with [read_workflows]
    also each repository's own YAML workflows, to [executor]. [tree] gives
    executors read access to the repository. A workflow may run for [timeout]
    seconds, 60 by default, and [concurrency] workflows, 2 by default, run at
    once.

    With [durable], each workflow is a job of that Duraml store, keyed by its
    pipeline and name and laned by its repository, run once {!serve} starts the
    store's workers. A workflow interrupted when the spindle stops stays running
    and runs again from the start when its job's lease runs out, with a fresh
    log, up to three times. Without it, an interrupted workflow fails. *)

val migrate : t -> (string * string) option -> unit

val load : t -> unit
(** [load engine] resumes pending work. Interrupted workflows are failed, or
    with a durable store run again. Completed histories are read from SQLite
    when requested. *)

val serve : ?lease:float -> t -> unit
(** [serve ~lease engine] starts the workers of a durable engine on its switch,
    after {!load}. A job's lease is [lease] seconds, 60 by default, so an
    interrupted workflow runs again that long after the spindle restarts. It
    does nothing without a durable store. *)

val push : string
val pull_request : string

val manual : string
(** The trigger NSIDs. *)

val kind : Jsont.json -> Job.kind

val definition : string -> string -> Jsont.json
(** [definition hostname name] is the [org.tangled.ci.defs#workflow] of the
    configured workflow [name]. *)

val view : t -> pipeline -> Jsont.json
(** [view t p] is [p] as an [org.tangled.ci.defs#pipeline]. *)

val managed : t -> string -> Catalog.repo
(** [managed t did] is the served repository [did]. It raises a 404
    [RepoNotFound] XRPC error for any other. *)

val authorize : t -> principal:string -> string -> unit
(** [authorize t ~principal repo] checks that [principal] satisfies
    {!Catalog.permitted} for the served repository [repo], raising a 403
    [AccessControl] XRPC error otherwise. Callers check before revealing a
    pipeline's state. *)

val find : t -> string -> pipeline option
(** [find t id] is pipeline [id], or [None] when [id] is not a TID. *)

val query :
  t ->
  repo:string ->
  limit:int ->
  cursor:string option ->
  triggers:string list ->
  commits:string list ->
  Jsont.json
(** [query engine ~repo ~limit ~cursor ~triggers ~commits] returns newest-first
    summaries with bounded memory. [total] counts every match, on every page,
    and each pipeline of a commit in [commits] matches. *)

val select_workflows : pipeline -> string list -> Runner.t list

val create :
  t ->
  ?dedup:string ->
  ?changed_files:string list ->
  ?default_ref:bool ->
  ?blocked:bool ->
  ?untrusted:bool ->
  ?principal:string ->
  automatic:bool ->
  actor:string ->
  Jsont.json ->
  string option
(** [create engine ~automatic ~actor request] checks that [principal], which
    defaults to [actor], satisfies {!Catalog.permitted} for the repository
    unless [automatic] is set. [actor] is recorded as the pipeline's actor. It
    selects OCaml workflows and atomically records the dispatch and optional
    deduplication key. Only verified event consumers may set [automatic].
    [blocked] holds the workflows until {!approve}. A blocked or [untrusted]
    pipeline, or one whose code comes from another repository, receives no
    secrets. *)

val ref_variables : string -> (string * string) list
(** [ref_variables ref_] is [TANGLED_REF], [TANGLED_REF_NAME] and
    [TANGLED_REF_TYPE] for [ref_]. Any ref but a tag is a branch. *)

val first : t -> string -> string -> bool
(** [first t namespace key] records [key] in [namespace] and holds if it was not
    already recorded. Receipts expire with event receipts. *)

val cancel :
  t -> actor:string -> repo:string -> id:string -> names:string list -> unit

val approve : t -> actor:string -> repo:string -> id:string -> unit
(** [approve t ~actor ~repo ~id] starts the blocked workflows of pipeline [id].
    [actor] must satisfy {!Catalog.permitted} for [repo]. *)

val retry :
  t ->
  principal:string ->
  actor:string ->
  repo:string ->
  id:string ->
  names:string list ->
  string option
(** [retry t ~principal ~actor ~repo ~id ~names] runs pipeline [id] again as a
    new pipeline with the same trigger, limited to [names] when given.
    [principal] must satisfy {!Catalog.permitted} for [repo]. [actor] is
    recorded as the new pipeline's actor. *)

val running : t -> string -> int
(** [running t did] counts the workflows running for repository or owner [did].
*)

val wipe : t -> string -> unit
(** [wipe t repo] cancels and deletes every pipeline of [repo]. *)
