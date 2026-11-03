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
  workflows : Runner.t list;
}

type reader = did:string -> commit:string -> (string * string) list
(** [reader ~did ~commit] is each file in [.tangled/workflows] of repository
    [did] at [commit], with its contents. *)

type t = private {
  store : Store.t;
  catalog : Catalog.t;
  settings : Settings.t;
  hostname : string;
  jobs : Job.t list;
  read_workflows : reader option;
  runner : Runner.env;
  sw : Eio.Switch.t;
  pipelines : (string, pipeline) Hashtbl.t;
  lock : Eio.Mutex.t;
  mutable last_tid : int64;
  running : (string, int) Hashtbl.t;
  quota_changed : Eio.Condition.t;
}

exception Capacity

val v :
  store:Store.t ->
  catalog:Catalog.t ->
  settings:Settings.t ->
  hostname:string ->
  jobs:Job.t list ->
  ?read_workflows:reader ->
  ?timeout:float ->
  executor:Executor.t ->
  system:Eio_unix.Stdenv.base ->
  directory:Eio.Fs.dir_ty Eio.Path.t ->
  sw:Eio.Switch.t ->
  unit ->
  t
(** [v ... ~jobs ~executor ()] hands the OCaml [jobs], and with [read_workflows]
    also each repository's own YAML workflows, to [executor]. A workflow may run
    for [timeout] seconds, 60 by default. *)

val migrate : t -> (string * string) option -> unit

val load : t -> unit
(** [load engine] resumes pending work and marks interrupted workflows failed.
    Completed histories are read from SQLite when requested. *)

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
val find : t -> string -> pipeline option

val query :
  t ->
  repo:string ->
  limit:int ->
  cursor:string option ->
  triggers:string list ->
  commits:string list ->
  Jsont.json
(** [query engine ~repo ~limit ~cursor ~triggers ~commits] returns newest-first
    summaries with bounded memory. [total] counts matches after the cursor.
    Commit filtering selects the latest match before applying the cursor. *)

val select_workflows : pipeline -> string list -> Runner.t list

val create :
  t ->
  ?dedup:string ->
  ?changed_files:string list ->
  ?default_ref:bool ->
  ?blocked:bool ->
  ?untrusted:bool ->
  automatic:bool ->
  actor:string ->
  Jsont.json ->
  string option
(** [create engine ~automatic ~actor request] checks that [actor] satisfies
    {!Catalog.permitted} for the repository unless [automatic] is set. It
    selects OCaml workflows and atomically records the dispatch and optional
    deduplication key. Only verified event consumers may set [automatic].
    [blocked] holds the workflows until {!approve}. A blocked or [untrusted]
    pipeline, or one whose code comes from another repository, receives no
    secrets. *)

val cancel :
  t -> actor:string -> repo:string -> id:string -> names:string list -> unit

val approve : t -> actor:string -> repo:string -> id:string -> unit
(** [approve t ~actor ~repo ~id] starts the blocked workflows of pipeline [id].
    [actor] must satisfy {!Catalog.permitted} for [repo]. *)

val retry :
  t ->
  actor:string ->
  repo:string ->
  id:string ->
  names:string list ->
  string option
(** [retry t ~actor ~repo ~id ~names] runs pipeline [id] again as a new pipeline
    with the same trigger, limited to [names] when given. *)

val running : t -> string -> int
(** [running t did] counts the workflows running for repository or owner [did].
*)

val wipe : t -> string -> unit
(** [wipe t repo] cancels and deletes every pipeline of [repo]. *)
