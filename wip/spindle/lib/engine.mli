(** Durable dispatch and workflow orchestration. *)

type pipeline = {
  id : string;
  repo : string;
  source : string;
  request : Jsont.json;
  metadata : Jsont.json;
  commit : string;
  created : string;
  workflows : Runner.t list;
}

type t = private {
  store : Store.t;
  catalog : Catalog.t;
  hostname : string;
  jobs : Job.t list;
  runner : Runner.env;
  sw : Eio.Switch.t;
  pipelines : (string, pipeline) Hashtbl.t;
  lock : Eio.Mutex.t;
  mutable last_tid : int64;
}

exception Capacity

val v :
  store:Store.t ->
  catalog:Catalog.t ->
  hostname:string ->
  jobs:Job.t list ->
  system:Eio_unix.Stdenv.base ->
  directory:Eio.Fs.dir_ty Eio.Path.t ->
  sw:Eio.Switch.t ->
  t

val migrate : t -> (string * string) option -> unit

val load : t -> unit
(** [load engine] resumes pending work and marks interrupted workflows failed.
    Completed histories are read from SQLite when requested. *)

val kind : Jsont.json -> Job.kind
val view : pipeline -> Jsont.json
val managed : t -> string -> Catalog.repo
val find : t -> string -> pipeline option

val query :
  t ->
  repo:string ->
  limit:int ->
  cursor:string option ->
  kinds:string list ->
  commits:string list ->
  Jsont.json
(** [query engine ~repo ~limit ~cursor ~kinds ~commits] returns newest-first
    summaries with bounded memory. [total] counts matches after the cursor.
    Commit filtering selects the latest match before applying the cursor. *)

val select_workflows : pipeline -> string list -> Runner.t list

val create :
  t ->
  ?dedup:string ->
  ?changed_files:string list ->
  ?default_ref:bool ->
  automatic:bool ->
  actor:string ->
  Jsont.json ->
  string option
(** [create engine ~automatic ~actor request] verifies repository ownership,
    selects OCaml workflows and atomically records the dispatch and optional
    deduplication key. Only verified event consumers may set [automatic]. *)

val cancel :
  t -> actor:string -> repo:string -> id:string -> names:string list -> unit
