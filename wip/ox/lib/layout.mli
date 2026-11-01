type selection = {
  root : string;
  owner : string option;
  roots : string list;
  snapshot : Stamp.snapshot;
}
(** Selected source projects and package roots. *)

val select :
  ?working:bool ->
  ?revision:string ->
  ?shared_only:bool ->
  Support.proc ->
  repo:string ->
  owner:string option ->
  unit ->
  selection
(** [select proc ~repo ~owner ()] discovers packages and closes local
    dependencies. Other owners are excluded before parsing their metadata.
    [working] defaults to true. [revision] defaults to HEAD. *)

val json : selection -> OpamJson.t
(** [json selection] describes package ownership, sources and binaries. *)

val stage : Support.proc -> selection -> output:string -> unit
(** [stage proc selection ~output] creates a working source tree from tracked
    and nonignored files in the selected projects. [output] must not exist.
    Escaping symlinks and cross-owner dependencies are rejected. *)

val locked_config : repo:string -> Runner.config -> Runner.config
(** [locked_config ~repo config] selects the exact compiler and repository
    revisions from toolchain.lock.json when present. Explicit repository
    arguments override the repository list, but not the compiler constraint. *)

val valid_owner : string -> bool
(** [valid_owner name] checks that [name] is a single safe owner component. *)

val init : Support.proc -> repo:string -> owner:string -> unit
(** [init proc ~repo ~owner] creates a new owner's manifest, Dockerfile and
    minimal executable from the repository template. Existing owners fail. *)
