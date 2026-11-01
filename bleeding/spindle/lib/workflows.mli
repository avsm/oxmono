(** Workflows read from a repository's [.tangled/workflows] directory, in the
    reference spindle's YAML format.

    Each file is one workflow, named by its file name. [when] selects events as
    the reference spindle does, with doublestar globs for branches and tags.
    Steps run with [bash -c]. Engines, dependencies, caches and images are not
    provided: steps run on the spindle's host with what it has. *)

val directory : string

val valid_name : string -> bool
(** [valid_name name] holds for a file name this spindle can run: letters,
    digits, [_], [-] and [.], not starting with [.], at most 128 bytes. *)

val matches : string -> string list -> bool
(** [matches name patterns] holds if a doublestar glob in [patterns] matches
    [name]. *)

val workflow_matches : Jsont.json -> Jsont.json -> bool
(** [workflow_matches trigger workflow] applies [workflow]'s [when] to
    [trigger]. A manual trigger always matches. *)

val definitions :
  repo:string -> commit:string -> (string * string) list -> Jsont.json list
(** [definitions ~repo ~commit files] is the [org.tangled.ci.defs#workflow] of
    each readable file. *)

val select :
  repo:string ->
  commit:string ->
  trigger:Jsont.json ->
  names:string list ->
  (string * string) list ->
  Job.t list * (string * string) list
(** [select ~repo ~commit ~trigger ~names files] is the workflows among [files],
    each a name and its contents, that [trigger] selects, limited to [names]
    when given, and each file that could not be read with why. *)
