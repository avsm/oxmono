(** Workflows read from a repository's [.tangled/workflows] directory, in the
    reference spindle's YAML format.

    Each file is one workflow, named by its file name. [when] selects events as
    the reference spindle does, with doublestar globs for branches and tags.
    A selected workflow, with its file's text, goes to the {!Executor}. *)

val directory : string

val valid_name : string -> bool
(** [valid_name name] holds for a file name this spindle can run: 1 to 255 bytes
    of printable UTF-8 without [/], other than [.] and [..]. *)

val matches : string -> string list -> bool
(** [matches name patterns] holds if a doublestar glob in [patterns] matches
    [name]. *)

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
