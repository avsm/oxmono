val name : Solve.package -> string
(** Opam action filters and package environments without opam state. *)

val version : Solve.package -> string

val resolver :
  ?config_var:(string -> string -> OpamVariable.variable_contents option) ->
  solution:Solve.t ->
  installed:string list ->
  prefix:string ->
  build_dir:string ->
  jobs:int ->
  Solve.package ->
  OpamFilter.env
(** [resolver ~solution ~installed ~prefix ~build_dir ~jobs package] resolves
    platform, package and generated configuration variables for an action.
    [config_var] supplies a fallback for unknown package variables. *)

val environment : prefix:string -> string array
(** [environment ~prefix] selects host build variables and package paths.
    Compiler paths inherited from an opam environment are removed. *)

val build_environment :
  ?base_env:string array ->
  ?config_var:(string -> string -> OpamVariable.variable_contents option) ->
  solution:Solve.t ->
  installed:string list ->
  prefix:string ->
  build_dir:string ->
  jobs:int ->
  Solve.package ->
  string array
(** [build_environment ~solution ~installed ~prefix ~build_dir ~jobs package]
    applies dependency environment updates and the package's build environment.
    [base_env] replaces the host environment and [config_var] supplies the
    resolver's fallback for generated variables. *)

val runtime_environment :
  solution:Solve.t -> prefix:string -> jobs:int -> string array
(** [runtime_environment ~solution ~prefix ~jobs] applies selected packages'
    exported environment updates to the assembled run prefix. *)

val shell : string list list -> string
(** [shell commands] quotes each argument for a POSIX shell. *)

val prepare :
  solution:Solve.t ->
  installed:string list ->
  jobs:int ->
  Solve.package ->
  prefix:string ->
  build_dir:string ->
  D10ir.Plan.node ->
  D10ir.Plan.node
(** [prepare ~solution ~installed ~jobs package ~prefix ~build_dir node]
    resolves opam actions and environment after dependencies are installed. It
    expands source substitutions and adds package configuration capture. *)

val export :
  solution:Solve.t ->
  installed:string list ->
  jobs:int ->
  source_dir:string ->
  Solve.package ->
  D10ir.Plan.node ->
  D10ir.Plan.node * D10ir.Makefile.config_var list
(** [export ~solution ~installed ~jobs ~source_dir package node] resolves a
    standalone recipe with a Linux build environment. It expands [.in] files in
    [source_dir], retaining path sentinels for the Makefile backend. Supply a
    writable copy of the sources. Host environment and paths are excluded.
    Generated scalar configuration variables are returned as deferred bindings
    for the Makefile backend. Filters must be resolvable before building. *)
