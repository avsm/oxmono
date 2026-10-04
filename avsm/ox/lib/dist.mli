(** Source bundles and native Linux packaging through d10 and osdist. *)

val targets : string -> string -> Osdist.Target.t list
(** [targets tags arch] resolves comma-separated distribution tags and sets
    their architecture. Unknown tags and architectures raise [Failure]. *)

val run :
  Support.proc ->
  clock:D10.Config.clk ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  Runner.config ->
  target:string ->
  with_packages:string list ->
  targets:Osdist.Target.t list ->
  pkg_name:string option ->
  pkg_version:string option ->
  maintainer:string option ->
  output:string ->
  build:bool ->
  unit
(** [run proc ~clock ~fs ~sys config ~target ~with_packages ~targets ~pkg_name ~pkg_version ~maintainer ~output ~build]
    exports a source bundle and packaging context for each target. Each bundle
    includes its compiler and dependency sources and builds offline with GNU
    make. [output] must not exist. [build] additionally runs the generated
    Docker Compose driver. Action filters must be resolvable before building.
    Scalar configuration values used in commands and environments are read
    during the build. *)
