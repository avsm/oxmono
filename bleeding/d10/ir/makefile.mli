(** Standalone Makefile builds from a resolved plan.

    Sources must be unpacked under [sources/<node.archive.sha256>/] before
    building. Generated recipes require POSIX shell tools, GNU make and the
    plan's system dependencies. They perform no downloads.

    Builds use one shared temporary prefix and run nodes serially. Installed
    dependency layers are copied into that prefix before each node. The output
    under [dest/] contains the selected roots' [bin/] and [sbin/] files.
    [make install PREFIX=/usr DESTDIR=...] also copies their [share/] files.
    Applications must support installation away from their build prefix.

    Actions must be selected before export. Scalar configuration values may use
    deferred bindings. External layers must be provided by the build host. *)

type local = {
  name : string;
  sha256 : string;
  script : string;
  env : string list;
  prefix : string;
  deps : string list;
}
(** Additional source root stored under [sources/<sha256>/] or [<sha256>/]. *)

type config_var = string * string * string
(** A replacement token, a prefix-relative opam configuration file, and a
    variable name. Values are read from the file's [variables] section after
    dependency staging. Booleans, integers and single-line strings without
    quotes or backslashes are supported. *)

val emit :
  Plan.t ->
  output:string ->
  ?binaries:string list ->
  ?bin_roots:string list ->
  ?emit_dest_install:bool ->
  ?local:local ->
  ?config_vars:(string * config_var list) list ->
  unit ->
  unit
(** [emit plan ~output ()] writes a Makefile, shell helpers, recipe files and
    [plan.json]. [binaries] supplies names for display. [bin_roots] selects
    layers to install, defaulting to the plan roots. Set [emit_dest_install] to
    [false] to supply custom [dest] and [install] rules.

    The prefix sentinel in each recipe is replaced at build time. Its [.build]
    child denotes that node's source directory and its [.opam-dir/opam] child
    denotes [metadata/<package>.opam] in the bundle. Source substitutions use
    the node's [substs] and [subst_vars]. The build inherits [OI_STATIC].

    [config_vars] associates layer hashes with deferred configuration bindings.
    Tokens must be unique strings of letters, digits and underscores. They are
    replaced in scripts, environment entries and prepared substitution files.
    Missing values and unsupported scalar syntax fail the build.

    Raises [Failure] before writing if a buildable node has no source hash. *)
