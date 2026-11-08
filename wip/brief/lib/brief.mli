(** Reports of the brief project detector.

    {{:https://github.com/git-pkgs/brief} brief} reads a project's files and
    reports its languages, package managers and tools as JSON. {!scan} runs the
    [brief] command on a directory and {!of_string} reads a report it wrote.
    Every member that brief may omit is an option or a list, and members this
    module does not know are ignored, so a report from a newer brief still
    reads. The types follow brief 0.15. *)

(** {1 Detections} *)

type confidence =
  | High
  | Medium
  | Low
  | Other of string
      (** How sure brief is of a detection. [Other] is a value this module does
          not know. *)

type source =
  | Project_script
  | Knowledge_base
  | Config_file
  | Other of string
      (** Where a detection's command comes from. [Other] is a value this module
          does not know. *)

type command = {
  run : string;
  alternatives : string list;
  source : source;
  inferred_tool : string option;
}
(** The command that runs a detected tool. *)

type taxonomy = {
  role : string list;
  function_ : string list;
  layer : string list;
  domain : string list;
  audience : string list;
  technology : string list;
}
(** The oss-taxonomy facets of a tool, as kebab-case terms. *)

type detection = {
  name : string;  (** The tool's name as brief spells it, such as [OCaml]. *)
  category : string;
      (** Its category, such as [language], [package_manager], [build] or
          [test]. *)
  confidence : confidence;
  command : command option;
  config_files : string list;
  lockfile : string option;
  homepage : string option;
  docs : string option;
  repo : string option;
  description : string option;
  taxonomy : taxonomy option;
}
(** A language, package manager or tool found in the project. *)

type script = { name : string; run : string; source : string }
(** A task the project defines, such as a Makefile target, and the file it comes
    from. *)

type manifest = { ecosystem : string; path : string; kind : string }
(** A dependency manifest or lockfile, with its ecosystem such as [cargo] or
    [npm], and whether it is a [manifest] or a [lockfile]. *)

type dependency_source = {
  kind : string;  (** [git], [registry], [path], [github] or [url]. *)
  value : string option;
  branch : string option;
  tag : string option;
  ref_ : string option;
  rel : string option;
}
(** Where a dependency comes from when it is not its ecosystem's default
    registry. *)

type dependency = {
  manifest : string;  (** The path of the manifest that declares it. *)
  name : string;
  version : string option;
  purl : string;
  scope : string option;  (** [runtime], [development], [test] or [build]. *)
  direct : bool;
  source : dependency_source option;
}
(** A dependency a manifest declares. *)

(** {1 Project facts} *)

type style = {
  indentation : string option;
  indent_source : string option;
  line_ending : string option;
  trailing_newline : bool option;
}

type layout = {
  source_dirs : string list;
  test_dirs : string list;
  mirrors_source : bool;
}

type platforms = {
  runtime_version_files : (string * string) list;
  ci_matrix_versions : (string * string list) list;
  ci_matrix_os : string list;
}

type resources = {
  readme : string option;
  changelog : string option;
  roadmap : string option;
  license : string option;
  license_type : string option;  (** The licence's SPDX identifier. *)
  legal : (string * string) list;
  community : (string * string) list;
  security : (string * string) list;
  metadata : (string * string) list;
  agents : (string * string) list;
      (** Agent instruction files, such as [AGENTS.md], by name. *)
}
(** The project's documentation and policy files, as paths from its root. *)

type skill = {
  name : string;
  description : string option;
  path : string;
  format : string;
}

type git = {
  branch : string option;
  default_branch : string option;
  remotes : (string * string) list;
  commit_count : int;
}

type lines = {
  total_files : int;
  total_lines : int;
  by_language : (string * int) list;
  source : string;  (** [scc], [tokei] or [fallback]. *)
}

type stats = {
  duration_ms : float;
  files_checked : int;
  tools_matched : int;
  tools_checked : int;
  scan_entries : int;
  scan_truncated : bool;
      (** Whether the scan stopped at brief's limit on entries, 10,000 by
          default. *)
}

(** {1 Reports} *)

type t = {
  version : string;  (** The brief that wrote the report. *)
  path : string;  (** The directory scanned. *)
  diff_ref : string option;
  diff_commits : string list;
  changed_files : string list;
  languages : detection list;
      (** The languages found, the one with most source files first. *)
  package_managers : detection list;
  scripts : script list;
  tools : (string * detection list) list;
      (** The other detections by category, such as [build] or [ci], sorted by
          category. *)
  style : style option;
  layout : layout option;
  platforms : platforms option;
  resources : resources option;
  skills : skill list;
  git : git option;  (** Absent when the directory is not a git checkout. *)
  lines : lines option;
  manifests : manifest list;
  dependencies : dependency list;
  stats : stats;
}
(** A brief report. *)

val jsont : t Jsont.t
(** [jsont] reads and writes reports as brief's JSON. *)

val of_string : string -> (t, string) result
(** [of_string s] is the report in the JSON [s], or why it is not one. *)

val to_string : t -> string
(** [to_string t] is [t] as indented JSON. Members [t] does not model are not
    written. *)

(** {1 Queries} *)

val language : t -> string -> detection option
(** [language t name] is the language called [name] in [t], compared without
    regard to case, if brief found it. *)

val package_manager : t -> string -> detection option
(** [package_manager t name] is the package manager called [name] in [t],
    compared without regard to case, if brief found it. *)

val tool : t -> category:string -> string -> detection option
(** [tool t ~category name] is the tool called [name] in [t]'s [category],
    compared without regard to case, if brief found it. *)

val ecosystems : t -> string list
(** [ecosystems t] is the ecosystems of [t]'s manifests, in order of first
    appearance. *)

val summary : t -> string
(** [summary t] is a few lines naming the languages, package managers and tools
    of [t], in the manner of brief's own readable output. *)

(** {1 Running brief} *)

val run :
  ?brief:string ->
  ?timeout:float ->
  process:_ Eio.Process.mgr ->
  clock:_ Eio.Time.Mono.t ->
  string ->
  (string, string) result
(** [run ~brief ~timeout ~process ~clock dir] is the JSON that the command
    [brief] writes for the directory [dir], or why it gave none. [brief]
    defaults to [brief], found on the path. The command is given [timeout]
    seconds, 60 by default, and may write at most 16 MiB. It reads only [dir],
    and nothing in [dir] is run. *)

val scan :
  ?brief:string ->
  ?timeout:float ->
  process:_ Eio.Process.mgr ->
  clock:_ Eio.Time.Mono.t ->
  string ->
  (t, string) result
(** [scan ~brief ~timeout ~process ~clock dir] is the report of {!run} on [dir],
    read with {!of_string}. *)
