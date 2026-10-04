(** Fetch the per-os_key remote index files the registry publishes and project
    them into the maps that [oi run] / [oi build] consume.

    Two files are read, both shipped by a server-side Clickhouse query (see
    [foo.sql] in the repo root):

    - [<base>/<os_key>/index.json] — minimum index, hash → tarball sha256

    + size only. Drives the layer-fetch path in
      {!Build_pipeline.plan_remote_fetches}.

    - [<base>/<os_key>/index-full.json] — adds package name/version, overlay,
      binaries, findlib, deps. Drives the [oi run BARE_BINARY] resolution in
      {!Layer_index.package_of_binary}.

    There is no fallback to the legacy [index.db]; the streaming upload pipeline
    never publishes one. *)

(** {1 Wire types}

    These mirror the JSON shapes the Clickhouse query emits. Exposed so callers
    ({!Layer_index}, future {!Oi.search}) can pattern-match the parsed records
    directly rather than going through SQLite. *)

type layer_min = {
  hash : string;
  tarball_sha256 : string;
  tarball_size : int64;
}

type index_min = {
  schema : int;
  os_key : string;
  generated_at : string;
  n_layers : int;
  layers : layer_min list;
}

type dep = { name : string; version : string; hash : string }

type findlib_entry = {
  package_dir : string;
  findlib_pkg : string;
  archive : string;  (** Empty string when the META has no [archive(…)]. *)
}

type layer_full = {
  hash : string;
  package_name : string;
  package_ver : string;
  exit_status : int;
  created : float;
  overlay_handle : string option;
  overlay_version : string option;
  tarball_sha256 : string option;
  tarball_size : int64 option;
  deps : dep list;
  binaries : string list;
  findlib : findlib_entry list;
}

type index_full = {
  schema : int;
  os_key : string;
  arch : string;
  distro : string;
  os_version : string;
  os : string;
  generated_at : string;
  n_layers : int;
  layers : layer_full list;
}

val index_min_codec : index_min Jsont.t
(** [index_min_codec] is the Jsont codec for the minimum index ([index.json]).
    Exposed so an alternative client (a test fixture, an out-of-process indexer)
    can roundtrip the same wire format. *)

val index_full_codec : index_full Jsont.t
(** [index_full_codec] is the Jsont codec for the rich index
    ([index-full.json]). *)

(** {1 Fetch entry points}

    Both functions memoise per [(remote, os_key)] so a multi-group solve pays
    the HTTP cost once. *)

val fetch :
  Config.t ->
  session:Sysops.Http.session ->
  remote:Layer.remote ->
  Layer.remote_index
(** [fetch c ~session ~remote] downloads [<remote>/<os_key>/index.json] and
    returns the map from layer hash to its tarball SHA-256 + size. Missing or
    undecodable responses yield an empty map (and emit a warn log). *)

val fetch_full :
  Config.t ->
  session:Sysops.Http.session ->
  remote:Layer.remote ->
  index_full option
(** [fetch_full c ~session ~remote] downloads
    [<remote>/<os_key>/index-full.json] and returns the decoded document. [None]
    on missing or undecodable responses. *)
