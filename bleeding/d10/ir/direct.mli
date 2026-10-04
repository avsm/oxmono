(** Execute day10 recipes on the host filesystem. Both single-node and plan
    execution use the same build phases. Callers serialize cache mutations. *)

type prefix_policy =
  | Staging
  | Permanent
      (** [Staging] builds in a temporary prefix and captures a layer before
          cleanup. Recipes and outputs must support relocation to their
          consumption prefix. [Permanent] builds at [D10.Prefix.path d10 ~hash],
          retains the prefix and restores dependency prefixes on cache hits. The
          caller must include that location and policy in the node's cache
          identity. Neither policy makes arbitrary compiled artifacts
          relocatable. *)

(** A discrete step within a single node's build. Emitted both as a transition
    marker ({!Node_phase}) and, on failure, attached to {!Node_failed} so
    callers can tell e.g. an unpack failure from a build-script failure. *)
type phase =
  | Stage_deps
  | Unpack_archive
  | Apply_substs
  | Snapshot_pre
  | Run_script
  | Apply_install_file
  | Diff_layer
  | Store_layer

val string_of_phase : phase -> string
(** [string_of_phase p] is a short kebab-case name for [p] suitable for a status
    column (e.g. ["stage-deps"], ["run-script"]). *)

(** Structured progress events.

    The lifecycle is:

    {v
      Plan_started
      ├── Node_queued (one per node)
      │     └── Node_started → Node_phase* → Node_{built,cached,failed}
      │           or Node_skipped (deps failed)
      │           or Node_cached (layer already present)
      └── Plan_done
    v}

    [Node_queued] fires as soon as the fiber is forked, before any dep wait.
    [Node_started] fires after the build slot is acquired, immediately before
    [Stage_deps]. A cached permanent prefix may emit [Stage_deps] while being
    restored before [Node_cached]. *)
type event =
  | Plan_started of { total : int }
  | Plan_done of { built : int; cached : int; failed : int; skipped : int }
  | Node_queued of { node : Plan.node }
  | Node_started of { node : Plan.node }
  | Node_phase of { node : Plan.node; phase : phase }
  | Node_cached of { node : Plan.node }
  | Node_built of { node : Plan.node; duration_s : float; log_path : string }
  | Node_failed of {
      node : Plan.node;
      phase : phase;
      log_path : string;
      error : string;
      duration_s : float;
    }
  | Node_skipped of { node : Plan.node; reason : string }

type reporter = { event : event -> unit }

type failure = {
  package : Plan.package;
  phase : phase;
  log_path : string;
  error : string;
      (** Tidied human-readable summary of the underlying exception (e.g.
          ["exit 1"], not the full [Printexc.to_string] dump of [Eio.Io.E]). *)
}
(** A single build failure as accumulated in [result.failures]. *)

val pp_failures : failure list Fmt.t
(** [pp_failures ppf failures] prints a count and each package's failed phase,
    error and log path. An empty list prints nothing. *)

type result = {
  built : int;
  cached : int;
  failed : int;
  skipped : int;
  failures : failure list;
      (** Per-package details for [failed], in the order they finished. Empty
          when [failed = 0]. *)
}

val unpack_archive :
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  plan_dir:string ->
  archive_root:string ->
  Plan.node ->
  build_dir:string ->
  unit
(** [unpack_archive ~proc_mgr ~fs ~plan_dir ~archive_root n ~build_dir] wipes
    [build_dir], recreates it, and extracts [n]'s archive into it
    ([tar -x --strip-components=n.archive.strip_components]). The archive path
    is resolved relative to [plan_dir]/[archive_root] when not absolute. Raises
    if the archive file is missing or its non-empty SHA256 does not match. *)

val run :
  config:Config.t ->
  d10:D10.Config.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:_ Eio.Process.mgr ->
  clock:D10.Config.clk ->
  ?reporter:reporter ->
  ?plan_dir:string ->
  ?install_to:string ->
  ?prefix_policy:prefix_policy ->
  Plan.t ->
  result
(** [run ~config ~d10 ~fs ~proc_mgr ~clock ?reporter ?plan_dir ?install_to plan]
    executes every node in [plan] and returns aggregate counts. [prefix_policy]
    defaults to [Staging]. [Permanent] retains cached prefixes and is
    incompatible with [install_to].

    [plan_dir] is the directory containing [plan] and defaults to the current
    working directory. It resolves relative [archive_root] and archive paths.

    [install_to] builds every node directly into one shared prefix. It skips
    dependency staging, cache lookup and layer capture. The prefix is retained
    after execution. The caller must supply environments that find dependencies
    in that prefix and arrange for concurrently scheduled nodes to install
    without conflicts.

    Call {!Plan.validate} before running a serialized plan. Execution does not
    validate the graph or sandbox scripts. *)

val run_node :
  config:Config.t ->
  d10:D10.Config.t ->
  proc_mgr:_ Eio.Process.mgr ->
  ?prefix_policy:prefix_policy ->
  ?source_dir:string ->
  ?prepare:(prefix:string -> build_dir:string -> Plan.node -> Plan.node) ->
  ?reporter:reporter ->
  ?plan_dir:string ->
  Plan.node ->
  ([ `Built | `Cached ], failure) Stdlib.result
(** [run_node ~config ~d10 ~proc_mgr node] executes one node whose dependencies
    are already in the store. [prefix_policy] defaults to [Staging].

    [source_dir] supplies a source tree instead of [node.archive]. The tree is
    copied, then archived after preparation with its checksum recorded in the
    stored recipe. The supplied archive fields are ignored in this case.

    [prepare ~prefix ~build_dir node] runs after dependency assembly and source
    extraction, before execution. It may refine the recipe and prepare source
    files, but must preserve the package, hash, prefix and dependency
    identities. It must not mutate the installation prefix. It is skipped on
    cache hits. Source edits should use [source_dir] so the stored recipe can
    replay them.

    Cache keys remain the caller's responsibility. [config.inherit_path = false]
    executes with the recipe PATH unchanged. *)
