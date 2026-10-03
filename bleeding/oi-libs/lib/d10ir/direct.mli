(** Direct executor: builds plan nodes against the host filesystem using d10 for
    layer storage and restore.

    No sandbox, no container. Works on Linux and macOS.

    Per-node procedure (each step emits a {!Node_phase} event so callers can
    drive a progress UI without polling):
    + If d10 already has the layer (succeeded), emit {!Node_cached}.
    + [Stage_deps]: hardlink each [dep_layer_hash]'s [fs/] subtree into a
      per-node staging dir.
    + [Unpack_archive]: untar the source archive into a per-node build dir.
    + [Apply_substs]: walk [n.substs] and expand [%{var}%] placeholders against
      [n.subst_vars] (rebased to staging).
    + [Snapshot_pre]: snapshot the staging prefix for the post-build diff.
    + [Run_script]: rebase the script's prefix sentinel and run
      [/bin/sh -e -c <script>] from build dir, capturing output to a per-node
      log.
    + [Apply_install_file]: if [<build_dir>/<package.name>.install] exists,
      apply it.
    + [Diff_layer]: diff staging vs. snapshot.
    + [Store_layer]: tar the diff into a d10 layer.

    Events are emitted from the same fiber that performs the work, so they
    reflect the actual execution order on each node. *)

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
    [Stage_deps]. *)
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
(** A single build failure as accumulated in {!result.failures}. *)

val pp_failures : failure list Fmt.t
(** Print a header [Build failures (N):] followed by each failure via
    {!pp_failure}. Empty list prints nothing. *)

type result = {
  built : int;
  cached : int;
  failed : int;
  skipped : int;
  failures : failure list;
      (** Per-package details for {!failed}, in the order they finished. Empty
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
    if the archive file is missing. *)

val run :
  config:Config.t ->
  d10:D10.Config.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:_ Eio.Process.mgr ->
  clock:D10.Config.clk ->
  ?reporter:reporter ->
  ?plan_dir:string ->
  ?install_to:string ->
  Plan.t ->
  result
(** [run ~config ~d10 ~fs ~proc_mgr ~clock ?reporter ?plan_dir ?install_to plan]
    executes every node in [plan] and returns aggregate counts.

    [plan_dir] is the directory containing [plan] (and its [archive_root]);
    defaults to the current working directory. Used to resolve [archive.path].

    [install_to] redirects the per-node install destination from the normal
    per-layer-hash [build/staging/<hash>] dir to a single shared user-owned
    prefix (e.g. the toolchain prefix in {!Oi.Aux_install}'s flow). When set:

    - opam's [%{prefix}%] expansion ([n.prefix] sentinel rebase at run time)
      targets [install_to] for every node, so packages install directly into the
      location they'll be consumed from — no staging-then-restore, no
      baked-staging-path-in-binary problem.
    - The dep-staging phase is skipped (siblings have installed to the same
      prefix; the build env's PATH/OCAMLPATH already covers it via
      {!Solver.Ctx.switch_env}).
    - The layer-cache lookup is skipped: a prior cached entry was captured
      against a per-node staging dir with that path baked into binaries, so
      reusing it would leave [install_to] either empty or full of stale paths.
    - The pre-build snapshot, post-build diff, and layer store are all skipped:
      the snapshot would be of the whole toolchain prefix and the stored layer
      would carry [install_to] baked into binaries, useless as a cache entry on
      any other host.
    - [install_to] is NOT cleaned up after the build; only [build_dir] (the
      per-node sources/work area) is. *)
