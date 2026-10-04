(** A d10ir plan: a graph of nodes, each producing one d10 layer.

    Nodes are identified by their {!Layer_hash.t}. The dep graph is implicit:
    edge [A → B] iff [A.layer_hash ∈ B.dep_layer_hashes]. Producer-consumer
    matching across nodes is by hash equality. *)

type package = { name : string; version : string }
type overlay = { handle : string; version : string }

type node = {
  package : package;
  layer_hash : Layer_hash.t;
      (** This node's output d10 layer hash (computed by the producer using
          {!D10.Layer.hash}). *)
  dep_layer_hashes : Layer_hash.t list;
      (** d10 layers required at staging time. Each must either be present in
          the d10 store or produced by another node in the same plan. *)
  archive : Archive.t;
  script : string;
      (** Shell script — fully resolved by the producer. The executor runs it
          via [/bin/sh -e -c <script>] from the unpacked source root, with
          [n.env] plus [OCAMLFIND_LDCONF=ignore] injected. *)
  env : string list;
      (** Process environment as Docker-style [KEY=VALUE] entries,
          order-preserving. May contain references to {!prefix} which the
          executor rebases to the staging location at run time. *)
  depexts : string list;
      (** System packages required, OS-resolved. Diagnostic / docker base only;
          the executor doesn't install them. *)
  prefix : string;
      (** Install destination — the path the script believes it is installing
          to. The executor rebases this to a per-node staging dir before
          running. *)
  substs : string list;
      (** Basenames (relative to the unpacked source root) of opam [substs:]
          entries. After unpacking the archive, the executor reads each
          [<base>.in], substitutes [%{var}%] placeholders using {!subst_vars},
          and writes the result to [<base>]. The archive itself contains the raw
          [.in] files unchanged so archives stay byte-portable across machines.
      *)
  subst_vars : string list;
      (** Resolved opam variables for substitution as Docker-style [KEY=VALUE]
          entries. Used by the executor's [.in]→outcome pass. Values may
          reference the {!prefix} sentinel; the executor rebases each value
          before applying it, just like {!script} and {!env}.

          Same wire format as {!env} for consistency. Keys can contain colons
          (opam's [<pkg>:foo] qualified-variable syntax) but never [=], so
          split-on-first-[=] is unambiguous. *)
  overlay : overlay option;
      (** Reporepo handle the package came from, for diagnostics. *)
  opam_file_sha256 : string;
      (** Source opam file's hash, for audit / provenance. *)
}

type toolchain = {
  name : string;  (** e.g. ["ocaml-5.4"], diagnostics only. *)
  base_layer : Layer_hash.t;  (** The toolchain root, present in {!t.nodes}. *)
}

type metadata = {
  oi_version : string;
  generated_at : float;  (** Unix time, diagnostics only. *)
  cli_invocation : string list;
}

type mount = {
  name : string;
      (** Stable kebab-case identifier ([dune-cache], [ccache], ...). Diagnostic
          only — the executor matches on no field but [source] and [env]. *)
  source : string;  (** Absolute host path. Created if missing. *)
  target : string;
      (** Path the build sees the mount at. Equal to [source] on the native
          executor; differs only when the backend interposes a virtual
          filesystem (sandbox / container). *)
  mode : [ `Ro | `Rw ];
  env : string list;
      (** [KEY=VALUE] entries added to every node's process environment when
          this mount is active. Typical shape:
          [["DUNE_CACHE=enabled"; "DUNE_CACHE_ROOT=<target>"]]. *)
}
(** A persistent host directory that the recipe wants made available to every
    node during execution.

    Used for shared build caches like dune's cache, ccache, and (later) sccache.
    The native ({!Direct}) executor ensures [source] exists on the host and adds
    {!env} entries to every node's process environment — no actual mounting
    happens, since on macOS / Linux-host builds run on the host filesystem
    already.

    A future sandboxed backend (e.g. obuilder) will translate the same record
    into a bind mount of [source] at [target] inside the sandbox, with the
    sandbox-internal path appearing in the env entries. For Direct executions,
    [source] and [target] are typically equal because there is no path
    translation. *)

type t = {
  schema_version : int;
  os_key : string;
  toolchain : toolchain;
  archive_root : string;
      (** Directory holding referenced archives, relative to the plan's
          serialised location (or absolute for in-memory plans). *)
  nodes : node list;
  roots : Layer_hash.t list;
  mounts : mount list;
      (** Shared host directories applied to every node. Order is preserved, but
          the executor doesn't order-sensitively process them. Empty for older
          recipes — defaults to [[]] on decode. *)
  external_layers : Layer_hash.t list;
      (** Layer hashes the host provides — typically a non-relocatable
          toolchain's compiler-stack packages, which the executor doesn't build
          but which still appear in consumer nodes' [dep_layer_hashes] because
          consumer hashes mix the toolchain hash in for invalidation. The
          executor and validator treat these as already-satisfied: no producer
          needed in {!nodes}, no [{!D10.Layer.succeeded}] required in the d10
          store, and no staging from a layer fs/ tree (the binaries are reached
          via the build env's PATH). Empty for older recipes — defaults to [[]]
          on decode. *)
  metadata : metadata;
}

(** {1 Schema} *)

val current_schema_version : int

(** {1 JSON codec} *)

val codec : t Jsont.t
(** [codec] decodes/encodes a plan as JSON. *)

val to_string : t -> string
(** [to_string t] is the indented JSON serialisation of [t]. *)

val of_string : string -> (t, string) result
(** [of_string s] parses a JSON-encoded plan; errors carry a human message. *)

val save : _ Eio.Path.t -> t -> unit
(** [save path t] writes [to_string t] to [path] atomically. *)

val load : _ Eio.Path.t -> (t, string) result
(** [load path] reads and decodes a plan from [path]. *)

val pp : t Fmt.t
(** [pp ppf t] renders a one-line summary: schema version, OS key, toolchain
    name, root and node counts. *)

(** {2 Single-node form}

    The d10 layer cache stores the {!node} that produced each layer alongside
    the layer itself ({!D10.Layer.load_recipe_json}). This lets a layer be
    reconstructed from inputs (recipe + content-addressed source archive + dep
    layers) and audited end-to-end. Only the node is stored — not the full plan
    — because dependencies are expressed by layer hash and chase through the d10
    store recursively. *)

val encode_node : node -> string
(** [encode_node n] is the indented JSON serialisation of a single [node]. *)

val decode_node : string -> (node, string) result
(** [decode_node s] decodes the output of {!encode_node}. *)

(** {1 Validation} *)

type validate_error =
  | Schema_mismatch of { found : int; expected : int }
  | Cycle of Layer_hash.t list list
  | Unsatisfiable_dep of { node : Layer_hash.t; missing_dep : Layer_hash.t }
  | Duplicate_layer of Layer_hash.t
  | Archive_missing of { node : Layer_hash.t; path : string }
  | Archive_sha_mismatch of {
      node : Layer_hash.t;
      path : string;
      expected : string;
      actual : string;
    }

val pp_validate_error : validate_error Fmt.t
(** [pp_validate_error] renders a {!validate_error} as a one-line diagnostic. *)

val validate :
  ?d10:D10.Config.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  plan_dir:string ->
  t ->
  (unit, validate_error) result
(** [validate ?d10 ~fs ~plan_dir t] checks:
    - schema_version matches;
    - layer_hashes are pairwise unique;
    - the graph is acyclic;
    - every [dep_layer_hash] is satisfiable (in-recipe producer or, when [d10]
      is provided, present and succeeded in the d10 store);
    - every [archive.path] resolves under [plan_dir/archive_root] and has a
      matching sha256, except for nodes whose layer is already succeeded in
      [d10] (their archive is no longer needed to build).

    [plan_dir] is the directory containing [recipe.json]; archive paths are
    joined with [archive_root] relative to it. *)

(** {1 Schedule} *)

val producers_table : t -> (Layer_hash.t, node) Hashtbl.t
(** [producers_table t] is a map from a node's [layer_hash] to the node, for
    O(1) producer lookup. *)

val merge : t list -> (t, string) result
(** [merge plans] folds a collection of plans into a single plan the executor
    can schedule across as one unified DAG.

    Nodes are deduplicated by [layer_hash] (if several input plans share a
    transitive dep, only one copy survives). Roots and mounts are union'd;
    mounts dedupe by [name]. Metadata's [cli_invocation] is concatenated so the
    merged plan's audit trail records every batched invocation.

    Returns [Error msg] when the inputs disagree on any field that must be
    globally consistent for the executor: schema version, [os_key],
    [toolchain.base_layer], or [archive_root]. Two solve groups built against
    different overlays or toolchains cannot be merged this way; build them in
    separate runs. The empty list also returns [Error]. *)
