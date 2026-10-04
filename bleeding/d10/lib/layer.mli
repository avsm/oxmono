(** Content-addressed binary package layers.

    Each layer is a directory containing a filesystem snapshot ([fs/]) and build
    metadata ([layer.json]). The directory name is the layer's hash, computed
    from the opam [effective_part] of the package and all its direct
    dependencies.

    Two sidecar files live next to [layer.json]: [provenance.json]
    ({!Oi.Provenance.t}) carries the immutable content-addressed inputs that
    produced the layer, and [audit.jsonl] (one per cache root, not per layer)
    records every caller invocation that contributed a build attempt. The
    [layer.json] tracked here is the d10-internal cache record; the richer audit
    / provenance views are queried via the [oi] modules.

    {2 On-disk structure}

    {v
    <cache>/layers/<os_key>/<hash>/
      layer.json     # {!meta} record as JSON
      fs/            # filesystem snapshot (hardlinked from build prefix)
        bin/
        lib/
        share/
        ...
    v}

    The [fs/] tree is a subset of the build prefix -- only files new or modified
    by this package's install step are captured (via {!Prefix.diff}). Both
    regular files and symlinks are preserved. *)

(** {1 Hash computation} *)

val hash : packages_dirs:string list -> OpamPackage.t list -> string
(** [hash ~packages_dirs pkgs] computes the layer hash for a set of packages.
    The hash is the MD5 of the concatenated per-package opam [effective_part]
    hashes (SHA-512). Callers should pass the package and its full transitive
    dependency closure so that any change in the dependency tree invalidates the
    cache. *)

(** {1 Layer metadata}

    Stored as [layer.json] inside each layer directory. *)

type meta = {
  package : string;  (** Package name.version (e.g. ["dune.3.22.1"]). *)
  exit_status : int;  (** Build exit status (0 = success). *)
  deps : string list;  (** Direct dependency name.versions. *)
  hashes : string list;  (** Layer hashes of direct dependencies. *)
  created : float;  (** Unix timestamp of layer creation. *)
}
(** Cache-internal record; the richer per-caller event log and immutable content
    provenance (which includes overlay attribution) live in [Oi.Audit] and
    [Oi.Provenance] sidecars next to this file. *)

val load_meta : _ Eio.Path.t -> meta option
(** [load_meta path] reads and parses [layer.json] from [path]. Returns [None]
    if the file does not exist or cannot be parsed. *)

val meta_codec : meta Jsont.t
(** Codec [save_meta] / [load_meta] use for [layer.json]. Exposed so
    higher-level commands can embed [meta] inside their own JSON envelopes (e.g.
    [oi cache show --format=json]). *)

(** {1 Paths and queries} *)

val dir : Config.t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [dir c ~hash] is [<root>/layers/<os_key>/<hash>]. *)

val json_path : Config.t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [json_path c ~hash] is [<root>/layers/<os_key>/<hash>/layer.json]. *)

val exists : Config.t -> hash:string -> bool
(** [exists c ~hash] is [true] if [layer.json] exists for this hash. *)

val succeeded : Config.t -> hash:string -> bool
(** [succeeded c ~hash] is [true] if the layer exists and has [exit_status = 0].
    Used for cache hit detection. *)

(** {1 Storage and retrieval} *)

val store :
  Config.t ->
  hash:string ->
  prefix:string ->
  files:string list ->
  package:string ->
  deps:string list ->
  parent_hashes:string list ->
  exit_status:int ->
  ?recipe_json:string ->
  unit ->
  unit
(** [store c ~hash ~prefix ~files ~package ~deps ~parent_hashes ~exit_status
     ?recipe_json ()] creates a layer at [<root>/layers/<os_key>/<hash>/]. Each
    file in [files] (relative paths within [prefix]) is hardlinked into [fs/].
    Symlinks are preserved by recreating them with the same target. Writes
    [layer.json] with the provided metadata.

    [recipe_json], when supplied, is written verbatim to [recipe.json] in the
    layer directory. The d10 layer is opaque to the IR — d10 just stores the
    blob. The producer (typically [d10ir.Direct]) writes a single-node d10ir
    [Plan.node] here so the layer is reconstructible from inputs (recipe +
    content-addressed source archive + dep layers).

    {b Cross-process safety} is handled by the {!Oi.Lock.acquire_global}
    invocation in {!Cmd.Harness.bootstrap}: only one [oi] process touches the
    cache at a time, so [store] needs no internal lock. In-process concurrency
    is controlled by the build scheduler ({!D10ir.Direct}) and per-hash
    in-flight tables ({!D10ir.Registry.In_flight}). *)

val load_recipe_json : Config.t -> hash:string -> string option
(** [load_recipe_json c ~hash] reads the layer's [recipe.json] verbatim, or
    [None] when absent / unreadable. The caller decodes via
    [D10ir.Plan.decode_node]. *)

val restore : Config.t -> hash:string -> prefix:string -> unit
(** [restore c ~hash ~prefix] hardlinks the layer's [fs/] tree into [prefix] via
    {!Sysops.link_tree}. No-op if the layer has no [fs/] directory (e.g. virtual
    packages that install no files). *)

(** {1 Remote registry} *)

type remote = [ `Http_remote of string ]
(** A remote layer source. [`Http_remote url] fetches layers as
    [<url>/<os_key>/<hash>.tar.zst]. *)

(** {2 Index}

    Each os_key directory in the registry contains an [index.db] sqlite file
    listing every layer available remotely, plus its tarball sha256 and size
    when one is published (a "layer-cache" registry). Bin-index registries leave
    the tarball columns NULL — restore is impossible from those, but the same
    database still answers "what package provides binary X" / "what opam pkg
    installs ocamlfind library Y". Clients fetch [index.db] once per command and
    read its [layers] rows. *)

type index_entry = { sha256 : string; size : int64 }

type remote_index = (string, index_entry) Hashtbl.t
(** [remote_index] is built from [<remote>/<os_key>/index.db]'s [layers] rows
    whose [tarball_sha256] / [tarball_size] columns are populated. The fetch
    helper itself lives in {!Remote_index} (cycle break — [Index.rebuild] calls
    [Layer.load_meta], so [Layer] can't import [Index] back). *)

type fetch_phase =
  | Fetching
  | Verifying
  | Extracting  (** Stages [pull_remote] passes through, in order. *)

val pull_remote :
  Config.t ->
  session:Sysops.Http.session ->
  remote:remote ->
  hash:string ->
  ?on_progress:(received:int64 -> total:int64 option -> unit) ->
  ?on_phase:(fetch_phase -> unit) ->
  ?sha256:string ->
  unit ->
  bool
(** [pull_remote c ~session ~remote ?sha256 ~hash] downloads layer [hash] from
    [remote] through [session]'s connection pool, optionally verifying the
    SHA-256 checksum of the downloaded archive. Returns [true] if the layer is
    now available with [exit_status = 0]. No-op (returns [true]) if the layer
    already exists locally.

    Concurrent fibers sharing a [session] multiplex over the same HTTP/2
    connection (one handshake per host regardless of how many layers are
    pulled).

    [on_progress] forwards through to {!Sysops.Http.fetch_session} for download
    progress; see that function's docs.

    [on_phase] is called once per phase boundary as the fiber transitions from
    {!Fetching} to {!Verifying} to {!Extracting}. Drives the per-phase counters
    in the [oi] progress bar so post-fetch CPU work (sha256, zstd-tar extract)
    is visible instead of looking like a stall. *)

(** {1 Export} *)

val export : Config.t -> hash:string -> dst:_ Eio.Path.t -> bool
(** [export c ~hash ~dst] creates [<dst>/<os_key>/<hash>.tar.zst] from local
    layer [hash]. Returns [true] if a new archive was created. Returns [false]
    if the layer doesn't exist locally or the archive already exists.

    Used by the streaming uploader to stage one layer's tarball before it is PUT
    to [<base>/<os_key>/layers/<hash>.tar.zst]. There is no bulk [export_all] /
    [oi build --export] anymore — the registry is written exclusively by the
    per-layer streaming upload. *)
