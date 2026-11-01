(** Installed files and metadata stored under a caller-supplied hash.

    [<root>/layers/<os_key>/<hash>/] contains [layer.json], an optional opaque
    [recipe.json], and [fs/] with the installed files. {!Prefix.diff} identifies
    files to capture. Regular files are hardlinked and symlinks retain their
    targets. Callers keep completed layers immutable and serialize writes. *)

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

val load_meta : _ Eio.Path.t -> meta option
(** [load_meta path] reads and parses [layer.json] from [path]. Returns [None]
    if the file does not exist or cannot be parsed. *)

val meta_codec : meta Jsont.t
(** [meta_codec] encodes and decodes [layer.json] metadata. *)

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
(** [store c ~hash ~prefix ~files ~package ~deps ~parent_hashes ~exit_status ?recipe_json ()]
    creates a layer at [<root>/layers/<os_key>/<hash>/]. Each file in [files]
    (relative paths within [prefix]) is hardlinked into [fs/]. Symlinks are
    preserved by recreating them with the same target. Writes [layer.json] with
    the provided metadata.

    [recipe_json], when supplied, is written verbatim to [recipe.json] in the
    layer directory. D10 treats this as an opaque blob. An IR producer can store
    a serialized [D10ir.Plan.node] for replay with its source archive and
    dependency layers.

    Callers must serialize writes to the same layer across processes and fibers.
    This operation takes no lock internally. *)

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
    [<url>/<os_key>/layers/<hash>.tar.zst]. *)

type index_entry = { sha256 : string; size : int64 }

type remote_index = (string, index_entry) Hashtbl.t
(** Checksums and sizes keyed by layer hash, as fetched by {!Remote_index}. *)

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
    already succeeded locally.

    [on_progress] has the contract of {!Sysops.Http.fetch_session}. [on_phase]
    reports download, verification and extraction boundaries. *)

(** {1 Export} *)

val export : Config.t -> hash:string -> dst:_ Eio.Path.t -> bool
(** [export c ~hash ~dst] creates [<dst>/<os_key>/<hash>.tar.zst] from local
    layer [hash]. Returns [true] if a new archive was created. Returns [false]
    if the layer doesn't exist locally or the archive already exists. *)
