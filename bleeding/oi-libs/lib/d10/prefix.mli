(** Hardlink-assembled prefixes.

    A prefix is a complete OCaml installation directory (with [bin/], [lib/],
    etc.) assembled by hardlinking files from multiple {!Layer} [fs/]
    directories. Layers are applied in topological order so that later packages
    can overwrite files from earlier ones (e.g. [ld.conf]).

    Assembled prefixes are cached by their {!solve_hash} -- the MD5 of all layer
    hashes -- so identical dependency sets produce identical prefixes without
    re-assembly. A [.ready] marker file indicates a complete prefix.

    {2 Prefix diffing}

    During builds, {!snapshot} and {!diff} are used to capture which files a
    package installed. Before installing a package, {!snapshot} records mtimes
    of all files in the prefix. After install, {!diff} compares against the
    snapshot and returns the new or modified files, which are then stored as a
    {!Layer}. Both regular files and symlinks are tracked. *)

(** {1 Assembly} *)

val assemble : Config.t -> layer_hashes:string list -> dst:_ Eio.Path.t -> unit
(** [assemble c ~layer_hashes ~dst] hardlinks each layer's [fs/] directory into
    [dst] in order and rebases [dune-package] metadata. Layers without an [fs/]
    directory (virtual packages) are silently skipped. *)

val assemble_cached : Config.t -> layer_hashes:string list -> string
(** [assemble_cached c ~layer_hashes] assembles a prefix and caches it at
    [<root>/prefixes/<os_key>/<solve_hash>/]. Returns the native path to the
    prefix. If a [.ready] marker exists, the cached prefix is returned
    immediately without re-assembly. *)

val solve_hash : string list -> string
(** [solve_hash hashes] computes the prefix cache key: the MD5 of the sorted,
    newline-joined layer hashes. *)

(** {1 Prefix diff} *)

type snapshot
(** An opaque snapshot of file modification times in a prefix, used to detect
    newly installed files. *)

val snapshot : fs:_ Eio.Path.t -> string -> snapshot
(** [snapshot ~fs prefix] captures the mtime of every regular file and symlink
    under [prefix], traversing directories recursively. The [fs] capability is
    used for directory listing and stat. *)

val diff :
  fs:_ Eio.Path.t -> prefix:string -> before:snapshot -> (string * string) list
(** [diff ~fs ~prefix ~before] returns [(rel_path, abs_path)] pairs for every
    regular file or symlink in [prefix] that is either new (not in [before]) or
    has a newer mtime than recorded in [before]. Used to determine which files a
    package's install step added. *)
