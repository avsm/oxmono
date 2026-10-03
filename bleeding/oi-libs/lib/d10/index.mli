(** Layer index (SQLite).

    Provides fast lookup of cached layers by package name, binary name, or
    dependency chain. The index is a SQLite database stored at
    [<cache>/layers/index.db] and is rebuilt on demand by scanning the layer
    directories.

    {2 Schema}

    {v
    layers:          hash, os_key, arch, os, distro, os_version,
                     package_name, package_ver, exit_status, created,
                     overlay_handle, overlay_version,
                     tarball_sha256, tarball_size
    layer_deps:      layer_hash, dep_name, dep_version, dep_hash
    layer_binaries:  layer_hash, binary_name        -- files under bin/
    layer_meta:      layer_hash, package_dir,       -- one row per
                     findlib_pkg, archive             ocamlfind subpackage
    layer_files:     layer_hash, path               -- only when [include_files]
    v}

    [overlay_handle] / [overlay_version] identify the reporepo overlay that
    contributed the opam file used to build a layer. Both are NULL for layers
    built before tagging was introduced or for packages that came from a
    pin-depends tree.

    [tarball_sha256] / [tarball_size] are legacy columns: they were only ever
    filled by the deleted registry-export path. The live remote-resolution path
    reads tarball sha/size from the server-published [index.json] instead, so
    these are NULL in the purely-local cache index this module now backs.

    [layer_binaries] enables [oi run <binary>] to look up the package providing
    a binary without scanning layer trees. [layer_meta] is the equivalent for
    ocamlfind: each [lib/<dir>/META] in a layer contributes one row per declared
    subpackage so [oi search ppx_deriving] can route findlib lookups to the
    producing opam package. *)

(** {1 Database lifecycle} *)

type db
(** An open SQLite database handle. *)

val open_ : fs:Eio.Fs.dir_ty Eio.Path.t -> path:string -> db
(** [open_ ~fs ~path] opens (or creates) the index database at [path]. Tables
    are created if they don't already exist. [fs] is the Eio filesystem
    capability, used to create the parent directory of [path] if it doesn't
    already exist. *)

val close : db -> unit
(** [close db] closes the database handle. *)

val indexer_version : string
(** Stamp that {!rebuild} writes into the [index_meta] table per [os_key].
    Callers compare {!indexer_stamp} against this to decide whether the on-disk
    index was produced by the current logic shape; a mismatch is the "force a
    full rebuild" signal. *)

val indexer_stamp : db -> os_key:string -> string option
(** [indexer_stamp db ~os_key] reads the indexer-version stamp recorded for
    [os_key], or [None] when no rebuild has run for that platform yet (e.g. on a
    legacy [index.db] that pre-dates the [index_meta] table). *)

(** {1 Layer-fs scanners}

    Exposed so the [oi]-level layer manifest writer can reuse the same
    findlib-meta parser the SQLite indexer uses, without duplicating the META
    scanning logic. *)

val parse_meta_file :
  package_dir:string -> string -> (string * string option) list
(** [parse_meta_file ~package_dir contents] parses a findlib [META] file and
    returns [(findlib_pkg, archive_opt)] pairs — one for the top-level
    [package_dir] package and one per nested [package "X" (...)] block. *)

val scan_meta :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  string ->
  (string * string * string option) list
(** [scan_meta ~fs fs_dir] walks every [<fs_dir>/lib/<dir>/META] and returns
    [(package_dir, findlib_pkg, archive_opt)] triples for each findlib
    subpackage declared in those files. *)

(** {1 Indexing} *)

val rebuild :
  Config.t ->
  ?overlay_for:(hash:string -> Overlay.t option) ->
  ?include_files:bool ->
  db ->
  unit
(** [rebuild c ?overlay_for ?include_files db] scans all layers under
    [<root>/layers/<os_key>/] and populates the index tables. Existing data for
    [c.os_key] is replaced atomically within a transaction. Each layer's
    [layer.json] is parsed for metadata; its [fs/] tree is scanned for binary
    names ([fs/bin/], [fs/sbin/]) and findlib package metadata (every
    [fs/lib/<dir>/META] is parsed and its declared subpackages recorded in
    [layer_meta]).

    [include_files] (default [false]) controls whether the full file path list
    lands in [layer_files]. The bin-index registry shape leaves this off — the
    table is the bulk of [index.db]'s on-disk size and is only needed by the
    layer-cache shape (where [oi build] verifies tarball contents). [oi search]
    / [binaries_for] / [meta_for] don't consult [layer_files] and work with
    [include_files = false].

    [overlay_for] supplies the per-layer overlay attribution (defaulted to
    [fun ~hash:_ -> None]). The [oi] cache wires this to read from the layer's
    [provenance.json] sidecar; tools that don't care about overlay routing can
    leave it at the default. *)

(** {1 Queries} *)

val find_layer :
  db -> name:string -> version:string -> os_key:string -> (string * int) option
(** [find_layer db ~name ~version ~os_key] returns [(hash, exit_status)] for the
    layer matching the given package, or [None]. *)

val binaries_for :
  db ->
  binary:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [binaries_for db ~binary ~os_key] returns all layers that provide
    [bin/<binary>] or [sbin/<binary>], as
    [(package_name, package_version, layer_hash, overlay)], sorted by opam
    version descending (latest version first). [overlay] is set when the layer
    was tagged with a reporepo overlay; [None] otherwise (pin-depends, local
    trees). *)

val search_binary :
  db ->
  pattern:string ->
  os_key:string ->
  (string * string * string * string * Overlay.t option) list
(** [search_binary db ~pattern ~os_key] searches for binaries matching
    [pattern], returning
    [(binary_name, package_name, package_version, layer_hash, overlay)]. The
    pattern is matched exactly by default; use [*] as a wildcard (mapped to SQL
    [LIKE %]). Results are sorted by binary name then opam version descending.
*)

val search_package :
  db ->
  pattern:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [search_package db ~pattern ~os_key] searches for built packages whose name
    matches [pattern], returning
    [(package_name, package_version, layer_hash, overlay)]. Pattern matching and
    the [overlay] field have the same semantics as {!search_binary}. Results are
    sorted by package name then opam version descending. *)

val meta_for :
  db ->
  findlib_pkg:string ->
  os_key:string ->
  (string * string * string * Overlay.t option) list
(** [meta_for db ~findlib_pkg ~os_key] returns layers whose findlib metadata
    declares [findlib_pkg] (e.g. ["cohttp.async"]), as
    [(package_name, package_version, layer_hash, overlay)] sorted by opam
    version descending. Use [*] as a wildcard for substring search. Reads the
    [layer_meta] table populated by {!rebuild}. *)

val deps : db -> hash:string -> (string * string * string) list
(** [deps db ~hash] returns the direct dependencies of a layer as
    [(dep_name, dep_version, dep_hash)]. *)

val files : db -> hash:string -> string list
(** [files db ~hash] returns all file paths stored in the layer. *)

val all_binaries : db -> os_key:string -> (string * string * string) list
(** [all_binaries db ~os_key] returns all indexed binaries as
    [(binary_name, package_name, package_version)]. *)

type stats = {
  layers : int;
  binaries : int;
  files : int;
  findlib : int;
  tarballs : int;
}
(** Row counts for a single [os_key], scoped to that platform via the [layers]
    join. [files] is zero unless the index was rebuilt with
    [include_files:true]; [tarballs] counts layers whose legacy [tarball_sha256]
    column is non-NULL (always 0 for a fresh local index). *)

val stats : db -> os_key:string -> stats
(** [stats db ~os_key] gathers every count for one platform in a single sqlite
    round-trip per row. *)

(** {1 Invalidation} *)

val dependents : db -> hashes:string list -> os_key:string -> string list
(** [dependents db ~hashes ~os_key] returns the hashes of layers in [os_key]
    that directly depend on any layer in [hashes]. Apply iteratively to compute
    the transitive set of layers poisoned by an invalidated package. Returns
    [[]] when [hashes] is empty. *)

val delete_layers : db -> hashes:string list -> unit
(** [delete_layers db ~hashes] removes the layers and their associated rows from
    [layers], [layer_deps], [layer_binaries], and [layer_files]. The on-disk
    layer directories are not touched — the caller must
    [rmtree <root>/layers/<os_key>/<hash>/] for each entry. No-op when [hashes]
    is empty. *)
