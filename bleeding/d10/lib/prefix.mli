(** Layer assembly, writable installation prefixes and installed-file deltas.
    Callers must serialize mutations to each prefix and the layer store. *)

val assemble : Config.t -> layer_hashes:string list -> dst:_ Eio.Path.t -> unit
(** [assemble c ~layer_hashes ~dst] restores layers in order, hardlinking files
    and rebasing dune-package metadata. The destination is not cleared. *)

val path : Config.t -> hash:string -> string
(** [path c ~hash] is the permanent installation prefix for [hash]. *)

val prepare : Config.t -> layer_hashes:string list -> dst:_ Eio.Path.t -> unit
(** [prepare c ~layer_hashes ~dst] replaces [dst] with a writable layer union.
    Hardlinks to the store are detached before returning. *)

val ready : fs:_ Eio.Path.t -> key:string -> string -> bool
(** [ready ~fs ~key prefix] tests the prefix's completion marker. *)

val mark_ready : fs:_ Eio.Path.t -> key:string -> string -> unit
(** [mark_ready ~fs ~key prefix] atomically records successful completion. *)

val ensure :
  Config.t -> key:string -> layer_hashes:string list -> dst:_ Eio.Path.t -> unit
(** [ensure c ~key ~layer_hashes ~dst] prepares and marks an incomplete prefix.
    [key] must identify the ordered layer list. *)

val closure : Config.t -> string list -> string list
(** [closure c hashes] reads layer metadata and returns each layer once, with
    dependencies first. Missing, failed and cyclic dependencies raise. *)

val restore : Config.t -> hash:string -> unit
(** [restore c ~hash] reconstructs missing permanent prefixes for the layer and
    its dependencies using their stored metadata. *)

val assemble_cached : Config.t -> layer_hashes:string list -> string
(** [assemble_cached c ~layer_hashes] returns a cached writable layer union. *)

val solve_hash : string list -> string
(** [solve_hash hashes] identifies an ordered layer list. *)

type snapshot
(** File contents, permissions and symlink targets before installation. *)

val snapshot : fs:_ Eio.Path.t -> string -> snapshot
(** [snapshot ~fs prefix] records files without following directory symlinks.
    The prefix completion marker is excluded. *)

val diff :
  fs:_ Eio.Path.t -> prefix:string -> before:snapshot -> (string * string) list
(** [diff ~fs ~prefix ~before] returns changed or added files as relative and
    absolute path pairs. Removing dependency files raises an exception. *)
