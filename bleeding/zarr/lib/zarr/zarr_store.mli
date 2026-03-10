(** {1 Store Interface}

    Abstract key-value store for Zarr v3 data persistence.

    Follows the {{:https://zarr-specs.readthedocs.io/en/latest/v3/stores/index.html}
    Zarr v3.1 spec, "Stores"} section.  Keys are Unicode strings (no
    trailing [/]).  Values are byte sequences.

    The spec defines three capability levels that implementations may
    support: readable, writable, and listable.  A full {!STORE} provides
    all three.

    {2 Key conventions}

    - Metadata: [<path>/zarr.json] (root metadata at [zarr.json])
    - Chunk data: [<path>/<chunk_key>] where [chunk_key] is determined by
      the chunk key encoding (e.g. [c/0/1/2] for the default encoding)
    - Keys are case-sensitive *)

(** {2 Capability Signatures} *)

(** Readable store: supports key lookup and existence checks.

    {b Zarr v3.1 spec:} [get(key)], [get_partial_values(key_ranges)]. *)
module type READABLE = sig
  type t
  val get : t -> string -> bytes option
  val get_partial : t -> string -> (int * int option) list -> bytes list option
  val exists : t -> string -> bool
end

(** Writable store: supports key mutation and deletion.

    {b Zarr v3.1 spec:} [set(key, value)], [erase(key)],
    [erase_prefix(prefix)]. *)
module type WRITABLE = sig
  type t
  val set : t -> string -> bytes -> unit
  val set_partial : t -> (string * int * bytes) list -> unit
  val erase : t -> string -> unit
  val erase_prefix : t -> string -> unit
end

(** Listable store: supports key enumeration.

    {b Zarr v3.1 spec:} [list()], [list_prefix(prefix)],
    [list_dir(prefix)]. *)
module type LISTABLE = sig
  type t
  val list : t -> string list
  val list_prefix : t -> string -> string list
  val list_dir : t -> string -> string list * string list
  (** [list_dir store prefix] returns [(keys, prefixes)] where [keys]
      are direct children and [prefixes] are child directories. *)
end

(** Full store combining readable, writable, and listable capabilities. *)
module type STORE = sig
  include READABLE
  include WRITABLE with type t := t
  include LISTABLE with type t := t
end
