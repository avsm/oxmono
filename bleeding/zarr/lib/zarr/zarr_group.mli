(** {1 Zarr v3 Group Operations}

    Groups are container nodes in the Zarr hierarchy.  They hold metadata
    (including user-defined attributes) and may contain child arrays and
    sub-groups.

    See {{:https://zarr-specs.readthedocs.io/en/latest/v3/core/index.html}
    Zarr v3.1 spec, "Group" section}.

    {b Zarr v3.1 spec:} A group is an internal node in the hierarchy
    tree.  It has metadata (stored at [<path>/zarr.json]) with
    [node_type = "group"] and optional [attributes]. *)

(** Group handle containing store, path, and metadata. *)
type 'store t

(** Store operations required by the group functor. *)
module type STORE_OPS = sig
  type t
  val get : t -> string -> bytes option
  val set : t -> string -> bytes -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
  val list_dir : t -> string -> string list * string list
end

(** Functor creating group operations over a specific store type. *)
module Make (S : STORE_OPS) : sig
  type store = S.t
  type nonrec t = S.t t

  val create :
    store -> path:string -> ?attributes:Jsont.json -> unit ->
    (t, string) result
  (** [create store ~path ?attributes ()] creates a new group at [path],
      writing its [zarr.json] metadata to the store. *)

  val open_ : store -> path:string -> (t, string) result
  (** [open_ store ~path] opens an existing group by reading its metadata. *)

  val metadata : t -> Zarr_metadata.group_metadata
  val path : t -> string

  val children : t -> string list
  (** [children g] lists the names of direct child nodes.  Discovers
      children by combining [zarr.json] file listings and directory
      prefixes from {!STORE_OPS.list_dir}. *)

  val child_exists : t -> string -> bool
  (** [child_exists g name] checks whether a child node exists by
      probing for [<path>/<name>/zarr.json]. *)

  val child_type : t -> string -> [ `Array | `Group ] option
  (** [child_type g name] returns the node type of a child by reading
      its [node_type] field, or [None] if the child does not exist or
      its metadata cannot be parsed. *)

  val attrs : t -> Jsont.json
  (** [attrs g] returns the user-defined attributes (JSON null if absent). *)

  val set_attrs : t -> Jsont.json -> unit
  (** [set_attrs g json] updates the group's attributes in the store. *)

  val delete : t -> unit
  (** [delete g] erases the group's metadata from the store. *)
end

(** {2 Hierarchy Operations}

    Operations that span the entire Zarr node hierarchy.

    {b Zarr v3.1 spec:} "Discover All Nodes: recursively apply discover
    children starting from root.  Alternative: [list_prefix("/")], all
    [zarr.json] keys are nodes." *)

module Hierarchy : sig
  module type STORE_OPS = sig
    type t
    val get : t -> string -> bytes option
    val exists : t -> string -> bool
    val erase_prefix : t -> string -> unit
    val list_prefix : t -> string -> string list
  end

  module Make (S : STORE_OPS) : sig
    type store = S.t

    val walk : store -> (string -> [ `Array | `Group ] -> unit) -> unit
    (** [walk store f] calls [f path node_type] for every node in the
        store, visiting all [zarr.json] files. *)

    val exists : store -> string -> bool
    (** [exists store path] checks whether a node exists at [path]. *)

    val delete : store -> string -> unit
    (** [delete store path] deletes a node and all its descendants
        using [erase_prefix]. *)
  end
end
