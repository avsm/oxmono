type t = { path : string; digest : string }
(** Ordered opam metadata repositories and executable ownership. *)

val defaults : string list

val prepare : Support.proc -> data:string -> refresh:bool -> string -> t
(** [prepare proc ~data ~refresh source] opens a local repository or caches a
    Git clone. *)

val resolve_binary : t list -> string -> string list -> string * string list
(** [resolve_binary repos target packages] returns the executable name and
    solver roots. Unconstrained snapshot roots select exact snapshot versions. *)

val constrain : data:string -> t list -> string list * t list
(** [constrain ~data repos] retains OxCaml patch guards for external packages.
    Explicitly stamped packages supply their own patched recipes, so generated
    guards omit constraints on those local package names. *)

val snapshot_root : t list -> string -> string
(** [snapshot_root repos atom] pins an unconstrained local root to its snapshot. *)
