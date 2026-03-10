(** {1 Store Interface}

    Abstract key-value store interface for Zarr v3.
    See Zarr v3.1 spec, "Store" section.

    Implementations include filesystem, memory, and cloud stores.
    The interface is split into readable, writable, and listable
    capabilities that can be combined. *)

(** Readable store operations. *)
module type READABLE = sig
  type t
  val get : t -> string -> bytes option
  val get_partial : t -> string -> (int * int option) list -> bytes list option
  val exists : t -> string -> bool
end

(** Writable store operations. *)
module type WRITABLE = sig
  type t
  val set : t -> string -> bytes -> unit
  val set_partial : t -> (string * int * bytes) list -> unit
  val erase : t -> string -> unit
  val erase_prefix : t -> string -> unit
end

(** Listable store operations. *)
module type LISTABLE = sig
  type t
  val list : t -> string list
  val list_prefix : t -> string -> string list
  val list_dir : t -> string -> string list * string list
end

(** Complete store combining all capabilities. *)
module type STORE = sig
  include READABLE
  include WRITABLE with type t := t
  include LISTABLE with type t := t
end
