(** {1 Synchronous Store Implementations for Zarr}

    Provides synchronous (blocking) store implementations for Zarr v3,
    including in-memory and filesystem stores using Unix I/O.

    Each store type comes with pre-instantiated array, group, and hierarchy
    modules for convenience. *)

(** {2 Stores} *)

(** In-memory store backed by a hash table. Suitable for testing
    and transient data. *)
module Memory_store : sig
  type t

  val create : unit -> t
  (** [create ()] creates a new empty in-memory store. *)

  val get : t -> string -> bytes option
  val get_partial : t -> string -> (int * int option) list -> bytes list option
  val set : t -> string -> bytes -> unit
  val set_partial : t -> (string * int * bytes) list -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
  val list : t -> string list
  val list_prefix : t -> string -> string list
  val list_dir : t -> string -> string list * string list
  val erase_prefix : t -> string -> unit
  val clear : t -> unit
  val length : t -> int
end

(** Filesystem store using Unix blocking I/O. Each key maps to a file
    under the root directory. *)
module Filesystem_store : sig
  type t

  val create : string -> t
  (** [create root] creates a store at directory [root], creating it
      if it does not exist. *)

  val open_ : string -> t option
  (** [open_ root] opens an existing store at [root], returning [None]
      if the directory does not exist. *)

  val get : t -> string -> bytes option
  val get_partial : t -> string -> (int * int option) list -> bytes list option
  val set : t -> string -> bytes -> unit
  val set_partial : t -> (string * int * bytes) list -> unit
  val erase : t -> string -> unit
  val exists : t -> string -> bool
  val list : t -> string list
  val list_prefix : t -> string -> string list
  val list_dir : t -> string -> string list * string list
  val erase_prefix : t -> string -> unit
  val root : t -> string
end

(** {2 Array Operations} *)

(** Array operations using the in-memory store. *)
module Memory_array : module type of Zarr.Array.Make(Memory_store)

(** Array operations using the filesystem store. *)
module Filesystem_array : module type of Zarr.Array.Make(Filesystem_store)

(** {2 Group Operations} *)

(** Group operations using the in-memory store. *)
module Memory_group : sig
  type store = Memory_store.t
  type t

  val create :
    store -> path:string -> ?attributes:Jsont.json -> unit ->
    (t, string) result
  val open_ : store -> path:string -> (t, string) result
  val metadata : t -> Zarr.Metadata.group_metadata
  val path : t -> string
  val children : t -> string list
  val child_exists : t -> string -> bool
  val child_type : t -> string -> [ `Array | `Group ] option
  val attrs : t -> Jsont.json
  val set_attrs : t -> Jsont.json -> unit
  val delete : t -> unit
end

(** Group operations using the filesystem store. *)
module Filesystem_group : sig
  type store = Filesystem_store.t
  type t

  val create :
    store -> path:string -> ?attributes:Jsont.json -> unit ->
    (t, string) result
  val open_ : store -> path:string -> (t, string) result
  val metadata : t -> Zarr.Metadata.group_metadata
  val path : t -> string
  val children : t -> string list
  val child_exists : t -> string -> bool
  val child_type : t -> string -> [ `Array | `Group ] option
  val attrs : t -> Jsont.json
  val set_attrs : t -> Jsont.json -> unit
  val delete : t -> unit
end

(** {2 Hierarchy Operations} *)

(** Hierarchy operations using the in-memory store. *)
module Memory_hierarchy : sig
  type store = Memory_store.t
  val walk : store -> (string -> [ `Array | `Group ] -> unit) -> unit
  val exists : store -> string -> bool
  val delete : store -> string -> unit
end

(** Hierarchy operations using the filesystem store. *)
module Filesystem_hierarchy : sig
  type store = Filesystem_store.t
  val walk : store -> (string -> [ `Array | `Group ] -> unit) -> unit
  val exists : store -> string -> bool
  val delete : store -> string -> unit
end
