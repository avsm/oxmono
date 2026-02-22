(** Abstract store interface for Zarr v3 *)

(** Readable store operations *)
module type READABLE = sig
  type t

  (** Get the full contents of a key, or None if not found *)
  val get : t -> string -> bytes option

  (** Get partial content from a key.
      Each range is (offset, length option). None length means to end of key.
      Returns None if key not found. *)
  val get_partial : t -> string -> (int * int option) list -> bytes list option

  (** Check if a key exists *)
  val exists : t -> string -> bool
end

(** Writable store operations *)
module type WRITABLE = sig
  type t

  (** Set the contents of a key *)
  val set : t -> string -> bytes -> unit

  (** Set partial content in a key.
      Each tuple is (key, offset, bytes to write). *)
  val set_partial : t -> (string * int * bytes) list -> unit

  (** Erase a key *)
  val erase : t -> string -> unit

  (** Erase all keys with given prefix *)
  val erase_prefix : t -> string -> unit
end

(** Listable store operations *)
module type LISTABLE = sig
  type t

  (** List all keys in the store *)
  val list : t -> string list

  (** List all keys with given prefix *)
  val list_prefix : t -> string -> string list

  (** List directory contents: (keys, prefixes) where prefixes are "subdirectories" *)
  val list_dir : t -> string -> string list * string list
end

(** Complete store interface combining all operations *)
module type STORE = sig
  include READABLE
  include WRITABLE with type t := t
  include LISTABLE with type t := t
end

(** Store operations with result types for error handling *)
module type STORE_WITH_ERRORS = sig
  type t
  type error

  val get : t -> string -> (bytes, error) result
  val get_partial : t -> string -> (int * int option) list -> (bytes list, error) result
  val exists : t -> string -> (bool, error) result
  val set : t -> string -> bytes -> (unit, error) result
  val set_partial : t -> (string * int * bytes) list -> (unit, error) result
  val erase : t -> string -> (unit, error) result
  val erase_prefix : t -> string -> (unit, error) result
  val list : t -> (string list, error) result
  val list_prefix : t -> string -> (string list, error) result
  val list_dir : t -> string -> (string list * string list, error) result
end
