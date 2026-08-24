(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Key to bytes stores.

    A store is a record of closures rather than a functor, so a backend
    is an ordinary runtime value. Keys are the relative paths the Zarr
    specification defines, such as ["a/b/zarr.json"] and ["a/b/c/0/0"].

    A key that is not in the store is [None] everywhere. Any other
    failure raises {!Error.E} with a {!Error.Store} payload. A store
    with no [set], [erase] or [list] is read only in that respect, and
    the operation that needs it raises. *)

type t = {
  get : key:string -> Base_bigstring.t option;
      (** [get ~key] is the whole object at [key]. *)
  get_range : key:string -> Byte_range.t -> Base_bigstring.t option;
      (** [get_range ~key r] is the bytes of [r] in the object at [key],
          truncated to the object as {!Byte_range.resolve} truncates. *)
  get_ranges : key:string -> Byte_range.t list -> Base_bigstring.t list option;
      (** [get_ranges ~key rs] is one buffer per range, in order,
          possibly fetched in one batch. *)
  size : key:string -> int option;
      (** [size ~key] is the byte length of the object at [key]. A store
          that cannot answer without fetching the object reports [None],
          which a caller cannot distinguish from an absent key and
          must therefore treat as a reason to fall back to [get]. *)
  ranged : bool;
      (** [true] when [get_range] avoids fetching the whole object, so
          a partial read is cheaper than a full one. *)
  set : (key:string -> Base_bigstring.t -> unit) option;
      (** [set ~key b] stores [b] at [key], replacing any earlier
          object. *)
  erase : (key:string -> unit) option;
      (** [erase ~key] removes [key]. Removing an absent key is not an
          error. *)
  list : (prefix:string -> string list) option;
      (** [list ~prefix] are the keys that start with [prefix], sorted.
          An empty prefix lists the whole store. *)
}
(** The type for stores. *)

val memory : unit -> t
(** [memory ()] is a fresh store holding its objects in a hash table.
    Every operation is supported.

    [set] and [get] both copy, so a caller can neither mutate a stored
    object through the buffer it wrote nor through one it read.

    [ranged] is [true]. Slicing a buffer in memory is free, so a ranged
    read really is no more expensive than a whole one, and the flag
    keeps the memory store on the same code path as a store that
    fetches over a network. *)

val get_json : t -> key:string -> Jsont.json
(** [get_json t ~key] is the JSON document stored at [key].

    @raise Error.E [(Store _)] when [key] is absent, with a message
    naming the key as not found, and [(Metadata _)] when its bytes are
    not JSON. The two are distinguishable so that a caller can tell a
    missing node from a corrupt one. *)
