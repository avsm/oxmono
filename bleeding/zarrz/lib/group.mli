(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 groups.

    A group is a node that holds nothing but attributes. Its children
    are the nodes one path component below it, which only a store that
    can list keys can enumerate. *)

type t
(** The type for open groups. *)

val open_ : Store.t -> path:string -> t
(** [open_ store ~path] reads [<path>/zarr.json] and parses it as group
    metadata.

    @raise Error.E [(Store _)] when there is no metadata document at
    [path], with a message naming the key as not found, and
    [(Metadata _)] when the document is not group metadata. *)

val of_json : Store.t -> path:string -> Jsont.json -> t
(** [of_json store ~path j] is {!open_} on an already parsed metadata
    document [j], for a caller such as {!Node.open_} that has read
    [<path>/zarr.json] for another reason. *)

val create : ?attributes:Jsont.json -> Store.t -> path:string -> t
(** [create store ~path] writes [<path>/zarr.json] and is the group it
    describes.

    @raise Error.E [(Store _)] when [store] cannot be written. *)

val store : t -> Store.t
(** [store t] is the store [t] was opened in. *)

val path : t -> string
(** [path t] is the node path of [t]. *)

val metadata : t -> Metadata.group_meta
(** [metadata t] is the parsed [zarr.json] of [t]. *)

val attributes : t -> Jsont.json option
(** [attributes t] is the [attributes] member of the metadata. *)

val children : t -> string list option
(** [children t] are the names of the nodes directly below [t], sorted
    and without repeats, or [None] when the store cannot list keys.

    A name is reported when the store holds the key that a child's own
    metadata document would occupy, so a directory of chunks is not
    mistaken for a node and a node with no metadata is not reported. *)
