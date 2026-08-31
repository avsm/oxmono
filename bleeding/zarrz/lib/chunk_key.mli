(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Chunk key encodings.

    The two encodings of the Zarr V3 core. A chunk key names a chunk
    within a node. {!data_key} and {!meta_key} turn it and a node path
    into the store key. *)

type t =
  | Default of { separator : char }
      (** The key is ["c"] then the indices, all joined by [separator],
          which defaults to ['/']. *)
  | V2 of { separator : char }
      (** The key is the indices joined by [separator], which defaults
          to ['.']. *)

val default : t
(** [default] is [Default { separator = '/' }]. *)

val v2 : t
(** [v2] is [V2 { separator = '.' }]. *)

val of_ext : Ext.t -> (t, string) result
(** [of_ext e] is the encoding described by the [chunk_key_encoding]
    member [e]. The name must be ["default"] or ["v2"] and
    [must_understand] must be [true], which the spec requires of this
    extension point. The configuration, when present, must be an object
    with no member other than [separator], a string that is ["/"] or
    ["."]. An absent separator is the encoding's default. *)

val to_ext : t -> Ext.t
(** [to_ext t] is the [chunk_key_encoding] member for [t]. The separator
    is always written out. *)

val encode : t -> int array -> string
(** [encode t i] is the chunk key of the chunk at grid index [i]. A zero
    dimensional array has the single key ["c"] under {!Default} and
    ["0"] under {!V2}. Raises [Invalid_argument] if an index is
    negative. *)

val data_key : path:string -> string -> string
(** [data_key ~path k] is the store key of the chunk whose chunk key is
    [k] in the node at [path]. [path] is an absolute node path such as
    ["/"] or ["/foo/bar"]. The leading ['/'] is dropped, so the root
    gives [k] itself. *)

val meta_key : path:string -> string
(** [meta_key ~path] is the store key of the metadata document of the
    node at [path], ["zarr.json"] at the root. *)
