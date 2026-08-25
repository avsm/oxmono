(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 arrays.

    An array is a {!Store.t}, a node path in it, and the [zarr.json] at
    that path bound to a data type, a fill value, a chunk grid, a chunk
    key encoding and a {!Codec.chain}. Opening resolves all of those
    once, so a read or a write only talks to the store and the codecs.

    Shapes are [int array] here. {!Subset.t} and {!Slab.t} carry
    [int iarray], which [Stdlib_stable.Iarray.of_array] converts.

    Every failure raises {!Error.E}. A shape or an index that does not
    fit the array is a programming error and raises
    [Invalid_argument]. *)

type t
(** The type for open arrays. *)

(** {1 Opening and creating} *)

val open_ : ?codecs:Codec.resolver -> Store.t -> path:string -> t
(** [open_ store ~path] reads [<path>/zarr.json] and binds it. [codecs]
    is consulted before the built-in codecs, so a caller can supply its
    own.

    @raise Error.E [(Store _)] when there is no metadata document at
    [path], with a message naming the key as not found. [(Metadata _)]
    when the document is not array metadata this library supports, which
    covers an unknown data type, a data type carrying a configuration or
    [must_understand] [false], a chunk grid other than ["regular"], an
    unknown chunk key encoding, a fill value that does not match the data
    type, and a storage transformer whose [must_understand] is true.
    [(Codec _)] when the codec chain cannot be built. *)

val of_json : ?codecs:Codec.resolver -> Store.t -> path:string ->
  Jsont.json -> t
(** [of_json store ~path j] is {!open_} on an already parsed metadata
    document [j], for a caller such as {!Node.open_} that has read
    [<path>/zarr.json] for another reason and must not read it twice. *)

val create :
  ?attributes:Jsont.json ->
  ?dimension_names:string option list ->
  ?codecs:Ext.t list ->
  ?chunk_key_encoding:Chunk_key.t ->
  ?resolver:Codec.resolver ->
  shape:int array ->
  chunk_shape:int array ->
  dtype:Dtype.t ->
  fill_value:Fill_value.t ->
  Store.t ->
  path:string ->
  t
(** [create ~shape ~chunk_shape ~dtype ~fill_value store ~path] writes
    [<path>/zarr.json] and is the array it describes. No chunk is
    written, so the array reads as [fill_value] everywhere.

    [codecs] defaults to the single little endian [bytes] codec, which
    is the chain every Zarr V3 reader understands.
    [chunk_key_encoding] defaults to {!Chunk_key.default}. [resolver]
    plays the part [codecs] plays in {!open_}, resolving names the
    built-ins do not know.

    @raise Error.E [(Metadata _)] when the shapes disagree, when a chunk
    length is not positive, when [dimension_names] is not as long as
    [shape], or when [fill_value] is not one element of [dtype] wide.
    [(Codec _)] when the chain cannot be built. [(Store _)] when [store]
    cannot be written. *)

(** {1 Properties} *)

val store : t -> Store.t
(** [store t] is the store [t] was opened in. *)

val path : t -> string
(** [path t] is the node path of [t]. *)

val metadata : t -> Metadata.array_meta
(** [metadata t] is the parsed [zarr.json] of [t]. *)

val shape : t -> int array
(** [shape t] is a copy of the extent of [t] in each dimension. *)

val dtype : t -> Dtype.t
(** [dtype t] is the data type of the elements of [t]. *)

val fill_value : t -> Fill_value.t
(** [fill_value t] is the value an absent chunk of [t] reads as. *)

val attributes : t -> Jsont.json option
(** [attributes t] is the [attributes] member of the metadata. *)

val dimension_names : t -> string option list option
(** [dimension_names t] is the [dimension_names] member, a [None] entry
    being an unnamed dimension. *)

val chunk_shape : t -> int array
(** [chunk_shape t] is a copy of the shape every chunk of [t] is stored
    at, edge chunks included. *)

val grid_shape : t -> int array
(** [grid_shape t] is a copy of the number of chunks along each
    dimension. *)

val chunk_key : t -> int array -> string
(** [chunk_key t i] is the store key of the chunk at grid index [i]. It
    is not checked that [i] is within {!grid_shape}. *)

(** {1 Reading and writing} *)

val read_chunk : t -> int array -> Slab.t
(** [read_chunk t i] is the chunk at grid index [i], always at the full
    {!chunk_shape}. A chunk that is not in the store is a fresh slab
    filled with {!fill_value}, so the part of an edge chunk that lies
    beyond the array holds the fill value too.

    @raise Invalid_argument if [i] is not an index of {!grid_shape}. *)

val read_chunk_opt : t -> int array -> Slab.t option
(** [read_chunk_opt t i] is {!read_chunk} but [None] when the chunk is
    not in the store, for a caller that must tell an absent chunk from
    one written full of fill values.

    @raise Invalid_argument if [i] is not an index of {!grid_shape}. *)

val read : t -> Subset.t -> Slab.t
(** [read t sub] is the region [sub] of [t] as a slab of shape
    [sub.shape], assembled from every chunk [sub] overlaps. The part of
    [sub] whose chunk is absent from the store reads as {!fill_value},
    and an absent region costs one fill chunk however many chunks it
    spans.

    A subset that is exactly one whole chunk is {!read_chunk}, with no
    assembly. Otherwise the chunks are read in C order, each whole,
    save that a chunk is read through ranged store requests when
    {!Codec.supports_partial} holds of the chain, the store's [ranged]
    is set and its [size] answers. That is the path on which a shard
    costs the inner chunks [sub] touches rather than the whole object.

    @raise Invalid_argument if [sub] does not lie within {!shape}. *)

val write_chunk : t -> int array -> Slab.t -> unit
(** [write_chunk t i s] encodes [s] and stores it as the chunk at grid
    index [i]. [s] must have {!dtype} and the full {!chunk_shape},
    including the part of an edge chunk beyond the array.

    @raise Invalid_argument if [i] is not an index of {!grid_shape} or
    [s] does not match.
    @raise Error.E [(Store _)] if the store cannot be written. *)

val write : t -> Subset.t -> Slab.t -> unit
(** [write t sub s] writes the slab [s], of shape [sub.shape], into the
    region [sub] of [t].

    A chunk the subset covers whole is built from [s] alone. Any other
    chunk is read first, so that the elements outside [sub] and the part
    of an edge chunk beyond the array keep the values they had, or the
    fill value when the chunk was absent. An edge chunk is never covered
    whole, since [sub] cannot reach past the array, so writing to one
    always costs a read.

    @raise Invalid_argument if [sub] does not lie within {!shape} or [s]
    does not match [sub].
    @raise Error.E [(Store _)] if the store cannot be written. *)
