(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zarr V3 sharding indexes, decoded through zarrz codecs. *)
type t
val of_json : Jsont.json -> t option
(** [of_json metadata] is a layout for an array whose sole outer codec is
    [sharding_indexed], or [None] for other layouts. Index codecs and shard
    geometry are validated. Invalid shard metadata raises [Invalid_argument]. *)
val index_range : t -> size:int -> Coverage.t
(** [index_range layout ~size] locates the encoded index at the beginning or
    end of the [size]-byte object. *)
val ranges : t -> size:int -> string -> Coverage.t list
(** [ranges layout ~size bytes] decodes and checks the index [bytes], returning
    encoded inner-chunk intervals and the index interval in offset order.
    Absent chunks are omitted. Invalid offsets, lengths or checksums raise. *)
val expand : Coverage.t list -> Coverage.t -> Coverage.t list
(** [expand chunks requested] covers [requested] and complete encoded inner
    chunks it intersects. Adjacent intervals are coalesced. *)
