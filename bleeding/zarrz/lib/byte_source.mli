(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Ranged access to one stored chunk's bytes.

    A value of this type stands for the encoded bytes of a chunk that is
    known to exist. Missing chunks are handled before one is built. The
    sharding codec reads its index and its inner chunks through this
    interface, so a ranged store can serve a shard without fetching it
    whole. *)

type t = {
  size : unit -> int;  (** Total byte length of the chunk. *)
  read : Byte_range.t -> Base_bigstring.t;
      (** [read r] is the bytes of [r]. Raises {!Error.E} on a range
          beyond the end. *)
  read_many : Byte_range.t list -> Base_bigstring.t list;
      (** [read_many rs] is [List.map read rs], possibly batched. *)
}

val of_bigstring : Base_bigstring.t -> t
(** [of_bigstring b] serves ranges of [b] from memory. Reads return
    copies, so callers may mutate the results freely. *)
