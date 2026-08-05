(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Merkle Search Tree for AT Protocol repositories.

    The MST is a deterministic search tree where:
    - Keys are sorted lexicographically
    - Tree structure is determined by SHA-256 hash of keys
    - Nodes are content-addressed via CIDs
    - Subtrees are loaded lazily

    {2 Tree Structure}

    Each node contains:
    - An optional left subtree (keys less than first entry)
    - A list of entries, each with:
    - Key (with prefix compression)
    - Value CID (pointing to record data)
    - Optional right subtree (keys between this entry and next)

    The layer of each key is determined by counting leading zero pairs in its
    SHA-256 hash. Higher layers are closer to the root. *)

(** {1 Raw Serialization Types} *)

module Raw : sig
  type entry = {
    p : int;  (** Prefix length shared with previous key *)
    k : string;  (** Key suffix after shared prefix *)
    v : Cid.t;  (** Link to record data *)
    t : Cid.t option;  (** Link to right subtree, if any *)
  }
  (** Entry in serialized form with prefix compression. *)

  type node = {
    l : Cid.t option;  (** Link to left subtree *)
    e : entry list;  (** Ordered entries at this node *)
  }
  (** Node in serialized form. *)

  val encode : node -> Dagcbor.value
  (** Encode node as DAG-CBOR value. *)

  val decode : Dagcbor.value -> node
  (** Decode node from DAG-CBOR value.
      @raise Eio.Io on invalid format. *)

  val decode_bytes : string -> node
  (** Decode node from DAG-CBOR bytes.
      @raise Eio.Io on invalid format. *)

  val encode_bytes : node -> string
  (** Encode node to DAG-CBOR bytes. *)
end

(** {1 In-Memory Types} *)

type entry = {
  layer : int;
  key : string;
  value : Cid.t;
  mutable right : node option Lazy.t;
}
(** Entry with full key and lazy right subtree. *)

and node = {
  layer : int;
  mutable left : node option Lazy.t;
  mutable entries : entry list;
}
(** Node with lazy children. *)

(** {1 Errors} *)

type error =
  [ `Mst_invalid_key of string
  | `Mst_missing_block of Cid.t
  | `Mst_corrupt_node of string
  | `Mst_keys_not_sorted ]
(** MST errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err += E of error  (** Eio exception wrapper for MST errors. *)

(** {1 Layer Calculation} *)

val layer_of_key : string -> int
(** [layer_of_key key] computes the MST layer for a key.

    Algorithm per draft-holmgren-at-repository.md Section 4.1.3: 1. Compute the
    SHA-256 hash of the key 2. Count the number of leading binary zeros in the
    hash 3. Divide by 2, rounding down to the nearest integer

    This provides an average fanout of 4 (since 2^2 = 4). *)

(** {1 Creation} *)

val empty : node
(** Empty MST (layer 0, no entries). *)

val leaf : string -> Cid.t -> node
(** [leaf key value] creates a single-entry MST. *)

(** {1 Queries} *)

val get : string -> node -> store:Blockstore.readable -> Cid.t option
(** [get key node ~store] looks up value for [key]. *)

val mem : string -> node -> store:Blockstore.readable -> bool
(** [mem key node ~store] checks if [key] exists. *)

val leaves : node -> store:Blockstore.readable -> (string * Cid.t) Seq.t
(** All key-value pairs in sorted order (in-order traversal). *)

(** {1 Mutations} *)

val add : string -> Cid.t -> node -> store:Blockstore.writable -> node
(** [add key value node ~store] inserts or updates entry. Returns new MST root
    (old nodes unchanged). *)

val remove : string -> node -> store:Blockstore.writable -> node
(** [remove key node ~store] deletes entry if present. *)

(** {1 Persistence} *)

val to_cid : node -> store:Blockstore.writable -> Cid.t
(** [to_cid node ~store] serializes node to store, returns root CID. *)

val of_cid : Cid.t -> store:Blockstore.readable -> node
(** [of_cid cid ~store] loads node from store. Children loaded lazily on access.
*)

(** {1 Export} *)

val to_blocks : node -> store:Blockstore.readable -> (Cid.t * string) Seq.t
(** All node blocks in breadth-first order (for CAR export). *)

(** {1 Diff} *)

type diff_op =
  [ `Add of string * Cid.t
  | `Update of string * Cid.t * Cid.t
  | `Remove of string * Cid.t ]
(** Difference operation: add, update (old, new), or remove. *)

val diff : old:node -> new_:node -> store:Blockstore.readable -> diff_op Seq.t
(** [diff ~old ~new_ ~store] computes differences between two MSTs. *)
