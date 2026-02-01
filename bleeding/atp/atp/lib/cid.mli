(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Content Identifiers (CIDv1) for content-addressed data.

    CIDs are self-describing content-addressed identifiers used in IPLD. This
    implementation supports CIDv1 with SHA-256 hashing and DAG-CBOR or raw
    content codecs.

    {2 Wire Format}

    CIDv1 binary format (36 bytes for SHA-256):
    {v
    [version: 0x01]
    [codec: 0x55 raw | 0x71 dag-cbor]
    [hash-codec: 0x12 sha256]
    [hash-length: 0x20 = 32]
    [hash: 32 bytes]
    v}

    With 0x00 multibase identity prefix: 37 bytes. Base32 encoded: 59 characters
    (with 'b' prefix). *)

(** {1 Types} *)

type codec = [ `Raw | `Dag_cbor ]
(** Content codec.
    - [`Raw] (0x55): arbitrary bytes
    - [`Dag_cbor] (0x71): DAG-CBOR encoded IPLD data *)

type t
(** Opaque CIDv1 content identifier. *)

(** {1 Errors} *)

type error =
  [ `Invalid_cid_version of int
  | `Invalid_cid_codec of int
  | `Invalid_cid_hash_codec of int
  | `Invalid_cid_length of int * int  (** expected, got *)
  | `Invalid_cid_multibase of string
  | `Cid_trailing_bytes of int
  | `Cid_encode_error of string ]
(** CID parsing and encoding errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err += E of error  (** Eio exception wrapper for CID errors. *)

(** {1 Creation} *)

val create : [< codec ] -> string -> t
(** [create codec data] hashes [data] with SHA-256 and creates a CID.

    Example:
    {[
      let cid = Cid.create `Dag_cbor cbor_bytes
    ]} *)

val empty : [< codec ] -> t
(** [empty codec] creates a CID with zero-length digest. Used for representing
    empty/null nodes. *)

val of_digest : [< codec ] -> string -> t
(** [of_digest codec digest] creates a CID from a pre-computed SHA-256 digest.

    @raise Invalid_argument if [digest] is not 32 bytes. *)

(** {1 Parsing} *)

val of_bytes : string -> t
(** [of_bytes s] parses a CID from binary format with 0x00 multibase prefix.

    Expects 37 bytes (1 prefix + 36 CID) or 5 bytes (empty CID).

    @raise Eio.Io on invalid format. *)

val of_raw_bytes : string -> t
(** [of_raw_bytes s] parses a CID from raw binary format (no multibase prefix).

    Expects 36 bytes or 4 bytes (empty CID).

    @raise Eio.Io on invalid format. *)

val of_raw_bytes_prefix : string -> t * int
(** [of_raw_bytes_prefix s] parses a CID from the beginning of [s].

    Returns [(cid, len)] where [len] is the number of bytes consumed. This is
    useful for parsing CIDs embedded in larger buffers (e.g., CAR blocks).

    @raise Eio.Io on invalid format. *)

val of_string : string -> t
(** [of_string s] parses a base32-encoded CID.

    Expects 59 characters (full CID) or 8 characters (empty CID).

    @raise Eio.Io on invalid format. *)

(** {1 Serialization} *)

val to_bytes : t -> string
(** [to_bytes cid] serializes with 0x00 multibase identity prefix.

    Returns 37 bytes for full CID, 5 bytes for empty CID. *)

val to_raw_bytes : t -> string
(** [to_raw_bytes cid] returns raw CID bytes without multibase prefix.

    Returns 36 bytes for full CID, 4 bytes for empty CID. *)

val to_string : t -> string
(** [to_string cid] encodes as base32 string with 'b' prefix.

    Returns 59 characters for full CID, 8 characters for empty CID.

    @raise Eio.Io on encoding error. *)

(** {1 Accessors} *)

val codec : t -> codec
(** [codec cid] returns the content codec. *)

val digest : t -> string
(** [digest cid] returns the raw SHA-256 digest bytes (32 bytes or empty). *)

val version : t -> int
(** [version cid] returns the CID version (always 1). *)

val is_empty : t -> bool
(** [is_empty cid] is [true] if this is an empty CID (zero-length digest). *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff CIDs [a] and [b] are identical. *)

val compare : t -> t -> int
(** [compare a b] is a total ordering on CIDs. *)

val hash : t -> int
(** [hash cid] returns a hash suitable for hash tables. *)

(** {1 Pretty Printing} *)

val pp : t Fmt.t
(** [pp ppf cid] prints the base32 representation. *)

(** {1 JSON Encoding} *)

val jsont : t Jsont.t
(** Jsont codec for CIDs.

    Encodes as [{"$link": "<base32-cid>"}] per AT Protocol JSON format. *)

(** {1 Collections} *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

(** {1 AT Protocol Format}

    AT Protocol uses a simplified CID encoding without the multibase prefix and
    without the multihash length byte. This reduces the encoded size from 37
    bytes to 35 bytes.

    Per draft-holmgren-at-repository.md Section 6.5 (line 368): "SHA-256 hash
    links are represented as CBOR byte strings under tag 42, with the byte
    string containing the 32-byte hash value prefixed by the fixed byte sequence
    0x017112."

    AT Protocol format (35 bytes):
    {v
    [version: 0x01]
    [codec: 0x55 raw | 0x71 dag-cbor]
    [hash-codec: 0x12 sha256]
    [hash: 32 bytes]
    v}

    Note: No 0x00 multibase prefix, no 0x20 length byte. *)

val to_atproto_bytes : t -> string
(** [to_atproto_bytes cid] returns the AT Protocol CID format.

    Returns 35 bytes: version (0x01) + codec + hash-codec (0x12) + 32-byte hash.
    For raw codec, prefix is 0x015512. For dag-cbor, prefix is 0x017112. *)

val of_atproto_bytes : string -> t
(** [of_atproto_bytes s] parses a CID from AT Protocol format.

    Expects exactly 35 bytes.

    @raise Eio.Io on invalid format. *)
