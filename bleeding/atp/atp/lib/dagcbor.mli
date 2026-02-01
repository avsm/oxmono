(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** DAG-CBOR codec for IPLD data.

    DAG-CBOR is a strict subset of CBOR designed for content-addressed data
    structures in IPLD (InterPlanetary Linked Data).

    {2 Key Differences from CBOR}

    DAG-CBOR enforces:
    - Only tag 42 (CID links) is allowed; other tags are rejected
    - Map keys must be strings (no integer keys)
    - Floats are always encoded as 64-bit
    - No NaN, Infinity, or -Infinity values
    - Deterministic encoding (shortest integers, sorted map keys)
    - No indefinite-length encoding

    {2 IPLD Data Model}

    The IPLD data model extends JSON with:
    - {b Bytes}: Binary data (CBOR major type 2)
    - {b Link}: Content identifiers (CIDs) encoded with tag 42 *)

(** {1 Types} *)

type value =
  [ `Null
  | `Bool of bool
  | `Int of int64
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Link of Cid.t
  | `List of value list
  | `Map of (string * value) list
    (** Keys sorted by length, then lexicographically *) ]
(** IPLD value type as polymorphic variant. *)

type cid_format = [ `Standard | `Atproto ]
(** CID encoding format for Link values.

    - [`Standard]: Standard DAG-CBOR format with 0x00 multibase prefix and
      multihash length byte. Tag 42 + ByteString(0x00 + CID raw bytes). Total 38
      bytes for SHA-256.

    - [`Atproto]: AT Protocol simplified format without multibase prefix or
      length byte. Tag 42 + ByteString(0x01 + codec + 0x12 + hash). Total 36
      bytes for SHA-256. Per draft-holmgren-at-repository.md Section 6.5 (line
      368). *)

(** {1 Errors} *)

type error =
  [ `Dagcbor_invalid_tag of int
  | `Dagcbor_invalid_map_key
  | `Dagcbor_invalid_float of string
  | `Dagcbor_unsorted_keys
  | `Dagcbor_non_canonical_int
  | `Dagcbor_non_canonical_float
  | `Dagcbor_indefinite_length
  | `Dagcbor_invalid_cid of string
  | `Dagcbor_trailing_data
  | `Dagcbor_unexpected_eof
  | `Dagcbor_decode_error of string ]
(** DAG-CBOR encoding/decoding errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err +=
  | E of error  (** Eio exception wrapper for DAG-CBOR errors. *)

(** {1 Decoding} *)

val decode :
  ?strict:bool -> ?cid_format:cid_format -> Bytesrw.Bytes.Reader.t -> value
(** [decode ?strict ?cid_format reader] decodes a DAG-CBOR value from [reader].

    @param strict
      If [true] (default), enforces canonical encoding: sorted map keys,
      shortest integer encoding, 64-bit floats only. If [false], accepts
      non-canonical encodings.

    @param cid_format
      CID encoding format, default [`Standard]. Use [`Atproto] for AT Protocol
      repositories.

    @raise Eio.Io on decode errors. *)

val decode_string : ?strict:bool -> ?cid_format:cid_format -> string -> value
(** [decode_string ?strict ?cid_format s] decodes a DAG-CBOR value from string
    [s].

    @raise Eio.Io on decode errors. *)

(** {1 Encoding} *)

val encode :
  ?cid_format:cid_format -> value -> eod:bool -> Bytesrw.Bytes.Writer.t -> unit
(** [encode ?cid_format v ~eod writer] encodes IPLD value [v] as DAG-CBOR to
    [writer].

    Encoding is always canonical:
    - Integers use shortest encoding
    - Map keys are sorted by length first, then lexicographically
    - Floats are always 64-bit

    @param cid_format
      CID encoding format, default [`Standard]. Use [`Atproto] for AT Protocol
      repositories.

    @param eod If [true], writes end-of-data marker after encoding.

    @raise Eio.Io if value contains NaN or Infinity floats. *)

val encode_string : ?cid_format:cid_format -> value -> string
(** [encode_string ?cid_format v] encodes IPLD value [v] as a DAG-CBOR byte
    string.

    @raise Eio.Io if value contains NaN or Infinity floats. *)

(** {1 Utilities} *)

val equal : value -> value -> bool
(** Structural equality on IPLD values. *)

val compare : value -> value -> int
(** Total ordering on IPLD values. *)

val pp : value Fmt.t
(** Pretty-print an IPLD value. *)

(** {1 Map Key Ordering} *)

val canonical_key_compare : string -> string -> int
(** [canonical_key_compare k1 k2] compares keys in DAG-CBOR canonical order:
    shorter keys first, then lexicographically. *)

(** {1 JSON Interop} *)

val of_json : Jsont.json -> (value, string) result
(** [of_json j] converts a JSON value to IPLD. Returns [Error] if the JSON
    contains NaN or Infinity. *)

val to_json : value -> (Jsont.json, string) result
(** [to_json v] converts an IPLD value to JSON. Returns [Error] if the value
    contains Bytes or Link types (which have no JSON representation). *)
