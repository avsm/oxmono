(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Lexicon data types for AT Protocol records.

    Lexicon extends the IPLD data model with blob references and provides the
    value types used in AT Protocol repository records.

    {2 Value Types}

    Lexicon values are a superset of IPLD values:
    - All IPLD primitives: null, bool, int, float, string, bytes, link
    - {b Blob references}: Maps with [$type: "blob"] are parsed as blob refs
    - {b Recursive containers}: Arrays and maps containing Lexicon values

    {2 Records}

    Repository records are Lexicon maps that must contain a [$type] field
    identifying the record schema (e.g., ["app.bsky.feed.post"]).

    {2 JSON Representation}

    When exchanged over HTTP/XRPC, Lexicon values use JSON:
    - Bytes are encoded as [{"$bytes": "<base64>"}]
    - CID links are encoded as [{"$link": "<cid-string>"}]
    - Blob refs include [$type: "blob"] with ref/mimeType/size *)

(** {1 Values} *)

type value =
  [ `Null
  | `Bool of bool
  | `Int of int64
  | `Float of float
  | `String of string
  | `Bytes of string
  | `Link of Cid.t
  | `Blob of Blob_ref.t
  | `List of value list
  | `Map of (string * value) list ]
(** Lexicon value type. Extends IPLD with blob reference detection. *)

(** {1 IPLD Conversion} *)

val of_dagcbor : Dagcbor.value -> value
(** [of_dagcbor v] converts an IPLD value to a Lexicon value. Maps with
    [$type: "blob"] or legacy [cid]+[mimeType] are converted to [`Blob]
    references. *)

val to_dagcbor : value -> Dagcbor.value
(** [to_dagcbor v] converts a Lexicon value to an IPLD value. Blob references
    are encoded as maps with [$type: "blob"]. *)

(** {1 CBOR Encoding} *)

val encode_cbor : value -> string
(** [encode_cbor v] encodes a Lexicon value as DAG-CBOR bytes. *)

val decode_cbor : string -> value
(** [decode_cbor s] decodes DAG-CBOR bytes to a Lexicon value.
    @raise Eio.Io on decode errors. *)

val to_block : value -> Cid.t * string
(** [to_block v] encodes value as DAG-CBOR and returns (CID, bytes). *)

(** {1 JSON Encoding} *)

val jsont : value Jsont.t
(** Jsont codec for Lexicon values.

    Handles special AT Protocol JSON encodings:
    - [{"$bytes": "<base64>"}] decodes to [`Bytes]
    - [{"$link": "<cid>"}] decodes to [`Link]
    - [{"$type": "blob", ...}] decodes to [`Blob]

    Use with {!Jsont_bytesrw} for string encoding/decoding:
    {[
      Jsont_bytesrw.decode_string Lex.jsont json_string
        Jsont_bytesrw.encode_string Lex.jsont value
    ]} *)

(** {1 Records} *)

type record = (string * value) list
(** A repository record is a Lexicon map. Must contain a [$type] field. *)

val record_type : record -> string option
(** [record_type r] returns the [$type] field value if present. *)

val encode_record_cbor : record -> string
(** [encode_record_cbor r] encodes a record as DAG-CBOR bytes. *)

val decode_record_cbor : string -> record
(** [decode_record_cbor s] decodes DAG-CBOR bytes to a record.
    @raise Eio.Io if not a valid record (must be a map). *)

val record_to_block : record -> Cid.t * string
(** [record_to_block r] encodes record as DAG-CBOR and returns (CID, bytes). *)

(** {1 Blob Detection} *)

val is_blob_map : (string * Dagcbor.value) list -> bool
(** [is_blob_map entries] returns [true] if the map represents a blob ref. *)

(** {1 Utilities} *)

val find_blobs : value -> Blob_ref.t list
(** [find_blobs v] recursively finds all blob references in a value. *)

val pp : value Fmt.t
(** Pretty-print a Lexicon value. *)
