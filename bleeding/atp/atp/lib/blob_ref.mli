(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** References to binary blobs in AT Protocol.

    Blob references are used to link to binary data (images, videos, etc.)
    stored separately from records. They contain the CID, MIME type, and size.
*)

(** {1 Types} *)

type t = { cid : Cid.t; mime_type : string; size : int64 }
(** A blob reference. *)

(** {1 Errors} *)

type error =
  [ `Blob_ref_missing_field of string
  | `Blob_ref_invalid_type of string
  | `Blob_ref_invalid_format ]
(** Blob reference decoding errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err +=
  | E of error  (** Eio exception wrapper for blob reference errors. *)

(** {1 Conversion} *)

val to_dagcbor : t -> Dagcbor.value
(** [to_dagcbor t] encodes blob reference as IPLD Map:
    {v {$type: "blob", ref: <cid>, mimeType: ..., size: ...} v} *)

val of_dagcbor : Dagcbor.value -> t
(** [of_dagcbor v] decodes blob reference from IPLD.

    Handles both typed format (with [$type: "blob"]) and legacy format (with
    [cid] string field).

    @raise Eio.Io on invalid format. *)

(** {1 Utilities} *)

val equal : t -> t -> bool
(** Equality on blob references. *)

val compare : t -> t -> int
(** Total ordering on blob references. *)

val pp : t Fmt.t
(** Pretty-print a blob reference. *)

(** {1 JSON Encoding} *)

val jsont : t Jsont.t
(** Jsont codec for blob references.

    Encodes as:
{v
{"$type": "blob", "ref": {"$link": "..."}, "mimeType": "...", "size": ...}
v} *)
