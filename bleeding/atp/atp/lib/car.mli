(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Content Addressable aRchive (CAR v1) format.

    CAR files bundle content-addressed blocks for transport and storage. They
    are used in AT Protocol for repository export and synchronization.

    {2 Format}

    CAR v1 format:
    {v
    [varint: header_length]
    [header: DAG-CBOR {version: 1, roots: [cid, ...]}]
    [block]*

    block:
    [varint: cid_length + data_length]
    [cid: raw bytes (36)]
    [data: block bytes]
    v} *)

(** {1 Types} *)

type block = Cid.t * string
(** A block: CID and its data. *)

type header = {
  version : int;  (** Always 1 *)
  roots : Cid.t list;  (** Root CIDs *)
}
(** CAR file header. *)

type cid_format = Dagcbor.cid_format
(** CID encoding format.

    - [`Standard]: Standard DAG-CBOR/CAR format with 0x00 multibase prefix and
      multihash length byte. CIDs are 36/37 bytes.

    - [`Atproto]: AT Protocol simplified format without multibase prefix or
      length byte. CIDs are 35 bytes. Per draft-holmgren-at-repository.md
      Section 7.2. *)

(** {1 Errors} *)

type error =
  [ `Car_invalid_header of string
  | `Car_invalid_block of string
  | `Car_unsupported_version of int
  | `Car_unexpected_eof ]
(** CAR format errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err += E of error  (** Eio exception wrapper for CAR errors. *)

(** {1 Writing} *)

val write_header :
  ?cid_format:cid_format -> header -> Bytesrw.Bytes.Writer.t -> unit
(** [write_header ?cid_format h w] writes CAR header to [w].

    @param cid_format
      CID encoding format, default [`Standard]. Use [`Atproto] for AT Protocol
      repositories. *)

val write_block :
  ?cid_format:cid_format -> block -> Bytesrw.Bytes.Writer.t -> unit
(** [write_block ?cid_format (cid, data) w] writes block to [w].

    @param cid_format CID encoding format, default [`Standard]. *)

val write :
  ?cid_format:cid_format ->
  header ->
  block Seq.t ->
  Bytesrw.Bytes.Writer.t ->
  unit
(** [write ?cid_format header blocks w] writes complete CAR file to [w].

    @param cid_format CID encoding format, default [`Standard]. *)

val to_string : ?cid_format:cid_format -> header -> block Seq.t -> string
(** [to_string ?cid_format header blocks] encodes CAR to string.

    @param cid_format CID encoding format, default [`Standard]. *)

(** {1 Reading} *)

val read_header : ?cid_format:cid_format -> Bytesrw.Bytes.Reader.t -> header
(** [read_header ?cid_format r] reads and parses CAR header from [r].

    @param cid_format CID encoding format, default [`Standard].

    @raise Eio.Io on invalid header. *)

val read_block :
  ?cid_format:cid_format -> Bytesrw.Bytes.Reader.t -> block option
(** [read_block ?cid_format r] reads next block from [r].

    Returns [None] at EOF.

    @param cid_format CID encoding format, default [`Standard].

    @raise Eio.Io on invalid block. *)

val read :
  ?cid_format:cid_format -> Bytesrw.Bytes.Reader.t -> header * block Seq.t
(** [read ?cid_format r] reads complete CAR file from [r].

    Blocks are read lazily as the sequence is consumed.

    @param cid_format CID encoding format, default [`Standard].

    @raise Eio.Io on invalid format. *)

val of_string : ?cid_format:cid_format -> string -> header * block list
(** [of_string ?cid_format s] parses CAR from string (reads all blocks eagerly).

    @param cid_format CID encoding format, default [`Standard].

    @raise Eio.Io on invalid format. *)

(** {1 Utilities} *)

val import :
  ?cid_format:cid_format ->
  Blockstore.writable ->
  Bytesrw.Bytes.Reader.t ->
  header
(** [import ?cid_format store r] imports CAR from [r] into [store], returns
    header.

    @param cid_format CID encoding format, default [`Standard]. *)

val export :
  ?cid_format:cid_format ->
  root:Cid.t ->
  Blockstore.readable ->
  Cid.t Seq.t ->
  string
(** [export ?cid_format ~root store cids] exports blocks from [store] as CAR.

    @param cid_format CID encoding format, default [`Standard]. *)
