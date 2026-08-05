(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol (atp) library.

    This library implements IPLD (InterPlanetary Linked Data) with specific
    support for the AT Protocol's data encoding conventions. It provides:

    {2 Core IPLD Types}

    - {!Cid}: Content Identifiers (CIDv1) for content-addressed data
    - {!Dagcbor}: DAG-CBOR codec for IPLD values
    - {!Varint}: Variable-length integer encoding (LEB128)
    - {!Blob_ref}: AT Protocol blob references

    {2 AT Protocol Syntax Types}

    - {!Tid}: Timestamp-based identifiers
    - {!Handle}: User handle validation
    - {!Did}: Decentralized Identifier validation
    - {!Nsid}: Namespaced Identifier validation
    - {!Record_key}: Record key validation
    - {!At_uri}: AT-URI parsing and validation

    {2 Data Structures}

    - {!Block_map}: Content-addressed block storage map
    - {!Car}: Content Addressable aRchive format
    - {!Mst}: Merkle Search Tree for repositories

    {2 Content Addressing}

    IPLD uses cryptographic hashes to create self-describing, immutable
    identifiers for data. A {!Cid.t} contains:

    - Version (always 1 for CIDv1)
    - Codec identifier (what format the data is in)
    - Multihash (hash algorithm + digest)

    {2 DAG-CBOR}

    DAG-CBOR is a strict subset of CBOR that:

    - Uses deterministic encoding (canonical form)
    - Only allows string map keys
    - Encodes CID links with CBOR tag 42
    - Forbids NaN, Infinity, and indefinite-length encoding

    {2 AT Protocol Support}

    This library includes support for the AT Protocol's simplified CID format
    which omits the multibase prefix and length bytes for efficiency in
    repository storage.

    {2 Example}

    {[
      (* Create a DAG-CBOR document with a link *)
      let doc = `Map [
        ("name", `String "example");
        ("parent", `Link parent_cid);
      ] in
      let bytes = Dagcbor.encode_string doc in
      let cid = Cid.create `Dag_cbor bytes
    ]} *)

(** {1 Error Handling} *)

module Eio_error = Eio_error
(** Eio error registration helpers. *)

(** {1 Core IPLD Modules} *)

module Cid = Cid
(** Content Identifiers (CIDv1) for content-addressed data. *)

module Dagcbor = Dagcbor
(** DAG-CBOR codec for IPLD values. *)

module Varint = Varint
(** Unsigned variable-length integer encoding (LEB128). *)

module Blob_ref = Blob_ref
(** References to binary blobs in AT Protocol. *)

module Lex = Lex
(** Lexicon data types for AT Protocol records. *)

(** {1 AT Protocol Syntax Types} *)

module Tid = Tid
(** Timestamp-based identifiers for AT Protocol. *)

module Handle = Handle
(** AT Protocol handle validation. *)

module Did = Did
(** Decentralized Identifier (DID) validation. *)

module Nsid = Nsid
(** Namespaced Identifier (NSID) validation. *)

module Record_key = Record_key
(** AT Protocol record key validation. *)

module At_uri = At_uri
(** AT Protocol URI validation and parsing. *)

(** {1 Data Structures} *)

module Block_map = Block_map
(** Content-addressed block storage map. *)

module Blockstore = Blockstore
(** Content-addressed block storage with Eio capabilities. *)

module Car = Car
(** Content Addressable aRchive (CAR v1) format. *)

module Mst = Mst
(** Merkle Search Tree (MST) for AT Protocol repositories. *)

module Repo_key = Repo_key
(** AT Protocol repository key validation. *)
