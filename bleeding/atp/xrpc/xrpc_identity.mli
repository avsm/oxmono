(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Resolving handles and DIDs to the server that holds their records.

    A handle names an account and a DID identifies it. The DID document of an
    account lists the personal data server that stores its records, which is
    where to read them from. These functions use HTTPS only, so a handle that is
    published only as a DNS TXT record does not resolve.

    Each function that makes a request returns [Error] for an answer it cannot
    use, such as a 404 or a body that is not a DID. A network failure raises
    [Eio.Io] as {!Fetch} does.

    Each request goes to a host that the caller named, with the redirect policy
    of the client given. A server that resolves handles supplied by its users
    should give a client that limits where it may connect. The DID that a handle
    names is not checked against the handle that its document lists. *)

val pds_of_document : string -> string option
(** [pds_of_document json] is the personal data server in the DID document
    [json], or [None] if it lists none or is not valid JSON. *)

val document_url : string -> (string, string) result
(** [document_url did] is where the DID document of [did] is published. Only
    [did:plc] and [did:web] identifiers are supported. The host of a [did:web]
    may carry a port written [%3A]. A host with anything else encoded, or
    anything that is not a domain name, is an error. *)

val did_of_handle : _ Fetch.t -> string -> (string, string) result
(** [did_of_handle http handle] is the DID that [handle] publishes at
    [https://<handle>/.well-known/atproto-did]. *)

val pds_of_did : _ Fetch.t -> string -> (string, string) result
(** [pds_of_did http did] is the personal data server in the DID document of
    [did]. *)

val pds_of_handle : _ Fetch.t -> string -> (string, string) result
(** [pds_of_handle http handle] is [pds_of_did] of [did_of_handle handle]. *)
