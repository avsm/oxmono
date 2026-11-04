(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Resolving handles and DIDs to the server that holds their records.

    A handle names an account and a DID identifies it. The DID document of an
    account lists the personal data server that stores its records, which is
    where to read them from. A handle's DNS TXT record is read over HTTPS from
    Cloudflare's DNS-over-HTTPS resolver for now, so these functions make only
    HTTPS requests.

    Each function that makes a request returns [Error] for an answer it cannot
    use, such as a 404 or a body that is not a DID. A network failure raises
    [Eio.Io] as {!Fetch} does.

    Each request goes to a host that the caller named, with the redirect policy
    of the client given. A server that resolves handles supplied by its users
    should give a client that limits where it may connect. Only
    {!verified_handle} checks a handle against the DID it names. *)

val pds_of_document : string -> string option
(** [pds_of_document json] is the personal data server in the DID document
    [json], or [None] if it lists none or is not valid JSON. *)

val service_of_document : id:string -> string -> string option
(** [service_of_document ~id json] is the endpoint of the service named [id] in
    the DID document [json], such as ["#tangled_knot"]. A service whose id is
    the document's DID followed by [id] also matches. *)

val document_url : ?plc:string -> string -> (string, string) result
(** [document_url ?plc did] is where the DID document of [did] is published.
    [plc] is the PLC directory origin, [https://plc.directory] by default. Only
    [did:plc] and [did:web] identifiers are supported. The host of a [did:web]
    may carry a port written [%3A]. A host with anything else encoded, or
    anything that is not a domain name, is an error. *)

val did_of_handle : _ Fetch.t -> string -> (string, string) result
(** [did_of_handle http handle] is the DID that [handle] publishes in the DNS
    TXT record [_atproto.<handle>], as [did=<DID>], or else at
    [https://<handle>/.well-known/atproto-did]. The TXT record is read from
    [https://cloudflare-dns.com/dns-query], and a handle with more than one
    [did=] record has none there. *)

val document : ?plc:string -> _ Fetch.t -> string -> (string, string) result
(** [document ?plc http did] is the DID document of [did] as JSON text. *)

val pds_of_did : ?plc:string -> _ Fetch.t -> string -> (string, string) result
(** [pds_of_did ?plc http did] is the personal data server in the DID document
    of [did]. *)

val service_of_did :
  ?plc:string -> _ Fetch.t -> string -> id:string -> (string, string) result
(** [service_of_did ?plc http did ~id] is [service_of_document ~id] of the DID
    document of [did]. *)

val handle_of_document : did:string -> string -> string option
(** [handle_of_document ~did json] is the first handle that the DID document
    [json] claims in [alsoKnownAs], as [at://<handle>], if the document's [id]
    is [did]. The claim is not checked. *)

val verified_handle : ?plc:string -> _ Fetch.t -> string -> string option
(** [verified_handle ?plc http did] is the handle of [did], lower case, if its
    DID document claims one and {!did_of_handle} of that handle is [did]. It is
    [None] for any answer that cannot be used. A network failure raises as
    {!Fetch} does. *)

val pds_of_handle :
  ?plc:string -> _ Fetch.t -> string -> (string, string) result
(** [pds_of_handle ?plc http handle] is [pds_of_did] of [did_of_handle handle].
*)
