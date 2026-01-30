(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP protocol version identification and ALPN support.

    This module provides types for HTTP protocol versions and utilities for
    TLS Application-Layer Protocol Negotiation (ALPN) as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.1}RFC 9113 Section 3.1}.

    {2 ALPN Protocol Identifiers}

    Per RFC 9113 Section 3.1:
    - ["h2"] identifies HTTP/2 over TLS
    - ["http/1.1"] identifies HTTP/1.1

    {2 Example}

    {[
      (* Configure TLS with HTTP/2 preference *)
      let alpn = Http_version.alpn_protocols ~preferred:[Http_2; Http_1_1] in
      (* alpn = ["h2"; "http/1.1"] *)
    ]} *)

(** {1 Version Type} *)

type t =
  | Http_1_0  (** HTTP/1.0 *)
  | Http_1_1  (** HTTP/1.1 *)
  | Http_2    (** HTTP/2 per RFC 9113 *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string version] returns a human-readable string.
    Examples: ["HTTP/1.0"], ["HTTP/1.1"], ["HTTP/2"] *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for versions. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal v1 v2] returns true if versions are equal. *)

val compare : t -> t -> int
(** [compare v1 v2] compares versions. HTTP/2 > HTTP/1.1 > HTTP/1.0. *)

(** {1 ALPN Protocol Negotiation}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.1}RFC 9113 Section 3.1}. *)

val alpn_h2 : string
(** ["h2"] - ALPN protocol identifier for HTTP/2 over TLS.
    Serialized as the two-octet sequence: 0x68, 0x32. *)

val alpn_http_1_1 : string
(** ["http/1.1"] - ALPN protocol identifier for HTTP/1.1. *)

val alpn_of_version : t -> string option
(** [alpn_of_version version] returns the ALPN identifier for a version.
    Returns [None] for HTTP/1.0 which has no ALPN identifier. *)

val version_of_alpn : string -> t option
(** [version_of_alpn alpn] returns the version for an ALPN identifier.
    Returns [None] for unrecognized identifiers. *)

val alpn_protocols : preferred:t list -> string list
(** [alpn_protocols ~preferred] returns ALPN protocol identifiers in preference order.
    HTTP/1.0 is filtered out (no ALPN identifier).

    Example:
    {[
      alpn_protocols ~preferred:[Http_2; Http_1_1]
      (* Returns: ["h2"; "http/1.1"] *)
    ]} *)

(** {1 Version Detection} *)

val supports_multiplexing : t -> bool
(** [supports_multiplexing version] returns true if the version supports
    request multiplexing over a single connection. Only HTTP/2 supports this. *)

val supports_server_push : t -> bool
(** [supports_server_push version] returns true if the version supports
    server push. Only HTTP/2 supports this. *)

val supports_header_compression : t -> bool
(** [supports_header_compression version] returns true if the version supports
    header compression. Only HTTP/2 (HPACK) supports this. *)
