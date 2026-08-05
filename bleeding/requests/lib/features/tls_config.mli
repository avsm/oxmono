(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TLS configuration utilities

    This module provides shared TLS configuration creation to ensure consistent
    behavior across session-based and one-shot request modes.

    Supports ALPN (Application-Layer Protocol Negotiation) for HTTP/2 upgrade
    per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.3}RFC 9113 Section 3.3}. *)

val src : Logs.src
(** Logs source for this module *)

(** {1 TLS Version Types} *)

(** Minimum TLS version configuration.
    Per Recommendation #6: Allow enforcing minimum TLS version. *)
type tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)

val tls_version_to_tls : tls_version -> Tls.Core.tls_version
(** Convert our TLS version type to the underlying library's type *)

(** {1 ALPN Protocol Negotiation}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.3}RFC 9113 Section 3.3},
    HTTP/2 connections over TLS use ALPN to negotiate the protocol. *)

(** ALPN protocol identifiers. *)
val alpn_h2 : string
(** ALPN identifier for HTTP/2: "h2" *)

val alpn_http11 : string
(** ALPN identifier for HTTP/1.1: "http/1.1" *)

(** HTTP protocol mode for ALPN negotiation. *)
type protocol_mode =
  | Auto        (** Prefer HTTP/2 if available, fall back to HTTP/1.1 *)
  | Http1_only  (** Use HTTP/1.1 only *)
  | Http2_only  (** Require HTTP/2 *)

val alpn_protocols : protocol_mode -> string list
(** [alpn_protocols mode] returns the ALPN protocol list for the given mode.
    - Auto: ["h2"; "http/1.1"]
    - Http1_only: ["http/1.1"]
    - Http2_only: ["h2"] *)

(** {1 Configuration Creation} *)

val create_client :
  ?verify_tls:bool ->
  ?min_tls_version:tls_version ->
  ?protocol_mode:protocol_mode ->
  host:string ->
  unit ->
  Tls.Config.client
(** [create_client ~host ()] creates a TLS client configuration.

    @param verify_tls If true (default), use system CA certificates for verification
    @param min_tls_version Minimum TLS version to accept (default TLS_1_2)
    @param protocol_mode HTTP protocol mode for ALPN (default Auto)
    @param host Hostname for error messages
    @return TLS client configuration
    @raise Error.Tls_handshake_failed if configuration cannot be created *)

val create_client_opt :
  ?existing_config:Tls.Config.client ->
  verify_tls:bool ->
  min_tls_version:tls_version ->
  ?protocol_mode:protocol_mode ->
  host:string ->
  unit ->
  Tls.Config.client option
(** [create_client_opt ~verify_tls ~min_tls_version ~host ()] creates a TLS
    client configuration, or returns the existing one if provided.

    @param existing_config If provided, return this instead of creating new
    @param verify_tls If true, use system CA certificates for verification
    @param min_tls_version Minimum TLS version to accept
    @param protocol_mode HTTP protocol mode for ALPN (default Auto)
    @param host Hostname for error messages
    @return Some TLS client configuration *)

(** {1 ALPN Result Extraction}

    Helper functions for extracting negotiated protocol from TLS epoch. *)

(** Negotiated HTTP protocol from ALPN. *)
type negotiated_protocol =
  | Http1_1  (** HTTP/1.1 *)
  | Http2    (** HTTP/2 *)

val get_alpn_from_epoch : Tls.Core.epoch_data -> string option
(** [get_alpn_from_epoch epoch] extracts the negotiated ALPN protocol
    from TLS epoch data. Returns [None] if ALPN was not negotiated. *)

val negotiated_of_alpn : string -> negotiated_protocol option
(** [negotiated_of_alpn alpn] parses ALPN result string.
    - "h2" -> Some Http2
    - "http/1.1" -> Some Http1_1
    - other -> None *)

val default_protocol : negotiated_protocol
(** Default protocol (HTTP/1.1) when ALPN is not available. *)

val detect_protocol : mode:protocol_mode -> string option -> negotiated_protocol
(** [detect_protocol ~mode alpn_result] determines the protocol to use.
    @raise Failure if Http2_only mode but HTTP/2 not negotiated *)

val negotiated_to_string : negotiated_protocol -> string
(** Convert negotiated protocol to string ("HTTP/1.1" or "HTTP/2"). *)

val pp_negotiated : Format.formatter -> negotiated_protocol -> unit
(** Pretty print negotiated protocol. *)
