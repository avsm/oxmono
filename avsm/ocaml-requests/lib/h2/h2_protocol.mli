(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** HTTP Protocol Abstraction Layer.

    This module provides a unified interface for making HTTP requests
    over either HTTP/1.1 or HTTP/2, with automatic protocol selection
    via TLS ALPN negotiation.

    {2 Protocol Selection}

    For HTTPS connections, ALPN (Application-Layer Protocol Negotiation)
    is used during the TLS handshake to agree on the protocol:
    - If both client and server support HTTP/2, "h2" is selected
    - Otherwise, "http/1.1" is used as fallback

    {2 Usage}

    {[
      (* Configure for automatic protocol selection *)
      let mode = H2_protocol.Auto in
      let tls_config = H2_protocol.create_tls_config ~mode ~verify_tls:true ~host () in

      (* After TLS handshake, detect the protocol *)
      let alpn_result = H2_protocol.get_alpn_from_epoch epoch in
      let protocol = H2_protocol.detect_protocol ~mode alpn_result in

      (* Create a protocol-aware connection *)
      let conn = H2_protocol.create_connection ~protocol in

      (* Make requests *)
      let request = H2_protocol.make_request
        ~meth:"GET"
        ~uri:(Huri.of_string "https://example.com/")
        ()
      in
    ]}

    See {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3}RFC 9113 Section 3}
    for protocol identification requirements. *)

(** {1 Protocol Selection} *)

(** Protocol selection mode. *)
type mode =
  | Auto
      (** Use ALPN negotiation for HTTPS, prefer HTTP/2 if available. *)
  | Http1_only
      (** Force HTTP/1.1 only. *)
  | Http2_only
      (** Require HTTP/2, fail if not available. *)

val pp_mode : Format.formatter -> mode -> unit
(** Pretty print mode. *)

val mode_to_string : mode -> string
(** Convert mode to string. *)

(** {1 ALPN Configuration} *)

val alpn_protocols : mode -> string list
(** [alpn_protocols mode] returns ALPN protocol identifiers for TLS config.
    - Auto: ["h2"; "http/1.1"]
    - Http1_only: ["http/1.1"]
    - Http2_only: ["h2"] *)

val create_tls_config : mode:mode -> verify_tls:bool -> host:string -> unit ->
  Tls.Config.client
(** [create_tls_config ~mode ~verify_tls ~host ()] creates TLS configuration
    with appropriate ALPN protocols for the given mode. *)

val get_alpn_from_epoch : Tls.Core.epoch_data -> string option
(** [get_alpn_from_epoch epoch] extracts the negotiated ALPN protocol
    from TLS epoch data after handshake. *)

(** {1 Negotiated Protocol} *)

(** The negotiated HTTP protocol for a connection. *)
type negotiated =
  | Http1_1
      (** HTTP/1.1 protocol *)
  | Http2
      (** HTTP/2 protocol *)

val pp_negotiated : Format.formatter -> negotiated -> unit
(** Pretty print negotiated protocol. *)

val negotiated_to_string : negotiated -> string
(** Convert negotiated protocol to string. *)

val negotiated_of_alpn : string -> negotiated option
(** [negotiated_of_alpn alpn] parses ALPN result string.
    - "h2" -> Some Http2
    - "http/1.1" -> Some Http1_1
    - other -> None *)

val default_protocol : unit -> negotiated
(** [default_protocol ()] returns the default protocol (HTTP/1.1)
    when ALPN is not available. *)

val detect_protocol : mode:mode -> string option -> negotiated
(** [detect_protocol ~mode alpn_result] determines the protocol to use.
    @raise Failure if Http2_only mode but HTTP/2 not negotiated *)

(** {1 Request/Response Types} *)

(** HTTP request representation. *)
type request = {
  meth : string;
      (** HTTP method (GET, POST, etc) *)
  uri : Uri.t;
      (** Request URI *)
  headers : (string * string) list;
      (** Request headers (name, value) pairs *)
  body : string option;
      (** Optional request body *)
}

val make_request : meth:string -> uri:Uri.t -> ?headers:(string * string) list ->
  ?body:string -> unit -> request
(** [make_request ~meth ~uri ?headers ?body ()] creates a request. *)

val make_request_from_strings : meth:string -> scheme:string -> host:string ->
  ?port:int -> ?path:string -> ?query:(string * string list) list ->
  ?headers:(string * string) list -> ?body:string -> unit -> request
(** [make_request_from_strings ~meth ~scheme ~host ?port ?path ?query ?headers ?body ()]
    creates a request from string components. This avoids Uri module conflicts when
    calling from libraries that have their own Uri module. *)

val pp_request : Format.formatter -> request -> unit
(** Pretty print request. *)

(** HTTP response representation. *)
type response = {
  status : int;
      (** HTTP status code *)
  headers : (string * string) list;
      (** Response headers *)
  body : string;
      (** Response body *)
  protocol : negotiated;
      (** Protocol used for this response *)
}

val pp_response : Format.formatter -> response -> unit
(** Pretty print response. *)

(** {1 Connection Management} *)

(** A protocol-aware connection. *)
type connection

val create_connection : protocol:negotiated -> connection
(** [create_connection ~protocol] creates a connection for the given protocol.
    For HTTP/2, this initializes the connection state machine. *)

val connection_protocol : connection -> negotiated
(** [connection_protocol conn] returns the negotiated protocol. *)

val get_h2_connection : connection -> H2_connection.t option
(** [get_h2_connection conn] returns the HTTP/2 connection state if using HTTP/2,
    or [None] if using HTTP/1.1. Useful for accessing HTTP/2-specific features. *)

val is_http2 : connection -> bool
(** [is_http2 conn] returns true if using HTTP/2. *)

val is_http1 : connection -> bool
(** [is_http1 conn] returns true if using HTTP/1.1. *)

(** {1 HTTP/2 Conversion} *)

val request_to_h2_headers : request -> H2_hpack.header list
(** [request_to_h2_headers request] converts a request to HTTP/2 headers
    including the required pseudo-headers (:method, :scheme, :authority, :path).
    Regular headers are converted to lowercase as required by HTTP/2. *)

val h2_headers_to_response : H2_hpack.header list -> int * (string * string) list
(** [h2_headers_to_response headers] extracts status code and regular headers
    from HTTP/2 response headers. Returns (status_code, header_list). *)
