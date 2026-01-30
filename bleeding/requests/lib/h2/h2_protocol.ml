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

    For plain HTTP connections, only HTTP/1.1 is used (h2c not supported).

    See {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3}RFC 9113 Section 3}
    for protocol identification requirements. *)

let src = Logs.Src.create "h2.protocol" ~doc:"HTTP Protocol Abstraction"
module Log = (val Logs.src_log src : Logs.LOG)

(* ============================================================
   Protocol Selection Mode
   ============================================================ *)

(** Protocol selection mode.

    Controls how the HTTP protocol version is selected for connections. *)
type mode =
  | Auto
      (** Use ALPN negotiation for HTTPS, prefer HTTP/2 if available.
          Falls back to HTTP/1.1 if peer doesn't support HTTP/2. *)
  | Http1_only
      (** Force HTTP/1.1 only, no ALPN negotiation for HTTP/2.
          Use when connecting to servers known to have HTTP/2 issues. *)
  | Http2_only
      (** Require HTTP/2, fail if not available.
          Use when HTTP/2 features (multiplexing, etc) are required. *)

let pp_mode fmt = function
  | Auto -> Format.fprintf fmt "Auto"
  | Http1_only -> Format.fprintf fmt "Http1_only"
  | Http2_only -> Format.fprintf fmt "Http2_only"

let mode_to_string = function
  | Auto -> "auto"
  | Http1_only -> "http1-only"
  | Http2_only -> "http2-only"

(* ============================================================
   ALPN Protocol List Generation
   ============================================================ *)

(** Get ALPN protocols for TLS configuration based on mode.

    @param mode Protocol selection mode
    @return List of ALPN protocol identifiers in preference order *)
let alpn_protocols mode =
  match mode with
  | Auto ->
      (* Prefer HTTP/2, fallback to HTTP/1.1 *)
      ["h2"; "http/1.1"]
  | Http1_only ->
      (* Only HTTP/1.1 *)
      ["http/1.1"]
  | Http2_only ->
      (* Only HTTP/2 *)
      ["h2"]

(* ============================================================
   Negotiated Protocol
   ============================================================ *)

(** The negotiated HTTP protocol for a connection. *)
type negotiated =
  | Http1_1
      (** HTTP/1.1 protocol *)
  | Http2
      (** HTTP/2 protocol *)

let pp_negotiated fmt = function
  | Http1_1 -> Format.fprintf fmt "HTTP/1.1"
  | Http2 -> Format.fprintf fmt "HTTP/2"

let negotiated_to_string = function
  | Http1_1 -> "HTTP/1.1"
  | Http2 -> "HTTP/2"

(** Parse ALPN result string to negotiated protocol. *)
let negotiated_of_alpn = function
  | "h2" -> Some Http2
  | "http/1.1" -> Some Http1_1
  | _ -> None

(** Get the default protocol when ALPN is not available.

    For plain HTTP connections (no TLS), HTTP/1.1 is always used.
    For TLS connections without ALPN, HTTP/1.1 is the safe default. *)
let default_protocol () = Http1_1

(* ============================================================
   TLS Configuration with ALPN
   ============================================================ *)

(** Create TLS configuration with ALPN protocols.

    @param mode Protocol selection mode
    @param verify_tls Whether to verify server certificate
    @param host Hostname for SNI
    @return TLS client configuration with ALPN protocols configured *)
let create_tls_config ~mode ~verify_tls ~host:_ () =
  let alpn = alpn_protocols mode in
  Log.debug (fun m -> m "Creating TLS config with ALPN: %s"
    (String.concat ", " alpn));

  (* Build authenticator *)
  let authenticator =
    if verify_tls then
      match Ca_certs.authenticator () with
      | Ok auth -> auth
      | Error (`Msg msg) ->
          Log.err (fun m -> m "Failed to load CA certificates: %s" msg);
          failwith ("CA certificates error: " ^ msg)
    else
      (* No verification *)
      fun ?ip:_ ~host:_ _ -> Ok None
  in

  (* Create config with ALPN *)
  match Tls.Config.client
    ~authenticator
    ~alpn_protocols:alpn
    ~version:(`TLS_1_2, `TLS_1_3)
    ()
  with
  | Ok cfg -> cfg
  | Error (`Msg msg) ->
      Log.err (fun m -> m "Failed to create TLS config: %s" msg);
      failwith ("TLS config error: " ^ msg)

(** Get negotiated ALPN protocol from TLS epoch data.

    After TLS handshake, the epoch data contains the negotiated ALPN.

    @param epoch TLS epoch data from handshake
    @return Negotiated protocol, or None if ALPN not used *)
let get_alpn_from_epoch epoch =
  epoch.Tls.Core.alpn_protocol

(* ============================================================
   Protocol Detection
   ============================================================ *)

(** Detect the negotiated protocol after TLS handshake.

    @param mode Protocol selection mode
    @param alpn_result ALPN result from TLS epoch (if any)
    @return Negotiated protocol

    @raise Failure if Http2_only mode but HTTP/2 not negotiated *)
let detect_protocol ~mode alpn_result =
  match alpn_result with
  | Some alpn ->
      Log.debug (fun m -> m "ALPN negotiated: %s" alpn);
      (match negotiated_of_alpn alpn with
       | Some proto -> proto
       | None ->
           Log.warn (fun m -> m "Unknown ALPN result: %s, defaulting to HTTP/1.1" alpn);
           Http1_1)
  | None ->
      Log.debug (fun m -> m "No ALPN result, using default protocol");
      match mode with
      | Http2_only ->
          failwith "HTTP/2 required but ALPN negotiation failed or not supported"
      | Auto | Http1_only ->
          default_protocol ()

(* ============================================================
   Request/Response Types
   ============================================================ *)

(** HTTP request representation.

    A protocol-agnostic request that can be sent over HTTP/1.1 or HTTP/2. *)
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

(** Create a request.

    @param meth HTTP method
    @param uri Request URI (can be absolute or path-only)
    @param headers Request headers
    @param body Optional request body
    @return Request value *)
let make_request ~meth ~uri ?(headers = []) ?body () =
  { meth; uri; headers; body }

(** Create request from string components.

    @param meth HTTP method
    @param scheme URL scheme (http or https)
    @param host Hostname
    @param port Optional port number
    @param path Request path (defaults to "/")
    @param query Optional query parameters
    @param headers Request headers
    @param body Optional request body *)
let make_request_from_strings ~meth ~scheme ~host ?port ?(path="/") ?(query=[]) ?(headers = []) ?body () =
  let uri = Uri.make ~scheme ~host ?port ~path ~query () in
  { meth; uri; headers; body }

(** HTTP response representation.

    A protocol-agnostic response that can come from HTTP/1.1 or HTTP/2. *)
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

(** Pretty print response. *)
let pp_response fmt r =
  Format.fprintf fmt "Response{status=%d; protocol=%a; body_len=%d}"
    r.status pp_negotiated r.protocol (String.length r.body)

(* ============================================================
   Connection State
   ============================================================ *)

(** Protocol-specific connection state.

    This is an internal type that tracks the state needed for
    making requests over a specific protocol. *)
type connection_state =
  | Http1_state
      (** HTTP/1.1 connection - stateless, each request is independent *)
  | Http2_state of H2_connection.t
      (** HTTP/2 connection - maintains multiplexed state *)

(** A protocol-aware connection.

    Wraps the underlying transport with protocol-specific state. *)
type connection = {
  protocol : negotiated;
      (** Negotiated protocol *)
  state : connection_state;
      (** Protocol-specific state *)
}

(** Create a connection with detected protocol.

    @param protocol Negotiated protocol
    @return Connection value *)
let create_connection ~protocol =
  let state = match protocol with
    | Http1_1 -> Http1_state
    | Http2 ->
        let conn = H2_connection.create H2_connection.Client in
        Http2_state conn
  in
  { protocol; state }

(** Get the protocol for a connection. *)
let connection_protocol conn = conn.protocol

(** Get the HTTP/2 connection state, if using HTTP/2. *)
let get_h2_connection conn =
  match conn.state with
  | Http2_state c -> Some c
  | Http1_state -> None

(** Check if connection is using HTTP/2. *)
let is_http2 conn = conn.protocol = Http2

(** Check if connection is using HTTP/1.1. *)
let is_http1 conn = conn.protocol = Http1_1

(* ============================================================
   Request Conversion for HTTP/2
   ============================================================ *)

(** Convert request to HTTP/2 pseudo-headers.

    HTTP/2 uses pseudo-headers (prefixed with ':') instead of the
    request line. See RFC 9113 Section 8.3.1.

    @param request The request to convert
    @return List of HPACK headers including pseudo-headers *)
let request_to_h2_headers request =
  let uri = request.uri in
  let scheme = Uri.scheme uri |> Option.value ~default:"https" in
  let authority = match Uri.host uri, Uri.port uri with
    | Some h, Some p -> Printf.sprintf "%s:%d" h p
    | Some h, None -> h
    | None, _ -> ""
  in
  let path =
    let p = Uri.path uri in
    let q = Uri.query uri in
    if q = [] then
      (if p = "" then "/" else p)
    else
      let query_str = Uri.encoded_of_query q in
      (if p = "" then "/" else p) ^ "?" ^ query_str
  in

  (* Pseudo-headers must come first *)
  let pseudo_headers = [
    { H2_hpack.name = ":method"; value = request.meth; sensitive = false };
    { H2_hpack.name = ":scheme"; value = scheme; sensitive = false };
    { H2_hpack.name = ":authority"; value = authority; sensitive = false };
    { H2_hpack.name = ":path"; value = path; sensitive = false };
  ] in

  (* Convert regular headers *)
  let regular_headers = List.map (fun (name, value) ->
    (* HTTP/2 requires lowercase header names *)
    let name_lower = String.lowercase_ascii name in
    (* Mark Authorization and Cookie as sensitive *)
    let sensitive = name_lower = "authorization" || name_lower = "cookie" in
    { H2_hpack.name = name_lower; value; sensitive }
  ) request.headers in

  pseudo_headers @ regular_headers

(** Convert HTTP/2 response headers to header list.

    Extracts the :status pseudo-header and regular headers.

    @param h2_headers HPACK headers from response
    @return (status_code, headers list) *)
let h2_headers_to_response h2_headers =
  let fold_header (status, headers) (h : H2_hpack.header) =
    if h.name = ":status" then
      (int_of_string h.value, headers)
    else if String.length h.name > 0 && h.name.[0] = ':' then
      (status, headers)  (* Skip other pseudo-headers *)
    else
      (status, (h.name, h.value) :: headers)
  in
  let status, headers = List.fold_left fold_header (200, []) h2_headers in
  (status, List.rev headers)

(* ============================================================
   Pretty Printing
   ============================================================ *)

let pp_request fmt r =
  Format.fprintf fmt "Request{%s %a; headers=%d; body=%s}"
    r.meth Uri.pp r.uri
    (List.length r.headers)
    (match r.body with Some b -> string_of_int (String.length b) | None -> "none")
