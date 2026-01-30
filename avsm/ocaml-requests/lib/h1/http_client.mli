(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Low-level HTTP/1.1 client over raw TCP connections

    This module provides low-level request execution over a raw TCP/TLS flow,
    handling request serialization, response parsing, and decompression.

    For most use cases, prefer {!Requests} (session-based) or {!One} (stateless). *)

val src : Logs.Src.t
(** Log source for HTTP client operations *)

(** {1 Types} *)

type limits = Response_limits.t
(** Configuration for response size limits. *)

val default_limits : limits
(** Default response limits. *)

type expect_100_config = Expect_continue.t
(** Configuration for HTTP 100-Continue support. *)

val default_expect_100_config : expect_100_config
(** Default 100-continue configuration. *)

(** {1 Decompression} *)

val decompress_body : limits:limits -> content_encoding:string -> string -> string
(** [decompress_body ~limits ~content_encoding body] decompresses [body] based
    on the [content_encoding] value (e.g., "gzip", "deflate", "identity").

    Supports gzip, deflate (both raw and zlib-wrapped), and identity encodings.
    Unknown encodings return the body unchanged with a warning logged.

    @raise Error if decompression would exceed size or ratio limits
    (decompression bomb protection). *)

(** {1 Request Execution} *)

val make_request :
  ?limits:limits ->
  sw:Eio.Switch.t ->
  method_:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  body:Body.t ->
  _ Eio.Flow.two_way ->
  int * Headers.t * string
(** [make_request ~sw ~method_ ~uri ~headers ~body flow] executes an HTTP/1.1
    request over [flow] and returns [(status_code, response_headers, body)].

    @param limits Response size limits (default: {!default_limits})
    @param sw Eio switch for resource management
    @param method_ HTTP method
    @param uri Request URI
    @param headers Request headers
    @param body Request body
    @param flow A two-way flow (TCP or TLS connection) *)

val make_request_decompress :
  ?limits:limits ->
  sw:Eio.Switch.t ->
  method_:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  body:Body.t ->
  auto_decompress:bool ->
  _ Eio.Flow.two_way ->
  int * Headers.t * string
(** Like {!make_request} but with optional automatic decompression.

    When [auto_decompress] is [true], responses with [Content-Encoding: gzip]
    or [deflate] are automatically decompressed and the header removed. *)

(** {1 HTTP 100-Continue Protocol}

    Per RFC 9110 Section 10.1.1 (Expect) and Section 15.2.1 (100 Continue).

    The 100-continue protocol allows clients to send headers first with
    [Expect: 100-continue], wait for server confirmation, then send the body.
    This saves bandwidth when the server would reject the request based on
    headers alone. *)

val make_request_100_continue :
  ?limits:limits ->
  ?expect_100:expect_100_config ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  method_:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  body:Body.t ->
  _ Eio.Flow.two_way ->
  int * Headers.t * string
(** [make_request_100_continue ~clock ~sw ~method_ ~uri ~headers ~body flow]
    executes an HTTP/1.1 request with 100-continue support.

    If the body exceeds the configured threshold and 100-continue is enabled:
    1. Sends headers with [Expect: 100-continue]
    2. Waits for server response (100 Continue or error)
    3. Sends body only if 100 Continue received
    4. Returns error response without sending body if rejected

    @param limits Response size limits
    @param expect_100 100-continue configuration (default: {!default_expect_100_config})
    @param clock Eio clock for timeout handling *)

val make_request_100_continue_decompress :
  ?limits:limits ->
  ?expect_100:expect_100_config ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  method_:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  body:Body.t ->
  auto_decompress:bool ->
  _ Eio.Flow.two_way ->
  int * Headers.t * string
(** Like {!make_request_100_continue} but with optional automatic decompression. *)
