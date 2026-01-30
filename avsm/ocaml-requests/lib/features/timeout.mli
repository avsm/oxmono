(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Timeout configuration

    This module provides timeout configuration for HTTP requests.
    Supports connection, read, total request, and 100-continue timeouts.

    Per Recommendation #7: HTTP 100-Continue support requires a short
    timeout (default 1s) to wait for the continue response before
    sending the request body. *)

(** Log source for timeout operations *)
val src : Logs.Src.t

type t
(** Timeout configuration *)

val none : t
(** No timeouts *)

val create :
  ?connect:float ->
  ?read:float ->
  ?total:float ->
  ?expect_100_continue:float ->
  unit -> t
(** Create timeout configuration with optional timeouts in seconds.
    @param connect TCP connection timeout
    @param read Read timeout per operation
    @param total Total request timeout (includes redirects, retries)
    @param expect_100_continue Timeout for 100-continue response (default: 1s) *)

val default : t
(** Sensible defaults: 10s connect, 30s read, no total limit, 1s expect *)

val connect : t -> float option
(** Get connection timeout *)

val read : t -> float option
(** Get read timeout *)

val total : t -> float option
(** Get total request timeout *)

val expect_100_continue : t -> float option
(** Get timeout for waiting for HTTP 100 Continue response.
    Per RFC 9110 Section 10.1.1, clients should wait a reasonable time
    for the 100 Continue response before sending the request body.
    If the timeout expires, the body is sent anyway. *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for timeout configuration *)