(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP/2 Adapter for Requests Library.

    This module provides integration between the H2 implementation and
    the Requests library, handling automatic response decompression.

    NOTE: Connection state caching was removed because it was incompatible
    with how Conpool manages TCP connections. For proper HTTP/2 multiplexing
    with connection reuse, the connection management needs to be integrated
    at the Conpool level.

    {2 Usage}

    {[
      match H2_adapter.request ~flow ~uri ~headers ~method_:`GET
              ~auto_decompress:true () with
      | Ok resp -> Printf.printf "Status: %d\n" resp.status
      | Error msg -> Printf.printf "Error: %s\n" msg
    ]} *)

(** {1 Response Type} *)

(** HTTP/2 response with {!Headers.t}. *)
type response = {
  status : int;
  headers : Headers.t;
  body : string;
}

(** {1 Body Conversion} *)

val body_to_string_opt : Body.t -> string option
(** [body_to_string_opt body] converts a request body to a string option.
    Returns [None] for empty bodies or streaming bodies that can't be converted. *)

(** {1 Response Construction} *)

val make_response : auto_decompress:bool -> H2_protocol.response -> response
(** [make_response ~auto_decompress h2_resp] converts an H2_protocol response
    to an adapter response, applying decompression if enabled. *)

(** {1 Request Functions} *)

val request :
  sw:Eio.Switch.t ->
  flow:[> Eio.Flow.two_way_ty] Eio.Resource.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  ?body:Body.t ->
  method_:Method.t ->
  auto_decompress:bool ->
  unit ->
  (response, string) result
(** [request ~sw ~flow ~uri ~headers ?body ~method_ ~auto_decompress ()]
    makes an HTTP/2 request.

    This function creates a fresh H2_client for each request and performs
    a new handshake. For proper HTTP/2 multiplexing with connection reuse,
    the connection management needs to be integrated at the Conpool level.

    @param sw Switch for the reader fiber
    @param flow The underlying TLS connection
    @param uri Request URI
    @param headers Request headers
    @param body Optional request body
    @param method_ HTTP method
    @param auto_decompress Whether to decompress gzip/deflate response bodies
    @return Response on success, Error msg on failure *)

val one_request :
  sw:Eio.Switch.t ->
  flow:[> Eio.Flow.two_way_ty] Eio.Resource.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  ?body:Body.t ->
  method_:Method.t ->
  auto_decompress:bool ->
  unit ->
  (response, string) result
(** [one_request] is an alias for {!request}. *)

(** {1 Connection Management} *)

val clear_connection_cache : unit -> unit
(** [clear_connection_cache ()] is a no-op (caching was removed). *)

val connection_cache_stats : unit -> int * string list
(** [connection_cache_stats ()] always returns [(0, [])] (caching was removed). *)
