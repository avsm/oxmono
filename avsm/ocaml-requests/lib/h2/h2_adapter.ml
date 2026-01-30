(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP/2 Adapter for Requests Library.

    This module provides integration between the H2 implementation and
    the Requests library, handling automatic response decompression.

    NOTE: Connection state caching was removed because it was incompatible
    with how Conpool manages TCP connections - the cached H2_client state
    would become stale when Conpool provides a new TCP connection, leading
    to flow control window overflow errors. For proper HTTP/2 multiplexing
    with connection reuse, the connection management needs to be integrated
    at the Conpool level, not here. *)

let src = Logs.Src.create "requests.h2_adapter" ~doc:"HTTP/2 Adapter"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Response Type} *)

(** HTTP/2 response with Headers.t instead of string pairs. *)
type response = {
  status : int;
  headers : Headers.t;
  body : string;
}

(** {1 Body Conversion} *)

let body_to_string_opt (body : Body.t) : string option =
  if Body.Private.is_empty body then None
  else
    try Some (Body.Private.to_string body)
    with Failure _ ->
      Log.warn (fun m -> m "Complex body types not yet fully supported for HTTP/2");
      None

(** {1 Decompression} *)

let decompress_body ~auto_decompress ~headers body_str =
  if not auto_decompress then body_str
  else
    match Headers.get `Content_encoding headers with
    | Some encoding ->
        let limits = Response_limits.default in
        Http_client.decompress_body ~limits ~content_encoding:encoding body_str
    | None -> body_str

(** {1 Response Construction} *)

(** Convert H2 protocol response to adapter response with decompression. *)
let make_response ~auto_decompress (h2_resp : H2_protocol.response) =
  let headers = Headers.of_list h2_resp.H2_protocol.headers in
  let body = decompress_body ~auto_decompress ~headers h2_resp.H2_protocol.body in
  { status = h2_resp.H2_protocol.status; headers; body }

(** {1 Request Functions} *)

(** Make an HTTP/2 request.

    This function creates a fresh H2_client for each request and performs
    a new handshake. For proper HTTP/2 multiplexing with connection reuse,
    the connection management needs to be integrated at the Conpool level.

    @param sw Switch for the reader fiber
    @param flow The underlying TLS connection
    @param uri Request URI
    @param headers Request headers
    @param body Optional request body
    @param method_ HTTP method
    @param auto_decompress Whether to decompress response body
    @return Response with Headers.t, or error message *)
let request
    ~sw
    ~(flow : [> Eio.Flow.two_way_ty] Eio.Resource.t)
    ~(uri : Uri.t)
    ~(headers : Headers.t)
    ?(body : Body.t option)
    ~(method_ : Method.t)
    ~auto_decompress
    ()
  : (response, string) result =
  (* Validate HTTP/2 header constraints per RFC 9113 Section 8.2.2 and 8.3 *)
  match Headers.validate_h2_user_headers headers with
  | Error e ->
      Error (Format.asprintf "Invalid HTTP/2 request headers: %a"
        Headers.pp_h2_validation_error e)
  | Ok () ->
  let h2_headers = Headers.to_list headers in
  let h2_body = Option.bind body body_to_string_opt in
  let meth = Method.to_string method_ in
  H2_client.one_request ~sw flow ~meth ~uri ~headers:h2_headers ?body:h2_body ()
  |> Result.map (make_response ~auto_decompress)

(** Make a one-shot HTTP/2 request (same as {!request}). *)
let one_request = request

(** {1 Connection Management} *)

(** Clear all cached HTTP/2 connections.
    NOTE: This is now a no-op since caching was removed. *)
let clear_connection_cache () =
  Log.debug (fun m -> m "clear_connection_cache called (caching disabled)")

(** Get statistics about cached connections.
    NOTE: Always returns (0, []) since caching was removed. *)
let connection_cache_stats () = (0, [])
