(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP request serialization using Eio.Buf_write

    This module provides efficient HTTP/1.1 request serialization using Eio's
    buffered write API. It avoids intermediate string allocations by writing
    directly to the output buffer.

    Example:
    {[
      Http_write.with_flow flow (fun w ->
        Http_write.request w ~sw ~method_:`GET ~uri
          ~headers:Headers.empty ~body:Body.empty
      )
    ]} *)

(** {1 Low-level Writers} *)

val crlf : Eio.Buf_write.t -> unit
(** [crlf w] writes a CRLF line terminator ("\r\n") to [w]. *)

val request_line : Eio.Buf_write.t -> method_:string -> uri:Uri.t -> unit
(** [request_line w ~method_ ~uri] writes an HTTP request line.
    For example: "GET /path?query HTTP/1.1\r\n" *)

val header : Eio.Buf_write.t -> name:string -> value:string -> unit
(** [header w ~name ~value] writes a single header line.
    For example: "Content-Type: application/json\r\n" *)

val headers : Eio.Buf_write.t -> Headers.t -> unit
(** [headers w hdrs] writes all headers from [hdrs], followed by
    a blank line (CRLF) to terminate the headers section. *)

(** {1 Request Headers} *)

val request_headers : Eio.Buf_write.t -> method_:string -> uri:Uri.t ->
                      headers:Headers.t -> content_length:int64 option -> unit
(** [request_headers w ~method_ ~uri ~headers ~content_length] writes a complete
    HTTP request header section, including:
    - Request line (method, path, HTTP/1.1)
    - Host header (extracted from URI if not present)
    - Connection: keep-alive (if not present)
    - Content-Length (if [content_length] provided and > 0)
    - All headers from [headers]
    - Terminating blank line *)

val request_headers_only : Eio.Buf_write.t -> method_:Method.t -> uri:Uri.t ->
                           headers:Headers.t -> content_length:int64 option -> unit
(** [request_headers_only] is like {!request_headers} but takes a [Method.t]
    instead of a string. Used for 100-continue flow where headers are sent first. *)

(** {1 Body Writing} *)

val body_string : Eio.Buf_write.t -> string -> unit
(** [body_string w s] writes string [s] as the request body.
    Does nothing if [s] is empty. *)

val body_stream : Eio.Buf_write.t -> Eio.Flow.source_ty Eio.Resource.t -> unit
(** [body_stream w source] copies data from [source] to [w] until EOF.
    Uses 8KB chunks for efficiency. The caller must ensure Content-Length
    is set correctly in headers. *)

val body_chunked : Eio.Buf_write.t -> Eio.Flow.source_ty Eio.Resource.t -> unit
(** [body_chunked w source] writes data from [source] using HTTP chunked
    transfer encoding. Each chunk is prefixed with its size in hex,
    followed by CRLF, the data, and another CRLF. Ends with "0\r\n\r\n". *)

(** {1 High-level Request Writing} *)

val request : Eio.Buf_write.t -> sw:Eio.Switch.t -> method_:Method.t ->
              uri:Uri.t -> headers:Headers.t -> body:Body.t -> unit
(** [request w ~sw ~method_ ~uri ~headers ~body] writes a complete HTTP request
    including headers and body. Automatically handles:
    - Content-Type header from body
    - Content-Length header for sized bodies
    - Transfer-Encoding: chunked for unsized streams
    - Multipart body encoding *)

(** {1 Convenience Wrappers} *)

val with_flow : ?initial_size:int -> _ Eio.Flow.sink ->
                (Eio.Buf_write.t -> 'a) -> 'a
(** [with_flow flow fn] runs [fn writer] where [writer] is a buffer that
    flushes to [flow]. Data is automatically flushed when [fn] returns.

    This is a thin wrapper around {!Eio.Buf_write.with_flow}.

    {b Note:} This function creates an internal switch and may cause issues
    with nested fibers. Consider using {!write_and_flush} instead. *)

val write_and_flush : ?initial_size:int -> _ Eio.Flow.sink ->
                      (Eio.Buf_write.t -> unit) -> unit
(** [write_and_flush flow fn] runs [fn writer] where [writer] is a buffer,
    then serializes all written data to a string and copies it to [flow].

    Unlike {!with_flow}, this does not create a nested switch and is safe
    to use in complex fiber hierarchies. The tradeoff is that the entire
    request is buffered in memory before being written. *)

(** {1 Proxy Request Writing} *)

val request_line_absolute : Eio.Buf_write.t -> method_:string -> uri:Uri.t -> unit
(** [request_line_absolute w ~method_ ~uri] writes an HTTP request line
    using absolute-URI form for proxy requests.
    Per RFC 9112 Section 3.2.2: "A client MUST send a request-line with
    absolute-form as the request-target when making a request to a proxy."
    For example: "GET http://www.example.com/path HTTP/1.1\r\n" *)

val request_via_proxy : Eio.Buf_write.t -> sw:Eio.Switch.t -> method_:Method.t ->
                        uri:Uri.t -> headers:Headers.t -> body:Body.t ->
                        proxy_auth:string option -> unit
(** [request_via_proxy w ~sw ~method_ ~uri ~headers ~body ~proxy_auth]
    writes a complete HTTP request using absolute-URI form for proxying.

    Per RFC 9112 Section 3.2.2, when sending a request to a proxy for an
    HTTP URL (not HTTPS), the client MUST use the absolute-URI form:
    {v
    GET http://www.example.com/path HTTP/1.1
    Host: www.example.com
    Proxy-Authorization: Basic ...
    v}

    @param proxy_auth Optional proxy authentication to add as Proxy-Authorization header *)
