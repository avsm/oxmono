(** Httpz - Zero-allocation HTTP/1.1 protocol library for OxCaml.

    This library parses and generates HTTP/1.1 messages using stack allocation
    and unboxed types. The caller owns all buffers - httpz never allocates.

    {2 Architecture}

    The library is split into three packages:
    - [httpz] (this package): Core protocol types, parsing, and response writing
    - [httpz.server]: Server-side routing with zero-allocation dispatch
    - [httpz.eio]: Eio-based connection handling

    {2 Security}

    The parser implements {{:https://datatracker.ietf.org/doc/html/rfc7230}RFC 7230}
    security requirements:
    - Content-Length overflow protection with configurable limits
    - Bare CR detection (HTTP request smuggling prevention per Section 3.5)
    - Ambiguous framing detection (both Content-Length and Transfer-Encoding)
    - Host header requirement for HTTP/1.1

    {2 Basic Usage}

    {[
      (* 1. Allocate a buffer - httpz never allocates *)
      let buf = Bytes.create Httpz.buffer_size in

      (* 2. Read data into the buffer from your I/O layer *)
      let len = read_from_socket buf in

      (* 3. Parse the request *)
      let #(status, req, headers) =
        Httpz.parse buf ~len:(Buf_read.i16 len) ~limits:Httpz.default_limits
      in

      (* 4. Handle the result *)
      match status with
      | Buf_read.Complete ->
        (* Access cached content headers from request struct *)
        let content_len = req.#content_length in  (* int64# or -1L if absent *)
        let is_chunked = req.#is_chunked in       (* Transfer-Encoding: chunked *)
        let keep_alive = req.#keep_alive in       (* Connection semantics *)

        (* Look up other headers by name *)
        (match Header.find headers Header_name.Accept with
         | Some hdr -> use_span buf hdr.Header.value
         | None -> ());

        (* For custom headers, use name_span with Header_name.Other *)
        List.iter (fun (hdr : Header.t) ->
          if hdr.name = Header_name.Other
             && Span.equal_caseless buf hdr.name_span "x-request-id"
          then use_span buf hdr.value
        ) headers

      | Buf_read.Partial -> (* Need more data *)
        read_more_and_retry ()

      | Buf_read.Content_length_overflow
      | Buf_read.Headers_too_large ->
        send_413_payload_too_large ()

      | Buf_read.Bare_cr_detected
      | Buf_read.Ambiguous_framing ->
        send_400_bad_request ()  (* Security violation *)

      | Buf_read.Missing_host_header ->
        send_400_bad_request ()

      | _ ->
        send_400_bad_request ()
    ]}

    {2 Content Headers}

    The following headers are parsed and cached in {!Req.t} during parsing,
    and are excluded from the returned header list:
    - [Content-Length]: Available as [req.#content_length] ([-1L] if absent)
    - [Transfer-Encoding]: Available as [req.#is_chunked]
    - [Connection]: Available as [req.#keep_alive]
    - [Expect]: Available as [req.#expect_continue]

    This design eliminates header lookups for the most common operations. *)

(** {1 Core Modules} *)

(** Buffer reading primitives and parse status. *)
module Buf_read = Buf_read

(** Buffer writing primitives for response generation. *)
module Buf_write = Buf_write

(** Unboxed spans referencing buffer regions. *)
module Span = Span

(** HTTP request methods. *)
module Method = Method

(** HTTP protocol versions. *)
module Version = Version

(** Known HTTP header name enumeration. *)
module Header_name = Header_name

(** HTTP header representation. *)
module Header = Header

(** Parsed HTTP request. *)
module Req = Req

(** Request target (path and query) parsing. *)
module Target = Target

(** {1 Response Modules} *)

(** HTTP response status codes and header writing. *)
module Res = Res

(** Chunked transfer encoding parsing and writing. *)
module Chunk = Chunk

(** {1 Conditional Request Modules} *)

(** ETag parsing and comparison per
    {{:https://datatracker.ietf.org/doc/html/rfc7232}RFC 7232}. *)
module Etag = Etag

(** HTTP-date parsing and formatting per
    {{:https://datatracker.ietf.org/doc/html/rfc7231#section-7.1.1.1}RFC 7231 Section 7.1.1.1}. *)
module Date = Date

(** Range request parsing per
    {{:https://datatracker.ietf.org/doc/html/rfc7233}RFC 7233}. *)
module Range = Range

(** {1 Constants} *)

val buffer_size : int
(** Required buffer size: 32KB.

    Callers must allocate buffers of at least this size. This library never
    allocates buffers - all buffer management is the caller's responsibility.

    {[
      let buf = Bytes.create Httpz.buffer_size in
      (* Use buf for parsing... *)
    ]} *)

val max_headers : int16#
(** Maximum number of headers per request (100). *)

val default_limits : Buf_read.limits
(** Default security limits.

    - [max_content_length]: 100MB
    - [max_header_size]: 16KB
    - [max_header_count]: 100
    - [max_chunk_size]: 16MB *)

(** {1 Type Aliases}

    Convenient aliases for the most commonly used types. *)

type buffer = bytes
(** Buffer type. Callers allocate with [Bytes.create Httpz.buffer_size]. *)

type span = Span.t
type method_ = Method.t
type version = Version.t
type header_name = Header_name.t
type header = Header.t
type status = Buf_read.status
type limits = Buf_read.limits
type req = Req.t
type chunk_status = Chunk.status
type trailer_status = Chunk.trailer_status
type chunk = Chunk.t
type res_status = Res.status

(** {1 Parsing} *)

val parse : buffer -> len:int16# -> limits:limits -> #(Buf_read.status * Req.t * Header.t list) @ local
(** [parse buf ~len ~limits] parses an HTTP/1.1 request from [buf].

    Returns an unboxed tuple [(status, req, headers)] where:
    - [status]: Parse result (see {!Buf_read.status})
    - [req]: Parsed request with cached content headers
    - [headers]: List of remaining headers (excludes Content-Length, etc.)

    The returned [req] and [headers] reference spans within [buf] - do not
    modify [buf] while using the parsed values.

    {b Security checks performed:}
    - Content-Length within [limits.max_content_length]
    - No bare CR in header values (smuggling prevention)
    - Rejects ambiguous framing (both Content-Length and Transfer-Encoding)
    - Requires Host header for HTTP/1.1
    - Rejects unsupported Transfer-Encoding values *)

(** {1 Low-Level Parsing} *)

module Parser = Parser
(** Stack-allocated parser combinators for custom parsing needs. *)

(** {1 Error Handling} *)

module Err = Err
(** Exception-based error handling for parser combinators. *)
