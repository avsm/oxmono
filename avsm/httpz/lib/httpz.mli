(** Httpz - Stack-allocated HTTP/1.1 request parser for OxCaml.

    Parses HTTP/1.1 requests from a 32KB bigarray buffer, returning results entirely on
    the caller's stack. No heap allocation during parsing, no mutable state.

    {2 Security Features}

    This parser implements RFC 7230 security checks:
    - Content-Length overflow protection with configurable limits
    - Bare CR detection (HTTP request smuggling prevention)
    - Ambiguous framing detection (both Content-Length and Transfer-Encoding)
    - Host header requirement for HTTP/1.1

    {2 Usage}

    {[
      let buf = Httpz.create_buffer () in
      let len = read_from_socket buf in
      let #(status, req, headers) = Httpz.parse buf ~len ~limits:Httpz.default_limits in
      match status with
      | Buf_read.Complete ->
        (* Content headers are cached in the request struct *)
        let content_len = req.#content_length in
        let is_chunked = req.#is_chunked in
        let keep_alive = req.#keep_alive in
        (* Other headers are in the list *)
        List.iter (fun hdr ->
          match hdr.Header.name with
          | Header.Name.Host ->
            let host = hdr.Header.value in ...
          | Header.Name.Other ->
            if Span.equal_caseless buf hdr.Header.name_span "x-custom" then ...
          | _ -> ()
        ) headers
      | Buf_read.Partial -> need_more_data ()
      | Buf_read.Headers_too_large -> send_413 ()
      | Buf_read.Content_length_overflow -> send_413 ()
      | Buf_read.Bare_cr_detected -> send_400 () (* security violation *)
      | Buf_read.Ambiguous_framing -> send_400 () (* security violation *)
      | Buf_read.Missing_host_header -> send_400 ()
      | _ -> send_400 ()
    ]} *)

(** {1 Modules} *)

module Buf_read = Buf_read
module Buf_write = Buf_write
module Span = Span
module Method = Method
module Version = Version
module Header_name = Header_name
module Header = Header
module Req = Req
module Target = Target
module Route = Route
module Chunk = Chunk
module Res = Res
module Etag = Etag
module Date = Date
module Range = Range

(** {1 Constants} *)

(** Maximum buffer size: 32KB. *)
val buffer_size : int

(** Maximum headers per request. *)
val max_headers : int16#

(** Default security limits. *)
val default_limits : Buf_read.limits

(** {1 Types} *)

type buffer = Base_bigstring.t
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

(** {1 Buffer} *)

(** Create a new 32KB buffer. *)
val create_buffer : unit -> buffer

(** {1 Parsing} *)

(** Parse HTTP/1.1 request with security limits.

    Performs the following RFC 7230 security checks:
    - Content-Length value within [limits.max_content_length]
    - No bare CR in header values (smuggling prevention)
    - Rejects requests with both Content-Length and Transfer-Encoding
    - Requires Host header for HTTP/1.1 *)
val parse : buffer -> len:int16# -> limits:limits -> #(Buf_read.status * Req.t * Header.t list) @ local

(** {1 Parser Module} *)

module Parser = Parser

(** {1 Error Handling} *)

module Err = Err
