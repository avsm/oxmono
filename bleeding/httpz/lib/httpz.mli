(** This module provides bounded-buffer HTTP/1.1 parsing and serialization.

    Httpz parses request and response heads from caller-owned byte buffers and
    writes protocol elements into caller-owned output buffers. Parsed {!Span.t}
    values borrow their bytes from the input buffer, so that buffer must remain
    unchanged while the parsed values are in use.

    The request parser validates request-targets, message framing, the Host
    field, and line endings. Its rules follow
    {{:https://www.rfc-editor.org/rfc/rfc9112.html}RFC 9112}. The caller remains
    responsible for reading message bodies, applying timeouts, and choosing an
    HTTP response for each parse error.

    {2 Request parsing}

    {[
    match Httpz.parse buf ~len ~limits:Httpz.default_limits with
    | Buf_read.Complete, req, headers -> handle req headers
    | Buf_read.Partial, _, _ -> read_more ()
    | status, _, _ -> reject status
    ]}

    [req] and [headers] refer to [buf]. Content-Length, Transfer-Encoding,
    Connection, and Expect are summarized in {!Req.t}; those fields are not
    repeated in the request header list.

    The library has no shared mutable parsing state. A buffer and every value
    that refers to it should remain with the code that owns that buffer; use
    {!Span.to_string} when an independent copy is required.

    {2 Modes}

    Parsing and writing functions are declared [portable] where they have no
    hidden domain-local state. Their byte buffers remain caller-owned. A
    borrowed {!Span.t}, {!Req.t}, or {!Header.t} only describes offsets into
    its input buffer and must be used while that buffer is still available and
    unchanged; {!Span.to_string} makes an independent copy when one is needed.

    Many syntax checks accept [local_] strings or bytes, and hot paths marked
    [zero_alloc] can therefore inspect borrowed input without promoting it.
    Functions that construct strings, lists, URI values, or diagnostics return
    ordinary allocated values. Portability and allocation guarantees are stated
    on each exported signature rather than implied for the whole library. *)

module Scan = Scan
(** This module provides byte-class scanning primitives. *)

module Buf_read = Buf_read
(** This module provides buffer-reading primitives, limits, and parse statuses.
*)

module Buf_write = Buf_write
(** This module provides unchecked buffer-writing primitives. *)

module Span = Span
(** This module represents borrowed regions of a parse buffer. *)

module Method = Method
(** This module represents recognized request methods. *)

module Version = Version
(** This module represents supported HTTP versions. *)

module Header_name = Header_name
(** This module represents recognized HTTP field names. *)

module Header = Header
(** This module represents parsed HTTP fields. *)

module Upgrade = Upgrade
(** Upgrade protocol identifiers and offer lists. *)

module Req = Req
(** This module provides parsed requests and request-line writing. *)

module Target = Target
(** This module provides request-target parsing and matching. *)

module Res = Res
(** This module provides response-head parsing and HTTP head writing. *)

module Chunk = Chunk
(** This module provides chunked transfer-coding parsing and writing. *)

module Etag = Etag
(** This module provides entity-tag parsing, comparison, and writing. *)

module Date = Date
(** This module provides HTTP-date parsing and writing. *)

module Range = Range
(** This module provides byte-range parsing, resolution, and writing. *)

module Urlencoded = Urlencoded
(** This module provides the [application/x-www-form-urlencoded] codec. *)

module Multipart = Multipart
(** This module parses [multipart/form-data] bodies. *)

module Media = Media
(** This module provides typed media codecs. *)

module Json = Json
(** This module provides bounded Jsont codecs integrated with {!Media}. *)

module Sse = Sse
(** This module writes Server-Sent Event wire framing. *)

module Raw = Uriz.Raw
module Uriz = Httpz_uri
(** This module parses and normalizes RFC 3986 URI references. Its
    {!Uriz.Scanner} submodule is the allocation-free span scanner used by
    {!Target}. *)

module Uri_template = Uri_template
(** This module parses and expands RFC 6570 Level 4 URI Templates. *)

module Ip = Ip
(** This module recognizes IP address literals as a resolver does. *)

val buffer_size : int @@ portable
(** [buffer_size] is the maximum supported parse-buffer size, 32 KiB. *)

val default_limits : Buf_read.limits @@ portable
(** [default_limits] is the default set of request and chunk bounds. See
    {!Buf_read.default_limits}.

    [max_header_size] bounds the message head alone, not the bytes buffered
    with it: body bytes that arrive in the same read do not count against it.

    [max_content_length] bounds only what a caller streams for itself. A caller
    that reads bodies out of the parse buffer is bounded first by
    {!buffer_size}; the Proffer backend is one such caller, and its effective
    request-body cap is its roughly 32 KiB read window rather than the 100 MB
    default here. *)

type buffer = bytes
(** A [buffer] is a caller-owned input or output buffer. *)

type span = Span.t
(** A [span] is a borrowed byte-buffer region. *)

type method_ = Method.t
(** A [method_] is a recognized request method. *)

type version = Version.t
(** A [version] is a supported HTTP version. *)

type header_name = Header_name.t
(** A [header_name] is a recognized field name. *)

type header = Header.t
(** A [header] is a parsed HTTP field. *)

type status = Buf_read.status
(** A [status] is an HTTP parse status. *)

type limits = Buf_read.limits
(** A [limits] value sets HTTP parser resource limits. *)

type req = Req.t
(** A [req] is a parsed HTTP request. *)

type chunk_status = Chunk.status
(** A [chunk_status] is a chunk parse status. *)

type trailer_status = Chunk.trailer_status
(** A [trailer_status] is a trailer-section parse status. *)

type chunk = Chunk.t
(** A [chunk] is a parsed chunk. *)

type res_status = Res.status
(** A [res_status] is a recognized response status. *)

val[@zero_alloc] parse :
  buffer
  -> len:int16#
  -> limits:limits
  -> #(Buf_read.status * Req.t * Header.t list) @ local
  @@ portable
(** [parse buf ~len ~limits] is the result of parsing one HTTP request head from
    the first [len] bytes of [buf]. It contains the parse status, a request, and
    the non-framing fields in reverse arrival order.

    {!Buf_read.Partial} means that more bytes are required. When the status is
    neither {!Buf_read.Complete} nor {!Buf_read.Partial}, the returned request
    and field list are placeholders and must be ignored.

    A complete request contains path and query spans already validated and split
    according to {{:https://www.rfc-editor.org/rfc/rfc3986.html}RFC 3986}. The
    parser also enforces the request-target forms in
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2}RFC 9112, Section
     3.2}, rejects ambiguous message framing, and applies [limits].

    A Host field must name a non-empty authority, and an absolute-form target
    must agree with it: the host is compared ignoring ASCII case and the port
    exactly, per
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-3.2.2}RFC 9112,
     Section 3.2.2}. A mismatch is {!Buf_read.Invalid_header}. *)

module Parser = Parser
(** This module provides low-level parsers for request lines, status lines, and
    fields. *)

module Err = Err
(** This module provides exception-based guards for the low-level parsers. *)
