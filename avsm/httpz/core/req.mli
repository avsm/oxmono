(** Parsed HTTP request.

    The request type is an unboxed record containing the parsed request line
    and cached content headers. Content-related headers (Content-Length,
    Transfer-Encoding, Connection, Expect) are parsed during header parsing
    and stored directly in the request for fast access.

    {2 Content Header Caching}

    Instead of searching the header list for common headers, access them
    directly from the request struct:

    {[
      let #(status, req, headers) = Httpz.parse buf ~len ~limits in
      (* Direct access - no header search needed *)
      let body_length = req.#content_length in  (* -1L if absent *)
      let is_chunked = req.#is_chunked in
      let keep_alive = req.#keep_alive in
      let expects_continue = req.#expect_continue in
    ]}

    These headers are {b excluded} from the returned [headers] list since
    they're already in the request struct.

    {2 Body Access}

    Use {!body_span} to get the request body when it's fully available in
    the buffer, or {!body_bytes_needed} to determine how much more data
    to read:

    {[
      if Req.body_in_buffer ~len req then
        let body = Req.body_span ~len req in
        process_body buf body
      else
        let needed = Req.body_bytes_needed ~len req in
        read_more_bytes needed
    ]} *)

(** {1 Types} *)

type t =
  #{ meth : Method.t
       (** HTTP method (GET, POST, etc.) *)
   ; target : Span.t
       (** Request target span (e.g., "/path?query") *)
   ; version : Version.t
       (** HTTP version (1.0 or 1.1) *)
   ; body_off : int16#
       (** Byte offset where body starts in buffer *)
   ; content_length : int64#
       (** Content-Length value, or [-1L] if header absent *)
   ; is_chunked : bool
       (** [true] if Transfer-Encoding: chunked *)
   ; keep_alive : bool
       (** [true] for persistent connection.
           Considers HTTP version default and Connection header. *)
   ; expect_continue : bool
       (** [true] if Expect: 100-continue was present.
           See {{:https://datatracker.ietf.org/doc/html/rfc7231#section-5.1.1}RFC 7231 Section 5.1.1}. *)
   }
(** Unboxed parsed HTTP request.

    All fields are unboxed for zero heap allocation. Span fields reference
    positions in the parse buffer. *)

(** {1 Body Access} *)

val body_in_buffer : len:int16# -> t @ local -> bool
(** [body_in_buffer ~len req] returns [true] if the complete body is
    available in the buffer.

    Returns [true] if:
    - There is no body (GET, HEAD, etc.)
    - Content-Length is present and [body_off + content_length <= len]

    Returns [false] if:
    - Body is chunked (use {!Chunk.parse} instead)
    - Content-Length body extends beyond [len]

    {[
      if Req.body_in_buffer ~len req then
        let body = Req.body_span ~len req in
        handle_complete_body buf body
    ]} *)

val body_span : len:int16# -> t @ local -> Span.t
(** [body_span ~len req] returns a span covering the request body.

    {b Precondition:} Only valid if {!body_in_buffer} returns [true].

    For chunked encoding, returns a span with [len = -1] to indicate
    that {!Chunk.parse} should be used instead.

    {[
      let body = Req.body_span ~len req in
      if Span.len body >= 0 then
        let body_str = Span.to_string buf body in
        process body_str
    ]} *)

val body_bytes_needed : len:int16# -> t @ local -> int16#
(** [body_bytes_needed ~len req] returns how many more bytes are needed.

    Returns:
    - [0] if body is complete
    - Positive value for bytes still needed
    - [-1] for chunked encoding (length unknown)

    {[
      let needed = Req.body_bytes_needed ~len req in
      if needed > 0 then
        let n = read_at_least buf ~pos:len ~min:(Span.to_int needed) in
        (* retry with increased len *)
    ]} *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** [pp fmt req] prints the request structure (offsets and flags). *)

val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit
(** [pp_with_buf buf fmt req] prints the request with actual values from [buf].

    Shows method, target path, version, and content headers. *)
