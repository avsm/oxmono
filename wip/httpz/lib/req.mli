(** This module provides parsed HTTP requests and request-line writing.

    Every span in a request borrows from the input buffer. The parser stores
    message-framing and connection information directly in {!t}; the Content-Length,
    Transfer-Encoding, Connection, and Expect fields are therefore omitted from the field
    list returned by {!Httpz.parse}. *)

(** A [t] is a parsed HTTP request. *)
type t =
  #{ meth : Method.t (** [meth] is the request method. *)
   ; target : Span.t (** [target] is the complete request-target. *)
   ; path : Span.t
   (** [path] is the path exactly as it appears in the target. In origin-form it always
       begins with a slash; in absolute-form it is whatever the URI carries, so
       [http://host] and [http://host?q=1] both give an empty span. *)
   ; query : Span.t (** [query] is the query without [?], or an empty span when absent. *)
   ; version : Version.t (** [version] is the request's HTTP version. *)
   ; body_off : int16# (** [body_off] is the first byte after the request head. *)
   ; content_length : int64#
   (** [content_length] is Content-Length, or [-1L] when absent. *)
   ; is_chunked : bool
   (** [is_chunked] is [true] when the body uses chunked transfer coding. *)
   ; keep_alive : bool
   (** [keep_alive] is [true] when HTTP connection persistence applies. *)
   ; connection_upgrade : bool
   (** [connection_upgrade] is [true] when an HTTP/1.1 request has a Connection field
       containing the [upgrade] option. It is always [false] for HTTP/1.0, which RFC 9110
       Section 7.8 gives no Upgrade. The Upgrade field itself remains in the returned
       header list. *)
   ; expect_continue : bool
   (** [expect_continue] is [true] when [100-continue] was requested. *)
   ; unsupported_expectation : bool
   (** [unsupported_expectation] is [true] when an Expect field contained an unsupported
       expectation. A server can answer 417 before reading the body. *)
   }

(** [body_in_buffer ~len request] is [true] when a non-chunked request body is complete
    between [request.body_off] and the exclusive end offset [len]. It is always [false]
    for chunked bodies. *)
val body_in_buffer : len:int16# -> t @ local -> bool @@ portable
[@@zero_alloc]

(** [body_span ~len request] is the complete non-chunked body span. It has length zero
    when Content-Length is absent or zero, and length [-1] when the body is chunked or
    incomplete. *)
val body_span : len:int16# -> t @ local -> Span.t @@ portable
[@@zero_alloc opt]

(** [body_bytes_needed ~len request] is the number of additional bytes needed for a
    Content-Length body. It is [0] for a complete or absent body, and [-1] both for
    chunked transfer coding and for a Content-Length so large that no count of remaining
    bytes fits an OCaml [int]. A negative result therefore means the body cannot be
    completed in the parse buffer, never that fewer bytes are needed. Values larger than
    the positive range of [int16#] saturate at [32767]. *)
val body_bytes_needed : len:int16# -> t @ local -> int16# @@ portable
[@@zero_alloc]

(** [write_request_line buf ~off ~meth ~target version] is the next offset after writing
    [meth SP target SP version CRLF]. [meth] may name an extension method. The strings are
    written verbatim and buffer bounds are not checked. Neither is their content: the
    caller must supply [meth] and [target] free of CR, LF, NUL, any other control byte and
    space, since a single one of those splits the request line and lets a second request
    be smuggled after it. *)
val write_request_line
  :  bytes
  -> off:int16#
  -> meth:local_ string
  -> target:local_ string
  -> Version.t
  -> int16#
  @@ portable

(** [pp formatter request] is the formatter operation that prints request metadata and
    span positions. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable

(** [pp_with_buf buf formatter request] is the formatter operation that prints the request
    line represented by [request]. *)
val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit @@ portable
