(** Range request parsing and Content-Range response writing per RFC 7233.

    Supports parsing byte range requests and generating partial content responses.
    Uses unboxed types for zero-allocation parsing where possible.

    {2 Usage}

    {[
      (* Parse Range header *)
      let #(status, range) = Range.parse buf range_header_span in
      match status with
      | Range.Valid ->
        let #(result, resolved) = Range.evaluate range ~resource_length:file_size in
        (match result with
        | Range.Full_content -> (* serve full file *)
        | Range.Single_range -> (* use resolved for 206 response *)
        | Range.Multiple_ranges -> (* multipart/byteranges *)
        | Range.Not_satisfiable -> (* 416 error *))
      | Range.Invalid -> (* serve full content *)
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc7233> RFC 7233 *)

(** {1 Types} *)

(** A single byte range specification - unboxed.
    Use query functions ([is_range], [is_suffix], [is_open]) to inspect the kind. *)
type byte_range = private
  #{ kind : int
   ; start : int64#
   ; end_ : int64#
   }

(** Maximum number of ranges that can be parsed. *)
val max_ranges : int16#

(** Empty byte_range for array initialization. *)
val empty : byte_range

(** {2 Range Queries} *)

(** Returns true if this is a standard range with explicit start and end.
    Example: [bytes=0-499] - access via [r.#start] and [r.#end_] *)
val is_range : byte_range -> bool

(** Returns true if this is a suffix range (last N bytes).
    Example: [bytes=-500] - suffix length stored in [r.#start] *)
val is_suffix : byte_range -> bool

(** Returns true if this is an open-ended range (start to EOF).
    Example: [bytes=9500-] - access start via [r.#start] *)
val is_open : byte_range -> bool

(** Parse status. *)
type parse_status =
  | Valid
  | Invalid

(** A resolved byte range with concrete start/end positions - unboxed. *)
type resolved =
  #{ start : int64#    (** First byte position (0-indexed) *)
   ; end_ : int64#     (** Last byte position (inclusive) *)
   ; length : int64#   (** Number of bytes: end_ - start + 1 *)
   }

(** Range evaluation result. *)
type eval_result =
  | Full_content       (** Serve full content *)
  | Single_range       (** Single range - use first resolved *)
  | Multiple_ranges    (** Multiple ranges - use resolved array *)
  | Not_satisfiable    (** 416 error *)

(** {1 Parsing} *)

(** Parse Range header value into array of byte_ranges.
    Returns (status, count) where count is number of ranges parsed.
    Only supports "bytes" unit.

    Examples:
    - ["bytes=0-499"] -> single range, first 500 bytes
    - ["bytes=500-999"] -> single range, bytes 500-999
    - ["bytes=-500"] -> suffix range, last 500 bytes
    - ["bytes=9500-"] -> open range, byte 9500 to end
    - ["bytes=0-0,-1"] -> multiple ranges *)
val parse
  :  local_ bytes
  -> Span.t
  -> byte_range array
  -> #(parse_status * int16#)

(** Parse Range header from a string. Creates a local buffer internally.
    More ergonomic when you have the header value as a string.

    {[
      let #(status, count) = Range.parse_string "bytes=0-499" ranges in
      ...
    ]} *)
val parse_string : string -> byte_range array -> #(parse_status * int16#)

(** {1 Range Resolution} *)

(** Resolve ranges and evaluate result.
    Takes parsed ranges (count from parse), resource length, and output array.
    Returns (result, resolved_count).
    Resolved ranges are written to the output array. *)
val evaluate
  :  byte_range array
  -> count:int16#
  -> resource_length:int64#
  -> resolved array
  -> #(eval_result * int16#)

(** Resolve a single byte_range against resource length.
    Returns (valid, resolved) where valid indicates if range is satisfiable. *)
val resolve_range
  :  byte_range
  -> resource_length:int64#
  -> #(bool * resolved)

(** {1 Response Writing} *)

(** Write [Accept-Ranges: bytes\r\n] header. Returns new offset. *)
val write_accept_ranges : bytes -> off:int16# -> int16#

(** Write [Accept-Ranges: none\r\n] header. Returns new offset. *)
val write_accept_ranges_none : bytes -> off:int16# -> int16#

(** Write [Content-Range: bytes start-end/total\r\n] header.
    Use for 206 Partial Content responses.
    Returns new offset. *)
val write_content_range
  :  bytes
  -> off:int16#
  -> start:int64#
  -> end_:int64#
  -> total:int64#
  -> int16#

(** Write Content-Range header from resolved range.
    Returns new offset. *)
val write_content_range_resolved
  :  bytes
  -> off:int16#
  -> resolved
  -> total:int64#
  -> int16#

(** Write [Content-Range: bytes * /total\r\n] header.
    Use for 416 Range Not Satisfiable responses.
    Returns new offset. *)
val write_content_range_unsatisfiable
  :  bytes
  -> off:int16#
  -> total:int64#
  -> int16#

(** {1 Multipart Helpers} *)

(** Write multipart boundary line: [--boundary\r\n].
    Returns new offset. *)
val write_multipart_boundary : bytes -> off:int16# -> boundary:string -> int16#

(** Write final multipart boundary: [--boundary--\r\n].
    Returns new offset. *)
val write_multipart_final : bytes -> off:int16# -> boundary:string -> int16#

(** Generate a random boundary string suitable for multipart responses. *)
val generate_boundary : unit -> string

(** Empty resolved range constant. *)
val empty_resolved : resolved
