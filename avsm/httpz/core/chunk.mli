(** Chunked transfer encoding parser.

    Parses chunked transfer encoded request bodies per
    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-4.1}RFC 7230 Section 4.1}.

    {2 Parsing Chunks}

    Call {!parse} repeatedly until it returns {!Done}:

    {[
      let rec read_chunks buf off len acc =
        let #(status, chunk) = Chunk.parse buf ~off ~len in
        match status with
        | Complete ->
          let data = Bytes.sub_string buf
            (Span.to_int chunk.#data_off)
            (Span.to_int chunk.#data_len) in
          read_chunks buf chunk.#next_off len (data :: acc)
        | Done ->
          (* Parse trailers if any *)
          let #(trailer_status, _, trailers) =
            Chunk.parse_trailers buf ~off:chunk.#next_off ~len
              ~max_header_count:(i16 10) in
          (List.rev acc, trailers)
        | Partial ->
          (* Need more data *)
          read_more ()
        | Malformed | Chunk_too_large ->
          failwith "Invalid chunk"
    ]}

    {2 Trailer Headers}

    Chunked bodies may end with trailer headers. Use {!parse_trailers}
    after receiving the final chunk. Forbidden trailers (Content-Length,
    Transfer-Encoding, etc.) are automatically filtered per RFC 7230. *)

(** {1 Chunk Parsing} *)

type status =
  | Complete      (** Chunk data available, more chunks follow *)
  | Partial       (** Need more data to complete chunk *)
  | Done          (** Final zero-length chunk received *)
  | Malformed     (** Invalid chunk encoding *)
  | Chunk_too_large  (** Chunk size exceeds configured limit *)
(** Chunk parse status. *)

val status_to_string : status -> string
(** Convert status to string. *)

val pp_status : Stdlib.Format.formatter -> status -> unit
(** Pretty-print status. *)

(** {2 Parsed Chunk} *)

type t =
  #{ data_off : int16#  (** Offset of chunk data in buffer *)
   ; data_len : int16#  (** Length of chunk data *)
   ; next_off : int16#  (** Offset where next chunk (or trailers) starts *)
   }
(** Unboxed parsed chunk. Zero allocation. *)

(** {2 Limits} *)

val max_hex_digits : int16#
(** Maximum hex digits in chunk size (16 = 64-bit max). *)

val default_max_chunk_size : int
(** Default maximum chunk size: 16MB. *)

(** {2 Parse Functions} *)

val parse : bytes -> off:int16# -> len:int16# -> #(status * t)
(** [parse buf ~off ~len] parses a chunk starting at [off].

    No size limit checking. Use {!parse_with_limit} for untrusted input.

    Returns [(status, chunk)] where:
    - For {!Complete}: [chunk.data_off] and [chunk.data_len] locate the data;
      call again at [chunk.next_off] for the next chunk
    - For {!Done}: No data; optionally parse trailers at [chunk.next_off]
    - For {!Partial}: Need more data
    - For {!Malformed}: Invalid encoding *)

val parse_with_limit : bytes -> off:int16# -> len:int16# -> max_chunk_size:int -> #(status * t)
(** [parse_with_limit buf ~off ~len ~max_chunk_size] parses with size limit.

    Returns {!Chunk_too_large} if chunk size exceeds [max_chunk_size]. *)

val pp : Stdlib.Format.formatter -> t -> unit
(** Pretty-print chunk. *)

(** {1 Trailer Headers}

    Trailer headers may follow the final chunk per
    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.2}RFC 7230 Section 4.1.2}. *)

type trailer_status =
  | Trailer_complete  (** Trailers parsed successfully *)
  | Trailer_partial   (** Need more data *)
  | Trailer_malformed (** Invalid trailer syntax *)
  | Trailer_bare_cr   (** Bare CR detected (security violation) *)
(** Trailer parse status. *)

val trailer_status_to_string : trailer_status -> string
(** Convert trailer status to string. *)

val pp_trailer_status : Stdlib.Format.formatter -> trailer_status -> unit
(** Pretty-print trailer status. *)

val is_forbidden_trailer : Header_name.t -> bool
(** [is_forbidden_trailer name] returns [true] if [name] is forbidden in trailers.

    Forbidden headers per RFC 7230 Section 4.1.2:
    - Transfer-Encoding, Content-Length (framing)
    - Host (routing)
    - Cache-Control, Expect, etc. (must be in headers) *)

val parse_trailers
  :  bytes
  -> off:int16#
  -> len:int16#
  -> max_header_count:int16#
  -> #(trailer_status * int16# * Header.t list) @ local
(** [parse_trailers buf ~off ~len ~max_header_count] parses trailer headers.

    Call after {!parse} returns {!Done}, using the [next_off] from the
    final chunk.

    Forbidden trailers are silently ignored (not included in result).

    Returns [(status, end_off, headers)] where [end_off] is the offset
    after the trailing CRLF. *)
