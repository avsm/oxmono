(** Chunked transfer encoding parser. *)

(** Chunked transfer encoding parse status. *)
type status =
  | Complete       (** Chunk parsed successfully *)
  | Partial        (** Need more data *)
  | Done           (** Final chunk (zero-length) *)
  | Malformed      (** Invalid chunk *)
  | Chunk_too_large  (** Chunk size exceeds limit *)

(** Convert status to string representation. *)
val status_to_string : status -> string

(** Pretty-print status. *)
val pp_status : Stdlib.Format.formatter -> status -> unit

(** Unboxed chunk record. *)
type t =
  #{ data_off : int16# (** Offset of chunk data in buffer *)
   ; data_len : int16# (** Length of chunk data *)
   ; next_off : int16# (** Offset where next chunk starts *)
   }

(** Maximum hex digits for chunk size (16 = 64-bit max). *)
val max_hex_digits : int16#

(** Default maximum chunk size: 16MB. *)
val default_max_chunk_size : int

(** Parse a single chunk starting at [off] with size limit. Buffer contains [len] bytes total.
    Returns chunk info and status. For [Complete], use [next_off] to parse the next chunk.
    For [Done], parsing is complete. Returns [Chunk_too_large] if size exceeds limit. *)
val parse_with_limit : bytes -> off:int16# -> len:int16# -> max_chunk_size:int -> #(status * t)

(** Parse a single chunk starting at [off]. Buffer contains [len] bytes total. Returns
    chunk info and status. For [Complete], use [next_off] to parse the next chunk. For
    [Done], parsing is complete. No size limit checking. *)
val parse : bytes -> off:int16# -> len:int16# -> #(status * t)

(** Pretty-print chunk. *)
val pp : Stdlib.Format.formatter -> t -> unit

(** {2 Trailer Headers}

    {{:https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.2}RFC 7230 Section 4.1.2}
    specifies that trailer headers may follow the final chunk. *)

(** Trailer parsing status. *)
type trailer_status =
  | Trailer_complete  (** Trailers parsed successfully *)
  | Trailer_partial   (** Need more data *)
  | Trailer_malformed (** Invalid trailer *)

(** Convert trailer status to string. *)
val trailer_status_to_string : trailer_status -> string

(** Pretty-print trailer status. *)
val pp_trailer_status : Stdlib.Format.formatter -> trailer_status -> unit

(** Check if a header name is forbidden in trailers per RFC 7230 Section 4.1.2.
    Forbidden headers include: Transfer-Encoding, Content-Length, Host, and
    headers necessary for message framing or routing. *)
val is_forbidden_trailer : Header_name.t -> bool

(** Parse trailer headers after the final chunk.
    Call this after [parse] returns [Done]. The [off] should be the [next_off]
    from the final chunk. Returns status, new offset, and parsed headers.
    Forbidden trailer headers are silently ignored per RFC 7230. *)
val parse_trailers
  :  bytes
  -> off:int16#
  -> len:int16#
  -> max_header_count:int16#
  -> #(trailer_status * int16# * Header.t list) @ local
