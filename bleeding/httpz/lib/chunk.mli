(** This module provides chunked transfer-coding parsing.

    Chunked coding carries each data chunk as a hexadecimal size line, the indicated
    bytes, and CRLF. A zero-size chunk ends the data and can be followed by trailer
    fields. See
    {{:https://www.rfc-editor.org/rfc/rfc9112.html#section-7.1} RFC 9112, Section 7.1}.

    {!parse_with_limit} parses a complete chunk already present in a buffer.
    {!parse_header} is suitable for streaming a chunk whose data does not fit in that
    buffer. *)

(** A [status] is the result of parsing a chunk or chunk-size line. *)
type status =
  | Complete (** [Complete] means a non-final chunk or size line is valid. *)
  | Partial (** [Partial] means more input is required. *)
  | Done (** [Done] means the zero-size final chunk was parsed. *)
  | Malformed (** [Malformed] means the chunk syntax or delimiters are invalid. *)
  | Chunk_too_large
  (** [Chunk_too_large] means the declared size exceeds the supplied limit. *)

(** [status_to_string status] is a stable constructor-like description of [status]. *)
val status_to_string : status -> string @@ portable

(** [pp_status formatter status] is the formatter operation that prints
    {!status_to_string}. *)
val pp_status : Stdlib.Format.formatter -> status -> unit @@ portable

(** A [t] is a complete parsed chunk. *)
type t =
  #{ data_off : int16# (** [data_off] is the first chunk-data byte. *)
   ; data_len : int16# (** [data_len] is the number of chunk-data bytes. *)
   ; next_off : int16#
   (** [next_off] is the first byte of the next chunk. For a final chunk it is
       the first trailer field, or the byte after the terminating CRLF when an
       already-present empty trailer section was consumed. *)
   }

(** [default_max_chunk_size] is 16 MiB. *)
val default_max_chunk_size : int @@ portable

(** [parse buf ~off ~len] is the result of parsing one complete chunk from [off] up to but
    not including [len], with no application size limit. {!Complete} and {!Done} return
    meaningful chunk metadata; other statuses return an empty placeholder. For
    {!Done}, see {!type-t}'s [next_off] distinction between a pending trailer
    section and an empty section already consumed. Use {!parse_with_limit} for
    untrusted sizes. *)
val parse : bytes -> off:int16# -> len:int16# -> #(status * t) @@ portable
[@@zero_alloc opt]

(** [parse_with_limit buf ~off ~len ~max_chunk_size] is {!parse} with the additional bound
    [max_chunk_size]. *)
val parse_with_limit
  :  bytes
  -> off:int16#
  -> len:int16#
  -> max_chunk_size:int
  -> #(status * t)
  @@ portable

(** [parse_header buf ~off ~len ~max_chunk_size] is [(status, size, data_off)] after
    parsing the chunk-size line and its extensions. On {!Complete}, [size] data bytes
    begin at [data_off], whether or not they have all arrived. On {!Done}, [data_off]
    begins the trailer section. The integer and offset are placeholders for other
    statuses. *)
val parse_header
  :  bytes
  -> off:int16#
  -> len:int16#
  -> max_chunk_size:int
  -> #(status * int * int16#)
  @@ portable

(** [pp formatter chunk] is the formatter operation that prints [chunk]'s offsets and
    length. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable

(** A [trailer_status] is the result of parsing a trailer section. *)
type trailer_status =
  | Trailer_complete
  (** [Trailer_complete] means the terminating empty line was parsed. *)
  | Trailer_partial (** [Trailer_partial] means more input is required. *)
  | Trailer_malformed
  (** [Trailer_malformed] means trailer syntax or a configured limit is invalid. *)
  | Trailer_bare_cr (** [Trailer_bare_cr] means a bare CR or LF was detected. *)

(** [trailer_status_to_string status] is a stable constructor-like description of
    [status]. *)
val trailer_status_to_string : trailer_status -> string @@ portable

(** [pp_trailer_status formatter status] is the formatter operation that prints
    {!trailer_status_to_string}. *)
val pp_trailer_status : Stdlib.Format.formatter -> trailer_status -> unit @@ portable

(** [is_forbidden_trailer name] is [true] for a recognized field that Httpz will not
    expose from a trailer because it affects framing, routing, authentication, response
    control, or content interpretation. Trailer restrictions are described by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-6.5.1} RFC 9110, Section 6.5.1}. *)
val is_forbidden_trailer : Header_name.t -> bool @@ portable

val[@zero_alloc] is_forbidden_trailer_name : local_ string -> bool @@ portable
(** [is_forbidden_trailer_name name] applies {!is_forbidden_trailer} to a
    field name supplied outside Httpz's byte-span parser, such as a trailer
    delivered by an HTTP/2 implementation. Field names are matched
    case-insensitively. The caller remains responsible for validating the
    field-name syntax. *)

(** [parse_trailers ?max_trailer_size buf ~off ~len ~max_header_count] is the result of
    parsing the trailer section beginning at [off]. It contains the status, the offset
    after the terminating empty line, and accepted fields in reverse arrival order.
    Recognized forbidden fields are consumed but omitted.

    [max_trailer_size] defaults to 16 KiB. Exceeding either configured limit produces
    {!Trailer_malformed}. On an incomplete or invalid result, the returned offset and
    fields describe only progress made before the failure. *)
val parse_trailers
  :  ?max_trailer_size:int
  -> bytes
  -> off:int16#
  -> len:int16#
  -> max_header_count:int16#
  -> #(trailer_status * int16# * Header.t list) @ local
  @@ portable
