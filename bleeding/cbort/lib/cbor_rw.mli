(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Low-level CBOR encoding and decoding primitives.

    This module provides basic encoding and decoding operations for CBOR
    (Concise Binary Object Representation) data as specified in
    {{:https://www.rfc-editor.org/rfc/rfc8949}RFC 8949}.

    Both encoder and decoder use buffered approaches for efficiency.

    {2 CBOR Data Model}

    CBOR uses a type system based on major types (0-7) that determine how the
    following bytes should be interpreted:

    - Major type 0: Unsigned integer
    - Major type 1: Negative integer
    - Major type 2: Byte string
    - Major type 3: Text string (UTF-8)
    - Major type 4: Array of data items
    - Major type 5: Map of pairs of data items
    - Major type 6: Tagged data item
    - Major type 7: Simple values and floating-point numbers

    {2 Encoding Example}

    {[
      let encode_person name age =
        let buf = Buffer.create 64 in
        let writer = Bytesrw.Bytes.Writer.of_buffer buf in
        let enc = Cbor_rw.make_encoder writer in
        Cbor_rw.write_map_start enc 2;
        Cbor_rw.write_text enc "name";
        Cbor_rw.write_text enc name;
        Cbor_rw.write_text enc "age";
        Cbor_rw.write_int enc age;
        Cbor_rw.flush_encoder enc;
        Buffer.contents buf
    ]}

    {2 Decoding Example}

    {[
      let decode_person bytes =
        let reader = Bytesrw.Bytes.Reader.of_string bytes in
        let dec = Cbor_rw.make_decoder reader in
        let len = Cbor_rw.read_map_start dec in
        (* ... read key-value pairs ... *)
    ]} *)

(** {1 CBOR Major Types}

    Constants for the seven CBOR major types as defined in RFC 8949 Section 3.1.
    These are used as the high 3 bits of the initial byte. *)

val major_uint : int
(** Major type 0: Unsigned integer (0x00-0x1b). *)

val major_nint : int
(** Major type 1: Negative integer (0x20-0x3b). The value is [-1 - n] where [n]
    is the encoded unsigned integer. *)

val major_bytes : int
(** Major type 2: Byte string (0x40-0x5b). *)

val major_text : int
(** Major type 3: Text string in UTF-8 encoding (0x60-0x7b). *)

val major_array : int
(** Major type 4: Array of data items (0x80-0x9b). *)

val major_map : int
(** Major type 5: Map of pairs of data items (0xa0-0xbb). *)

val major_tag : int
(** Major type 6: Tagged data item (0xc0-0xdb). *)

val major_simple : int
(** Major type 7: Simple values and floating-point (0xe0-0xfb). *)

(** {1 CBOR Simple Values}

    Simple values are encoded using major type 7. These constants represent the
    most commonly used simple values. *)

val simple_false : int
(** Simple value 20: Boolean false. *)

val simple_true : int
(** Simple value 21: Boolean true. *)

val simple_null : int
(** Simple value 22: Null (absence of value). *)

val simple_undefined : int
(** Simple value 23: Undefined value. *)

(** {1 CBOR Additional Information}

    The low 5 bits of the initial byte encode additional information about the
    data item. Values 0-23 encode the value directly; values 24-27 indicate that
    1, 2, 4, or 8 bytes follow containing the value. *)

val ai_1byte : int
(** Additional info 24: One byte follows with the value. *)

val ai_2byte : int
(** Additional info 25: Two bytes follow in big-endian order. *)

val ai_4byte : int
(** Additional info 26: Four bytes follow in big-endian order. *)

val ai_8byte : int
(** Additional info 27: Eight bytes follow in big-endian order. *)

val ai_indefinite : int
(** Additional info 31: Indefinite-length encoding (used with break code). *)

val break_code : int
(** Break stop code (0xff) for terminating indefinite-length items. *)

(** {1:encoder Encoder} *)

type encoder
(** A buffered CBOR encoder that writes to a {!Bytesrw.Bytes.Writer.t}. *)

val make_encoder : Bytesrw.Bytes.Writer.t -> encoder
(** [make_encoder writer] creates a new encoder that outputs to [writer]. The
    encoder uses an internal buffer of 4096 bytes. *)

val flush_encoder : encoder -> unit
(** [flush_encoder enc] writes any buffered data to the underlying writer. Must
    be called after encoding is complete to ensure all data is written. *)

(** {2 Low-level Write Operations}

    These functions write raw bytes to the encoder's buffer. They are used
    internally by the higher-level encoding functions. *)

val write_byte : encoder -> int -> unit
(** [write_byte enc b] writes a single byte [b] (0-255). *)

val write_bytes : encoder -> string -> unit
(** [write_bytes enc s] writes the raw bytes of string [s]. *)

val write_u16_be : encoder -> int -> unit
(** [write_u16_be enc v] writes [v] as a 16-bit big-endian unsigned integer. *)

val write_u32_be : encoder -> int32 -> unit
(** [write_u32_be enc v] writes [v] as a 32-bit big-endian integer. *)

val write_u64_be : encoder -> int64 -> unit
(** [write_u64_be enc v] writes [v] as a 64-bit big-endian integer. *)

(** {2 CBOR Type Header Encoding}

    These functions encode the CBOR initial byte and any following argument
    bytes using the shortest possible encoding (deterministic encoding). *)

val write_type_arg : encoder -> int -> int -> unit
(** [write_type_arg enc major arg] writes a CBOR type header with the given
    major type and argument value. Uses the shortest encoding for [arg]. *)

val write_type_arg64 : encoder -> int -> int64 -> unit
(** [write_type_arg64 enc major arg] writes a CBOR type header with a 64-bit
    argument value. Uses the shortest encoding for [arg]. *)

(** {2 CBOR Value Encoding}

    High-level functions for encoding CBOR data items. *)

val write_null : encoder -> unit
(** [write_null enc] encodes a CBOR null value (simple value 22). *)

val write_undefined : encoder -> unit
(** [write_undefined enc] encodes a CBOR undefined value (simple value 23). *)

val write_bool : encoder -> bool -> unit
(** [write_bool enc b] encodes a CBOR boolean value. *)

val write_simple : encoder -> int -> unit
(** [write_simple enc n] encodes simple value [n] (0-255). *)

val write_float16 : encoder -> float -> unit
(** [write_float16 enc f] encodes a 16-bit half-precision float. *)

val write_float32 : encoder -> float -> unit
(** [write_float32 enc f] encodes a 32-bit single-precision float. *)

val write_float : encoder -> float -> unit
(** [write_float enc f] encodes a 64-bit double-precision float. *)

val write_int : encoder -> int -> unit
(** [write_int enc n] encodes an OCaml [int] as a CBOR integer. Positive values
    use major type 0; negative values use major type 1. *)

val write_int64 : encoder -> int64 -> unit
(** [write_int64 enc n] encodes a 64-bit integer as a CBOR integer. *)

val write_text : encoder -> string -> unit
(** [write_text enc s] encodes a UTF-8 text string (major type 3). The string
    should be valid UTF-8; no validation is performed. *)

val write_text_start : encoder -> unit
(** [write_text_start enc] starts an indefinite-length text string. Call
    {!write_text_chunk} for each chunk and {!write_break} to end. *)

val write_text_chunk : encoder -> string -> unit
(** [write_text_chunk enc s] writes a text chunk in an indefinite string. *)

val write_bytes_data : encoder -> string -> unit
(** [write_bytes_data enc s] encodes a byte string (major type 2). *)

val write_bytes_header : encoder -> int -> unit
(** [write_bytes_header enc len] writes the header for a byte string of length
    [len]. The caller must then write exactly [len] bytes using {!write_bytes}.
*)

val write_bytes_start : encoder -> unit
(** [write_bytes_start enc] starts an indefinite-length byte string. *)

val write_bytes_chunk : encoder -> string -> unit
(** [write_bytes_chunk enc s] writes a byte chunk in an indefinite string. *)

val write_array_start : encoder -> int -> unit
(** [write_array_start enc n] writes the header for a definite-length array of
    [n] items. The caller must then encode exactly [n] data items. *)

val write_array_indef : encoder -> unit
(** [write_array_indef enc] starts an indefinite-length array. Encode items then
    call {!write_break} to end. *)

val write_map_start : encoder -> int -> unit
(** [write_map_start enc n] writes the header for a definite-length map of [n]
    key-value pairs. The caller must then encode exactly [2*n] data items
    (alternating keys and values). *)

val write_map_indef : encoder -> unit
(** [write_map_indef enc] starts an indefinite-length map. Encode key-value
    pairs then call {!write_break} to end. *)

val write_tag : encoder -> int -> unit
(** [write_tag enc n] writes tag number [n]. The next item written is the tagged
    content. *)

val write_break : encoder -> unit
(** [write_break enc] writes the break stop code (0xff) to terminate an
    indefinite-length item. *)

(** {1:decoder Decoder} *)

type decoder
(** A buffered CBOR decoder that reads from a {!Bytesrw.Bytes.Reader.t}. *)

val make_decoder : Bytesrw.Bytes.Reader.t -> decoder
(** [make_decoder reader] creates a new decoder that reads from [reader]. *)

(** {2 Decoder State} *)

val decoder_at_end : decoder -> bool
(** [decoder_at_end dec] returns [true] if the decoder has reached the end of
    input. *)

val decoder_position : decoder -> int
(** [decoder_position dec] returns the current byte offset in the input. *)

(** {2 Low-level Read Operations} *)

val peek_byte : decoder -> int option
(** [peek_byte dec] returns the next byte without consuming it, or [None] at end
    of input. *)

val read_byte : decoder -> int
(** [read_byte dec] reads and returns a single byte (0-255).
    @raise End_of_file if at end of input. *)

val read_bytes : decoder -> int -> string
(** [read_bytes dec n] reads exactly [n] bytes as a string.
    @raise End_of_file if fewer than [n] bytes available. *)

val read_u16_be : decoder -> int
(** [read_u16_be dec] reads a 16-bit big-endian unsigned integer. *)

val read_u32_be : decoder -> int32
(** [read_u32_be dec] reads a 32-bit big-endian integer. *)

val read_u64_be : decoder -> int64
(** [read_u64_be dec] reads a 64-bit big-endian integer. *)

(** {2 CBOR Type Header Decoding} *)

type header = {
  major : int;  (** Major type (0-7). *)
  info : int;  (** Additional info (0-31). *)
}
(** A decoded CBOR initial byte. *)

val read_header : decoder -> header
(** [read_header dec] reads the initial byte of a CBOR item, returning the major
    type and additional info. *)

val read_argument : decoder -> header -> int64
(** [read_argument dec hdr] reads the argument value based on the additional
    info in [hdr]. For info 0-23, returns that value. For 24-27, reads the
    following 1/2/4/8 bytes. Returns -1 for indefinite length (31). *)

(** {2 CBOR Value Decoding} *)

val read_int : decoder -> int64
(** [read_int dec] reads a CBOR integer (major type 0 or 1). Returns the signed
    value.
    @raise Failure if not an integer type. *)

val read_text : decoder -> string
(** [read_text dec] reads a CBOR text string (major type 3). Handles both
    definite and indefinite-length strings.
    @raise Failure if not a text string. *)

val read_bytes_data : decoder -> string
(** [read_bytes_data dec] reads a CBOR byte string (major type 2). Handles both
    definite and indefinite-length strings.
    @raise Failure if not a byte string. *)

val read_float : decoder -> float
(** [read_float dec] reads a CBOR float (major type 7 with info 25-27). Handles
    half, single, and double precision.
    @raise Failure if not a float. *)

val read_bool : decoder -> bool
(** [read_bool dec] reads a CBOR boolean (simple values 20 or 21).
    @raise Failure if not a boolean. *)

val read_null : decoder -> unit
(** [read_null dec] reads a CBOR null (simple value 22).
    @raise Failure if not null. *)

val read_undefined : decoder -> unit
(** [read_undefined dec] reads a CBOR undefined (simple value 23).
    @raise Failure if not undefined. *)

val read_simple : decoder -> int
(** [read_simple dec] reads a CBOR simple value (major type 7 with info 0-24).
    Returns the simple value number.
    @raise Failure if not a simple value. *)

val read_array_start : decoder -> int option
(** [read_array_start dec] reads an array header (major type 4). Returns
    [Some n] for definite-length array of [n] items, or [None] for
    indefinite-length array.
    @raise Failure if not an array. *)

val read_map_start : decoder -> int option
(** [read_map_start dec] reads a map header (major type 5). Returns [Some n] for
    definite-length map of [n] pairs, or [None] for indefinite-length map.
    @raise Failure if not a map. *)

val read_tag : decoder -> int
(** [read_tag dec] reads a tag number (major type 6).
    @raise Failure if not a tag. *)

val is_break : decoder -> bool
(** [is_break dec] returns [true] if the next byte is the break code (0xff).
    Does not consume the byte. Use {!skip_break} to consume it. *)

val skip_break : decoder -> unit
(** [skip_break dec] consumes the break code.
    @raise Failure if next byte is not break. *)

(** {2 Skipping} *)

val skip : decoder -> unit
(** [skip dec] skips the next complete CBOR data item, including any nested
    items in arrays, maps, or tags. *)

(** {1:cbor CBOR Value I/O}

    Functions for reading and writing complete {!Cbor.t} values. *)

val write_cbor : encoder -> Cbor.t -> unit
(** [write_cbor enc v] encodes CBOR value [v] using deterministic encoding. *)

val read_cbor : decoder -> Cbor.t
(** [read_cbor dec] reads a complete CBOR data item.
    @raise Failure on invalid CBOR or unexpected end of input. *)
