(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP/2 Frame Layer.

    This module implements HTTP/2 frame parsing and serialization as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-4}RFC 9113 Section 4}
    and {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6}RFC 9113 Section 6}.

    {2 Frame Structure}

    All HTTP/2 frames share a common 9-octet header:
    {v
    +-----------------------------------------------+
    |                 Length (24)                   |
    +---------------+---------------+---------------+
    |   Type (8)    |   Flags (8)   |
    +-+-------------+---------------+-------------------------------+
    |R|                 Stream Identifier (31)                      |
    +=+=============================================================+
    |                   Frame Payload (0...)                      ...
    +---------------------------------------------------------------+
    v}

    Per RFC 9113 Section 4.1:
    - Length: 24-bit unsigned integer (payload length, not including header)
    - Type: 8-bit frame type
    - Flags: 8-bit flags specific to frame type
    - R: Reserved 1-bit (MUST be 0 when sending, MUST be ignored when receiving)
    - Stream Identifier: 31-bit unsigned integer (0 for connection-level frames)
*)

(** {1 Stream Identifier} *)

type stream_id = int32
(** Stream identifier. Per RFC 9113 Section 5.1.1:
    - Client-initiated streams use odd numbers
    - Server-initiated streams use even numbers
    - Stream 0 is reserved for connection-level frames *)

val stream_id_is_client_initiated : stream_id -> bool
(** [stream_id_is_client_initiated id] returns true for odd stream IDs. *)

val stream_id_is_server_initiated : stream_id -> bool
(** [stream_id_is_server_initiated id] returns true for even non-zero stream IDs. *)

(** {1 Frame Types}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6}RFC 9113 Section 6}. *)

type frame_type =
  | Data          (** 0x00 - RFC 9113 Section 6.1 *)
  | Headers       (** 0x01 - RFC 9113 Section 6.2 *)
  | Priority      (** 0x02 - RFC 9113 Section 6.3 (deprecated) *)
  | Rst_stream    (** 0x03 - RFC 9113 Section 6.4 *)
  | Settings      (** 0x04 - RFC 9113 Section 6.5 *)
  | Push_promise  (** 0x05 - RFC 9113 Section 6.6 *)
  | Ping          (** 0x06 - RFC 9113 Section 6.7 *)
  | Goaway        (** 0x07 - RFC 9113 Section 6.8 *)
  | Window_update (** 0x08 - RFC 9113 Section 6.9 *)
  | Continuation  (** 0x09 - RFC 9113 Section 6.10 *)
  | Unknown of int (** Unknown frame type - MUST be ignored per RFC 9113 Section 5.5 *)

val frame_type_to_int : frame_type -> int
(** Convert frame type to its numeric value. *)

val frame_type_of_int : int -> frame_type
(** Convert numeric value to frame type. *)

val pp_frame_type : Format.formatter -> frame_type -> unit
(** Pretty printer for frame types. *)

(** {1 Frame Flags} *)

module Flags : sig
  type t = int
  (** Frame flags as a bitmask. *)

  (** {2 Common Flags} *)

  val none : t
  (** No flags set. *)

  val end_stream : t
  (** END_STREAM (0x01) - Indicates final frame in a stream. *)

  val end_headers : t
  (** END_HEADERS (0x04) - Indicates header block is complete. *)

  val padded : t
  (** PADDED (0x08) - Frame is padded. *)

  val priority : t
  (** PRIORITY (0x20) - Priority information present. *)

  val ack : t
  (** ACK (0x01) - Acknowledgment (for SETTINGS and PING). *)

  (** {2 Flag Operations} *)

  val test : t -> t -> bool
  (** [test flags flag] returns true if [flag] is set in [flags]. *)

  val set : t -> t -> t
  (** [set flags flag] returns [flags] with [flag] set. *)

  val clear : t -> t -> t
  (** [clear flags flag] returns [flags] with [flag] cleared. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for flags. *)
end

(** {1 Frame Header} *)

type frame_header = {
  length : int;
  (** Payload length (24-bit unsigned). MUST NOT exceed SETTINGS_MAX_FRAME_SIZE. *)
  frame_type : frame_type;
  (** Frame type (8-bit). *)
  flags : Flags.t;
  (** Frame-specific flags (8-bit). *)
  stream_id : stream_id;
  (** Stream identifier (31-bit). 0 for connection-level frames. *)
}

val frame_header_length : int
(** Frame header is always 9 octets. *)

val pp_frame_header : Format.formatter -> frame_header -> unit
(** Pretty printer for frame headers. *)

(** {1 Error Codes}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-7}RFC 9113 Section 7}. *)

type error_code =
  | No_error             (** 0x00 - Graceful shutdown *)
  | Protocol_error       (** 0x01 - Protocol error detected *)
  | Internal_error       (** 0x02 - Implementation fault *)
  | Flow_control_error   (** 0x03 - Flow control limits exceeded *)
  | Settings_timeout     (** 0x04 - Settings not acknowledged in time *)
  | Stream_closed        (** 0x05 - Frame received for closed stream *)
  | Frame_size_error     (** 0x06 - Frame size incorrect *)
  | Refused_stream       (** 0x07 - Stream not processed *)
  | Cancel               (** 0x08 - Stream cancelled *)
  | Compression_error    (** 0x09 - Compression state not updated *)
  | Connect_error        (** 0x0a - TCP connection error for CONNECT *)
  | Enhance_your_calm    (** 0x0b - Processing capacity exceeded *)
  | Inadequate_security  (** 0x0c - Negotiated TLS parameters not acceptable *)
  | Http_1_1_required    (** 0x0d - Use HTTP/1.1 for this request *)
  | Unknown_error of int32 (** Unknown error code *)

val error_code_to_int32 : error_code -> int32
(** Convert error code to its numeric value. *)

val error_code_of_int32 : int32 -> error_code
(** Convert numeric value to error code. *)

val error_code_to_string : error_code -> string
(** Convert error code to its string representation. *)

val pp_error_code : Format.formatter -> error_code -> unit
(** Pretty printer for error codes. *)

(** {1 Settings}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6.5.2}RFC 9113 Section 6.5.2}. *)

type setting =
  | Header_table_size of int       (** 0x01 - HPACK dynamic table size *)
  | Enable_push of bool            (** 0x02 - Server push enabled *)
  | Max_concurrent_streams of int32 (** 0x03 - Maximum concurrent streams *)
  | Initial_window_size of int32   (** 0x04 - Initial flow control window *)
  | Max_frame_size of int          (** 0x05 - Maximum frame payload size *)
  | Max_header_list_size of int    (** 0x06 - Maximum header list size *)
  | No_rfc7540_priorities of bool  (** 0x09 - RFC 9113: Deprecate RFC 7540 priorities *)
  | Unknown_setting of int * int32 (** Unknown setting (id, value) *)

val setting_to_pair : setting -> int * int32
(** Convert setting to (identifier, value) pair. *)

val setting_of_pair : int -> int32 -> setting
(** Convert (identifier, value) pair to setting. *)

val pp_setting : Format.formatter -> setting -> unit
(** Pretty printer for settings. *)

(** {1 Priority}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6.3}RFC 9113 Section 6.3}.

    Note: Stream prioritization is deprecated in RFC 9113 but MUST still be parsed. *)

type priority = {
  exclusive : bool;
  (** Exclusive dependency flag. *)
  stream_dependency : stream_id;
  (** Stream this one depends on. *)
  weight : int;
  (** Weight 1-256 (stored as 1-256, not 0-255). *)
}

val default_priority : priority
(** Default priority: non-exclusive, depends on 0, weight 16. *)

val pp_priority : Format.formatter -> priority -> unit
(** Pretty printer for priority. *)

(** {1 Frame Payloads} *)

type frame_payload =
  | Data_payload of {
      data : Cstruct.t;
      (** The actual data being transferred. *)
    }
  | Headers_payload of {
      priority : priority option;
      (** Priority if PRIORITY flag set. *)
      header_block : Cstruct.t;
      (** Encoded header block fragment (HPACK). *)
    }
  | Priority_payload of priority
    (** Priority specification (deprecated). *)
  | Rst_stream_payload of error_code
    (** Error code for stream termination. *)
  | Settings_payload of setting list
    (** List of settings. Empty list for ACK. *)
  | Push_promise_payload of {
      promised_stream_id : stream_id;
      (** Stream ID being reserved. *)
      header_block : Cstruct.t;
      (** Encoded header block fragment. *)
    }
  | Ping_payload of Cstruct.t
    (** 8 bytes of opaque data. *)
  | Goaway_payload of {
      last_stream_id : stream_id;
      (** Highest processed stream ID. *)
      error_code : error_code;
      (** Reason for closing connection. *)
      debug_data : Cstruct.t;
      (** Optional diagnostic data. *)
    }
  | Window_update_payload of int32
    (** Window size increment (1 to 2^31-1). *)
  | Continuation_payload of {
      header_block : Cstruct.t;
      (** Continuation of header block. *)
    }
  | Unknown_payload of Cstruct.t
    (** Payload for unknown frame types. *)

(** {1 Complete Frame} *)

type frame = {
  header : frame_header;
  (** Frame header. *)
  payload : frame_payload;
  (** Frame payload. *)
}

val pp_frame : Format.formatter -> frame -> unit
(** Pretty printer for frames. *)

(** {1 Frame Parsing}

    Parse frames from Eio buffered input. *)

type parse_error =
  | Incomplete
    (** Not enough data available. *)
  | Frame_size_error of string
    (** Frame size exceeds limits. *)
  | Protocol_error of string
    (** Protocol violation. *)

val pp_parse_error : Format.formatter -> parse_error -> unit
(** Pretty printer for parse errors. *)

val parse_frame_header : Cstruct.t -> (frame_header, parse_error) result
(** [parse_frame_header buf] parses a 9-byte frame header.
    Returns [Error Incomplete] if buffer is too small. *)

val parse_frame_payload :
  frame_header ->
  Cstruct.t ->
  (frame_payload, parse_error) result
(** [parse_frame_payload header buf] parses frame payload based on type.
    The buffer should contain exactly [header.length] bytes. *)

val parse_frame :
  Cstruct.t ->
  max_frame_size:int ->
  (frame * int, parse_error) result
(** [parse_frame buf ~max_frame_size] parses a complete frame.
    Returns the frame and number of bytes consumed.
    [max_frame_size] is the current SETTINGS_MAX_FRAME_SIZE value.
    Returns [Error Frame_size_error] if payload exceeds limit. *)

(** {1 Frame Serialization}

    Serialize frames to Eio buffered output. *)

val serialize_frame_header : frame_header -> Cstruct.t
(** [serialize_frame_header header] serializes a frame header to 9 bytes. *)

val serialize_frame : frame -> Cstruct.t
(** [serialize_frame frame] serializes a complete frame. *)

val write_frame : Eio.Buf_write.t -> frame -> unit
(** [write_frame writer frame] writes a frame to the buffer. *)

(** {1 Frame Construction Helpers} *)

val make_data :
  stream_id:stream_id ->
  ?end_stream:bool ->
  Cstruct.t ->
  frame
(** [make_data ~stream_id ?end_stream data] creates a DATA frame. *)

val make_headers :
  stream_id:stream_id ->
  ?end_stream:bool ->
  ?end_headers:bool ->
  ?priority:priority ->
  Cstruct.t ->
  frame
(** [make_headers ~stream_id ?end_stream ?end_headers ?priority block]
    creates a HEADERS frame. *)

val make_rst_stream :
  stream_id:stream_id ->
  error_code ->
  frame
(** [make_rst_stream ~stream_id code] creates a RST_STREAM frame. *)

val make_settings :
  ?ack:bool ->
  setting list ->
  frame
(** [make_settings ?ack settings] creates a SETTINGS frame. *)

val make_ping :
  ?ack:bool ->
  Cstruct.t ->
  frame
(** [make_ping ?ack data] creates a PING frame.
    [data] must be exactly 8 bytes. *)

val make_goaway :
  last_stream_id:stream_id ->
  error_code ->
  ?debug:string ->
  unit ->
  frame
(** [make_goaway ~last_stream_id code ?debug ()] creates a GOAWAY frame. *)

val make_window_update :
  stream_id:stream_id ->
  int32 ->
  frame
(** [make_window_update ~stream_id increment] creates a WINDOW_UPDATE frame. *)

val make_continuation :
  stream_id:stream_id ->
  ?end_headers:bool ->
  Cstruct.t ->
  frame
(** [make_continuation ~stream_id ?end_headers block] creates a CONTINUATION frame. *)

(** {1 Constants} *)

val default_max_frame_size : int
(** Default SETTINGS_MAX_FRAME_SIZE: 16384 (2^14). *)

val max_max_frame_size : int
(** Maximum SETTINGS_MAX_FRAME_SIZE: 16777215 (2^24 - 1). *)

val default_initial_window_size : int32
(** Default initial flow control window: 65535 (2^16 - 1). *)

val max_window_size : int32
(** Maximum flow control window: 2147483647 (2^31 - 1). *)

val connection_preface : string
(** HTTP/2 connection preface (magic string):
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" *)

val connection_preface_length : int
(** Length of connection preface: 24 bytes. *)
