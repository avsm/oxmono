(*---------------------------------------------------------------------------
  Copyright (c) 2019 Antonio Nuno Monteiro.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** HTTP/2 Connection Management per RFC 9113.

    Manages connection-level state, settings, and stream multiplexing.

    {2 Usage}

    {[
      (* Create a client connection *)
      let conn = H2_connection.create Client in

      (* Create a new stream *)
      match H2_connection.create_stream conn with
      | Ok stream -> (* use stream *)
      | Error _ -> (* handle error *)

      (* Encode headers for sending *)
      let header_block = H2_connection.encode_headers conn headers in

      (* Decode received headers *)
      match H2_connection.decode_headers conn header_block with
      | Ok headers -> (* process headers *)
      | Error _ -> (* handle error *)
    ]}
*)

(** {1 Connection Preface} *)

val connection_preface : string
(** HTTP/2 connection preface sent by client:
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" *)

val connection_preface_length : int
(** Length of connection preface: 24 bytes. *)

val is_connection_preface : Cstruct.t -> bool
(** [is_connection_preface buf] returns true if [buf] starts with
    the HTTP/2 connection preface. *)

(** {1 Settings} *)

(** Connection settings per RFC 9113 Section 6.5.2. *)
type settings = {
  header_table_size : int;
      (** SETTINGS_HEADER_TABLE_SIZE (default 4096). *)
  enable_push : bool;
      (** SETTINGS_ENABLE_PUSH (default true for server, false for client). *)
  max_concurrent_streams : int option;
      (** SETTINGS_MAX_CONCURRENT_STREAMS (no limit by default). *)
  initial_window_size : int;
      (** SETTINGS_INITIAL_WINDOW_SIZE (default 65535). *)
  max_frame_size : int;
      (** SETTINGS_MAX_FRAME_SIZE (default 16384, max 16777215). *)
  max_header_list_size : int option;
      (** SETTINGS_MAX_HEADER_LIST_SIZE (no limit by default). *)
  no_rfc7540_priorities : bool;
      (** SETTINGS_NO_RFC7540_PRIORITIES (RFC 9113 Section 5.3.2, default false). *)
}

val default_settings : settings
(** Default settings per RFC 9113. *)

val default_client_settings : settings
(** Default settings for client connections (enable_push = false). *)

val apply_settings : settings -> (int32 * int32) list ->
  (settings, H2_frame.error_code * string) result
(** [apply_settings settings pairs] applies settings changes.
    Returns updated settings or error. *)

val settings_to_pairs : settings -> (int * int32) list
(** [settings_to_pairs settings] converts to (id, value) pairs for encoding. *)

(** {1 Connection Role} *)

(** Role in the connection. *)
type role = Client | Server

val next_stream_id : role -> int32 -> int32
(** [next_stream_id role current] returns the next stream ID for this role. *)

val valid_stream_id_for_role : role -> int32 -> bool
(** [valid_stream_id_for_role role id] checks if [id] can be initiated by [role]. *)

(** {1 Connection State} *)

(** Connection state. *)
type state =
  | Handshaking
      (** Waiting for settings exchange. *)
  | Open
      (** Connection is active. *)
  | Closing of { last_stream_id : int32; error_code : H2_frame.error_code; debug : string }
      (** Sent or received GOAWAY. *)
  | Closed
      (** Connection is closed. *)

(** Connection error. *)
type connection_error = {
  error_code : H2_frame.error_code;
  debug_data : string;
  stream_id : int32 option;
}

(** {1 Connection Type} *)

(** An HTTP/2 connection. *)
type t

val create : ?settings:settings -> role -> t
(** [create ?settings role] creates a new connection.
    Default settings are used if not specified. *)

(** {1 Connection Properties} *)

val role : t -> role
(** [role t] returns the connection role. *)

val state : t -> state
(** [state t] returns the connection state. *)

val is_open : t -> bool
(** [is_open t] returns true if connection is open. *)

val is_closing : t -> bool
(** [is_closing t] returns true if connection is closing or closed. *)

val local_settings : t -> settings
(** [local_settings t] returns our settings. *)

val peer_settings : t -> settings
(** [peer_settings t] returns peer's settings. *)

val active_stream_count : t -> int
(** [active_stream_count t] returns number of active streams. *)

val send_window : t -> int
(** [send_window t] returns connection-level send window. *)

val recv_window : t -> int
(** [recv_window t] returns connection-level receive window. *)

val max_send_window : t -> int
(** [max_send_window t] returns max send window from peer's settings. *)

(** {1 Stream Management} *)

val get_stream : t -> int32 -> H2_stream.t option
(** [get_stream t id] returns the stream with [id], or None. *)

val create_stream : t -> (H2_stream.t, H2_frame.error_code * string) result
(** [create_stream t] creates a new stream initiated by us.
    Returns error if connection is closing. *)

val register_peer_stream : t -> int32 -> (H2_stream.t, H2_frame.error_code * string) result
(** [register_peer_stream t id] registers a stream initiated by peer. *)

val remove_stream : t -> int32 -> unit
(** [remove_stream t id] removes a closed stream. *)

val exceeds_max_concurrent_streams : t -> bool
(** [exceeds_max_concurrent_streams t] checks if we've hit the limit. *)

val iter_streams : t -> (H2_stream.t -> unit) -> unit
(** [iter_streams t f] calls [f] on each active stream. *)

(** {1 Flow Control} *)

val consume_send_window : t -> int -> int
(** [consume_send_window t bytes] consumes from connection send window.
    Returns bytes actually consumed. *)

val credit_send_window : t -> int -> (unit, H2_frame.error_code * string) result
(** [credit_send_window t increment] credits send window. *)

val consume_recv_window : t -> int -> unit
(** [consume_recv_window t bytes] consumes from receive window. *)

val credit_recv_window : t -> int -> unit
(** [credit_recv_window t increment] credits receive window. *)

(** {1 Settings Handling} *)

val handle_settings : t -> ack:bool -> (int32 * int32) list ->
  ([`Ack_received | `Settings_received], H2_frame.error_code * string) result
(** [handle_settings t ~ack pairs] processes a received SETTINGS frame. *)

val mark_settings_sent : t -> unit
(** [mark_settings_sent t] marks that we've sent SETTINGS. *)

(** {1 HPACK Encoding/Decoding} *)

val encode_headers : t -> H2_hpack.header list -> Cstruct.t
(** [encode_headers t headers] encodes headers using connection HPACK context. *)

val decode_headers : t -> Cstruct.t -> (H2_hpack.header list, H2_hpack.error) result
(** [decode_headers t block] decodes header block. *)

(** {1 GOAWAY Handling} *)

val go_away : t -> H2_frame.error_code -> string -> unit
(** [go_away t code debug] initiates graceful shutdown. *)

val handle_goaway : t -> last_stream_id:int32 -> error_code:H2_frame.error_code ->
  debug:string -> unit
(** [handle_goaway t ~last_stream_id ~error_code ~debug] handles received GOAWAY. *)

val close : t -> unit
(** [close t] closes the connection. *)

(** {1 Preface Handling} *)

val mark_preface_sent : t -> unit
(** [mark_preface_sent t] marks preface as sent. *)

val mark_preface_received : t -> unit
(** [mark_preface_received t] marks preface as received. *)

val handshake_complete : t -> bool
(** [handshake_complete t] returns true if handshake is done. *)

(** {1 Pretty Printing} *)

val pp_role : Format.formatter -> role -> unit
(** Pretty print role. *)

val pp_state : Format.formatter -> state -> unit
(** Pretty print state. *)

val pp : Format.formatter -> t -> unit
(** Pretty print connection. *)
