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

(** HTTP/2 Stream State Machine per RFC 9113 Section 5.1.

    This module implements the HTTP/2 stream lifecycle as defined in
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.1}RFC 9113 Section 5.1}.

    {2 Stream States}

{v
                                +--------+
                        send PP |        | recv PP
                       ,--------+  idle  +--------.
                      /         |        |         \
                     v          +--------+          v
              +----------+          |           +----------+
              |          |          | send H /  |          |
       ,------+ reserved |          | recv H    | reserved +------.
       |      | (local)  |          |           | (remote) |      |
       |      +---+------+          v           +------+---+      |
       |          |             +--------+             |          |
       |          |     recv ES |        | send ES     |          |
       |   send H |     ,-------+  open  +-------.     | recv H   |
       |          |    /        |        |        \    |          |
       |          v   v         +---+----+         v   v          |
       |      +----------+          |           +----------+      |
       |      |   half-  |          |           |   half-  |      |
       |      |  closed  |          | send R /  |  closed  |      |
       |      | (remote) |          | recv R    | (local)  |      |
       |      +----+-----+          |           +-----+----+      |
       |           |                |                 |           |
       |           | send ES /      |       recv ES / |           |
       |           |  send R /      v        send R / |           |
       |           |  recv R    +--------+   recv R   |           |
       | send R /  `----------->|        |<-----------'  send R / |
       | recv R                 | closed |               recv R   |
       `----------------------->|        |<-----------------------'
                                +--------+
v}

    {2 Usage}

    {[
      (* Create a new stream *)
      let stream = H2_stream.create 1l in

      (* Apply events to transition state *)
      match H2_stream.apply_event stream (Send_headers { end_stream = false }) with
      | Ok () -> (* stream is now Open *)
      | Error (code, msg) -> (* protocol error *)
    ]}
*)

(** {1 Types} *)

(** Why a stream was closed. *)
type closed_reason =
  | Finished
      (** Both endpoints sent END_STREAM. *)
  | ResetByUs of H2_frame.error_code
      (** We sent RST_STREAM. *)
  | ResetByThem of H2_frame.error_code
      (** Peer sent RST_STREAM. *)

(** Stream state per RFC 9113 Section 5.1. *)
type state =
  | Idle
      (** Initial state. *)
  | Reserved_local
      (** We sent PUSH_PROMISE. *)
  | Reserved_remote
      (** Peer sent PUSH_PROMISE. *)
  | Open
      (** Both sides can send frames. *)
  | Half_closed_local
      (** We sent END_STREAM. *)
  | Half_closed_remote
      (** Peer sent END_STREAM. *)
  | Closed of closed_reason
      (** Terminal state. *)

(** Events that cause state transitions. *)
type event =
  | Send_headers of { end_stream : bool }
  | Recv_headers of { end_stream : bool }
  | Send_data of { end_stream : bool }
  | Recv_data of { end_stream : bool }
  | Send_push_promise
  | Recv_push_promise
  | Send_rst_stream of H2_frame.error_code
  | Recv_rst_stream of H2_frame.error_code
  | Send_end_stream
  | Recv_end_stream

(** Result of applying an event to a state. *)
type transition_result =
  | Transition_ok of state
  | Transition_error of H2_frame.error_code * string

(** A single HTTP/2 stream. *)
type t

(** {1 Stream Creation} *)

val default_initial_window_size : int
(** Default initial flow control window size: 65535 bytes. *)

val create : ?initial_send_window:int -> ?initial_recv_window:int -> int32 -> t
(** [create ?initial_send_window ?initial_recv_window id] creates a new stream.
    Stream starts in [Idle] state. *)

(** {1 Stream Properties} *)

val id : t -> int32
(** [id t] returns the stream identifier. *)

val state : t -> state
(** [state t] returns the current stream state. *)

val is_idle : t -> bool
(** [is_idle t] returns true if stream is in Idle state. *)

val is_active : t -> bool
(** [is_active t] returns true if stream is Open or Half-closed. *)

val is_open : t -> bool
(** [is_open t] returns true if stream is fully Open. *)

val is_closed : t -> bool
(** [is_closed t] returns true if stream is Closed. *)

val can_send : t -> bool
(** [can_send t] returns true if we can send frames on this stream. *)

val can_recv : t -> bool
(** [can_recv t] returns true if we can receive frames on this stream. *)

(** {1 State Transitions} *)

val transition : state -> event -> transition_result
(** [transition state event] computes the new state after applying [event].
    Returns [Ok new_state] or [Error (code, msg)] if transition is invalid. *)

val apply_event : t -> event -> (unit, H2_frame.error_code * string) result
(** [apply_event t event] applies [event] to stream [t], updating its state.
    Returns [Ok ()] on success or [Error (code, msg)] on protocol error. *)

val reset : t -> H2_frame.error_code -> unit
(** [reset t code] resets the stream with the given error code. *)

(** {1 Flow Control} *)

val send_window : t -> int
(** [send_window t] returns bytes available in send window. *)

val recv_window : t -> int
(** [recv_window t] returns bytes available in receive window. *)

val initial_recv_window : t -> int
(** [initial_recv_window t] returns the initial receive window size. *)

val consume_send_window : t -> int -> int
(** [consume_send_window t bytes] consumes up to [bytes] from send window.
    Returns number of bytes actually consumed. *)

val credit_send_window : t -> int -> (unit, H2_frame.error_code * string) result
(** [credit_send_window t increment] adds [increment] to send window.
    Returns error on overflow. *)

val consume_recv_window : t -> int -> unit
(** [consume_recv_window t bytes] consumes [bytes] from receive window. *)

val credit_recv_window : t -> int -> unit
(** [credit_recv_window t increment] adds [increment] to receive window. *)

val update_initial_window_size : t -> int -> (unit, H2_frame.error_code * string) result
(** [update_initial_window_size t new_size] updates the initial window size
    from a SETTINGS frame. Adjusts current window by the delta. *)

(** {1 Stream Identifiers} *)

val connection_stream_id : int32
(** Stream ID 0 is used for connection-level frames. *)

val is_client_initiated : int32 -> bool
(** [is_client_initiated id] returns true if [id] is odd (client-initiated). *)

val is_server_initiated : int32 -> bool
(** [is_server_initiated id] returns true if [id] is even and non-zero. *)

val is_valid_id : int32 -> bool
(** [is_valid_id id] returns true if [id] is greater than 0. *)

val is_connection_stream : int32 -> bool
(** [is_connection_stream id] returns true if [id] is 0. *)

(** {1 Headers} *)

val request_headers : t -> H2_hpack.header list option
(** [request_headers t] returns the request headers if set. *)

val set_request_headers : t -> H2_hpack.header list -> unit
(** [set_request_headers t headers] sets the request headers. *)

val response_headers : t -> H2_hpack.header list option
(** [response_headers t] returns the response headers if set. *)

val set_response_headers : t -> H2_hpack.header list -> unit
(** [set_response_headers t headers] sets the response headers. *)

val trailers : t -> H2_hpack.header list option
(** [trailers t] returns the trailers if set. *)

val set_trailers : t -> H2_hpack.header list -> unit
(** [set_trailers t headers] sets the trailers. *)

(** {1 Pretty Printing} *)

val pp_state : Format.formatter -> state -> unit
(** Pretty print stream state. *)

val pp_closed_reason : Format.formatter -> closed_reason -> unit
(** Pretty print closed reason. *)

val state_to_string : state -> string
(** Convert state to string. *)

val pp : Format.formatter -> t -> unit
(** Pretty print a stream. *)
