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

    Implements the stream lifecycle:
    - idle -> open -> half-closed -> closed
    - With reserved states for server push (PUSH_PROMISE)

    See {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.1}RFC 9113 Section 5.1}. *)

(* ============================================================
   Stream States
   ============================================================ *)

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
      (** Initial state. Transitions to open on send/recv HEADERS,
          or to reserved on PUSH_PROMISE. *)
  | Reserved_local
      (** We sent PUSH_PROMISE. Can only send HEADERS (-> half_closed_remote)
          or RST_STREAM (-> closed). *)
  | Reserved_remote
      (** Peer sent PUSH_PROMISE. Can only receive HEADERS (-> half_closed_local)
          or send/recv RST_STREAM (-> closed). *)
  | Open
      (** Both sides can send frames. Transitions to half_closed on END_STREAM,
          or closed on RST_STREAM. *)
  | Half_closed_local
      (** We sent END_STREAM. We can only receive frames.
          Transitions to closed on recv END_STREAM or RST_STREAM. *)
  | Half_closed_remote
      (** Peer sent END_STREAM. We can only send frames.
          Transitions to closed on send END_STREAM or RST_STREAM. *)
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

(* ============================================================
   State Transition Logic
   ============================================================ *)

(** [transition state event] applies [event] to [state] and returns
    the new state or an error.

    Implements RFC 9113 Section 5.1 state machine. *)
let transition state event =
  match state, event with
  (* === Idle state transitions === *)
  | Idle, Send_headers { end_stream = false } ->
    Transition_ok Open
  | Idle, Send_headers { end_stream = true } ->
    Transition_ok Half_closed_local
  | Idle, Recv_headers { end_stream = false } ->
    Transition_ok Open
  | Idle, Recv_headers { end_stream = true } ->
    Transition_ok Half_closed_remote
  | Idle, Send_push_promise ->
    Transition_ok Reserved_local
  | Idle, Recv_push_promise ->
    Transition_ok Reserved_remote
  | Idle, _ ->
    Transition_error (H2_frame.Protocol_error,
           "Invalid frame on idle stream")

  (* === Reserved (local) state transitions === *)
  | Reserved_local, Send_headers { end_stream = false } ->
    Transition_ok Half_closed_remote
  | Reserved_local, Send_headers { end_stream = true } ->
    (* Send HEADERS with END_STREAM on reserved -> closed *)
    Transition_ok (Closed Finished)
  | Reserved_local, Send_rst_stream code ->
    Transition_ok (Closed (ResetByUs code))
  | Reserved_local, Recv_rst_stream code ->
    Transition_ok (Closed (ResetByThem code))
  | Reserved_local, _ ->
    Transition_error (H2_frame.Protocol_error,
           "Invalid frame on reserved (local) stream")

  (* === Reserved (remote) state transitions === *)
  | Reserved_remote, Recv_headers { end_stream = false } ->
    Transition_ok Half_closed_local
  | Reserved_remote, Recv_headers { end_stream = true } ->
    (* Recv HEADERS with END_STREAM on reserved -> closed *)
    Transition_ok (Closed Finished)
  | Reserved_remote, Send_rst_stream code ->
    Transition_ok (Closed (ResetByUs code))
  | Reserved_remote, Recv_rst_stream code ->
    Transition_ok (Closed (ResetByThem code))
  | Reserved_remote, _ ->
    Transition_error (H2_frame.Protocol_error,
           "Invalid frame on reserved (remote) stream")

  (* === Open state transitions === *)
  | Open, Send_headers { end_stream = true } ->
    Transition_ok Half_closed_local
  | Open, Send_headers { end_stream = false } ->
    Transition_ok Open (* Trailers without END_STREAM, unusual but valid *)
  | Open, Recv_headers { end_stream = true } ->
    Transition_ok Half_closed_remote
  | Open, Recv_headers { end_stream = false } ->
    Transition_ok Open (* Trailers without END_STREAM *)
  | Open, Send_data { end_stream = true } ->
    Transition_ok Half_closed_local
  | Open, Send_data { end_stream = false } ->
    Transition_ok Open
  | Open, Recv_data { end_stream = true } ->
    Transition_ok Half_closed_remote
  | Open, Recv_data { end_stream = false } ->
    Transition_ok Open
  | Open, Send_end_stream ->
    Transition_ok Half_closed_local
  | Open, Recv_end_stream ->
    Transition_ok Half_closed_remote
  | Open, Send_rst_stream code ->
    Transition_ok (Closed (ResetByUs code))
  | Open, Recv_rst_stream code ->
    Transition_ok (Closed (ResetByThem code))
  | Open, Send_push_promise | Open, Recv_push_promise ->
    (* PUSH_PROMISE is sent on an existing stream but creates a new reserved stream *)
    Transition_ok Open

  (* === Half-closed (local) state transitions === *)
  | Half_closed_local, Recv_headers { end_stream = true } ->
    Transition_ok (Closed Finished)
  | Half_closed_local, Recv_headers { end_stream = false } ->
    Transition_ok Half_closed_local
  | Half_closed_local, Recv_data { end_stream = true } ->
    Transition_ok (Closed Finished)
  | Half_closed_local, Recv_data { end_stream = false } ->
    Transition_ok Half_closed_local
  | Half_closed_local, Recv_end_stream ->
    Transition_ok (Closed Finished)
  | Half_closed_local, Send_rst_stream code ->
    Transition_ok (Closed (ResetByUs code))
  | Half_closed_local, Recv_rst_stream code ->
    Transition_ok (Closed (ResetByThem code))
  | Half_closed_local, (Send_headers _ | Send_data _ | Send_end_stream) ->
    Transition_error (H2_frame.Stream_closed,
           "Cannot send on half-closed (local) stream")
  | Half_closed_local, _ ->
    Transition_ok Half_closed_local (* WINDOW_UPDATE, PRIORITY allowed *)

  (* === Half-closed (remote) state transitions === *)
  | Half_closed_remote, Send_headers { end_stream = true } ->
    Transition_ok (Closed Finished)
  | Half_closed_remote, Send_headers { end_stream = false } ->
    Transition_ok Half_closed_remote
  | Half_closed_remote, Send_data { end_stream = true } ->
    Transition_ok (Closed Finished)
  | Half_closed_remote, Send_data { end_stream = false } ->
    Transition_ok Half_closed_remote
  | Half_closed_remote, Send_end_stream ->
    Transition_ok (Closed Finished)
  | Half_closed_remote, Send_rst_stream code ->
    Transition_ok (Closed (ResetByUs code))
  | Half_closed_remote, Recv_rst_stream code ->
    Transition_ok (Closed (ResetByThem code))
  | Half_closed_remote, (Recv_headers _ | Recv_data _ | Recv_end_stream) ->
    Transition_error (H2_frame.Stream_closed,
           "Received data on half-closed (remote) stream")
  | Half_closed_remote, _ ->
    Transition_ok Half_closed_remote (* WINDOW_UPDATE, PRIORITY allowed *)

  (* === Closed state - terminal === *)
  | Closed reason, Recv_rst_stream _ ->
    (* Can receive RST_STREAM on closed stream (race condition) *)
    Transition_ok (Closed reason)
  | Closed _, _ ->
    Transition_error (H2_frame.Stream_closed,
           "Stream is closed")

(* ============================================================
   Stream Type
   ============================================================ *)

(** Flow control state for a stream. *)
type flow_control = {
  mutable send_window : int;
      (** Bytes we're allowed to send. *)
  mutable recv_window : int;
      (** Bytes we've advertised we can receive. *)
  initial_send_window : int;
      (** Initial send window from SETTINGS. *)
  initial_recv_window : int;
      (** Initial receive window from SETTINGS. *)
}

(** A single HTTP/2 stream. *)
type t = {
  id : int32;
      (** Stream identifier (odd = client-initiated, even = server-initiated). *)
  mutable state : state;
      (** Current stream state. *)
  flow : flow_control;
      (** Flow control windows. *)
  mutable request_headers : H2_hpack.header list option;
      (** Request headers once received/sent. *)
  mutable response_headers : H2_hpack.header list option;
      (** Response headers once received/sent. *)
  mutable trailers : H2_hpack.header list option;
      (** Trailing headers. *)
}

(** Default initial flow control window size per RFC 9113. *)
let default_initial_window_size = 65535

(** Create a new stream with the given ID. *)
let create ?(initial_send_window = default_initial_window_size)
           ?(initial_recv_window = default_initial_window_size)
           id =
  { id;
    state = Idle;
    flow = {
      send_window = initial_send_window;
      recv_window = initial_recv_window;
      initial_send_window;
      initial_recv_window;
    };
    request_headers = None;
    response_headers = None;
    trailers = None;
  }

(** Get the stream ID. *)
let id t = t.id

(** Get the current state. *)
let state t = t.state

(** Helper for state predicate checks. *)
let state_matches pred t = pred t.state

(** Check if stream is in idle state. *)
let is_idle = state_matches (function Idle -> true | _ -> false)

(** Check if stream is open (including half-closed). *)
let is_active = state_matches (function
  | Open | Half_closed_local | Half_closed_remote -> true
  | _ -> false)

(** Check if stream is fully open (both directions). *)
let is_open = state_matches (function Open -> true | _ -> false)

(** Check if stream is closed. *)
let is_closed = state_matches (function Closed _ -> true | _ -> false)

(** Check if we can send on this stream. *)
let can_send = state_matches (function
  | Open | Half_closed_remote | Reserved_local -> true
  | _ -> false)

(** Check if we can receive on this stream. *)
let can_recv = state_matches (function
  | Open | Half_closed_local | Reserved_remote -> true
  | _ -> false)

(** Apply an event to the stream, updating its state.
    Returns Ok () on success, or Error with an error code and message. *)
let apply_event t event =
  match transition t.state event with
  | Transition_ok new_state ->
    t.state <- new_state;
    Ok ()
  | Transition_error (code, msg) ->
    Error (code, msg)

(* ============================================================
   Flow Control
   ============================================================ *)

(** Consume bytes from the send window.
    Returns the number of bytes actually consumed (may be less if window exhausted). *)
let consume_send_window t bytes =
  let available = min bytes t.flow.send_window in
  t.flow.send_window <- t.flow.send_window - available;
  available

(** Add bytes to the send window (from WINDOW_UPDATE). *)
let credit_send_window t increment =
  let new_window = t.flow.send_window + increment in
  if new_window > 0x7FFFFFFF then
    Error (H2_frame.Flow_control_error,
           "Flow control window overflow")
  else begin
    t.flow.send_window <- new_window;
    Ok ()
  end

(** Consume bytes from the receive window.
    Call this when receiving DATA frames. *)
let consume_recv_window t bytes =
  t.flow.recv_window <- t.flow.recv_window - bytes

(** Credit the receive window (we're sending WINDOW_UPDATE). *)
let credit_recv_window t increment =
  t.flow.recv_window <- t.flow.recv_window + increment

(** Get available send window. *)
let send_window t = t.flow.send_window

(** Get available receive window. *)
let recv_window t = t.flow.recv_window

(** Get initial receive window size. *)
let initial_recv_window t = t.flow.initial_recv_window

(** Update initial window size (from SETTINGS).
    Adjusts the current window by the delta. *)
let update_initial_window_size t new_initial_size =
  let delta = new_initial_size - t.flow.initial_send_window in
  let new_window = t.flow.send_window + delta in
  if new_window > 0x7FFFFFFF || new_window < 0 then
    Error (H2_frame.Flow_control_error,
           "Flow control window overflow after SETTINGS update")
  else begin
    t.flow.send_window <- new_window;
    Ok ()
  end

(* ============================================================
   Stream Identifier Management
   ============================================================ *)

(** Check if stream ID is client-initiated (odd). *)
let is_client_initiated id = Int32.(logand id 1l = 1l)

(** Check if stream ID is server-initiated (even, non-zero). *)
let is_server_initiated id = Int32.(id > 0l && logand id 1l = 0l)

(** Check if stream ID is valid (non-zero). *)
let is_valid_id id = Int32.compare id 0l > 0

(** Connection-level stream ID is 0. *)
let connection_stream_id = 0l

(** Check if this is the connection-level stream. *)
let is_connection_stream id = Int32.equal id 0l

(* ============================================================
   Pretty Printing
   ============================================================ *)

let pp_closed_reason fmt = function
  | Finished -> Format.fprintf fmt "Finished"
  | ResetByUs code ->
    Format.fprintf fmt "ResetByUs(%a)" H2_frame.pp_error_code code
  | ResetByThem code ->
    Format.fprintf fmt "ResetByThem(%a)" H2_frame.pp_error_code code

let pp_state fmt = function
  | Idle -> Format.fprintf fmt "Idle"
  | Reserved_local -> Format.fprintf fmt "Reserved(local)"
  | Reserved_remote -> Format.fprintf fmt "Reserved(remote)"
  | Open -> Format.fprintf fmt "Open"
  | Half_closed_local -> Format.fprintf fmt "HalfClosed(local)"
  | Half_closed_remote -> Format.fprintf fmt "HalfClosed(remote)"
  | Closed reason -> Format.fprintf fmt "Closed(%a)" pp_closed_reason reason

let state_to_string state =
  Format.asprintf "%a" pp_state state

let pp fmt t =
  Format.fprintf fmt "Stream{id=%ld; state=%a; send_window=%d; recv_window=%d}"
    t.id pp_state t.state t.flow.send_window t.flow.recv_window

(* ============================================================
   Headers Management
   ============================================================ *)

(** Set request headers. *)
let set_request_headers t headers = t.request_headers <- Some headers

(** Get request headers. *)
let request_headers t = t.request_headers

(** Set response headers. *)
let set_response_headers t headers = t.response_headers <- Some headers

(** Get response headers. *)
let response_headers t = t.response_headers

(** Set trailers. *)
let set_trailers t headers = t.trailers <- Some headers

(** Get trailers. *)
let trailers t = t.trailers

(** Reset the stream with an error code. *)
let reset t code =
  ignore (apply_event t (Send_rst_stream code))
