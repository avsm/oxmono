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

    Manages connection state including:
    - Settings negotiation
    - Stream multiplexing
    - Connection-level flow control
    - HPACK encoding/decoding context

    See {{:https://datatracker.ietf.org/doc/html/rfc9113}RFC 9113}. *)

(* ============================================================
   Connection Preface
   ============================================================ *)

(** HTTP/2 connection preface sent by client.
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" *)
let connection_preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

(** Length of connection preface: 24 bytes. *)
let connection_preface_length = String.length connection_preface

(** Check if buffer starts with the HTTP/2 connection preface. *)
let is_connection_preface buf =
  Cstruct.length buf >= connection_preface_length &&
  Cstruct.to_string ~off:0 ~len:connection_preface_length buf = connection_preface

(* ============================================================
   Settings
   ============================================================ *)

(** Connection settings per RFC 9113 Section 6.5.2. *)
type settings = {
  header_table_size : int;
      (** SETTINGS_HEADER_TABLE_SIZE (0x01): Default 4096. *)
  enable_push : bool;
      (** SETTINGS_ENABLE_PUSH (0x02): Default true for server, false for client. *)
  max_concurrent_streams : int option;
      (** SETTINGS_MAX_CONCURRENT_STREAMS (0x03): No limit by default. *)
  initial_window_size : int;
      (** SETTINGS_INITIAL_WINDOW_SIZE (0x04): Default 65535. *)
  max_frame_size : int;
      (** SETTINGS_MAX_FRAME_SIZE (0x05): Default 16384, max 16777215. *)
  max_header_list_size : int option;
      (** SETTINGS_MAX_HEADER_LIST_SIZE (0x06): No limit by default. *)
  no_rfc7540_priorities : bool;
      (** SETTINGS_NO_RFC7540_PRIORITIES (0x09): RFC 9113 deprecates RFC 7540 priorities. *)
}

(** Default settings per RFC 9113. *)
let default_settings = {
  header_table_size = 4096;
  enable_push = true;
  max_concurrent_streams = None;
  initial_window_size = 65535;
  max_frame_size = 16384;
  max_header_list_size = None;
  no_rfc7540_priorities = false;  (* RFC 7540 priorities enabled by default for compatibility *)
}

(** Client-appropriate default settings (no push). *)
let default_client_settings = {
  default_settings with
  enable_push = false;
}

(** Helper to create error tuple with correct type. *)
let protocol_error msg : H2_frame.error_code * string =
  (H2_frame.Protocol_error, msg)

let flow_control_error msg : H2_frame.error_code * string =
  (H2_frame.Flow_control_error, msg)

let refused_stream_error msg : H2_frame.error_code * string =
  (H2_frame.Refused_stream, msg)

(** Result bind operator for cleaner error handling. *)
let ( let* ) = Result.bind

(** Apply a single setting to settings.
    Returns updated settings or error. *)
let apply_single_setting s (setting : H2_frame.setting) =
  match setting with
  | H2_frame.Header_table_size v ->
      Ok { s with header_table_size = v }
  | H2_frame.Enable_push v ->
      Ok { s with enable_push = v }
  | H2_frame.Max_concurrent_streams v ->
      Ok { s with max_concurrent_streams = Some (Int32.to_int v) }
  | H2_frame.Initial_window_size v ->
      (* Values >= 2^31 are represented as negative Int32 *)
      if Int32.compare v 0l < 0 then
        Error (flow_control_error "Initial window size too large")
      else
        Ok { s with initial_window_size = Int32.to_int v }
  | H2_frame.Max_frame_size v ->
      if v < 16384 || v > 16777215 then
        Error (protocol_error "Invalid max frame size")
      else
        Ok { s with max_frame_size = v }
  | H2_frame.Max_header_list_size v ->
      Ok { s with max_header_list_size = Some v }
  | H2_frame.No_rfc7540_priorities v ->
      Ok { s with no_rfc7540_priorities = v }
  | H2_frame.Unknown_setting _ ->
      (* Unknown settings MUST be ignored per RFC 9113 *)
      Ok s

(** Apply a list of H2_frame.setting to settings.
    Returns updated settings or error. *)
let apply_setting_list settings setting_list =
  List.fold_left (fun acc setting ->
    let* s = acc in
    apply_single_setting s setting
  ) (Ok settings) setting_list

(** Apply a list of (setting_id, value) pairs to settings.
    Returns updated settings or error. *)
let apply_settings settings pairs =
  let settings_list = List.map (fun (id, value) ->
    H2_frame.setting_of_pair (Int32.to_int id) value
  ) pairs in
  apply_setting_list settings settings_list

(** Convert settings to a list of H2_frame.setting for encoding. *)
let settings_to_list settings =
  let open H2_frame in
  (* Required settings *)
  let required = [
    Header_table_size settings.header_table_size;
    Enable_push settings.enable_push;
    Initial_window_size (Int32.of_int settings.initial_window_size);
    Max_frame_size settings.max_frame_size;
  ] in
  (* Optional settings - only include if set *)
  let optional = List.filter_map Fun.id [
    Option.map (fun n -> Max_concurrent_streams (Int32.of_int n)) settings.max_concurrent_streams;
    Option.map (fun n -> Max_header_list_size n) settings.max_header_list_size;
  ] in
  optional @ required

(** Convert settings to a list of (setting_id, value) pairs for encoding. *)
let settings_to_pairs settings =
  let setting_list = settings_to_list settings in
  List.map H2_frame.setting_to_pair setting_list

(* ============================================================
   Connection Role
   ============================================================ *)

(** Role in the connection. *)
type role = Client | Server

(** Get next stream ID for this role. Client uses odd, server uses even. *)
let next_stream_id role current_id =
  match role with
  | Client ->
    if current_id = 0l then 1l
    else Int32.add current_id 2l
  | Server ->
    if current_id = 0l then 2l
    else Int32.add current_id 2l

(** Check if stream ID is valid for the given role to initiate. *)
let valid_stream_id_for_role role id =
  match role with
  | Client -> H2_stream.is_client_initiated id
  | Server -> H2_stream.is_server_initiated id

(* ============================================================
   Connection State
   ============================================================ *)

(** Connection state. *)
type state =
  | Handshaking
      (** Waiting for settings exchange. *)
  | Open
      (** Connection is active. *)
  | Closing of { last_stream_id : int32; error_code : H2_frame.error_code; debug : string }
      (** Sent or received GOAWAY, draining. *)
  | Closed
      (** Connection is closed. *)

(** Connection error. *)
type connection_error = {
  error_code : H2_frame.error_code;
  debug_data : string;
  stream_id : int32 option;
      (** If this was a stream error, the stream ID. *)
}

(* ============================================================
   Connection Type
   ============================================================ *)

(** An HTTP/2 connection. *)
type t = {
  role : role;
      (** Client or server. *)
  mutable state : state;
      (** Connection state. *)
  local_settings : settings;
      (** Our settings (sent to peer). *)
  mutable peer_settings : settings;
      (** Peer's settings (received from peer). *)
  mutable settings_ack_pending : bool;
      (** We sent SETTINGS and are waiting for ACK. *)
  hpack_encoder : H2_hpack.Encoder.t;
      (** HPACK encoder (shared for all streams). *)
  hpack_decoder : H2_hpack.Decoder.t;
      (** HPACK decoder (shared for all streams). *)
  streams : (int32, H2_stream.t) Hashtbl.t;
      (** Active streams by ID. *)
  mutable next_stream_id : int32;
      (** Next stream ID we will initiate. *)
  mutable last_peer_stream_id : int32;
      (** Highest stream ID initiated by peer. *)
  mutable send_window : int;
      (** Connection-level send window. *)
  mutable recv_window : int;
      (** Connection-level receive window. *)
  mutable max_send_window : int;
      (** Maximum send window (from peer's SETTINGS). *)
  mutable received_preface : bool;
      (** Have we received the connection preface? *)
  mutable sent_preface : bool;
      (** Have we sent the connection preface? *)
}

(** Create a new connection. *)
let create ?(settings = default_settings) role =
  let initial_settings = match role with
    | Client -> { settings with enable_push = false }
    | Server -> settings
  in
  let next_id = match role with
    | Client -> 1l
    | Server -> 2l
  in
  { role;
    state = Handshaking;
    local_settings = initial_settings;
    peer_settings = default_settings;
    settings_ack_pending = false;
    hpack_encoder = H2_hpack.Encoder.create initial_settings.header_table_size;
    hpack_decoder = H2_hpack.Decoder.create default_settings.header_table_size;
    streams = Hashtbl.create 16;
    next_stream_id = next_id;
    last_peer_stream_id = 0l;
    send_window = default_settings.initial_window_size;
    recv_window = default_settings.initial_window_size;
    max_send_window = default_settings.initial_window_size;
    received_preface = false;
    sent_preface = false;
  }

(* ============================================================
   Connection Properties
   ============================================================ *)

(** Get the connection role. *)
let role t = t.role

(** Get the connection state. *)
let state t = t.state

(** Check if connection is open. *)
let is_open t = match t.state with Open -> true | _ -> false

(** Check if connection is closing or closed. *)
let is_closing t = match t.state with
  | Closing _ | Closed -> true
  | _ -> false

(** Get our settings. *)
let local_settings t = t.local_settings

(** Get peer's settings. *)
let peer_settings t = t.peer_settings

(** Get number of active streams. *)
let active_stream_count t = Hashtbl.length t.streams

(** Get connection-level send window. *)
let send_window t = t.send_window

(** Get connection-level receive window. *)
let recv_window t = t.recv_window

(** Get maximum connection-level send window (from peer's settings). *)
let max_send_window t = t.max_send_window

(* ============================================================
   Stream Management
   ============================================================ *)

(** Get a stream by ID, or None if not found. *)
let get_stream t id = Hashtbl.find_opt t.streams id

(** Create a new stream initiated by us.
    Returns the stream or error if connection is closing. *)
let create_stream t =
  if is_closing t then
    Error (refused_stream_error "Connection is closing")
  else begin
    let id = t.next_stream_id in
    t.next_stream_id <- next_stream_id t.role id;
    let stream = H2_stream.create
      ~initial_send_window:t.peer_settings.initial_window_size
      ~initial_recv_window:t.local_settings.initial_window_size
      id in
    Hashtbl.add t.streams id stream;
    Ok stream
  end

(** Register a stream initiated by peer. *)
let register_peer_stream t id =
  if Int32.compare id t.last_peer_stream_id <= 0 then
    Error (protocol_error "Stream ID must be greater than previous")
  else if not (valid_stream_id_for_role (match t.role with Client -> Server | Server -> Client) id) then
    Error (protocol_error "Invalid stream ID for peer role")
  else begin
    t.last_peer_stream_id <- id;
    let stream = H2_stream.create
      ~initial_send_window:t.peer_settings.initial_window_size
      ~initial_recv_window:t.local_settings.initial_window_size
      id in
    Hashtbl.add t.streams id stream;
    Ok stream
  end

(** Remove a closed stream from the table. *)
let remove_stream t id =
  Hashtbl.remove t.streams id

(** Check if we've exceeded max concurrent streams. *)
let exceeds_max_concurrent_streams t =
  match t.peer_settings.max_concurrent_streams with
  | None -> false
  | Some max -> Hashtbl.length t.streams >= max

(** Iterate over all streams. *)
let iter_streams t f =
  Hashtbl.iter (fun _ stream -> f stream) t.streams

(* ============================================================
   Flow Control
   ============================================================ *)

(** Consume bytes from connection send window.
    Returns bytes actually consumed. *)
let consume_send_window t bytes =
  let available = min bytes t.send_window in
  t.send_window <- t.send_window - available;
  available

(** Credit connection send window (from WINDOW_UPDATE). *)
let credit_send_window t increment =
  let new_window = t.send_window + increment in
  if new_window > 0x7FFFFFFF then
    Error (flow_control_error "Connection window overflow")
  else begin
    t.send_window <- new_window;
    Ok ()
  end

(** Consume bytes from connection receive window. *)
let consume_recv_window t bytes =
  t.recv_window <- t.recv_window - bytes

(** Credit connection receive window. *)
let credit_recv_window t increment =
  t.recv_window <- t.recv_window + increment

(** Update stream windows after initial_window_size change in SETTINGS. *)
let update_stream_windows t new_initial_size =
  Hashtbl.fold (fun id stream errors ->
    match H2_stream.update_initial_window_size stream new_initial_size with
    | Ok () -> errors
    | Error e -> (id, e) :: errors
  ) t.streams []

(* ============================================================
   Settings Handling
   ============================================================ *)

(** Process received SETTINGS frame.
    Returns Ok () if successful, or connection error. *)
let handle_settings t ~ack pairs =
  if ack then begin
    if t.settings_ack_pending then begin
      t.settings_ack_pending <- false;
      Ok `Ack_received
    end else
      Error (protocol_error "Unexpected SETTINGS ACK")
  end else begin
    match apply_settings t.peer_settings pairs with
    | Ok new_settings ->
      (* Update HPACK decoder table size if changed *)
      if new_settings.header_table_size <> t.peer_settings.header_table_size then begin
        match H2_hpack.Decoder.set_capacity t.hpack_decoder new_settings.header_table_size with
        | Ok () -> ()
        | Error _ -> ()  (* Ignore decoder errors *)
      end;
      (* Update stream windows if initial_window_size changed *)
      let window_errors = update_stream_windows t new_settings.initial_window_size in
      if window_errors <> [] then
        (* Return first window error *)
        let (id, (code, msg)) = List.hd window_errors in
        Error (code, Printf.sprintf "Stream %ld: %s" id msg)
      else begin
        t.peer_settings <- new_settings;
        t.max_send_window <- new_settings.initial_window_size;
        Ok `Settings_received
      end
    | Error (code, msg) ->
      Error (code, msg)
  end

(** Mark that we've sent a SETTINGS frame. *)
let mark_settings_sent t =
  t.settings_ack_pending <- true

(* ============================================================
   HPACK Encoding/Decoding
   ============================================================ *)

(** Encode headers using the connection's HPACK encoder.
    Returns the encoded header block. *)
let encode_headers t headers =
  let buf = Cstruct.create (t.peer_settings.max_frame_size * 2) in
  let len = H2_hpack.Encoder.encode_headers t.hpack_encoder buf headers in
  Cstruct.sub buf 0 len

(** Decode headers using the connection's HPACK decoder. *)
let decode_headers t header_block =
  H2_hpack.Decoder.decode t.hpack_decoder header_block

(* ============================================================
   GOAWAY Handling
   ============================================================ *)

(** Initiate connection shutdown by sending GOAWAY. *)
let go_away t error_code debug =
  match t.state with
  | Closed -> ()
  | _ ->
    t.state <- Closing {
      last_stream_id = t.last_peer_stream_id;
      error_code;
      debug;
    }

(** Handle received GOAWAY frame. *)
let handle_goaway t ~last_stream_id ~error_code ~debug =
  t.state <- Closing { last_stream_id; error_code; debug }

(** Close the connection. *)
let close t =
  t.state <- Closed;
  Hashtbl.clear t.streams

(* ============================================================
   Preface Handling
   ============================================================ *)

(** Mark that we've sent the connection preface. *)
let mark_preface_sent t =
  t.sent_preface <- true

(** Mark that we've received the connection preface. *)
let mark_preface_received t =
  t.received_preface <- true;
  (* After preface exchange, connection is open *)
  if t.sent_preface && t.received_preface then
    t.state <- Open

(** Check if handshake is complete. *)
let handshake_complete t =
  t.sent_preface && t.received_preface && not t.settings_ack_pending

(* ============================================================
   Pretty Printing
   ============================================================ *)

let pp_role fmt = function
  | Client -> Format.fprintf fmt "Client"
  | Server -> Format.fprintf fmt "Server"

let pp_state fmt = function
  | Handshaking -> Format.fprintf fmt "Handshaking"
  | Open -> Format.fprintf fmt "Open"
  | Closing { last_stream_id; error_code; debug = _ } ->
    Format.fprintf fmt "Closing(last=%ld, error=%a)"
      last_stream_id H2_frame.pp_error_code error_code
  | Closed -> Format.fprintf fmt "Closed"

let pp fmt t =
  Format.fprintf fmt "Connection{role=%a; state=%a; streams=%d; send_window=%d}"
    pp_role t.role
    pp_state t.state
    (Hashtbl.length t.streams)
    t.send_window
