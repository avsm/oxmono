(*---------------------------------------------------------------------------
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

(** HTTP/2 Client with Eio-based Concurrent Dispatch.

    This implementation supports true HTTP/2 multiplexing by using:
    - A centralized frame reader fiber that dispatches to stream handlers
    - Per-stream Eio.Stream queues for frame delivery
    - Eio.Promise for stream completion signaling

    Multiple concurrent requests can share a single HTTP/2 connection. *)

let src = Logs.Src.create "h2.client" ~doc:"HTTP/2 Client"
module Log = (val Logs.src_log src : Logs.LOG)

(** Result bind operator for cleaner error handling. *)
let ( let* ) = Result.bind

(* ============================================================
   Frame Types for Dispatch
   ============================================================ *)

(** Events dispatched to stream handlers. *)
type stream_event =
  | Headers of { headers : H2_hpack.header list; end_stream : bool }
  | Data of { data : Cstruct.t; end_stream : bool }
  | Rst_stream of H2_frame.error_code
  | Window_update of int
  | Connection_error of string

(* ============================================================
   Per-Stream Handler
   ============================================================ *)

(** State for a single stream's response handling. *)
type stream_handler = {
  stream : H2_stream.t;
  events : stream_event Eio.Stream.t;  (** Events for this stream *)
}

let create_stream_handler stream =
  {
    stream;
    events = Eio.Stream.create 64;  (* Buffer up to 64 events per stream *)
  }

(* ============================================================
   Client State
   ============================================================ *)

type t = {
  conn : H2_connection.t;
  handlers : (int32, stream_handler) Hashtbl.t;
  handlers_mutex : Eio.Mutex.t;
  mutable reader_running : bool;
  mutable connection_error : string option;
  connection_error_mutex : Eio.Mutex.t;
}

let create ?settings () =
  let conn = H2_connection.create ?settings H2_connection.Client in
  {
    conn;
    handlers = Hashtbl.create 16;
    handlers_mutex = Eio.Mutex.create ();
    reader_running = false;
    connection_error = None;
    connection_error_mutex = Eio.Mutex.create ();
  }

(* ============================================================
   Frame I/O (same as before, but factored out)
   ============================================================ *)

let write_frame flow frame =
  let buf = H2_frame.serialize_frame frame in
  Log.debug (fun m -> m "Writing frame: %a (%d bytes)"
    H2_frame.pp_frame_type frame.H2_frame.header.frame_type
    (Cstruct.length buf));
  Eio.Flow.copy_string (Cstruct.to_string buf) flow

let write_preface flow conn =
  Log.debug (fun m -> m "Sending connection preface");
  Eio.Flow.copy_string H2_connection.connection_preface flow;

  let settings_pairs = H2_connection.settings_to_pairs (H2_connection.local_settings conn) in
  let settings_list = List.map (fun (id, value) ->
    H2_frame.setting_of_pair id value
  ) settings_pairs in
  let settings_frame = H2_frame.make_settings settings_list in
  write_frame flow settings_frame;
  H2_connection.mark_settings_sent conn;
  H2_connection.mark_preface_sent conn

let read_exactly flow n =
  let buf = Cstruct.create n in
  let rec loop off remaining =
    if remaining = 0 then ()
    else begin
      let sub = Cstruct.sub buf off remaining in
      let got = Eio.Flow.single_read flow sub in
      loop (off + got) (remaining - got)
    end
  in
  loop 0 n;
  buf

let read_frame flow =
  try
    let header_buf = read_exactly flow 9 in
    let length = (Cstruct.get_uint8 header_buf 0 lsl 16)
               lor (Cstruct.get_uint8 header_buf 1 lsl 8)
               lor (Cstruct.get_uint8 header_buf 2) in

    Log.debug (fun m -> m "Reading frame: length=%d" length);

    let payload = if length > 0 then read_exactly flow length else Cstruct.empty in
    let frame_buf = Cstruct.concat [header_buf; payload] in

    match H2_frame.parse_frame frame_buf ~max_frame_size:H2_frame.default_max_frame_size with
    | Ok (frame, _) ->
        Log.debug (fun m -> m "Received frame: %a on stream %ld"
          H2_frame.pp_frame_type frame.H2_frame.header.frame_type
          frame.H2_frame.header.stream_id);
        Some frame
    | Error e ->
        Log.err (fun m -> m "Frame parse error: %a" H2_frame.pp_parse_error e);
        None
  with
  | End_of_file -> None
  | Eio.Io _ -> None
  | Eio.Cancel.Cancelled _ -> None

let send_settings_ack flow =
  let ack_frame = H2_frame.make_settings ~ack:true [] in
  write_frame flow ack_frame

let send_window_update flow ~stream_id ~increment =
  let frame = H2_frame.make_window_update ~stream_id (Int32.of_int increment) in
  write_frame flow frame

let send_ping_ack flow data =
  let frame = H2_frame.make_ping ~ack:true data in
  write_frame flow frame

(** Helper for testing end_stream flag. *)
let is_end_stream flags = H2_frame.Flags.test flags H2_frame.Flags.end_stream

(** Helper for testing ack flag. *)
let is_ack flags = H2_frame.Flags.test flags H2_frame.Flags.ack

(* ============================================================
   Connection Handshake (blocking, before reader starts)
   ============================================================ *)

let handshake flow t =
  Log.info (fun m -> m "Starting HTTP/2 handshake");
  write_preface flow t.conn;

  let rec wait_for_handshake () =
    if H2_connection.handshake_complete t.conn then begin
      Log.info (fun m -> m "HTTP/2 handshake complete");
      Ok ()
    end else begin
      match read_frame flow with
      | None ->
          Error "Connection closed during handshake"
      | Some frame ->
          let flags = frame.H2_frame.header.flags in
          match frame.H2_frame.payload with
          | H2_frame.Settings_payload settings ->
              let pairs = List.map H2_frame.setting_to_pair settings in
              let pairs32 = List.map (fun (id, v) -> (Int32.of_int id, v)) pairs in
              (match H2_connection.handle_settings t.conn ~ack:(is_ack flags) pairs32 with
               | Ok `Settings_received ->
                   Log.debug (fun m -> m "Received server SETTINGS, sending ACK");
                   send_settings_ack flow;
                   H2_connection.mark_preface_received t.conn;
                   wait_for_handshake ()
               | Ok `Ack_received ->
                   Log.debug (fun m -> m "Received SETTINGS ACK");
                   wait_for_handshake ()
               | Error (_, msg) ->
                   Error ("Settings error: " ^ msg))
          | H2_frame.Ping_payload data ->
              if is_ack flags then
                Log.debug (fun m -> m "Received PING ACK")
              else begin
                Log.debug (fun m -> m "Received PING, sending ACK");
                send_ping_ack flow data
              end;
              wait_for_handshake ()
          | H2_frame.Window_update_payload increment ->
              let inc = Int32.to_int increment in
              Log.debug (fun m -> m "Received WINDOW_UPDATE: %d" inc);
              (match H2_connection.credit_send_window t.conn inc with
               | Ok () -> wait_for_handshake ()
               | Error (_, msg) -> Error ("Window update error: " ^ msg))
          | H2_frame.Goaway_payload { last_stream_id; error_code; debug_data } ->
              let debug = Cstruct.to_string debug_data in
              Log.warn (fun m -> m "Received GOAWAY: last_stream=%ld, error=%a, debug=%s"
                last_stream_id H2_frame.pp_error_code error_code debug);
              Error ("Server sent GOAWAY: " ^ debug)
          | _ ->
              Log.debug (fun m -> m "Ignoring frame during handshake: %a"
                H2_frame.pp_frame_type frame.H2_frame.header.frame_type);
              wait_for_handshake ()
    end
  in
  wait_for_handshake ()

(* ============================================================
   Frame Dispatch to Stream Handlers
   ============================================================ *)

(** Dispatch a frame to the appropriate stream handler. *)
let dispatch_frame t flow frame =
  let stream_id = frame.H2_frame.header.stream_id in
  let flags = frame.H2_frame.header.flags in

  (* Connection-level frames (stream 0) *)
  if Int32.equal stream_id 0l then begin
    match frame.H2_frame.payload with
    | H2_frame.Settings_payload settings ->
        let pairs = List.map H2_frame.setting_to_pair settings in
        let pairs32 = List.map (fun (id, v) -> (Int32.of_int id, v)) pairs in
        (match H2_connection.handle_settings t.conn ~ack:(is_ack flags) pairs32 with
         | Ok `Settings_received ->
             Log.debug (fun m -> m "Received SETTINGS, sending ACK");
             send_settings_ack flow;
             `Continue
         | Ok `Ack_received ->
             Log.debug (fun m -> m "Received SETTINGS ACK");
             `Continue
         | Error (_, msg) ->
             Log.err (fun m -> m "Settings error: %s" msg);
             `Error msg)

    | H2_frame.Ping_payload data ->
        if not (is_ack flags) then begin
          Log.debug (fun m -> m "Received PING, sending ACK");
          send_ping_ack flow data
        end;
        `Continue

    | H2_frame.Window_update_payload increment ->
        let inc = Int32.to_int increment in
        Log.debug (fun m -> m "Connection WINDOW_UPDATE: %d" inc);
        (match H2_connection.credit_send_window t.conn inc with
         | Ok () -> `Continue
         | Error (_, msg) ->
             Log.err (fun m -> m "Window update error: %s" msg);
             `Error msg)

    | H2_frame.Goaway_payload { last_stream_id; error_code; debug_data } ->
        let debug = Cstruct.to_string debug_data in
        Log.warn (fun m -> m "Received GOAWAY: last_stream=%ld, error=%a, debug=%s"
          last_stream_id H2_frame.pp_error_code error_code debug);
        H2_connection.handle_goaway t.conn ~last_stream_id ~error_code ~debug;
        (* Notify all active streams about the connection error *)
        Eio.Mutex.use_ro t.handlers_mutex (fun () ->
          Hashtbl.iter (fun _id handler ->
            Eio.Stream.add handler.events (Connection_error ("GOAWAY: " ^ debug))
          ) t.handlers
        );
        `Goaway (last_stream_id, error_code, debug)

    | _ ->
        Log.debug (fun m -> m "Ignoring connection-level frame: %a"
          H2_frame.pp_frame_type frame.H2_frame.header.frame_type);
        `Continue
  end
  else begin
    (* Stream-specific frame - dispatch to handler *)
    let handler_opt = Eio.Mutex.use_ro t.handlers_mutex (fun () ->
      Hashtbl.find_opt t.handlers stream_id
    ) in

    match handler_opt with
    | None ->
        Log.debug (fun m -> m "Received frame for unknown stream %ld, ignoring"
          stream_id);
        `Continue

    | Some handler ->
        let event = match frame.H2_frame.payload with
          | H2_frame.Headers_payload { header_block; _ } ->
              (match H2_connection.decode_headers t.conn header_block with
               | Ok headers ->
                   Some (Headers { headers; end_stream = is_end_stream flags })
               | Error _ ->
                   Log.err (fun m -> m "Failed to decode headers on stream %ld" stream_id);
                   Some (Connection_error "Header decode failed"))

          | H2_frame.Data_payload { data } ->
              (* Handle flow control *)
              let data_len = Cstruct.length data in
              if data_len > 0 then begin
                H2_connection.consume_recv_window t.conn data_len;
                H2_stream.consume_recv_window handler.stream data_len;
                send_window_update flow ~stream_id:0l ~increment:data_len;
                send_window_update flow ~stream_id ~increment:data_len;
                H2_connection.credit_recv_window t.conn data_len;
                H2_stream.credit_recv_window handler.stream data_len
              end;
              Some (Data { data; end_stream = is_end_stream flags })

          | H2_frame.Rst_stream_payload error_code ->
              Some (Rst_stream error_code)

          | H2_frame.Window_update_payload increment ->
              let inc = Int32.to_int increment in
              (match H2_stream.credit_send_window handler.stream inc with
               | Ok () -> Some (Window_update inc)
               | Error (_, msg) ->
                   Log.warn (fun m -> m "Stream %ld window update error: %s" stream_id msg);
                   None)

          | _ ->
              Log.debug (fun m -> m "Ignoring frame %a on stream %ld"
                H2_frame.pp_frame_type frame.H2_frame.header.frame_type stream_id);
              None
        in

        (match event with
         | Some e -> Eio.Stream.add handler.events e
         | None -> ());

        `Continue
  end

(* ============================================================
   Background Frame Reader Fiber
   ============================================================ *)

(** Start the background frame reader.
    This runs in a fiber and dispatches frames to stream handlers.

    @param sw Switch to spawn the reader fiber on (should be connection-lifetime)
    @param flow The underlying connection flow
    @param t Client state
    @param on_goaway Optional callback invoked when GOAWAY is received *)
let start_reader ~sw flow t ~on_goaway =
  if t.reader_running then
    ()  (* Already running *)
  else begin
    t.reader_running <- true;
    (* Use fork_daemon so the reader doesn't prevent switch completion.
       The reader will be automatically cancelled when the switch completes. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
      (try
        Log.debug (fun m -> m "Frame reader fiber started");
        let rec read_loop () =
          match read_frame flow with
          | None ->
              Log.info (fun m -> m "Frame reader: connection closed");
              Eio.Mutex.use_rw ~protect:true t.connection_error_mutex (fun () ->
                if t.connection_error = None then
                  t.connection_error <- Some "Connection closed"
              );
              (* Notify all handlers about connection close *)
              Eio.Mutex.use_ro t.handlers_mutex (fun () ->
                Hashtbl.iter (fun _id handler ->
                  Eio.Stream.add handler.events (Connection_error "Connection closed")
                ) t.handlers
              )

          | Some frame ->
              match dispatch_frame t flow frame with
              | `Continue -> read_loop ()
              | `Goaway (last_stream_id, error_code, debug) ->
                  (* Call the GOAWAY callback if provided *)
                  on_goaway ~last_stream_id ~error_code ~debug;
                  (* Continue reading to drain any remaining frames *)
                  read_loop ()
              | `Error msg ->
                  Log.err (fun m -> m "Frame reader error: %s" msg);
                  Eio.Mutex.use_rw ~protect:true t.connection_error_mutex (fun () ->
                    t.connection_error <- Some msg
                  );
                  (* Notify all handlers *)
                  Eio.Mutex.use_ro t.handlers_mutex (fun () ->
                    Hashtbl.iter (fun _id handler ->
                      Eio.Stream.add handler.events (Connection_error msg)
                    ) t.handlers
                  )
        in
        read_loop ();
        t.reader_running <- false;
        Log.debug (fun m -> m "Frame reader fiber stopped")
      with
      | Eio.Cancel.Cancelled _ ->
          Log.debug (fun m -> m "Frame reader fiber cancelled")
      | exn ->
          Log.err (fun m -> m "Frame reader fiber error: %s" (Printexc.to_string exn)));
      `Stop_daemon
    )
  end

(* ============================================================
   Request/Response with Concurrent Dispatch
   ============================================================ *)

(** Response accumulator for a stream. *)
type pending_response = {
  mutable status : int option;
  mutable headers : (string * string) list;
  mutable body_parts : string list;
  mutable done_ : bool;
  mutable error : string option;
}

(** Make a request and wait for its response.
    This can be called concurrently from multiple fibers. *)
let request ~sw flow t (req : H2_protocol.request) =
  (* Check for connection errors first *)
  (match Eio.Mutex.use_ro t.connection_error_mutex (fun () -> t.connection_error) with
   | Some err -> Error ("Connection error: " ^ err)
   | None ->

  Log.info (fun m -> m "Sending HTTP/2 request: %s %s"
    req.meth (Uri.to_string req.uri));

  (* Ensure reader is running - use no-op callback for ad-hoc usage *)
  start_reader ~sw flow t ~on_goaway:(fun ~last_stream_id:_ ~error_code:_ ~debug:_ -> ());

  (* Create a new stream *)
  match H2_connection.create_stream t.conn with
  | Error (_, msg) ->
      Error ("Failed to create stream: " ^ msg)

  | Ok stream ->
      let stream_id = H2_stream.id stream in
      Log.debug (fun m -> m "Created stream %ld" stream_id);

      (* Create and register the stream handler *)
      let handler = create_stream_handler stream in
      Eio.Mutex.use_rw ~protect:true t.handlers_mutex (fun () ->
        Hashtbl.add t.handlers stream_id handler
      );

      (* Send request headers and body *)
      let h2_headers = H2_protocol.request_to_h2_headers req in
      let header_block = H2_connection.encode_headers t.conn h2_headers in

      let has_body = Option.is_some req.body in
      let end_stream_on_headers = not has_body in

      let headers_frame = H2_frame.make_headers
        ~stream_id
        ~end_stream:end_stream_on_headers
        ~end_headers:true
        header_block
      in
      write_frame flow headers_frame;

      let _ = H2_stream.apply_event stream
        (H2_stream.Send_headers { end_stream = end_stream_on_headers }) in

      (match req.body with
       | None -> ()
       | Some body ->
           let data = Cstruct.of_string body in
           let data_frame = H2_frame.make_data ~stream_id ~end_stream:true data in
           write_frame flow data_frame;
           let _ = H2_stream.apply_event stream
             (H2_stream.Send_data { end_stream = true }) in
           ());

      (* Wait for response by consuming events from the stream's queue *)
      let pending = {
        status = None;
        headers = [];
        body_parts = [];
        done_ = false;
        error = None;
      } in

      let rec wait_for_response () =
        if pending.done_ then
          ()
        else begin
          let event = Eio.Stream.take handler.events in
          match event with
          | Headers { headers; end_stream } ->
              Log.debug (fun m -> m "Stream %ld: received HEADERS (end_stream=%b)"
                stream_id end_stream);
              let status, hdrs = H2_protocol.h2_headers_to_response headers in
              pending.status <- Some status;
              pending.headers <- hdrs;
              let _ = H2_stream.apply_event stream
                (H2_stream.Recv_headers { end_stream }) in
              if end_stream then pending.done_ <- true
              else wait_for_response ()

          | Data { data; end_stream } ->
              Log.debug (fun m -> m "Stream %ld: received DATA (%d bytes, end_stream=%b)"
                stream_id (Cstruct.length data) end_stream);
              pending.body_parts <- Cstruct.to_string data :: pending.body_parts;
              let _ = H2_stream.apply_event stream
                (H2_stream.Recv_data { end_stream }) in
              if end_stream then pending.done_ <- true
              else wait_for_response ()

          | Rst_stream error_code ->
              Log.warn (fun m -> m "Stream %ld: reset with %a"
                stream_id H2_frame.pp_error_code error_code);
              let _ = H2_stream.apply_event stream
                (H2_stream.Recv_rst_stream error_code) in
              pending.error <- Some (Printf.sprintf "Stream reset: %s"
                (H2_frame.error_code_to_string error_code));
              pending.done_ <- true

          | Window_update _ ->
              (* Window updates are informational for requests *)
              wait_for_response ()

          | Connection_error msg ->
              Log.warn (fun m -> m "Stream %ld: connection error: %s"
                stream_id msg);
              pending.error <- Some msg;
              pending.done_ <- true
        end
      in

      wait_for_response ();

      (* Unregister the handler *)
      Eio.Mutex.use_rw ~protect:true t.handlers_mutex (fun () ->
        Hashtbl.remove t.handlers stream_id
      );

      (* Return result *)
      match pending.error with
      | Some err -> Error err
      | None ->
          match pending.status with
          | None -> Error "No response status received"
          | Some status ->
              let body = String.concat "" (List.rev pending.body_parts) in
              Ok { H2_protocol.
                status;
                headers = pending.headers;
                body;
                protocol = H2_protocol.Http2;
              })

(* ============================================================
   Synchronous Request (for one-shot requests without multiplexing)
   ============================================================ *)

(** Make a single request synchronously without spawning a background reader.
    This is more efficient for one-shot requests since it doesn't require
    fiber management. *)
let request_sync flow t (req : H2_protocol.request) =
  (* Check for connection errors first *)
  match Eio.Mutex.use_ro t.connection_error_mutex (fun () -> t.connection_error) with
  | Some err -> Error ("Connection error: " ^ err)
  | None ->

  Log.info (fun m -> m "Sending HTTP/2 request (sync): %s %s"
    req.meth (Uri.to_string req.uri));

  (* Create a new stream *)
  match H2_connection.create_stream t.conn with
  | Error (_, msg) ->
      Error ("Failed to create stream: " ^ msg)

  | Ok stream ->
      let stream_id = H2_stream.id stream in
      Log.debug (fun m -> m "Created stream %ld" stream_id);

      (* Send request headers and body *)
      let h2_headers = H2_protocol.request_to_h2_headers req in
      let header_block = H2_connection.encode_headers t.conn h2_headers in

      let has_body = Option.is_some req.body in
      let end_stream_on_headers = not has_body in

      let headers_frame = H2_frame.make_headers
        ~stream_id
        ~end_stream:end_stream_on_headers
        ~end_headers:true
        header_block
      in
      write_frame flow headers_frame;

      let _ = H2_stream.apply_event stream
        (H2_stream.Send_headers { end_stream = end_stream_on_headers }) in

      (match req.body with
       | None -> ()
       | Some body ->
           let data = Cstruct.of_string body in
           let data_frame = H2_frame.make_data ~stream_id ~end_stream:true data in
           write_frame flow data_frame;
           let _ = H2_stream.apply_event stream
             (H2_stream.Send_data { end_stream = true }) in
           ());

      (* Read response synchronously *)
      let pending = {
        status = None;
        headers = [];
        body_parts = [];
        done_ = false;
        error = None;
      } in

      let rec read_response () =
        if pending.done_ then ()
        else begin
          match read_frame flow with
          | None ->
              pending.error <- Some "Connection closed";
              pending.done_ <- true

          | Some frame ->
              let frame_stream_id = frame.H2_frame.header.stream_id in
              let flags = frame.H2_frame.header.flags in

              (* Handle connection-level frames *)
              if Int32.equal frame_stream_id 0l then begin
                (match frame.H2_frame.payload with
                 | H2_frame.Settings_payload settings when not (is_ack flags) ->
                     let pairs = List.map H2_frame.setting_to_pair settings in
                     let pairs32 = List.map (fun (id, v) -> (Int32.of_int id, v)) pairs in
                     let _ = H2_connection.handle_settings t.conn ~ack:false pairs32 in
                     send_settings_ack flow
                 | H2_frame.Ping_payload data when not (is_ack flags) ->
                     send_ping_ack flow data
                 | H2_frame.Window_update_payload increment ->
                     let _ = H2_connection.credit_send_window t.conn (Int32.to_int increment) in
                     ()
                 | H2_frame.Goaway_payload { last_stream_id; error_code; debug_data } ->
                     let debug = Cstruct.to_string debug_data in
                     H2_connection.handle_goaway t.conn ~last_stream_id ~error_code ~debug;
                     pending.error <- Some ("GOAWAY: " ^ debug);
                     pending.done_ <- true
                 | _ -> ());
                if not pending.done_ then read_response ()
              end
              (* Handle frames for our stream *)
              else if Int32.equal frame_stream_id stream_id then begin
                (match frame.H2_frame.payload with
                 | H2_frame.Headers_payload { header_block; _ } ->
                     (match H2_connection.decode_headers t.conn header_block with
                      | Ok headers ->
                          let status, hdrs = H2_protocol.h2_headers_to_response headers in
                          pending.status <- Some status;
                          pending.headers <- hdrs;
                          let _ = H2_stream.apply_event stream
                            (H2_stream.Recv_headers { end_stream = is_end_stream flags }) in
                          if is_end_stream flags then pending.done_ <- true
                      | Error _ ->
                          pending.error <- Some "Header decode failed";
                          pending.done_ <- true)

                 | H2_frame.Data_payload { data } ->
                     let data_len = Cstruct.length data in
                     if data_len > 0 then begin
                       H2_connection.consume_recv_window t.conn data_len;
                       H2_stream.consume_recv_window stream data_len;
                       send_window_update flow ~stream_id:0l ~increment:data_len;
                       send_window_update flow ~stream_id ~increment:data_len;
                       H2_connection.credit_recv_window t.conn data_len;
                       H2_stream.credit_recv_window stream data_len
                     end;
                     pending.body_parts <- Cstruct.to_string data :: pending.body_parts;
                     let _ = H2_stream.apply_event stream
                       (H2_stream.Recv_data { end_stream = is_end_stream flags }) in
                     if is_end_stream flags then pending.done_ <- true

                 | H2_frame.Rst_stream_payload error_code ->
                     let _ = H2_stream.apply_event stream
                       (H2_stream.Recv_rst_stream error_code) in
                     pending.error <- Some (Printf.sprintf "Stream reset: %s"
                       (H2_frame.error_code_to_string error_code));
                     pending.done_ <- true

                 | H2_frame.Window_update_payload increment ->
                     let _ = H2_stream.credit_send_window stream (Int32.to_int increment) in
                     ()

                 | _ -> ());
                if not pending.done_ then read_response ()
              end
              (* Ignore frames for other streams *)
              else read_response ()
        end
      in

      read_response ();

      (* Return result *)
      match pending.error with
      | Some err -> Error err
      | None ->
          match pending.status with
          | None -> Error "No response status received"
          | Some status ->
              let body = String.concat "" (List.rev pending.body_parts) in
              Ok { H2_protocol.
                status;
                headers = pending.headers;
                body;
                protocol = H2_protocol.Http2;
              }

(* ============================================================
   High-level API
   ============================================================ *)

let one_request ~sw:_ flow ~meth ~uri ?headers ?body () =
  let t = create () in
  let* () = handshake flow t in
  let headers = Option.value headers ~default:[] in
  let req = H2_protocol.make_request ~meth ~uri ~headers ?body () in
  (* Use synchronous request for one-shot - no background reader needed *)
  request_sync flow t req

let one_request_strings ~sw:_ flow ~meth ~scheme ~host ?port ?path ?(query=[]) ?headers ?body () =
  let t = create () in
  let* () = handshake flow t in
  let headers = Option.value headers ~default:[] in
  let req = H2_protocol.make_request_from_strings ~meth ~scheme ~host ?port ?path ~query ~headers ?body () in
  (* Use synchronous request for one-shot - no background reader needed *)
  request_sync flow t req

let is_open t = H2_connection.is_open t.conn

let connection t = t.conn

let close flow t =
  if not (H2_connection.is_closing t.conn) then begin
    Log.debug (fun m -> m "Sending GOAWAY");
    H2_connection.go_away t.conn H2_frame.No_error "client closing";
    let goaway_frame = H2_frame.make_goaway
      ~last_stream_id:0l
      H2_frame.No_error
      ~debug:"client closing"
      ()
    in
    write_frame flow goaway_frame
  end;
  H2_connection.close t.conn

(** Get number of active stream handlers.
    Useful for connection pool management. *)
let active_streams t =
  Eio.Mutex.use_ro t.handlers_mutex (fun () ->
    Hashtbl.length t.handlers
  )
