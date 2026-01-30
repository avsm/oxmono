(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP/2 Protocol Handler for Conpool.

    This module provides a protocol handler that enables HTTP/2 connection
    multiplexing through Conpool's typed pool API. It manages H2_client
    instances and supports concurrent stream multiplexing.

    Key features:
    - Shared connection mode (multiple streams per connection)
    - GOAWAY handling for graceful degradation
    - Stream slot management respecting peer's max_concurrent_streams *)

let src = Logs.Src.create "requests.h2_conpool" ~doc:"HTTP/2 Connection Pool Handler"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 State Type} *)

(** HTTP/2 connection state managed by Conpool.
    This wraps H2_client.t with additional tracking for stream management. *)
type h2_state = {
  client : H2_client.t;
      (** The underlying HTTP/2 client. *)
  flow : Conpool.Config.connection_flow;
      (** The connection flow (needed for H2 operations). *)
  sw : Eio.Switch.t;
      (** Connection-lifetime switch for the reader fiber. *)
  mutable reader_started : bool;
      (** Whether the background reader fiber has been started. *)
  mutable goaway_received : bool;
      (** Whether GOAWAY has been received from peer. *)
  mutable last_goaway_stream : int32;
      (** Last stream ID from GOAWAY (streams > this may be retried). *)
  mutable max_concurrent_streams : int;
      (** Cached max_concurrent_streams from peer settings. *)
}

(** {1 Protocol Configuration} *)

(** Initialize HTTP/2 state for a new connection.
    Performs the HTTP/2 handshake and extracts peer settings.
    The [sw] parameter is a connection-lifetime switch that will be used
    to spawn the background reader fiber when on_acquire is called. *)
let init_state ~sw ~flow ~tls_epoch:_ =
  Log.info (fun m -> m "Initializing HTTP/2 connection state");

  let client = H2_client.create () in

  (* Perform HTTP/2 handshake *)
  match H2_client.handshake flow client with
  | Ok () ->
      Log.info (fun m -> m "HTTP/2 handshake complete");

      (* Get max_concurrent_streams from peer settings *)
      let conn = H2_client.connection client in
      let peer_settings = H2_connection.peer_settings conn in
      let max_streams =
        match peer_settings.H2_connection.max_concurrent_streams with
        | Some n -> n
        | None -> 100  (* Default per RFC 9113 *)
      in

      Log.debug (fun m -> m "Peer max_concurrent_streams: %d" max_streams);

      {
        client;
        flow;
        sw;
        reader_started = false;
        goaway_received = false;
        last_goaway_stream = Int32.max_int;
        max_concurrent_streams = max_streams;
      }

  | Error msg ->
      Log.err (fun m -> m "HTTP/2 handshake failed: %s" msg);
      failwith ("HTTP/2 handshake failed: " ^ msg)

(** Called when a connection is acquired from the pool.
    Starts the background reader fiber on first acquisition. *)
let on_acquire state =
  if not state.reader_started then begin
    Log.info (fun m -> m "Starting HTTP/2 background reader fiber");
    H2_client.start_reader ~sw:state.sw state.flow state.client
      ~on_goaway:(fun ~last_stream_id ~error_code:_ ~debug ->
        Log.info (fun m -> m "GOAWAY received: last_stream_id=%ld, debug=%s"
          last_stream_id debug);
        state.goaway_received <- true;
        state.last_goaway_stream <- last_stream_id);
    state.reader_started <- true
  end

(** Called when a connection is released back to the pool.
    For HTTP/2, this is a no-op since the reader keeps running. *)
let on_release _state = ()

(** Check if the HTTP/2 connection is still healthy. *)
let is_healthy state =
  if state.goaway_received then begin
    Log.debug (fun m -> m "HTTP/2 connection unhealthy: GOAWAY received");
    false
  end else if not (H2_client.is_open state.client) then begin
    Log.debug (fun m -> m "HTTP/2 connection unhealthy: connection closed");
    false
  end else
    true

(** Cleanup when connection is destroyed. *)
let on_close state =
  Log.debug (fun m -> m "Closing HTTP/2 connection");
  (* Send GOAWAY if connection is still open *)
  if H2_client.is_open state.client then begin
    H2_client.close state.flow state.client
  end

(** Get access mode for this connection.
    HTTP/2 supports multiplexing: multiple concurrent streams per connection. *)
let access_mode state =
  Conpool.Config.Shared state.max_concurrent_streams

(** The protocol configuration for HTTP/2 connections. *)
let h2_protocol : h2_state Conpool.Config.protocol_config = {
  Conpool.Config.init_state;
  on_acquire;
  on_release;
  is_healthy;
  on_close;
  access_mode;
}

(** {1 Request Functions} *)

(** Make an HTTP/2 request using the pooled connection state.

    Uses the concurrent request path with the connection-lifetime reader fiber.
    Multiple requests can be made concurrently on the same connection.

    @param state The HTTP/2 state from Conpool
    @param uri Request URI
    @param headers Request headers
    @param body Optional request body
    @param method_ HTTP method
    @param auto_decompress Whether to decompress response body
    @return Response or error message *)
let request
    ~(state : h2_state)
    ~(uri : Uri.t)
    ~(headers : Headers.t)
    ?(body : Body.t option)
    ~(method_ : Method.t)
    ~auto_decompress
    ()
  : (H2_adapter.response, string) result =

  (* Validate HTTP/2 header constraints *)
  match Headers.validate_h2_user_headers headers with
  | Error e ->
      Error (Format.asprintf "Invalid HTTP/2 request headers: %a"
        Headers.pp_h2_validation_error e)
  | Ok () ->
      (* Check connection health before making request *)
      if state.goaway_received then
        Error "Connection received GOAWAY"
      else if not (H2_client.is_open state.client) then
        Error "Connection is closed"
      else begin
        let h2_headers = Headers.to_list headers in
        let h2_body = Option.bind body H2_adapter.body_to_string_opt in
        let meth = Method.to_string method_ in

        Log.debug (fun m -> m "Making HTTP/2 request on pooled connection: %s %s"
          meth (Uri.to_string uri));

        (* Use concurrent request path - reader fiber is already running *)
        match H2_client.request ~sw:state.sw state.flow state.client
                { H2_protocol.meth; uri; headers = h2_headers; body = h2_body } with
        | Ok resp ->
            (* Check if GOAWAY was received during request *)
            let conn = H2_client.connection state.client in
            if H2_connection.is_closing conn then begin
              Log.info (fun m -> m "GOAWAY received during request");
              state.goaway_received <- true
            end;

            (* Update max_concurrent_streams if it changed *)
            let peer_settings = H2_connection.peer_settings conn in
            (match peer_settings.H2_connection.max_concurrent_streams with
             | Some n when n <> state.max_concurrent_streams ->
                 Log.debug (fun m -> m "Peer max_concurrent_streams changed: %d -> %d"
                   state.max_concurrent_streams n);
                 state.max_concurrent_streams <- n
             | _ -> ());

            Ok (H2_adapter.make_response ~auto_decompress resp)

        | Error msg ->
            Log.warn (fun m -> m "HTTP/2 request failed: %s" msg);
            Error msg
      end
