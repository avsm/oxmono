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

(** Tests for H2_connection module. *)



let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

(* ============================================================
   Connection Preface Tests
   ============================================================ *)

let test_connection_preface () =
  Alcotest.(check int) "preface length" 24 H2_connection.connection_preface_length;
  Alcotest.(check string) "preface value"
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    H2_connection.connection_preface

let test_is_connection_preface () =
  let valid = Cstruct.of_string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
  Alcotest.(check bool) "valid preface" true
    (H2_connection.is_connection_preface valid);

  let with_extra = Cstruct.of_string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\nEXTRA" in
  Alcotest.(check bool) "valid with extra" true
    (H2_connection.is_connection_preface with_extra);

  let invalid = Cstruct.of_string "HTTP/1.1 200 OK\r\n" in
  Alcotest.(check bool) "invalid preface" false
    (H2_connection.is_connection_preface invalid);

  let too_short = Cstruct.of_string "PRI * HTTP" in
  Alcotest.(check bool) "too short" false
    (H2_connection.is_connection_preface too_short)

let preface_tests = [
  "preface constants", `Quick, test_connection_preface;
  "is_connection_preface", `Quick, test_is_connection_preface;
]

(* ============================================================
   Settings Tests
   ============================================================ *)

let test_default_settings () =
  let s = H2_connection.default_settings in
  Alcotest.(check int) "header_table_size" 4096 s.header_table_size;
  Alcotest.(check bool) "enable_push" true s.enable_push;
  Alcotest.(check (option int)) "max_concurrent_streams" None s.max_concurrent_streams;
  Alcotest.(check int) "initial_window_size" 65535 s.initial_window_size;
  Alcotest.(check int) "max_frame_size" 16384 s.max_frame_size;
  Alcotest.(check (option int)) "max_header_list_size" None s.max_header_list_size

let test_client_settings () =
  let s = H2_connection.default_client_settings in
  Alcotest.(check bool) "enable_push false" false s.enable_push

let test_apply_settings_header_table_size () =
  let s = H2_connection.default_settings in
  let pairs = [(1l, 8192l)] in  (* SETTINGS_HEADER_TABLE_SIZE = 1 *)
  match H2_connection.apply_settings s pairs with
  | Ok new_s ->
    Alcotest.(check int) "updated header_table_size" 8192 new_s.header_table_size
  | Error _ ->
    Alcotest.fail "apply_settings should succeed"

let test_apply_settings_initial_window_size () =
  let s = H2_connection.default_settings in
  let pairs = [(4l, 100000l)] in  (* SETTINGS_INITIAL_WINDOW_SIZE = 4 *)
  match H2_connection.apply_settings s pairs with
  | Ok new_s ->
    Alcotest.(check int) "updated initial_window_size" 100000 new_s.initial_window_size
  | Error _ ->
    Alcotest.fail "apply_settings should succeed"

let test_apply_settings_invalid_window_size () =
  let s = H2_connection.default_settings in
  (* Window size > 2^31-1 is invalid. A negative Int32 represents
     values >= 2^31 on the wire, which should be rejected. *)
  let pairs = [(4l, -1l)] in  (* -1l = 0xFFFFFFFF unsigned, > 2^31-1 *)
  match H2_connection.apply_settings s pairs with
  | Ok _ ->
    Alcotest.fail "should reject too large window size"
  | Error (code, _) ->
    Alcotest.(check bool) "flow control error"
      true (code = H2_frame.Flow_control_error)

let test_apply_settings_invalid_frame_size () =
  let s = H2_connection.default_settings in
  (* Frame size < 16384 is invalid *)
  let pairs = [(5l, 1000l)] in
  match H2_connection.apply_settings s pairs with
  | Ok _ ->
    Alcotest.fail "should reject too small frame size"
  | Error (code, _) ->
    Alcotest.(check bool) "protocol error"
      true (code = H2_frame.Protocol_error)

let test_settings_to_pairs () =
  let s = H2_connection.default_settings in
  let pairs = H2_connection.settings_to_pairs s in
  Alcotest.(check bool) "has pairs" true (List.length pairs > 0)

let settings_tests = [
  "default settings", `Quick, test_default_settings;
  "client settings", `Quick, test_client_settings;
  "apply header_table_size", `Quick, test_apply_settings_header_table_size;
  "apply initial_window_size", `Quick, test_apply_settings_initial_window_size;
  "invalid window size", `Quick, test_apply_settings_invalid_window_size;
  "invalid frame size", `Quick, test_apply_settings_invalid_frame_size;
  "settings_to_pairs", `Quick, test_settings_to_pairs;
]

(* ============================================================
   Connection Creation Tests
   ============================================================ *)

let test_create_client () =
  let conn = H2_connection.create H2_connection.Client in
  Alcotest.(check bool) "is client" true
    (H2_connection.role conn = H2_connection.Client);
  Alcotest.(check bool) "enable_push disabled" false
    (H2_connection.local_settings conn).enable_push;
  Alcotest.(check bool) "is handshaking" true
    (H2_connection.state conn = H2_connection.Handshaking)

let test_create_server () =
  let conn = H2_connection.create H2_connection.Server in
  Alcotest.(check bool) "is server" true
    (H2_connection.role conn = H2_connection.Server);
  Alcotest.(check bool) "is handshaking" true
    (H2_connection.state conn = H2_connection.Handshaking)

let test_connection_properties () =
  let conn = H2_connection.create H2_connection.Client in
  Alcotest.(check int) "active streams" 0 (H2_connection.active_stream_count conn);
  Alcotest.(check int) "send window" 65535 (H2_connection.send_window conn);
  Alcotest.(check int) "recv window" 65535 (H2_connection.recv_window conn);
  Alcotest.(check bool) "not open" false (H2_connection.is_open conn);
  Alcotest.(check bool) "not closing" false (H2_connection.is_closing conn)

let creation_tests = [
  "create client", `Quick, test_create_client;
  "create server", `Quick, test_create_server;
  "connection properties", `Quick, test_connection_properties;
]

(* ============================================================
   Stream ID Tests
   ============================================================ *)

let test_next_stream_id_client () =
  Alcotest.(check int32) "first client stream" 1l
    (H2_connection.next_stream_id H2_connection.Client 0l);
  Alcotest.(check int32) "second client stream" 3l
    (H2_connection.next_stream_id H2_connection.Client 1l);
  Alcotest.(check int32) "third client stream" 5l
    (H2_connection.next_stream_id H2_connection.Client 3l)

let test_next_stream_id_server () =
  Alcotest.(check int32) "first server stream" 2l
    (H2_connection.next_stream_id H2_connection.Server 0l);
  Alcotest.(check int32) "second server stream" 4l
    (H2_connection.next_stream_id H2_connection.Server 2l)

let test_valid_stream_id_for_role () =
  Alcotest.(check bool) "client can use odd" true
    (H2_connection.valid_stream_id_for_role H2_connection.Client 1l);
  Alcotest.(check bool) "client cannot use even" false
    (H2_connection.valid_stream_id_for_role H2_connection.Client 2l);
  Alcotest.(check bool) "server can use even" true
    (H2_connection.valid_stream_id_for_role H2_connection.Server 2l);
  Alcotest.(check bool) "server cannot use odd" false
    (H2_connection.valid_stream_id_for_role H2_connection.Server 1l)

let stream_id_tests = [
  "next stream id client", `Quick, test_next_stream_id_client;
  "next stream id server", `Quick, test_next_stream_id_server;
  "valid stream id for role", `Quick, test_valid_stream_id_for_role;
]

(* ============================================================
   Stream Management Tests
   ============================================================ *)

let test_create_stream () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.create_stream conn with
  | Ok stream ->
    Alcotest.(check int32) "first stream id" 1l (H2_stream.id stream);
    Alcotest.(check int) "active count" 1 (H2_connection.active_stream_count conn)
  | Error _ ->
    Alcotest.fail "create_stream should succeed"

let test_create_multiple_streams () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.create_stream conn with
  | Error _ -> Alcotest.fail "first stream should succeed"
  | Ok s1 ->
    Alcotest.(check int32) "first id" 1l (H2_stream.id s1);
    match H2_connection.create_stream conn with
    | Error _ -> Alcotest.fail "second stream should succeed"
    | Ok s2 ->
      Alcotest.(check int32) "second id" 3l (H2_stream.id s2);
      Alcotest.(check int) "count" 2 (H2_connection.active_stream_count conn)

let test_get_stream () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.create_stream conn with
  | Error _ -> Alcotest.fail "create should succeed"
  | Ok stream ->
    let id = H2_stream.id stream in
    match H2_connection.get_stream conn id with
    | Some s -> Alcotest.(check int32) "same stream" id (H2_stream.id s)
    | None -> Alcotest.fail "should find stream"

let test_get_nonexistent_stream () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.get_stream conn 999l with
  | None -> ()
  | Some _ -> Alcotest.fail "should not find stream"

let test_remove_stream () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.create_stream conn with
  | Error _ -> Alcotest.fail "create should succeed"
  | Ok stream ->
    let id = H2_stream.id stream in
    H2_connection.remove_stream conn id;
    Alcotest.(check int) "count after remove" 0
      (H2_connection.active_stream_count conn);
    match H2_connection.get_stream conn id with
    | None -> ()
    | Some _ -> Alcotest.fail "should not find removed stream"

let test_register_peer_stream () =
  let conn = H2_connection.create H2_connection.Client in
  (* Server would initiate stream 2 *)
  match H2_connection.register_peer_stream conn 2l with
  | Ok stream ->
    Alcotest.(check int32) "peer stream id" 2l (H2_stream.id stream);
    Alcotest.(check int) "count" 1 (H2_connection.active_stream_count conn)
  | Error (_, msg) ->
    Alcotest.fail (Printf.sprintf "register should succeed: %s" msg)

let test_register_peer_stream_invalid_id () =
  let conn = H2_connection.create H2_connection.Client in
  (* Client cannot receive odd stream IDs from peer *)
  match H2_connection.register_peer_stream conn 3l with
  | Ok _ -> Alcotest.fail "should reject odd stream for client"
  | Error (code, _) ->
    Alcotest.(check bool) "protocol error" true (code = H2_frame.Protocol_error)

let test_iter_streams () =
  let conn = H2_connection.create H2_connection.Client in
  ignore (H2_connection.create_stream conn);
  ignore (H2_connection.create_stream conn);
  ignore (H2_connection.create_stream conn);
  let count = ref 0 in
  H2_connection.iter_streams conn (fun _ -> incr count);
  Alcotest.(check int) "iterated 3 streams" 3 !count

let stream_management_tests = [
  "create stream", `Quick, test_create_stream;
  "create multiple streams", `Quick, test_create_multiple_streams;
  "get stream", `Quick, test_get_stream;
  "get nonexistent stream", `Quick, test_get_nonexistent_stream;
  "remove stream", `Quick, test_remove_stream;
  "register peer stream", `Quick, test_register_peer_stream;
  "register peer stream invalid", `Quick, test_register_peer_stream_invalid_id;
  "iter streams", `Quick, test_iter_streams;
]

(* ============================================================
   Flow Control Tests
   ============================================================ *)

let test_consume_send_window () =
  let conn = H2_connection.create H2_connection.Client in
  let consumed = H2_connection.consume_send_window conn 1000 in
  Alcotest.(check int) "consumed" 1000 consumed;
  Alcotest.(check int) "remaining" (65535 - 1000) (H2_connection.send_window conn)

let test_consume_send_window_exceeds () =
  let conn = H2_connection.create H2_connection.Client in
  let consumed = H2_connection.consume_send_window conn 100000 in
  Alcotest.(check int) "consumed only available" 65535 consumed;
  Alcotest.(check int) "window at 0" 0 (H2_connection.send_window conn)

let test_credit_send_window () =
  let conn = H2_connection.create H2_connection.Client in
  ignore (H2_connection.consume_send_window conn 10000);
  match H2_connection.credit_send_window conn 5000 with
  | Ok () ->
    Alcotest.(check int) "credited" (65535 - 10000 + 5000)
      (H2_connection.send_window conn)
  | Error _ ->
    Alcotest.fail "credit should succeed"

let test_credit_send_window_overflow () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.credit_send_window conn 0x7FFFFFFF with
  | Ok () -> Alcotest.fail "should reject overflow"
  | Error (code, _) ->
    Alcotest.(check bool) "flow control error" true
      (code = H2_frame.Flow_control_error)

let test_recv_window () =
  let conn = H2_connection.create H2_connection.Client in
  H2_connection.consume_recv_window conn 1000;
  Alcotest.(check int) "consumed" (65535 - 1000) (H2_connection.recv_window conn);
  H2_connection.credit_recv_window conn 500;
  Alcotest.(check int) "credited" (65535 - 1000 + 500) (H2_connection.recv_window conn)

let flow_control_tests = [
  "consume send window", `Quick, test_consume_send_window;
  "consume send window exceeds", `Quick, test_consume_send_window_exceeds;
  "credit send window", `Quick, test_credit_send_window;
  "credit send window overflow", `Quick, test_credit_send_window_overflow;
  "recv window", `Quick, test_recv_window;
]

(* ============================================================
   Settings Handling Tests
   ============================================================ *)

let test_handle_settings () =
  let conn = H2_connection.create H2_connection.Client in
  let pairs = [(1l, 8192l)] in  (* header_table_size = 8192 *)
  match H2_connection.handle_settings conn ~ack:false pairs with
  | Ok `Settings_received ->
    Alcotest.(check int) "peer settings updated" 8192
      (H2_connection.peer_settings conn).header_table_size
  | Ok `Ack_received ->
    Alcotest.fail "should be settings, not ack"
  | Error (_, msg) ->
    Alcotest.fail (Printf.sprintf "handle_settings failed: %s" msg)

let test_handle_settings_ack () =
  let conn = H2_connection.create H2_connection.Client in
  H2_connection.mark_settings_sent conn;
  match H2_connection.handle_settings conn ~ack:true [] with
  | Ok `Ack_received -> ()
  | Ok `Settings_received ->
    Alcotest.fail "should be ack"
  | Error (_, msg) ->
    Alcotest.fail (Printf.sprintf "should accept ack: %s" msg)

let test_handle_unexpected_ack () =
  let conn = H2_connection.create H2_connection.Client in
  (* Don't mark settings as sent *)
  match H2_connection.handle_settings conn ~ack:true [] with
  | Ok _ -> Alcotest.fail "should reject unexpected ack"
  | Error (code, _) ->
    Alcotest.(check bool) "protocol error" true (code = H2_frame.Protocol_error)

let settings_handling_tests = [
  "handle settings", `Quick, test_handle_settings;
  "handle settings ack", `Quick, test_handle_settings_ack;
  "unexpected ack", `Quick, test_handle_unexpected_ack;
]

(* ============================================================
   HPACK Tests
   ============================================================ *)

let test_encode_decode_headers () =
  let conn = H2_connection.create H2_connection.Client in
  let headers = [
    { H2_hpack.name = ":method"; value = "GET"; sensitive = false };
    { H2_hpack.name = ":path"; value = "/"; sensitive = false };
  ] in
  let encoded = H2_connection.encode_headers conn headers in
  match H2_connection.decode_headers conn encoded with
  | Ok decoded ->
    Alcotest.(check int) "header count" 2 (List.length decoded)
  | Error e ->
    Alcotest.fail (Format.asprintf "decode failed: %a" H2_hpack.pp_error e)

let hpack_tests = [
  "encode decode headers", `Quick, test_encode_decode_headers;
]

(* ============================================================
   GOAWAY Tests
   ============================================================ *)

let test_go_away () =
  let conn = H2_connection.create H2_connection.Client in
  H2_connection.mark_preface_sent conn;
  H2_connection.mark_preface_received conn;
  Alcotest.(check bool) "is open" true (H2_connection.is_open conn);
  H2_connection.go_away conn H2_frame.No_error "shutdown";
  Alcotest.(check bool) "is closing" true (H2_connection.is_closing conn)

let test_handle_goaway () =
  let conn = H2_connection.create H2_connection.Client in
  H2_connection.handle_goaway conn ~last_stream_id:5l
    ~error_code:H2_frame.No_error ~debug:"bye";
  Alcotest.(check bool) "is closing" true (H2_connection.is_closing conn)

let test_close () =
  let conn = H2_connection.create H2_connection.Client in
  ignore (H2_connection.create_stream conn);
  Alcotest.(check int) "has stream" 1 (H2_connection.active_stream_count conn);
  H2_connection.close conn;
  Alcotest.(check bool) "is closed" true
    (match H2_connection.state conn with H2_connection.Closed -> true | _ -> false);
  Alcotest.(check int) "streams cleared" 0 (H2_connection.active_stream_count conn)

let test_create_stream_when_closing () =
  let conn = H2_connection.create H2_connection.Client in
  H2_connection.go_away conn H2_frame.No_error "shutdown";
  match H2_connection.create_stream conn with
  | Ok _ -> Alcotest.fail "should reject new streams when closing"
  | Error (code, _) ->
    Alcotest.(check bool) "refused stream" true (code = H2_frame.Refused_stream)

let goaway_tests = [
  "go_away", `Quick, test_go_away;
  "handle_goaway", `Quick, test_handle_goaway;
  "close", `Quick, test_close;
  "create stream when closing", `Quick, test_create_stream_when_closing;
]

(* ============================================================
   Preface/Handshake Tests
   ============================================================ *)

let test_handshake_complete () =
  let conn = H2_connection.create H2_connection.Client in
  Alcotest.(check bool) "not complete initially" false
    (H2_connection.handshake_complete conn);
  H2_connection.mark_preface_sent conn;
  Alcotest.(check bool) "not complete after sent" false
    (H2_connection.handshake_complete conn);
  H2_connection.mark_preface_received conn;
  Alcotest.(check bool) "is open" true (H2_connection.is_open conn);
  (* Still waiting for settings ack *)
  H2_connection.mark_settings_sent conn;
  Alcotest.(check bool) "not complete waiting ack" false
    (H2_connection.handshake_complete conn);
  ignore (H2_connection.handle_settings conn ~ack:true []);
  Alcotest.(check bool) "complete after ack" true
    (H2_connection.handshake_complete conn)

let handshake_tests = [
  "handshake complete", `Quick, test_handshake_complete;
]

(* ============================================================
   Pretty Printing Tests
   ============================================================ *)

let test_pp_role () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  H2_connection.pp_role fmt H2_connection.Client;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "client" "Client" (Buffer.contents buf);
  Buffer.clear buf;
  H2_connection.pp_role fmt H2_connection.Server;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "server" "Server" (Buffer.contents buf)

let test_pp_state () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  H2_connection.pp_state fmt H2_connection.Handshaking;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "handshaking" "Handshaking" (Buffer.contents buf)

let test_pp_connection () =
  let conn = H2_connection.create H2_connection.Client in
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  H2_connection.pp fmt conn;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "contains Connection" true (String.length s > 0)

let pp_tests = [
  "pp_role", `Quick, test_pp_role;
  "pp_state", `Quick, test_pp_state;
  "pp connection", `Quick, test_pp_connection;
]

(* ============================================================
   Run Tests
   ============================================================ *)

let () =
  Alcotest.run "H2_connection" [
    "preface", preface_tests;
    "settings", settings_tests;
    "creation", creation_tests;
    "stream_id", stream_id_tests;
    "stream_management", stream_management_tests;
    "flow_control", flow_control_tests;
    "settings_handling", settings_handling_tests;
    "hpack", hpack_tests;
    "goaway", goaway_tests;
    "handshake", handshake_tests;
    "pp", pp_tests;
  ]
