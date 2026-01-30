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

(** Tests for H2_client module. *)



let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

(* ============================================================
   Client Creation Tests
   ============================================================ *)

let test_create_client () =
  let client = H2_client.create () in
  let conn = H2_client.connection client in
  (* A newly created client starts in Handshaking state, not Open.
     It becomes Open after handshake completes. *)
  Alcotest.(check bool) "client not closing" false (H2_connection.is_closing conn);
  Alcotest.(check bool) "role is client"
    true (H2_connection.role conn = H2_connection.Client)

let test_create_client_with_settings () =
  let settings = H2_connection.{
    header_table_size = 8192;
    enable_push = false;
    max_concurrent_streams = Some 50;
    initial_window_size = 32768;
    max_frame_size = 32768;
    max_header_list_size = Some 16384;
    no_rfc7540_priorities = true;  (* RFC 9113: Disable deprecated priorities *)
  } in
  let client = H2_client.create ~settings () in
  let conn = H2_client.connection client in
  let local = H2_connection.local_settings conn in
  Alcotest.(check int) "header_table_size" 8192 local.header_table_size;
  Alcotest.(check bool) "enable_push" false local.enable_push;
  Alcotest.(check int) "initial_window_size" 32768 local.initial_window_size;
  Alcotest.(check int) "max_frame_size" 32768 local.max_frame_size

let test_connection_accessor () =
  let client = H2_client.create () in
  let conn = H2_client.connection client in
  Alcotest.(check bool) "is client role"
    true (H2_connection.role conn = H2_connection.Client)

let creation_tests = [
  "create client", `Quick, test_create_client;
  "create client with settings", `Quick, test_create_client_with_settings;
  "connection accessor", `Quick, test_connection_accessor;
]

(* ============================================================
   Frame Serialization Tests
   ============================================================ *)

let test_settings_frame_serialize () =
  let settings = [
    H2_frame.Initial_window_size 65535l;
    H2_frame.Max_frame_size 16384;
  ] in
  let frame = H2_frame.make_settings settings in
  let buf = H2_frame.serialize_frame frame in
  Alcotest.(check bool) "frame serialized" true (Cstruct.length buf > 9)

let test_ping_frame_serialize () =
  let data = Cstruct.of_string "12345678" in
  let frame = H2_frame.make_ping data in
  let buf = H2_frame.serialize_frame frame in
  (* 9 byte header + 8 byte payload *)
  Alcotest.(check int) "ping frame size" 17 (Cstruct.length buf)

let test_goaway_frame_serialize () =
  let frame = H2_frame.make_goaway
    ~last_stream_id:1l
    H2_frame.No_error
    ~debug:"goodbye"
    ()
  in
  let buf = H2_frame.serialize_frame frame in
  Alcotest.(check bool) "goaway frame serialized" true (Cstruct.length buf > 9)

let test_headers_frame_serialize () =
  let header_block = Cstruct.of_string "\x82\x86\x84" in (* example HPACK *)
  let frame = H2_frame.make_headers
    ~stream_id:1l
    ~end_stream:true
    ~end_headers:true
    header_block
  in
  let buf = H2_frame.serialize_frame frame in
  Alcotest.(check bool) "headers frame serialized" true (Cstruct.length buf > 9)

let test_data_frame_serialize () =
  let data = Cstruct.of_string "Hello, HTTP/2!" in
  let frame = H2_frame.make_data ~stream_id:1l ~end_stream:true data in
  let buf = H2_frame.serialize_frame frame in
  (* 9 byte header + data length *)
  Alcotest.(check int) "data frame size" (9 + 14) (Cstruct.length buf)

let test_window_update_frame_serialize () =
  let frame = H2_frame.make_window_update ~stream_id:0l 65535l in
  let buf = H2_frame.serialize_frame frame in
  (* 9 byte header + 4 byte payload *)
  Alcotest.(check int) "window update frame size" 13 (Cstruct.length buf)

let frame_tests = [
  "settings frame serialize", `Quick, test_settings_frame_serialize;
  "ping frame serialize", `Quick, test_ping_frame_serialize;
  "goaway frame serialize", `Quick, test_goaway_frame_serialize;
  "headers frame serialize", `Quick, test_headers_frame_serialize;
  "data frame serialize", `Quick, test_data_frame_serialize;
  "window update frame serialize", `Quick, test_window_update_frame_serialize;
]

(* ============================================================
   Protocol Tests
   ============================================================ *)

let test_request_headers_conversion () =
  let uri = Uri.of_string "https://example.com/api/v1" in
  let req = H2_protocol.make_request ~meth:"POST" ~uri
    ~headers:[("Content-Type", "application/json"); ("Authorization", "Bearer token")]
    ~body:"{}"
    () in
  let headers = H2_protocol.request_to_h2_headers req in

  (* Check pseudo-headers *)
  let find name = List.find_opt (fun (h : H2_hpack.header) -> h.name = name) headers in
  Alcotest.(check bool) "has :method"
    true (Option.is_some (find ":method"));
  Alcotest.(check bool) "has :scheme"
    true (Option.is_some (find ":scheme"));
  Alcotest.(check bool) "has :authority"
    true (Option.is_some (find ":authority"));
  Alcotest.(check bool) "has :path"
    true (Option.is_some (find ":path"));

  (* Check values *)
  Alcotest.(check (option string)) ":method value"
    (Some "POST") (Option.map (fun (h : H2_hpack.header) -> h.value) (find ":method"));
  Alcotest.(check (option string)) ":path value"
    (Some "/api/v1") (Option.map (fun (h : H2_hpack.header) -> h.value) (find ":path"))

let test_response_headers_conversion () =
  let headers = [
    { H2_hpack.name = ":status"; value = "201"; sensitive = false };
    { H2_hpack.name = "content-type"; value = "application/json"; sensitive = false };
    { H2_hpack.name = "location"; value = "/resource/123"; sensitive = false };
  ] in
  let status, response_headers = H2_protocol.h2_headers_to_response headers in
  Alcotest.(check int) "status code" 201 status;
  Alcotest.(check int) "header count" 2 (List.length response_headers);
  Alcotest.(check (option (pair string string))) "content-type header"
    (Some ("content-type", "application/json"))
    (List.find_opt (fun (k, _) -> k = "content-type") response_headers)

let test_sensitive_headers () =
  let uri = Uri.of_string "https://example.com/" in
  let req = H2_protocol.make_request ~meth:"GET" ~uri
    ~headers:[
      ("Cookie", "session=abc123");
      ("Authorization", "Bearer secret");
      ("X-Custom", "not-sensitive");
    ]
    () in
  let headers = H2_protocol.request_to_h2_headers req in

  let find name = List.find_opt (fun (h : H2_hpack.header) -> h.name = name) headers in
  Alcotest.(check (option bool)) "cookie is sensitive"
    (Some true) (Option.map (fun (h : H2_hpack.header) -> h.sensitive) (find "cookie"));
  Alcotest.(check (option bool)) "authorization is sensitive"
    (Some true) (Option.map (fun (h : H2_hpack.header) -> h.sensitive) (find "authorization"));
  Alcotest.(check (option bool)) "x-custom is not sensitive"
    (Some false) (Option.map (fun (h : H2_hpack.header) -> h.sensitive) (find "x-custom"))

let protocol_tests = [
  "request headers conversion", `Quick, test_request_headers_conversion;
  "response headers conversion", `Quick, test_response_headers_conversion;
  "sensitive headers", `Quick, test_sensitive_headers;
]

(* ============================================================
   HPACK Encoding Tests
   ============================================================ *)

let test_encode_decode_request () =
  let conn = H2_connection.create H2_connection.Client in
  let headers = [
    { H2_hpack.name = ":method"; value = "GET"; sensitive = false };
    { H2_hpack.name = ":scheme"; value = "https"; sensitive = false };
    { H2_hpack.name = ":authority"; value = "example.com"; sensitive = false };
    { H2_hpack.name = ":path"; value = "/"; sensitive = false };
  ] in
  let encoded = H2_connection.encode_headers conn headers in
  Alcotest.(check bool) "encoded non-empty" true (Cstruct.length encoded > 0);

  (* Decode on "server" side *)
  let server_conn = H2_connection.create H2_connection.Server in
  match H2_connection.decode_headers server_conn encoded with
  | Ok decoded ->
      Alcotest.(check int) "decoded header count" 4 (List.length decoded)
  | Error _ ->
      Alcotest.fail "failed to decode headers"

let test_encode_decode_response () =
  let conn = H2_connection.create H2_connection.Server in
  let headers = [
    { H2_hpack.name = ":status"; value = "200"; sensitive = false };
    { H2_hpack.name = "content-type"; value = "text/html"; sensitive = false };
    { H2_hpack.name = "content-length"; value = "1234"; sensitive = false };
  ] in
  let encoded = H2_connection.encode_headers conn headers in
  Alcotest.(check bool) "encoded non-empty" true (Cstruct.length encoded > 0);

  (* Decode on "client" side *)
  let client_conn = H2_connection.create H2_connection.Client in
  match H2_connection.decode_headers client_conn encoded with
  | Ok decoded ->
      Alcotest.(check int) "decoded header count" 3 (List.length decoded);
      let status_h = List.find (fun (h : H2_hpack.header) -> h.name = ":status") decoded in
      Alcotest.(check string) "status value" "200" status_h.value
  | Error _ ->
      Alcotest.fail "failed to decode headers"

let hpack_tests = [
  "encode decode request", `Quick, test_encode_decode_request;
  "encode decode response", `Quick, test_encode_decode_response;
]

(* ============================================================
   Flow Control Tests
   ============================================================ *)

let test_connection_window () =
  let client = H2_client.create () in
  let conn = H2_client.connection client in
  let initial = H2_connection.send_window conn in
  Alcotest.(check int) "initial window" 65535 initial

let test_stream_window () =
  let conn = H2_connection.create H2_connection.Client in
  match H2_connection.create_stream conn with
  | Ok stream ->
      let window = H2_stream.send_window stream in
      Alcotest.(check int) "stream window" 65535 window
  | Error _ ->
      Alcotest.fail "failed to create stream"

let flow_control_tests = [
  "connection window", `Quick, test_connection_window;
  "stream window", `Quick, test_stream_window;
]

(* ============================================================
   Error Code Tests
   ============================================================ *)

let test_error_code_to_string () =
  Alcotest.(check string) "no_error"
    "NO_ERROR" (H2_frame.error_code_to_string H2_frame.No_error);
  Alcotest.(check string) "protocol_error"
    "PROTOCOL_ERROR" (H2_frame.error_code_to_string H2_frame.Protocol_error);
  Alcotest.(check string) "flow_control_error"
    "FLOW_CONTROL_ERROR" (H2_frame.error_code_to_string H2_frame.Flow_control_error);
  Alcotest.(check string) "cancel"
    "CANCEL" (H2_frame.error_code_to_string H2_frame.Cancel);
  Alcotest.(check string) "enhance_your_calm"
    "ENHANCE_YOUR_CALM" (H2_frame.error_code_to_string H2_frame.Enhance_your_calm)

let test_error_code_roundtrip () =
  let codes = [
    H2_frame.No_error;
    H2_frame.Protocol_error;
    H2_frame.Internal_error;
    H2_frame.Flow_control_error;
    H2_frame.Settings_timeout;
    H2_frame.Stream_closed;
    H2_frame.Frame_size_error;
    H2_frame.Refused_stream;
    H2_frame.Cancel;
    H2_frame.Compression_error;
    H2_frame.Connect_error;
    H2_frame.Enhance_your_calm;
    H2_frame.Inadequate_security;
    H2_frame.Http_1_1_required;
  ] in
  List.iter (fun code ->
    let n = H2_frame.error_code_to_int32 code in
    let code' = H2_frame.error_code_of_int32 n in
    Alcotest.(check bool) "roundtrip"
      true (code = code')
  ) codes

let error_tests = [
  "error_code_to_string", `Quick, test_error_code_to_string;
  "error_code_roundtrip", `Quick, test_error_code_roundtrip;
]

(* ============================================================
   Run Tests
   ============================================================ *)

let () =
  Alcotest.run "H2_client" [
    "creation", creation_tests;
    "frame", frame_tests;
    "protocol", protocol_tests;
    "hpack", hpack_tests;
    "flow_control", flow_control_tests;
    "error", error_tests;
  ]
