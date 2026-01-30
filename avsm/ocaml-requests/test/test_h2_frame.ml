(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unit tests for H2_frame module. *)



(* ============================================================
   Test Utilities
   ============================================================ *)

let check_ok msg = function
  | Ok v -> v
  | Error e ->
      Alcotest.fail (Format.asprintf "%s: %a" msg H2_frame.pp_parse_error e)

let frame_type_testable =
  Alcotest.testable H2_frame.pp_frame_type (=)

let error_code_testable =
  Alcotest.testable H2_frame.pp_error_code (=)

let setting_testable =
  Alcotest.testable H2_frame.pp_setting (=)

(* ============================================================
   Stream ID Tests
   ============================================================ *)

let test_stream_id_client_initiated () =
  Alcotest.(check bool) "stream 1 is client-initiated"
    true (H2_frame.stream_id_is_client_initiated 1l);
  Alcotest.(check bool) "stream 3 is client-initiated"
    true (H2_frame.stream_id_is_client_initiated 3l);
  Alcotest.(check bool) "stream 101 is client-initiated"
    true (H2_frame.stream_id_is_client_initiated 101l);
  Alcotest.(check bool) "stream 0 is not client-initiated"
    false (H2_frame.stream_id_is_client_initiated 0l);
  Alcotest.(check bool) "stream 2 is not client-initiated"
    false (H2_frame.stream_id_is_client_initiated 2l)

let test_stream_id_server_initiated () =
  Alcotest.(check bool) "stream 2 is server-initiated"
    true (H2_frame.stream_id_is_server_initiated 2l);
  Alcotest.(check bool) "stream 4 is server-initiated"
    true (H2_frame.stream_id_is_server_initiated 4l);
  Alcotest.(check bool) "stream 100 is server-initiated"
    true (H2_frame.stream_id_is_server_initiated 100l);
  Alcotest.(check bool) "stream 0 is not server-initiated"
    false (H2_frame.stream_id_is_server_initiated 0l);
  Alcotest.(check bool) "stream 1 is not server-initiated"
    false (H2_frame.stream_id_is_server_initiated 1l)

(* ============================================================
   Frame Type Tests
   ============================================================ *)

let test_frame_type_conversions () =
  let types = [
    (H2_frame.Data, 0x00);
    (H2_frame.Headers, 0x01);
    (H2_frame.Priority, 0x02);
    (H2_frame.Rst_stream, 0x03);
    (H2_frame.Settings, 0x04);
    (H2_frame.Push_promise, 0x05);
    (H2_frame.Ping, 0x06);
    (H2_frame.Goaway, 0x07);
    (H2_frame.Window_update, 0x08);
    (H2_frame.Continuation, 0x09);
  ] in
  List.iter (fun (ft, code) ->
    Alcotest.(check int) (Format.asprintf "%a to int" H2_frame.pp_frame_type ft)
      code (H2_frame.frame_type_to_int ft);
    Alcotest.check frame_type_testable
      (Printf.sprintf "int %d to frame type" code)
      ft (H2_frame.frame_type_of_int code)
  ) types

let test_frame_type_unknown () =
  Alcotest.check frame_type_testable "unknown type 0xff"
    (H2_frame.Unknown 0xff) (H2_frame.frame_type_of_int 0xff);
  Alcotest.(check int) "unknown type to int"
    0xff (H2_frame.frame_type_to_int (H2_frame.Unknown 0xff))

(* ============================================================
   Flags Tests
   ============================================================ *)

let test_flags_operations () =
  let open H2_frame.Flags in
  Alcotest.(check bool) "end_stream not in none"
    false (test none end_stream);
  Alcotest.(check bool) "end_stream in set flags"
    true (test (set none end_stream) end_stream);
  Alcotest.(check bool) "clear removes flag"
    false (test (clear (set none end_stream) end_stream) end_stream);
  (* Multiple flags *)
  let flags = set (set none end_stream) end_headers in
  Alcotest.(check bool) "end_stream in combined"
    true (test flags end_stream);
  Alcotest.(check bool) "end_headers in combined"
    true (test flags end_headers);
  Alcotest.(check bool) "padded not in combined"
    false (test flags padded)

let test_flags_values () =
  let open H2_frame.Flags in
  Alcotest.(check int) "END_STREAM = 0x01" 0x01 end_stream;
  Alcotest.(check int) "ACK = 0x01" 0x01 ack;
  Alcotest.(check int) "END_HEADERS = 0x04" 0x04 end_headers;
  Alcotest.(check int) "PADDED = 0x08" 0x08 padded;
  Alcotest.(check int) "PRIORITY = 0x20" 0x20 priority

(* ============================================================
   Error Code Tests
   ============================================================ *)

let test_error_code_conversions () =
  let codes = [
    (H2_frame.No_error, 0x0l);
    (H2_frame.Protocol_error, 0x1l);
    (H2_frame.Internal_error, 0x2l);
    (H2_frame.Flow_control_error, 0x3l);
    (H2_frame.Settings_timeout, 0x4l);
    (H2_frame.Stream_closed, 0x5l);
    (H2_frame.Frame_size_error, 0x6l);
    (H2_frame.Refused_stream, 0x7l);
    (H2_frame.Cancel, 0x8l);
    (H2_frame.Compression_error, 0x9l);
    (H2_frame.Connect_error, 0xal);
    (H2_frame.Enhance_your_calm, 0xbl);
    (H2_frame.Inadequate_security, 0xcl);
    (H2_frame.Http_1_1_required, 0xdl);
  ] in
  List.iter (fun (ec, code) ->
    Alcotest.(check int32) (Format.asprintf "%a to int32" H2_frame.pp_error_code ec)
      code (H2_frame.error_code_to_int32 ec);
    Alcotest.check error_code_testable
      (Printf.sprintf "int32 %ld to error code" code)
      ec (H2_frame.error_code_of_int32 code)
  ) codes

let test_error_code_unknown () =
  Alcotest.check error_code_testable "unknown error 0xff"
    (H2_frame.Unknown_error 0xffl) (H2_frame.error_code_of_int32 0xffl);
  Alcotest.(check int32) "unknown error to int32"
    0xffl (H2_frame.error_code_to_int32 (H2_frame.Unknown_error 0xffl))

(* ============================================================
   Settings Tests
   ============================================================ *)

let test_setting_conversions () =
  let settings = [
    (H2_frame.Header_table_size 4096, (0x1, 4096l));
    (H2_frame.Enable_push true, (0x2, 1l));
    (H2_frame.Enable_push false, (0x2, 0l));
    (H2_frame.Max_concurrent_streams 100l, (0x3, 100l));
    (H2_frame.Initial_window_size 65535l, (0x4, 65535l));
    (H2_frame.Max_frame_size 16384, (0x5, 16384l));
    (H2_frame.Max_header_list_size 8192, (0x6, 8192l));
  ] in
  List.iter (fun (s, (id, value)) ->
    let (got_id, got_value) = H2_frame.setting_to_pair s in
    Alcotest.(check int) (Format.asprintf "%a id" H2_frame.pp_setting s)
      id got_id;
    Alcotest.(check int32) (Format.asprintf "%a value" H2_frame.pp_setting s)
      value got_value;
    Alcotest.check setting_testable
      (Printf.sprintf "pair (%d, %ld) to setting" id value)
      s (H2_frame.setting_of_pair id value)
  ) settings

let test_setting_unknown () =
  let s = H2_frame.Unknown_setting (0xff, 123l) in
  let (id, value) = H2_frame.setting_to_pair s in
  Alcotest.(check int) "unknown setting id" 0xff id;
  Alcotest.(check int32) "unknown setting value" 123l value;
  Alcotest.check setting_testable "unknown setting roundtrip"
    s (H2_frame.setting_of_pair 0xff 123l)

(* ============================================================
   Frame Header Parsing Tests
   ============================================================ *)

let test_parse_frame_header () =
  (* Create a frame header: length=100, type=DATA, flags=END_STREAM, stream_id=1 *)
  let buf = Cstruct.create 9 in
  Cstruct.set_uint8 buf 0 0x00;  (* length high byte *)
  Cstruct.set_uint8 buf 1 0x00;  (* length mid byte *)
  Cstruct.set_uint8 buf 2 0x64;  (* length low byte = 100 *)
  Cstruct.set_uint8 buf 3 0x00;  (* type = DATA *)
  Cstruct.set_uint8 buf 4 0x01;  (* flags = END_STREAM *)
  Cstruct.BE.set_uint32 buf 5 1l;  (* stream_id = 1 *)
  let header = check_ok "parse header" (H2_frame.parse_frame_header buf) in
  Alcotest.(check int) "length" 100 header.length;
  Alcotest.check frame_type_testable "frame_type" H2_frame.Data header.frame_type;
  Alcotest.(check int) "flags" H2_frame.Flags.end_stream header.flags;
  Alcotest.(check int32) "stream_id" 1l header.stream_id

let test_parse_frame_header_incomplete () =
  let buf = Cstruct.create 8 in  (* Too short *)
  match H2_frame.parse_frame_header buf with
  | Error H2_frame.Incomplete -> ()
  | _ -> Alcotest.fail "expected Incomplete error"

let test_parse_frame_header_clears_reserved_bit () =
  let buf = Cstruct.create 9 in
  Cstruct.set_uint8 buf 0 0x00;
  Cstruct.set_uint8 buf 1 0x00;
  Cstruct.set_uint8 buf 2 0x00;
  Cstruct.set_uint8 buf 3 0x00;
  Cstruct.set_uint8 buf 4 0x00;
  (* Set reserved bit in stream ID *)
  Cstruct.BE.set_uint32 buf 5 0x80000001l;
  let header = check_ok "parse header" (H2_frame.parse_frame_header buf) in
  Alcotest.(check int32) "stream_id clears reserved bit" 1l header.stream_id

(* ============================================================
   Frame Serialization Tests
   ============================================================ *)

let test_serialize_frame_header () =
  let header = {
    H2_frame.length = 256;
    frame_type = H2_frame.Headers;
    flags = H2_frame.Flags.(set end_stream end_headers);
    stream_id = 3l;
  } in
  let buf = H2_frame.serialize_frame_header header in
  Alcotest.(check int) "buffer length" 9 (Cstruct.length buf);
  (* Check length encoding *)
  Alcotest.(check int) "length high" 0x00 (Cstruct.get_uint8 buf 0);
  Alcotest.(check int) "length mid" 0x01 (Cstruct.get_uint8 buf 1);
  Alcotest.(check int) "length low" 0x00 (Cstruct.get_uint8 buf 2);
  (* Check type *)
  Alcotest.(check int) "type" 0x01 (Cstruct.get_uint8 buf 3);
  (* Check flags *)
  Alcotest.(check int) "flags" 0x05 (Cstruct.get_uint8 buf 4);
  (* Check stream_id *)
  Alcotest.(check int32) "stream_id" 3l (Cstruct.BE.get_uint32 buf 5)

let test_frame_header_roundtrip () =
  let original = {
    H2_frame.length = 16384;
    frame_type = H2_frame.Settings;
    flags = H2_frame.Flags.ack;
    stream_id = 0l;
  } in
  let buf = H2_frame.serialize_frame_header original in
  let parsed = check_ok "roundtrip" (H2_frame.parse_frame_header buf) in
  Alcotest.(check int) "length matches" original.length parsed.length;
  Alcotest.check frame_type_testable "type matches" original.frame_type parsed.frame_type;
  Alcotest.(check int) "flags match" original.flags parsed.flags;
  Alcotest.(check int32) "stream_id matches" original.stream_id parsed.stream_id

(* ============================================================
   DATA Frame Tests
   ============================================================ *)

let test_data_frame_roundtrip () =
  let data = Cstruct.of_string "Hello, HTTP/2!" in
  let frame = H2_frame.make_data ~stream_id:1l ~end_stream:true data in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, consumed) = check_ok "parse data frame"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.(check int) "consumed bytes" (9 + 14) consumed;
  Alcotest.(check int32) "stream_id" 1l parsed.header.stream_id;
  Alcotest.(check bool) "end_stream flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.end_stream);
  match parsed.payload with
  | H2_frame.Data_payload { data = d } ->
      Alcotest.(check string) "data content"
        "Hello, HTTP/2!" (Cstruct.to_string d)
  | _ -> Alcotest.fail "expected Data_payload"

(* ============================================================
   HEADERS Frame Tests
   ============================================================ *)

let test_headers_frame_roundtrip () =
  let block = Cstruct.of_string "\x82\x86\x84" in  (* Fake HPACK data *)
  let frame = H2_frame.make_headers ~stream_id:1l ~end_stream:false
    ~end_headers:true block in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse headers frame"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Headers parsed.header.frame_type;
  Alcotest.(check bool) "end_headers flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.end_headers);
  match parsed.payload with
  | H2_frame.Headers_payload { priority; header_block } ->
      Alcotest.(check bool) "no priority" true (Option.is_none priority);
      Alcotest.(check int) "block length" 3 (Cstruct.length header_block)
  | _ -> Alcotest.fail "expected Headers_payload"

let test_headers_frame_with_priority () =
  let block = Cstruct.of_string "\x82" in
  let priority = { H2_frame.exclusive = true; stream_dependency = 0l; weight = 16 } in
  let frame = H2_frame.make_headers ~stream_id:1l ~priority block in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse headers with priority"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.(check bool) "priority flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.priority);
  match parsed.payload with
  | H2_frame.Headers_payload { priority = Some p; _ } ->
      Alcotest.(check bool) "exclusive" true p.exclusive;
      Alcotest.(check int32) "stream_dependency" 0l p.stream_dependency;
      Alcotest.(check int) "weight" 16 p.weight
  | _ -> Alcotest.fail "expected Headers_payload with priority"

(* ============================================================
   RST_STREAM Frame Tests
   ============================================================ *)

let test_rst_stream_frame () =
  let frame = H2_frame.make_rst_stream ~stream_id:5l H2_frame.Cancel in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse rst_stream"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Rst_stream parsed.header.frame_type;
  Alcotest.(check int32) "stream_id" 5l parsed.header.stream_id;
  match parsed.payload with
  | H2_frame.Rst_stream_payload code ->
      Alcotest.check error_code_testable "error code" H2_frame.Cancel code
  | _ -> Alcotest.fail "expected Rst_stream_payload"

let test_rst_stream_wrong_size () =
  let header = {
    H2_frame.length = 5;  (* Should be 4 *)
    frame_type = H2_frame.Rst_stream;
    flags = 0;
    stream_id = 1l;
  } in
  let payload = Cstruct.create 5 in
  match H2_frame.parse_frame_payload header payload with
  | Error (H2_frame.Frame_size_error _) -> ()
  | _ -> Alcotest.fail "expected Frame_size_error"

(* ============================================================
   SETTINGS Frame Tests
   ============================================================ *)

let test_settings_frame () =
  let settings = [
    H2_frame.Header_table_size 8192;
    H2_frame.Enable_push false;
    H2_frame.Max_concurrent_streams 100l;
  ] in
  let frame = H2_frame.make_settings settings in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse settings"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Settings parsed.header.frame_type;
  Alcotest.(check int32) "stream_id is 0" 0l parsed.header.stream_id;
  match parsed.payload with
  | H2_frame.Settings_payload parsed_settings ->
      Alcotest.(check int) "settings count" 3 (List.length parsed_settings)
  | _ -> Alcotest.fail "expected Settings_payload"

let test_settings_ack () =
  let frame = H2_frame.make_settings ~ack:true [] in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse settings ack"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.(check bool) "ack flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.ack);
  Alcotest.(check int) "empty payload" 0 parsed.header.length;
  match parsed.payload with
  | H2_frame.Settings_payload settings ->
      Alcotest.(check int) "empty settings list" 0 (List.length settings)
  | _ -> Alcotest.fail "expected Settings_payload"

let test_settings_ack_with_payload_fails () =
  (* SETTINGS ACK with non-empty payload is an error *)
  let header = {
    H2_frame.length = 6;
    frame_type = H2_frame.Settings;
    flags = H2_frame.Flags.ack;
    stream_id = 0l;
  } in
  let payload = Cstruct.create 6 in
  match H2_frame.parse_frame_payload header payload with
  | Error (H2_frame.Frame_size_error _) -> ()
  | _ -> Alcotest.fail "expected Frame_size_error for SETTINGS ACK with payload"

(* ============================================================
   PING Frame Tests
   ============================================================ *)

let test_ping_frame () =
  let data = Cstruct.of_string "12345678" in
  let frame = H2_frame.make_ping data in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse ping"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Ping parsed.header.frame_type;
  Alcotest.(check int32) "stream_id is 0" 0l parsed.header.stream_id;
  Alcotest.(check bool) "no ack flag"
    false (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.ack);
  match parsed.payload with
  | H2_frame.Ping_payload d ->
      Alcotest.(check string) "ping data" "12345678" (Cstruct.to_string d)
  | _ -> Alcotest.fail "expected Ping_payload"

let test_ping_ack () =
  let data = Cstruct.of_string "ACKDATA!" in
  let frame = H2_frame.make_ping ~ack:true data in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse ping ack"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.(check bool) "ack flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.ack)

let test_ping_invalid_size () =
  Alcotest.check_raises "ping wrong size"
    (Invalid_argument "PING data must be exactly 8 bytes")
    (fun () -> ignore (H2_frame.make_ping (Cstruct.of_string "short")))

(* ============================================================
   GOAWAY Frame Tests
   ============================================================ *)

let test_goaway_frame () =
  let frame = H2_frame.make_goaway ~last_stream_id:5l
    H2_frame.Protocol_error ~debug:"test error" () in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse goaway"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Goaway parsed.header.frame_type;
  Alcotest.(check int32) "stream_id is 0" 0l parsed.header.stream_id;
  match parsed.payload with
  | H2_frame.Goaway_payload { last_stream_id; error_code; debug_data } ->
      Alcotest.(check int32) "last_stream_id" 5l last_stream_id;
      Alcotest.check error_code_testable "error_code"
        H2_frame.Protocol_error error_code;
      Alcotest.(check string) "debug_data" "test error" (Cstruct.to_string debug_data)
  | _ -> Alcotest.fail "expected Goaway_payload"

let test_goaway_no_debug () =
  let frame = H2_frame.make_goaway ~last_stream_id:0l H2_frame.No_error () in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse goaway no debug"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  match parsed.payload with
  | H2_frame.Goaway_payload { debug_data; _ } ->
      Alcotest.(check int) "no debug data" 0 (Cstruct.length debug_data)
  | _ -> Alcotest.fail "expected Goaway_payload"

(* ============================================================
   WINDOW_UPDATE Frame Tests
   ============================================================ *)

let test_window_update_frame () =
  let frame = H2_frame.make_window_update ~stream_id:1l 32768l in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse window_update"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Window_update parsed.header.frame_type;
  Alcotest.(check int32) "stream_id" 1l parsed.header.stream_id;
  match parsed.payload with
  | H2_frame.Window_update_payload increment ->
      Alcotest.(check int32) "increment" 32768l increment
  | _ -> Alcotest.fail "expected Window_update_payload"

let test_window_update_connection_level () =
  let frame = H2_frame.make_window_update ~stream_id:0l 65535l in
  Alcotest.(check int32) "connection-level stream_id"
    0l frame.header.stream_id

let test_window_update_zero_increment () =
  Alcotest.check_raises "zero increment"
    (Invalid_argument "WINDOW_UPDATE increment must be > 0")
    (fun () -> ignore (H2_frame.make_window_update ~stream_id:1l 0l))

let test_window_update_parse_zero_increment () =
  (* Create a window update with zero increment to test parser *)
  let header = {
    H2_frame.length = 4;
    frame_type = H2_frame.Window_update;
    flags = 0;
    stream_id = 1l;
  } in
  let payload = Cstruct.create 4 in
  Cstruct.BE.set_uint32 payload 0 0l;  (* Zero increment *)
  match H2_frame.parse_frame_payload header payload with
  | Error (H2_frame.Protocol_error _) -> ()
  | _ -> Alcotest.fail "expected Protocol_error for zero increment"

(* ============================================================
   CONTINUATION Frame Tests
   ============================================================ *)

let test_continuation_frame () =
  let block = Cstruct.of_string "\x82\x86" in
  let frame = H2_frame.make_continuation ~stream_id:1l ~end_headers:true block in
  let serialized = H2_frame.serialize_frame frame in
  let (parsed, _) = check_ok "parse continuation"
    (H2_frame.parse_frame serialized ~max_frame_size:16384) in
  Alcotest.check frame_type_testable "type" H2_frame.Continuation parsed.header.frame_type;
  Alcotest.(check bool) "end_headers flag"
    true (H2_frame.Flags.test parsed.header.flags H2_frame.Flags.end_headers);
  match parsed.payload with
  | H2_frame.Continuation_payload { header_block } ->
      Alcotest.(check int) "block length" 2 (Cstruct.length header_block)
  | _ -> Alcotest.fail "expected Continuation_payload"

(* ============================================================
   Frame Size Limit Tests
   ============================================================ *)

let test_frame_exceeds_max_size () =
  let header_buf = Cstruct.create 9 in
  (* Set length to 20000, exceeding default max of 16384 *)
  Cstruct.set_uint8 header_buf 0 0x00;
  Cstruct.set_uint8 header_buf 1 0x4e;
  Cstruct.set_uint8 header_buf 2 0x20;
  Cstruct.set_uint8 header_buf 3 0x00;  (* DATA *)
  Cstruct.set_uint8 header_buf 4 0x00;
  Cstruct.BE.set_uint32 header_buf 5 1l;
  let payload = Cstruct.create 20000 in
  let buf = Cstruct.concat [header_buf; payload] in
  match H2_frame.parse_frame buf ~max_frame_size:16384 with
  | Error (H2_frame.Frame_size_error _) -> ()
  | _ -> Alcotest.fail "expected Frame_size_error"

(* ============================================================
   Constants Tests
   ============================================================ *)

let test_constants () =
  Alcotest.(check int) "default_max_frame_size"
    16384 H2_frame.default_max_frame_size;
  Alcotest.(check int) "max_max_frame_size"
    16777215 H2_frame.max_max_frame_size;
  Alcotest.(check int32) "default_initial_window_size"
    65535l H2_frame.default_initial_window_size;
  Alcotest.(check int32) "max_window_size"
    0x7fffffffl H2_frame.max_window_size;
  Alcotest.(check string) "connection_preface"
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" H2_frame.connection_preface;
  Alcotest.(check int) "connection_preface_length"
    24 H2_frame.connection_preface_length;
  Alcotest.(check int) "frame_header_length"
    9 H2_frame.frame_header_length

(* ============================================================
   Priority Tests
   ============================================================ *)

let test_default_priority () =
  let p = H2_frame.default_priority in
  Alcotest.(check bool) "not exclusive" false p.exclusive;
  Alcotest.(check int32) "depends on stream 0" 0l p.stream_dependency;
  Alcotest.(check int) "weight 16" 16 p.weight

let test_priority_frame () =
  (* Create raw PRIORITY frame manually since we don't have a make_priority *)
  let header = {
    H2_frame.length = 5;
    frame_type = H2_frame.Priority;
    flags = 0;
    stream_id = 3l;
  } in
  let payload = Cstruct.create 5 in
  (* exclusive=true, depends on stream 1, weight=32 *)
  Cstruct.BE.set_uint32 payload 0 (Int32.logor 1l 0x80000000l);
  Cstruct.set_uint8 payload 4 31;  (* weight 32 = 31 + 1 *)
  let parsed = check_ok "parse priority payload"
    (H2_frame.parse_frame_payload header payload) in
  match parsed with
  | H2_frame.Priority_payload p ->
      Alcotest.(check bool) "exclusive" true p.exclusive;
      Alcotest.(check int32) "stream_dependency" 1l p.stream_dependency;
      Alcotest.(check int) "weight" 32 p.weight
  | _ -> Alcotest.fail "expected Priority_payload"

(* ============================================================
   Test Suite
   ============================================================ *)

let () =
  Alcotest.run "H2_frame" [
    "stream_id", [
      Alcotest.test_case "client initiated" `Quick test_stream_id_client_initiated;
      Alcotest.test_case "server initiated" `Quick test_stream_id_server_initiated;
    ];
    "frame_type", [
      Alcotest.test_case "conversions" `Quick test_frame_type_conversions;
      Alcotest.test_case "unknown" `Quick test_frame_type_unknown;
    ];
    "flags", [
      Alcotest.test_case "operations" `Quick test_flags_operations;
      Alcotest.test_case "values" `Quick test_flags_values;
    ];
    "error_code", [
      Alcotest.test_case "conversions" `Quick test_error_code_conversions;
      Alcotest.test_case "unknown" `Quick test_error_code_unknown;
    ];
    "settings", [
      Alcotest.test_case "conversions" `Quick test_setting_conversions;
      Alcotest.test_case "unknown" `Quick test_setting_unknown;
    ];
    "frame_header", [
      Alcotest.test_case "parse" `Quick test_parse_frame_header;
      Alcotest.test_case "parse incomplete" `Quick test_parse_frame_header_incomplete;
      Alcotest.test_case "clears reserved bit" `Quick test_parse_frame_header_clears_reserved_bit;
      Alcotest.test_case "serialize" `Quick test_serialize_frame_header;
      Alcotest.test_case "roundtrip" `Quick test_frame_header_roundtrip;
    ];
    "data_frame", [
      Alcotest.test_case "roundtrip" `Quick test_data_frame_roundtrip;
    ];
    "headers_frame", [
      Alcotest.test_case "roundtrip" `Quick test_headers_frame_roundtrip;
      Alcotest.test_case "with priority" `Quick test_headers_frame_with_priority;
    ];
    "rst_stream_frame", [
      Alcotest.test_case "roundtrip" `Quick test_rst_stream_frame;
      Alcotest.test_case "wrong size" `Quick test_rst_stream_wrong_size;
    ];
    "settings_frame", [
      Alcotest.test_case "roundtrip" `Quick test_settings_frame;
      Alcotest.test_case "ack" `Quick test_settings_ack;
      Alcotest.test_case "ack with payload fails" `Quick test_settings_ack_with_payload_fails;
    ];
    "ping_frame", [
      Alcotest.test_case "roundtrip" `Quick test_ping_frame;
      Alcotest.test_case "ack" `Quick test_ping_ack;
      Alcotest.test_case "invalid size" `Quick test_ping_invalid_size;
    ];
    "goaway_frame", [
      Alcotest.test_case "roundtrip" `Quick test_goaway_frame;
      Alcotest.test_case "no debug" `Quick test_goaway_no_debug;
    ];
    "window_update_frame", [
      Alcotest.test_case "roundtrip" `Quick test_window_update_frame;
      Alcotest.test_case "connection level" `Quick test_window_update_connection_level;
      Alcotest.test_case "zero increment construct" `Quick test_window_update_zero_increment;
      Alcotest.test_case "zero increment parse" `Quick test_window_update_parse_zero_increment;
    ];
    "continuation_frame", [
      Alcotest.test_case "roundtrip" `Quick test_continuation_frame;
    ];
    "frame_size", [
      Alcotest.test_case "exceeds max" `Quick test_frame_exceeds_max_size;
    ];
    "constants", [
      Alcotest.test_case "values" `Quick test_constants;
    ];
    "priority", [
      Alcotest.test_case "default" `Quick test_default_priority;
      Alcotest.test_case "parse" `Quick test_priority_frame;
    ];
  ]
