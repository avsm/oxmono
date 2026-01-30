(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unit tests for H2_stream module. *)



(* ============================================================
   Test Utilities
   ============================================================ *)

let pp_result fmt = function
  | Ok () -> Format.fprintf fmt "Ok ()"
  | Error (code, msg) ->
    Format.fprintf fmt "Error (%a, %s)" H2_frame.pp_error_code code msg

let result_testable =
  Alcotest.testable pp_result
    (fun a b -> match a, b with
     | Ok (), Ok () -> true
     | Error (c1, _), Error (c2, _) ->
       H2_frame.error_code_to_int32 c1 = H2_frame.error_code_to_int32 c2
     | _ -> false)

let state_testable =
  Alcotest.testable H2_stream.pp_state (=)

(* ============================================================
   Stream Creation Tests
   ============================================================ *)

let test_create () =
  let stream = H2_stream.create 1l in
  Alcotest.(check int32) "stream id" 1l (H2_stream.id stream);
  Alcotest.check state_testable "initial state" H2_stream.Idle (H2_stream.state stream);
  Alcotest.(check bool) "is_idle" true (H2_stream.is_idle stream);
  Alcotest.(check bool) "is_active" false (H2_stream.is_active stream);
  Alcotest.(check bool) "is_closed" false (H2_stream.is_closed stream)

let test_create_with_custom_windows () =
  let stream = H2_stream.create ~initial_send_window:1000 ~initial_recv_window:2000 3l in
  Alcotest.(check int) "send window" 1000 (H2_stream.send_window stream);
  Alcotest.(check int) "recv window" 2000 (H2_stream.recv_window stream)

let test_default_window_size () =
  Alcotest.(check int) "default window" 65535 H2_stream.default_initial_window_size

(* ============================================================
   State Transition Tests
   ============================================================ *)

let test_idle_to_open_send_headers () =
  let stream = H2_stream.create 1l in
  let result = H2_stream.apply_event stream (Send_headers { end_stream = false }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Open" H2_stream.Open (H2_stream.state stream);
  Alcotest.(check bool) "is_open" true (H2_stream.is_open stream)

let test_idle_to_open_recv_headers () =
  let stream = H2_stream.create 2l in
  let result = H2_stream.apply_event stream (Recv_headers { end_stream = false }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Open" H2_stream.Open (H2_stream.state stream)

let test_idle_to_half_closed_local () =
  let stream = H2_stream.create 1l in
  let result = H2_stream.apply_event stream (Send_headers { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_local"
    H2_stream.Half_closed_local (H2_stream.state stream);
  Alcotest.(check bool) "can_send" false (H2_stream.can_send stream);
  Alcotest.(check bool) "can_recv" true (H2_stream.can_recv stream)

let test_idle_to_half_closed_remote () =
  let stream = H2_stream.create 2l in
  let result = H2_stream.apply_event stream (Recv_headers { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_remote"
    H2_stream.Half_closed_remote (H2_stream.state stream);
  Alcotest.(check bool) "can_send" true (H2_stream.can_send stream);
  Alcotest.(check bool) "can_recv" false (H2_stream.can_recv stream)

let test_idle_to_reserved_local () =
  let stream = H2_stream.create 2l in
  let result = H2_stream.apply_event stream Send_push_promise in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Reserved_local"
    H2_stream.Reserved_local (H2_stream.state stream)

let test_idle_to_reserved_remote () =
  let stream = H2_stream.create 2l in
  let result = H2_stream.apply_event stream Recv_push_promise in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Reserved_remote"
    H2_stream.Reserved_remote (H2_stream.state stream)

let test_idle_invalid_data () =
  let stream = H2_stream.create 1l in
  let result = H2_stream.apply_event stream (Send_data { end_stream = false }) in
  match result with
  | Ok () -> Alcotest.fail "Expected error"
  | Error (code, _) ->
    Alcotest.(check int32) "protocol error"
      (H2_frame.error_code_to_int32 H2_frame.Protocol_error)
      (H2_frame.error_code_to_int32 code)

let test_open_to_half_closed_send_end_stream () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = false }));
  let result = H2_stream.apply_event stream (Send_data { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_local"
    H2_stream.Half_closed_local (H2_stream.state stream)

let test_open_to_half_closed_recv_end_stream () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = false }));
  let result = H2_stream.apply_event stream (Recv_data { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_remote"
    H2_stream.Half_closed_remote (H2_stream.state stream)

let test_open_to_closed_rst_stream () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = false }));
  let result = H2_stream.apply_event stream (Send_rst_stream H2_frame.Cancel) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  match H2_stream.state stream with
  | H2_stream.Closed (ResetByUs code) ->
    Alcotest.(check int32) "cancel code"
      (H2_frame.error_code_to_int32 H2_frame.Cancel)
      (H2_frame.error_code_to_int32 code)
  | _ -> Alcotest.fail "Expected Closed(ResetByUs)"

let test_half_closed_local_to_closed () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = true }));
  let result = H2_stream.apply_event stream (Recv_headers { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  match H2_stream.state stream with
  | H2_stream.Closed Finished -> ()
  | _ -> Alcotest.fail "Expected Closed(Finished)"

let test_half_closed_remote_to_closed () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Recv_headers { end_stream = true }));
  let result = H2_stream.apply_event stream (Send_headers { end_stream = true }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  match H2_stream.state stream with
  | H2_stream.Closed Finished -> ()
  | _ -> Alcotest.fail "Expected Closed(Finished)"

let test_half_closed_local_cannot_send () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = true }));
  let result = H2_stream.apply_event stream (Send_data { end_stream = false }) in
  match result with
  | Ok () -> Alcotest.fail "Expected error"
  | Error (code, _) ->
    Alcotest.(check int32) "stream closed error"
      (H2_frame.error_code_to_int32 H2_frame.Stream_closed)
      (H2_frame.error_code_to_int32 code)

let test_half_closed_remote_cannot_recv () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Recv_headers { end_stream = true }));
  let result = H2_stream.apply_event stream (Recv_data { end_stream = false }) in
  match result with
  | Ok () -> Alcotest.fail "Expected error"
  | Error (code, _) ->
    Alcotest.(check int32) "stream closed error"
      (H2_frame.error_code_to_int32 H2_frame.Stream_closed)
      (H2_frame.error_code_to_int32 code)

let test_closed_stream_error () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = true }));
  ignore (H2_stream.apply_event stream (Recv_headers { end_stream = true }));
  let result = H2_stream.apply_event stream (Send_data { end_stream = false }) in
  match result with
  | Ok () -> Alcotest.fail "Expected error"
  | Error (code, _) ->
    Alcotest.(check int32) "stream closed error"
      (H2_frame.error_code_to_int32 H2_frame.Stream_closed)
      (H2_frame.error_code_to_int32 code)

let test_reserved_local_transitions () =
  let stream = H2_stream.create 2l in
  ignore (H2_stream.apply_event stream Send_push_promise);
  (* Can send HEADERS to transition to half-closed remote *)
  let result = H2_stream.apply_event stream (Send_headers { end_stream = false }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_remote"
    H2_stream.Half_closed_remote (H2_stream.state stream)

let test_reserved_remote_transitions () =
  let stream = H2_stream.create 2l in
  ignore (H2_stream.apply_event stream Recv_push_promise);
  (* Can recv HEADERS to transition to half-closed local *)
  let result = H2_stream.apply_event stream (Recv_headers { end_stream = false }) in
  Alcotest.check result_testable "transition ok" (Ok ()) result;
  Alcotest.check state_testable "state is Half_closed_local"
    H2_stream.Half_closed_local (H2_stream.state stream)

(* ============================================================
   Flow Control Tests
   ============================================================ *)

let test_send_window_consume () =
  let stream = H2_stream.create ~initial_send_window:1000 1l in
  let consumed = H2_stream.consume_send_window stream 500 in
  Alcotest.(check int) "consumed" 500 consumed;
  Alcotest.(check int) "remaining window" 500 (H2_stream.send_window stream)

let test_send_window_consume_exceeds () =
  let stream = H2_stream.create ~initial_send_window:100 1l in
  let consumed = H2_stream.consume_send_window stream 500 in
  Alcotest.(check int) "consumed limited" 100 consumed;
  Alcotest.(check int) "window exhausted" 0 (H2_stream.send_window stream)

let test_send_window_credit () =
  let stream = H2_stream.create ~initial_send_window:100 1l in
  ignore (H2_stream.consume_send_window stream 100);
  let result = H2_stream.credit_send_window stream 500 in
  match result with
  | Ok () ->
    Alcotest.(check int) "credited window" 500 (H2_stream.send_window stream)
  | Error _ -> Alcotest.fail "Expected Ok"

let test_send_window_overflow () =
  let stream = H2_stream.create ~initial_send_window:0x7FFFFFF0 1l in
  let result = H2_stream.credit_send_window stream 100 in
  match result with
  | Ok () -> Alcotest.fail "Expected overflow error"
  | Error (code, _) ->
    Alcotest.(check int32) "flow control error"
      (H2_frame.error_code_to_int32 H2_frame.Flow_control_error)
      (H2_frame.error_code_to_int32 code)

let test_recv_window () =
  let stream = H2_stream.create ~initial_recv_window:1000 1l in
  H2_stream.consume_recv_window stream 300;
  Alcotest.(check int) "recv window" 700 (H2_stream.recv_window stream);
  H2_stream.credit_recv_window stream 500;
  Alcotest.(check int) "recv window after credit" 1200 (H2_stream.recv_window stream)

(* ============================================================
   Stream Identifier Tests
   ============================================================ *)

let test_client_initiated () =
  Alcotest.(check bool) "1 is client" true (H2_stream.is_client_initiated 1l);
  Alcotest.(check bool) "3 is client" true (H2_stream.is_client_initiated 3l);
  Alcotest.(check bool) "2 is not client" false (H2_stream.is_client_initiated 2l);
  Alcotest.(check bool) "0 is not client" false (H2_stream.is_client_initiated 0l)

let test_server_initiated () =
  Alcotest.(check bool) "2 is server" true (H2_stream.is_server_initiated 2l);
  Alcotest.(check bool) "4 is server" true (H2_stream.is_server_initiated 4l);
  Alcotest.(check bool) "1 is not server" false (H2_stream.is_server_initiated 1l);
  Alcotest.(check bool) "0 is not server" false (H2_stream.is_server_initiated 0l)

let test_valid_id () =
  Alcotest.(check bool) "1 is valid" true (H2_stream.is_valid_id 1l);
  Alcotest.(check bool) "0 is not valid" false (H2_stream.is_valid_id 0l)

let test_connection_stream () =
  Alcotest.(check int32) "connection id" 0l H2_stream.connection_stream_id;
  Alcotest.(check bool) "0 is connection" true (H2_stream.is_connection_stream 0l);
  Alcotest.(check bool) "1 is not connection" false (H2_stream.is_connection_stream 1l)

(* ============================================================
   Headers Tests
   ============================================================ *)

let test_request_headers () =
  let stream = H2_stream.create 1l in
  Alcotest.(check (option (list (pair string string)))) "no headers"
    None (Option.map (List.map (fun h -> h.H2_hpack.name, h.H2_hpack.value))
           (H2_stream.request_headers stream));
  let headers = [
    { H2_hpack.name = ":method"; value = "GET"; sensitive = false };
    { H2_hpack.name = ":path"; value = "/"; sensitive = false };
  ] in
  H2_stream.set_request_headers stream headers;
  match H2_stream.request_headers stream with
  | Some hdrs ->
    Alcotest.(check int) "header count" 2 (List.length hdrs)
  | None -> Alcotest.fail "Expected headers"

let test_response_headers () =
  let stream = H2_stream.create 1l in
  Alcotest.(check bool) "no response headers"
    true (H2_stream.response_headers stream = None);
  let headers = [
    { H2_hpack.name = ":status"; value = "200"; sensitive = false };
  ] in
  H2_stream.set_response_headers stream headers;
  match H2_stream.response_headers stream with
  | Some hdrs ->
    Alcotest.(check int) "header count" 1 (List.length hdrs)
  | None -> Alcotest.fail "Expected headers"

let test_trailers () =
  let stream = H2_stream.create 1l in
  Alcotest.(check bool) "no trailers" true (H2_stream.trailers stream = None);
  let trailers = [
    { H2_hpack.name = "grpc-status"; value = "0"; sensitive = false };
  ] in
  H2_stream.set_trailers stream trailers;
  Alcotest.(check bool) "has trailers" true (H2_stream.trailers stream <> None)

(* ============================================================
   Reset Tests
   ============================================================ *)

let test_reset () =
  let stream = H2_stream.create 1l in
  ignore (H2_stream.apply_event stream (Send_headers { end_stream = false }));
  H2_stream.reset stream H2_frame.Cancel;
  match H2_stream.state stream with
  | H2_stream.Closed (ResetByUs code) ->
    Alcotest.(check int32) "cancel code"
      (H2_frame.error_code_to_int32 H2_frame.Cancel)
      (H2_frame.error_code_to_int32 code)
  | _ -> Alcotest.fail "Expected Closed(ResetByUs)"

(* ============================================================
   Pretty Printing Tests
   ============================================================ *)

let test_state_to_string () =
  Alcotest.(check string) "idle" "Idle" (H2_stream.state_to_string H2_stream.Idle);
  Alcotest.(check string) "open" "Open" (H2_stream.state_to_string H2_stream.Open);
  Alcotest.(check string) "half closed local" "HalfClosed(local)"
    (H2_stream.state_to_string H2_stream.Half_closed_local);
  Alcotest.(check string) "half closed remote" "HalfClosed(remote)"
    (H2_stream.state_to_string H2_stream.Half_closed_remote);
  Alcotest.(check string) "reserved local" "Reserved(local)"
    (H2_stream.state_to_string H2_stream.Reserved_local);
  Alcotest.(check string) "reserved remote" "Reserved(remote)"
    (H2_stream.state_to_string H2_stream.Reserved_remote)

(* ============================================================
   Test Suite
   ============================================================ *)

let () =
  Alcotest.run "H2_stream" [
    "creation", [
      Alcotest.test_case "create" `Quick test_create;
      Alcotest.test_case "custom windows" `Quick test_create_with_custom_windows;
      Alcotest.test_case "default window size" `Quick test_default_window_size;
    ];
    "state_transitions", [
      Alcotest.test_case "idle to open send headers" `Quick test_idle_to_open_send_headers;
      Alcotest.test_case "idle to open recv headers" `Quick test_idle_to_open_recv_headers;
      Alcotest.test_case "idle to half closed local" `Quick test_idle_to_half_closed_local;
      Alcotest.test_case "idle to half closed remote" `Quick test_idle_to_half_closed_remote;
      Alcotest.test_case "idle to reserved local" `Quick test_idle_to_reserved_local;
      Alcotest.test_case "idle to reserved remote" `Quick test_idle_to_reserved_remote;
      Alcotest.test_case "idle invalid data" `Quick test_idle_invalid_data;
      Alcotest.test_case "open to half closed send" `Quick test_open_to_half_closed_send_end_stream;
      Alcotest.test_case "open to half closed recv" `Quick test_open_to_half_closed_recv_end_stream;
      Alcotest.test_case "open to closed rst" `Quick test_open_to_closed_rst_stream;
      Alcotest.test_case "half closed local to closed" `Quick test_half_closed_local_to_closed;
      Alcotest.test_case "half closed remote to closed" `Quick test_half_closed_remote_to_closed;
      Alcotest.test_case "half closed local cannot send" `Quick test_half_closed_local_cannot_send;
      Alcotest.test_case "half closed remote cannot recv" `Quick test_half_closed_remote_cannot_recv;
      Alcotest.test_case "closed stream error" `Quick test_closed_stream_error;
      Alcotest.test_case "reserved local transitions" `Quick test_reserved_local_transitions;
      Alcotest.test_case "reserved remote transitions" `Quick test_reserved_remote_transitions;
    ];
    "flow_control", [
      Alcotest.test_case "send window consume" `Quick test_send_window_consume;
      Alcotest.test_case "send window exceeds" `Quick test_send_window_consume_exceeds;
      Alcotest.test_case "send window credit" `Quick test_send_window_credit;
      Alcotest.test_case "send window overflow" `Quick test_send_window_overflow;
      Alcotest.test_case "recv window" `Quick test_recv_window;
    ];
    "stream_identifiers", [
      Alcotest.test_case "client initiated" `Quick test_client_initiated;
      Alcotest.test_case "server initiated" `Quick test_server_initiated;
      Alcotest.test_case "valid id" `Quick test_valid_id;
      Alcotest.test_case "connection stream" `Quick test_connection_stream;
    ];
    "headers", [
      Alcotest.test_case "request headers" `Quick test_request_headers;
      Alcotest.test_case "response headers" `Quick test_response_headers;
      Alcotest.test_case "trailers" `Quick test_trailers;
    ];
    "reset", [
      Alcotest.test_case "reset stream" `Quick test_reset;
    ];
    "pretty_printing", [
      Alcotest.test_case "state to string" `Quick test_state_to_string;
    ];
  ]
