(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unit tests for HTTP/2 shared type extensions.

    Tests for:
    - Http_version module (version types, ALPN)
    - Headers module (pseudo-header support, H2 validation)
    - Error module (H2 error types and query functions) *)

open Requests

(* ============================================================
   Http_version Tests
   ============================================================ *)

let test_version_to_string () =
  Alcotest.(check string) "Http_1_0" "HTTP/1.0" (Http_version.to_string Http_version.Http_1_0);
  Alcotest.(check string) "Http_1_1" "HTTP/1.1" (Http_version.to_string Http_version.Http_1_1);
  Alcotest.(check string) "Http_2" "HTTP/2" (Http_version.to_string Http_version.Http_2)

let test_version_compare () =
  let open Http_version in
  Alcotest.(check bool) "Http_2 > Http_1_1" true (compare Http_2 Http_1_1 > 0);
  Alcotest.(check bool) "Http_1_1 > Http_1_0" true (compare Http_1_1 Http_1_0 > 0);
  Alcotest.(check bool) "Http_1_0 < Http_2" true (compare Http_1_0 Http_2 < 0);
  Alcotest.(check bool) "Http_2 = Http_2" true (compare Http_2 Http_2 = 0)

let test_alpn_identifiers () =
  Alcotest.(check string) "alpn_h2" "h2" Http_version.alpn_h2;
  Alcotest.(check string) "alpn_http_1_1" "http/1.1" Http_version.alpn_http_1_1

let test_alpn_of_version () =
  let open Http_version in
  Alcotest.(check (option string)) "Http_2" (Some "h2") (alpn_of_version Http_2);
  Alcotest.(check (option string)) "Http_1_1" (Some "http/1.1") (alpn_of_version Http_1_1);
  Alcotest.(check (option string)) "Http_1_0" None (alpn_of_version Http_1_0)

let test_version_of_alpn () =
  let open Http_version in
  let version_eq = Alcotest.testable pp equal in
  Alcotest.(check (option version_eq)) "h2" (Some Http_2) (version_of_alpn "h2");
  Alcotest.(check (option version_eq)) "http/1.1" (Some Http_1_1) (version_of_alpn "http/1.1");
  Alcotest.(check (option version_eq)) "unknown" None (version_of_alpn "spdy")

let test_alpn_protocols () =
  let open Http_version in
  Alcotest.(check (list string)) "h2 preferred"
    ["h2"; "http/1.1"]
    (alpn_protocols ~preferred:[Http_2; Http_1_1]);
  Alcotest.(check (list string)) "h1.1 only"
    ["http/1.1"]
    (alpn_protocols ~preferred:[Http_1_1]);
  Alcotest.(check (list string)) "http/1.0 filtered out"
    ["h2"]
    (alpn_protocols ~preferred:[Http_1_0; Http_2])

let test_version_capabilities () =
  let open Http_version in
  (* Multiplexing *)
  Alcotest.(check bool) "Http_2 multiplexing" true (supports_multiplexing Http_2);
  Alcotest.(check bool) "Http_1_1 no multiplexing" false (supports_multiplexing Http_1_1);
  (* Server push *)
  Alcotest.(check bool) "Http_2 server push" true (supports_server_push Http_2);
  Alcotest.(check bool) "Http_1_1 no server push" false (supports_server_push Http_1_1);
  (* Header compression *)
  Alcotest.(check bool) "Http_2 header compression" true (supports_header_compression Http_2);
  Alcotest.(check bool) "Http_1_1 no header compression" false (supports_header_compression Http_1_1)

let http_version_tests = [
  "version_to_string", `Quick, test_version_to_string;
  "version_compare", `Quick, test_version_compare;
  "alpn_identifiers", `Quick, test_alpn_identifiers;
  "alpn_of_version", `Quick, test_alpn_of_version;
  "version_of_alpn", `Quick, test_version_of_alpn;
  "alpn_protocols", `Quick, test_alpn_protocols;
  "version_capabilities", `Quick, test_version_capabilities;
]

(* ============================================================
   Headers Pseudo-Header Tests
   ============================================================ *)

let test_is_pseudo_header () =
  Alcotest.(check bool) ":method is pseudo" true (Headers.is_pseudo_header ":method");
  Alcotest.(check bool) ":status is pseudo" true (Headers.is_pseudo_header ":status");
  Alcotest.(check bool) "content-type not pseudo" false (Headers.is_pseudo_header "content-type");
  Alcotest.(check bool) "empty not pseudo" false (Headers.is_pseudo_header "")

let test_pseudo_header_operations () =
  let h = Headers.empty in
  let h = Headers.set_pseudo "method" "GET" h in
  let h = Headers.set_pseudo "path" "/" h in

  Alcotest.(check (option string)) "get method" (Some "GET") (Headers.get_pseudo "method" h);
  Alcotest.(check (option string)) "get path" (Some "/") (Headers.get_pseudo "path" h);
  Alcotest.(check (option string)) "get missing" None (Headers.get_pseudo "scheme" h);

  Alcotest.(check bool) "mem method" true (Headers.mem_pseudo "method" h);
  Alcotest.(check bool) "mem path" true (Headers.mem_pseudo "path" h);
  Alcotest.(check bool) "mem scheme" false (Headers.mem_pseudo "scheme" h);

  let h = Headers.remove_pseudo "method" h in
  Alcotest.(check (option string)) "removed method" None (Headers.get_pseudo "method" h);
  Alcotest.(check bool) "still has path" true (Headers.mem_pseudo "path" h)

let test_has_pseudo_headers () =
  let h1 = Headers.empty in
  Alcotest.(check bool) "empty has no pseudos" false (Headers.has_pseudo_headers h1);

  let h2 = Headers.set_pseudo "method" "GET" h1 in
  Alcotest.(check bool) "has pseudos" true (Headers.has_pseudo_headers h2);

  let h3 = Headers.set `Content_type "text/html" Headers.empty in
  Alcotest.(check bool) "regular only has no pseudos" false (Headers.has_pseudo_headers h3)

let test_pseudo_and_regular_headers () =
  let h = Headers.empty
    |> Headers.set_pseudo "method" "GET"
    |> Headers.set_pseudo "path" "/"
    |> Headers.set `Content_type "text/html"
    |> Headers.set `Accept "application/json"
  in

  let pseudos = Headers.pseudo_headers h in
  let regulars = Headers.regular_headers h in

  Alcotest.(check int) "2 pseudo headers" 2 (List.length pseudos);
  Alcotest.(check int) "2 regular headers" 2 (List.length regulars);

  (* Check pseudo headers don't have colon prefix in returned names *)
  let pseudo_names = List.map fst pseudos in
  Alcotest.(check bool) "method in pseudos" true (List.mem "method" pseudo_names);
  Alcotest.(check bool) "path in pseudos" true (List.mem "path" pseudo_names)

let test_to_list_ordered () =
  let h = Headers.empty
    |> Headers.set `Content_type "text/html"  (* regular, added first *)
    |> Headers.set_pseudo "method" "GET"      (* pseudo *)
    |> Headers.set `Accept "application/json" (* regular *)
    |> Headers.set_pseudo "path" "/"          (* pseudo *)
  in

  let ordered = Headers.to_list_ordered h in
  (* Pseudo-headers should come first *)
  let first_name, _ = List.hd ordered in
  Alcotest.(check bool) "first header is pseudo"
    true (Headers.is_pseudo_header first_name);

  (* Check all pseudo headers come before regular headers *)
  let rec check_order saw_regular = function
    | [] -> true
    | (name, _) :: rest ->
        if Headers.is_pseudo_header name then
          if saw_regular then false
          else check_order false rest
        else
          check_order true rest
  in
  Alcotest.(check bool) "correct order" true (check_order false ordered)

let test_h2_request () =
  let h = Headers.empty
    |> Headers.h2_request ~meth:"GET" ~scheme:"https" ~authority:"example.com" ~path:"/"
    |> Headers.set `Accept "text/html"
  in

  Alcotest.(check (option string)) ":method" (Some "GET") (Headers.get_pseudo "method" h);
  Alcotest.(check (option string)) ":scheme" (Some "https") (Headers.get_pseudo "scheme" h);
  Alcotest.(check (option string)) ":authority" (Some "example.com") (Headers.get_pseudo "authority" h);
  Alcotest.(check (option string)) ":path" (Some "/") (Headers.get_pseudo "path" h)

let test_validate_h2_request_valid () =
  let h = Headers.empty
    |> Headers.h2_request ~meth:"GET" ~scheme:"https" ~path:"/"
    |> Headers.set_string "accept" "text/html"
  in
  match Headers.validate_h2_request h with
  | Ok () -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Ok, got: %a" Headers.pp_h2_validation_error e)

let test_validate_h2_request_missing_method () =
  let h = Headers.empty
    |> Headers.set_pseudo "scheme" "https"
    |> Headers.set_pseudo "path" "/"
  in
  match Headers.validate_h2_request h with
  | Error (Headers.Missing_pseudo "method") -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Missing_pseudo method, got: %a"
        Headers.pp_h2_validation_error e)
  | Ok () -> Alcotest.fail "Expected error"

let test_validate_h2_request_uppercase_forbidden () =
  let h = Headers.empty
    |> Headers.h2_request ~meth:"GET" ~scheme:"https" ~path:"/"
    |> Headers.set_string "Content-Type" "text/html"  (* uppercase! *)
  in
  match Headers.validate_h2_request h with
  | Error (Headers.Uppercase_header_name _) -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Uppercase_header_name, got: %a"
        Headers.pp_h2_validation_error e)
  | Ok () -> Alcotest.fail "Expected error for uppercase header"

let test_validate_h2_request_connection_forbidden () =
  let h = Headers.empty
    |> Headers.h2_request ~meth:"GET" ~scheme:"https" ~path:"/"
    |> Headers.set_string "connection" "keep-alive"  (* forbidden in H2, lowercase *)
  in
  match Headers.validate_h2_request h with
  | Error Headers.Connection_header_forbidden -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Connection_header_forbidden, got: %a"
        Headers.pp_h2_validation_error e)
  | Ok () -> Alcotest.fail "Expected error for Connection header"

let test_validate_h2_response_valid () =
  let h = Headers.empty
    |> Headers.set_pseudo "status" "200"
    |> Headers.set_string "content-type" "text/html"
  in
  match Headers.validate_h2_response h with
  | Ok () -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Ok, got: %a" Headers.pp_h2_validation_error e)

let test_validate_h2_response_missing_status () =
  let h = Headers.empty
    |> Headers.set_string "content-type" "text/html"
  in
  match Headers.validate_h2_response h with
  | Error (Headers.Missing_pseudo "status") -> ()
  | Error e ->
      Alcotest.fail (Format.asprintf "Expected Missing_pseudo status, got: %a"
        Headers.pp_h2_validation_error e)
  | Ok () -> Alcotest.fail "Expected error"

let test_remove_h2_forbidden () =
  let h = Headers.empty
    |> Headers.set `Connection "keep-alive"
    |> Headers.set `Transfer_encoding "chunked"
    |> Headers.set `Content_type "text/html"
  in
  let h' = Headers.remove_h2_forbidden h in

  Alcotest.(check bool) "Connection removed" false (Headers.mem `Connection h');
  Alcotest.(check bool) "Transfer-Encoding removed" false (Headers.mem `Transfer_encoding h');
  Alcotest.(check bool) "Content-Type kept" true (Headers.mem `Content_type h')

let headers_tests = [
  "is_pseudo_header", `Quick, test_is_pseudo_header;
  "pseudo_header_operations", `Quick, test_pseudo_header_operations;
  "has_pseudo_headers", `Quick, test_has_pseudo_headers;
  "pseudo_and_regular_headers", `Quick, test_pseudo_and_regular_headers;
  "to_list_ordered", `Quick, test_to_list_ordered;
  "h2_request", `Quick, test_h2_request;
  "validate_h2_request_valid", `Quick, test_validate_h2_request_valid;
  "validate_h2_request_missing_method", `Quick, test_validate_h2_request_missing_method;
  "validate_h2_request_uppercase_forbidden", `Quick, test_validate_h2_request_uppercase_forbidden;
  "validate_h2_request_connection_forbidden", `Quick, test_validate_h2_request_connection_forbidden;
  "validate_h2_response_valid", `Quick, test_validate_h2_response_valid;
  "validate_h2_response_missing_status", `Quick, test_validate_h2_response_missing_status;
  "remove_h2_forbidden", `Quick, test_remove_h2_forbidden;
]

(* ============================================================
   Error H2 Tests
   ============================================================ *)

let test_h2_error_codes () =
  Alcotest.(check string) "NO_ERROR" "NO_ERROR" (Error.h2_error_code_name 0x0l);
  Alcotest.(check string) "PROTOCOL_ERROR" "PROTOCOL_ERROR" (Error.h2_error_code_name 0x1l);
  Alcotest.(check string) "INTERNAL_ERROR" "INTERNAL_ERROR" (Error.h2_error_code_name 0x2l);
  Alcotest.(check string) "FLOW_CONTROL_ERROR" "FLOW_CONTROL_ERROR" (Error.h2_error_code_name 0x3l);
  Alcotest.(check string) "SETTINGS_TIMEOUT" "SETTINGS_TIMEOUT" (Error.h2_error_code_name 0x4l);
  Alcotest.(check string) "STREAM_CLOSED" "STREAM_CLOSED" (Error.h2_error_code_name 0x5l);
  Alcotest.(check string) "FRAME_SIZE_ERROR" "FRAME_SIZE_ERROR" (Error.h2_error_code_name 0x6l);
  Alcotest.(check string) "REFUSED_STREAM" "REFUSED_STREAM" (Error.h2_error_code_name 0x7l);
  Alcotest.(check string) "CANCEL" "CANCEL" (Error.h2_error_code_name 0x8l);
  Alcotest.(check string) "COMPRESSION_ERROR" "COMPRESSION_ERROR" (Error.h2_error_code_name 0x9l);
  Alcotest.(check string) "CONNECT_ERROR" "CONNECT_ERROR" (Error.h2_error_code_name 0xal);
  Alcotest.(check string) "ENHANCE_YOUR_CALM" "ENHANCE_YOUR_CALM" (Error.h2_error_code_name 0xbl);
  Alcotest.(check string) "INADEQUATE_SECURITY" "INADEQUATE_SECURITY" (Error.h2_error_code_name 0xcl);
  Alcotest.(check string) "HTTP_1_1_REQUIRED" "HTTP_1_1_REQUIRED" (Error.h2_error_code_name 0xdl)

let test_is_h2_error () =
  let protocol_err = Error.H2_protocol_error { code = 0x1l; message = "test" } in
  let stream_err = Error.H2_stream_error { stream_id = 1l; code = 0x8l; message = "cancel" } in
  let timeout_err = Error.Timeout { operation = "read"; duration = None } in

  Alcotest.(check bool) "protocol error is h2" true (Error.is_h2_error protocol_err);
  Alcotest.(check bool) "stream error is h2" true (Error.is_h2_error stream_err);
  Alcotest.(check bool) "timeout is not h2" false (Error.is_h2_error timeout_err)

let test_is_h2_connection_error () =
  let protocol_err = Error.H2_protocol_error { code = 0x1l; message = "test" } in
  let stream_err = Error.H2_stream_error { stream_id = 1l; code = 0x8l; message = "cancel" } in
  let conn_flow_err = Error.H2_flow_control_error { stream_id = None } in
  let stream_flow_err = Error.H2_flow_control_error { stream_id = Some 1l } in

  Alcotest.(check bool) "protocol error is connection error" true (Error.is_h2_connection_error protocol_err);
  Alcotest.(check bool) "stream error is not connection error" false (Error.is_h2_connection_error stream_err);
  Alcotest.(check bool) "conn flow control is connection error" true (Error.is_h2_connection_error conn_flow_err);
  Alcotest.(check bool) "stream flow control is not connection error" false (Error.is_h2_connection_error stream_flow_err)

let test_is_h2_stream_error () =
  let stream_err = Error.H2_stream_error { stream_id = 1l; code = 0x8l; message = "cancel" } in
  let stream_flow_err = Error.H2_flow_control_error { stream_id = Some 1l } in
  let protocol_err = Error.H2_protocol_error { code = 0x1l; message = "test" } in

  Alcotest.(check bool) "stream error is stream error" true (Error.is_h2_stream_error stream_err);
  Alcotest.(check bool) "stream flow is stream error" true (Error.is_h2_stream_error stream_flow_err);
  Alcotest.(check bool) "protocol error is not stream error" false (Error.is_h2_stream_error protocol_err)

let test_is_h2_retryable () =
  let goaway_no_error = Error.H2_goaway { last_stream_id = 1l; code = 0x0l; debug = "" } in
  let goaway_protocol = Error.H2_goaway { last_stream_id = 1l; code = 0x1l; debug = "" } in
  let refused_stream = Error.H2_stream_error { stream_id = 1l; code = 0x7l; message = "" } in
  let cancel = Error.H2_stream_error { stream_id = 1l; code = 0x8l; message = "" } in

  Alcotest.(check bool) "GOAWAY NO_ERROR is retryable" true (Error.is_h2_retryable goaway_no_error);
  Alcotest.(check bool) "GOAWAY PROTOCOL_ERROR not retryable" false (Error.is_h2_retryable goaway_protocol);
  Alcotest.(check bool) "REFUSED_STREAM is retryable" true (Error.is_h2_retryable refused_stream);
  Alcotest.(check bool) "CANCEL not retryable" false (Error.is_h2_retryable cancel)

let test_get_h2_error_code () =
  let protocol_err = Error.H2_protocol_error { code = 0x1l; message = "test" } in
  let stream_err = Error.H2_stream_error { stream_id = 1l; code = 0x8l; message = "" } in
  let goaway = Error.H2_goaway { last_stream_id = 1l; code = 0x0l; debug = "" } in
  let timeout = Error.Timeout { operation = "read"; duration = None } in

  Alcotest.(check (option int32)) "protocol error code" (Some 0x1l) (Error.get_h2_error_code protocol_err);
  Alcotest.(check (option int32)) "stream error code" (Some 0x8l) (Error.get_h2_error_code stream_err);
  Alcotest.(check (option int32)) "goaway code" (Some 0x0l) (Error.get_h2_error_code goaway);
  Alcotest.(check (option int32)) "timeout no code" None (Error.get_h2_error_code timeout)

let test_get_h2_stream_id () =
  let stream_err = Error.H2_stream_error { stream_id = 5l; code = 0x8l; message = "" } in
  let flow_err = Error.H2_flow_control_error { stream_id = Some 3l } in
  let goaway = Error.H2_goaway { last_stream_id = 7l; code = 0x0l; debug = "" } in
  let protocol_err = Error.H2_protocol_error { code = 0x1l; message = "" } in

  Alcotest.(check (option int32)) "stream error id" (Some 5l) (Error.get_h2_stream_id stream_err);
  Alcotest.(check (option int32)) "flow control id" (Some 3l) (Error.get_h2_stream_id flow_err);
  Alcotest.(check (option int32)) "goaway last stream" (Some 7l) (Error.get_h2_stream_id goaway);
  Alcotest.(check (option int32)) "protocol error no id" None (Error.get_h2_stream_id protocol_err)

let test_h2_error_constructors () =
  (* Test that constructors don't raise *)
  let _ = Error.h2_protocol_error ~code:0x1l ~message:"test" in
  let _ = Error.h2_stream_error ~stream_id:1l ~code:0x8l ~message:"cancel" in
  let _ = Error.h2_flow_control_error () in
  let _ = Error.h2_flow_control_error ~stream_id:1l () in
  let _ = Error.h2_compression_error ~message:"bad header" in
  let _ = Error.h2_settings_timeout () in
  let _ = Error.h2_goaway ~last_stream_id:0l ~code:0x0l ~debug:"shutting down" in
  let _ = Error.h2_frame_error ~frame_type:0 ~message:"invalid" in
  let _ = Error.h2_header_validation_error ~message:"uppercase" in
  ()

let error_tests = [
  "h2_error_codes", `Quick, test_h2_error_codes;
  "is_h2_error", `Quick, test_is_h2_error;
  "is_h2_connection_error", `Quick, test_is_h2_connection_error;
  "is_h2_stream_error", `Quick, test_is_h2_stream_error;
  "is_h2_retryable", `Quick, test_is_h2_retryable;
  "get_h2_error_code", `Quick, test_get_h2_error_code;
  "get_h2_stream_id", `Quick, test_get_h2_stream_id;
  "h2_error_constructors", `Quick, test_h2_error_constructors;
]

(* ============================================================
   Main
   ============================================================ *)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Warning);
  Alcotest.run "HTTP/2 Types" [
    "Http_version", http_version_tests;
    "Headers (H2)", headers_tests;
    "Error (H2)", error_tests;
  ]
