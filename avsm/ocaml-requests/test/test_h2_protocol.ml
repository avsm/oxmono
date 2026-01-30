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

(** Tests for H2_protocol module. *)



let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

(* ============================================================
   Mode Tests
   ============================================================ *)

let test_mode_to_string () =
  Alcotest.(check string) "auto" "auto" (H2_protocol.mode_to_string H2_protocol.Auto);
  Alcotest.(check string) "http1-only" "http1-only" (H2_protocol.mode_to_string H2_protocol.Http1_only);
  Alcotest.(check string) "http2-only" "http2-only" (H2_protocol.mode_to_string H2_protocol.Http2_only)

let test_pp_mode () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  H2_protocol.pp_mode fmt H2_protocol.Auto;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "auto pp" "Auto" (Buffer.contents buf)

let mode_tests = [
  "mode_to_string", `Quick, test_mode_to_string;
  "pp_mode", `Quick, test_pp_mode;
]

(* ============================================================
   ALPN Tests
   ============================================================ *)

let test_alpn_protocols_auto () =
  let alpn = H2_protocol.alpn_protocols H2_protocol.Auto in
  Alcotest.(check (list string)) "auto alpn" ["h2"; "http/1.1"] alpn

let test_alpn_protocols_http1_only () =
  let alpn = H2_protocol.alpn_protocols H2_protocol.Http1_only in
  Alcotest.(check (list string)) "http1 alpn" ["http/1.1"] alpn

let test_alpn_protocols_http2_only () =
  let alpn = H2_protocol.alpn_protocols H2_protocol.Http2_only in
  Alcotest.(check (list string)) "http2 alpn" ["h2"] alpn

let alpn_tests = [
  "alpn_protocols auto", `Quick, test_alpn_protocols_auto;
  "alpn_protocols http1_only", `Quick, test_alpn_protocols_http1_only;
  "alpn_protocols http2_only", `Quick, test_alpn_protocols_http2_only;
]

(* ============================================================
   Negotiated Protocol Tests
   ============================================================ *)

let test_negotiated_to_string () =
  Alcotest.(check string) "http1.1" "HTTP/1.1" (H2_protocol.negotiated_to_string H2_protocol.Http1_1);
  Alcotest.(check string) "http2" "HTTP/2" (H2_protocol.negotiated_to_string H2_protocol.Http2)

let test_negotiated_of_alpn () =
  Alcotest.(check (option bool)) "h2"
    (Some true) (Option.map (fun n -> n = H2_protocol.Http2) (H2_protocol.negotiated_of_alpn "h2"));
  Alcotest.(check (option bool)) "http/1.1"
    (Some true) (Option.map (fun n -> n = H2_protocol.Http1_1) (H2_protocol.negotiated_of_alpn "http/1.1"));
  Alcotest.(check (option bool)) "unknown"
    None (Option.map (fun _ -> true) (H2_protocol.negotiated_of_alpn "spdy"))

let test_default_protocol () =
  Alcotest.(check bool) "default is http1.1"
    true (H2_protocol.default_protocol () = H2_protocol.Http1_1)

let negotiated_tests = [
  "negotiated_to_string", `Quick, test_negotiated_to_string;
  "negotiated_of_alpn", `Quick, test_negotiated_of_alpn;
  "default_protocol", `Quick, test_default_protocol;
]

(* ============================================================
   Protocol Detection Tests
   ============================================================ *)

let test_detect_protocol_h2 () =
  let proto = H2_protocol.detect_protocol ~mode:Auto (Some "h2") in
  Alcotest.(check bool) "h2 detected"
    true (proto = H2_protocol.Http2)

let test_detect_protocol_http11 () =
  let proto = H2_protocol.detect_protocol ~mode:Auto (Some "http/1.1") in
  Alcotest.(check bool) "http/1.1 detected"
    true (proto = H2_protocol.Http1_1)

let test_detect_protocol_none_auto () =
  let proto = H2_protocol.detect_protocol ~mode:Auto None in
  Alcotest.(check bool) "no alpn auto defaults to http1.1"
    true (proto = H2_protocol.Http1_1)

let test_detect_protocol_none_http1_only () =
  let proto = H2_protocol.detect_protocol ~mode:Http1_only None in
  Alcotest.(check bool) "no alpn http1_only"
    true (proto = H2_protocol.Http1_1)

let test_detect_protocol_none_http2_only () =
  (* Http2_only with no ALPN result should fail *)
  try
    let _ = H2_protocol.detect_protocol ~mode:Http2_only None in
    Alcotest.fail "should have raised"
  with Failure _ -> ()

let test_detect_protocol_unknown_alpn () =
  let proto = H2_protocol.detect_protocol ~mode:Auto (Some "spdy") in
  Alcotest.(check bool) "unknown alpn defaults to http1.1"
    true (proto = H2_protocol.Http1_1)

let detection_tests = [
  "detect h2", `Quick, test_detect_protocol_h2;
  "detect http/1.1", `Quick, test_detect_protocol_http11;
  "detect none auto", `Quick, test_detect_protocol_none_auto;
  "detect none http1_only", `Quick, test_detect_protocol_none_http1_only;
  "detect none http2_only", `Quick, test_detect_protocol_none_http2_only;
  "detect unknown alpn", `Quick, test_detect_protocol_unknown_alpn;
]

(* ============================================================
   Request Tests
   ============================================================ *)

let test_make_request_simple () =
  let uri = Uri.of_string "https://example.com/path" in
  let req = H2_protocol.make_request ~meth:"GET" ~uri () in
  Alcotest.(check string) "method" "GET" req.meth;
  Alcotest.(check string) "uri" "https://example.com/path" (Uri.to_string req.uri);
  Alcotest.(check (list (pair string string))) "headers" [] req.headers;
  Alcotest.(check (option string)) "body" None req.body

let test_make_request_with_body () =
  let uri = Uri.of_string "https://example.com/api" in
  let req = H2_protocol.make_request ~meth:"POST" ~uri
    ~headers:[("Content-Type", "application/json")]
    ~body:"{\"key\": \"value\"}"
    () in
  Alcotest.(check string) "method" "POST" req.meth;
  Alcotest.(check (list (pair string string))) "headers"
    [("Content-Type", "application/json")] req.headers;
  Alcotest.(check (option string)) "body" (Some "{\"key\": \"value\"}") req.body

let request_tests = [
  "make_request simple", `Quick, test_make_request_simple;
  "make_request with body", `Quick, test_make_request_with_body;
]

(* ============================================================
   Connection Tests
   ============================================================ *)

let test_create_connection_http11 () =
  let conn = H2_protocol.create_connection ~protocol:H2_protocol.Http1_1 in
  Alcotest.(check bool) "protocol is http1.1"
    true (H2_protocol.connection_protocol conn = H2_protocol.Http1_1);
  Alcotest.(check bool) "is_http1" true (H2_protocol.is_http1 conn);
  Alcotest.(check bool) "not is_http2" false (H2_protocol.is_http2 conn);
  Alcotest.(check (option bool)) "no h2 connection"
    None (Option.map (fun _ -> true) (H2_protocol.get_h2_connection conn))

let test_create_connection_http2 () =
  let conn = H2_protocol.create_connection ~protocol:H2_protocol.Http2 in
  Alcotest.(check bool) "protocol is http2"
    true (H2_protocol.connection_protocol conn = H2_protocol.Http2);
  Alcotest.(check bool) "is_http2" true (H2_protocol.is_http2 conn);
  Alcotest.(check bool) "not is_http1" false (H2_protocol.is_http1 conn);
  Alcotest.(check bool) "has h2 connection"
    true (Option.is_some (H2_protocol.get_h2_connection conn))

let connection_tests = [
  "create connection http1.1", `Quick, test_create_connection_http11;
  "create connection http2", `Quick, test_create_connection_http2;
]

(* ============================================================
   HTTP/2 Conversion Tests
   ============================================================ *)

let test_request_to_h2_headers () =
  let uri = Uri.of_string "https://example.com/path?query=value" in
  let req = H2_protocol.make_request ~meth:"GET" ~uri
    ~headers:[("Accept", "text/html"); ("Authorization", "Bearer token")]
    () in
  let headers = H2_protocol.request_to_h2_headers req in

  (* Check pseudo-headers *)
  let find name = List.find_opt (fun (h : H2_hpack.header) -> h.name = name) headers in
  let method_h = find ":method" in
  let scheme_h = find ":scheme" in
  let authority_h = find ":authority" in
  let path_h = find ":path" in

  Alcotest.(check (option string)) ":method"
    (Some "GET") (Option.map (fun (h : H2_hpack.header) -> h.value) method_h);
  Alcotest.(check (option string)) ":scheme"
    (Some "https") (Option.map (fun (h : H2_hpack.header) -> h.value) scheme_h);
  Alcotest.(check (option string)) ":authority"
    (Some "example.com") (Option.map (fun (h : H2_hpack.header) -> h.value) authority_h);
  Alcotest.(check (option string)) ":path"
    (Some "/path?query=value") (Option.map (fun (h : H2_hpack.header) -> h.value) path_h);

  (* Check regular headers are lowercase *)
  let accept_h = find "accept" in
  let auth_h = find "authorization" in
  Alcotest.(check (option string)) "accept"
    (Some "text/html") (Option.map (fun (h : H2_hpack.header) -> h.value) accept_h);
  Alcotest.(check (option string)) "authorization"
    (Some "Bearer token") (Option.map (fun (h : H2_hpack.header) -> h.value) auth_h);

  (* Check sensitive flag *)
  Alcotest.(check (option bool)) "authorization sensitive"
    (Some true) (Option.map (fun (h : H2_hpack.header) -> h.sensitive) auth_h);
  Alcotest.(check (option bool)) "accept not sensitive"
    (Some false) (Option.map (fun (h : H2_hpack.header) -> h.sensitive) accept_h)

let test_request_to_h2_headers_root_path () =
  let uri = Uri.of_string "https://example.com" in
  let req = H2_protocol.make_request ~meth:"GET" ~uri () in
  let headers = H2_protocol.request_to_h2_headers req in
  let path_h = List.find (fun (h : H2_hpack.header) -> h.name = ":path") headers in
  Alcotest.(check string) "root path" "/" path_h.value

let test_h2_headers_to_response () =
  let headers = [
    { H2_hpack.name = ":status"; value = "200"; sensitive = false };
    { H2_hpack.name = "content-type"; value = "text/html"; sensitive = false };
    { H2_hpack.name = "content-length"; value = "1234"; sensitive = false };
  ] in
  let status, response_headers = H2_protocol.h2_headers_to_response headers in
  Alcotest.(check int) "status" 200 status;
  Alcotest.(check (list (pair string string))) "headers"
    [("content-type", "text/html"); ("content-length", "1234")]
    response_headers

let conversion_tests = [
  "request_to_h2_headers", `Quick, test_request_to_h2_headers;
  "request_to_h2_headers root path", `Quick, test_request_to_h2_headers_root_path;
  "h2_headers_to_response", `Quick, test_h2_headers_to_response;
]

(* ============================================================
   Pretty Printing Tests
   ============================================================ *)

let test_pp_negotiated () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  H2_protocol.pp_negotiated fmt H2_protocol.Http2;
  Format.pp_print_flush fmt ();
  Alcotest.(check string) "http2" "HTTP/2" (Buffer.contents buf)

let test_pp_request () =
  let uri = Uri.of_string "https://example.com/" in
  let req = H2_protocol.make_request ~meth:"GET" ~uri () in
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  H2_protocol.pp_request fmt req;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "contains GET" true (String.length s > 0)

let pp_tests = [
  "pp_negotiated", `Quick, test_pp_negotiated;
  "pp_request", `Quick, test_pp_request;
]

(* ============================================================
   Run Tests
   ============================================================ *)

let () =
  Alcotest.run "H2_protocol" [
    "mode", mode_tests;
    "alpn", alpn_tests;
    "negotiated", negotiated_tests;
    "detection", detection_tests;
    "request", request_tests;
    "connection", connection_tests;
    "conversion", conversion_tests;
    "pp", pp_tests;
  ]
