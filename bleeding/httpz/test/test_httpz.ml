open Base

module I64 = Stdlib_upstream_compatible.Int64_u

let limits = Httpz.default_limits
let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

let copy_to_buffer buf s =
  let len = String.length s in
  for i = 0 to len - 1 do
    Bytes.set buf i (String.get s i)
  done;
  len
;;

let parse_ok buf request = exclave_
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  if Poly.( <> ) status Httpz.Buf_read.Complete
  then failwith (Printf.sprintf "Expected Ok, got %s" (Httpz.Buf_read.status_to_string status));
  #(len, buf, req, headers)
;;

let parse_status buf request =
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) =
    Httpz.parse buf ~len:(i16 len) ~limits
  in
  status
;;

let test_simple_get () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "GET /index.html HTTP/1.1\r\nHost: example.com\r\nContent-Length: 0\r\n\r\n"
  in
  let #(_len, parse_buf, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#meth Httpz.Method.Get);
  assert (Httpz.Span.equal parse_buf req.#target "/index.html");
  assert (Poly.( = ) req.#version Httpz.Version.Http_1_1);
  assert (I64.equal req.#content_length #0L);
  assert (List.length headers = 1);
  (match headers with
   | [ hdr0 ] ->
     assert (Poly.( = ) hdr0.Httpz.Header.name Httpz.Header.Name.Host);
     assert (Httpz.Span.equal parse_buf hdr0.Httpz.Header.value "example.com")
   | _ -> assert false);
  Stdio.printf "test_simple_get: PASSED\n"
;;

let test_post_with_body () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     Content-Length: 13\r\n\
     \r\n\
     {\"key\":\"val\"}"
  in
  let #(len, parse_buf, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#meth Httpz.Method.Post);
  assert (Httpz.Span.equal parse_buf req.#target "/api/data");
  assert (Poly.( = ) req.#version Httpz.Version.Http_1_1);
  assert (List.length headers = 2);
  assert (to_int req.#body_off = len - 13);
  assert (I64.equal req.#content_length #13L);
  Stdio.printf "test_post_with_body: PASSED\n"
;;

let test_unknown_method () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "PURGE /cache HTTP/1.1\r\nHost: cdn.example.com\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Unsupported_method);
  let malformed = "GE(T /cache HTTP/1.1\r\nHost: cdn.example.com\r\n\r\n" in
  let len = copy_to_buffer buf malformed in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Invalid_method);
  Stdio.printf "test_unknown_method: PASSED\n"
;;

let test_unknown_header () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "GET / HTTP/1.1\r\nHost: example.com\r\nX-Custom-Header: custom-value\r\n\r\n"
  in
  let #(_len, parse_buf, _req, headers) = parse_ok buf request in
  assert (List.length headers = 2);
  (match headers with
   | [ hdr0; _ ] ->
     (match hdr0.Httpz.Header.name with
      | Httpz.Header.Name.Other ->
        assert (
          Httpz.Span.equal_caseless parse_buf hdr0.Httpz.Header.name_span "x-custom-header")
      | _ -> assert false);
     assert (Httpz.Span.equal parse_buf hdr0.Httpz.Header.value "custom-value")
   | _ -> assert false);
  Stdio.printf "test_unknown_header: PASSED\n"
;;

let test_x_request_id_header_name () =
  let wire = Httpz.Header_name.canonical Httpz.Header_name.X_request_id in
  assert (String.equal wire "X-Request-Id");
  let buf = Bytes.of_string wire in
  let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length wire)) in
  assert (
    Poly.( = )
      (Httpz.Header_name.of_span buf span)
      Httpz.Header_name.X_request_id);
  Stdio.printf "test_x_request_id_header_name: PASSED\n"
;;

let test_additional_standard_header_names () =
  List.iter
    [ "TE", Httpz.Header_name.Te;
      "Trailer", Httpz.Header_name.Trailer;
      "Max-Forwards", Httpz.Header_name.Max_forwards;
      "Proxy-Authenticate", Httpz.Header_name.Proxy_authenticate;
      "Proxy-Authorization", Httpz.Header_name.Proxy_authorization ]
    ~f:(fun (wire, expected) ->
      let buf = Bytes.of_string wire in
      let span = Httpz.Span.make ~off:(i16 0) ~len:(i16 (String.length wire)) in
      assert (Poly.( = ) (Httpz.Header_name.of_span buf span) expected);
      assert (String.equal (Httpz.Header_name.canonical expected) wire));
  Stdio.printf "test_additional_standard_header_names: PASSED\n"
;;

let test_partial () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "GET /index.html HTTP/1.1\r\nHost: exam" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Partial);
  Stdio.printf "test_partial: PASSED\n"
;;

let test_http10 () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#version Httpz.Version.Http_1_0);
  assert (List.length headers = 0);
  Stdio.printf "test_http10: PASSED\n"
;;

let test_higher_minor_request_version () =
  let buf = Bytes.create Httpz.buffer_size in
  let len =
    copy_to_buffer buf "GET / HTTP/1.2\r\nHost: example.com\r\n\r\n"
  in
  let #(status, req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Complete);
  assert (Poly.( = ) req.#version Httpz.Version.Http_1_1);
  Stdio.printf "test_higher_minor_request_version: PASSED\n"
;;

let test_leading_empty_request_lines () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "\r\n\r\nGET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Complete);
  assert (Poly.( = ) req.#meth Httpz.Method.Get);
  let len = copy_to_buffer buf "\r" in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Partial);
  let eight = String.concat ~sep:"" (List.init 8 ~f:(fun _ -> "\r\n")) in
  assert (
    Poly.( = )
      (parse_status buf (eight ^ "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"))
      Httpz.Buf_read.Complete);
  assert (
    Poly.( = )
      (parse_status buf
         ("\r\n" ^ eight ^ "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"))
      Httpz.Buf_read.Invalid_method);
  Stdio.printf "test_leading_empty_request_lines: PASSED\n"
;;

let test_impossible_version_prefix () =
  let buf = Bytes.create Httpz.buffer_size in
  assert (
    Poly.( = )
      (parse_status buf "GET / \r\n")
      Httpz.Buf_read.Invalid_version);
  let len = copy_to_buffer buf "GET / HT" in
  let #(status, _, _) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Partial);
  Stdio.printf "test_impossible_version_prefix: PASSED\n"
;;

let test_error_result_closes () =
  let buf = Bytes.create Httpz.buffer_size in
  let wire =
    "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 1\r\n\
     Transfer-Encoding: chunked\r\n\r\n"
  in
  let len = copy_to_buffer buf wire in
  let #(status, req, _) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Ambiguous_framing);
  assert (not req.#keep_alive);
  Stdio.printf "test_error_result_closes: PASSED\n"
;;

let test_huge_body_window_math () =
  let buf = Bytes.create Httpz.buffer_size in
  let permissive =
    #{ limits with
       Httpz.Buf_read.max_content_length = #9223372036854775807L }
  in
  let wire =
    "POST / HTTP/1.1\r\nHost: x\r\n\
     Content-Length: 9223372036854775807\r\n\r\n"
  in
  let len = copy_to_buffer buf wire in
  let #(status, req, _) = Httpz.parse buf ~len:(i16 len) ~limits:permissive in
  assert (Poly.( = ) status Httpz.Buf_read.Complete);
  assert (not (Httpz.Req.body_in_buffer ~len:(i16 len) req));
  assert (Httpz.Span.len (Httpz.Req.body_span ~len:(i16 len) req) = -1);
  assert (to_int (Httpz.Req.body_bytes_needed ~len:(i16 len) req) > 0);
  Stdio.printf "test_huge_body_window_math: PASSED\n"
;;

let test_keep_alive () =
  let buf = Bytes.create Httpz.buffer_size in
  let request1 = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let #(_len1, _parse_buf1, req1, _headers1) = parse_ok buf request1 in
  assert req1.#keep_alive;
  let request2 = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len2, _parse_buf2, req2, _headers2) = parse_ok buf request2 in
  assert (not req2.#keep_alive);
  Stdio.printf "test_keep_alive: PASSED\n"
;;

let test_connection_upgrade () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: keep-alive, Upgrade\r\nUpgrade: echo\r\n\r\n"
  in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert req.#connection_upgrade;
  let rec has_upgrade (local_ headers) =
    match headers with
    | [] -> false
    | (h : Httpz.Header.t) :: rest ->
      Poly.(h.name = Httpz.Header_name.Upgrade) || has_upgrade rest
  in
  assert (has_upgrade headers);
  let request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert (not req.#connection_upgrade);
  List.iter [ "Upgrade, bad/value"; "Upgrade, bad option"; "\"Upgrade\"" ]
    ~f:(fun value ->
      let request =
        Printf.sprintf
          "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: %s\r\n\r\n" value
      in
      assert (Poly.( = ) (parse_status buf request) Httpz.Buf_read.Invalid_header));
  List.iter [ "", false; " , ", false; ",,Upgrade,,", true ]
    ~f:(fun (value, expected_upgrade) ->
      let request =
        Printf.sprintf
          "GET / HTTP/1.1\r\nHost: example.com\r\nConnection: %s\r\n\r\n" value
      in
      let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
      assert (Bool.equal req.#connection_upgrade expected_upgrade));
  Stdio.printf "test_connection_upgrade: PASSED\n"
;;

let test_upgrade_syntax () =
  let module U = Httpz.Upgrade in
  List.iter [ "websocket"; "IRC/6.9"; "chat/V1"; "!#$%&'*+-.^_`|~" ]
    ~f:(fun protocol -> assert (U.valid_protocol protocol));
  List.iter [ ""; "/v1"; "chat/"; "chat/v/2"; "chat v1"; "chat," ]
    ~f:(fun protocol -> assert (not (U.valid_protocol protocol)));
  assert (U.valid_protocol_list "websocket, IRC/6.9");
  List.iter [ ""; ",websocket"; "websocket,"; "websocket,,IRC/6.9"; "chat/v/2" ]
    ~f:(fun protocols -> assert (not (U.valid_protocol_list protocols)));
  assert (U.matches_offer ~offer:"websocket, IRC/6.9" ~selected:"irc/6.9");
  assert (U.matches_offer ~offer:", websocket,, IRC/6.9," ~selected:"irc/6.9");
  assert (not (U.matches_offer ~offer:"CHAT/V1" ~selected:"chat/v1"));
  assert (not (U.matches_offer ~offer:"notwebsocket" ~selected:"websocket"));
  assert
    (not (U.matches_offer ~offer:"websocket, bad/value/more" ~selected:"websocket"));
  Stdio.printf "test_upgrade_syntax: PASSED\n"
;;

let test_chunked () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert req.#is_chunked;
  assert (List.length headers = 1);
  let request = "POST /upload HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Unsupported_transfer_encoding);
  Stdio.printf "test_chunked: PASSED\n"
;;

let test_find_header () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "GET / HTTP/1.1\r\nHost: example.com\r\nAccept: text/html\r\n\r\n" in
  let #(_len, parse_buf, _req, headers) = parse_ok buf request in
  (match Httpz.Header.find headers Httpz.Header.Name.Host with
   | Some hdr -> assert (Httpz.Span.equal parse_buf hdr.Httpz.Header.value "example.com")
   | None -> assert false);
  (match Httpz.Header.find headers Httpz.Header.Name.Content_length with
   | Some _ -> assert false
   | None -> ());
  Stdio.printf "test_find_header: PASSED\n"
;;

let test_find_header_string () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "GET / HTTP/1.1\r\nHost: example.com\r\nX-Trace-Id: abc123\r\n\r\n"
  in
  let #(_len, parse_buf, _req, headers) = parse_ok buf request in
  (match Httpz.Header.find_string parse_buf headers "HoSt" with
   | Some hdr -> assert (Httpz.Span.equal parse_buf hdr.Httpz.Header.value "example.com")
   | None -> assert false);
  (match Httpz.Header.find_string parse_buf headers "x-TRACE-id" with
   | Some hdr -> assert (Httpz.Span.equal parse_buf hdr.Httpz.Header.value "abc123")
   | None -> assert false);
  (match Httpz.Header.find_string parse_buf headers "x-missing" with
   | Some _ -> assert false
   | None -> ());
  Stdio.printf "test_find_header_string: PASSED\n"
;;

let test_missing_host_http11 () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "GET / HTTP/1.1\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Missing_host_header);
  Stdio.printf "test_missing_host_http11: PASSED\n"
;;

let test_ambiguous_framing () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 10\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Ambiguous_framing);
  Stdio.printf "test_ambiguous_framing: PASSED\n"
;;

let test_content_length_overflow () =
  let buf = Bytes.create Httpz.buffer_size in
  let small_limits = #{ Httpz.Buf_read.max_content_length = #1000L
                      ; max_header_size = i16 16384
                      ; max_header_count = i16 100
                      ; max_chunk_size = 16777216
                      ; max_target_length = i16 8192
                      } in
  let request = "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 1000000\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits:small_limits in
  assert (Poly.( = ) status Httpz.Buf_read.Content_length_overflow);
  Stdio.printf "test_content_length_overflow: PASSED\n"
;;

let test_request_body_bytes_in_buffer () =
  (* Body bytes after the head do not count against the header-size limit,
     however many of them arrived in the same read. *)
  let buf = Bytes.create Httpz.buffer_size in
  let body = String.make 20000 'x' in
  let #(len, _parse_buf, req, _headers) =
    parse_ok
      buf
      ("POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 20000\r\n\r\n"
       ^ body)
  in
  assert (I64.equal req.#content_length (I64.of_int64 20000L));
  assert (to_int req.#body_off = len - 20000);
  Stdio.printf "test_request_body_bytes_in_buffer: PASSED\n"
;;

let test_request_header_block_too_large () =
  let buf = Bytes.create Httpz.buffer_size in
  let strict = #{ limits with max_header_size = i16 64 } in
  let request = "GET / HTTP/1.1\r\nX-Pad: " ^ String.make 100 'p' ^ "\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits:strict in
  assert (Poly.( = ) status Httpz.Buf_read.Headers_too_large);
  Stdio.printf "test_request_header_block_too_large: PASSED\n"
;;

let test_request_header_block_boundary () =
  (* The limit bounds the head exactly: a head of [max_header_size] bytes is
     accepted, one byte more is not, and trailing body bytes never count. *)
  let buf = Bytes.create Httpz.buffer_size in
  let head pad =
    "GET / HTTP/1.1\r\nHost: x\r\nX-Pad: " ^ String.make pad 'p' ^ "\r\n\r\n"
  in
  let exact = String.length (head 100) in
  let strict = #{ limits with max_header_size = i16 exact } in
  let status request =
    let len = copy_to_buffer buf request in
    let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits:strict in
    status
  in
  assert (Poly.( = ) (status (head 100)) Httpz.Buf_read.Complete);
  assert (Poly.( = ) (status (head 101)) Httpz.Buf_read.Headers_too_large);
  assert (
    Poly.( = ) (status (head 100 ^ String.make 500 'b')) Httpz.Buf_read.Complete);
  Stdio.printf "test_request_header_block_boundary: PASSED\n"
;;

let test_host_authority_cross_check () =
  let buf = Bytes.create Httpz.buffer_size in
  let bad request =
    assert (Poly.( = ) (parse_status buf request) Httpz.Buf_read.Invalid_header)
  and ok request =
    assert (Poly.( = ) (parse_status buf request) Httpz.Buf_read.Complete)
  in
  (* RFC 9110 4.2.1: no http(s) URI has an empty host, so an empty Host names
     nothing and RFC 9112 3.2 asks for a 400. *)
  bad "GET / HTTP/1.1\r\nHost:\r\n\r\n";
  bad "GET / HTTP/1.1\r\nHost: \r\n\r\n";
  (* RFC 9112 3.2.2: the absolute-form authority and Host must agree. *)
  bad "GET http://b/ HTTP/1.1\r\nHost: a\r\n\r\n";
  ok "GET http://b/ HTTP/1.1\r\nHost: B\r\n\r\n";
  ok "GET http://B/ HTTP/1.1\r\nHost: b\r\n\r\n";
  ok "GET http://b:8080/ HTTP/1.1\r\nHost: b:8080\r\n\r\n";
  bad "GET http://b:8080/ HTTP/1.1\r\nHost: b\r\n\r\n";
  (* A scheme default port is not supplied on either side. *)
  bad "GET http://b/ HTTP/1.1\r\nHost: b:80\r\n\r\n";
  ok "GET http://[::1]:443/ HTTP/1.1\r\nHost: [::1]:443\r\n\r\n";
  bad "GET http://[::1]/ HTTP/1.1\r\nHost: [::2]\r\n\r\n";
  (* Origin-form carries no authority, so Host stands alone. *)
  ok "GET / HTTP/1.1\r\nHost: anything.example\r\n\r\n";
  Stdio.printf "test_host_authority_cross_check: PASSED\n"
;;

let test_bare_cr () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "GET / HTTP/1.1\r\nHost: example\rcom\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Bare_cr_detected);
  Stdio.printf "test_bare_cr: PASSED\n"
;;

let test_unsupported_transfer_encoding () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: gzip, chunked\r\n\r\n"
  in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Unsupported_transfer_encoding);
  Stdio.printf "test_unsupported_transfer_encoding: PASSED\n"
;;

let test_transfer_encoding_identity () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: identity\r\n\r\n"
  in
  assert
    (Poly.( = )
       (parse_status buf request)
       Httpz.Buf_read.Ambiguous_framing);
  Stdio.printf "test_transfer_encoding_identity: PASSED\n"
;;

let test_strict_content_length () =
  let buf = Bytes.create Httpz.buffer_size in
  List.iter
    [ "abc"; "+5"; "-5"; "12x"; "1_0"; "0x10"; "5 5"; "" ]
    ~f:(fun value ->
      let request =
        Printf.sprintf
          "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: %s\r\n\r\n"
          value
      in
      assert
        (Poly.( = )
           (parse_status buf request)
           Httpz.Buf_read.Invalid_header));
  assert
    (Poly.( = )
       (parse_status buf
          "POST / HTTP/1.1\r\nHost: x\r\n\
           Content-Length: 9223372036854775808\r\n\r\n")
       Httpz.Buf_read.Content_length_overflow);
  let #(_len, _buf, req, _) =
    parse_ok buf
      "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 007\r\n\r\n"
  in
  assert (I64.equal req.#content_length #7L);
  Stdio.printf "test_strict_content_length: PASSED\n"
;;

let test_duplicate_content_length () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, _buf, req, _) =
    parse_ok buf
      "POST / HTTP/1.1\r\nHost: x\r\n\
       Content-Length: 5, 5\r\nContent-Length: 5\r\n\r\n"
  in
  assert (I64.equal req.#content_length #5L);
  List.iter
    [ "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 5, 6\r\n\r\n"
    ; "POST / HTTP/1.1\r\nHost: x\r\n\
       Content-Length: 5\r\nContent-Length: 6\r\n\r\n"
    ]
    ~f:(fun request ->
      assert
        (Poly.( = )
           (parse_status buf request)
           Httpz.Buf_read.Ambiguous_framing));
  Stdio.printf "test_duplicate_content_length: PASSED\n"
;;

let test_framing_token_lists () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, _buf, req, _) =
    parse_ok buf
      "GET / HTTP/1.1\r\nHost: x\r\n\
       Connection: upgrade, close\r\n\r\n"
  in
  assert (not req.#keep_alive);
  let #(_len, _buf, req, _) =
    parse_ok buf
      "GET / HTTP/1.0\r\nConnection: foo, keep-alive\r\n\r\n"
  in
  assert req.#keep_alive;
  let #(_len, _buf, req, _) =
    parse_ok buf
      "GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\
       Connection: keep-alive\r\n\r\n"
  in
  assert (not req.#keep_alive);
  List.iter
    [ "POST / HTTP/1.1\r\nHost: x\r\n\
       Transfer-Encoding: chunked, chunked\r\n\r\n"
    ; "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\
       Transfer-Encoding: chunked\r\n\r\n"
    ]
    ~f:(fun request ->
      assert
        (Poly.( = )
           (parse_status buf request)
           Httpz.Buf_read.Ambiguous_framing));
  Stdio.printf "test_framing_token_lists: PASSED\n"
;;

let test_host_and_field_validation () =
  let buf = Bytes.create Httpz.buffer_size in
  List.iter
    [ "example.com"; "example.com:8080"; "[::1]"; "[::1]:8080" ]
    ~f:(fun host ->
      let request = Printf.sprintf "GET / HTTP/1.1\r\nHost: %s\r\n\r\n" host in
      assert (Poly.( = ) (parse_status buf request) Httpz.Buf_read.Complete));
  assert
    (Poly.( = )
       (parse_status buf
          "GET / HTTP/1.1\r\nHost: a.example\r\nHost: b.example\r\n\r\n")
       Httpz.Buf_read.Missing_host_header);
  assert
    (Poly.( = )
       (parse_status buf
          "GET / HTTP/1.1\r\nHost: x\nSmuggle: 1\r\n\r\n")
       Httpz.Buf_read.Bare_cr_detected);
  assert
    (Poly.( = )
       (parse_status buf
          "GET / HTTP/1.1\r\nHost: x\r\nX: a\000b\r\n\r\n")
       Httpz.Buf_read.Invalid_header);
  List.iter
    [ "localhost, attacker.invalid"; "user@localhost"; "[::1"; "x:65536" ]
    ~f:(fun host ->
      let request = Printf.sprintf "GET / HTTP/1.1\r\nHost: %s\r\n\r\n" host in
      assert
        (Poly.( = ) (parse_status buf request) Httpz.Buf_read.Invalid_header));
  Stdio.printf "test_host_and_field_validation: PASSED\n"
;;

let test_chunk_framing_hardening () =
  let buf = Bytes.create Httpz.buffer_size in
  let status ?(max_chunk_size = Int.max_value) line =
    let len = copy_to_buffer buf line in
    let #(status, _, _) =
      Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
        ~max_chunk_size
    in
    status
  in
  List.iter [ "+5\r\n"; "-1\r\n"; "0x10\r\n"; "5 5\r\n"; "5\n" ]
    ~f:(fun line ->
      assert (Poly.( = ) (status line) Httpz.Chunk.Malformed));
  assert
    (Poly.( = )
       (status ~max_chunk_size:4 "5\r\n")
       Httpz.Chunk.Chunk_too_large);
  assert
    (Poly.( = )
       (status "5 ; ext = value\r\nhello")
       Httpz.Chunk.Complete);
  let extension_header = "5;foo=bar\r\n" in
  for length = 1 to String.length extension_header - 1 do
    assert
      (Poly.( = )
         (status (String.prefix extension_header length))
         Httpz.Chunk.Partial)
  done;
  assert (Poly.( = ) (status "5;\r\n") Httpz.Chunk.Malformed);
  assert
    (Poly.( = )
       (status "ffffffffffffffff\r\n")
       Httpz.Chunk.Chunk_too_large);
  let data = "X-One: 1\r\nX-Two: 2\r\n\r\n" in
  let len = copy_to_buffer buf data in
  let #(trailer_status, _, _) =
    Httpz.Chunk.parse_trailers ~max_trailer_size:20 buf ~off:(i16 0)
      ~len:(i16 len) ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_malformed);
  let data = "X: one\nInjected: yes\r\n\r\n" in
  let len = copy_to_buffer buf data in
  let #(trailer_status, _, _) =
    Httpz.Chunk.parse_trailers buf ~off:(i16 0) ~len:(i16 len)
      ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_bare_cr);
  Stdio.printf "test_chunk_framing_hardening: PASSED\n"
;;

let test_expect_continue () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nExpect: 100-continue\r\nContent-Length: 1000\r\n\r\n"
  in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert req.#expect_continue;
  assert (not req.#unsupported_expectation);
  assert (List.length headers = 1);
  let request =
    "POST /upload HTTP/1.1\r\n\
     Host: example.com\r\n\
     Expect: 100-continue, 100-continue\r\n\
     Content-Length: 1000\r\n\
     \r\n"
  in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert req.#expect_continue;
  assert (not req.#unsupported_expectation);
  let request =
    "POST /upload HTTP/1.0\r\n\
     Expect: other, 100-continue\r\n\
     Content-Length: 1000\r\n\
     \r\n"
  in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert (not req.#expect_continue);
  assert (not req.#unsupported_expectation);
  Stdio.printf "test_expect_continue: PASSED\n"
;;

let test_expect_continue_absent () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 100\r\n\r\n" in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert (not req.#expect_continue);
  assert (not req.#unsupported_expectation);
  let request =
    "POST /upload HTTP/1.1\r\n\
     Host: example.com\r\n\
     Expect: fancy-feature\r\n\
     Content-Length: 100\r\n\
     \r\n"
  in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert (not req.#expect_continue);
  assert req.#unsupported_expectation;
  Stdio.printf "test_expect_continue_absent: PASSED\n"
;;

let test_write_chunk_header () =
  let dst = Bytes.create 100 in
  let off = Httpz.Res.write_chunk_header dst ~off:(i16 0) ~size:255 in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.( = ) written "ff\r\n");
  let off2 = Httpz.Res.write_chunk_header dst ~off:(i16 0) ~size:0 in
  let written2 = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off2) in
  assert (String.( = ) written2 "0\r\n");
  let off3 = Httpz.Res.write_chunk_header dst ~off:(i16 0) ~size:4096 in
  let written3 = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off3) in
  assert (String.( = ) written3 "1000\r\n");
  Stdio.printf "test_write_chunk_header: PASSED\n"
;;

let test_write_final_chunk () =
  let dst = Bytes.create 100 in
  let off = Httpz.Res.write_final_chunk dst ~off:(i16 0) in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.( = ) written "0\r\n\r\n");
  Stdio.printf "test_write_final_chunk: PASSED\n"
;;

let test_parse_trailers () =
  let buf = Bytes.create Httpz.buffer_size in
  let data = "5\r\nhello\r\n0\r\nX-Checksum: abc123\r\nX-Other: value\r\n\r\n" in
  let len = copy_to_buffer buf data in
  let #(status1, chunk1) = Httpz.Chunk.parse (buf) ~off:(i16 0) ~len:(i16 len) in
  assert (Poly.( = ) status1 Httpz.Chunk.Complete);
  assert (to_int chunk1.#data_len = 5);
  let #(status2, chunk2) = Httpz.Chunk.parse (buf) ~off:chunk1.#next_off ~len:(i16 len) in
  assert (Poly.( = ) status2 Httpz.Chunk.Done);
  let #(trailer_status, _end_off, trailers) =
    Httpz.Chunk.parse_trailers (buf) ~off:chunk2.#next_off ~len:(i16 len) ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_complete);
  assert (List.length trailers = 2);
  let len = copy_to_buffer buf "X-En" in
  let #(trailer_status, _, _) =
    Httpz.Chunk.parse_trailers buf ~off:(i16 0) ~len:(i16 len)
      ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_partial);
  Stdio.printf "test_parse_trailers: PASSED\n"
;;

let test_forbidden_trailers () =
  let buf = Bytes.create Httpz.buffer_size in
  let data =
    "0\r\nContent-Length: 100\r\nTrailer: X-Late\r\nTE: trailers\r\n\
     If-Match: *\r\nProxy-Authorization: Basic xx\r\n\
     Content-Language: en\r\nX-Custom: value\r\n\r\n"
  in
  let len = copy_to_buffer buf data in
  let #(status, chunk) = Httpz.Chunk.parse (buf) ~off:(i16 0) ~len:(i16 len) in
  assert (Poly.( = ) status Httpz.Chunk.Done);
  let #(trailer_status, _end_off, trailers) =
    Httpz.Chunk.parse_trailers (buf) ~off:chunk.#next_off ~len:(i16 len) ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_complete);
  assert (List.length trailers = 1);
  Stdio.printf "test_forbidden_trailers: PASSED\n"
;;

let test_write_chunked_response () =
  let dst = Bytes.create 500 in
  let off = i16 0 in
  let off = Httpz.Res.write_status_line dst ~off Httpz.Res.Success Httpz.Version.Http_1_1 in
  let off = Httpz.Res.write_transfer_encoding_chunked dst ~off in
  let off = Httpz.Res.write_crlf dst ~off in
  let off = Httpz.Res.write_chunk_header dst ~off ~size:5 in
  Bytes.From_string.blit ~src:"Hello" ~src_pos:0 ~dst ~dst_pos:(to_int off) ~len:5;
  let off = i16 (to_int off + 5) in
  let off = Httpz.Res.write_chunk_footer dst ~off in
  let off = Httpz.Res.write_chunk_header dst ~off ~size:6 in
  Bytes.From_string.blit ~src:" World" ~src_pos:0 ~dst ~dst_pos:(to_int off) ~len:6;
  let off = i16 (to_int off + 6) in
  let off = Httpz.Res.write_chunk_footer dst ~off in
  let off = Httpz.Res.write_final_chunk dst ~off in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  let expected =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n World\r\n0\r\n\r\n"
  in
  assert (String.( = ) written expected);
  Stdio.printf "test_write_chunked_response: PASSED\n"
;;

let test_etag_parse () =
  let buf = Bytes.create Httpz.buffer_size in
  let etag_str = "\"xyzzy\"" in
  let len = copy_to_buffer buf etag_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, etag) = Httpz.Etag.parse (buf) sp in
  assert (Poly.( = ) status Httpz.Etag.Valid);
  assert (not etag.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) etag) "xyzzy");
  let weak_str = "W/\"weak-tag\"" in
  let len2 = copy_to_buffer buf weak_str in
  let sp2 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len2) in
  let #(status2, etag2) = Httpz.Etag.parse (buf) sp2 in
  assert (Poly.( = ) status2 Httpz.Etag.Valid);
  assert etag2.#weak;
  assert (String.equal (Httpz.Etag.to_string (buf) etag2) "weak-tag");
  let empty_str = "\"\"" in
  let len3 = copy_to_buffer buf empty_str in
  let sp3 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len3) in
  let #(status3, etag3) = Httpz.Etag.parse (buf) sp3 in
  assert (Poly.( = ) status3 Httpz.Etag.Valid);
  assert (not etag3.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) etag3) "");
  List.iter [ "\"a b\""; "\"a\001b\""; "\"a\"b\"" ] ~f:(fun value ->
    let len = copy_to_buffer buf value in
    let #(status, _) =
      Httpz.Etag.parse
        buf (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
    in
    assert (Poly.( = ) status Httpz.Etag.Invalid));
  Stdio.printf "test_etag_parse: PASSED\n"
;;

let test_etag_match_header () =
  let buf = Bytes.create Httpz.buffer_size in
  let tags_arr = Array.create ~len:(to_int Httpz.Etag.max_tags) Httpz.Etag.empty in
  let star_str = "*" in
  let len = copy_to_buffer buf star_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(cond, _count) = Httpz.Etag.parse_match_header (buf) sp tags_arr in
  assert (Poly.( = ) cond Httpz.Etag.Any);
  let list_str = "\"tag1\", W/\"tag2\", \"tag3\"" in
  let len2 = copy_to_buffer buf list_str in
  let sp2 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len2) in
  let #(cond2, count2) = Httpz.Etag.parse_match_header (buf) sp2 tags_arr in
  assert (Poly.( = ) cond2 Httpz.Etag.Tags);
  assert (to_int count2 = 3);
  let tag1 = Array.get tags_arr 0 in
  assert (not tag1.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) tag1) "tag1");
  List.iter
    [ "\"valid\", invalid, \"later\"";
      String.concat ~sep:", "
        (List.init 17 ~f:(fun i -> Printf.sprintf "\"t%d\"" i)) ]
    ~f:(fun value ->
      let len = copy_to_buffer buf value in
      let #(condition, count) =
        Httpz.Etag.parse_match_header buf
          (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
          tags_arr
      in
      assert (Poly.( = ) condition Httpz.Etag.Empty);
      assert (to_int count = 0));
  Stdio.printf "test_etag_match_header: PASSED\n"
;;

let test_etag_comparison () =
  let buf = Bytes.create Httpz.buffer_size in
  let str1 = "\"same\"" in
  let len1 = copy_to_buffer buf str1 in
  let sp1 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len1) in
  let #(_, etag1) = Httpz.Etag.parse buf sp1 in
  let str2 = "\"same\"" in
  let off2 = len1 in
  for i = 0 to String.length str2 - 1 do
    Bytes.set buf (off2 + i) (String.get str2 i)
  done;
  let sp2 = Httpz.Span.make ~off:(i16 off2) ~len:(i16 (String.length str2)) in
  let #(_, etag2) = Httpz.Etag.parse buf sp2 in
  assert (Httpz.Etag.strong_match buf etag1 etag2);
  assert (Httpz.Etag.weak_match buf etag1 etag2);
  Stdio.printf "test_etag_comparison: PASSED\n"
;;

let test_write_etag () =
  let dst = Bytes.create 100 in
  let off = Httpz.Etag.write_etag_string dst ~off:(i16 0) ~weak:false "abc123" in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "ETag: \"abc123\"\r\n");
  let off2 = Httpz.Etag.write_etag_string dst ~off:(i16 0) ~weak:true "weak-one" in
  let written2 = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off2) in
  assert (String.equal written2 "ETag: W/\"weak-one\"\r\n");
  List.iter [ "bad tag"; "bad\"tag"; "bad\rtag"; "bad\127tag" ]
    ~f:(fun value ->
      assert (
        match Httpz.Etag.write_etag_string dst ~off:(i16 0) ~weak:false value with
        | _ -> false
        | exception Invalid_argument _ -> true));
  Stdio.printf "test_write_etag: PASSED\n"
;;

module F64 = Stdlib_upstream_compatible.Float_u

let test_date_parse_imf () =
  let buf = Bytes.create Httpz.buffer_size in
  let date_str = "Sun, 06 Nov 1994 08:49:37 GMT" in
  let len = copy_to_buffer buf date_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, ts) = Httpz.Date.parse (buf) sp in
  assert (Poly.( = ) status Httpz.Date.Valid);
  assert (Float.( = ) (F64.to_float ts) 784111777.0);
  let junk = date_str ^ " junk" in
  let len = copy_to_buffer buf junk in
  let #(status, _) =
    Httpz.Date.parse
      buf (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
  in
  assert (Poly.( = ) status Httpz.Date.Invalid);
  let asctime_junk = "Sun Nov  6 08:49:37 1994 junk" in
  let len = copy_to_buffer buf asctime_junk in
  let #(status, _) =
    Httpz.Date.parse buf (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
  in
  assert (Poly.( = ) status Httpz.Date.Invalid);
  List.iter
    [ "Fry, 06 Nov 1994 08:49:37 GMT"
    ; "Sux Nov  6 08:49:37 1994"
    ; "Sund, 06-Nov-94 08:49:37 GMT"
    ; "Sundayish, 06-Nov-94 08:49:37 GMT"
    ]
    ~f:(fun invalid_weekday ->
      let len = copy_to_buffer buf invalid_weekday in
      let #(status, _) =
        Httpz.Date.parse buf (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
      in
      assert (Poly.( = ) status Httpz.Date.Invalid));
  (* The spelling is syntax; correspondence with the calendar remains
     deliberately unchecked for HTTP compatibility. *)
  let disagreeing_weekday = "Mon, 06 Nov 1994 08:49:37 GMT" in
  let len = copy_to_buffer buf disagreeing_weekday in
  let #(status, _) =
    Httpz.Date.parse buf (Httpz.Span.make ~off:(i16 0) ~len:(i16 len))
  in
  assert (Poly.( = ) status Httpz.Date.Valid);
  Stdio.printf "test_date_parse_imf: PASSED\n"
;;

let test_date_parse_rfc850_moving_year () =
  let check ~now value expected =
    let buf = Bytes.create Httpz.buffer_size in
    let len = copy_to_buffer buf value in
    let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
    let #(status, ts) = Httpz.Date.parse ~now (buf) sp in
    assert (Poly.( = ) status Httpz.Date.Valid);
    assert (String.equal (Httpz.Date.format ts) expected)
  in
  let now = 1_788_307_200.0 (* 2026-09-02 00:00:00 GMT *) in
  check
    ~now
    "Wednesday, 06-Nov-75 08:49:37 GMT"
    "Wed, 06 Nov 2075 08:49:37 GMT";
  check
    ~now
    "Wednesday, 02-Sep-76 00:00:00 GMT"
    "Wed, 02 Sep 2076 00:00:00 GMT";
  check
    ~now
    "Wednesday, 02-Sep-76 00:00:01 GMT"
    "Thu, 02 Sep 1976 00:00:01 GMT";
  check
    ~now:4_070_908_800.0
    "Friday, 01-Jan-00 00:00:00 GMT"
    "Fri, 01 Jan 2100 00:00:00 GMT";
  Stdio.printf "test_date_parse_rfc850_moving_year: PASSED\n"
;;

let test_date_format () =
  let ts = F64.of_float 784111777.0 in
  let formatted = Httpz.Date.format ts in
  assert (String.equal formatted "Sun, 06 Nov 1994 08:49:37 GMT");
  Stdio.printf "test_date_format: PASSED\n"
;;

let test_write_date_header () =
  let dst = Bytes.create 100 in
  let ts = F64.of_float 0.0 in
  let off = Httpz.Date.write_date_header dst ~off:(i16 0) ts in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n");
  Stdio.printf "test_write_date_header: PASSED\n"
;;

let test_range_parse_single () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let range_str = "bytes=0-499" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, count) = Httpz.Range.parse (buf) sp ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 1);
  let r = Array.get ranges 0 in
  assert (Httpz.Range.is_range r);
  assert (I64.equal r.#start #0L);
  assert (I64.equal r.#end_ #499L);
  Stdio.printf "test_range_parse_single: PASSED\n"
;;

let test_range_parse_suffix () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let range_str = "bytes=-500" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, count) = Httpz.Range.parse (buf) sp ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 1);
  let r = Array.get ranges 0 in
  assert (Httpz.Range.is_suffix r);
  assert (I64.equal r.#start #500L);
  Stdio.printf "test_range_parse_suffix: PASSED\n"
;;

let test_range_parse_open () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let range_str = "bytes=9500-" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, count) = Httpz.Range.parse (buf) sp ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 1);
  let r = Array.get ranges 0 in
  assert (Httpz.Range.is_open r);
  assert (I64.equal r.#start #9500L);
  Stdio.printf "test_range_parse_open: PASSED\n"
;;

let test_range_parse_multiple () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let range_str = "bytes=0-499, 1000-1499" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, count) = Httpz.Range.parse (buf) sp ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 2);
  Stdio.printf "test_range_parse_multiple: PASSED\n"
;;

let test_range_parse_string () =
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let #(status, count) = Httpz.Range.parse_string "bytes=100-199" ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 1);
  let r = Array.get ranges 0 in
  assert (Httpz.Range.is_range r);
  assert (Int64.equal (I64.to_int64 r.#start) 100L);
  assert (Int64.equal (I64.to_int64 r.#end_) 199L);
  let #(status2, count2) = Httpz.Range.parse_string "bytes=-500" ranges in
  assert (Poly.( = ) status2 Httpz.Range.Valid);
  assert (to_int count2 = 1);
  assert (Httpz.Range.is_suffix (Array.get ranges 0));
  let #(status3, count3) = Httpz.Range.parse_string "bytes=-0" ranges in
  assert (Poly.( = ) status3 Httpz.Range.Valid);
  assert (to_int count3 = 1);
  assert (Httpz.Range.is_suffix (Array.get ranges 0));
  List.iter
    [ "bytes=2-1";
      "bytes=0-1, nope, 4-5";
      "bytes=25000000000000000000-";
      "bytes="
      ^ String.concat ~sep:","
          (List.init 17 ~f:(fun i -> Printf.sprintf "%d-%d" i i)) ]
    ~f:(fun value ->
      let #(status, count) = Httpz.Range.parse_string value ranges in
      assert (Poly.( = ) status Httpz.Range.Invalid);
      assert (to_int count = 0));
  Stdio.printf "test_range_parse_string: PASSED\n"
;;

let test_range_satisfiable () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let resolved = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty_resolved in
  let range_str = "bytes=0-499" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(_, count) = Httpz.Range.parse (buf) sp ranges in
  let #(result, res_count) = Httpz.Range.evaluate ranges ~count ~resource_length:#1000L resolved in
  assert (Poly.( = ) result Httpz.Range.Single_range);
  assert (to_int res_count = 1);
  let r = Array.get resolved 0 in
  assert (I64.equal r.#start #0L);
  assert (I64.equal r.#end_ #499L);
  assert (I64.equal r.#length #500L);
  let #(result2, res_count2) = Httpz.Range.evaluate ranges ~count ~resource_length:#100L resolved in
  assert (Poly.( = ) result2 Httpz.Range.Single_range);
  assert (to_int res_count2 = 1);
  let r2 = Array.get resolved 0 in
  assert (I64.equal r2.#end_ #99L);
  Stdio.printf "test_range_satisfiable: PASSED\n"
;;

let test_range_unsatisfiable () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let resolved = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty_resolved in
  let range_str = "bytes=1000-1999" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(_, count) = Httpz.Range.parse (buf) sp ranges in
  let #(result, _) = Httpz.Range.evaluate ranges ~count ~resource_length:#500L resolved in
  assert (Poly.( = ) result Httpz.Range.Not_satisfiable);
  Stdio.printf "test_range_unsatisfiable: PASSED\n"
;;

let test_range_evaluate () =
  let buf = Bytes.create Httpz.buffer_size in
  let ranges = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty in
  let resolved = Array.create ~len:(to_int Httpz.Range.max_ranges) Httpz.Range.empty_resolved in
  let range_str = "bytes=0-99" in
  let len = copy_to_buffer buf range_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(_, count) = Httpz.Range.parse (buf) sp ranges in
  let #(result, res_count) = Httpz.Range.evaluate ranges ~count ~resource_length:#1000L resolved in
  assert (Poly.( = ) result Httpz.Range.Single_range);
  assert (to_int res_count = 1);
  let r = Array.get resolved 0 in
  assert (I64.equal r.#start #0L);
  assert (I64.equal r.#end_ #99L);
  let #(result2, _) = Httpz.Range.evaluate ranges ~count:(i16 0) ~resource_length:#1000L resolved in
  assert (Poly.( = ) result2 Httpz.Range.Full_content);
  Stdio.printf "test_range_evaluate: PASSED\n"
;;

let test_write_content_range () =
  let dst = Bytes.create 100 in
  let off = Httpz.Range.write_content_range dst ~off:(i16 0) ~start:#0L ~end_:#499L ~total:#1000L in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "Content-Range: bytes 0-499/1000\r\n");
  Stdio.printf "test_write_content_range: PASSED\n"
;;

let test_write_content_range_unsatisfiable () =
  let dst = Bytes.create 100 in
  let off = Httpz.Range.write_content_range_unsatisfiable dst ~off:(i16 0) ~total:#1000L in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "Content-Range: bytes */1000\r\n");
  Stdio.printf "test_write_content_range_unsatisfiable: PASSED\n"
;;

let test_generate_boundary () =
  (* Seeding the default generator the same way twice must not reproduce a
     boundary: on the default state every process emitted the same sequence
     from startup, so a restart reused boundaries an attacker had seen. *)
  Random.init 42;
  let first = Httpz.Range.generate_boundary () in
  Random.init 42;
  let second = Httpz.Range.generate_boundary () in
  assert (String.length first = 24);
  assert (String.for_all first ~f:Char.is_alphanum);
  assert (String.( <> ) first second);
  Stdio.printf "test_generate_boundary: PASSED\n"
;;

let test_write_accept_ranges () =
  let dst = Bytes.create 100 in
  let off = Httpz.Range.write_accept_ranges dst ~off:(i16 0) in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "Accept-Ranges: bytes\r\n");
  Stdio.printf "test_write_accept_ranges: PASSED\n"
;;

let () =
  test_simple_get ();
  test_post_with_body ();
  test_unknown_method ();
  test_unknown_header ();
  test_x_request_id_header_name ();
  test_additional_standard_header_names ();
  test_partial ();
  test_http10 ();
  test_higher_minor_request_version ();
  test_leading_empty_request_lines ();
  test_impossible_version_prefix ();
  test_error_result_closes ();
  test_huge_body_window_math ();
  test_keep_alive ();
  test_connection_upgrade ();
  test_upgrade_syntax ();
  test_chunked ();
  test_find_header ();
  test_find_header_string ();
  test_missing_host_http11 ();
  test_ambiguous_framing ();
  test_request_body_bytes_in_buffer ();
  test_request_header_block_too_large ();
  test_request_header_block_boundary ();
  test_host_authority_cross_check ();
  test_content_length_overflow ();
  test_bare_cr ();
  test_unsupported_transfer_encoding ();
  test_transfer_encoding_identity ();
  test_strict_content_length ();
  test_duplicate_content_length ();
  test_framing_token_lists ();
  test_host_and_field_validation ();
  test_chunk_framing_hardening ();
  test_expect_continue ();
  test_expect_continue_absent ();
  test_write_chunk_header ();
  test_write_final_chunk ();
  test_write_chunked_response ();
  test_parse_trailers ();
  test_forbidden_trailers ();
  test_etag_parse ();
  test_etag_match_header ();
  test_etag_comparison ();
  test_write_etag ();
  test_date_parse_imf ();
  test_date_parse_rfc850_moving_year ();
  test_date_format ();
  test_write_date_header ();
  test_range_parse_single ();
  test_range_parse_suffix ();
  test_range_parse_open ();
  test_range_parse_multiple ();
  test_range_parse_string ();
  test_range_satisfiable ();
  test_range_unsatisfiable ();
  test_range_evaluate ();
  test_write_content_range ();
  test_write_content_range_unsatisfiable ();
  test_write_accept_ranges ();
  test_generate_boundary ();
  Stdio.printf "\nAll tests passed!\n"
;;
