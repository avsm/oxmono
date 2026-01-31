(* test_httpz.ml - Tests for the Httpz parser *)
open Base

module I64 = Stdlib_upstream_compatible.Int64_u

let limits = Httpz.default_limits
let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

(* Copy string to bytes buffer *)
let copy_to_buffer buf s =
  let len = String.length s in
  for i = 0 to len - 1 do
    Bytes.set buf i (String.get s i)
  done;
  len
;;

(* Helper to parse a request and assert success.
   Returns the unboxed tuple with buffer for span operations. *)
let parse_ok buf request = exclave_
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  if Poly.( <> ) status Httpz.Buf_read.Complete
  then failwith (Printf.sprintf "Expected Ok, got %s" (Httpz.Buf_read.status_to_string status));
  #(len, buf, req, headers)
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
  (* Content-Length is now cached in request struct and excluded from headers *)
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
  (* Content-Length excluded from headers, only Host and Content-Type remain *)
  assert (List.length headers = 2);
  assert (to_int req.#body_off = len - 13);
  (* Content-Length is now in the request struct *)
  assert (I64.equal req.#content_length #13L);
  Stdio.printf "test_post_with_body: PASSED\n"
;;

let test_unknown_method () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "PURGE /cache HTTP/1.1\r\nHost: cdn.example.com\r\n\r\n" in
  let len = copy_to_buffer buf request in
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
  (* Headers are returned in reverse order: X-Custom-Header is first, Host is second *)
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
  (* HTTP/1.0 doesn't require Host header *)
  let request = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#version Httpz.Version.Http_1_0);
  assert (List.length headers = 0);
  Stdio.printf "test_http10: PASSED\n"
;;

let test_keep_alive () =
  let buf = Bytes.create Httpz.buffer_size in
  (* HTTP/1.1 default is keep-alive *)
  let request1 = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let #(_len1, _parse_buf1, req1, _headers1) = parse_ok buf request1 in
  (* Use cached keep_alive from request struct *)
  assert req1.#keep_alive;
  (* HTTP/1.0 default is close *)
  let request2 = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len2, _parse_buf2, req2, _headers2) = parse_ok buf request2 in
  assert (not req2.#keep_alive);
  Stdio.printf "test_keep_alive: PASSED\n"
;;

let test_chunked () =
  let buf = Bytes.create Httpz.buffer_size in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  (* Transfer-Encoding is now cached in request struct and excluded from headers *)
  assert req.#is_chunked;
  (* Only Host header remains *)
  assert (List.length headers = 1);
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

(* Security tests *)
let test_missing_host_http11 () =
  let buf = Bytes.create Httpz.buffer_size in
  (* HTTP/1.1 without Host header should fail *)
  let request = "GET / HTTP/1.1\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Missing_host_header);
  Stdio.printf "test_missing_host_http11: PASSED\n"
;;

let test_ambiguous_framing () =
  let buf = Bytes.create Httpz.buffer_size in
  (* Both Content-Length and Transfer-Encoding is an error *)
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
  (* Very large Content-Length should fail with configured limits *)
  let small_limits = #{ Httpz.Buf_read.max_content_length = #1000L
                      ; max_header_size = i16 16384
                      ; max_header_count = i16 100
                      ; max_chunk_size = 16777216
                      } in
  let request = "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 1000000\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits:small_limits in
  assert (Poly.( = ) status Httpz.Buf_read.Content_length_overflow);
  Stdio.printf "test_content_length_overflow: PASSED\n"
;;

let test_bare_cr () =
  let buf = Bytes.create Httpz.buffer_size in
  (* Bare CR in header value is a security violation *)
  let request = "GET / HTTP/1.1\r\nHost: example\rcom\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Bare_cr_detected);
  Stdio.printf "test_bare_cr: PASSED\n"
;;

let test_unsupported_transfer_encoding () =
  let buf = Bytes.create Httpz.buffer_size in
  (* RFC 7230 Section 3.3.1 - unsupported Transfer-Encoding should fail *)
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: gzip\r\n\r\n"
  in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Unsupported_transfer_encoding);
  Stdio.printf "test_unsupported_transfer_encoding: PASSED\n"
;;

let test_transfer_encoding_identity () =
  let buf = Bytes.create Httpz.buffer_size in
  (* identity Transfer-Encoding should be accepted (RFC 7230 deprecated but allowed) *)
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: identity\r\n\r\n"
  in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  (* identity means no encoding, so is_chunked should be false *)
  assert (not req.#is_chunked);
  Stdio.printf "test_transfer_encoding_identity: PASSED\n"
;;

let test_expect_continue () =
  let buf = Bytes.create Httpz.buffer_size in
  (* RFC 7231 Section 5.1.1 - Expect: 100-continue *)
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nExpect: 100-continue\r\nContent-Length: 1000\r\n\r\n"
  in
  let #(_len, _parse_buf, req, headers) = parse_ok buf request in
  assert req.#expect_continue;
  (* Expect header is cached in request struct, not in headers list *)
  assert (List.length headers = 1);  (* Only Host header *)
  Stdio.printf "test_expect_continue: PASSED\n"
;;

let test_expect_continue_absent () =
  let buf = Bytes.create Httpz.buffer_size in
  let request = "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 100\r\n\r\n" in
  let #(_len, _parse_buf, req, _headers) = parse_ok buf request in
  assert (not req.#expect_continue);
  Stdio.printf "test_expect_continue_absent: PASSED\n"
;;

(* Chunked response writing tests *)
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
  (* Chunked body with trailers *)
  let data = "5\r\nhello\r\n0\r\nX-Checksum: abc123\r\nX-Other: value\r\n\r\n" in
  let len = copy_to_buffer buf data in
  (* Parse the first chunk *)
  let #(status1, chunk1) = Httpz.Chunk.parse (buf) ~off:(i16 0) ~len:(i16 len) in
  assert (Poly.( = ) status1 Httpz.Chunk.Complete);
  assert (to_int chunk1.#data_len = 5);
  (* Parse the final chunk *)
  let #(status2, chunk2) = Httpz.Chunk.parse (buf) ~off:chunk1.#next_off ~len:(i16 len) in
  assert (Poly.( = ) status2 Httpz.Chunk.Done);
  (* Parse the trailers *)
  let #(trailer_status, _end_off, trailers) =
    Httpz.Chunk.parse_trailers (buf) ~off:chunk2.#next_off ~len:(i16 len) ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_complete);
  assert (List.length trailers = 2);
  Stdio.printf "test_parse_trailers: PASSED\n"
;;

let test_forbidden_trailers () =
  let buf = Bytes.create Httpz.buffer_size in
  (* Chunked body with forbidden trailer (Content-Length should be filtered) *)
  let data = "0\r\nContent-Length: 100\r\nX-Custom: value\r\n\r\n" in
  let len = copy_to_buffer buf data in
  (* Parse the final chunk *)
  let #(status, chunk) = Httpz.Chunk.parse (buf) ~off:(i16 0) ~len:(i16 len) in
  assert (Poly.( = ) status Httpz.Chunk.Done);
  (* Parse the trailers - forbidden ones should be filtered *)
  let #(trailer_status, _end_off, trailers) =
    Httpz.Chunk.parse_trailers (buf) ~off:chunk.#next_off ~len:(i16 len) ~max_header_count:(i16 10)
  in
  assert (Poly.( = ) trailer_status Httpz.Chunk.Trailer_complete);
  (* Only X-Custom should be in the list, Content-Length is forbidden *)
  assert (List.length trailers = 1);
  Stdio.printf "test_forbidden_trailers: PASSED\n"
;;

let test_write_chunked_response () =
  (* Simulate writing a complete chunked response *)
  let dst = Bytes.create 500 in
  let off = i16 0 in
  (* Status line *)
  let off = Httpz.Res.write_status_line dst ~off Httpz.Res.Success Httpz.Version.Http_1_1 in
  (* Transfer-Encoding: chunked *)
  let off = Httpz.Res.write_transfer_encoding_chunked dst ~off in
  (* End of headers *)
  let off = Httpz.Res.write_crlf dst ~off in
  (* First chunk: "Hello" (5 bytes) *)
  let off = Httpz.Res.write_chunk_header dst ~off ~size:5 in
  Bytes.From_string.blit ~src:"Hello" ~src_pos:0 ~dst ~dst_pos:(to_int off) ~len:5;
  let off = i16 (to_int off + 5) in
  let off = Httpz.Res.write_chunk_footer dst ~off in
  (* Second chunk: " World" (6 bytes) *)
  let off = Httpz.Res.write_chunk_header dst ~off ~size:6 in
  Bytes.From_string.blit ~src:" World" ~src_pos:0 ~dst ~dst_pos:(to_int off) ~len:6;
  let off = i16 (to_int off + 6) in
  let off = Httpz.Res.write_chunk_footer dst ~off in
  (* Final chunk *)
  let off = Httpz.Res.write_final_chunk dst ~off in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  let expected =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n World\r\n0\r\n\r\n"
  in
  assert (String.( = ) written expected);
  Stdio.printf "test_write_chunked_response: PASSED\n"
;;

(* ETag parsing tests *)
let test_etag_parse () =
  let buf = Bytes.create Httpz.buffer_size in
  (* Strong ETag *)
  let etag_str = "\"xyzzy\"" in
  let len = copy_to_buffer buf etag_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, etag) = Httpz.Etag.parse (buf) sp in
  assert (Poly.( = ) status Httpz.Etag.Valid);
  assert (not etag.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) etag) "xyzzy");
  (* Weak ETag *)
  let weak_str = "W/\"weak-tag\"" in
  let len2 = copy_to_buffer buf weak_str in
  let sp2 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len2) in
  let #(status2, etag2) = Httpz.Etag.parse (buf) sp2 in
  assert (Poly.( = ) status2 Httpz.Etag.Valid);
  assert etag2.#weak;
  assert (String.equal (Httpz.Etag.to_string (buf) etag2) "weak-tag");
  (* Empty tag *)
  let empty_str = "\"\"" in
  let len3 = copy_to_buffer buf empty_str in
  let sp3 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len3) in
  let #(status3, etag3) = Httpz.Etag.parse (buf) sp3 in
  assert (Poly.( = ) status3 Httpz.Etag.Valid);
  assert (not etag3.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) etag3) "");
  Stdio.printf "test_etag_parse: PASSED\n"
;;

let test_etag_match_header () =
  let buf = Bytes.create Httpz.buffer_size in
  let tags_arr = Array.create ~len:(to_int Httpz.Etag.max_tags) Httpz.Etag.empty in
  (* Test wildcard *)
  let star_str = "*" in
  let len = copy_to_buffer buf star_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(cond, _count) = Httpz.Etag.parse_match_header (buf) sp tags_arr in
  assert (Poly.( = ) cond Httpz.Etag.Any);
  (* Test list *)
  let list_str = "\"tag1\", W/\"tag2\", \"tag3\"" in
  let len2 = copy_to_buffer buf list_str in
  let sp2 = Httpz.Span.make ~off:(i16 0) ~len:(i16 len2) in
  let #(cond2, count2) = Httpz.Etag.parse_match_header (buf) sp2 tags_arr in
  assert (Poly.( = ) cond2 Httpz.Etag.Tags);
  assert (to_int count2 = 3);
  let tag1 = Array.get tags_arr 0 in
  assert (not tag1.#weak);
  assert (String.equal (Httpz.Etag.to_string (buf) tag1) "tag1");
  Stdio.printf "test_etag_match_header: PASSED\n"
;;

let test_etag_comparison () =
  let buf = Bytes.create Httpz.buffer_size in
  (* Create two identical strong tags *)
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
  (* Strong match should succeed for two strong identical tags *)
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
  Stdio.printf "test_write_etag: PASSED\n"
;;

(* Date parsing tests *)
module F64 = Stdlib_upstream_compatible.Float_u

let test_date_parse_imf () =
  let buf = Bytes.create Httpz.buffer_size in
  let date_str = "Sun, 06 Nov 1994 08:49:37 GMT" in
  let len = copy_to_buffer buf date_str in
  let sp = Httpz.Span.make ~off:(i16 0) ~len:(i16 len) in
  let #(status, ts) = Httpz.Date.parse (buf) sp in
  assert (Poly.( = ) status Httpz.Date.Valid);
  (* Nov 6, 1994 08:49:37 UTC = 784111777 seconds since epoch *)
  assert (Float.( = ) (F64.to_float ts) 784111777.0);
  Stdio.printf "test_date_parse_imf: PASSED\n"
;;

let test_date_format () =
  let ts = F64.of_float 784111777.0 in  (* Nov 6, 1994 08:49:37 UTC *)
  let formatted = Httpz.Date.format ts in
  assert (String.equal formatted "Sun, 06 Nov 1994 08:49:37 GMT");
  Stdio.printf "test_date_format: PASSED\n"
;;

let test_write_date_header () =
  let dst = Bytes.create 100 in
  let ts = F64.of_float 0.0 in  (* Jan 1, 1970 00:00:00 UTC *)
  let off = Httpz.Date.write_date_header dst ~off:(i16 0) ts in
  let written = Bytes.To_string.sub dst ~pos:0 ~len:(to_int off) in
  assert (String.equal written "Date: Thu, 01 Jan 1970 00:00:00 GMT\r\n");
  Stdio.printf "test_write_date_header: PASSED\n"
;;

(* Range parsing tests *)
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
  assert (I64.equal r.#start #500L);  (* suffix length stored in start *)
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
  (* Test parse_string - more ergonomic API for string input *)
  let #(status, count) = Httpz.Range.parse_string "bytes=100-199" ranges in
  assert (Poly.( = ) status Httpz.Range.Valid);
  assert (to_int count = 1);
  let r = Array.get ranges 0 in
  assert (Httpz.Range.is_range r);
  assert (Int64.equal (I64.to_int64 r.#start) 100L);
  assert (Int64.equal (I64.to_int64 r.#end_) 199L);
  (* Test with suffix range *)
  let #(status2, count2) = Httpz.Range.parse_string "bytes=-500" ranges in
  assert (Poly.( = ) status2 Httpz.Range.Valid);
  assert (to_int count2 = 1);
  assert (Httpz.Range.is_suffix (Array.get ranges 0));
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
  (* Test with a 1000-byte resource *)
  let #(result, res_count) = Httpz.Range.evaluate ranges ~count ~resource_length:#1000L resolved in
  assert (Poly.( = ) result Httpz.Range.Single_range);
  assert (to_int res_count = 1);
  let r = Array.get resolved 0 in
  assert (I64.equal r.#start #0L);
  assert (I64.equal r.#end_ #499L);
  assert (I64.equal r.#length #500L);
  (* Test with a 100-byte resource (range exceeds length) - should clamp *)
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
  (* Test with a 500-byte resource - range starts beyond end *)
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
  (* Test with no ranges - should return Full_content *)
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
  test_partial ();
  test_http10 ();
  test_keep_alive ();
  test_chunked ();
  test_find_header ();
  (* Security tests *)
  test_missing_host_http11 ();
  test_ambiguous_framing ();
  test_content_length_overflow ();
  test_bare_cr ();
  test_unsupported_transfer_encoding ();
  test_transfer_encoding_identity ();
  (* RFC 7231 tests *)
  test_expect_continue ();
  test_expect_continue_absent ();
  (* Chunked response writing tests *)
  test_write_chunk_header ();
  test_write_final_chunk ();
  test_write_chunked_response ();
  (* Trailer parsing tests *)
  test_parse_trailers ();
  test_forbidden_trailers ();
  (* ETag tests *)
  test_etag_parse ();
  test_etag_match_header ();
  test_etag_comparison ();
  test_write_etag ();
  (* Date tests *)
  test_date_parse_imf ();
  test_date_format ();
  test_write_date_header ();
  (* Range tests *)
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
  Stdio.printf "\nAll tests passed!\n"
;;
