(* test_client.ml - Tests for the client side: response parsing,
   request-line writing, and chunk-header parsing *)
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

(* Parse a response and assert success. *)
let parse_ok buf response = exclave_
  let len = copy_to_buffer buf response in
  let #(status, res, headers) =
    Httpz.Res.parse buf ~len:(i16 len) ~limits
  in
  if Poly.( <> ) status Httpz.Buf_read.Complete
  then
    failwith
      (Printf.sprintf "Expected Complete, got %s"
         (Httpz.Buf_read.status_to_string status));
  #(len, res, headers)
;;

let parse_status buf response =
  let len = copy_to_buffer buf response in
  let #(status, _res, _headers) =
    Httpz.Res.parse buf ~len:(i16 len) ~limits
  in
  status
;;

let test_simple_response () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, headers) =
    parse_ok buf
      "HTTP/1.1 200 OK\r\n\
       Content-Type: text/plain\r\n\
       Content-Length: 5\r\n\
       \r\n\
       hello"
  in
  assert (Poly.( = ) res.#version Httpz.Version.Http_1_1);
  assert (to_int res.#code = 200);
  assert (Httpz.Span.equal buf res.#reason "OK");
  assert (I64.equal res.#content_length #5L);
  assert (not res.#is_chunked);
  assert res.#keep_alive;
  (* Every header stays in the list, framing ones included. *)
  assert (List.length headers = 2);
  assert (Option.is_some
            (Httpz.Header.find headers Httpz.Header_name.Content_length));
  Stdio.printf "test_simple_response: PASSED\n"
;;

let test_reason_optional () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _headers) = parse_ok buf "HTTP/1.1 204 \r\n\r\n" in
  assert (to_int res.#code = 204);
  assert (Httpz.Span.is_empty res.#reason);
  (* Some servers omit the SP after the code as well. *)
  let #(_len, res, _headers) = parse_ok buf "HTTP/1.1 204\r\n\r\n" in
  assert (to_int res.#code = 204);
  assert (Httpz.Span.is_empty res.#reason);
  Stdio.printf "test_reason_optional: PASSED\n"
;;

let test_unknown_code () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _headers) =
    parse_ok buf "HTTP/1.1 418 I'm a teapot\r\n\r\n"
  in
  assert (to_int res.#code = 418);
  assert (Httpz.Span.equal buf res.#reason "I'm a teapot");
  Stdio.printf "test_unknown_code: PASSED\n"
;;

let test_status_line_errors () =
  let buf = Bytes.create Httpz.buffer_size in
  assert (Poly.( = )
            (parse_status buf "HTTP/1.1 2000 OK\r\n\r\n")
            Httpz.Buf_read.Invalid_status);
  assert (Poly.( = )
            (parse_status buf "HTTP/1.1 20x OK\r\n\r\n")
            Httpz.Buf_read.Invalid_status);
  assert (Poly.( = )
            (parse_status buf "HTTP/2.0 200 OK\r\n\r\n")
            Httpz.Buf_read.Invalid_version);
  assert (Poly.( = )
            (parse_status buf "HTTP/1.1 200 O\rK\r\n\r\n")
            Httpz.Buf_read.Bare_cr_detected);
  Stdio.printf "test_status_line_errors: PASSED\n"
;;

let test_partial_response () =
  let buf = Bytes.create Httpz.buffer_size in
  assert (Poly.( = ) (parse_status buf "HTTP/1.1 20")
            Httpz.Buf_read.Partial);
  assert (Poly.( = ) (parse_status buf "HTTP/1.1 200 OK\r\nContent-")
            Httpz.Buf_read.Partial);
  Stdio.printf "test_partial_response: PASSED\n"
;;

let test_keep_alive () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\n\r\n" in
  assert res.#keep_alive;
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n"
  in
  assert (not res.#keep_alive);
  let #(_len, res, _) = parse_ok buf "HTTP/1.0 200 OK\r\n\r\n" in
  assert (not res.#keep_alive);
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.0 200 OK\r\nConnection: keep-alive\r\n\r\n"
  in
  assert res.#keep_alive;
  Stdio.printf "test_keep_alive: PASSED\n"
;;

let test_response_framing () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  assert res.#is_chunked;
  (* No framing header at all: length -1, body ends with the
     connection. *)
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\n\r\n" in
  assert (I64.equal res.#content_length (I64.of_int64 (-1L)));
  assert (not res.#is_chunked);
  assert (Poly.( = )
            (parse_status buf
               "HTTP/1.1 200 OK\r\n\
                Content-Length: 5\r\n\
                Transfer-Encoding: chunked\r\n\r\n")
            Httpz.Buf_read.Ambiguous_framing);
  Stdio.printf "test_response_framing: PASSED\n"
;;

let test_body_bytes_in_buffer () =
  (* Body bytes after the head do not count against the header-size
     limit, however many of them arrived in the same read. *)
  let buf = Bytes.create Httpz.buffer_size in
  let body = String.make 20000 'x' in
  let #(_len, res, _) =
    parse_ok buf
      ("HTTP/1.1 200 OK\r\nContent-Length: 20000\r\n\r\n" ^ body)
  in
  assert (I64.equal res.#content_length #20000L);
  Stdio.printf "test_body_bytes_in_buffer: PASSED\n"
;;

let test_header_block_too_large () =
  let buf = Bytes.create Httpz.buffer_size in
  let strict =
    #{ limits with max_header_size = i16 64 }
  in
  let response =
    "HTTP/1.1 200 OK\r\nX-Pad: " ^ String.make 100 'p' ^ "\r\n\r\n"
  in
  let len = copy_to_buffer buf response in
  let #(status, _res, _headers) =
    Httpz.Res.parse buf ~len:(i16 len) ~limits:strict
  in
  assert (Poly.( = ) status Httpz.Buf_read.Headers_too_large);
  Stdio.printf "test_header_block_too_large: PASSED\n"
;;

let test_write_request_line () =
  let buf = Bytes.create 256 in
  let off =
    Httpz.Req.write_request_line buf ~off:(i16 0) ~meth:"GET"
      ~target:"/index.html" Httpz.Version.Http_1_1
  in
  let s = Bytes.To_string.sub buf ~pos:0 ~len:(to_int off) in
  assert (String.equal s "GET /index.html HTTP/1.1\r\n");
  (* An extension method goes out as given. *)
  let off =
    Httpz.Req.write_request_line buf ~off:(i16 0) ~meth:"PURGE" ~target:"/x"
      Httpz.Version.Http_1_0
  in
  let s = Bytes.To_string.sub buf ~pos:0 ~len:(to_int off) in
  assert (String.equal s "PURGE /x HTTP/1.0\r\n");
  Stdio.printf "test_write_request_line: PASSED\n"
;;

let test_request_round_trip () =
  (* What the client writer emits, the server parser accepts. *)
  let buf = Bytes.create Httpz.buffer_size in
  let off =
    Httpz.Req.write_request_line buf ~off:(i16 0) ~meth:"POST"
      ~target:"/api/data" Httpz.Version.Http_1_1
  in
  let off = Httpz.Res.write_header buf ~off "Host" "example.com" in
  let off = Httpz.Res.write_content_length buf ~off 0 in
  let off = Httpz.Res.write_crlf buf ~off in
  let #(status, req, _headers) = Httpz.parse buf ~len:off ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Complete);
  assert (Poly.( = ) req.#meth Httpz.Method.Post);
  assert (Httpz.Span.equal buf req.#target "/api/data");
  Stdio.printf "test_request_round_trip: PASSED\n"
;;

let test_chunk_parse_header () =
  let buf = Bytes.create Httpz.buffer_size in
  let max = Httpz.Chunk.default_max_chunk_size in
  (* A size line whose data has not arrived is still parsed. *)
  let len = copy_to_buffer buf "ffff\r\nab" in
  let #(status, size, data_off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Complete);
  assert (size = 0xffff);
  assert (to_int data_off = 6);
  (* Extensions are skipped. *)
  let len = copy_to_buffer buf "5;ext=1\r\nhello\r\n" in
  let #(status, size, data_off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Complete);
  assert (size = 5);
  assert (to_int data_off = 9);
  (* The final chunk points at the trailer area. *)
  let len = copy_to_buffer buf "0\r\nX-T: v\r\n\r\n" in
  let #(status, size, data_off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Done);
  assert (size = 0);
  assert (to_int data_off = 3);
  (* Partial and malformed size lines. *)
  let len = copy_to_buffer buf "ff" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Partial);
  let len = copy_to_buffer buf "xyz\r\n" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Malformed);
  let len = copy_to_buffer buf "ffffffff\r\n" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len)
      ~max_chunk_size:1024
  in
  assert (Poly.( = ) status Httpz.Chunk.Chunk_too_large);
  Stdio.printf "test_chunk_parse_header: PASSED\n"
;;

let () =
  Stdio.printf "Running httpz client tests...\n\n";
  test_simple_response ();
  test_reason_optional ();
  test_unknown_code ();
  test_status_line_errors ();
  test_partial_response ();
  test_keep_alive ();
  test_response_framing ();
  test_body_bytes_in_buffer ();
  test_header_block_too_large ();
  test_write_request_line ();
  test_request_round_trip ();
  test_chunk_parse_header ();
  Stdio.printf "\nAll client tests passed!\n"
;;
