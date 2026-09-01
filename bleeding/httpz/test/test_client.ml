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

let parse_ok ?request_method buf response = exclave_
  let len = copy_to_buffer buf response in
  let #(status, res, headers) =
    Httpz.Res.parse ?request_method buf ~len:(i16 len) ~limits
  in
  if Poly.( <> ) status Httpz.Buf_read.Complete
  then
    failwith
      (Printf.sprintf
         "Expected Complete, got %s"
         (Httpz.Buf_read.status_to_string status));
  #(len, res, headers)
;;

let parse_status ?request_method buf response =
  let len = copy_to_buffer buf response in
  let #(status, _res, _headers) =
    Httpz.Res.parse ?request_method buf ~len:(i16 len) ~limits
  in
  status
;;

let test_simple_response () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, headers) =
    parse_ok
      buf
      "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello"
  in
  assert (Poly.( = ) res.#version Httpz.Version.Http_1_1);
  assert (to_int res.#code = 200);
  assert (Httpz.Span.equal buf res.#reason "OK");
  assert (I64.equal res.#content_length #5L);
  assert (not res.#is_chunked);
  assert res.#keep_alive;
  assert (List.length headers = 2);
  assert (Option.is_some (Httpz.Header.find headers Httpz.Header_name.Content_length));
  Stdio.printf "test_simple_response: PASSED\n"
;;

let test_reason_optional () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _headers) = parse_ok buf "HTTP/1.1 204 \r\n\r\n" in
  assert (to_int res.#code = 204);
  assert (Httpz.Span.is_empty res.#reason);
  let #(_len, res, _headers) = parse_ok buf "HTTP/1.1 204\r\n\r\n" in
  assert (to_int res.#code = 204);
  assert (Httpz.Span.is_empty res.#reason);
  Stdio.printf "test_reason_optional: PASSED\n"
;;

let test_unknown_code () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _headers) = parse_ok buf "HTTP/1.1 418 I'm a teapot\r\n\r\n" in
  assert (to_int res.#code = 418);
  assert (Httpz.Span.equal buf res.#reason "I'm a teapot");
  Stdio.printf "test_unknown_code: PASSED\n"
;;

let test_status_line_errors () =
  let buf = Bytes.create Httpz.buffer_size in
  assert (
    Poly.( = ) (parse_status buf "HTTP/1.1 2000 OK\r\n\r\n") Httpz.Buf_read.Invalid_status);
  assert (
    Poly.( = ) (parse_status buf "HTTP/1.1 20x OK\r\n\r\n") Httpz.Buf_read.Invalid_status);
  assert (
    Poly.( = ) (parse_status buf "HTTP/2.0 200 OK\r\n\r\n") Httpz.Buf_read.Invalid_version);
  assert (
    Poly.( = )
      (parse_status buf "HTTP/1.1 200 O\rK\r\n\r\n")
      Httpz.Buf_read.Bare_cr_detected);
  Stdio.printf "test_status_line_errors: PASSED\n"
;;

let test_higher_minor_response_version () =
  let buf = Bytes.create Httpz.buffer_size in
  List.iter [ "HTTP/1.2"; "HTTP/1.9" ] ~f:(fun wire_version ->
    let #(_len, res, _headers) =
      parse_ok buf (wire_version ^ " 200 OK\r\nContent-Length: 0\r\n\r\n")
    in
    assert (Poly.( = ) res.#version Httpz.Version.Http_1_1));
  Stdio.printf "test_higher_minor_response_version: PASSED\n"
;;

let test_partial_response () =
  let buf = Bytes.create Httpz.buffer_size in
  assert (Poly.( = ) (parse_status buf "HTTP/1.1 20") Httpz.Buf_read.Partial);
  assert (
    Poly.( = ) (parse_status buf "HTTP/1.1 200 OK\r\nContent-") Httpz.Buf_read.Partial);
  Stdio.printf "test_partial_response: PASSED\n"
;;

let test_response_obs_fold () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, headers) =
    parse_ok
      buf
      "HTTP/1.1 200 OK\r\nX-Fold: one\r\n two\r\nContent-Length:\r\n 5\r\n\r\nhello"
  in
  assert (I64.equal res.#content_length #5L);
  let folded =
    match Httpz.Header.find_string buf headers "x-fold" with
    | Some folded -> folded
    | None -> assert false
  in
  assert (Httpz.Span.equal buf folded.Httpz.Header.value "one   two");
  (* A retry after a read split at obs-fold sees the original CRLF. *)
  let partial = "HTTP/1.1 200 OK\r\nX-Fold: one\r\n " in
  let partial_len = copy_to_buffer buf partial in
  let #(status, _res, _headers) = Httpz.Res.parse buf ~len:(i16 partial_len) ~limits in
  assert (Poly.( = ) status Httpz.Buf_read.Partial);
  assert (Char.equal (Bytes.get buf (partial_len - 3)) '\r');
  Stdio.printf "test_response_obs_fold: PASSED\n"
;;

let test_keep_alive () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\n\r\n" in
  assert (not res.#keep_alive);
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
  assert res.#keep_alive;
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n" in
  assert (not res.#keep_alive);
  let #(_len, res, _) = parse_ok buf "HTTP/1.0 200 OK\r\n\r\n" in
  assert (not res.#keep_alive);
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.0 200 OK\r\nConnection: keep-alive\r\nContent-Length: 0\r\n\r\n"
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
  (* Without a framing field, the body ends when the connection closes. *)
  let #(_len, res, _) = parse_ok buf "HTTP/1.1 200 OK\r\n\r\n" in
  assert (I64.equal res.#content_length (I64.of_int64 (-1L)));
  assert (not res.#is_chunked);
  assert (
    Poly.( = )
      (parse_status
         buf
         "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nTransfer-Encoding: chunked\r\n\r\n")
      Httpz.Buf_read.Ambiguous_framing);
  assert (
    Poly.( = )
      (parse_status buf "HTTP/1.0 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
      Httpz.Buf_read.Unsupported_transfer_encoding);
  Stdio.printf "test_response_framing: PASSED\n"
;;

let test_strict_response_framing () =
  let buf = Bytes.create Httpz.buffer_size in
  List.iter [ "12x"; "+5"; "1_0"; "5 5"; "" ] ~f:(fun value ->
    let response = Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Length: %s\r\n\r\n" value in
    assert (Poly.( = ) (parse_status buf response) Httpz.Buf_read.Invalid_header));
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.1 200 OK\r\nContent-Length: 5, 5\r\nContent-Length: 5\r\n\r\n"
  in
  assert (I64.equal res.#content_length #5L);
  assert (
    Poly.( = )
      (parse_status
         buf
         "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Length: 6\r\n\r\n")
      Httpz.Buf_read.Ambiguous_framing);
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip, chunked\r\n\r\n"
  in
  assert res.#is_chunked;
  let #(_len, res, _) =
    parse_ok buf "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\n"
  in
  assert (not res.#is_chunked);
  assert (not res.#keep_alive);
  List.iter
    [ ", chunked"
    ; "chunked,"
    ; "gzip,,chunked"
    ; "gzip;level=fast, chunked"
    ; "gzip;note=\"a,b\", chunked"
    ]
    ~f:(fun value ->
      let #(_len, res, _) =
        parse_ok
          buf
          (Printf.sprintf "HTTP/1.1 200 OK\r\nTransfer-Encoding: %s\r\n\r\n" value)
      in
      assert res.#is_chunked);
  let #(_len, res, _) =
    parse_ok
      buf
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  assert res.#is_chunked;
  List.iter
    [ "chunked, chunked"
    ; "chunked, gzip"
    ; "chunked;foo=bar"
    ; "gzip;level, chunked"
    ; "chun ked"
    ; "\"chunked\""
    ]
    ~f:(fun value ->
      let response =
        Printf.sprintf "HTTP/1.1 200 OK\r\nTransfer-Encoding: %s\r\n\r\n" value
      in
      assert (
        Poly.( = )
          (parse_status buf response)
          Httpz.Buf_read.Unsupported_transfer_encoding));
  let #(_len, res, _) =
    parse_ok
      buf
      "HTTP/1.1 200 OK\r\nConnection: upgrade, close\r\nContent-Length: 0\r\n\r\n"
  in
  assert (not res.#keep_alive);
  Stdio.printf "test_strict_response_framing: PASSED\n"
;;

let test_bodyless_response_framing () =
  let buf = Bytes.create Httpz.buffer_size in
  let #(_len, res, _) =
    parse_ok
      ~request_method:Httpz.Method.Head
      buf
      "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  assert res.#bodyless;
  assert (not res.#is_chunked);
  assert (I64.equal res.#content_length #5L);
  List.iter [ 100; 204; 304 ] ~f:(fun code ->
    let #(_len, res, _) =
      parse_ok buf (Printf.sprintf "HTTP/1.1 %d Status\r\n\r\n" code)
    in
    assert res.#bodyless);
  let wire = "HTTP/1.1 205 Reset Content\r\nContent-Length: 5\r\n\r\nhello" in
  let #(_len, res, _) = parse_ok buf wire in
  assert (not res.#bodyless);
  assert (I64.equal res.#content_length #5L);
  assert (to_int res.#body_off + 5 = String.length wire);
  let #(_len, res, _) =
    parse_ok
      ~request_method:Httpz.Method.Connect
      buf
      "HTTP/1.1 200 Connection Established\r\n\r\n"
  in
  assert res.#bodyless;
  Stdio.printf "test_bodyless_response_framing: PASSED\n"
;;

let test_body_bytes_in_buffer () =
  (* Body bytes after the head do not count against the header-size limit, however many of
     them arrived in the same read. *)
  let buf = Bytes.create Httpz.buffer_size in
  let body = String.make 20000 'x' in
  let #(_len, res, _) =
    parse_ok buf ("HTTP/1.1 200 OK\r\nContent-Length: 20000\r\n\r\n" ^ body)
  in
  assert (I64.equal res.#content_length #20000L);
  Stdio.printf "test_body_bytes_in_buffer: PASSED\n"
;;

let test_header_block_too_large () =
  let buf = Bytes.create Httpz.buffer_size in
  let strict = #{ limits with max_header_size = i16 64 } in
  let response = "HTTP/1.1 200 OK\r\nX-Pad: " ^ String.make 100 'p' ^ "\r\n\r\n" in
  let len = copy_to_buffer buf response in
  let #(status, _res, _headers) = Httpz.Res.parse buf ~len:(i16 len) ~limits:strict in
  assert (Poly.( = ) status Httpz.Buf_read.Headers_too_large);
  Stdio.printf "test_header_block_too_large: PASSED\n"
;;

let test_write_request_line () =
  let buf = Bytes.create 256 in
  let off =
    Httpz.Req.write_request_line
      buf
      ~off:(i16 0)
      ~meth:"GET"
      ~target:"/index.html"
      Httpz.Version.Http_1_1
  in
  let s = Bytes.To_string.sub buf ~pos:0 ~len:(to_int off) in
  assert (String.equal s "GET /index.html HTTP/1.1\r\n");
  let off =
    Httpz.Req.write_request_line
      buf
      ~off:(i16 0)
      ~meth:"PURGE"
      ~target:"/x"
      Httpz.Version.Http_1_0
  in
  let s = Bytes.To_string.sub buf ~pos:0 ~len:(to_int off) in
  assert (String.equal s "PURGE /x HTTP/1.0\r\n");
  Stdio.printf "test_write_request_line: PASSED\n"
;;

let test_request_round_trip () =
  let buf = Bytes.create Httpz.buffer_size in
  let off =
    Httpz.Req.write_request_line
      buf
      ~off:(i16 0)
      ~meth:"POST"
      ~target:"/api/data"
      Httpz.Version.Http_1_1
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
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Complete);
  assert (size = 0xffff);
  assert (to_int data_off = 6);
  let len = copy_to_buffer buf "5;ext=1\r\nhello\r\n" in
  let #(status, size, data_off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Complete);
  assert (size = 5);
  assert (to_int data_off = 9);
  let len = copy_to_buffer buf "0\r\nX-T: v\r\n\r\n" in
  let #(status, size, data_off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Done);
  assert (size = 0);
  assert (to_int data_off = 3);
  let len = copy_to_buffer buf "ff" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Partial);
  let len = copy_to_buffer buf "xyz\r\n" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Malformed);
  List.iter [ "5 \r\nhello\r\n"; "5\t\r\nhello\r\n" ] ~f:(fun line ->
    let len = copy_to_buffer buf line in
    let #(status, _size, _off) =
      Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
    in
    assert (Poly.( = ) status Httpz.Chunk.Malformed));
  let len = copy_to_buffer buf "5 \t; \tname \t= \tvalue\r\nhello\r\n" in
  let #(status, size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:max
  in
  assert (Poly.( = ) status Httpz.Chunk.Complete);
  assert (size = 5);
  let len = copy_to_buffer buf "ffffffff\r\n" in
  let #(status, _size, _off) =
    Httpz.Chunk.parse_header buf ~off:(i16 0) ~len:(i16 len) ~max_chunk_size:1024
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
  test_higher_minor_response_version ();
  test_partial_response ();
  test_response_obs_fold ();
  test_keep_alive ();
  test_response_framing ();
  test_strict_response_framing ();
  test_bodyless_response_framing ();
  test_body_bytes_in_buffer ();
  test_header_block_too_large ();
  test_write_request_line ();
  test_request_round_trip ();
  test_chunk_parse_header ();
  Stdio.printf "\nAll client tests passed!\n"
;;
