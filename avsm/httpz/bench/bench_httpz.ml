(* bench_httpz.ml - core_bench benchmarks for httpz HTTP parser *)

open Core
open Core_bench

(* Sample HTTP requests of varying complexity *)

let minimal_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

let simple_request =
  {|GET /path/to/resource HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0
Accept: text/html
Connection: keep-alive

|}
  |> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"
;;

let make_request_with_headers n_headers =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "GET /api/v1/users/12345 HTTP/1.1\r\n";
  Buffer.add_string buf "Host: api.example.com\r\n";
  for i = 1 to n_headers do
    Buffer.add_string buf (Printf.sprintf "X-Custom-Header-%d: value-%d\r\n" i i)
  done;
  Buffer.add_string buf "\r\n";
  Buffer.contents buf
;;

let request_5_headers = make_request_with_headers 5
let request_10_headers = make_request_with_headers 10
let request_20_headers = make_request_with_headers 20
let request_50_headers = make_request_with_headers 50

let make_request_with_body body_size =
  let body = String.make body_size 'x' in
  Printf.sprintf
    "POST /upload HTTP/1.1\r\n\
     Host: example.com\r\n\
     Content-Length: %d\r\n\
     Content-Type: application/octet-stream\r\n\
     \r\n\
     %s"
    body_size
    body
;;

let request_body_100 = make_request_with_body 100
let request_body_1k = make_request_with_body 1024
let request_body_10k = make_request_with_body 10240

let browser_request =
  {|GET /index.html HTTP/1.1
Host: www.example.com
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8
Accept-Language: en-US,en;q=0.9
Accept-Encoding: gzip, deflate, br
Connection: keep-alive
Cache-Control: max-age=0
Cookie: session=abc123; tracking=xyz789; preferences=dark-mode
If-None-Match: "abc123"
If-Modified-Since: Mon, 01 Jan 2024 00:00:00 GMT

|}
  |> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"
;;

(* Benchmark helpers *)

(* Helper to copy string into httpz bytes buffer *)
let copy_to_httpz_buffer buf data =
  let len = String.length data in
  for i = 0 to len - 1 do
    Bytes.set buf i (String.get data i)
  done;
  len
;;

let limits = Httpz.default_limits
let i16 = Httpz.Buf_read.i16

(* Parse request using httpz - note: result is stack-allocated (local) *)
let parse_request_httpz buf data =
  let len = copy_to_httpz_buffer buf data in
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  (* Extract values to prevent them from being optimized away *)
  let _ = req.#body_off in
  let _ = headers in
  status
;;

(* Variant to test if List.length causes allocation *)
let parse_request_httpz_with_length buf data =
  let len = copy_to_httpz_buffer buf data in
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  let _ = req.#body_off in
  let _ = List.length headers in
  status
;;

(* Test just the copy - no parsing *)
let just_copy_httpz buf data =
  let len = copy_to_httpz_buffer buf data in
  len
;;

(* Test parse only - assume data already in buffer *)
let just_parse_httpz buf len =
  let #(status, req, headers) = Httpz.parse buf ~len:(i16 len) ~limits in
  let _ = req.#body_off in
  let _ = headers in
  status
;;

(* Httpz parsing benchmarks *)
let httpz_buf = Bytes.create Httpz.buffer_size
let minimal_len = String.length minimal_request

(* Pre-populate buffer for parse-only test *)
let () =
  for i = 0 to minimal_len - 1 do
    Bytes.set httpz_buf i (String.get minimal_request i)
  done
;;

let httpz_parsing_benchmarks =
  [ Bench.Test.create ~name:"httpz_noop" (fun () -> ())
  ; Bench.Test.create ~name:"httpz_just_copy" (fun () ->
      ignore (just_copy_httpz httpz_buf minimal_request))
  ; Bench.Test.create ~name:"httpz_just_parse" (fun () ->
      ignore (just_parse_httpz httpz_buf minimal_len))
  ; Bench.Test.create ~name:"httpz_minimal" (fun () ->
      ignore (parse_request_httpz httpz_buf minimal_request))
  ; Bench.Test.create ~name:"httpz_minimal_len" (fun () ->
      ignore (parse_request_httpz_with_length httpz_buf minimal_request))
  ; Bench.Test.create ~name:"httpz_simple" (fun () ->
      ignore (parse_request_httpz httpz_buf simple_request))
  ; Bench.Test.create ~name:"httpz_browser" (fun () ->
      ignore (parse_request_httpz httpz_buf browser_request))
  ; Bench.Test.create ~name:"httpz_browser_len" (fun () ->
      ignore (parse_request_httpz_with_length httpz_buf browser_request))
  ; Bench.Test.create ~name:"httpz_5_headers" (fun () ->
      ignore (parse_request_httpz httpz_buf request_5_headers))
  ; Bench.Test.create ~name:"httpz_10_headers" (fun () ->
      ignore (parse_request_httpz httpz_buf request_10_headers))
  ; Bench.Test.create ~name:"httpz_20_headers" (fun () ->
      ignore (parse_request_httpz httpz_buf request_20_headers))
  ; Bench.Test.create ~name:"httpz_50_headers" (fun () ->
      ignore (parse_request_httpz httpz_buf request_50_headers))
  ]
;;

(* Httpz header lookup benchmarks *)
let httpz_header_benchmarks =
  let buf = Bytes.create Httpz.buffer_size in
  let len = i16 (copy_to_httpz_buffer buf browser_request) in
  [ Bench.Test.create ~name:"httpz_parse_and_find_host" (fun () ->
      let #(_status, _req, headers) = Httpz.parse buf ~len ~limits in
      ignore (Httpz.Header.find headers Httpz.Header_name.Host))
  ; Bench.Test.create ~name:"httpz_parse_and_is_keepalive" (fun () ->
      let #(_status, req, _headers) = Httpz.parse buf ~len ~limits in
      (* Now using cached keep_alive from request struct *)
      ignore req.#keep_alive)
  ; Bench.Test.create ~name:"httpz_parse_and_content_length" (fun () ->
      let #(_status, req, _headers) = Httpz.parse buf ~len ~limits in
      (* Now using cached content_length from request struct *)
      ignore req.#content_length)
  ]
;;

(* Httpz body handling benchmarks *)
let httpz_body_benchmarks =
  let buf = Bytes.create Httpz.buffer_size in
  [ Bench.Test.create ~name:"httpz_body_100B" (fun () ->
      let len = i16 (copy_to_httpz_buffer buf request_body_100) in
      let #(_status, req, _headers) = Httpz.parse buf ~len ~limits in
      let body = Httpz.Req.body_span ~len req in
      ignore body.#len)
  ; Bench.Test.create ~name:"httpz_body_1KB" (fun () ->
      let len = i16 (copy_to_httpz_buffer buf request_body_1k) in
      let #(_status, req, _headers) = Httpz.parse buf ~len ~limits in
      let body = Httpz.Req.body_span ~len req in
      ignore body.#len)
  ; Bench.Test.create ~name:"httpz_body_10KB" (fun () ->
      let len = i16 (copy_to_httpz_buffer buf request_body_10k) in
      let #(_status, req, _headers) = Httpz.parse buf ~len ~limits in
      let body = Httpz.Req.body_span ~len req in
      ignore body.#len)
  ]
;;

(* Httpz response serialization benchmarks *)
let httpz_serialize_benchmarks =
  let response_buf = Bytes.create 4096 in
  [ Bench.Test.create ~name:"httpz_write_status_line" (fun () ->
      ignore
        (Httpz.Res.write_status_line
           response_buf
           ~off:(Httpz.Buf_write.i16 0)
           Httpz.Res.Success
           Httpz.Version.Http_1_1))
  ; Bench.Test.create ~name:"httpz_write_response_headers" (fun () ->
      let off =
        Httpz.Res.write_status_line
          response_buf
          ~off:(Httpz.Buf_write.i16 0)
          Httpz.Res.Success
          Httpz.Version.Http_1_1
      in
      let off = Httpz.Res.write_header_name response_buf ~off Httpz.Header_name.Content_type "application/json" in
      let off = Httpz.Res.write_content_length response_buf ~off 1024 in
      let off = Httpz.Res.write_connection response_buf ~off ~keep_alive:true in
      let off = Httpz.Res.write_crlf response_buf ~off in
      ignore off)
  ]
;;

(* Httpz throughput benchmarks *)
let httpz_throughput_benchmarks =
  let buf = Bytes.create Httpz.buffer_size in
  let iterations = 1000 in
  [ Bench.Test.create ~name:"httpz_1k_simple" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf simple_request)
      done)
  ; Bench.Test.create ~name:"httpz_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf browser_request)
      done)
  ; Bench.Test.create ~name:"httpz_1k_50headers" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf request_50_headers)
      done)
  ]
;;

let command =
  Bench.make_command
    [ Bench.Test.create_group ~name:"httpz_parsing" httpz_parsing_benchmarks
    ; Bench.Test.create_group ~name:"httpz_headers" httpz_header_benchmarks
    ; Bench.Test.create_group ~name:"httpz_body" httpz_body_benchmarks
    ; Bench.Test.create_group ~name:"httpz_serialize" httpz_serialize_benchmarks
    ; Bench.Test.create_group ~name:"httpz_throughput" httpz_throughput_benchmarks
    ]
;;

let () = Command_unix.run command
