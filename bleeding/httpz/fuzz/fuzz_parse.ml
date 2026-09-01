(* Crowbar fuzz testing for Httpz request and response parsing: arbitrary bytes
   must never raise, and every offset or span in a complete parse must remain
   within the parsed buffer.
   To run: dune exec httpz/fuzz/fuzz_parse.exe
   With AFL: afl-fuzz -i fuzz/corpus -o fuzz/findings -- ./_build/default/httpz/fuzz/fuzz_parse.exe @@ *)
open Crowbar

let buffer_size = Httpz.Buf_read.buffer_size
let i16 = Httpz.Buf_read.i16
let to_int = Httpz.Buf_read.to_int

let span_in_bounds len span =
  let off = Httpz.Span.off span
  and size = Httpz.Span.len span in
  off >= 0 && size >= 0 && off <= len && size <= len - off
;;

let rec check_headers len (headers : Httpz.Header.t list @ local) =
  match headers with
  | [] -> ()
  | header :: rest ->
      check (span_in_bounds len header.name_span);
      check (span_in_bounds len header.value);
      check_headers len rest
;;

let check_parse buf len =
  try
    let #(status, req, headers) =
      Httpz.parse buf ~len:(i16 len) ~limits:Httpz.default_limits
    in
    match status with
    | Httpz.Buf_read.Complete ->
        let body_off = to_int req.#body_off in
        check (body_off >= 0);
        check (body_off <= len);
        check (span_in_bounds len req.#target);
        check (span_in_bounds len req.#path);
        check (span_in_bounds len req.#query);
        check_headers len headers
    | _ -> check true
  with e -> failf "Httpz.parse raised: %s" (Printexc.to_string e)
;;

let check_response buf len request_method =
  try
    let #(status, response, headers) =
      Httpz.Res.parse ~request_method buf ~len:(i16 len)
        ~limits:Httpz.default_limits
    in
    match status with
    | Httpz.Buf_read.Complete ->
        let body_off = to_int response.#body_off in
        check (body_off >= 0);
        check (body_off <= len);
        check (span_in_bounds len response.#reason);
        check_headers len headers
    | _ -> check true
  with e -> failf "Httpz.Res.parse raised: %s" (Printexc.to_string e)
;;

(* Arbitrary bytes, truncated or zero-padded to the parser's fixed-size
   buffer. *)
let test_raw_bytes input =
  let len = min (String.length input) buffer_size in
  let buf = Bytes.make buffer_size '\000' in
  Bytes.blit_string input 0 buf 0 len;
  check_parse buf len
;;

let test_raw_response input =
  let len = min (String.length input) buffer_size in
  let buf = Bytes.make buffer_size '\000' in
  Bytes.blit_string input 0 buf 0 len;
  check_response buf len Httpz.Method.Get
;;

(* Structured request heads let the fuzzer reach parser states that pure
   random input rarely does. *)
let method_gen =
  choose
    [ const "GET"
    ; const "POST"
    ; const "PUT"
    ; const "HEAD"
    ; const "DELETE"
    ; const "OPTIONS"
    ; const "TRACE"
    ; const "PATCH"
    ; const "CONNECT"
    ; const ""
    ; bytes
    ]
;;

let target_gen =
  choose
    [ const "/"
    ; const "/a/b?c=d"
    ; const "*"
    ; const "http://example.com/"
    ; const "http://example.com:8080/x"
    ; const "//x"
    ; const "/%2e%2e/%2e%2e/etc/passwd"
    ; const "/\x00\x01"
    ; map [ bytes ] (fun s -> "/" ^ s)
    ]
;;

let version_gen =
  choose
    [ const "HTTP/1.1"; const "HTTP/1.0"; const "HTTP/0.9";
      const "HTTP/2.0"; bytes ]
;;

let header_line_gen =
  choose
    [ map [ bytes ] (fun v -> "Host: " ^ v)
    ; map [ range 1_000_000 ] (fun n ->
        Printf.sprintf "Content-Length: %d" n)
    ; const "Transfer-Encoding: chunked"
    ; map [ bytes ] (fun v -> "Transfer-Encoding: " ^ v)
    ; const "Connection: keep-alive"
    ; const "Connection: close"
    ; const "Expect: 100-continue"
    ; map [ bytes; bytes ] (fun name value -> name ^ ": " ^ value)
    ]
;;

let headers_gen = list header_line_gen

let request_gen =
  map [ method_gen; target_gen; version_gen; headers_gen ]
    (fun meth target version headers ->
      String.concat "\r\n"
        ((meth ^ " " ^ target ^ " " ^ version)
         :: (headers @ [ ""; "" ])))
;;

let test_structured head =
  let len = min (String.length head) buffer_size in
  let buf = Bytes.make buffer_size '\000' in
  Bytes.blit_string head 0 buf 0 len;
  check_parse buf len
;;

let () =
  add_test ~name:"Httpz.parse: raw bytes never raise" [ bytes ] test_raw_bytes;
  add_test
    ~name:"Httpz.parse: structured request heads never raise"
    [ request_gen ] test_structured;
  add_test
    ~name:"Httpz.Res.parse: raw bytes never raise"
    [ bytes ] test_raw_response
;;
