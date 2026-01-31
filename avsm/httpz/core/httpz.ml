(* httpz.ml - Stack-allocated HTTP/1.1 parser for OxCaml *)

open Base

module Buf_read = Buf_read
module Buf_write = Buf_write
module Span = Span
module Method = Method
module Version = Version
module Header_name = Header_name
module Header = Header
module Req = Req
module Target = Target
module Chunk = Chunk
module Res = Res
module Parser = Parser
module Err = Err
module Etag = Etag
module Date = Date
module Range = Range

(* Buffer type: bytes for reading, writing, and parsing *)
type buffer = bytes
type span = Span.t
type method_ = Method.t
type version = Version.t
type header_name = Header_name.t
type header = Header.t
type status = Buf_read.status
type limits = Buf_read.limits
type req = Req.t
type chunk_status = Chunk.status
type trailer_status = Chunk.trailer_status
type chunk = Chunk.t
type res_status = Res.status

let buffer_size = Buf_read.buffer_size
let max_headers = Buf_read.max_headers
let default_limits = Buf_read.default_limits

(* Parsing implementation using Parser combinators *)

(* Use centralized int16# helpers from Span *)
module I64 = Stdlib_upstream_compatible.Int64_u
let i16 = Span.of_int
let gt16 = Span.gt
let gte16 = Span.gte
let add16 = Span.add
let one16 = Span.one

(* Connection header disposition *)
type conn_value = Conn_default | Conn_close | Conn_keep_alive

(* Header parsing state - unboxed record for zero-allocation parsing. *)
type header_state =
 #{ count : int16#
  ; content_len : int64#
  ; chunked : bool
  ; conn : conn_value
  ; has_cl : bool
  ; has_te : bool
  ; has_host : bool
  ; expect_continue : bool
  }

let minus_one_i64 : int64# = I64.of_int64 (-1L)

let initial_header_state : header_state =
 #{ count = i16 0
  ; content_len = minus_one_i64
  ; chunked = false
  ; conn = Conn_default
  ; has_cl = false
  ; has_te = false
  ; has_host = false
  ; expect_continue = false
  }

(* Helper to create error result with empty request *)
let[@inline] error_result status = exclave_
  #( status
   , #{ Req.meth = Method.Get
      ; target = Span.make ~off:(i16 0) ~len:(i16 0)
      ; version = Version.Http_1_1
      ; body_off = i16 0
      ; content_length = minus_one_i64
      ; is_chunked = false
      ; keep_alive = true
      ; expect_continue = false
      }
   , ([] : Header.t list) )

(* Build successful request from parsed components and state *)
let[@inline] build_request ~meth ~target ~version ~(body_off : int16#)
    (st : header_state) ~headers = exclave_
  let keep_alive =
    match st.#conn with
    | Conn_close -> false
    | Conn_keep_alive -> true
    | Conn_default -> Poly.( = ) version Version.Http_1_1
  in
  let req =
    #{ Req.meth
     ; target
     ; version
     ; body_off
     ; content_length = st.#content_len
     ; is_chunked = st.#chunked
     ; keep_alive
     ; expect_continue = st.#expect_continue
     }
  in
  #(Buf_read.Complete, req, headers)

(* Determine Connection header value *)
let[@inline] parse_connection_value buf value_span ~default =
  if Span.equal_caseless buf value_span "close" then Conn_close
  else if Span.equal_caseless buf value_span "keep-alive" then Conn_keep_alive
  else default

(* Parse headers using Parser combinators. Raises Err.Parse_error on failure.
   Position is threaded explicitly for zero allocation. *)
let rec parse_headers_loop (pst : Parser.pstate) ~pos ~acc (st : header_state) ~limits
  : #(int16# * header_state * Header.t list) = exclave_
  let open Buf_read in
  if Parser.is_headers_end pst ~pos then (
    let pos = Parser.end_headers pst ~pos in
    #(pos, st, acc)
  )
  else (
    Err.when_ (gte16 st.#count limits.#max_header_count) Err.Headers_too_large;
    let #(name, name_span, value_span, pos, has_bare_cr) = Parser.parse_header pst ~pos in
    Err.when_ has_bare_cr Err.Bare_cr_detected;
    let next_count = add16 st.#count one16 in
    match name with
    | Header_name.Content_length ->
      Err.when_ st.#has_te Err.Ambiguous_framing;
      let #(parsed_len, overflow) = Span.parse_int64 pst.#buf value_span in
      Err.when_ (overflow || I64.compare parsed_len limits.#max_content_length > 0)
        Err.Content_length_overflow;
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; content_len = parsed_len; has_cl = true }
    | Header_name.Transfer_encoding ->
      Err.when_ st.#has_cl Err.Ambiguous_framing;
      let is_chunked = Span.equal_caseless pst.#buf value_span "chunked" in
      let is_identity = Span.equal_caseless pst.#buf value_span "identity" in
      Err.when_ (not (is_chunked || is_identity)) Err.Unsupported_transfer_encoding;
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; chunked = is_chunked; has_te = true }
    | Header_name.Host ->
      let hdr = { Header.name; name_span; value = value_span } in
      parse_headers_loop pst ~pos ~acc:(hdr :: acc) ~limits
        #{ st with count = next_count; has_host = true }
    | Header_name.Connection ->
      let new_conn = parse_connection_value pst.#buf value_span ~default:st.#conn in
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; conn = new_conn }
    | Header_name.Expect ->
      let is_continue = Span.equal_caseless pst.#buf value_span "100-continue" in
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; expect_continue = is_continue || st.#expect_continue }
    | _ ->
      let hdr = { Header.name; name_span; value = value_span } in
      parse_headers_loop pst ~pos ~acc:(hdr :: acc) ~limits
        #{ st with count = next_count }
  )

(* Parse HTTP request with configurable limits and full RFC 7230 validation.
   Uses Parser combinators for cleaner, more maintainable parsing. *)
let parse (buf : buffer) ~(len : int16#) ~limits = exclave_
  let open Buf_read in
  if to_int len > buffer_size || gt16 len limits.#max_header_size then
    error_result Headers_too_large
  else
    try
      let pst = Parser.make buf ~len in
      let #(meth, target, version, pos) = Parser.request_line pst ~pos:(i16 0) in
      let #(body_off, st, headers) =
        parse_headers_loop pst ~pos ~acc:[] initial_header_state ~limits
      in
      (* Only missing Host header needs end-of-parse check *)
      match (version, st.#has_host) with
      | (Version.Http_1_1, false) -> error_result Missing_host_header
      | _ -> build_request ~meth ~target ~version ~body_off st ~headers
    with Err.Parse_error status ->
      error_result status

