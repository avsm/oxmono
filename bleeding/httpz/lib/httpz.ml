open Base

module Scan = Scan
module Buf_read = Buf_read
module Buf_write = Buf_write
module Span = Span
module Method = Method
module Version = Version
module Header_name = Header_name
module Header = Header
module Upgrade = Upgrade
module Req = Req
module Target = Target
module Chunk = Chunk
module Res = Res
module Parser = Parser
module Err = Err
module Etag = Etag
module Date = Date
module Range = Range
module Urlencoded = Urlencoded
module Multipart = Multipart
module Media = Media
module Json = Json
module Sse = Sse
module Raw = Uriz.Raw
module Uriz = Httpz_uri
module Uri_template = Uri_template
module Ip = Ip

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
let default_limits = Buf_read.default_limits

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline] i16 x = I16.of_int x
let[@inline] gt16 a b = I16.compare a b > 0
let[@inline] gte16 a b = I16.compare a b >= 0
let[@inline] add16 a b = I16.add a b
let one16 : int16# = I16.of_int 1

type header_state =
 #{ count : int16#
  ; content_len : int64#
  ; chunked : bool
  ; conn : Parser.conn_value
  ; connection_upgrade : bool
  ; has_cl : bool
  ; has_te : bool
  ; has_host : bool
  ; expect_continue : bool
  ; unsupported_expectation : bool
  ; host : Span.t
  }

let minus_one_i64 : int64# = I64.of_int64 (-1L)

let initial_header_state : header_state =
 #{ count = i16 0
  ; content_len = minus_one_i64
  ; chunked = false
  ; conn = Parser.Conn_default
  ; connection_upgrade = false
  ; has_cl = false
  ; has_te = false
  ; has_host = false
  ; expect_continue = false
  ; unsupported_expectation = false
  ; host = Span.make ~off:(i16 0) ~len:(i16 0)
  }

let[@inline] error_result status = exclave_
  #( status
   , #{ Req.meth = Method.Get
      ; target = Span.make ~off:(i16 0) ~len:(i16 0)
      ; path = Span.make ~off:(i16 0) ~len:(i16 0)
      ; query = Span.make ~off:(i16 0) ~len:(i16 0)
      ; version = Version.Http_1_1
      ; body_off = i16 0
      ; content_length = minus_one_i64
      ; is_chunked = false
      ; keep_alive = false
      ; connection_upgrade = false
      ; expect_continue = false
      ; unsupported_expectation = false
      }
   , ([] : Header.t list) )

let[@inline] build_request ~meth ~target ~target_parsed ~version ~(body_off : int16#)
    (st : header_state) ~headers = exclave_
  let keep_alive =
    match st.#conn with
    | Parser.Conn_close -> false
    | Parser.Conn_keep_alive -> true
    | Parser.Conn_default -> phys_equal version Version.Http_1_1
  in
  let req =
    #{ Req.meth
     ; target
     ; path = Target.path target_parsed
     ; query = Target.query target_parsed
     ; version
     ; body_off
     ; content_length = st.#content_len
     ; is_chunked = st.#chunked
     ; keep_alive
     ; connection_upgrade = st.#connection_upgrade
     ; expect_continue =
         phys_equal version Version.Http_1_1 && st.#expect_continue
     ; unsupported_expectation =
         phys_equal version Version.Http_1_1 && st.#unsupported_expectation
     }
  in
  #(Buf_read.Complete, req, headers)

let rec parse_headers_loop (pst : Parser.pstate) ~pos ~acc (st : header_state) ~limits
  : #(int16# * header_state * Header.t list) = exclave_
  let open Buf_read in
  if Parser.is_headers_end pst ~pos then (
    let pos = Parser.end_headers pst ~pos in
    #(pos, st, acc)
  )
  else (
    Err.when_ (gte16 st.#count limits.#max_header_count) Err.Headers_too_large;
    let #(name, name_span, value_span, pos) = Parser.parse_header pst ~pos in
    let next_count = add16 st.#count one16 in
    match name with
    | Header_name.Content_length ->
      Err.when_ st.#has_te Err.Ambiguous_framing;
      let parsed_len =
        Parser.content_length_value
          pst.#buf
          value_span
          ~has_cl:st.#has_cl
          ~current:st.#content_len
          ~max_content_length:limits.#max_content_length
      in
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; content_len = parsed_len; has_cl = true }
    | Header_name.Transfer_encoding ->
      Err.when_ st.#has_cl Err.Ambiguous_framing;
      Err.when_ st.#has_te Err.Ambiguous_framing;
      let #(count, chunked_count, is_chunked, valid) =
        Span.parse_transfer_encoding pst.#buf value_span
      in
      Err.when_
        ((not valid) || count = 0 || chunked_count > 1 || not is_chunked)
        Err.Ambiguous_framing;
      Err.when_ (count <> 1) Err.Unsupported_transfer_encoding;
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with count = next_count; chunked = is_chunked; has_te = true }
    | Header_name.Host ->
      Err.when_ st.#has_host Err.Missing_host_header;
      Err.when_ (not (Target.valid_host pst.#buf value_span)) Err.Invalid_header;
      let hdr = { Header.name; name_span; value = value_span } in
      parse_headers_loop pst ~pos ~acc:(hdr :: acc) ~limits
        #{ st with count = next_count; has_host = true; host = value_span }
    | Header_name.Connection ->
      let new_conn =
        Parser.parse_connection_value pst.#buf value_span ~default:st.#conn
      in
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with
           count = next_count
         ; conn = new_conn
         ; connection_upgrade =
             st.#connection_upgrade
             || Span.token_list_contains pst.#buf value_span "upgrade"
         }
    | Header_name.Expect ->
      let #(count, all_continue) =
        Span.token_list_all_are pst.#buf value_span "100-continue"
      in
      let is_continue = count > 0 && all_continue in
      parse_headers_loop pst ~pos ~acc ~limits
        #{ st with
           count = next_count
         ; expect_continue = is_continue || st.#expect_continue
         ; unsupported_expectation = (not is_continue) || st.#unsupported_expectation
         }
    | _ ->
      let hdr = { Header.name; name_span; value = value_span } in
      parse_headers_loop pst ~pos ~acc:(hdr :: acc) ~limits
        #{ st with count = next_count }
  )

let[@zero_alloc] parse (buf : buffer) ~(len : int16#) ~limits = exclave_
  let open Buf_read in
  if to_int len > buffer_size then error_result Headers_too_large
  else
    try
      let pst = Parser.make buf ~len in
      let #(meth, target, target_parsed, version, pos) =
        Parser.request_line pst ~pos:(i16 0) ~limits
      in
      let #(body_off, st, headers) =
        parse_headers_loop pst ~pos ~acc:[] initial_header_state ~limits
      in
      (* The limit bounds the head, not the read: body bytes that arrived in
         the same segment are not part of it. [Res.parse] checks the same way. *)
      Err.when_ (gt16 body_off limits.#max_header_size) Err.Headers_too_large;
      Err.when_
        (phys_equal version Version.Http_1_0 && st.#has_te)
        Err.Unsupported_transfer_encoding;
      (* RFC 9112 3.2.2: an absolute-form target carries its own authority, and
         a Host naming a different one leaves this server and any intermediary
         disagreeing about the origin. Refuse the pair rather than pick one. *)
      Err.when_
        (st.#has_host
         && Target.is_absolute target_parsed
         && not (Target.authority_matches buf target_parsed st.#host))
        Err.Invalid_header;
      match (version, st.#has_host) with
      | (Version.Http_1_1, false) -> error_result Missing_host_header
      | _ -> build_request ~meth ~target ~target_parsed ~version ~body_off st ~headers
    with Err.Parse_error status ->
      error_result status
