type status =
  | Continue
  | Switching_protocols
  | Success
  | Created
  | Accepted
  | No_content
  | Reset_content
  | Partial_content
  | Multi_status
  | Moved_permanently
  | Found
  | See_other
  | Not_modified
  | Temporary_redirect
  | Permanent_redirect
  | Bad_request
  | Unauthorized
  | Forbidden
  | Not_found
  | Method_not_allowed
  | Not_acceptable
  | Proxy_authentication_required
  | Request_timeout
  | Conflict
  | Gone
  | Length_required
  | Precondition_failed
  | Payload_too_large
  | Uri_too_long
  | Unsupported_media_type
  | Range_not_satisfiable
  | Expectation_failed
  | Unprocessable_entity
  | Locked
  | Failed_dependency
  | Upgrade_required
  | Precondition_required
  | Too_many_requests
  | Request_header_fields_too_large
  | Internal_server_error
  | Not_implemented
  | Bad_gateway
  | Service_unavailable
  | Gateway_timeout
  | Http_version_not_supported
  | Insufficient_storage

(* One table of constant tuples keeps the code, reason phrase and the two preformatted
   lines in step. The tuples are structured constants, so the projections below load from
   static data rather than allocate. *)
let status_info = function
  | Continue -> 100, "Continue", "100 Continue", "HTTP/1.1 100 Continue\r\n"
  | Switching_protocols ->
    ( 101
    , "Switching Protocols"
    , "101 Switching Protocols"
    , "HTTP/1.1 101 Switching Protocols\r\n" )
  | Success -> 200, "OK", "200 OK", "HTTP/1.1 200 OK\r\n"
  | Created -> 201, "Created", "201 Created", "HTTP/1.1 201 Created\r\n"
  | Accepted -> 202, "Accepted", "202 Accepted", "HTTP/1.1 202 Accepted\r\n"
  | No_content -> 204, "No Content", "204 No Content", "HTTP/1.1 204 No Content\r\n"
  | Reset_content ->
    205, "Reset Content", "205 Reset Content", "HTTP/1.1 205 Reset Content\r\n"
  | Partial_content ->
    206, "Partial Content", "206 Partial Content", "HTTP/1.1 206 Partial Content\r\n"
  | Multi_status ->
    207, "Multi-Status", "207 Multi-Status", "HTTP/1.1 207 Multi-Status\r\n"
  | Moved_permanently ->
    ( 301
    , "Moved Permanently"
    , "301 Moved Permanently"
    , "HTTP/1.1 301 Moved Permanently\r\n" )
  | Found -> 302, "Found", "302 Found", "HTTP/1.1 302 Found\r\n"
  | See_other -> 303, "See Other", "303 See Other", "HTTP/1.1 303 See Other\r\n"
  | Not_modified ->
    304, "Not Modified", "304 Not Modified", "HTTP/1.1 304 Not Modified\r\n"
  | Temporary_redirect ->
    ( 307
    , "Temporary Redirect"
    , "307 Temporary Redirect"
    , "HTTP/1.1 307 Temporary Redirect\r\n" )
  | Permanent_redirect ->
    ( 308
    , "Permanent Redirect"
    , "308 Permanent Redirect"
    , "HTTP/1.1 308 Permanent Redirect\r\n" )
  | Bad_request -> 400, "Bad Request", "400 Bad Request", "HTTP/1.1 400 Bad Request\r\n"
  | Unauthorized ->
    401, "Unauthorized", "401 Unauthorized", "HTTP/1.1 401 Unauthorized\r\n"
  | Forbidden -> 403, "Forbidden", "403 Forbidden", "HTTP/1.1 403 Forbidden\r\n"
  | Not_found -> 404, "Not Found", "404 Not Found", "HTTP/1.1 404 Not Found\r\n"
  | Method_not_allowed ->
    ( 405
    , "Method Not Allowed"
    , "405 Method Not Allowed"
    , "HTTP/1.1 405 Method Not Allowed\r\n" )
  | Not_acceptable ->
    406, "Not Acceptable", "406 Not Acceptable", "HTTP/1.1 406 Not Acceptable\r\n"
  | Proxy_authentication_required ->
    ( 407
    , "Proxy Authentication Required"
    , "407 Proxy Authentication Required"
    , "HTTP/1.1 407 Proxy Authentication Required\r\n" )
  | Request_timeout ->
    408, "Request Timeout", "408 Request Timeout", "HTTP/1.1 408 Request Timeout\r\n"
  | Conflict -> 409, "Conflict", "409 Conflict", "HTTP/1.1 409 Conflict\r\n"
  | Gone -> 410, "Gone", "410 Gone", "HTTP/1.1 410 Gone\r\n"
  | Length_required ->
    411, "Length Required", "411 Length Required", "HTTP/1.1 411 Length Required\r\n"
  | Precondition_failed ->
    ( 412
    , "Precondition Failed"
    , "412 Precondition Failed"
    , "HTTP/1.1 412 Precondition Failed\r\n" )
  | Payload_too_large ->
    ( 413
    , "Payload Too Large"
    , "413 Payload Too Large"
    , "HTTP/1.1 413 Payload Too Large\r\n" )
  | Uri_too_long ->
    414, "URI Too Long", "414 URI Too Long", "HTTP/1.1 414 URI Too Long\r\n"
  | Unsupported_media_type ->
    ( 415
    , "Unsupported Media Type"
    , "415 Unsupported Media Type"
    , "HTTP/1.1 415 Unsupported Media Type\r\n" )
  | Range_not_satisfiable ->
    ( 416
    , "Range Not Satisfiable"
    , "416 Range Not Satisfiable"
    , "HTTP/1.1 416 Range Not Satisfiable\r\n" )
  | Expectation_failed ->
    ( 417
    , "Expectation Failed"
    , "417 Expectation Failed"
    , "HTTP/1.1 417 Expectation Failed\r\n" )
  | Unprocessable_entity ->
    ( 422
    , "Unprocessable Entity"
    , "422 Unprocessable Entity"
    , "HTTP/1.1 422 Unprocessable Entity\r\n" )
  | Locked -> 423, "Locked", "423 Locked", "HTTP/1.1 423 Locked\r\n"
  | Failed_dependency ->
    ( 424
    , "Failed Dependency"
    , "424 Failed Dependency"
    , "HTTP/1.1 424 Failed Dependency\r\n" )
  | Upgrade_required ->
    426, "Upgrade Required", "426 Upgrade Required", "HTTP/1.1 426 Upgrade Required\r\n"
  | Precondition_required ->
    ( 428
    , "Precondition Required"
    , "428 Precondition Required"
    , "HTTP/1.1 428 Precondition Required\r\n" )
  | Too_many_requests ->
    ( 429
    , "Too Many Requests"
    , "429 Too Many Requests"
    , "HTTP/1.1 429 Too Many Requests\r\n" )
  | Request_header_fields_too_large ->
    ( 431
    , "Request Header Fields Too Large"
    , "431 Request Header Fields Too Large"
    , "HTTP/1.1 431 Request Header Fields Too Large\r\n" )
  | Internal_server_error ->
    ( 500
    , "Internal Server Error"
    , "500 Internal Server Error"
    , "HTTP/1.1 500 Internal Server Error\r\n" )
  | Not_implemented ->
    501, "Not Implemented", "501 Not Implemented", "HTTP/1.1 501 Not Implemented\r\n"
  | Bad_gateway -> 502, "Bad Gateway", "502 Bad Gateway", "HTTP/1.1 502 Bad Gateway\r\n"
  | Service_unavailable ->
    ( 503
    , "Service Unavailable"
    , "503 Service Unavailable"
    , "HTTP/1.1 503 Service Unavailable\r\n" )
  | Gateway_timeout ->
    504, "Gateway Timeout", "504 Gateway Timeout", "HTTP/1.1 504 Gateway Timeout\r\n"
  | Http_version_not_supported ->
    ( 505
    , "HTTP Version Not Supported"
    , "505 HTTP Version Not Supported"
    , "HTTP/1.1 505 HTTP Version Not Supported\r\n" )
  | Insufficient_storage ->
    ( 507
    , "Insufficient Storage"
    , "507 Insufficient Storage"
    , "HTTP/1.1 507 Insufficient Storage\r\n" )
;;

let status_code t =
  let code, _, _, _ = status_info t in
  code
;;

let status_reason t =
  let _, reason, _, _ = status_info t in
  reason
;;

let status_code_reason t =
  let _, _, code_reason, _ = status_info t in
  code_reason
;;

let status_line_http_1_1 t =
  let _, _, _, line = status_info t in
  line
;;

let status_of_int = function
  | 100 -> Some Continue
  | 101 -> Some Switching_protocols
  | 200 -> Some Success
  | 201 -> Some Created
  | 202 -> Some Accepted
  | 204 -> Some No_content
  | 205 -> Some Reset_content
  | 206 -> Some Partial_content
  | 207 -> Some Multi_status
  | 301 -> Some Moved_permanently
  | 302 -> Some Found
  | 303 -> Some See_other
  | 304 -> Some Not_modified
  | 307 -> Some Temporary_redirect
  | 308 -> Some Permanent_redirect
  | 400 -> Some Bad_request
  | 401 -> Some Unauthorized
  | 403 -> Some Forbidden
  | 404 -> Some Not_found
  | 405 -> Some Method_not_allowed
  | 406 -> Some Not_acceptable
  | 407 -> Some Proxy_authentication_required
  | 408 -> Some Request_timeout
  | 409 -> Some Conflict
  | 410 -> Some Gone
  | 411 -> Some Length_required
  | 412 -> Some Precondition_failed
  | 413 -> Some Payload_too_large
  | 414 -> Some Uri_too_long
  | 415 -> Some Unsupported_media_type
  | 416 -> Some Range_not_satisfiable
  | 417 -> Some Expectation_failed
  | 422 -> Some Unprocessable_entity
  | 423 -> Some Locked
  | 424 -> Some Failed_dependency
  | 426 -> Some Upgrade_required
  | 428 -> Some Precondition_required
  | 429 -> Some Too_many_requests
  | 431 -> Some Request_header_fields_too_large
  | 500 -> Some Internal_server_error
  | 501 -> Some Not_implemented
  | 502 -> Some Bad_gateway
  | 503 -> Some Service_unavailable
  | 504 -> Some Gateway_timeout
  | 505 -> Some Http_version_not_supported
  | 507 -> Some Insufficient_storage
  | _ -> None
;;

let[@zero_alloc] status_to_string t = status_code_reason t
let pp_status fmt t = Stdlib.Format.fprintf fmt "%s" (status_to_string t)

let[@inline] write_status_line dst ~off status version =
  match version with
  | Version.Http_1_1 -> Buf_write.string dst ~off (status_line_http_1_1 status)
  | Version.Http_1_0 ->
    let off = Buf_write.string dst ~off (Version.to_string version) in
    let off = Buf_write.char dst ~off ' ' in
    let off = Buf_write.string dst ~off (status_code_reason status) in
    Buf_write.crlf dst ~off
;;

let[@inline] write_header dst ~off (local_ name) (local_ value) =
  let off = Buf_write.string dst ~off name in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off value in
  Buf_write.crlf dst ~off
;;

let[@inline] write_header_int dst ~off (local_ name) value =
  let off = Buf_write.string dst ~off name in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.int dst ~off value in
  Buf_write.crlf dst ~off
;;

let[@inline] write_header_name dst ~off name (local_ value) =
  write_header dst ~off (Header_name.canonical name) value
;;

let[@inline] write_header_name_int dst ~off name value =
  write_header_int dst ~off (Header_name.canonical name) value
;;

let[@inline] write_crlf dst ~off = Buf_write.crlf dst ~off

let[@inline] write_content_length dst ~off len =
  write_header_name_int dst ~off Header_name.Content_length len
;;

let[@inline] write_connection dst ~off ~keep_alive =
  Buf_write.string
    dst
    ~off
    (if keep_alive then "Connection: keep-alive\r\n" else "Connection: close\r\n")
;;

let[@inline] write_transfer_encoding_chunked dst ~off =
  Buf_write.string dst ~off "Transfer-Encoding: chunked\r\n"
;;

let[@inline] write_chunk_header dst ~off ~size =
  let off = Buf_write.hex dst ~off size in
  Buf_write.crlf dst ~off
;;

let[@inline] write_chunk_footer dst ~off = Buf_write.crlf dst ~off

let[@inline] write_final_chunk dst ~off =
  let off = Buf_write.char dst ~off '0' in
  let off = Buf_write.crlf dst ~off in
  Buf_write.crlf dst ~off
;;

open Base
module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let[@inline always] add16 a b = I16.add a b
let[@inline always] gt16 a b = I16.compare a b > 0
let[@inline always] gte16 a b = I16.compare a b >= 0
let one16 : int16# = i16 1
let minus_one_i64 : int64# = I64.of_int64 (-1L)

type t =
  #{ version : Version.t
   ; code : int16#
   ; reason : Span.t
   ; body_off : int16#
   ; content_length : int64#
   ; is_chunked : bool
   ; bodyless : bool
   ; keep_alive : bool
   }

type header_state =
  #{ count : int16#
   ; content_len : int64#
   ; chunked : bool
   ; conn : Parser.conn_value
   ; has_cl : bool
   ; has_te : bool
   }

let initial_header_state : header_state =
  #{ count = i16 0
   ; content_len = minus_one_i64
   ; chunked = false
   ; conn = Parser.Conn_default
   ; has_cl = false
   ; has_te = false
   }
;;

let[@inline] error_result status = exclave_
  #( status
   , #{ version = Version.Http_1_1
      ; code = i16 0
      ; reason = Span.make ~off:(i16 0) ~len:(i16 0)
      ; body_off = i16 0
      ; content_length = minus_one_i64
      ; is_chunked = false
      ; bodyless = false
      ; keep_alive = false
      }
   , ([] : Header.t list) )
;;

let rec parse_headers_loop
  (pst : Parser.pstate)
  ~pos
  ~acc
  (st : header_state)
  ~(limits : Buf_read.limits)
  : #(int16# * header_state * Header.t list)
  = exclave_
  if Parser.is_headers_end pst ~pos
  then (
    let pos = Parser.end_headers pst ~pos in
    #(pos, st, acc))
  else (
    Err.when_ (gte16 st.#count limits.#max_header_count) Err.Headers_too_large;
    let #(name, name_span, value_span, pos) = Parser.parse_header pst ~pos in
    let next_count = add16 st.#count one16 in
    let hdr = { Header.name; name_span; value = value_span } in
    let acc = hdr :: acc in
    match name with
    | Header_name.Content_length ->
      let parsed_len =
        Parser.content_length_value
          pst.#buf
          value_span
          ~has_cl:st.#has_cl
          ~current:st.#content_len
          ~max_content_length:limits.#max_content_length
      in
      parse_headers_loop
        pst
        ~pos
        ~acc
        ~limits
        #{ st with count = next_count; content_len = parsed_len; has_cl = true }
    | Header_name.Transfer_encoding ->
      let #(count, chunked_count, is_chunked, valid) =
        Span.parse_transfer_encoding pst.#buf value_span
      in
      Err.when_
        ((not valid)
         || count = 0
         || chunked_count > 1
         || (chunked_count > 0 && not is_chunked)
         || (st.#chunked && count > 0))
        Err.Unsupported_transfer_encoding;
      parse_headers_loop
        pst
        ~pos
        ~acc
        ~limits
        #{ st with count = next_count; chunked = is_chunked; has_te = true }
    | Header_name.Connection ->
      let new_conn =
        Parser.parse_connection_value pst.#buf value_span ~default:st.#conn
      in
      parse_headers_loop
        pst
        ~pos
        ~acc
        ~limits
        #{ st with count = next_count; conn = new_conn }
    | _ -> parse_headers_loop pst ~pos ~acc ~limits #{ st with count = next_count })
;;

let[@inline] bodyless ~request_method code =
  match request_method with
  | Some Method.Head -> true
  | Some Method.Connect when code >= 200 && code < 300 -> true
  | Some _ | None -> (code >= 100 && code < 200) || code = 204 || code = 304
;;

let parse ?request_method (buf : bytes) ~(len : int16#) ~(limits : Buf_read.limits)
  = exclave_
  if to_int len > Buf_read.buffer_size
  then error_result Buf_read.Headers_too_large
  else (
    try
      let pst = Parser.make buf ~len in
      let #(version, code, reason, pos) = Parser.status_line pst ~pos:(i16 0) in
      let #(body_off, st, headers) =
        parse_headers_loop pst ~pos ~acc:[] initial_header_state ~limits
      in
      Err.when_ (gt16 body_off limits.#max_header_size) Err.Headers_too_large;
      Err.when_
        (phys_equal version Version.Http_1_0 && st.#has_te)
        Err.Unsupported_transfer_encoding;
      let bodyless = bodyless ~request_method (to_int code) in
      Err.when_ ((not bodyless) && st.#has_cl && st.#has_te) Err.Ambiguous_framing;
      let is_chunked = (not bodyless) && st.#has_te && st.#chunked in
      let close_delimited =
        (not bodyless)
        && ((st.#has_te && not st.#chunked) || ((not st.#has_te) && not st.#has_cl))
      in
      let keep_alive =
        if close_delimited
        then false
        else (
          match st.#conn with
          | Parser.Conn_close -> false
          | Parser.Conn_keep_alive -> true
          | Parser.Conn_default -> phys_equal version Version.Http_1_1)
      in
      #( Buf_read.Complete
       , #{ version
          ; code
          ; reason
          ; body_off
          ; content_length = st.#content_len
          ; is_chunked
          ; bodyless
          ; keep_alive
          }
       , headers )
    with
    | Err.Parse_error status -> error_result status)
;;

let pp fmt (r : t) =
  Stdlib.Format.fprintf
    fmt
    "#{ version = %a; code = %d; body_off = %d; content_length = %Ld; is_chunked = %b; \
     bodyless = %b; keep_alive = %b }"
    Version.pp
    r.#version
    (to_int r.#code)
    (to_int r.#body_off)
    (I64.to_int64 r.#content_length)
    r.#is_chunked
    r.#bodyless
    r.#keep_alive
;;

let pp_with_buf (buf : bytes) fmt (r : t) =
  Stdlib.Format.fprintf
    fmt
    "%s %d %s"
    (Version.to_string r.#version)
    (to_int r.#code)
    (Span.to_string buf r.#reason)
;;
