(* res.ml - HTTP response writing utilities *)

type status =
  (* 1xx Informational *)
  | Continue                (* 100 - for Expect: 100-continue *)
  | Switching_protocols     (* 101 - for Upgrade *)
  (* 2xx Success *)
  | Success                 (* 200 *)
  | Created                 (* 201 *)
  | Accepted                (* 202 *)
  | No_content              (* 204 *)
  | Partial_content         (* 206 - for Range requests *)
  | Multi_status            (* 207 - WebDAV multistatus *)
  (* 3xx Redirection *)
  | Moved_permanently       (* 301 *)
  | Found                   (* 302 *)
  | See_other               (* 303 *)
  | Not_modified            (* 304 *)
  | Temporary_redirect      (* 307 *)
  | Permanent_redirect      (* 308 *)
  (* 4xx Client Error *)
  | Bad_request             (* 400 *)
  | Unauthorized            (* 401 *)
  | Forbidden               (* 403 *)
  | Not_found               (* 404 *)
  | Method_not_allowed      (* 405 *)
  | Not_acceptable          (* 406 *)
  | Request_timeout         (* 408 *)
  | Conflict                (* 409 *)
  | Gone                    (* 410 *)
  | Length_required         (* 411 *)
  | Precondition_failed     (* 412 *)
  | Payload_too_large       (* 413 *)
  | Uri_too_long            (* 414 *)
  | Unsupported_media_type  (* 415 *)
  | Range_not_satisfiable   (* 416 *)
  | Expectation_failed      (* 417 *)
  | Unprocessable_entity    (* 422 *)
  | Locked                  (* 423 - WebDAV *)
  | Failed_dependency       (* 424 - WebDAV *)
  | Upgrade_required        (* 426 *)
  | Precondition_required   (* 428 *)
  | Too_many_requests       (* 429 *)
  (* 5xx Server Error *)
  | Internal_server_error   (* 500 *)
  | Not_implemented         (* 501 *)
  | Bad_gateway             (* 502 *)
  | Service_unavailable     (* 503 *)
  | Gateway_timeout         (* 504 *)
  | Http_version_not_supported (* 505 *)
  | Insufficient_storage   (* 507 - WebDAV *)

let status_code = function
  | Continue -> 100
  | Switching_protocols -> 101
  | Success -> 200
  | Created -> 201
  | Accepted -> 202
  | No_content -> 204
  | Partial_content -> 206
  | Multi_status -> 207
  | Moved_permanently -> 301
  | Found -> 302
  | See_other -> 303
  | Not_modified -> 304
  | Temporary_redirect -> 307
  | Permanent_redirect -> 308
  | Bad_request -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | Not_found -> 404
  | Method_not_allowed -> 405
  | Not_acceptable -> 406
  | Request_timeout -> 408
  | Conflict -> 409
  | Gone -> 410
  | Length_required -> 411
  | Precondition_failed -> 412
  | Payload_too_large -> 413
  | Uri_too_long -> 414
  | Unsupported_media_type -> 415
  | Range_not_satisfiable -> 416
  | Expectation_failed -> 417
  | Unprocessable_entity -> 422
  | Locked -> 423
  | Failed_dependency -> 424
  | Upgrade_required -> 426
  | Precondition_required -> 428
  | Too_many_requests -> 429
  | Internal_server_error -> 500
  | Not_implemented -> 501
  | Bad_gateway -> 502
  | Service_unavailable -> 503
  | Gateway_timeout -> 504
  | Http_version_not_supported -> 505
  | Insufficient_storage -> 507
;;

let status_of_int = function
  | 100 -> Some Continue
  | 101 -> Some Switching_protocols
  | 200 -> Some Success
  | 201 -> Some Created
  | 202 -> Some Accepted
  | 204 -> Some No_content
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
  | 500 -> Some Internal_server_error
  | 501 -> Some Not_implemented
  | 502 -> Some Bad_gateway
  | 503 -> Some Service_unavailable
  | 504 -> Some Gateway_timeout
  | 505 -> Some Http_version_not_supported
  | 507 -> Some Insufficient_storage
  | _ -> None
;;

let status_reason = function
  | Continue -> "Continue"
  | Switching_protocols -> "Switching Protocols"
  | Success -> "OK"
  | Created -> "Created"
  | Accepted -> "Accepted"
  | No_content -> "No Content"
  | Partial_content -> "Partial Content"
  | Multi_status -> "Multi-Status"
  | Moved_permanently -> "Moved Permanently"
  | Found -> "Found"
  | See_other -> "See Other"
  | Not_modified -> "Not Modified"
  | Temporary_redirect -> "Temporary Redirect"
  | Permanent_redirect -> "Permanent Redirect"
  | Bad_request -> "Bad Request"
  | Unauthorized -> "Unauthorized"
  | Forbidden -> "Forbidden"
  | Not_found -> "Not Found"
  | Method_not_allowed -> "Method Not Allowed"
  | Not_acceptable -> "Not Acceptable"
  | Request_timeout -> "Request Timeout"
  | Conflict -> "Conflict"
  | Gone -> "Gone"
  | Length_required -> "Length Required"
  | Precondition_failed -> "Precondition Failed"
  | Payload_too_large -> "Payload Too Large"
  | Uri_too_long -> "URI Too Long"
  | Unsupported_media_type -> "Unsupported Media Type"
  | Range_not_satisfiable -> "Range Not Satisfiable"
  | Expectation_failed -> "Expectation Failed"
  | Unprocessable_entity -> "Unprocessable Entity"
  | Locked -> "Locked"
  | Failed_dependency -> "Failed Dependency"
  | Upgrade_required -> "Upgrade Required"
  | Precondition_required -> "Precondition Required"
  | Too_many_requests -> "Too Many Requests"
  | Internal_server_error -> "Internal Server Error"
  | Not_implemented -> "Not Implemented"
  | Bad_gateway -> "Bad Gateway"
  | Service_unavailable -> "Service Unavailable"
  | Gateway_timeout -> "Gateway Timeout"
  | Http_version_not_supported -> "HTTP Version Not Supported"
  | Insufficient_storage -> "Insufficient Storage"
;;

(* Status code and reason phrase as one static string, so writing a status
   line is a single memcpy. Must agree with [status_code] and [status_reason]
   above; nothing but proximity enforces that. *)
let status_code_reason = function
  | Continue -> "100 Continue"
  | Switching_protocols -> "101 Switching Protocols"
  | Success -> "200 OK"
  | Created -> "201 Created"
  | Accepted -> "202 Accepted"
  | No_content -> "204 No Content"
  | Partial_content -> "206 Partial Content"
  | Multi_status -> "207 Multi-Status"
  | Moved_permanently -> "301 Moved Permanently"
  | Found -> "302 Found"
  | See_other -> "303 See Other"
  | Not_modified -> "304 Not Modified"
  | Temporary_redirect -> "307 Temporary Redirect"
  | Permanent_redirect -> "308 Permanent Redirect"
  | Bad_request -> "400 Bad Request"
  | Unauthorized -> "401 Unauthorized"
  | Forbidden -> "403 Forbidden"
  | Not_found -> "404 Not Found"
  | Method_not_allowed -> "405 Method Not Allowed"
  | Not_acceptable -> "406 Not Acceptable"
  | Request_timeout -> "408 Request Timeout"
  | Conflict -> "409 Conflict"
  | Gone -> "410 Gone"
  | Length_required -> "411 Length Required"
  | Precondition_failed -> "412 Precondition Failed"
  | Payload_too_large -> "413 Payload Too Large"
  | Uri_too_long -> "414 URI Too Long"
  | Unsupported_media_type -> "415 Unsupported Media Type"
  | Range_not_satisfiable -> "416 Range Not Satisfiable"
  | Expectation_failed -> "417 Expectation Failed"
  | Unprocessable_entity -> "422 Unprocessable Entity"
  | Locked -> "423 Locked"
  | Failed_dependency -> "424 Failed Dependency"
  | Upgrade_required -> "426 Upgrade Required"
  | Precondition_required -> "428 Precondition Required"
  | Too_many_requests -> "429 Too Many Requests"
  | Internal_server_error -> "500 Internal Server Error"
  | Not_implemented -> "501 Not Implemented"
  | Bad_gateway -> "502 Bad Gateway"
  | Service_unavailable -> "503 Service Unavailable"
  | Gateway_timeout -> "504 Gateway Timeout"
  | Http_version_not_supported -> "505 HTTP Version Not Supported"
  | Insufficient_storage -> "507 Insufficient Storage"
;;

(* The complete HTTP/1.1 status line. HTTP/1.0 responses are rare enough
   to assemble from the pieces above. *)
let status_line_http_1_1 = function
  | Continue -> "HTTP/1.1 100 Continue\r\n"
  | Switching_protocols -> "HTTP/1.1 101 Switching Protocols\r\n"
  | Success -> "HTTP/1.1 200 OK\r\n"
  | Created -> "HTTP/1.1 201 Created\r\n"
  | Accepted -> "HTTP/1.1 202 Accepted\r\n"
  | No_content -> "HTTP/1.1 204 No Content\r\n"
  | Partial_content -> "HTTP/1.1 206 Partial Content\r\n"
  | Multi_status -> "HTTP/1.1 207 Multi-Status\r\n"
  | Moved_permanently -> "HTTP/1.1 301 Moved Permanently\r\n"
  | Found -> "HTTP/1.1 302 Found\r\n"
  | See_other -> "HTTP/1.1 303 See Other\r\n"
  | Not_modified -> "HTTP/1.1 304 Not Modified\r\n"
  | Temporary_redirect -> "HTTP/1.1 307 Temporary Redirect\r\n"
  | Permanent_redirect -> "HTTP/1.1 308 Permanent Redirect\r\n"
  | Bad_request -> "HTTP/1.1 400 Bad Request\r\n"
  | Unauthorized -> "HTTP/1.1 401 Unauthorized\r\n"
  | Forbidden -> "HTTP/1.1 403 Forbidden\r\n"
  | Not_found -> "HTTP/1.1 404 Not Found\r\n"
  | Method_not_allowed -> "HTTP/1.1 405 Method Not Allowed\r\n"
  | Not_acceptable -> "HTTP/1.1 406 Not Acceptable\r\n"
  | Request_timeout -> "HTTP/1.1 408 Request Timeout\r\n"
  | Conflict -> "HTTP/1.1 409 Conflict\r\n"
  | Gone -> "HTTP/1.1 410 Gone\r\n"
  | Length_required -> "HTTP/1.1 411 Length Required\r\n"
  | Precondition_failed -> "HTTP/1.1 412 Precondition Failed\r\n"
  | Payload_too_large -> "HTTP/1.1 413 Payload Too Large\r\n"
  | Uri_too_long -> "HTTP/1.1 414 URI Too Long\r\n"
  | Unsupported_media_type -> "HTTP/1.1 415 Unsupported Media Type\r\n"
  | Range_not_satisfiable -> "HTTP/1.1 416 Range Not Satisfiable\r\n"
  | Expectation_failed -> "HTTP/1.1 417 Expectation Failed\r\n"
  | Unprocessable_entity -> "HTTP/1.1 422 Unprocessable Entity\r\n"
  | Locked -> "HTTP/1.1 423 Locked\r\n"
  | Failed_dependency -> "HTTP/1.1 424 Failed Dependency\r\n"
  | Upgrade_required -> "HTTP/1.1 426 Upgrade Required\r\n"
  | Precondition_required -> "HTTP/1.1 428 Precondition Required\r\n"
  | Too_many_requests -> "HTTP/1.1 429 Too Many Requests\r\n"
  | Internal_server_error -> "HTTP/1.1 500 Internal Server Error\r\n"
  | Not_implemented -> "HTTP/1.1 501 Not Implemented\r\n"
  | Bad_gateway -> "HTTP/1.1 502 Bad Gateway\r\n"
  | Service_unavailable -> "HTTP/1.1 503 Service Unavailable\r\n"
  | Gateway_timeout -> "HTTP/1.1 504 Gateway Timeout\r\n"
  | Http_version_not_supported -> "HTTP/1.1 505 HTTP Version Not Supported\r\n"
  | Insufficient_storage -> "HTTP/1.1 507 Insufficient Storage\r\n"
;;

(* The result is a shared static string rather than a fresh one; callers must
   not mutate it via [Bytes.unsafe_of_string] or rely on physical inequality
   of two results. *)
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

(* Chunked Transfer Encoding - RFC 7230 Section 4.1 *)

let[@inline] write_transfer_encoding_chunked dst ~off =
  Buf_write.string dst ~off "Transfer-Encoding: chunked\r\n"
;;

let[@inline] write_chunk_header dst ~off ~size =
  let off = Buf_write.hex dst ~off size in
  Buf_write.crlf dst ~off
;;

let[@inline] write_chunk_footer dst ~off =
  Buf_write.crlf dst ~off
;;

let[@inline] write_final_chunk dst ~off =
  let off = Buf_write.char dst ~off '0' in
  let off = Buf_write.crlf dst ~off in
  Buf_write.crlf dst ~off
;;
