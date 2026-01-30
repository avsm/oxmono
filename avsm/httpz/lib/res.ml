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

let status_code = function
  | Continue -> 100
  | Switching_protocols -> 101
  | Success -> 200
  | Created -> 201
  | Accepted -> 202
  | No_content -> 204
  | Partial_content -> 206
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
  | Upgrade_required -> 426
  | Precondition_required -> 428
  | Too_many_requests -> 429
  | Internal_server_error -> 500
  | Not_implemented -> 501
  | Bad_gateway -> 502
  | Service_unavailable -> 503
  | Gateway_timeout -> 504
  | Http_version_not_supported -> 505
;;

let status_of_int = function
  | 100 -> Some Continue
  | 101 -> Some Switching_protocols
  | 200 -> Some Success
  | 201 -> Some Created
  | 202 -> Some Accepted
  | 204 -> Some No_content
  | 206 -> Some Partial_content
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
  | 426 -> Some Upgrade_required
  | 428 -> Some Precondition_required
  | 429 -> Some Too_many_requests
  | 500 -> Some Internal_server_error
  | 501 -> Some Not_implemented
  | 502 -> Some Bad_gateway
  | 503 -> Some Service_unavailable
  | 504 -> Some Gateway_timeout
  | 505 -> Some Http_version_not_supported
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
  | Upgrade_required -> "Upgrade Required"
  | Precondition_required -> "Precondition Required"
  | Too_many_requests -> "Too Many Requests"
  | Internal_server_error -> "Internal Server Error"
  | Not_implemented -> "Not Implemented"
  | Bad_gateway -> "Bad Gateway"
  | Service_unavailable -> "Service Unavailable"
  | Gateway_timeout -> "Gateway Timeout"
  | Http_version_not_supported -> "HTTP Version Not Supported"
;;

let status_to_string t = Stdlib.Printf.sprintf "%d %s" (status_code t) (status_reason t)

let pp_status fmt t = Stdlib.Format.fprintf fmt "%s" (status_to_string t)

let write_status_line dst ~off status version =
  let off = Buf_write.string dst ~off (Version.to_string version) in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.int dst ~off (status_code status) in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off (status_reason status) in
  Buf_write.crlf dst ~off
;;

let write_header dst ~off name value =
  let off = Buf_write.string dst ~off name in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off value in
  Buf_write.crlf dst ~off
;;

let write_header_int dst ~off name value =
  let off = Buf_write.string dst ~off name in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.int dst ~off value in
  Buf_write.crlf dst ~off
;;

let write_header_name dst ~off name value =
  write_header dst ~off (Header_name.canonical name) value
;;

let write_header_name_int dst ~off name value =
  write_header_int dst ~off (Header_name.canonical name) value
;;

let write_crlf dst ~off = Buf_write.crlf dst ~off

let write_content_length dst ~off len =
  write_header_name_int dst ~off Header_name.Content_length len
;;

let write_connection dst ~off ~keep_alive =
  let value = if keep_alive then "keep-alive" else "close" in
  write_header_name dst ~off Header_name.Connection value
;;

(* Chunked Transfer Encoding - RFC 7230 Section 4.1 *)

let write_transfer_encoding_chunked dst ~off =
  write_header_name dst ~off Header_name.Transfer_encoding "chunked"
;;

let write_chunk_header dst ~off ~size =
  let off = Buf_write.hex dst ~off size in
  Buf_write.crlf dst ~off
;;

let write_chunk_footer dst ~off =
  Buf_write.crlf dst ~off
;;

let write_final_chunk dst ~off =
  let off = Buf_write.char dst ~off '0' in
  let off = Buf_write.crlf dst ~off in
  Buf_write.crlf dst ~off
;;
