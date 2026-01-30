(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP status codes following RFC 7231 and extensions *)

let src = Logs.Src.create "requests.status" ~doc:"HTTP Status Codes"
module Log = (val Logs.src_log src : Logs.LOG)

type informational = [
  | `Continue
  | `Switching_protocols
  | `Processing
  | `Early_hints
]

type success = [
  | `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  | `Multi_status
  | `Already_reported
  | `Im_used
]

type redirection = [
  | `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  | `Permanent_redirect
]

type client_error = [
  | `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Payload_too_large
  | `Uri_too_long
  | `Unsupported_media_type
  | `Range_not_satisfiable
  | `Expectation_failed
  | `I_m_a_teapot
  | `Misdirected_request
  | `Unprocessable_entity
  | `Locked
  | `Failed_dependency
  | `Too_early
  | `Upgrade_required
  | `Precondition_required
  | `Too_many_requests
  | `Request_header_fields_too_large
  | `Unavailable_for_legal_reasons
]

type server_error = [
  | `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported
  | `Variant_also_negotiates
  | `Insufficient_storage
  | `Loop_detected
  | `Not_extended
  | `Network_authentication_required
]

type standard = [
  | informational
  | success
  | redirection
  | client_error
  | server_error
]

type t = [
  | `Code of int
  | standard
]

let to_int = function
  (* Informational *)
  | `Continue -> 100
  | `Switching_protocols -> 101
  | `Processing -> 102
  | `Early_hints -> 103
  (* Success *)
  | `OK -> 200
  | `Created -> 201
  | `Accepted -> 202
  | `Non_authoritative_information -> 203
  | `No_content -> 204
  | `Reset_content -> 205
  | `Partial_content -> 206
  | `Multi_status -> 207
  | `Already_reported -> 208
  | `Im_used -> 226
  (* Redirection *)
  | `Multiple_choices -> 300
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Use_proxy -> 305
  | `Temporary_redirect -> 307
  | `Permanent_redirect -> 308
  (* Client Error *)
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Payment_required -> 402
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Not_acceptable -> 406
  | `Proxy_authentication_required -> 407
  | `Request_timeout -> 408
  | `Conflict -> 409
  | `Gone -> 410
  | `Length_required -> 411
  | `Precondition_failed -> 412
  | `Payload_too_large -> 413
  | `Uri_too_long -> 414
  | `Unsupported_media_type -> 415
  | `Range_not_satisfiable -> 416
  | `Expectation_failed -> 417
  | `I_m_a_teapot -> 418
  | `Misdirected_request -> 421
  | `Unprocessable_entity -> 422
  | `Locked -> 423
  | `Failed_dependency -> 424
  | `Too_early -> 425
  | `Upgrade_required -> 426
  | `Precondition_required -> 428
  | `Too_many_requests -> 429
  | `Request_header_fields_too_large -> 431
  | `Unavailable_for_legal_reasons -> 451
  (* Server Error *)
  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Bad_gateway -> 502
  | `Service_unavailable -> 503
  | `Gateway_timeout -> 504
  | `Http_version_not_supported -> 505
  | `Variant_also_negotiates -> 506
  | `Insufficient_storage -> 507
  | `Loop_detected -> 508
  | `Not_extended -> 510
  | `Network_authentication_required -> 511
  (* Custom code *)
  | `Code c -> c

let of_int = function
  (* Informational *)
  | 100 -> `Continue
  | 101 -> `Switching_protocols
  | 102 -> `Processing
  | 103 -> `Early_hints
  (* Success *)
  | 200 -> `OK
  | 201 -> `Created
  | 202 -> `Accepted
  | 203 -> `Non_authoritative_information
  | 204 -> `No_content
  | 205 -> `Reset_content
  | 206 -> `Partial_content
  | 207 -> `Multi_status
  | 208 -> `Already_reported
  | 226 -> `Im_used
  (* Redirection *)
  | 300 -> `Multiple_choices
  | 301 -> `Moved_permanently
  | 302 -> `Found
  | 303 -> `See_other
  | 304 -> `Not_modified
  | 305 -> `Use_proxy
  | 307 -> `Temporary_redirect
  | 308 -> `Permanent_redirect
  (* Client Error *)
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 402 -> `Payment_required
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | 406 -> `Not_acceptable
  | 407 -> `Proxy_authentication_required
  | 408 -> `Request_timeout
  | 409 -> `Conflict
  | 410 -> `Gone
  | 411 -> `Length_required
  | 412 -> `Precondition_failed
  | 413 -> `Payload_too_large
  | 414 -> `Uri_too_long
  | 415 -> `Unsupported_media_type
  | 416 -> `Range_not_satisfiable
  | 417 -> `Expectation_failed
  | 418 -> `I_m_a_teapot
  | 421 -> `Misdirected_request
  | 422 -> `Unprocessable_entity
  | 423 -> `Locked
  | 424 -> `Failed_dependency
  | 425 -> `Too_early
  | 426 -> `Upgrade_required
  | 428 -> `Precondition_required
  | 429 -> `Too_many_requests
  | 431 -> `Request_header_fields_too_large
  | 451 -> `Unavailable_for_legal_reasons
  (* Server Error *)
  | 500 -> `Internal_server_error
  | 501 -> `Not_implemented
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 504 -> `Gateway_timeout
  | 505 -> `Http_version_not_supported
  | 506 -> `Variant_also_negotiates
  | 507 -> `Insufficient_storage
  | 508 -> `Loop_detected
  | 510 -> `Not_extended
  | 511 -> `Network_authentication_required
  (* Unknown code *)
  | c -> `Code c

let to_string t = string_of_int (to_int t)

let reason_phrase t =
  match t with
  (* Informational *)
  | `Continue -> "Continue"
  | `Switching_protocols -> "Switching Protocols"
  | `Processing -> "Processing"
  | `Early_hints -> "Early Hints"
  (* Success *)
  | `OK -> "OK"
  | `Created -> "Created"
  | `Accepted -> "Accepted"
  | `Non_authoritative_information -> "Non-Authoritative Information"
  | `No_content -> "No Content"
  | `Reset_content -> "Reset Content"
  | `Partial_content -> "Partial Content"
  | `Multi_status -> "Multi-Status"
  | `Already_reported -> "Already Reported"
  | `Im_used -> "IM Used"
  (* Redirection *)
  | `Multiple_choices -> "Multiple Choices"
  | `Moved_permanently -> "Moved Permanently"
  | `Found -> "Found"
  | `See_other -> "See Other"
  | `Not_modified -> "Not Modified"
  | `Use_proxy -> "Use Proxy"
  | `Temporary_redirect -> "Temporary Redirect"
  | `Permanent_redirect -> "Permanent Redirect"
  (* Client Error *)
  | `Bad_request -> "Bad Request"
  | `Unauthorized -> "Unauthorized"
  | `Payment_required -> "Payment Required"
  | `Forbidden -> "Forbidden"
  | `Not_found -> "Not Found"
  | `Method_not_allowed -> "Method Not Allowed"
  | `Not_acceptable -> "Not Acceptable"
  | `Proxy_authentication_required -> "Proxy Authentication Required"
  | `Request_timeout -> "Request Timeout"
  | `Conflict -> "Conflict"
  | `Gone -> "Gone"
  | `Length_required -> "Length Required"
  | `Precondition_failed -> "Precondition Failed"
  | `Payload_too_large -> "Payload Too Large"
  | `Uri_too_long -> "URI Too Long"
  | `Unsupported_media_type -> "Unsupported Media Type"
  | `Range_not_satisfiable -> "Range Not Satisfiable"
  | `Expectation_failed -> "Expectation Failed"
  | `I_m_a_teapot -> "I'm a teapot"
  | `Misdirected_request -> "Misdirected Request"
  | `Unprocessable_entity -> "Unprocessable Entity"
  | `Locked -> "Locked"
  | `Failed_dependency -> "Failed Dependency"
  | `Too_early -> "Too Early"
  | `Upgrade_required -> "Upgrade Required"
  | `Precondition_required -> "Precondition Required"
  | `Too_many_requests -> "Too Many Requests"
  | `Request_header_fields_too_large -> "Request Header Fields Too Large"
  | `Unavailable_for_legal_reasons -> "Unavailable For Legal Reasons"
  (* Server Error *)
  | `Internal_server_error -> "Internal Server Error"
  | `Not_implemented -> "Not Implemented"
  | `Bad_gateway -> "Bad Gateway"
  | `Service_unavailable -> "Service Unavailable"
  | `Gateway_timeout -> "Gateway Timeout"
  | `Http_version_not_supported -> "HTTP Version Not Supported"
  | `Variant_also_negotiates -> "Variant Also Negotiates"
  | `Insufficient_storage -> "Insufficient Storage"
  | `Loop_detected -> "Loop Detected"
  | `Not_extended -> "Not Extended"
  | `Network_authentication_required -> "Network Authentication Required"
  (* Custom code - provide generic reason based on category *)
  | `Code c ->
      if c >= 100 && c < 200 then "Informational"
      else if c >= 200 && c < 300 then "Success"
      else if c >= 300 && c < 400 then "Redirection"
      else if c >= 400 && c < 500 then "Client Error"
      else if c >= 500 && c < 600 then "Server Error"
      else "Unknown"

(* Classification functions *)
let is_informational t =
  let code = to_int t in
  code >= 100 && code < 200

let is_success t =
  let code = to_int t in
  code >= 200 && code < 300

let is_redirection t =
  let code = to_int t in
  code >= 300 && code < 400

let is_client_error t =
  let code = to_int t in
  code >= 400 && code < 500

let is_server_error t =
  let code = to_int t in
  code >= 500 && code < 600

let is_error t =
  let code = to_int t in
  code >= 400 && code < 600

(* Retry policy functions *)
let is_retryable t =
  match t with
  | `Request_timeout
  | `Too_many_requests
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout -> true
  (* 501 and 505 indicate permanent conditions that won't be fixed by retrying *)
  | `Not_implemented -> false         (* 501: Server doesn't support the functionality *)
  | `Http_version_not_supported -> false  (* 505: Protocol version not supported *)
  | _ -> is_server_error t  (* Other 5xx errors are generally retryable *)

let should_retry_on_different_host t =
  match t with
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout -> true
  | _ -> false

(* Pretty printing *)
let pp ppf t =
  Format.fprintf ppf "%d" (to_int t)

let pp_hum ppf t =
  Format.fprintf ppf "%d %s" (to_int t) (reason_phrase t)