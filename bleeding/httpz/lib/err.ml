type status = Buf_read.status =
  | Complete
  | Partial
  | Invalid_method
  | Unsupported_method
  | Invalid_target
  | Uri_too_long
  | Invalid_version
  | Invalid_status
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow
  | Ambiguous_framing
  | Bare_cr_detected
  | Missing_host_header
  | Unsupported_transfer_encoding

exception Parse_error of status

(* One exception per status, built once. [parse] catches these to report a
   status, so one built at the point of failure would be a heap block on an
   otherwise allocation-free parse. *)
let complete = Parse_error Complete
let partial = Parse_error Partial
let invalid_method = Parse_error Invalid_method
let unsupported_method = Parse_error Unsupported_method
let invalid_target = Parse_error Invalid_target
let uri_too_long = Parse_error Uri_too_long
let invalid_version = Parse_error Invalid_version
let invalid_status = Parse_error Invalid_status
let invalid_header = Parse_error Invalid_header
let headers_too_large = Parse_error Headers_too_large
let malformed = Parse_error Malformed
let content_length_overflow = Parse_error Content_length_overflow
let ambiguous_framing = Parse_error Ambiguous_framing
let bare_cr_detected = Parse_error Bare_cr_detected
let missing_host_header = Parse_error Missing_host_header
let unsupported_transfer_encoding = Parse_error Unsupported_transfer_encoding

let[@inline] exn_of status =
  match status with
  | Complete -> complete
  | Partial -> partial
  | Invalid_method -> invalid_method
  | Unsupported_method -> unsupported_method
  | Invalid_target -> invalid_target
  | Uri_too_long -> uri_too_long
  | Invalid_version -> invalid_version
  | Invalid_status -> invalid_status
  | Invalid_header -> invalid_header
  | Headers_too_large -> headers_too_large
  | Malformed -> malformed
  | Content_length_overflow -> content_length_overflow
  | Ambiguous_framing -> ambiguous_framing
  | Bare_cr_detected -> bare_cr_detected
  | Missing_host_header -> missing_host_header
  | Unsupported_transfer_encoding -> unsupported_transfer_encoding
;;

let[@inline] fail status = raise (exn_of status)
let[@inline] when_ cond status = if cond then raise (exn_of status)
let[@inline] partial_when cond = if cond then raise partial
let[@inline] malformed_when cond = if cond then raise malformed
