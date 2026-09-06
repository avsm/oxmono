type status =
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

let status_to_string = function
  | Complete -> "Complete"
  | Partial -> "Partial"
  | Invalid_method -> "Invalid_method"
  | Unsupported_method -> "Unsupported_method"
  | Invalid_target -> "Invalid_target"
  | Uri_too_long -> "Uri_too_long"
  | Invalid_version -> "Invalid_version"
  | Invalid_status -> "Invalid_status"
  | Invalid_header -> "Invalid_header"
  | Headers_too_large -> "Headers_too_large"
  | Malformed -> "Malformed"
  | Content_length_overflow -> "Content_length_overflow"
  | Ambiguous_framing -> "Ambiguous_framing"
  | Bare_cr_detected -> "Bare_cr_detected"
  | Missing_host_header -> "Missing_host_header"
  | Unsupported_transfer_encoding -> "Unsupported_transfer_encoding"
;;

let pp_status fmt t = Stdlib.Format.fprintf fmt "%s" (status_to_string t)

open Base
module I16 = Stdlib_stable.Int16_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let buffer_size = 32768

module Char_u = Stdlib_stable.Char_u

let[@inline always] char_u c = Char_u.of_char c

let[@inline always] peek (local_ (buf : bytes)) (pos : int16#) : char# =
  char_u (Bytes.unsafe_get buf (to_int pos))
;;

let[@inline always] ( =. ) (a : char#) (b : char#) = Char_u.equal a b
let[@inline always] ( <>. ) (a : char#) (b : char#) = not (Char_u.equal a b)
let[@inline always] is_token_char (c : char#) = Httpz_syntax.is_token_char c

let[@inline always] skip_token (local_ (buf : bytes)) ~pos ~limit =
  Scan.skip_token buf ~pos ~limit
;;

let[@inline always] is_space (c : char#) = Httpz_syntax.is_space c

let[@inline always] is_field_value_char (c : char#) = Httpz_syntax.is_field_value_char c

let[@inline always] is_qdtext_char (c : char#) = Httpz_syntax.is_qdtext_char c

let[@inline always] is_quoted_pair_char (c : char#) = Httpz_syntax.is_quoted_pair_char c

let[@inline always] is_digit (c : char#) = Httpz_syntax.is_digit c

let[@inline always] digit_value (c : char#) : int = Httpz_syntax.digit_value c

let[@inline always] skip_ows (local_ (buf : bytes)) ~(pos : int16#) ~(len : int16#)
  : int16#
  =
  let mutable p = to_int pos in
  let len = to_int len in
  while p < len && is_space (peek buf (i16 p)) do
    p <- p + 1
  done;
  i16 p
;;

let[@inline always] to_lower (c : char#) : char# = Httpz_syntax.to_lower c

(* A final CR is bare because no LF can follow it within the input window. *)
let find_crlf_check_bare_cr (local_ (buf : bytes)) ~(pos : int16#) ~(len : int16#)
  : #(int16# * bool)
  =
  let pos = to_int pos in
  let len = to_int len in
  if len - pos < 2
  then #(i16 (-1), false)
  else (
    let last_check = len - 2 in
    let mutable p = pos in
    let mutable crlf_pos = -1 in
    let mutable found_bare_cr = false in
    let mutable stop = false in
    while not stop do
      let cr = Scan.find_cr buf ~pos:p ~limit:len in
      let mutable i = p in
      while i < cr do
        if peek buf (i16 i) =. #'\n' then found_bare_cr <- true;
        i <- i + 1
      done;
      if cr >= len
      then stop <- true
      else if cr > last_check
      then (
        found_bare_cr <- true;
        stop <- true)
      else if peek buf (i16 (cr + 1)) =. #'\n'
      then (
        crlf_pos <- cr;
        stop <- true)
      else (
        found_bare_cr <- true;
        p <- cr + 1)
    done;
    #(i16 crlf_pos, found_bare_cr))
;;

let valid_field_value (local_ (buf : bytes)) ~(pos : int16#) ~(len : int16#) =
  let mutable p = to_int pos in
  let stop = to_int len in
  let mutable valid = true in
  while valid && p < stop do
    valid <- is_field_value_char (peek buf (i16 p));
    p <- p + 1
  done;
  valid
;;

type limits =
  #{ max_content_length : int64#
   ; max_header_size : int16#
   ; max_header_count : int16#
   ; max_chunk_size : int
   ; max_target_length : int16#
   }

let default_limits =
  #{ max_content_length = #104857600L
   ; max_header_size = i16 16384
   ; max_header_count = i16 100
   ; max_chunk_size = 16777216
   ; max_target_length = i16 8192
   }
;;
