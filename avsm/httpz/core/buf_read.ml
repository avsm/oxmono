(* parse_buffer.ml - Buffer type and utilities for HTTP parsing *)

type status =
  | Complete
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow    (* Content-Length value too large or invalid *)
  | Ambiguous_framing          (* Both Content-Length and Transfer-Encoding present *)
  | Bare_cr_detected           (* CR without LF - HTTP smuggling attempt *)
  | Missing_host_header        (* HTTP/1.1 requires Host header *)
  | Unsupported_transfer_encoding (* Transfer-Encoding other than chunked/identity *)

let status_to_string = function
  | Complete -> "Complete"
  | Partial -> "Partial"
  | Invalid_method -> "Invalid_method"
  | Invalid_target -> "Invalid_target"
  | Invalid_version -> "Invalid_version"
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

(* int16# conversion helpers *)
module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u
let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x

let buffer_size = 32768
let max_headers : int16# = i16 32

(* Unboxed char helpers *)
module Char_u = Stdlib_stable.Char_u
let[@inline always] char_u c = Char_u.of_char c

let[@inline always] peek (local_ buf : bytes) (pos : int16#) : char# =
  char_u (Bytes.unsafe_get buf (to_int pos))
let[@inline always] ( =. ) (a : char#) (b : char#) = Char_u.equal a b
let[@inline always] ( <>. ) (a : char#) (b : char#) = not (Char_u.equal a b)

let[@inline always] is_token_char (c : char#) =
  match c with
  | #'a' .. #'z' | #'A' .. #'Z' | #'0' .. #'9' -> true
  | #'!' | #'#' | #'$' | #'%' | #'&' | #'\'' | #'*' | #'+' | #'-' | #'.' -> true
  | #'^' | #'_' | #'`' | #'|' | #'~' -> true
  | _ -> false
;;

let[@inline always] is_space (c : char#) =
  match c with
  | #' ' | #'\t' -> true
  | _ -> false
;;

let[@inline always] to_lower (c : char#) : char# =
  match c with
  | #'A' .. #'Z' -> Char_u.chr (Char_u.code c + 32)
  | _ -> c
;;

(* Find CRLF and check for bare CR in one pass.
   Returns #(crlf_pos, has_bare_cr) where crlf_pos is -1 if not found.
   A bare CR is any CR not immediately followed by LF (RFC 7230 Section 3.5). *)
let find_crlf_check_bare_cr (local_ buf : bytes) ~(pos : int16#) ~(len : int16#)
  : #(int16# * bool) =
  let pos = to_int pos in
  let len = to_int len in
  if len - pos < 2
  then #(i16 (-1), false)
  else (
    let mutable p = pos in
    let mutable found_crlf = false in
    let mutable found_bare_cr = false in
    let last_check = len - 2 in
    while (not found_crlf) && p <= last_check do
      let c = Bytes.unsafe_get buf p in
      if Char.equal c '\r' then (
        (* Check if followed by LF *)
        if Char.equal (Bytes.unsafe_get buf (p + 1)) '\n'
        then found_crlf <- true  (* Valid CRLF - stop here *)
        else (
          found_bare_cr <- true;  (* Bare CR detected *)
          p <- p + 1
        )
      ) else
        p <- p + 1
    done;
    (* Check final position for lone CR at end *)
    if (not found_crlf) && (not found_bare_cr) && p = last_check + 1 && p < len then (
      if Char.equal (Bytes.unsafe_get buf p) '\r' then
        found_bare_cr <- true
    );
    let crlf_pos = if found_crlf then i16 p else i16 (-1) in
    #(crlf_pos, found_bare_cr))
;;

let pp fmt _t = Stdlib.Format.fprintf fmt "<buffer %d bytes>" buffer_size

(* Security limits - configurable per-server *)
type limits =
  #{ max_content_length : int64#  (* Default: 100MB *)
   ; max_header_size : int16#     (* Default: 16KB - size of all headers combined *)
   ; max_header_count : int16#    (* Default: 100 *)
   ; max_chunk_size : int         (* Default: 16MB *)
   }

let default_limits =
  #{ max_content_length = #104857600L  (* 100MB *)
   ; max_header_size = i16 16384       (* 16KB *)
   ; max_header_count = i16 100
   ; max_chunk_size = 16777216         (* 16MB *)
   }

