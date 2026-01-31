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

(* CRLF as int16: little-endian 0x0A0D = '\n' << 8 | '\r' *)
let crlf_int16 = 0x0A0D

let find_crlf (local_ buf : bytes) ~(pos : int16#) ~(len : int16#) : int16# =
  let pos = to_int pos in
  let len = to_int len in
  if len - pos < 2
  then i16 (-1)
  else (
    let mutable p = pos in
    let mutable found = false in
    let last_check = len - 2 in
    while (not found) && p <= last_check do
      (* Read 2 bytes at once and compare to CRLF constant *)
      if Bytes.unsafe_get_int16 buf p = crlf_int16
      then found <- true
      else p <- p + 1
    done;
    if found then i16 p else i16 (-1))
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

(* Detect bare CR (CR not followed by LF) - RFC 7230 Section 3.5
   Used to prevent HTTP request smuggling attacks *)
let[@inline] has_bare_cr (local_ buf : bytes) ~(pos : int16#) ~(len : int16#) =
  let pos = to_int pos in
  let len = to_int len in
  let end_pos = pos + len in
  let mutable p = pos in
  let mutable found = false in
  while (not found) && p < end_pos do
    if peek buf (i16 p) =. #'\r' then (
      if p + 1 >= end_pos || peek buf (i16 (p + 1)) <>. #'\n' then
        found <- true
      else
        p <- p + 2  (* Skip past valid CRLF *)
    ) else
      p <- p + 1
  done;
  found
;;

(* Check if a value contains CRLF injection attempt *)
let[@inline] has_crlf_injection (local_ buf : bytes) ~(pos : int16#) ~(len : int16#) =
  let pos = to_int pos in
  let len = to_int len in
  let end_pos = pos + len in
  let mutable p = pos in
  let mutable found = false in
  while (not found) && p < end_pos do
    match peek buf (i16 p) with
    | #'\r' | #'\n' -> found <- true
    | _ -> p <- p + 1
  done;
  found
;;
