(* parse_buffer.ml - Buffer type and utilities for HTTP parsing *)

type status =
  | Complete
  | Partial
  | Invalid_method
  | Invalid_target
  | Uri_too_long               (* Request-target longer than max_target_length *)
  | Invalid_version
  | Invalid_status             (* Malformed status line in a response *)
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

(* Both defined in {!Scan_portable}, so that {!Scan} can vectorise
   [skip_token] over the same character class and fall back to the scalar
   table loop for the sub-16-byte tail. *)
let[@inline always] is_token_char (c : char#) = Scan_portable.is_token_char c

(* Index of the first byte in [pos, limit) that is not a token character, or
   [limit] if there is none. No bounds checking is performed.

   {!Scan.skip_token} dispatches to the scalar loop itself when fewer than
   sixteen bytes remain. *)
let[@inline always] skip_token (local_ buf : bytes) ~pos ~limit =
  Scan.skip_token buf ~pos ~limit
;;

let[@inline always] is_space (c : char#) =
  match c with
  | #' ' | #'\t' -> true
  | _ -> false
;;

let[@inline always] is_digit (c : char#) =
  match c with
  | #'0' .. #'9' -> true
  | _ -> false
;;

(* Returns digit value 0-9, or -1 if not a digit *)
let[@inline always] digit_value (c : char#) : int =
  match c with
  | #'0' .. #'9' -> Char_u.code c - 48
  | _ -> -1
;;

(* Skip optional whitespace (OWS = SP / HTAB) *)
let[@inline always] skip_ows (local_ buf : bytes) ~(pos : int16#) ~(len : int16#) : int16# =
  let mutable p = to_int pos in
  let len = to_int len in
  while p < len && is_space (peek buf (i16 p)) do
    p <- p + 1
  done;
  i16 p
;;

let[@inline always] to_lower (c : char#) : char# =
  match c with
  | #'A' .. #'Z' -> Char_u.chr (Char_u.code c + 32)
  | _ -> c
;;

(* Find CRLF and check for bare CR in one pass.
   Returns #(crlf_pos, has_bare_cr) where crlf_pos is -1 if not found.
   A bare CR is any CR not immediately followed by LF (RFC 7230 Section 3.5).

   The scan between candidate CRs is delegated to {!Scan.find_cr}, which
   examines sixteen bytes at a time. Each CR is then classified here: followed
   by LF it ends the line, otherwise it is a bare CR and the search continues
   past it. A CR in the final byte cannot be followed by LF, so it is bare. *)
let find_crlf_check_bare_cr (local_ buf : bytes) ~(pos : int16#) ~(len : int16#)
  : #(int16# * bool) =
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
      if cr >= len
      then stop <- true (* no CR at all *)
      else if cr > last_check
      then (
        (* CR is the last byte: nothing can follow it *)
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

let pp fmt _t = Stdlib.Format.fprintf fmt "<buffer %d bytes>" buffer_size

(* Security limits - configurable per-server *)
type limits =
  #{ max_content_length : int64#  (* Default: 100MB *)
   ; max_header_size : int16#     (* Default: 16KB - size of all headers combined *)
   ; max_header_count : int16#    (* Default: 100 *)
   ; max_chunk_size : int         (* Default: 16MB *)
   ; max_target_length : int16#   (* Default: 8KB *)
   }

let default_limits =
  #{ max_content_length = #104857600L  (* 100MB *)
   ; max_header_size = i16 16384       (* 16KB *)
   ; max_header_count = i16 100
   ; max_chunk_size = 16777216         (* 16MB *)
   ; max_target_length = i16 8192      (* 8KB *)
   }

