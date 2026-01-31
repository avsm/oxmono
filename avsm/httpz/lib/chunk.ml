open Base

module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u

(* int16# conversion and arithmetic helpers *)
let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x

type status =
  | Complete  (** Chunk parsed successfully *)
  | Partial   (** Need more data *)
  | Done      (** Final chunk (zero-length) *)
  | Malformed (** Invalid chunk *)
  | Chunk_too_large  (** Chunk size exceeds limit *)

let status_to_string = function
  | Complete -> "Complete"
  | Partial -> "Partial"
  | Done -> "Done"
  | Malformed -> "Malformed"
  | Chunk_too_large -> "Chunk_too_large"
;;

let pp_status fmt t = Stdlib.Format.fprintf fmt "%s" (status_to_string t)

type t =
  #{ data_off : int16#
   ; data_len : int16#
   ; next_off : int16#
   }

let empty = #{ data_off = i16 0; data_len = i16 0; next_off = i16 0 }

(* Parse hex digit, returns -1 if invalid *)
let[@inline] hex_digit_value (c : char#) =
  match c with
  | #'0' .. #'9' -> Char_u.code c - 48
  | #'a' .. #'f' -> Char_u.code c - 87
  | #'A' .. #'F' -> Char_u.code c - 55
  | _ -> -1
;;

(* Maximum hex digits for chunk size (16 = 64-bit max) *)
let max_hex_digits : int16# = i16 16

(* Default maximum chunk size: 16MB *)
let default_max_chunk_size = 16777216

(* Parse hex chunk size with overflow protection.
   Returns #(size, end_pos, overflow) where:
   - size: parsed chunk size (or 0 if overflow)
   - end_pos: position after hex digits
   - overflow: true if size exceeds max or too many digits *)
let[@inline] parse_hex_size_limited (buf : bytes) ~off ~len ~max_size =
  let module P = Buf_read in
  let mutable pos = off in
  let mutable size = 0 in
  let mutable valid = true in
  let mutable overflow = false in
  let mutable digit_count = 0 in
  while valid && pos < len do
    let digit = hex_digit_value (P.peek buf (i16 pos)) in
    if digit >= 0 then (
      digit_count <- digit_count + 1;
      if digit_count > to_int max_hex_digits then (
        overflow <- true;
        valid <- false
      ) else (
        let new_size = (size * 16) + digit in
        if new_size > max_size then (
          overflow <- true;
          valid <- false
        ) else (
          size <- new_size;
          pos <- pos + 1
        )
      )
    ) else
      valid <- false
  done;
  #(size, pos, overflow)
;;


(* Skip to CRLF after chunk size (handles optional chunk extensions) *)
let[@inline] skip_to_crlf (buf : bytes) ~pos ~len =
  let module P = Buf_read in
  let mutable p = pos in
  while p < len && P.(P.peek buf (i16 p) <>. #'\r') do
    p <- p + 1
  done;
  p
;;

(* Check for CRLF at position *)
let[@inline] is_crlf (buf : bytes) pos =
  let module P = Buf_read in
  P.(P.peek buf (i16 pos) =. #'\r') && P.(P.peek buf (i16 (pos + 1)) =. #'\n')
;;

(* Handle final (zero-size) chunk *)
let[@inline] parse_final_chunk (buf : bytes) ~data_off ~len =
  if data_off + 1 >= len then #(Partial, empty)
  else if is_crlf buf data_off then #(Done, #{ data_off = i16 data_off; data_len = i16 0; next_off = i16 (data_off + 2) })
  else #(Done, #{ data_off = i16 data_off; data_len = i16 0; next_off = i16 data_off })
;;

(* Handle data chunk with given size *)
let[@inline] parse_data_chunk (buf : bytes) ~data_off ~size ~len =
  let module P = Buf_read in
  let data_end = data_off + size in
  if data_end + 1 >= len then #(Partial, empty)
  else if P.(P.peek buf (i16 data_end) <>. #'\r') || P.(P.peek buf (i16 (data_end + 1)) <>. #'\n')
  then #(Malformed, empty)
  else #(Complete, #{ data_off = i16 data_off; data_len = i16 size; next_off = i16 (data_end + 2) })
;;

(* Parse chunk with configurable size limit - returns Chunk_too_large on overflow *)
let parse_with_limit (buf : bytes) ~(off : int16#) ~(len : int16#) ~max_chunk_size =
  let module P = Buf_read in
  let off = to_int off in
  let len = to_int len in
  if off >= len then #(Partial, empty)
  else
    let #(size, hex_end, overflow) = parse_hex_size_limited buf ~off ~len ~max_size:max_chunk_size in
    if overflow then #(Chunk_too_large, empty)
    else if hex_end = off then #(Malformed, empty)
    else
      let crlf_pos = skip_to_crlf buf ~pos:hex_end ~len in
      if crlf_pos + 1 >= len then #(Partial, empty)
      else if P.(P.peek buf (i16 (crlf_pos + 1)) <>. #'\n') then #(Malformed, empty)
      else
        let data_off = crlf_pos + 2 in
        if size = 0
        then parse_final_chunk buf ~data_off ~len
        else parse_data_chunk buf ~data_off ~size ~len
;;

(* Parse chunk without size limit - for backwards compatibility *)
let parse (buf : bytes) ~(off : int16#) ~(len : int16#) =
  parse_with_limit buf ~off ~len ~max_chunk_size:Int.max_value
;;

let pp fmt (chunk : t) =
  Stdlib.Format.fprintf fmt "{ data_off = %d; data_len = %d; next_off = %d }"
    (to_int chunk.#data_off)
    (to_int chunk.#data_len)
    (to_int chunk.#next_off)
;;

(* Trailer header support - RFC 7230 Section 4.1.2 *)

type trailer_status =
  | Trailer_complete
  | Trailer_partial
  | Trailer_malformed

let trailer_status_to_string = function
  | Trailer_complete -> "Trailer_complete"
  | Trailer_partial -> "Trailer_partial"
  | Trailer_malformed -> "Trailer_malformed"
;;

let pp_trailer_status fmt t = Stdlib.Format.fprintf fmt "%s" (trailer_status_to_string t)

(* RFC 7230 Section 4.1.2 - Headers forbidden in trailers.
   A sender MUST NOT generate a trailer that contains a field necessary for
   message framing, routing, authentication, integrity, or content negotiation. *)
let is_forbidden_trailer = function
  (* Message framing headers *)
  | Header_name.Transfer_encoding -> true
  | Header_name.Content_length -> true
  (* Routing headers *)
  | Header_name.Host -> true
  (* Control headers *)
  | Header_name.Cache_control -> true
  | Header_name.Expect -> true
  | Header_name.Range -> true
  (* Content-* headers that affect message interpretation *)
  | Header_name.Content_encoding -> true
  | Header_name.Content_type -> true
  | Header_name.Content_range -> true
  (* Authentication headers *)
  | Header_name.Www_authenticate -> true
  | Header_name.Authorization -> true
  | _ -> false
;;

(* Parse a single trailer header, similar to httpz.ml:parse_header *)
let[@inline] parse_trailer_header (buf : bytes) ~pos ~len =
  let module P = Buf_read in
  let mutable colon_pos = pos in
  while colon_pos < len && P.is_token_char (P.peek buf (i16 colon_pos)) do
    colon_pos <- colon_pos + 1
  done;
  let name_len = colon_pos - pos in
  if name_len = 0 || colon_pos >= len || P.(P.peek buf (i16 colon_pos) <>. #':')
  then #(Trailer_malformed, Header_name.Host, i16 0, i16 0, i16 0, i16 0, i16 0)
  else (
    let name_span = Span.make ~off:(i16 pos) ~len:(i16 name_len) in
    let name = Header_name.of_span buf name_span in
    let mutable p = colon_pos + 1 in
    while p < len && P.is_space (P.peek buf (i16 p)) do
      p <- p + 1
    done;
    let value_start = p in
    let crlf_pos = P.find_crlf buf ~pos:(i16 p) ~len:(i16 len) in
    let crlf_pos_int = to_int crlf_pos in
    if crlf_pos_int < 0
    then #(Trailer_partial, Header_name.Host, i16 0, i16 0, i16 0, i16 0, i16 0)
    else (
      let mutable value_end = crlf_pos_int in
      while value_end > value_start && P.is_space (P.peek buf (i16 (value_end - 1))) do
        value_end <- value_end - 1
      done;
      #(Trailer_complete, name, i16 pos, i16 name_len, i16 value_start, i16 (value_end - value_start), i16 (crlf_pos_int + 2))))
;;

(* Parse trailer headers after final chunk *)
let rec parse_trailers_loop (buf : bytes) ~pos ~len ~count ~acc ~max_header_count = exclave_
  let module P = Buf_read in
  if pos + 1 >= len then
    #(Trailer_partial, i16 pos, acc)
  else if P.(P.peek buf (i16 pos) =. #'\r') && P.(P.peek buf (i16 (pos + 1)) =. #'\n') then
    (* Empty line marks end of trailers *)
    #(Trailer_complete, i16 (pos + 2), acc)
  else if count >= max_header_count then
    #(Trailer_malformed, i16 pos, acc)
  else
    let #(s, name, noff, nlen, voff, vlen, new_pos) = parse_trailer_header buf ~pos ~len in
    match s with
    | Trailer_partial -> #(Trailer_partial, i16 pos, acc)
    | Trailer_malformed -> #(Trailer_malformed, i16 pos, acc)
    | Trailer_complete ->
      (* Skip forbidden trailer headers per RFC 7230 Section 4.1.2 *)
      if is_forbidden_trailer name then
        parse_trailers_loop buf ~pos:(to_int new_pos) ~len ~count:(count + 1) ~acc ~max_header_count
      else
        let value_span = Span.make ~off:voff ~len:vlen in
        let hdr =
          { Header.name
          ; name_span = Span.make ~off:noff ~len:nlen
          ; value = value_span
          }
        in
        parse_trailers_loop buf ~pos:(to_int new_pos) ~len ~count:(count + 1) ~acc:(hdr :: acc) ~max_header_count
;;

let parse_trailers (buf : bytes) ~(off : int16#) ~(len : int16#) ~(max_header_count : int16#) = exclave_
  parse_trailers_loop buf ~pos:(to_int off) ~len:(to_int len) ~count:0 ~acc:[] ~max_header_count:(to_int max_header_count)
;;
