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
let[@inline] hex_digit_value_match (c : char#) =
  match c with
  | #'0' .. #'9' -> Char_u.code c - 48
  | #'a' .. #'f' -> Char_u.code c - 87
  | #'A' .. #'F' -> Char_u.code c - 55
  | _ -> -1
;;

(* One byte per code point, holding the hex value biased by one so that 0
   means "not a hex digit". The match above compiles to a decision tree whose
   branches depend on the input byte and so mispredict, exactly as for
   [Scan_portable.tchar_table]. Derived from [hex_digit_value_match] so the
   two cannot drift apart. *)
let hex_table =
  String.init 256 ~f:(fun i ->
    Stdlib.Char.unsafe_chr
      (hex_digit_value_match (Char_u.of_char (Char.of_int_exn i)) + 1))
;;

let[@inline] hex_digit_value (c : char#) =
  Char.to_int (String.unsafe_get hex_table (Char_u.code c)) - 1
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
  while valid && pos < len do
    let digit = hex_digit_value (P.peek buf (i16 pos)) in
    if digit >= 0 then (
      if max_size < 0 || digit > max_size
         || size > (max_size - digit) / 16
      then (
        overflow <- true;
        valid <- false
      ) else (
        let new_size = (size * 16) + digit in
        size <- new_size;
        pos <- pos + 1
      )
    ) else
      valid <- false
  done;
  #(size, pos, overflow)
;;


(* Validate the optional chunk extensions and return the first data byte.
   This rejects signed sizes, OCaml-style integer literals, bare newlines and
   arbitrary bytes that an intermediary might frame differently. *)
let[@inline] parse_size_line_end (buf : bytes) ~pos ~len =
  let module P = Buf_read in
  let mutable p = pos in
  let mutable malformed = false in
  let mutable complete = false in
  let mutable data_off = 0 in
  while not malformed && not complete && p < len do
    let ows_start = p in
    while p < len && P.is_space (P.peek buf (i16 p)) do p <- p + 1 done;
    let had_ows = p > ows_start in
    if p < len then
      if P.(P.peek buf (i16 p) =. #'\r') then
        if had_ows then malformed <- true
        else if p + 1 < len then
          if P.(P.peek buf (i16 (p + 1)) =. #'\n') then (
            data_off <- p + 2;
            complete <- true)
          else malformed <- true
        else p <- len
      else if P.(P.peek buf (i16 p) <>. #';') then malformed <- true
      else (
        p <- p + 1;
        while p < len && P.is_space (P.peek buf (i16 p)) do p <- p + 1 done;
        let name_start = p in
        while p < len && P.is_token_char (P.peek buf (i16 p)) do
          p <- p + 1
        done;
        if p = name_start then malformed <- true
        else (
          let mutable equals = p in
          while equals < len && P.is_space (P.peek buf (i16 equals)) do
            equals <- equals + 1
          done;
          if equals < len && P.(P.peek buf (i16 equals) =. #'=') then (
            p <- equals + 1;
            while p < len && P.is_space (P.peek buf (i16 p)) do
              p <- p + 1
            done;
            if p < len then
              if P.(P.peek buf (i16 p) =. #'"') then (
                p <- p + 1;
                let mutable closed = false in
                while not malformed && not closed && p < len do
                  let c = P.peek buf (i16 p) in
                  let code = Char_u.code c in
                  if P.(c =. #'"') then (
                    closed <- true;
                    p <- p + 1)
                  else if P.(c =. #'\\') then
                    if p + 1 >= len then p <- len
                    else (
                      let escaped = Char_u.code (P.peek buf (i16 (p + 1))) in
                      if escaped = 0x09 || escaped = 0x20
                         || (escaped >= 0x21 && escaped <= 0x7e)
                         || escaped >= 0x80
                      then p <- p + 2
                      else malformed <- true)
                  else if code = 0x09 || code = 0x20 || code = 0x21
                          || (code >= 0x23 && code <= 0x5b)
                          || (code >= 0x5d && code <= 0x7e) || code >= 0x80
                  then p <- p + 1
                  else malformed <- true
                done;
                if p < len && not closed then malformed <- true)
              else (
                let value_start = p in
                while p < len && P.is_token_char (P.peek buf (i16 p)) do
                  p <- p + 1
                done;
                if p = value_start then malformed <- true))))
  done;
  if malformed then #(Malformed, i16 0)
  else if complete then #(Complete, i16 data_off)
  else #(Partial, i16 0)
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
  let off = to_int off in
  let len = to_int len in
  if off >= len then #(Partial, empty)
  else
    let #(size, hex_end, overflow) = parse_hex_size_limited buf ~off ~len ~max_size:max_chunk_size in
    if overflow then #(Chunk_too_large, empty)
    else if hex_end = off then #(Malformed, empty)
    else
      let #(line_status, data_off) =
        parse_size_line_end buf ~pos:hex_end ~len
      in
      match line_status with
      | Partial -> #(Partial, empty)
      | Malformed | Chunk_too_large | Done -> #(Malformed, empty)
      | Complete ->
        let data_off = to_int data_off in
        if size = 0
        then parse_final_chunk buf ~data_off ~len
        else parse_data_chunk buf ~data_off ~size ~len
;;

(* Parse chunk without size limit - for backwards compatibility *)
let parse (buf : bytes) ~(off : int16#) ~(len : int16#) =
  parse_with_limit buf ~off ~len ~max_chunk_size:Int.max_value
;;

(* The size line alone, for a caller streaming chunk data that need not
   fit the parse buffer. [parse] can only answer [Complete] once the
   whole chunk is in the buffer, which caps a chunk at the buffer size;
   a client receiving a response has no say in how large a server's
   chunks are. *)
let parse_header (buf : bytes) ~(off : int16#) ~(len : int16#) ~max_chunk_size =
  let off = to_int off in
  let len = to_int len in
  if off >= len then #(Partial, 0, i16 0)
  else (
    let #(size, hex_end, overflow) =
      parse_hex_size_limited buf ~off ~len ~max_size:max_chunk_size
    in
    if overflow then #(Chunk_too_large, 0, i16 0)
    else if hex_end = off then #(Malformed, 0, i16 0)
    else (
      let #(line_status, data_off) =
        parse_size_line_end buf ~pos:hex_end ~len
      in
      match line_status with
      | Partial -> #(Partial, 0, i16 0)
      | Malformed | Chunk_too_large | Done -> #(Malformed, 0, i16 0)
      | Complete ->
        if size = 0 then #(Done, 0, data_off)
        else #(Complete, size, data_off)))
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
  | Trailer_bare_cr  (* RFC 7230 Section 3.5 - bare CR detected *)

let trailer_status_to_string = function
  | Trailer_complete -> "Trailer_complete"
  | Trailer_partial -> "Trailer_partial"
  | Trailer_malformed -> "Trailer_malformed"
  | Trailer_bare_cr -> "Trailer_bare_cr"
;;

let pp_trailer_status fmt t = Stdlib.Format.fprintf fmt "%s" (trailer_status_to_string t)

(* RFC 7230 Section 4.1.2 - Headers forbidden in trailers.
   A sender MUST NOT generate a trailer that contains a field necessary for
   message framing, routing, authentication, integrity, or content negotiation. *)
let is_forbidden_trailer = function
  (* Message framing headers *)
  | Header_name.Transfer_encoding -> true
  | Header_name.Content_length -> true
  | Header_name.Connection -> true
  | Header_name.Upgrade -> true
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
  | Header_name.Cookie -> true
  | Header_name.Set_cookie -> true
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
    let #(crlf_pos, has_bare_cr) = P.find_crlf_check_bare_cr buf ~pos:(i16 p) ~len:(i16 len) in
    let crlf_pos_int = to_int crlf_pos in
    if crlf_pos_int < 0
    then #(Trailer_partial, Header_name.Host, i16 0, i16 0, i16 0, i16 0, i16 0)
    else if has_bare_cr
    then #(Trailer_bare_cr, Header_name.Host, i16 0, i16 0, i16 0, i16 0, i16 0)
    else if not (P.valid_field_value buf ~pos:(i16 value_start) ~len:crlf_pos)
    then #(Trailer_malformed, Header_name.Host, i16 0, i16 0, i16 0, i16 0, i16 0)
    else (
      let mutable value_end = crlf_pos_int in
      while value_end > value_start && P.is_space (P.peek buf (i16 (value_end - 1))) do
        value_end <- value_end - 1
      done;
      #(Trailer_complete, name, i16 pos, i16 name_len, i16 value_start, i16 (value_end - value_start), i16 (crlf_pos_int + 2))))
;;

(* Parse trailer headers after final chunk *)
let rec parse_trailers_loop (buf : bytes) ~start ~pos ~len ~count ~acc
    ~max_header_count ~max_trailer_size = exclave_
  let module P = Buf_read in
  if pos + 1 >= len then
    if len - start >= max_trailer_size then
      #(Trailer_malformed, i16 pos, acc)
    else #(Trailer_partial, i16 pos, acc)
  else if P.(P.peek buf (i16 pos) =. #'\r') && P.(P.peek buf (i16 (pos + 1)) =. #'\n') then
    (* Empty line marks end of trailers *)
    if pos + 2 - start > max_trailer_size then
      #(Trailer_malformed, i16 pos, acc)
    else #(Trailer_complete, i16 (pos + 2), acc)
  else if count >= max_header_count || pos - start >= max_trailer_size then
    #(Trailer_malformed, i16 pos, acc)
  else
    let #(s, name, noff, nlen, voff, vlen, new_pos) = parse_trailer_header buf ~pos ~len in
    match s with
    | Trailer_partial ->
      if len - start >= max_trailer_size then
        #(Trailer_malformed, i16 pos, acc)
      else #(Trailer_partial, i16 pos, acc)
    | Trailer_malformed -> #(Trailer_malformed, i16 pos, acc)
    | Trailer_bare_cr -> #(Trailer_bare_cr, i16 pos, acc)
    | Trailer_complete ->
      (* Skip forbidden trailer headers per RFC 7230 Section 4.1.2 *)
      if to_int new_pos - start > max_trailer_size then
        #(Trailer_malformed, i16 pos, acc)
      else if is_forbidden_trailer name then
        parse_trailers_loop buf ~start ~pos:(to_int new_pos) ~len
          ~count:(count + 1) ~acc ~max_header_count ~max_trailer_size
      else
        let value_span = Span.make ~off:voff ~len:vlen in
        let hdr =
          { Header.name
          ; name_span = Span.make ~off:noff ~len:nlen
          ; value = value_span
          }
        in
        parse_trailers_loop buf ~start ~pos:(to_int new_pos) ~len
          ~count:(count + 1) ~acc:(hdr :: acc) ~max_header_count
          ~max_trailer_size
;;

let parse_trailers ?(max_trailer_size = 16384) (buf : bytes)
    ~(off : int16#) ~(len : int16#) ~(max_header_count : int16#) = exclave_
  let start = to_int off in
  parse_trailers_loop buf ~start ~pos:start ~len:(to_int len) ~count:0 ~acc:[]
    ~max_header_count:(to_int max_header_count) ~max_trailer_size
;;
