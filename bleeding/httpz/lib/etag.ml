open Base
module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let ( =. ) = Buf_read.( =. )

type t =
  #{ weak : bool
   ; off : int16#
   ; len : int16#
   }

type status =
  | Valid
  | Invalid

let empty = #{ weak = false; off = i16 0; len = i16 0 }
let max_tags : int16# = i16 16

let[@inline always] valid_tag_char c =
  let c = Char_u.code c in
  c = 0x21 || (c >= 0x23 && c <> 0x7f)
;;

(* Validate the tag delimiters while treating the enclosed bytes as opaque. *)
let parse (local_ buf) (sp : Span.t) : #(status * t) =
  let off = Span.off sp in
  let len = Span.len sp in
  if len < 2
  then #(Invalid, empty)
  else (
    let c0 = peek buf off in
    let c1 = peek buf (off + 1) in
    let weak, quote_start =
      if c0 =. #'W' && c1 =. #'/' && len >= 4 then true, off + 2 else false, off
    in
    let remaining = len - (quote_start - off) in
    if remaining < 2
    then #(Invalid, empty)
    else (
      let first = peek buf quote_start in
      let last = peek buf (quote_start + remaining - 1) in
      if first =. #'"' && last =. #'"'
      then (
        let tag_off = quote_start + 1 in
        let tag_len = remaining - 2 in
        let mutable pos = tag_off in
        let mutable valid = true in
        while valid && pos < tag_off + tag_len do
          if valid_tag_char (peek buf pos) then pos <- pos + 1 else valid <- false
        done;
        if valid
        then #(Valid, #{ weak; off = i16 tag_off; len = i16 tag_len })
        else #(Invalid, empty))
      else #(Invalid, empty)))
;;

let to_string (local_ (buf : bytes)) (etag : t) : string =
  Span.to_string buf (Span.make ~off:etag.#off ~len:etag.#len)
;;

type match_condition =
  | Any
  | Tags
  | Empty

let parse_match_header (local_ buf) (sp : Span.t) (local_ (tags : t array))
  : #(match_condition * int16#)
  =
  (* [tags] is written with [unsafe_set], so the loop below stops at the shorter of
     [max_tags] and what the caller actually provided. *)
  let capacity = Int.min (to_int max_tags) (Array.length tags) in
  let off = Span.off sp in
  let len = Span.len sp in
  let end_pos = off + len in
  let start = Buf_read.skip_ows buf ~pos:(i16 off) ~len:(i16 end_pos) in
  let start = to_int start in
  if start >= end_pos
  then #(Empty, i16 0)
  else if peek buf start =. #'*'
  then (
    let after_star =
      to_int (Buf_read.skip_ows buf ~pos:(i16 (start + 1)) ~len:(i16 end_pos))
    in
    if after_star >= end_pos then #(Any, i16 0) else #(Empty, i16 0))
  else (
    let mutable pos = start in
    let mutable count = 0 in
    let mutable valid = true in
    while valid && pos < end_pos && count < capacity do
      pos <- to_int (Buf_read.skip_ows buf ~pos:(i16 pos) ~len:(i16 end_pos));
      if pos >= end_pos
      then valid <- false
      else (
        let tag_start = pos in
        let mutable tag_end = pos in
        let mutable in_quote = false in
        while tag_end < end_pos && (in_quote || not (peek buf tag_end =. #',')) do
          if peek buf tag_end =. #'"' then in_quote <- not in_quote;
          tag_end <- tag_end + 1
        done;
        let mutable trimmed_end = tag_end in
        while trimmed_end > tag_start && Buf_read.is_space (peek buf (trimmed_end - 1)) do
          trimmed_end <- trimmed_end - 1
        done;
        let tag_span =
          Span.make ~off:(i16 tag_start) ~len:(i16 (trimmed_end - tag_start))
        in
        let #(status, etag) = parse buf tag_span in
        (match status with
         | Valid ->
           Array.unsafe_set tags count etag;
           count <- count + 1
         | Invalid -> valid <- false);
        if tag_end < end_pos && peek buf tag_end =. #','
        then (
          pos
          <- to_int (Buf_read.skip_ows buf ~pos:(i16 (tag_end + 1)) ~len:(i16 end_pos));
          if pos >= end_pos then valid <- false)
        else pos <- tag_end)
    done;
    if valid && count > 0 && pos >= end_pos then #(Tags, i16 count) else #(Empty, i16 0))
;;

let[@inline] compare_at_offsets (local_ (buf : bytes)) ~pos1 ~pos2 ~len =
  let mutable i = 0 in
  let mutable eq = true in
  while eq && i < len do
    if not (peek buf (pos1 + i) =. peek buf (pos2 + i)) then eq <- false else i <- i + 1
  done;
  eq
;;

let weak_match (local_ (buf : bytes)) (a : t) (b : t) : bool =
  let a_len = to_int a.#len in
  let b_len = to_int b.#len in
  if a_len <> b_len
  then false
  else compare_at_offsets buf ~pos1:(to_int a.#off) ~pos2:(to_int b.#off) ~len:a_len
;;

(* A strong comparison is the weak one with both weak flags refused, so the two differ
   only in that test (RFC 9110, Section 8.8.3.2). *)
let strong_match (local_ (buf : bytes)) (a : t) (b : t) : bool =
  (not a.#weak) && (not b.#weak) && weak_match buf a b
;;

(* [strong] selects the comparison rather than a closure so that the walk stays free of
   allocation. *)
let[@inline] matches_any
  (local_ buf)
  (etag : t)
  (tags : t array)
  ~(count : int16#)
  ~strong
  =
  let count = to_int count in
  let mutable i = 0 in
  let mutable found = false in
  while (not found) && i < count do
    let other = Array.unsafe_get tags i in
    let matched =
      if strong then strong_match buf etag other else weak_match buf etag other
    in
    if matched then found <- true else i <- i + 1
  done;
  found
;;

let matches_any_weak (local_ buf) (etag : t) (tags : t array) ~(count : int16#) =
  matches_any buf etag tags ~count ~strong:false
;;

let matches_any_strong (local_ buf) (etag : t) (tags : t array) ~(count : int16#) =
  matches_any buf etag tags ~count ~strong:true
;;

let write_etag dst ~off (etag : t) (local_ (src_buf : bytes)) =
  let off = Buf_write.string dst ~off "ETag: " in
  let off = if etag.#weak then Buf_write.string dst ~off "W/" else off in
  let off = Buf_write.char dst ~off '"' in
  let off =
    Buf_write.blit dst ~off ~src:src_buf ~src_off:etag.#off ~len:(to_int etag.#len)
  in
  let off = Buf_write.char dst ~off '"' in
  Buf_write.crlf dst ~off
;;

let write_etag_string dst ~off ~weak tag =
  if not (Stdlib.String.for_all (fun c -> valid_tag_char (Char_u.of_char c)) tag) then
    invalid_arg
      (Printf.sprintf
         "Etag.write_etag_string: opaque value %S contains a forbidden byte"
         tag);
  let off = Buf_write.string dst ~off "ETag: " in
  let off = if weak then Buf_write.string dst ~off "W/" else off in
  let off = Buf_write.char dst ~off '"' in
  let off = Buf_write.string dst ~off tag in
  let off = Buf_write.char dst ~off '"' in
  Buf_write.crlf dst ~off
;;

let pp (local_ buf) fmt (etag : t) =
  let tag = to_string buf etag in
  if etag.#weak
  then Stdlib.Format.fprintf fmt "W/\"%s\"" tag
  else Stdlib.Format.fprintf fmt "\"%s\"" tag
;;
