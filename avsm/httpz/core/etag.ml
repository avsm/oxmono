(* etag.ml - ETag parsing and comparison per RFC 7232 *)

open Base

module I16 = Stdlib_stable.Int16_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x

(* Entity tag - unboxed record pointing into buffer *)
type t =
  #{ weak : bool
   ; off : int16#
   ; len : int16#
   }

type status =
  | Valid
  | Invalid

let empty = #{ weak = false; off = i16 0; len = i16 0 }

(* Maximum number of ETags in If-Match/If-None-Match header *)
let max_tags : int16# = i16 16

(* Parse a single ETag value.
   Format: entity-tag = [ weak ] opaque-tag
           weak       = %x57.2F ; "W/", case-sensitive
           opaque-tag = DQUOTE *etagc DQUOTE
           etagc      = %x21 / %x23-7E / obs-text *)
let parse (local_ buf) (sp : Span.t) : #(status * t) =
  let off = Span.off sp in
  let len = Span.len sp in
  if len < 2 then #(Invalid, empty)  (* Minimum: "" *)
  else
    let c0 = Bytes.unsafe_get buf off in
    let c1 = Bytes.unsafe_get buf (off + 1) in
    (* Check for weak indicator W/ *)
    let weak, quote_start =
      if Char.equal c0 'W' && Char.equal c1 '/' && len >= 4 then
        (true, off + 2)
      else
        (false, off)
    in
    let remaining = len - (quote_start - off) in
    if remaining < 2 then #(Invalid, empty)
    else
      let first = Bytes.unsafe_get buf quote_start in
      let last = Bytes.unsafe_get buf (quote_start + remaining - 1) in
      if Char.equal first '"' && Char.equal last '"' then
        let tag_off = quote_start + 1 in
        let tag_len = remaining - 2 in
        #(Valid, #{ weak; off = i16 tag_off; len = i16 tag_len })
      else
        #(Invalid, empty)
;;

let to_string (local_ buf : bytes) (etag : t) : string =
  let sp_off = to_int etag.#off in
  let sp_len = to_int etag.#len in
  let dst = Bytes.create sp_len in
  for i = 0 to sp_len - 1 do
    Bytes.unsafe_set dst i (Bytes.unsafe_get buf (sp_off + i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

(* If-Match / If-None-Match parsing *)
type match_condition =
  | Any
  | Tags
  | Empty

(* Skip optional whitespace *)
let[@inline] skip_ows buf ~pos ~len =
  let mutable p = pos in
  while p < len && (
    let c = Bytes.unsafe_get buf p in
    Char.equal c ' ' || Char.equal c '\t'
  ) do
    p <- p + 1
  done;
  p
;;

(* Parse comma-separated list of entity tags into array *)
let parse_match_header (local_ buf) (sp : Span.t) (tags : t array) : #(match_condition * int16#) =
  let off = Span.off sp in
  let len = Span.len sp in
  let end_pos = off + len in
  (* Skip leading whitespace *)
  let start = skip_ows buf ~pos:off ~len:end_pos in
  if start >= end_pos then #(Empty, i16 0)
  else if Char.equal (Bytes.unsafe_get buf start) '*' then
    (* Check it's just "*" possibly with trailing whitespace *)
    let after_star = skip_ows buf ~pos:(start + 1) ~len:end_pos in
    if after_star >= end_pos then #(Any, i16 0) else #(Empty, i16 0)
  else
    (* Parse comma-separated list of entity tags *)
    let mutable pos = start in
    let mutable count = 0 in
    let mutable valid = true in
    while valid && pos < end_pos && count < to_int max_tags do
      pos <- skip_ows buf ~pos ~len:end_pos;
      if pos >= end_pos then
        valid <- false
      else
        (* Find end of this tag (comma or end) *)
        let tag_start = pos in
        let mutable tag_end = pos in
        let mutable in_quote = false in
        while tag_end < end_pos && (in_quote || not (Char.equal (Bytes.unsafe_get buf tag_end) ',')) do
          if Char.equal (Bytes.unsafe_get buf tag_end) '"' then
            in_quote <- not in_quote;
          tag_end <- tag_end + 1
        done;
        (* Trim trailing whitespace from tag *)
        let mutable trimmed_end = tag_end in
        while trimmed_end > tag_start && (
          let c = Bytes.unsafe_get buf (trimmed_end - 1) in
          Char.equal c ' ' || Char.equal c '\t'
        ) do
          trimmed_end <- trimmed_end - 1
        done;
        let tag_span = Span.make ~off:(i16 tag_start) ~len:(i16 (trimmed_end - tag_start)) in
        let #(status, etag) = parse buf tag_span in
        (match status with
        | Valid ->
          Array.unsafe_set tags count etag;
          count <- count + 1
        | Invalid -> ());
        (* Skip comma if present *)
        if tag_end < end_pos && Char.equal (Bytes.unsafe_get buf tag_end) ',' then
          pos <- tag_end + 1
        else
          pos <- tag_end
    done;
    if count > 0 then #(Tags, i16 count) else #(Empty, i16 0)
;;

(* Manual string comparison at two offsets *)
let[@inline] compare_at_offsets (local_ buf : bytes) ~pos1 ~pos2 ~len =
  let mutable i = 0 in
  let mutable eq = true in
  while eq && i < len do
    if not (Char.equal (Bytes.unsafe_get buf (pos1 + i)) (Bytes.unsafe_get buf (pos2 + i)))
    then eq <- false
    else i <- i + 1
  done;
  eq
;;

(* Strong comparison: both must be strong, tags must match exactly *)
let strong_match (local_ buf : bytes) (a : t) (b : t) : bool =
  if a.#weak || b.#weak then false
  else
    let a_len = to_int a.#len in
    let b_len = to_int b.#len in
    if a_len <> b_len then false
    else compare_at_offsets buf ~pos1:(to_int a.#off) ~pos2:(to_int b.#off) ~len:a_len
;;

(* Weak comparison: only tags must match, ignore weak indicator *)
let weak_match (local_ buf : bytes) (a : t) (b : t) : bool =
  let a_len = to_int a.#len in
  let b_len = to_int b.#len in
  if a_len <> b_len then false
  else compare_at_offsets buf ~pos1:(to_int a.#off) ~pos2:(to_int b.#off) ~len:a_len
;;

let matches_any_weak (local_ buf) (etag : t) (tags : t array) ~(count : int16#) : bool =
  let count = to_int count in
  let mutable i = 0 in
  let mutable found = false in
  while (not found) && i < count do
    if weak_match buf etag (Array.unsafe_get tags i) then
      found <- true
    else
      i <- i + 1
  done;
  found
;;

let matches_any_strong (local_ buf) (etag : t) (tags : t array) ~(count : int16#) : bool =
  let count = to_int count in
  let mutable i = 0 in
  let mutable found = false in
  while (not found) && i < count do
    if strong_match buf etag (Array.unsafe_get tags i) then
      found <- true
    else
      i <- i + 1
  done;
  found
;;

(* Response writing *)

let write_etag dst ~off (etag : t) (local_ src_buf : bytes) =
  (* ETag: [W/]"tag"\r\n *)
  let off = Buf_write.string dst ~off "ETag: " in
  let off = if etag.#weak then Buf_write.string dst ~off "W/" else off in
  let off = Buf_write.char dst ~off '"' in
  (* Copy tag value from source buffer *)
  let tag_off = to_int etag.#off in
  let tag_len = to_int etag.#len in
  let off_int = Buf_write.to_int off in
  for i = 0 to tag_len - 1 do
    Bytes.unsafe_set dst (off_int + i) (Bytes.unsafe_get src_buf (tag_off + i))
  done;
  let off = Buf_write.i16 (off_int + tag_len) in
  let off = Buf_write.char dst ~off '"' in
  Buf_write.crlf dst ~off
;;

let write_etag_string dst ~off ~weak tag =
  let off = Buf_write.string dst ~off "ETag: " in
  let off = if weak then Buf_write.string dst ~off "W/" else off in
  let off = Buf_write.char dst ~off '"' in
  let off = Buf_write.string dst ~off tag in
  let off = Buf_write.char dst ~off '"' in
  Buf_write.crlf dst ~off
;;

let pp (local_ buf) fmt (etag : t) =
  let tag = to_string buf etag in
  if etag.#weak then
    Stdlib.Format.fprintf fmt "W/\"%s\"" tag
  else
    Stdlib.Format.fprintf fmt "\"%s\"" tag
;;
