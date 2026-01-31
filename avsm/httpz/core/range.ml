(* range.ml - Range request parsing and Content-Range response writing per RFC 7233 *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_i16 x = I16.to_int x
let[@inline always] i64 x = I64.of_int64 x
let[@inline always] to_i64 x = I64.to_int64 x

(* Byte range specification - unboxed *)
type byte_range =
  #{ kind : int      (* 0=Range, 1=Suffix, 2=Open *)
   ; start : int64#
   ; end_ : int64#
   }

(* Kind constants - internal *)
let kind_range = 0
let kind_suffix = 1
let kind_open = 2

(* Maximum ranges to parse *)
let max_ranges : int16# = i16 16

let empty = #{ kind = 0; start = i64 0L; end_ = i64 0L }

(* Query functions *)
let[@inline always] is_range (r : byte_range) = r.#kind = kind_range
let[@inline always] is_suffix (r : byte_range) = r.#kind = kind_suffix
let[@inline always] is_open (r : byte_range) = r.#kind = kind_open

type parse_status =
  | Valid
  | Invalid

(* Resolved byte range - unboxed *)
type resolved =
  #{ start : int64#
   ; end_ : int64#
   ; length : int64#
   }

let empty_resolved = #{ start = i64 0L; end_ = i64 0L; length = i64 0L }

type eval_result =
  | Full_content
  | Single_range
  | Multiple_ranges
  | Not_satisfiable

(* Skip whitespace *)
let[@inline] skip_ws buf ~pos ~len =
  let mutable p = pos in
  while p < len && (
    let c = Bytes.unsafe_get buf p in
    Char.equal c ' ' || Char.equal c '\t'
  ) do
    p <- p + 1
  done;
  p
;;

(* Parse a non-negative int64, returns (value, end_pos, valid) *)
let[@inline] parse_int64 buf ~pos ~len =
  let start = pos in
  let mutable p = pos in
  let mutable acc = 0L in
  let mutable valid = true in
  while valid && p < len do
    let c = Bytes.unsafe_get buf p in
    if Char.is_digit c then (
      let digit = Int64.of_int (Char.to_int c - 48) in
      acc <- Int64.(acc * 10L + digit);
      p <- p + 1
    ) else
      valid <- false
  done;
  if p > start then #(acc, p, true) else #(0L, pos, false)
;;

(* Parse a single range-spec *)
let[@inline] parse_range_spec buf ~pos ~len =
  let pos = skip_ws buf ~pos ~len in
  if pos >= len then #(false, empty, pos)
  else
    let c = Bytes.unsafe_get buf pos in
    if Char.equal c '-' then
      (* Suffix range: -500 *)
      let #(suffix, end_pos, valid) = parse_int64 buf ~pos:(pos + 1) ~len in
      if (not valid) || Int64.(suffix = 0L) then
        #(false, empty, end_pos)
      else
        #(true, #{ kind = kind_suffix; start = i64 suffix; end_ = i64 0L }, end_pos)
    else
      (* Start-end or start- *)
      let #(start, after_start, valid) = parse_int64 buf ~pos ~len in
      if not valid then #(false, empty, after_start)
      else if after_start >= len then #(false, empty, after_start)
      else if not (Char.equal (Bytes.unsafe_get buf after_start) '-') then
        #(false, empty, after_start)
      else
        let after_dash = after_start + 1 in
        if after_dash >= len || (
          let c = Bytes.unsafe_get buf after_dash in
          Char.equal c ',' || Char.equal c ' ' || Char.equal c '\t'
        ) then
          (* Open range: start- *)
          #(true, #{ kind = kind_open; start = i64 start; end_ = i64 0L }, after_dash)
        else
          (* Closed range: start-end *)
          let #(end_val, end_pos, end_valid) = parse_int64 buf ~pos:after_dash ~len in
          if (not end_valid) || Int64.(end_val < start) then
            #(false, empty, end_pos)
          else
            #(true, #{ kind = kind_range; start = i64 start; end_ = i64 end_val }, end_pos)
;;

(* Parse Range header into array - internal implementation working on buffer region *)
let parse_region (local_ buf) ~off ~len (ranges : byte_range array) : #(parse_status * int16#) =
  let end_pos = off + len in
  (* Look for "=" to split unit and ranges *)
  let mutable eq_pos = off in
  while eq_pos < end_pos && not (Char.equal (Bytes.unsafe_get buf eq_pos) '=') do
    eq_pos <- eq_pos + 1
  done;
  if eq_pos >= end_pos then #(Invalid, i16 0)
  else
    (* Check for "bytes" unit *)
    let unit_len = eq_pos - off in
    if unit_len <> 5 then #(Invalid, i16 0)
    else
      let is_bytes =
        let c0 = Bytes.unsafe_get buf off in
        let c1 = Bytes.unsafe_get buf (off + 1) in
        let c2 = Bytes.unsafe_get buf (off + 2) in
        let c3 = Bytes.unsafe_get buf (off + 3) in
        let c4 = Bytes.unsafe_get buf (off + 4) in
        (Char.equal c0 'b' || Char.equal c0 'B') &&
        (Char.equal c1 'y' || Char.equal c1 'Y') &&
        (Char.equal c2 't' || Char.equal c2 'T') &&
        (Char.equal c3 'e' || Char.equal c3 'E') &&
        (Char.equal c4 's' || Char.equal c4 'S')
      in
      if not is_bytes then #(Invalid, i16 0)
      else
        (* Parse comma-separated range specs *)
        let mutable pos = eq_pos + 1 in
        let mutable count = 0 in
        let mutable valid = true in
        while valid && pos < end_pos && count < to_i16 max_ranges do
          pos <- skip_ws buf ~pos ~len:end_pos;
          if pos >= end_pos then
            valid <- false
          else
            let #(ok, range, after_range) = parse_range_spec buf ~pos ~len:end_pos in
            if ok then (
              Array.unsafe_set ranges count range;
              count <- count + 1
            );
            pos <- skip_ws buf ~pos:after_range ~len:end_pos;
            if pos < end_pos then
              if Char.equal (Bytes.unsafe_get buf pos) ',' then
                pos <- pos + 1
              else
                valid <- false
        done;
        if count > 0 then #(Valid, i16 count) else #(Invalid, i16 0)
;;

(* Parse Range header into array - from buffer and span *)
let parse (local_ buf) (sp : Span.t) (ranges : byte_range array) : #(parse_status * int16#) =
  parse_region buf ~off:(Span.off sp) ~len:(Span.len sp) ranges
;;

(* Parse Range header from string - creates local buffer *)
let parse_string (s : string) (ranges : byte_range array) : #(parse_status * int16#) =
  (* Convert string to bytes for parsing *)
  let buf = Bytes.of_string s in
  let #(status, count) = parse_region buf ~off:0 ~len:(String.length s) ranges in
  #(status, count)
;;

(* Resolve a single range *)
let resolve_range (range : byte_range) ~(resource_length : int64#) : #(bool * resolved) =
  let res_len = to_i64 resource_length in
  if Int64.(res_len <= 0L) then #(false, empty_resolved)
  else
    let kind = range.#kind in
    let start_val = to_i64 range.#start in
    let end_val = to_i64 range.#end_ in
    if kind = kind_range then
      (* Range: start-end *)
      if Int64.(start_val >= res_len) then #(false, empty_resolved)
      else
        let end_clamped = Int64.min end_val Int64.(res_len - 1L) in
        let length = Int64.(end_clamped - start_val + 1L) in
        #(true, #{ start = i64 start_val; end_ = i64 end_clamped; length = i64 length })
    else if kind = kind_suffix then
      (* Suffix: -N (last N bytes) *)
      let suffix = start_val in  (* stored in start field *)
      if Int64.(suffix <= 0L) then #(false, empty_resolved)
      else
        let start = Int64.max 0L Int64.(res_len - suffix) in
        let end_ = Int64.(res_len - 1L) in
        let length = Int64.(end_ - start + 1L) in
        #(true, #{ start = i64 start; end_ = i64 end_; length = i64 length })
    else
      (* Open: start- *)
      if Int64.(start_val >= res_len) then #(false, empty_resolved)
      else
        let end_ = Int64.(res_len - 1L) in
        let length = Int64.(end_ - start_val + 1L) in
        #(true, #{ start = i64 start_val; end_ = i64 end_; length = i64 length })
;;

(* Evaluate ranges *)
let evaluate (ranges : byte_range array) ~(count : int16#) ~(resource_length : int64#) (out : resolved array)
    : #(eval_result * int16#) =
  let count = to_i16 count in
  if count = 0 then #(Full_content, i16 0)
  else
    let mutable resolved_count = 0 in
    for i = 0 to count - 1 do
      let #(valid, r) = resolve_range (Array.unsafe_get ranges i) ~resource_length in
      if valid then (
        Array.unsafe_set out resolved_count r;
        resolved_count <- resolved_count + 1
      )
    done;
    if resolved_count = 0 then #(Not_satisfiable, i16 0)
    else if resolved_count = 1 then #(Single_range, i16 1)
    else #(Multiple_ranges, i16 resolved_count)
;;

(* Response writing helpers *)

let write_accept_ranges dst ~off =
  let off = Buf_write.string dst ~off "Accept-Ranges: bytes" in
  Buf_write.crlf dst ~off
;;

let write_accept_ranges_none dst ~off =
  let off = Buf_write.string dst ~off "Accept-Ranges: none" in
  Buf_write.crlf dst ~off
;;

(* Content-Range: bytes start-end/total *)
let write_content_range dst ~off ~(start : int64#) ~(end_ : int64#) ~(total : int64#) =
  let off = Buf_write.string dst ~off "Content-Range: bytes " in
  let off = Buf_write.int64 dst ~off start in
  let off = Buf_write.char dst ~off '-' in
  let off = Buf_write.int64 dst ~off end_ in
  let off = Buf_write.char dst ~off '/' in
  let off = Buf_write.int64 dst ~off total in
  Buf_write.crlf dst ~off
;;

let write_content_range_resolved dst ~off (r : resolved) ~(total : int64#) =
  write_content_range dst ~off ~start:r.#start ~end_:r.#end_ ~total
;;

(* Content-Range: bytes * /total (for 416 responses) *)
let write_content_range_unsatisfiable dst ~off ~(total : int64#) =
  let off = Buf_write.string dst ~off "Content-Range: bytes */" in
  let off = Buf_write.int64 dst ~off total in
  Buf_write.crlf dst ~off
;;

(* Multipart helpers *)

let write_multipart_boundary dst ~off ~boundary =
  let off = Buf_write.string dst ~off "--" in
  let off = Buf_write.string dst ~off boundary in
  Buf_write.crlf dst ~off
;;

let write_multipart_final dst ~off ~boundary =
  let off = Buf_write.string dst ~off "--" in
  let off = Buf_write.string dst ~off boundary in
  let off = Buf_write.string dst ~off "--" in
  Buf_write.crlf dst ~off
;;

(* Generate a random boundary string *)
let generate_boundary () =
  let chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let len = 24 in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let idx = Random.int (String.length chars) in
    Bytes.set buf i (String.get chars idx)
  done;
  Bytes.to_string buf
;;
