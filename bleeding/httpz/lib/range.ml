open Base

module Content = struct
  module Char_u = Stdlib_stable.Char_u

  type kind =
    | Invalid
    | Satisfied
    | Unsatisfied

  let[@zero_alloc] valid_bounds ~range ~complete_length =
    (match complete_length with
     | None -> true
     | Some n -> Int64.compare n 0L >= 0)
    &&
    match range, complete_length with
    | None, None -> false
    | None, Some _ -> true
    | Some (first, last), total ->
      Int64.compare first 0L >= 0
      && Int64.compare last first >= 0
      && (match total with
          | None -> true
          | Some n -> Int64.compare last n < 0)
  ;;

  let[@inline always] char_at (local_ s : string) i =
    Char_u.of_char (String.unsafe_get s i)
  ;;

  let rec left (local_ s : string) i stop =
    if i < stop && Buf_read.is_space (char_at s i) then left s (i + 1) stop else i
  ;;

  let rec right (local_ s : string) start i =
    if i > start && Buf_read.is_space (char_at s (i - 1))
    then right s start (i - 1)
    else i
  ;;

  let rec digits (local_ s : string) i stop =
    if i < stop
       && let c = char_at s i in
          Char_u.compare c #'0' >= 0 && Char_u.compare c #'9' <= 0
    then digits s (i + 1) stop
    else i
  ;;

  let rec nonzero (local_ s : string) i stop =
    if i < stop && Char_u.equal (char_at s i) #'0'
    then nonzero s (i + 1) stop
    else i
  ;;

  let rec compare_digits (local_ s : string) a b len =
    if len = 0
    then 0
    else
      let ca = char_at s a in
      let cb = char_at s b in
      if Char_u.equal ca cb
      then compare_digits s (a + 1) (b + 1) (len - 1)
      else Char_u.compare ca cb
  ;;

  (* Compare decimal values without converting or bounding their digit count.
     This admits leading zeros and cannot overflow on a received field. *)
  let[@inline] compare_decimal (local_ s : string) a b c d =
    let a = nonzero s a b in
    let c = nonzero s c d in
    let lengths = Int.compare (b - a) (d - c) in
    if lengths <> 0 then lengths else compare_digits s a c (b - a)
  ;;

  let rec unit_matches
      (local_ s : string) first (local_ unit : string) i =
    i = String.length unit
    || (Char_u.equal
          (Buf_read.to_lower (char_at s (first + i)))
          (Buf_read.to_lower (char_at unit i))
        && unit_matches s first unit (i + 1))
  ;;

  let[@zero_alloc] kind ~(unit : string @ local) (local_ s : string) =
    let first = left s 0 (String.length s) in
    let stop = right s first (String.length s) in
    let start = first + String.length unit in
    if not (Header.Syntax.is_token unit)
       || start >= stop
       || not (Char_u.equal (char_at s start) #' ')
       || not (unit_matches s first unit 0)
    then Invalid
    else (
      let a = start + 1 in
      if a + 2 <= stop
         && Char_u.equal (char_at s a) #'*'
         && Char_u.equal (char_at s (a + 1)) #'/'
      then (
        let total = a + 2 in
        if total < stop && digits s total stop = stop then Unsatisfied else Invalid)
      else (
        let dash = digits s a stop in
        if dash = a || dash >= stop || not (Char_u.equal (char_at s dash) #'-')
        then Invalid
        else (
          let b = dash + 1 in
          let slash = digits s b stop in
          if slash = b
             || slash >= stop
             || not (Char_u.equal (char_at s slash) #'/')
             || compare_decimal s a dash b slash > 0
          then Invalid
          else (
            let total = slash + 1 in
            if total + 1 = stop && Char_u.equal (char_at s total) #'*'
            then Satisfied
            else if total < stop
                    && digits s total stop = stop
                    && compare_decimal s b slash total stop < 0
            then Satisfied
            else Invalid))))
  ;;
end

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_i16 x = I16.to_int x
let[@inline always] i64 x = I64.of_int64 x

let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] digit_value c = Buf_read.digit_value c
let ( =. ) = Buf_read.( =. )

type byte_range =
  #{ kind : int
   ; start : int64#
   ; end_ : int64#
   }

let kind_range = 0
let kind_suffix = 1
let kind_open = 2

let max_ranges : int16# = i16 16
let empty = #{ kind = 0; start = i64 0L; end_ = i64 0L }
let[@inline always] is_range (r : byte_range) = r.#kind = kind_range
let[@inline always] is_suffix (r : byte_range) = r.#kind = kind_suffix
let[@inline always] is_open (r : byte_range) = r.#kind = kind_open

type parse_status =
  | Valid
  | Invalid

type resolved =
  #{ start : int64#
   ; end_ : int64#
   ; length : int64#
   }

let empty_resolved = #{ start = i64 0L; end_ = i64 0L; length = i64 0L }

let max_int64_div_10 : int64# = #922337203685477580L
let max_int64_last_digit = 7

type eval_result =
  | Full_content
  | Single_range
  | Multiple_ranges
  | Not_satisfiable

(* {!Buf_read.skip_ows} works in [int16#] while this parser indexes in [int]. *)
let[@inline] skip_ows buf ~pos ~len =
  to_i16 (Buf_read.skip_ows buf ~pos:(i16 pos) ~len:(i16 len))
;;

(* Accumulates in [int64#]: the boxed [Int64] version allocated a 24-byte box
   per digit through Base's arithmetic, which is what made the whole Range
   path allocate despite the interface's zero-allocation claim. *)
let[@inline] parse_int64 buf ~pos ~len =
  let start = pos in
  let mutable p = pos in
  let mutable acc : int64# = #0L in
  let mutable valid = true in
  let mutable overflow = false in
  while valid && p < len do
    let d = digit_value (peek buf p) in
    if d >= 0 then (
      if
        I64.compare acc max_int64_div_10 > 0
        || (I64.equal acc max_int64_div_10 && d > max_int64_last_digit)
      then (
        overflow <- true;
        valid <- false
      ) else (
        acc <- I64.add (I64.mul acc #10L) (I64.of_int d);
        p <- p + 1
      )
    ) else
      valid <- false
  done;
  if overflow then #(#0L, p, false)
  else if p > start then #(acc, p, true)
  else #(#0L, pos, false)
;;

let[@inline] parse_range_spec buf ~pos ~len =
  let pos = skip_ows buf ~pos ~len in
  if pos >= len then #(false, empty, pos)
  else
    let c = peek buf pos in
    if c =. #'-' then
      let #(suffix, end_pos, valid) = parse_int64 buf ~pos:(pos + 1) ~len in
      if not valid then
        #(false, empty, end_pos)
      else
        #(true, #{ kind = kind_suffix; start = suffix; end_ = #0L }, end_pos)
    else
      let #(start, after_start, valid) = parse_int64 buf ~pos ~len in
      if not valid then #(false, empty, after_start)
      else if after_start >= len then #(false, empty, after_start)
      else if not (peek buf after_start =. #'-') then
        #(false, empty, after_start)
      else
        let after_dash = after_start + 1 in
        if after_dash >= len || (
          let c = peek buf after_dash in
          c =. #',' || c =. #' ' || c =. #'\t'
        ) then
          #(true, #{ kind = kind_open; start; end_ = #0L }, after_dash)
        else
          let #(end_val, end_pos, end_valid) = parse_int64 buf ~pos:after_dash ~len in
          if (not end_valid) || I64.compare end_val start < 0 then
            #(false, empty, end_pos)
          else
            #(true, #{ kind = kind_range; start; end_ = end_val }, end_pos)
;;

let parse_region (local_ buf) ~off ~len (ranges : byte_range array)
  : #(parse_status * int16#)
  =
  let end_pos = off + len in
  let mutable eq_pos = off in
  while eq_pos < end_pos && not (peek buf eq_pos =. #'=') do
    eq_pos <- eq_pos + 1
  done;
  if eq_pos >= end_pos then #(Invalid, i16 0)
  else
    let unit_len = eq_pos - off in
    if unit_len <> 5 then #(Invalid, i16 0)
    else
      let is_bytes =
        let c0 = peek buf off in
        let c1 = peek buf (off + 1) in
        let c2 = peek buf (off + 2) in
        let c3 = peek buf (off + 3) in
        let c4 = peek buf (off + 4) in
        (c0 =. #'b' || c0 =. #'B') &&
        (c1 =. #'y' || c1 =. #'Y') &&
        (c2 =. #'t' || c2 =. #'T') &&
        (c3 =. #'e' || c3 =. #'E') &&
        (c4 =. #'s' || c4 =. #'S')
      in
      if not is_bytes then #(Invalid, i16 0)
      else
        let mutable pos = eq_pos + 1 in
        let mutable count = 0 in
        let mutable valid = true in
        while valid && pos < end_pos && count < to_i16 max_ranges do
          pos <- skip_ows buf ~pos ~len:end_pos;
          if pos >= end_pos then
            valid <- false
          else
            let #(ok, range, after_range) = parse_range_spec buf ~pos ~len:end_pos in
            if ok then (
              Array.unsafe_set ranges count range;
              count <- count + 1
            ) else
              valid <- false;
            pos <- skip_ows buf ~pos:after_range ~len:end_pos;
            if pos < end_pos then
              if peek buf pos =. #',' then (
                pos <- skip_ows buf ~pos:(pos + 1) ~len:end_pos;
                if pos >= end_pos then valid <- false
              )
              else
                valid <- false
        done;
        if valid && count > 0 && pos >= end_pos then
          #(Valid, i16 count)
        else
          #(Invalid, i16 0)
;;

let parse (local_ buf) (sp : Span.t) (ranges : byte_range array)
  : #(parse_status * int16#)
  =
  parse_region buf ~off:(Span.off sp) ~len:(Span.len sp) ranges
;;

(* Parse Range header from string. Sound only because [parse_region] is
   read-only: it reaches [s] through [Buf_read.peek] and never writes. Any
   future write into [buf] would corrupt an immutable string. *)
let parse_string (s : string) (ranges : byte_range array) : #(parse_status * int16#) =
  let buf = Stdlib.Bytes.unsafe_of_string s in
  parse_region buf ~off:0 ~len:(String.length s) ranges
;;

let resolve_range (range : byte_range) ~(resource_length : int64#)
  : #(bool * resolved)
  =
  let res_len = resource_length in
  if I64.compare res_len #0L <= 0 then #(false, empty_resolved)
  else
    let kind = range.#kind in
    let start_val = range.#start in
    let end_val = range.#end_ in
    let last = I64.sub res_len #1L in
    if kind = kind_range then
      if I64.compare start_val res_len >= 0 then #(false, empty_resolved)
      else
        let end_clamped = if I64.compare end_val last < 0 then end_val else last in
        let length = I64.add (I64.sub end_clamped start_val) #1L in
        #(true, #{ start = start_val; end_ = end_clamped; length })
    else if kind = kind_suffix then
      let suffix = start_val in
      if I64.compare suffix #0L <= 0 then #(false, empty_resolved)
      else
        let from_end = I64.sub res_len suffix in
        let start = if I64.compare from_end #0L > 0 then from_end else #0L in
        let length = I64.add (I64.sub last start) #1L in
        #(true, #{ start; end_ = last; length })
    else if I64.compare start_val res_len >= 0 then #(false, empty_resolved)
    else
      let length = I64.add (I64.sub last start_val) #1L in
      #(true, #{ start = start_val; end_ = last; length })
;;

let evaluate
  (ranges : byte_range array)
  ~(count : int16#)
  ~(resource_length : int64#)
  (out : resolved array)
  : #(eval_result * int16#)
  =
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

let write_accept_ranges dst ~off =
  let off = Buf_write.string dst ~off "Accept-Ranges: bytes" in
  Buf_write.crlf dst ~off
;;

let write_accept_ranges_none dst ~off =
  let off = Buf_write.string dst ~off "Accept-Ranges: none" in
  Buf_write.crlf dst ~off
;;

let write_content_range dst ~off ~(start : int64#) ~(end_ : int64#)
  ~(total : int64#)
  =
  let off = Buf_write.string dst ~off "Content-Range: bytes " in
  let off = Buf_write.int64 dst ~off start in
  let off = Buf_write.char dst ~off '-' in
  let off = Buf_write.int64 dst ~off end_ in
  let off = Buf_write.char dst ~off '/' in
  let off = Buf_write.int64 dst ~off total in
  Buf_write.crlf dst ~off
;;

let write_content_range_unsatisfiable dst ~off ~(total : int64#) =
  let off = Buf_write.string dst ~off "Content-Range: bytes */" in
  let off = Buf_write.int64 dst ~off total in
  Buf_write.crlf dst ~off
;;

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

(* The default [Random] state is seeded identically in every process, which
   would make every server's boundaries the same sequence from startup. A
   self-initialised state costs one lazy force and keeps them per-process. *)
let boundary_state = lazy (Random.State.make_self_init ())

let generate_boundary () =
  let chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let state = Lazy.force boundary_state in
  let len = 24 in
  let buf = Bytes.create len in
  for i = 0 to len - 1 do
    let idx = Random.State.int state (String.length chars) in
    Bytes.set buf i (String.get chars idx)
  done;
  Bytes.to_string buf
;;
