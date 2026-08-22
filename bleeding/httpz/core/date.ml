(* date.ml - HTTP-date parsing and formatting per RFC 7231 Section 7.1.1.1 *)

open Base

module F64 = Stdlib_upstream_compatible.Float_u
module Iarray = Stdlib_stable.Iarray
module I16 = Stdlib_stable.Int16_u

let[@inline always] f64 x = F64.of_float x
let[@inline always] to_float x = F64.to_float x
let[@inline always] i16 x = I16.of_int x

(* Unboxed char helpers - use Buf_read's primitives *)
let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] digit_value c = Buf_read.digit_value c
let ( =. ) = Buf_read.( =. )

type status =
  | Valid
  | Invalid

(* Parse 2-digit number at position, returns (value, valid) *)
let[@inline] parse_2digit buf pos =
  let c0 = peek buf pos in
  let c1 = peek buf (pos + 1) in
  let d0 = digit_value c0 in
  let d1 = digit_value c1 in
  if d0 >= 0 && d1 >= 0 then
    #(d0 * 10 + d1, true)
  else
    #(0, false)
;;

(* Parse 4-digit year at position, returns (value, valid) *)
let[@inline] parse_4digit buf pos =
  let d0 = digit_value (peek buf pos) in
  let d1 = digit_value (peek buf (pos + 1)) in
  let d2 = digit_value (peek buf (pos + 2)) in
  let d3 = digit_value (peek buf (pos + 3)) in
  if d0 >= 0 && d1 >= 0 && d2 >= 0 && d3 >= 0 then
    #(d0 * 1000 + d1 * 100 + d2 * 10 + d3, true)
  else
    #(0, false)
;;

(* Parse 1 or 2 digit day, returns (day, next_pos, valid) *)
let[@inline] parse_day buf pos len =
  if pos >= len then #(0, pos, false)
  else
    let c0 = peek buf pos in
    if c0 =. #' ' && pos + 1 < len then
      (* Space-padded single digit *)
      let d1 = digit_value (peek buf (pos + 1)) in
      if d1 >= 0 then #(d1, pos + 2, true)
      else #(0, pos, false)
    else
      let d0 = digit_value c0 in
      if d0 >= 0 && pos + 1 < len then
        let d1 = digit_value (peek buf (pos + 1)) in
        if d1 >= 0 then
          #(d0 * 10 + d1, pos + 2, true)
        else
          #(d0, pos + 1, true)
      else
        #(0, pos, false)
;;

(* Parse 3-letter month abbreviation, returns 0-11 or -1
   Uses unboxed tuple pattern matching for cleaner code *)
let[@inline] parse_month buf pos =
  let c0 = peek buf pos in
  let c1 = peek buf (pos + 1) in
  let c2 = peek buf (pos + 2) in
  match #(c0, c1, c2) with
  | #(#'J', #'a', #'n') -> 0
  | #(#'F', #'e', #'b') -> 1
  | #(#'M', #'a', #'r') -> 2
  | #(#'A', #'p', #'r') -> 3
  | #(#'M', #'a', #'y') -> 4
  | #(#'J', #'u', #'n') -> 5
  | #(#'J', #'u', #'l') -> 6
  | #(#'A', #'u', #'g') -> 7
  | #(#'S', #'e', #'p') -> 8
  | #(#'O', #'c', #'t') -> 9
  | #(#'N', #'o', #'v') -> 10
  | #(#'D', #'e', #'c') -> 11
  | _ -> -1
;;

(* Parse time HH:MM:SS at position, returns (hour, minute, second, valid) *)
let[@inline] parse_time buf pos =
  let #(hour, h_valid) = parse_2digit buf pos in
  if not h_valid then #(0, 0, 0, false)
  else if not (peek buf (pos + 2) =. #':') then #(0, 0, 0, false)
  else
    let #(minute, m_valid) = parse_2digit buf (pos + 3) in
    if not m_valid then #(0, 0, 0, false)
    else if not (peek buf (pos + 5) =. #':') then #(0, 0, 0, false)
    else
      let #(second, s_valid) = parse_2digit buf (pos + 6) in
      if not s_valid then #(0, 0, 0, false)
      else if hour > 23 || minute > 59 || second > 60 then #(0, 0, 0, false)  (* 60 for leap second *)
      else #(hour, minute, second, true)
;;

(* Days in each month (non-leap year) *)
let days_in_month : int iarray = [: 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 :]

(* Check if year is leap year *)
let[@inline] is_leap_year year =
  (year % 4 = 0 && year % 100 <> 0) || (year % 400 = 0)
;;

(* Days from epoch (1970-01-01) to start of year using formula.
   This avoids the O(year) loop in the original implementation. *)
let[@inline] days_to_year year =
  let y = year - 1 in
  let y0 = 1969 in  (* year before epoch *)
  (* Formula: count days for each year since 1970, accounting for leap years *)
  365 * (y - y0) + (y / 4 - y0 / 4) - (y / 100 - y0 / 100) + (y / 400 - y0 / 400)
;;

(* Cumulative days before each month (0-indexed, non-leap year) *)
let days_before_month : int iarray =
  [: 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 :]
;;

(* Convert date components to Unix timestamp, returns (timestamp, valid) *)
let to_timestamp ~year ~month ~day ~hour ~minute ~second =
  (* Validate ranges *)
  (* [days_to_year] divides only non-negative operands for any year from 1 on,
     so truncation is floor there and the formula holds below the epoch as
     well as above it. The floor is 1 because that is where [format] stops:
     rejecting 1970 and earlier meant [parse] could not read back what
     [format] wrote, and an If-Modified-Since naming any pre-epoch instant was
     silently dropped. *)
  if year < 1 || month < 0 || month > 11 then #(f64 0.0, false)
  else
    let max_day =
      if month = 1 && is_leap_year year then 29
      else Iarray.unsafe_get days_in_month month
    in
    if day < 1 || day > max_day then #(f64 0.0, false)
    else
      (* Calculate days since epoch using formula *)
      let days = days_to_year year in
      (* Add days for complete months in current year *)
      let days = days + Iarray.unsafe_get days_before_month month in
      (* Add leap day if past February in a leap year *)
      let days = if month > 1 && is_leap_year year then days + 1 else days in
      (* Add days in current month (day is 1-indexed) *)
      let days = days + (day - 1) in
      (* Convert to seconds and add time *)
      let timestamp =
        Float.of_int days *. 86400.0 +.
        Float.of_int hour *. 3600.0 +.
        Float.of_int minute *. 60.0 +.
        Float.of_int second
      in
      #(f64 timestamp, true)
;;

let invalid_result = #(f64 0.0, false)

(* Parse IMF-fixdate: Sun, 06 Nov 1994 08:49:37 GMT *)
let parse_imf_fixdate buf off len =
  (* Minimum length: "Sun, 06 Nov 1994 08:49:37 GMT" = 29 chars *)
  if len < 29 then invalid_result
  else
    (* Skip day name - find comma *)
    let mutable comma_pos = off in
    while comma_pos < off + 4 && not (peek buf comma_pos =. #',') do
      comma_pos <- comma_pos + 1
    done;
    if comma_pos >= off + len || not (peek buf comma_pos =. #',') then invalid_result
    else if not (peek buf (comma_pos + 1) =. #' ') then invalid_result
    else
      let day_pos = comma_pos + 2 in
      let #(day, day_valid) = parse_2digit buf day_pos in
      if not day_valid then invalid_result
      else if not (peek buf (day_pos + 2) =. #' ') then invalid_result
      else
        let month = parse_month buf (day_pos + 3) in
        if month < 0 then invalid_result
        else if not (peek buf (day_pos + 6) =. #' ') then invalid_result
        else
          let #(year, year_valid) = parse_4digit buf (day_pos + 7) in
          if not year_valid then invalid_result
          else if not (peek buf (day_pos + 11) =. #' ') then invalid_result
          else
            let #(hour, minute, second, time_valid) = parse_time buf (day_pos + 12) in
            if not time_valid then invalid_result
            else
              (* Check for " GMT" at end *)
              let gmt_pos = day_pos + 20 in
              if gmt_pos + 4 > off + len then invalid_result
              else if not (peek buf gmt_pos =. #' ') then invalid_result
              else if not (peek buf (gmt_pos + 1) =. #'G') then invalid_result
              else if not (peek buf (gmt_pos + 2) =. #'M') then invalid_result
              else if not (peek buf (gmt_pos + 3) =. #'T') then invalid_result
              else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Parse RFC 850 date: Sunday, 06-Nov-94 08:49:37 GMT *)
let parse_rfc850 buf off len =
  (* Find comma after full day name *)
  let mutable comma_pos = off in
  while comma_pos < off + 10 && not (peek buf comma_pos =. #',') do
    comma_pos <- comma_pos + 1
  done;
  if comma_pos >= off + len || not (peek buf comma_pos =. #',') then invalid_result
  else if not (peek buf (comma_pos + 1) =. #' ') then invalid_result
  else
    let pos = comma_pos + 2 in
    let #(day, day_valid) = parse_2digit buf pos in
    if not day_valid then invalid_result
    else if not (peek buf (pos + 2) =. #'-') then invalid_result
    else
      let month = parse_month buf (pos + 3) in
      if month < 0 then invalid_result
      else if not (peek buf (pos + 6) =. #'-') then invalid_result
      else
        let #(year2, year2_valid) = parse_2digit buf (pos + 7) in
        if not year2_valid then invalid_result
        else
          (* RFC 850 uses 2-digit year. Interpret 00-99 as 2000-2099 for dates >= 70,
             and 1970-1999 for dates < 70. Modern interpretation varies. *)
          let year = if year2 >= 70 then 1900 + year2 else 2000 + year2 in
          if not (peek buf (pos + 9) =. #' ') then invalid_result
          else
            let #(hour, minute, second, time_valid) = parse_time buf (pos + 10) in
            if not time_valid then invalid_result
            else
              (* Check for " GMT" *)
              let gmt_pos = pos + 18 in
              if gmt_pos + 4 > off + len then invalid_result
              else if not (peek buf gmt_pos =. #' ') then invalid_result
              else if not (peek buf (gmt_pos + 1) =. #'G') then invalid_result
              else if not (peek buf (gmt_pos + 2) =. #'M') then invalid_result
              else if not (peek buf (gmt_pos + 3) =. #'T') then invalid_result
              else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Parse asctime format: Sun Nov  6 08:49:37 1994 *)
let parse_asctime buf off len =
  (* Minimum length: "Sun Nov  6 08:49:37 1994" = 24 chars *)
  if len < 24 then invalid_result
  (* Skip 3-char day name and space *)
  else if not (peek buf (off + 3) =. #' ') then invalid_result
  else
    let month = parse_month buf (off + 4) in
    if month < 0 then invalid_result
    else if not (peek buf (off + 7) =. #' ') then invalid_result
    else
      let #(day, next_pos, day_valid) = parse_day buf (off + 8) len in
      if not day_valid then invalid_result
      else if not (peek buf next_pos =. #' ') then invalid_result
      else
        let #(hour, minute, second, time_valid) = parse_time buf (next_pos + 1) in
        if not time_valid then invalid_result
        else
          let year_pos = next_pos + 9 in
          if not (peek buf year_pos =. #' ') then invalid_result
          else
            let #(year, year_valid) = parse_4digit buf (year_pos + 1) in
            if not year_valid then invalid_result
            else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Main parse function - tries all three formats *)
let parse (local_ buf) (sp : Span.t) : #(status * float#) =
  let off = Span.off sp in
  let len = Span.len sp in
  if len < 24 then #(Invalid, f64 0.0)
  else
    (* Check for comma to distinguish IMF-fixdate/RFC850 from asctime *)
    let c4 = peek buf (off + 3) in
    let #(ts, valid) =
      if c4 =. #',' then
        (* IMF-fixdate: short day name + comma *)
        parse_imf_fixdate buf off len
      else if c4 =. #' ' then
        (* asctime: short day name + space *)
        parse_asctime buf off len
      else
        (* RFC 850: full day name, look for comma *)
        parse_rfc850 buf off len
    in
    if valid then #(Valid, ts) else #(Invalid, f64 0.0)
;;

(* ----- IMF-fixdate formatting -----

   [Unix.gmtime] allocates a [Unix.tm] record and costs an external call, and
   [Printf.sprintf] pulls in the whole [CamlinternalFormat] machinery, for an
   output that is 29 bytes of fixed layout. Both are replaced below by direct
   writes driven by static tables and Howard Hinnant's [civil_from_days]. *)

let day_table = "SunMonTueWedThuFriSat"
let month_table = "JanFebMarAprMayJunJulAugSepOctNovDec"

(* "00" "01" ... "99", so a two-digit field is two table reads. *)
let digit_pairs =
  String.init 200 ~f:(fun i ->
    if i % 2 = 0
    then Stdlib.Char.unsafe_chr (48 + (i / 2 / 10))
    else Stdlib.Char.unsafe_chr (48 + (i / 2 % 10)))
;;

let[@inline always] put3 dst off (tab : string) idx =
  Bytes.unsafe_set dst off (String.unsafe_get tab idx);
  Bytes.unsafe_set dst (off + 1) (String.unsafe_get tab (idx + 1));
  Bytes.unsafe_set dst (off + 2) (String.unsafe_get tab (idx + 2))
;;

let[@inline always] put2 dst off n =
  Bytes.unsafe_set dst off (String.unsafe_get digit_pairs (n * 2));
  Bytes.unsafe_set dst (off + 1) (String.unsafe_get digit_pairs ((n * 2) + 1))
;;

(* 0001-01-01 and 9999-12-31 as seconds from the epoch. The writes below index
   [day_table] and [month_table] with [String.unsafe_get], so a timestamp that
   drove the civil-from-days arithmetic out of range — or overflowed it — would
   write outside the tables. [Unix.gmtime] raised on such input; clamping keeps
   the same inputs safe without reintroducing the call. NaN and the infinities
   convert to an unspecified [int], which the clamp also contains. *)
let min_secs = -62_135_596_800
let max_secs = 253_402_300_799

let[@inline] write_http_date dst ~off (timestamp : float#) =
  let secs = Stdlib.int_of_float (to_float timestamp) in
  let secs = if secs < min_secs then min_secs else if secs > max_secs then max_secs else secs in
  (* Floor division: [sod] must stay in [0, 86399] for pre-epoch timestamps. *)
  let days = if secs >= 0 then secs / 86_400 else ((secs + 1) / 86_400) - 1 in
  let sod = secs - (days * 86_400) in
  let w = Stdlib.( mod ) (days + 4) 7 in
  let wday = if w < 0 then w + 7 else w in
  (* civil_from_days: shift the era to start in March so the leap day lands at
     the end of the year and the month-length pattern becomes affine. *)
  let z = days + 719_468 in
  let era = (if z >= 0 then z else z - 146_096) / 146_097 in
  let doe = z - (era * 146_097) in
  let yoe = (doe - (doe / 1460) + (doe / 36_524) - (doe / 146_096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = mp + if mp < 10 then 3 else -9 in
  let y = if m <= 2 then y + 1 else y in
  let o = Buf_write.to_int off in
  put3 dst o day_table (wday * 3);
  Bytes.unsafe_set dst (o + 3) ',';
  Bytes.unsafe_set dst (o + 4) ' ';
  put2 dst (o + 5) d;
  Bytes.unsafe_set dst (o + 7) ' ';
  put3 dst (o + 8) month_table ((m - 1) * 3);
  Bytes.unsafe_set dst (o + 11) ' ';
  put2 dst (o + 12) (y / 100);
  put2 dst (o + 14) (Stdlib.( mod ) y 100);
  Bytes.unsafe_set dst (o + 16) ' ';
  put2 dst (o + 17) (sod / 3600);
  Bytes.unsafe_set dst (o + 19) ':';
  put2 dst (o + 20) (Stdlib.( mod ) (sod / 60) 60);
  Bytes.unsafe_set dst (o + 22) ':';
  put2 dst (o + 23) (Stdlib.( mod ) sod 60);
  Bytes.unsafe_set dst (o + 25) ' ';
  Bytes.unsafe_set dst (o + 26) 'G';
  Bytes.unsafe_set dst (o + 27) 'M';
  Bytes.unsafe_set dst (o + 28) 'T';
  Buf_write.i16 (o + 29)
;;

(* Format timestamp as IMF-fixdate *)
let format (timestamp : float#) : string =
  let dst = Bytes.create 29 in
  let _ : int16# = write_http_date dst ~off:(Buf_write.i16 0) timestamp in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

let write_date_header dst ~off (timestamp : float#) =
  let off = Buf_write.string dst ~off "Date: " in
  let off = write_http_date dst ~off timestamp in
  Buf_write.crlf dst ~off
;;

let write_last_modified dst ~off (timestamp : float#) =
  let off = Buf_write.string dst ~off "Last-Modified: " in
  let off = write_http_date dst ~off timestamp in
  Buf_write.crlf dst ~off
;;

let write_expires dst ~off (timestamp : float#) =
  let off = Buf_write.string dst ~off "Expires: " in
  let off = write_http_date dst ~off timestamp in
  Buf_write.crlf dst ~off
;;

(* Comparison helpers - use unboxed floats *)
let is_modified_since ~(last_modified : float#) ~(if_modified_since : float#) =
  (* Resource is modified if last_modified > if_modified_since
     Note: HTTP dates have 1-second resolution, so we use > not >= *)
  F64.compare last_modified if_modified_since > 0
;;

let is_unmodified_since ~(last_modified : float#) ~(if_unmodified_since : float#) =
  (* Resource is unmodified if last_modified <= if_unmodified_since *)
  F64.compare last_modified if_unmodified_since <= 0
;;
