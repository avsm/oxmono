open Base

module F64 = Stdlib_upstream_compatible.Float_u
module Iarray = Stdlib_stable.Iarray
module I16 = Stdlib_stable.Int16_u
module Char_u = Stdlib_stable.Char_u

let[@inline always] f64 x = F64.of_float x
let[@inline always] to_float x = F64.to_float x
let[@inline always] i16 x = I16.of_int x

let[@inline always] peek buf pos = Buf_read.peek buf (i16 pos)
let[@inline always] digit_value c = Buf_read.digit_value c
let ( =. ) = Buf_read.( =. )

type status =
  | Valid
  | Invalid

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

(* [limit] is the first offset past the value, not a length: the span rarely
   starts at zero. *)
let[@inline] parse_day buf pos limit =
  if pos >= limit then #(0, pos, false)
  else
    let c0 = peek buf pos in
    if c0 =. #' ' && pos + 1 < limit then
      let d1 = digit_value (peek buf (pos + 1)) in
      if d1 >= 0 then #(d1, pos + 2, true)
      else #(0, pos, false)
    else
      let d0 = digit_value c0 in
      if d0 >= 0 && pos + 1 < limit then
        let d1 = digit_value (peek buf (pos + 1)) in
        if d1 >= 0 then
          #(d0 * 10 + d1, pos + 2, true)
        else
          #(d0, pos + 1, true)
      else
        #(0, pos, false)
;;

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
      else if hour > 23 || minute > 59 || second > 60 then #(0, 0, 0, false)
      else #(hour, minute, second, true)
;;

let days_in_month : int iarray = [: 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 :]

let[@inline] is_leap_year year =
  (year % 4 = 0 && year % 100 <> 0) || (year % 400 = 0)
;;

let[@inline] days_to_year year =
  let y = year - 1 in
  let y0 = 1969 in
  365 * (y - y0) + (y / 4 - y0 / 4) - (y / 100 - y0 / 100) + (y / 400 - y0 / 400)
;;

let days_before_month : int iarray =
  [: 0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334 :]
;;

let to_timestamp ~year ~month ~day ~hour ~minute ~second =
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
      let days = days_to_year year in
      let days = days + Iarray.unsafe_get days_before_month month in
      let days = if month > 1 && is_leap_year year then days + 1 else days in
      let days = days + (day - 1) in
      let timestamp =
        Float.of_int days *. 86400.0 +.
        Float.of_int hour *. 3600.0 +.
        Float.of_int minute *. 60.0 +.
        Float.of_int second
      in
      #(f64 timestamp, true)
;;

let invalid_result = #(f64 0.0, false)

(* Clamp to calendar years 1 to 9999 before converting a timestamp to civil
   components. *)
let min_secs = -62_135_596_800
let max_secs = 253_402_300_799
let min_secs_f : float# = f64 (-62_135_596_800.)
let max_secs_f : float# = f64 253_402_300_799.

let civil_time (timestamp : float#) =
  (* Compare before conversion: converting an out-of-range float to [int] has
     platform-dependent results. *)
  let secs =
    if F64.compare timestamp min_secs_f <= 0 then min_secs
    else if F64.compare timestamp max_secs_f >= 0 then max_secs
    else Stdlib.int_of_float (to_float timestamp)
  in
  (* Floor division keeps [sod] in [0, 86399] before the Unix epoch. *)
  let days = if secs >= 0 then secs / 86_400 else ((secs + 1) / 86_400) - 1 in
  let sod = secs - (days * 86_400) in
  let w = Stdlib.( mod ) (days + 4) 7 in
  let wday = if w < 0 then w + 7 else w in
  (* Shifting the civil calendar to March puts leap day at the end of an era. *)
  let z = days + 719_468 in
  let era = (if z >= 0 then z else z - 146_096) / 146_097 in
  let doe = z - (era * 146_097) in
  let yoe = (doe - (doe / 1460) + (doe / 36_524) - (doe / 146_096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = mp + if mp < 10 then 3 else -9 in
  let year = if month <= 2 then y + 1 else y in
  #(year, month - 1, day, wday, sod)
;;

let rfc850_year ~has_now (now : float#) ~month ~day ~hour ~minute ~second
    year2 =
  if not has_now then
    if year2 >= 70 then 1900 + year2 else 2000 + year2
  else
    let #(now_year, now_month, now_day, _, now_sod) = civil_time now in
    let limit_year = now_year + 50 in
    (* Start with the latest year having these final digits that can fall no
       later than the reference year plus 50. The date and time comparison
       below handles the boundary year exactly. *)
    let candidate = ((limit_year / 100) * 100) + year2 in
    let sod = (hour * 3600) + (minute * 60) + second in
    let beyond_limit =
      candidate > limit_year
      || (candidate = limit_year
          && (month > now_month
              || (month = now_month
                  && (day > now_day || (day = now_day && sod > now_sod)))))
    in
    if beyond_limit then candidate - 100 else candidate
;;

(* Only the GMT zone is accepted, spelled exactly, for both the preferred and
   the obsolete date forms (RFC 9110, Section 5.6.7). *)
let[@inline] has_gmt buf ~pos ~stop =
  pos + 4 = stop
  && peek buf pos =. #' '
  && peek buf (pos + 1) =. #'G'
  && peek buf (pos + 2) =. #'M'
  && peek buf (pos + 3) =. #'T'
;;

let[@inline] matches_literal (local_ buf) ~off literal =
  let mutable i = 0 in
  let literal_len = String.length literal in
  while
    i < literal_len
    && peek buf (off + i) =. Char_u.of_char (String.unsafe_get literal i)
  do
    i <- i + 1
  done;
  i = literal_len
;;

let[@inline] has_short_weekday (local_ buf) off =
  matches_literal buf ~off "Sun"
  || matches_literal buf ~off "Mon"
  || matches_literal buf ~off "Tue"
  || matches_literal buf ~off "Wed"
  || matches_literal buf ~off "Thu"
  || matches_literal buf ~off "Fri"
  || matches_literal buf ~off "Sat"
;;

let has_long_weekday (local_ buf) ~off ~len =
  match len with
  | 6 -> matches_literal buf ~off "Sunday" || matches_literal buf ~off "Monday"
         || matches_literal buf ~off "Friday"
  | 7 -> matches_literal buf ~off "Tuesday"
  | 8 -> matches_literal buf ~off "Thursday" || matches_literal buf ~off "Saturday"
  | 9 -> matches_literal buf ~off "Wednesday"
  | _ -> false
;;

let parse_imf_fixdate buf off len =
  if len < 29 then invalid_result
  else if not (has_short_weekday buf off) then invalid_result
  else
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
            else if not (has_gmt buf ~pos:(day_pos + 20) ~stop:(off + len))
            then invalid_result
            else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Bytes from the comma to the end of the value: "," SP "DD-Mon-YY" SP
   "HH:MM:SS" " GMT". Every peek below is within this many bytes of the comma,
   so the one guard covers them all. *)
let rfc850_min_after_comma = 24

let parse_rfc850 ~has_now (now : float#) buf off len =
  let mutable comma_pos = off in
  while comma_pos < off + 10 && not (peek buf comma_pos =. #',') do
    comma_pos <- comma_pos + 1
  done;
  if comma_pos >= off + len || not (peek buf comma_pos =. #',') then invalid_result
  else if not (has_long_weekday buf ~off ~len:(comma_pos - off)) then invalid_result
  else if off + len - comma_pos < rfc850_min_after_comma then invalid_result
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
        else if not (peek buf (pos + 9) =. #' ') then invalid_result
        else
          let #(hour, minute, second, time_valid) = parse_time buf (pos + 10) in
          if not time_valid then invalid_result
          else if not (has_gmt buf ~pos:(pos + 18) ~stop:(off + len))
          then invalid_result
          else
            let year =
              rfc850_year ~has_now now ~month ~day ~hour ~minute ~second year2
            in
            to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

let parse_asctime buf off len =
  if len <> 24 then invalid_result
  else if not (has_short_weekday buf off) then invalid_result
  else if not (peek buf (off + 3) =. #' ') then invalid_result
  else
    let month = parse_month buf (off + 4) in
    if month < 0 then invalid_result
    else if not (peek buf (off + 7) =. #' ') then invalid_result
    else
      let #(day, next_pos, day_valid) = parse_day buf (off + 8) (off + len) in
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

let parse_unboxed ~has_now (now : float#) (local_ buf) (sp : Span.t)
    : #(status * float#) =
  let off = Span.off sp in
  let len = Span.len sp in
  if len < 24 then #(Invalid, f64 0.0)
  else
    let c4 = peek buf (off + 3) in
    let #(ts, valid) =
      if c4 =. #',' then
        parse_imf_fixdate buf off len
      else if c4 =. #' ' then
        parse_asctime buf off len
      else
        parse_rfc850 ~has_now now buf off len
    in
    if valid then #(Valid, ts) else #(Invalid, f64 0.0)
;;

let parse ?now (local_ buf) (sp : Span.t) : #(status * float#) =
  match now with
  | None -> parse_unboxed ~has_now:false #0. buf sp
  | Some now -> parse_unboxed ~has_now:true (f64 now) buf sp
;;

let day_table = "SunMonTueWedThuFriSat"
let month_table = "JanFebMarAprMayJunJulAugSepOctNovDec"

let[@inline always] put3 dst off (tab : string) idx =
  Bytes.unsafe_set dst off (String.unsafe_get tab idx);
  Bytes.unsafe_set dst (off + 1) (String.unsafe_get tab (idx + 1));
  Bytes.unsafe_set dst (off + 2) (String.unsafe_get tab (idx + 2))
;;

let[@inline always] put2 dst off n =
  let pairs = Buf_write.digit_pairs in
  Bytes.unsafe_set dst off (String.unsafe_get pairs (n * 2));
  Bytes.unsafe_set dst (off + 1) (String.unsafe_get pairs ((n * 2) + 1))
;;

let[@inline] write_http_date dst ~off (timestamp : float#) =
  let #(y, month, d, wday, sod) = civil_time timestamp in
  let m = month + 1 in
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

let format (timestamp : float#) : string =
  let dst = Bytes.create 29 in
  let _ : int16# = write_http_date dst ~off:(Buf_write.i16 0) timestamp in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
;;

let[@inline] write_date_field dst ~off prefix (timestamp : float#) =
  let off = Buf_write.string dst ~off prefix in
  let off = write_http_date dst ~off timestamp in
  Buf_write.crlf dst ~off
;;

let write_date_header dst ~off (timestamp : float#) =
  write_date_field dst ~off "Date: " timestamp
;;

let write_last_modified dst ~off (timestamp : float#) =
  write_date_field dst ~off "Last-Modified: " timestamp
;;

let write_expires dst ~off (timestamp : float#) =
  write_date_field dst ~off "Expires: " timestamp
;;

let is_modified_since ~(last_modified : float#) ~(if_modified_since : float#) =
  F64.compare last_modified if_modified_since > 0
;;

let is_unmodified_since ~(last_modified : float#) ~(if_unmodified_since : float#) =
  F64.compare last_modified if_unmodified_since <= 0
;;
