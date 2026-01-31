(* date.ml - HTTP-date parsing and formatting per RFC 7231 Section 7.1.1.1 *)

open Base

module F64 = Stdlib_upstream_compatible.Float_u

let[@inline always] f64 x = F64.of_float x
let[@inline always] to_float x = F64.to_float x

type status =
  | Valid
  | Invalid

(* Day and month names for parsing and formatting *)
let day_names = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let month_names = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(* Parse 2-digit number at position, returns (value, valid) *)
let[@inline] parse_2digit buf pos =
  let c0 = Bytes.unsafe_get buf pos in
  let c1 = Bytes.unsafe_get buf (pos + 1) in
  if Char.is_digit c0 && Char.is_digit c1 then
    #((Char.to_int c0 - 48) * 10 + (Char.to_int c1 - 48), true)
  else
    #(0, false)
;;

(* Parse 4-digit year at position, returns (value, valid) *)
let[@inline] parse_4digit buf pos =
  let c0 = Bytes.unsafe_get buf pos in
  let c1 = Bytes.unsafe_get buf (pos + 1) in
  let c2 = Bytes.unsafe_get buf (pos + 2) in
  let c3 = Bytes.unsafe_get buf (pos + 3) in
  if Char.is_digit c0 && Char.is_digit c1 && Char.is_digit c2 && Char.is_digit c3 then
    #((Char.to_int c0 - 48) * 1000 + (Char.to_int c1 - 48) * 100 +
      (Char.to_int c2 - 48) * 10 + (Char.to_int c3 - 48), true)
  else
    #(0, false)
;;

(* Parse 1 or 2 digit day, returns (day, next_pos, valid) *)
let[@inline] parse_day buf pos len =
  if pos >= len then #(0, pos, false)
  else
    let c0 = Bytes.unsafe_get buf pos in
    if Char.equal c0 ' ' && pos + 1 < len then
      (* Space-padded single digit *)
      let c1 = Bytes.unsafe_get buf (pos + 1) in
      if Char.is_digit c1 then #(Char.to_int c1 - 48, pos + 2, true)
      else #(0, pos, false)
    else if Char.is_digit c0 && pos + 1 < len then
      let c1 = Bytes.unsafe_get buf (pos + 1) in
      if Char.is_digit c1 then
        #((Char.to_int c0 - 48) * 10 + (Char.to_int c1 - 48), pos + 2, true)
      else
        #(Char.to_int c0 - 48, pos + 1, true)
    else
      #(0, pos, false)
;;

(* Parse 3-letter month abbreviation, returns 0-11 or -1 *)
let[@inline] parse_month buf pos =
  let c0 = Bytes.unsafe_get buf pos in
  let c1 = Bytes.unsafe_get buf (pos + 1) in
  let c2 = Bytes.unsafe_get buf (pos + 2) in
  match (c0, c1, c2) with
  | ('J', 'a', 'n') -> 0
  | ('F', 'e', 'b') -> 1
  | ('M', 'a', 'r') -> 2
  | ('A', 'p', 'r') -> 3
  | ('M', 'a', 'y') -> 4
  | ('J', 'u', 'n') -> 5
  | ('J', 'u', 'l') -> 6
  | ('A', 'u', 'g') -> 7
  | ('S', 'e', 'p') -> 8
  | ('O', 'c', 't') -> 9
  | ('N', 'o', 'v') -> 10
  | ('D', 'e', 'c') -> 11
  | _ -> -1
;;

(* Parse time HH:MM:SS at position, returns (hour, minute, second, valid) *)
let[@inline] parse_time buf pos =
  let #(hour, h_valid) = parse_2digit buf pos in
  if not h_valid then #(0, 0, 0, false)
  else if not (Char.equal (Bytes.unsafe_get buf (pos + 2)) ':') then #(0, 0, 0, false)
  else
    let #(minute, m_valid) = parse_2digit buf (pos + 3) in
    if not m_valid then #(0, 0, 0, false)
    else if not (Char.equal (Bytes.unsafe_get buf (pos + 5)) ':') then #(0, 0, 0, false)
    else
      let #(second, s_valid) = parse_2digit buf (pos + 6) in
      if not s_valid then #(0, 0, 0, false)
      else if hour > 23 || minute > 59 || second > 60 then #(0, 0, 0, false)  (* 60 for leap second *)
      else #(hour, minute, second, true)
;;

(* Days in each month (non-leap year) *)
let days_in_month = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

(* Check if year is leap year *)
let[@inline] is_leap_year year =
  (year % 4 = 0 && year % 100 <> 0) || (year % 400 = 0)
;;

(* Convert date components to Unix timestamp, returns (timestamp, valid) *)
let to_timestamp ~year ~month ~day ~hour ~minute ~second =
  (* Validate ranges *)
  if year < 1970 || month < 0 || month > 11 then #(f64 0.0, false)
  else
    let max_day =
      if month = 1 && is_leap_year year then 29
      else days_in_month.(month)
    in
    if day < 1 || day > max_day then #(f64 0.0, false)
    else
      (* Calculate days since epoch *)
      let mutable days = 0 in
      (* Add days for complete years *)
      for y = 1970 to year - 1 do
        days <- days + (if is_leap_year y then 366 else 365)
      done;
      (* Add days for complete months in current year *)
      for m = 0 to month - 1 do
        days <- days + days_in_month.(m);
        if m = 1 && is_leap_year year then days <- days + 1
      done;
      (* Add days in current month *)
      days <- days + (day - 1);
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
    while comma_pos < off + 4 && not (Char.equal (Bytes.unsafe_get buf comma_pos) ',') do
      comma_pos <- comma_pos + 1
    done;
    if comma_pos >= off + len || not (Char.equal (Bytes.unsafe_get buf comma_pos) ',') then invalid_result
    else if not (Char.equal (Bytes.unsafe_get buf (comma_pos + 1)) ' ') then invalid_result
    else
      let day_pos = comma_pos + 2 in
      let #(day, day_valid) = parse_2digit buf day_pos in
      if not day_valid then invalid_result
      else if not (Char.equal (Bytes.unsafe_get buf (day_pos + 2)) ' ') then invalid_result
      else
        let month = parse_month buf (day_pos + 3) in
        if month < 0 then invalid_result
        else if not (Char.equal (Bytes.unsafe_get buf (day_pos + 6)) ' ') then invalid_result
        else
          let #(year, year_valid) = parse_4digit buf (day_pos + 7) in
          if not year_valid then invalid_result
          else if not (Char.equal (Bytes.unsafe_get buf (day_pos + 11)) ' ') then invalid_result
          else
            let #(hour, minute, second, time_valid) = parse_time buf (day_pos + 12) in
            if not time_valid then invalid_result
            else
              (* Check for " GMT" at end *)
              let gmt_pos = day_pos + 20 in
              if gmt_pos + 4 > off + len then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf gmt_pos) ' ') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 1)) 'G') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 2)) 'M') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 3)) 'T') then invalid_result
              else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Parse RFC 850 date: Sunday, 06-Nov-94 08:49:37 GMT *)
let parse_rfc850 buf off len =
  (* Find comma after full day name *)
  let mutable comma_pos = off in
  while comma_pos < off + 10 && not (Char.equal (Bytes.unsafe_get buf comma_pos) ',') do
    comma_pos <- comma_pos + 1
  done;
  if comma_pos >= off + len || not (Char.equal (Bytes.unsafe_get buf comma_pos) ',') then invalid_result
  else if not (Char.equal (Bytes.unsafe_get buf (comma_pos + 1)) ' ') then invalid_result
  else
    let pos = comma_pos + 2 in
    let #(day, day_valid) = parse_2digit buf pos in
    if not day_valid then invalid_result
    else if not (Char.equal (Bytes.unsafe_get buf (pos + 2)) '-') then invalid_result
    else
      let month = parse_month buf (pos + 3) in
      if month < 0 then invalid_result
      else if not (Char.equal (Bytes.unsafe_get buf (pos + 6)) '-') then invalid_result
      else
        let #(year2, year2_valid) = parse_2digit buf (pos + 7) in
        if not year2_valid then invalid_result
        else
          (* RFC 850 uses 2-digit year. Interpret 00-99 as 2000-2099 for dates >= 70,
             and 1970-1999 for dates < 70. Modern interpretation varies. *)
          let year = if year2 >= 70 then 1900 + year2 else 2000 + year2 in
          if not (Char.equal (Bytes.unsafe_get buf (pos + 9)) ' ') then invalid_result
          else
            let #(hour, minute, second, time_valid) = parse_time buf (pos + 10) in
            if not time_valid then invalid_result
            else
              (* Check for " GMT" *)
              let gmt_pos = pos + 18 in
              if gmt_pos + 4 > off + len then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf gmt_pos) ' ') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 1)) 'G') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 2)) 'M') then invalid_result
              else if not (Char.equal (Bytes.unsafe_get buf (gmt_pos + 3)) 'T') then invalid_result
              else to_timestamp ~year ~month ~day ~hour ~minute ~second
;;

(* Parse asctime format: Sun Nov  6 08:49:37 1994 *)
let parse_asctime buf off len =
  (* Minimum length: "Sun Nov  6 08:49:37 1994" = 24 chars *)
  if len < 24 then invalid_result
  (* Skip 3-char day name and space *)
  else if not (Char.equal (Bytes.unsafe_get buf (off + 3)) ' ') then invalid_result
  else
    let month = parse_month buf (off + 4) in
    if month < 0 then invalid_result
    else if not (Char.equal (Bytes.unsafe_get buf (off + 7)) ' ') then invalid_result
    else
      let #(day, next_pos, day_valid) = parse_day buf (off + 8) len in
      if not day_valid then invalid_result
      else if not (Char.equal (Bytes.unsafe_get buf next_pos) ' ') then invalid_result
      else
        let #(hour, minute, second, time_valid) = parse_time buf (next_pos + 1) in
        if not time_valid then invalid_result
        else
          let year_pos = next_pos + 9 in
          if not (Char.equal (Bytes.unsafe_get buf year_pos) ' ') then invalid_result
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
    let c4 = Bytes.unsafe_get buf (off + 3) in
    let #(ts, valid) =
      if Char.equal c4 ',' then
        (* IMF-fixdate: short day name + comma *)
        parse_imf_fixdate buf off len
      else if Char.equal c4 ' ' then
        (* asctime: short day name + space *)
        parse_asctime buf off len
      else
        (* RFC 850: full day name, look for comma *)
        parse_rfc850 buf off len
    in
    if valid then #(Valid, ts) else #(Invalid, f64 0.0)
;;

(* Format timestamp as IMF-fixdate *)
let format (timestamp : float#) : string =
  (* Use Unix module to break down timestamp *)
  let tm = Unix.gmtime (to_float timestamp) in
  Stdlib.Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    day_names.(tm.Unix.tm_wday)
    tm.Unix.tm_mday
    month_names.(tm.Unix.tm_mon)
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;

(* Write HTTP-date at offset without header name *)
let write_http_date dst ~off (timestamp : float#) =
  let tm = Unix.gmtime (to_float timestamp) in
  let off = Buf_write.string dst ~off day_names.(tm.Unix.tm_wday) in
  let off = Buf_write.string dst ~off ", " in
  let off = Buf_write.digit2 dst ~off tm.Unix.tm_mday in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.string dst ~off month_names.(tm.Unix.tm_mon) in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.digit4 dst ~off (tm.Unix.tm_year + 1900) in
  let off = Buf_write.char dst ~off ' ' in
  let off = Buf_write.digit2 dst ~off tm.Unix.tm_hour in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.digit2 dst ~off tm.Unix.tm_min in
  let off = Buf_write.char dst ~off ':' in
  let off = Buf_write.digit2 dst ~off tm.Unix.tm_sec in
  Buf_write.string dst ~off " GMT"
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
