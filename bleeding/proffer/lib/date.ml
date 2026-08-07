(* IMF-fixdate, the only HTTP date format this library writes or reads.

   RFC 9110 requires senders to use it and every browser does, so the obsolete
   RFC 850 and asctime forms are not accepted. A date that does not parse makes
   the conditional request that carried it ineffective, which is the safe
   direction: the client gets the full response.

   Month and weekday names come from match expressions rather than arrays. A
   module-level array is mutable data and reads as contended from portable
   code, which would make every caller nonportable. *)

let month_name m =
  match m with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | _ -> "Dec"

let month_of_name s =
  match s with
  | "Jan" -> Some 1
  | "Feb" -> Some 2
  | "Mar" -> Some 3
  | "Apr" -> Some 4
  | "May" -> Some 5
  | "Jun" -> Some 6
  | "Jul" -> Some 7
  | "Aug" -> Some 8
  | "Sep" -> Some 9
  | "Oct" -> Some 10
  | "Nov" -> Some 11
  | "Dec" -> Some 12
  | _ -> None

let weekday_name d =
  match d with
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | _ -> "Sat"

(* Days between the civil date and 1970-01-01, and its inverse. Both are
   Howard Hinnant's era-based algorithms, valid for any year the int range
   covers and free of the leap-year special cases a table would need. *)

let days_from_civil ~y ~m ~d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

let to_imf t =
  let secs = int_of_float (Float.floor t) in
  let days = if secs >= 0 then secs / 86400 else ((secs + 1) / 86400) - 1 in
  let rem = secs - (days * 86400) in
  let y, m, d = civil_from_days days in
  let dow = (((days + 4) mod 7) + 7) mod 7 in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" (weekday_name dow) d
    (month_name m) y (rem / 3600) (rem mod 3600 / 60) (rem mod 60)

let int_at s off len =
  let sub = String.sub s off len in
  if String.for_all (fun c -> c >= '0' && c <= '9') sub then
    int_of_string_opt sub
  else None

(* [of_imf s] is the epoch seconds of an IMF-fixdate, or [None] when [s] is not
   one. The layout is fixed width, so the fields are read at known offsets:
   "Sun, 06 Nov 1994 08:49:37 GMT". *)
let of_imf s =
  if String.length s <> 29 then None
  else if
    s.[3] <> ',' || s.[4] <> ' ' || s.[7] <> ' ' || s.[11] <> ' '
    || s.[16] <> ' ' || s.[19] <> ':' || s.[22] <> ':'
    || String.sub s 25 4 <> " GMT"
  then None
  else
    match
      ( int_at s 5 2,
        month_of_name (String.sub s 8 3),
        int_at s 12 4,
        int_at s 17 2,
        int_at s 20 2,
        int_at s 23 2 )
    with
    | Some d, Some m, Some y, Some hh, Some mm, Some ss ->
        if d < 1 || d > 31 || hh > 23 || mm > 59 || ss > 60 then None
        else
          let days = days_from_civil ~y ~m ~d in
          Some
            (float_of_int ((days * 86400) + (hh * 3600) + (mm * 60) + ss))
    | _ -> None
