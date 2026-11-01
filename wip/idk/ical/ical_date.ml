(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let error fmt = Format.kasprintf (fun s -> Error s) fmt
let ( let* ) = Result.bind
let is_digit c = c >= '0' && c <= '9'

let digits s i n =
  if i + n <= String.length s && String.for_all is_digit (String.sub s i n) then
    Some (int_of_string (String.sub s i n))
  else None

type date = { year : int; month : int; day : int }
type time = { hour : int; minute : int; second : int; utc : bool }
type date_time = { date : date; time : time }
type t = Date of date | Date_time of date_time

(* RFC 5545 Section 3.3.4: date-value = date-fullyear date-month date-mday,
   four, two and two digits with no separators. *)
let is_leap y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

let days_in_month y = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap y then 29 else 28
  | _ -> 0

let date_of_string s =
  if String.length s <> 8 then error "%S is not a DATE" s
  else
    match (digits s 0 4, digits s 4 2, digits s 6 2) with
    | Some year, Some month, Some day
      when month >= 1 && month <= 12 && day >= 1
           && day <= days_in_month year month ->
        Ok { year; month; day }
    | _ -> error "%S is not a DATE" s

let date_to_string d = Printf.sprintf "%04d%02d%02d" d.year d.month d.day

(* RFC 5545 Section 3.3.12: time = time-hour time-minute time-second
   [time-utc], each two digits, with an optional trailing "Z". *)
let time_of_string s =
  let n = String.length s in
  let utc = n > 0 && s.[n - 1] = 'Z' in
  let body = if utc then String.sub s 0 (n - 1) else s in
  if String.length body <> 6 then error "%S is not a TIME" s
  else
    match (digits body 0 2, digits body 2 2, digits body 4 2) with
    | Some hour, Some minute, Some second
      when hour <= 23 && minute <= 59 && second <= 60 ->
        Ok { hour; minute; second; utc }
    | _ -> error "%S is not a TIME" s

let time_to_string t =
  Printf.sprintf "%02d%02d%02d%s" t.hour t.minute t.second
    (if t.utc then "Z" else "")

(* RFC 5545 Section 3.3.5: date-time = date "T" time. *)
let date_time_of_string s =
  match String.index_opt s 'T' with
  | None -> error "%S is not a DATE-TIME, it holds no \"T\"" s
  | Some i ->
      let* date = date_of_string (String.sub s 0 i) in
      let* time =
        time_of_string (String.sub s (i + 1) (String.length s - i - 1))
      in
      Ok { date; time }

let date_time_to_string dt =
  date_to_string dt.date ^ "T" ^ time_to_string dt.time

let of_string s =
  if String.contains s 'T' then
    Result.map (fun dt -> Date_time dt) (date_time_of_string s)
  else Result.map (fun d -> Date d) (date_of_string s)

let to_string = function
  | Date d -> date_to_string d
  | Date_time dt -> date_time_to_string dt

let to_ptime dt =
  Ptime.of_date_time
    ( (dt.date.year, dt.date.month, dt.date.day),
      ((dt.time.hour, dt.time.minute, dt.time.second), 0) )

let of_ptime p =
  let (year, month, day), ((hour, minute, second), _) = Ptime.to_date_time p in
  { date = { year; month; day }; time = { hour; minute; second; utc = true } }

let start_of_day d =
  { date = d; time = { hour = 0; minute = 0; second = 0; utc = false } }

(* RFC 5545 Section 3.3.14: utc-offset = ("+" / "-") time-hour time-minute
   [time-second], and "-0000" and "-000000" are not allowed. *)
let utc_offset_of_string s =
  let n = String.length s in
  if n <> 5 && n <> 7 then error "%S is not a UTC-OFFSET" s
  else
    let* sign =
      match s.[0] with
      | '+' -> Ok 1
      | '-' -> Ok (-1)
      | _ -> error "%S is not a UTC-OFFSET, it does not start with a sign" s
    in
    match
      (digits s 1 2, digits s 3 2, if n = 7 then digits s 5 2 else Some 0)
    with
    | Some h, Some m, Some sec when h <= 23 && m <= 59 && sec <= 59 ->
        let total = sign * ((h * 3600) + (m * 60) + sec) in
        if sign < 0 && total = 0 then
          error "%S is not a UTC-OFFSET, \"-0000\" is not allowed" s
        else Ok total
    | _ -> error "%S is not a UTC-OFFSET" s

let utc_offset_to_string n =
  let sign = if n < 0 then '-' else '+' in
  let n = abs n in
  let h = n / 3600 and m = n mod 3600 / 60 and sec = n mod 60 in
  if sec = 0 then Printf.sprintf "%c%02d%02d" sign h m
  else Printf.sprintf "%c%02d%02d%02d" sign h m sec

let instant = function
  | Date d -> to_ptime (start_of_day d)
  | Date_time dt -> to_ptime dt

(* [to_ptime] reads a floating time in UTC, so two values at the same instant
   may still differ by their [utc] flag or by being a date rather than a
   date-time. The tie-break keeps [compare a b = 0] and [equal a b]
   agreeing. *)
let key = function
  | Date d -> (0, d.year, d.month, d.day, 0, 0, 0, false)
  | Date_time dt ->
      ( 1,
        dt.date.year,
        dt.date.month,
        dt.date.day,
        dt.time.hour,
        dt.time.minute,
        dt.time.second,
        dt.time.utc )

let compare a b =
  match (instant a, instant b) with
  | Some pa, Some pb ->
      let c = Ptime.compare pa pb in
      if c <> 0 then c else Stdlib.compare (key a) (key b)
  | _ -> Stdlib.compare (key a) (key b)

let equal_date a b = a.year = b.year && a.month = b.month && a.day = b.day

let equal_time a b =
  a.hour = b.hour && a.minute = b.minute && a.second = b.second
  && Bool.equal a.utc b.utc

let equal_date_time a b = equal_date a.date b.date && equal_time a.time b.time

let compare_date_time a b =
  let key dt =
    ( dt.date.year,
      dt.date.month,
      dt.date.day,
      dt.time.hour,
      dt.time.minute,
      dt.time.second,
      dt.time.utc )
  in
  match (to_ptime a, to_ptime b) with
  | Some pa, Some pb ->
      let c = Ptime.compare pa pb in
      if c <> 0 then c else Stdlib.compare (key a) (key b)
  | _ -> Stdlib.compare (key a) (key b)

let equal a b =
  match (a, b) with
  | Date a, Date b -> equal_date a b
  | Date_time a, Date_time b -> equal_date_time a b
  | _ -> false

let pp ppf t = Format.pp_print_string ppf (to_string t)
