(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Vcard_result

let is_digit c = c >= '0' && c <= '9'

let digits s i n =
  if i + n <= String.length s && String.for_all is_digit (String.sub s i n) then
    Some (int_of_string (String.sub s i n))
  else None

let pad2 n = Printf.sprintf "%02d" n

type zone = Utc | Offset of int

module Utc_offset = struct
  type t = int

  let of_string s =
    let n = String.length s in
    let* sign =
      if n <> 3 && n <> 5 then
        error
          "%S is not a UTC offset, which is a sign, two hour digits and two \
           optional minute digits"
          s
      else
        match s.[0] with
        | '+' -> Ok 1
        | '-' -> Ok (-1)
        | _ -> error "%S is not a UTC offset, which starts with a sign" s
    in
    match (digits s 1 2, if n = 5 then digits s 3 2 else Some 0) with
    | Some h, Some m when h <= 23 && m <= 59 -> Ok (sign * ((h * 60) + m))
    | _ -> error "%S is not a UTC offset" s

  let to_string t =
    let sign = if t < 0 then '-' else '+' in
    let t = abs t in
    Printf.sprintf "%c%s%s" sign (pad2 (t / 60)) (pad2 (t mod 60))

  let equal = Int.equal
  let compare = Int.compare
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

let zone_of_string s =
  if s = "" then Ok None
  else if s = "Z" then Ok (Some Utc)
  else Result.map (fun o -> Some (Offset o)) (Utc_offset.of_string s)

let zone_to_string = function
  | None -> ""
  | Some Utc -> "Z"
  | Some (Offset o) -> Utc_offset.to_string o

let equal_zone a b =
  match (a, b) with
  | None, None | Some Utc, Some Utc -> true
  | Some (Offset a), Some (Offset b) -> Int.equal a b
  | _ -> false

(* The zone is the suffix that starts at the first "Z", "+" or "-" after the
   leading truncation hyphens. *)
let split_zone s =
  let n = String.length s in
  let rec first_digit i =
    if i < n && s.[i] = '-' then first_digit (i + 1) else i
  in
  let rec go i =
    if i >= n then (s, "")
    else
      match s.[i] with
      | 'Z' | '+' | '-' -> (String.sub s 0 i, String.sub s i (n - i))
      | _ -> go (i + 1)
  in
  go (first_digit 0)

module Cal_date = struct
  type t = { year : int option; month : int option; day : int option }

  let validate d =
    let range name v hi =
      match v with
      | Some v -> check (v >= 1 && v <= hi) "%s %d is outside 1 to %d" name v hi
      | None -> Ok ()
    in
    let* () =
      match d.year with
      | Some y -> check (y >= 0 && y <= 9999) "year %d is outside 0 to 9999" y
      | None -> Ok ()
    in
    let* () = range "month" d.month 12 in
    let* () = range "day" d.day 31 in
    let* () =
      check
        (d.year <> None || d.month <> None || d.day <> None)
        "a date needs a year, a month or a day"
    in
    let* () =
      check
        (not (d.year <> None && d.month = None && d.day <> None))
        "a date with a year and a day needs a month"
    in
    Ok d

  (* RFC 6350 Section 4.3.1: date = year [month day] / year "-" month
     / "--" month [day] / "--" "-" day. The "--MM" and "YYYY" shapes are both
     four characters, so the truncated shapes are tried first. *)
  let of_string s =
    let n = String.length s in
    let some year month day = Some { year; month; day } in
    let shape =
      if String.starts_with ~prefix:"---" s && n = 5 then
        Option.bind (digits s 3 2) (fun d -> some None None (Some d))
      else if String.starts_with ~prefix:"--" s && n = 6 then
        match (digits s 2 2, digits s 4 2) with
        | Some m, Some d -> some None (Some m) (Some d)
        | _ -> None
      else if String.starts_with ~prefix:"--" s && n = 4 then
        Option.bind (digits s 2 2) (fun m -> some None (Some m) None)
      else if n = 8 then
        match (digits s 0 4, digits s 4 2, digits s 6 2) with
        | Some y, Some m, Some d -> some (Some y) (Some m) (Some d)
        | _ -> None
      else if n = 7 && s.[4] = '-' then
        match (digits s 0 4, digits s 5 2) with
        | Some y, Some m -> some (Some y) (Some m) None
        | _ -> None
      else if n = 4 then
        Option.bind (digits s 0 4) (fun y -> some (Some y) None None)
      else None
    in
    match shape with None -> error "%S is not a date" s | Some d -> validate d

  let to_string d =
    match validate d with
    | Error _ -> ""
    | Ok _ -> (
        match (d.year, d.month, d.day) with
        | Some y, Some m, Some x ->
            Printf.sprintf "%04d%s%s" y (pad2 m) (pad2 x)
        | Some y, Some m, None -> Printf.sprintf "%04d-%s" y (pad2 m)
        | Some y, None, None -> Printf.sprintf "%04d" y
        | None, Some m, Some x -> Printf.sprintf "--%s%s" (pad2 m) (pad2 x)
        | None, Some m, None -> Printf.sprintf "--%s" (pad2 m)
        | None, None, Some x -> Printf.sprintf "---%s" (pad2 x)
        | Some _, None, Some _ | None, None, None -> "")

  let is_complete d = d.year <> None && d.month <> None && d.day <> None

  let equal a b =
    Option.equal Int.equal a.year b.year
    && Option.equal Int.equal a.month b.month
    && Option.equal Int.equal a.day b.day

  let pp ppf d = Format.pp_print_string ppf (to_string d)
end

module Time = struct
  type t = {
    hour : int option;
    minute : int option;
    second : int option;
    zone : zone option;
  }

  let validate t =
    let range name v hi =
      match v with
      | Some v -> check (v >= 0 && v <= hi) "%s %d is outside 0 to %d" name v hi
      | None -> Ok ()
    in
    let* () = range "hour" t.hour 23 in
    let* () = range "minute" t.minute 59 in
    let* () = range "second" t.second 60 in
    let* () =
      check
        (t.hour <> None || t.minute <> None || t.second <> None)
        "a time needs an hour, a minute or a second"
    in
    let* () =
      check
        (not (t.hour <> None && t.minute = None && t.second <> None))
        "a time with an hour and a second needs a minute"
    in
    Ok t

  (* RFC 6350 Section 4.3.2: time = hour [minute [second]] [zone]
     / "-" minute [second] [zone] / "-" "-" second [zone]. *)
  let of_string s =
    let body, zone = split_zone s in
    let* zone = zone_of_string zone in
    let n = String.length body in
    let two i = digits body i 2 in
    let some hour minute second = Some { hour; minute; second; zone } in
    let shape =
      if String.starts_with ~prefix:"--" body && n = 4 then
        Option.bind (two 2) (fun sec -> some None None (Some sec))
      else if String.starts_with ~prefix:"-" body && n = 3 then
        Option.bind (two 1) (fun m -> some None (Some m) None)
      else if String.starts_with ~prefix:"-" body && n = 5 then
        match (two 1, two 3) with
        | Some m, Some sec -> some None (Some m) (Some sec)
        | _ -> None
      else if n = 2 then Option.bind (two 0) (fun h -> some (Some h) None None)
      else if n = 4 then
        match (two 0, two 2) with
        | Some h, Some m -> some (Some h) (Some m) None
        | _ -> None
      else if n = 6 then
        match (two 0, two 2, two 4) with
        | Some h, Some m, Some sec -> some (Some h) (Some m) (Some sec)
        | _ -> None
      else None
    in
    match shape with None -> error "%S is not a time" s | Some t -> validate t

  let to_string t =
    let z = zone_to_string t.zone in
    match validate t with
    | Error _ -> z
    | Ok _ -> (
        match (t.hour, t.minute, t.second) with
        | Some h, Some m, Some sec -> pad2 h ^ pad2 m ^ pad2 sec ^ z
        | Some h, Some m, None -> pad2 h ^ pad2 m ^ z
        | Some h, None, None -> pad2 h ^ z
        | None, Some m, Some sec -> "-" ^ pad2 m ^ pad2 sec ^ z
        | None, Some m, None -> "-" ^ pad2 m ^ z
        | None, None, Some sec -> "--" ^ pad2 sec ^ z
        | Some _, None, Some _ | None, None, None -> z)

  let is_complete t = t.hour <> None && t.minute <> None && t.second <> None

  let equal a b =
    Option.equal Int.equal a.hour b.hour
    && Option.equal Int.equal a.minute b.minute
    && Option.equal Int.equal a.second b.second
    && equal_zone a.zone b.zone

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Date_time = struct
  type t = { date : Cal_date.t; time : Time.t }

  (* RFC 6350 Section 4.3.3: date-time = date-noreduc "T" time-notrunc, so the
     date has a day and the time an hour. *)
  let of_string s =
    match String.index_opt s 'T' with
    | None -> error "%S is not a date-time, which holds a \"T\"" s
    | Some i ->
        let* date = Cal_date.of_string (String.sub s 0 i) in
        let* time =
          Time.of_string (String.sub s (i + 1) (String.length s - i - 1))
        in
        let* () =
          check
            (date.Cal_date.day <> None)
            "%S is not a date-time, its date has no day" s
        in
        let* () =
          check (time.Time.hour <> None)
            "%S is not a date-time, its time has no hour" s
        in
        Ok { date; time }

  let to_string t = Cal_date.to_string t.date ^ "T" ^ Time.to_string t.time
  let is_complete t = Cal_date.is_complete t.date && Time.is_complete t.time

  let validate t =
    let* _ = Cal_date.validate t.date in
    let* _ = Time.validate t.time in
    let* () =
      check (t.date.Cal_date.day <> None) "a date-time's date has no day"
    in
    let* () =
      check (t.time.Time.hour <> None) "a date-time's time has no hour"
    in
    Ok t

  let equal a b = Cal_date.equal a.date b.date && Time.equal a.time b.time
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Timestamp = struct
  type t = {
    year : int;
    month : int;
    day : int;
    hour : int;
    minute : int;
    second : int;
    zone : zone option;
  }

  let of_string s =
    let* dt = Date_time.of_string s in
    match (dt.date, dt.time) with
    | ( { year = Some year; month = Some month; day = Some day },
        { hour = Some hour; minute = Some minute; second = Some second; zone } )
      ->
        Ok { year; month; day; hour; minute; second; zone }
    | _ -> error "%S is not a timestamp, which is a complete date and time" s

  let to_string t =
    Printf.sprintf "%04d%s%sT%s%s%s%s" t.year (pad2 t.month) (pad2 t.day)
      (pad2 t.hour) (pad2 t.minute) (pad2 t.second) (zone_to_string t.zone)

  let equal a b =
    a.year = b.year && a.month = b.month && a.day = b.day && a.hour = b.hour
    && a.minute = b.minute && a.second = b.second && equal_zone a.zone b.zone

  let zone_seconds zone = function
    | None -> ( match zone with Utc -> 0 | Offset m -> m * 60)
    | Some Utc -> 0
    | Some (Offset m) -> m * 60

  let to_ptime ?(zone = Utc) t =
    Ptime.of_date_time
      ( (t.year, t.month, t.day),
        ((t.hour, t.minute, t.second), zone_seconds zone t.zone) )

  (* The fallback key must distinguish exactly what [equal] distinguishes, so
     an absent zone, [Utc] and an [Offset] stay apart. *)
  let zone_key = function
    | None -> (0, 0)
    | Some Utc -> (1, 0)
    | Some (Offset m) -> (2, m)

  (* Timestamps order by the instant they denote, with a local time read in
     UTC, and a timestamp Ptime cannot represent sorts by its parts. *)
  let compare a b =
    match (to_ptime a, to_ptime b) with
    | Some a, Some b -> Ptime.compare a b
    | _ ->
        Stdlib.compare
          (a.year, a.month, a.day, a.hour, a.minute, a.second, zone_key a.zone)
          (b.year, b.month, b.day, b.hour, b.minute, b.second, zone_key b.zone)

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let of_ptime ?(zone = Utc) p =
    let tz = match zone with Utc -> 0 | Offset minutes -> minutes * 60 in
    let (year, month, day), ((hour, minute, second), _) =
      Ptime.to_date_time ~tz_offset_s:tz p
    in
    { year; month; day; hour; minute; second; zone = Some zone }
end

type t = Date of Cal_date.t | Time of Time.t | Date_time of Date_time.t

(* RFC 6350 Section 4.3.4: a stand-alone time is preceded by "T". *)
let of_string s =
  if String.starts_with ~prefix:"T" s then
    Result.map
      (fun t -> Time t)
      (Time.of_string (String.sub s 1 (String.length s - 1)))
  else if String.contains s 'T' then
    Result.map (fun t -> Date_time t) (Date_time.of_string s)
  else Result.map (fun d -> Date d) (Cal_date.of_string s)

let to_string = function
  | Date d -> Cal_date.to_string d
  | Time t -> "T" ^ Time.to_string t
  | Date_time dt -> Date_time.to_string dt

let equal a b =
  match (a, b) with
  | Date a, Date b -> Cal_date.equal a b
  | Time a, Time b -> Time.equal a b
  | Date_time a, Date_time b -> Date_time.equal a b
  | _ -> false

let pp ppf t = Format.pp_print_string ppf (to_string t)
