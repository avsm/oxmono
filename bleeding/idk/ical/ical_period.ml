(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  start : Ical_date.date_time;
  ends : [ `End of Ical_date.date_time | `Duration of Ical_duration.t ];
}

let error fmt = Printf.ksprintf (fun m -> Error m) fmt

(* RFC 5545 Section 3.3.9: t = period-explicit / period-start,
   period-explicit = date-time "/" date-time,
   period-start = date-time "/" dur-value. *)
let of_string s =
  match String.index_opt s '/' with
  | None -> error "%S is not a PERIOD, it holds no \"/\"" s
  | Some i ->
      let ( let* ) = Result.bind in
      let* start = Ical_date.date_time_of_string (String.sub s 0 i) in
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      if
        String.length rest > 0
        && (rest.[0] = 'P' || rest.[0] = '-' || rest.[0] = '+')
      then
        let* dur = Ical_duration.of_string rest in
        (* RFC 5545 Section 3.3.9: the second form is "a start and a positive
           duration of time", so a period may not run backwards. *)
        if dur.Ical_duration.negative then
          error "%S is not a PERIOD, its duration is negative" s
        else Ok { start; ends = `Duration dur }
      else
        let* fin = Ical_date.date_time_of_string rest in
        Ok { start; ends = `End fin }

let to_string p =
  Ical_date.date_time_to_string p.start
  ^ "/"
  ^
  match p.ends with
  | `End dt -> Ical_date.date_time_to_string dt
  | `Duration d -> Ical_duration.to_string d

let finish p =
  match p.ends with `End dt -> dt | `Duration d -> Ical_duration.add p.start d

let equal (a : t) (b : t) = a = b
let pp ppf p = Format.pp_print_string ppf (to_string p)
