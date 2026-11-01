(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let error fmt = Format.kasprintf (fun s -> Error s) fmt
let is_digit c = c >= '0' && c <= '9'

type t = {
  negative : bool;
  weeks : int;
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

let zero =
  { negative = false; weeks = 0; days = 0; hours = 0; minutes = 0; seconds = 0 }

(* The largest value a component may hold. A week count of this magnitude is
   already 1.9e9 years, and it keeps [to_seconds] inside an [int]. *)
let max_component = 1_000_000_000

(* RFC 5545 Section 3.3.6: dur-value = (["+"] / "-") "P"
   (dur-date / dur-time / dur-week), dur-date = dur-day [dur-time],
   dur-time = "T" (dur-hour / dur-minute / dur-second). *)
let of_string s =
  let n = String.length s in
  let negative, i =
    if n > 0 && s.[0] = '-' then (true, 1)
    else if n > 0 && s.[0] = '+' then (false, 1)
    else (false, 0)
  in
  let fail () = error "%S is not a DURATION" s in
  if i >= n || s.[i] <> 'P' then fail ()
  else
    let i = i + 1 in
    let number j =
      let k = ref j in
      while !k < n && is_digit s.[!k] do
        incr k
      done;
      if !k = j then None
      else
        (* A digit run long enough to overflow [to_seconds] is refused here, so
           that [to_seconds] stays total and [int_of_string] cannot raise. *)
        match int_of_string_opt (String.sub s j (!k - j)) with
        | Some v when v <= max_component -> Some (v, !k)
        | _ -> None
    in
    let part j c =
      match number j with
      | Some (v, k) when k < n && s.[k] = c -> Some (v, k + 1)
      | _ -> None
    in
    match part i 'W' with
    | Some (weeks, j) when j = n ->
        Ok { negative; weeks; days = 0; hours = 0; minutes = 0; seconds = 0 }
    | _ ->
        let days, i, has_day =
          match part i 'D' with
          | Some (d, j) -> (d, j, true)
          | None -> (0, i, false)
        in
        if i < n && s.[i] = 'T' then
          let i = i + 1 in
          let hours, i, has_h =
            match part i 'H' with
            | Some (h, j) -> (h, j, true)
            | None -> (0, i, false)
          in
          let minutes, i, has_m =
            match part i 'M' with
            | Some (m, j) -> (m, j, true)
            | None -> (0, i, false)
          in
          let seconds, i, has_s =
            match part i 'S' with
            | Some (sv, j) -> (sv, j, true)
            | None -> (0, i, false)
          in
          if i <> n || not (has_h || has_m || has_s) then fail ()
          else Ok { negative; weeks = 0; days; hours; minutes; seconds }
        else if i = n && has_day then
          Ok { negative; weeks = 0; days; hours = 0; minutes = 0; seconds = 0 }
        else fail ()

let time_part hours minutes seconds =
  if hours = 0 && minutes = 0 && seconds = 0 then ""
  else
    let need_m = minutes <> 0 || (hours <> 0 && seconds <> 0) in
    let h = if hours <> 0 then Printf.sprintf "%dH" hours else "" in
    let m = if need_m then Printf.sprintf "%dM" minutes else "" in
    let sec = if seconds <> 0 then Printf.sprintf "%dS" seconds else "" in
    "T" ^ h ^ m ^ sec

let to_string d =
  let sign = if d.negative then "-" else "" in
  if d.weeks <> 0 then Printf.sprintf "%sP%dW" sign d.weeks
  else if d.days = 0 && d.hours = 0 && d.minutes = 0 && d.seconds = 0 then
    sign ^ "PT0S"
  else
    let days_part = if d.days <> 0 then Printf.sprintf "%dD" d.days else "" in
    sign ^ "P" ^ days_part ^ time_part d.hours d.minutes d.seconds

let to_seconds d =
  let mag =
    (d.weeks * 7 * 86400)
    + (d.days * 86400) + (d.hours * 3600) + (d.minutes * 60) + d.seconds
  in
  if d.negative then -mag else mag

let of_seconds n =
  let negative = n < 0 in
  (* [abs min_int] is [min_int], which would leave every part negative. *)
  let n = if n = min_int then max_int else abs n in
  let days = n / 86400 in
  let rem = n mod 86400 in
  let hours = rem / 3600 in
  let rem = rem mod 3600 in
  let minutes = rem / 60 in
  let seconds = rem mod 60 in
  { negative; weeks = 0; days; hours; minutes; seconds }

let add dt d =
  match Ical_date.to_ptime dt with
  | None -> dt
  | Some p -> (
      match Ptime.add_span p (Ptime.Span.of_int_s (to_seconds d)) with
      | None -> dt
      | Some p' ->
          let (year, month, day), ((hour, minute, second), _) =
            Ptime.to_date_time p'
          in
          {
            Ical_date.date = { year; month; day };
            time = { hour; minute; second; utc = dt.time.utc };
          })

let equal a b =
  Bool.equal a.negative b.negative
  && a.weeks = b.weeks && a.days = b.days && a.hours = b.hours
  && a.minutes = b.minutes && a.seconds = b.seconds

let pp ppf d = Format.pp_print_string ppf (to_string d)
