(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let num s i n = int_of_string_opt (String.sub s i n)

(* The arithmetic is [Schedule.utc]'s rather than another copy of it, so a date
   read here and a due time computed there agree by construction. *)
let instant ~year ~month ~day ~hour ~minute ~second =
  Schedule.utc.Schedule.instant { Schedule.year; month; day } ~hour ~minute
  +. float_of_int second

let of_date s =
  if String.length s <> 10 || s.[4] <> '-' || s.[7] <> '-' then None
  else
    match (num s 0 4, num s 5 2, num s 8 2) with
    | Some year, Some month, Some day ->
        Some (instant ~year ~month ~day ~hour:0 ~minute:0 ~second:0)
    | _ -> None

let of_rfc3339 s =
  if
    String.length s <> 20
    || s.[10] <> 'T'
    || s.[13] <> ':'
    || s.[16] <> ':'
    || s.[19] <> 'Z'
  then None
  else
    match (of_date (String.sub s 0 10), num s 11 2, num s 14 2, num s 17 2) with
    | Some day, Some hour, Some minute, Some second ->
        Some (day +. float_of_int ((hour * 3600) + (minute * 60) + second))
    | _ -> None

let of_since s =
  match of_rfc3339 s with
  | Some t -> Ok t
  | None -> (
      match of_date s with
      | Some t -> Ok t
      | None -> (
          match Schedule.duration_of_string s with
          | Ok secs -> Ok (Unix.gettimeofday () -. float_of_int secs)
          | Error _ ->
              Error
                (Printf.sprintf
                   "%S is not a time. Write a whole timestamp such as \
                    2026-08-08T09:14:07Z, a date such as 2026-08-08, which is \
                    midnight UTC on it, or how far back to go such as 2h or \
                    7d."
                   s)))
