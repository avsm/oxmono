(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Common types and Jsont codecs for Bushel *)

(** {1 Date Types} *)

type date = Ptime.date
(** A calendar date (year, month, day). *)

(** {1 Jsont Codecs} *)

let ptime_date_jsont : Ptime.date Jsont.t =
  let dec s =
    try
      match String.split_on_char '-' s with
      | [y; m; d] ->
        let year = int_of_string y in
        let month = int_of_string m in
        let day = int_of_string d in
        Ok (year, month, day)
      | _ ->
        Error (Printf.sprintf "Invalid date format: %s (expected YYYY-MM-DD)" s)
    with _ ->
      Error (Printf.sprintf "Invalid date: %s" s)
  in
  let enc (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d in
  Jsont.of_of_string ~kind:"Ptime.date" dec ~enc

let ptime_jsont : Ptime.t Jsont.t =
  let dec s =
    (* Try RFC3339 first *)
    match Ptime.of_rfc3339 s with
    | Ok (t, _, _) -> Ok t
    | Error _ ->
      (* Try date-only format *)
      try
        match String.split_on_char '-' s with
        | [y; m; d] ->
          let year = int_of_string y in
          let month = int_of_string m in
          let day = int_of_string d in
          (match Ptime.of_date (year, month, day) with
           | Some t -> Ok t
           | None -> Error (Printf.sprintf "Invalid date: %s" s))
        | _ ->
          Error (Printf.sprintf "Invalid timestamp: %s" s)
      with _ ->
        Error (Printf.sprintf "Invalid timestamp: %s" s)
  in
  let enc t =
    let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time t in
    if hh = 0 && mm = 0 && ss = 0 then
      Printf.sprintf "%04d-%02d-%02d" y m d
    else
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss
  in
  Jsont.of_of_string ~kind:"Ptime.t" dec ~enc

let ptime_option_jsont : Ptime.t option Jsont.t =
  let null = Jsont.null None in
  let some = Jsont.map ~dec:(fun t -> Some t) ~enc:(function Some t -> t | None -> assert false) ptime_jsont in
  Jsont.any ~dec_null:null ~dec_string:some ~enc:(function None -> null | Some _ -> some) ()

let string_option_jsont : string option Jsont.t =
  Jsont.option Jsont.string

(** {1 Helper Functions} *)

let ptime_of_date_exn date =
  match Ptime.of_date date with
  | Some t -> t
  | None ->
    let (y, m, d) = date in
    failwith (Printf.sprintf "Invalid date: %04d-%02d-%02d" y m d)

let date_of_ptime t = Ptime.to_date t

let compare_dates (d1 : date) (d2 : date) =
  let t1 = ptime_of_date_exn d1 in
  let t2 = ptime_of_date_exn d2 in
  Ptime.compare t1 t2

let format_date (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

let month_name = function
  | 1 -> "January" | 2 -> "February" | 3 -> "March" | 4 -> "April"
  | 5 -> "May" | 6 -> "June" | 7 -> "July" | 8 -> "August"
  | 9 -> "September" | 10 -> "October" | 11 -> "November" | 12 -> "December"
  | _ -> "Unknown"

let format_date_human (y, m, _d) =
  Printf.sprintf "%s %d" (month_name m) y
