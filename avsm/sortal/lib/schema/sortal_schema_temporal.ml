(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type date = Ptime.date

type range = {
  from: date option;
  until: date option;
}

let make ?from ?until () = { from; until }

let always = { from = None; until = None }

(* Compare Ptime dates (year, month, day tuples) *)
let date_compare ((y1, m1, d1) : date) ((y2, m2, d2) : date) : int =
  match compare y1 y2 with
  | 0 -> (
      match compare m1 m2 with
      | 0 -> compare d1 d2
      | c -> c)
  | c -> c

let date_gte d1 d2 = date_compare d1 d2 >= 0

let valid_at range_opt ~date =
  match range_opt with
  | None -> true  (* No range = always valid *)
  | Some { from; until } ->
    let after_start = match from with
      | None -> true
      | Some f -> date_gte date f
    in
    let before_end = match until with
      | None -> true
      | Some u -> date_compare date u < 0  (* until is exclusive *)
    in
    after_start && before_end

let overlaps r1 r2 =
  (* Two ranges overlap if neither ends before the other starts *)
  let r1_starts_before_r2_ends = match r2.until with
    | None -> true
    | Some u2 -> match r1.from with
      | None -> true
      | Some f1 -> date_compare f1 u2 < 0
  in
  let r2_starts_before_r1_ends = match r1.until with
    | None -> true
    | Some u1 -> match r2.from with
      | None -> true
      | Some f2 -> date_compare f2 u1 < 0
  in
  r1_starts_before_r2_ends && r2_starts_before_r1_ends

let today () =
  Ptime_clock.now () |> Ptime.to_date

let is_current range_opt =
  valid_at range_opt ~date:(today ())

let current ~get list =
  (* Find first currently valid item, or first item without temporal bounds *)
  let current_items = List.filter (fun item -> is_current (get item)) list in
  match current_items with
  | x :: _ -> Some x
  | [] ->
    (* No current items, try to find one without temporal bounds *)
    List.find_opt (fun item -> get item = None) list

let at_date ~get ~date list =
  List.filter (fun item -> valid_at (get item) ~date) list

let filter ~get ~from ~until list =
  let query_range = { from; until } in
  List.filter (fun item ->
    match get item with
    | None -> true  (* Items without range match all queries *)
    | Some r -> overlaps r query_range
  ) list

(* Parse ISO 8601 date string to Ptime.date, handling partial dates *)
let parse_date_string (s : string) : date option =
  match String.split_on_char '-' s with
  | [year_s] -> (
      try
        let year = int_of_string year_s in
        Some (year, 1, 1)  (* Year only → January 1st *)
      with Failure _ -> None)
  | [year_s; month_s] -> (
      try
        let year = int_of_string year_s in
        let month = int_of_string month_s in
        if month >= 1 && month <= 12 then
          Some (year, month, 1)  (* Year-Month → 1st of month *)
        else None
      with Failure _ -> None)
  | [year_s; month_s; day_s] -> (
      try
        let year = int_of_string year_s in
        let month = int_of_string month_s in
        let day = int_of_string day_s in
        if month >= 1 && month <= 12 && day >= 1 && day <= 31 then
          Some (year, month, day)
        else None
      with Failure _ -> None)
  | _ -> None

(* Format Ptime.date as ISO 8601 string YYYY-MM-DD *)
let format_date ((year, month, day) : date) : string =
  Printf.sprintf "%04d-%02d-%02d" year month day

let json_t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in

  (* Create a jsont type for date that converts between string and Ptime.date *)
  let date_jsont =
    let dec meta s =
      match parse_date_string s with
      | Some d -> d
      | None -> Error.msgf meta "TemporalDate: invalid ISO 8601 date: %S" s
    in
    let enc = format_date in
    Base.string (Base.map ~kind:"TemporalDate" ~dec ~enc ())
  in

  let make_range from until = { from; until } in
  map ~kind:"TemporalRange" make_range
  |> mem_opt "from" (some date_jsont) ~enc:(fun r -> r.from)
  |> mem_opt "until" (some date_jsont) ~enc:(fun r -> r.until)
  |> finish
