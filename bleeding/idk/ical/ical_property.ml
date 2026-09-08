(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let error fmt = Format.kasprintf (fun s -> Error s) fmt

type t = Vcard.Property.t

let v ?params name value = Vcard.Property.v ?params name value
let name = Vcard.Property.name
let params = Vcard.Property.params
let value = Vcard.Property.value
let find_values = Vcard.Property.find_values
let find_first = Vcard.Property.find_first
let equal = Vcard.Property.equal
let pp = Vcard.Property.pp
let uri = Vcard.Property.uri
let boolean = Vcard.Property.boolean
let text = Vcard.Property.text
let text_list = Vcard.Property.text_list
let of_text ?params name s = Vcard.Property.of_text ?params name s

let value_type p =
  match find_first p "VALUE" with
  | Some vt -> Ical_value_type.of_string vt
  | None -> (
      match Ical_registry.value_type (name p) with
      | Some vt -> vt
      | None -> Ical_value_type.Text)

let of_date_time ?(params = []) ?tzid name d =
  let value_param =
    match d with
    | Ical_date.Date _ -> [ Vcard.Param.v "VALUE" [ "DATE" ] ]
    | Ical_date.Date_time _ -> []
  in
  let tzid_param =
    match tzid with Some z -> [ Vcard.Param.v "TZID" [ z ] ] | None -> []
  in
  v ~params:(params @ value_param @ tzid_param) name (Ical_date.to_string d)

let tzid p = find_first p "TZID"

let date_time p =
  match value_type p with
  | Ical_value_type.Date ->
      Result.map
        (fun d -> Ical_date.Date d)
        (Ical_date.date_of_string (value p))
  | _ ->
      Result.map
        (fun dt -> Ical_date.Date_time dt)
        (Ical_date.date_time_of_string (value p))

let date_times p =
  let parse_one =
    match value_type p with
    | Ical_value_type.Date ->
        fun s ->
          Result.map (fun d -> Ical_date.Date d) (Ical_date.date_of_string s)
    | _ ->
        fun s ->
          Result.map
            (fun dt -> Ical_date.Date_time dt)
            (Ical_date.date_time_of_string s)
  in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> (
        match parse_one s with Ok d -> go (d :: acc) rest | Error _ as e -> e)
  in
  go [] (String.split_on_char ',' (value p))

let duration p = Ical_duration.of_string (value p)
let period p = Ical_period.of_string (value p)

let periods p =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> (
        match Ical_period.of_string s with
        | Ok x -> go (x :: acc) rest
        | Error _ as e -> e)
  in
  go [] (String.split_on_char ',' (value p))

let recur p = Ical_recur.of_string (value p)

let integer p =
  let s = value p in
  let is_digit c = c >= '0' && c <= '9' in
  let body =
    if String.length s > 0 && (s.[0] = '+' || s.[0] = '-') then
      String.sub s 1 (String.length s - 1)
    else s
  in
  if String.length body > 0 && String.for_all is_digit body then
    match int_of_string_opt s with
    | Some n -> Ok n
    | None -> error "%S is not an INTEGER" s
  else error "%S is not an INTEGER" s

let float p =
  let s = value p in
  let is_digit c = c >= '0' && c <= '9' in
  let is_unsigned s = String.length s > 0 && String.for_all is_digit s in
  let is_signed s =
    String.length s > 0
    &&
    match s.[0] with
    | '+' | '-' -> is_unsigned (String.sub s 1 (String.length s - 1))
    | _ -> is_unsigned s
  in
  let is_float s =
    match String.index_opt s '.' with
    | None -> is_signed s
    | Some i ->
        is_signed (String.sub s 0 i)
        && is_unsigned (String.sub s (i + 1) (String.length s - i - 1))
  in
  match float_of_string_opt s with
  | Some f when is_float s -> Ok f
  | _ -> error "%S is not a FLOAT" s

let utc_offset p = Ical_date.utc_offset_of_string (value p)
