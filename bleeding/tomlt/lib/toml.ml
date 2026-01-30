(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* TOML value representation *)

type t =
  | String of string
  | Int of int64
  | Float of float
  | Bool of bool
  | Datetime of string  (* Offset datetime *)
  | Datetime_local of string  (* Local datetime *)
  | Date_local of string  (* Local date *)
  | Time_local of string  (* Local time *)
  | Array of t list
  | Table of (string * t) list

(* ============================================
   Value Constructors
   ============================================ *)

let string s = String s
let int i = Int i
let int_of_int i = Int (Int64.of_int i)
let float f = Float f
let bool b = Bool b
let array vs = Array vs
let table pairs = Table pairs
let datetime s = Datetime s
let datetime_local s = Datetime_local s
let date_local s = Date_local s
let time_local s = Time_local s

(* ============================================
   Ptime Conversions
   ============================================ *)

let datetime_of_ptime ?(tz_offset_s = 0) ?(frac_s = 0) ptime =
  Datetime (Ptime.to_rfc3339 ~tz_offset_s ~frac_s ptime)

let date_of_ptime ?(tz_offset_s = 0) ptime =
  let (year, month, day) = Ptime.to_date ~tz_offset_s ptime in
  Date_local (Printf.sprintf "%04d-%02d-%02d" year month day)

(* Helper to normalize TOML datetime for ptime parsing.
   TOML 1.1 allows optional seconds (e.g., "1979-05-27T07:32Z"),
   but ptime requires seconds. We add ":00" when missing. *)
let normalize_datetime_for_ptime s =
  let len = String.length s in
  if len < 16 then s (* Too short, let ptime handle the error *)
  else
    (* Check if we have HH:MM followed by timezone or end without seconds *)
    (* Format: YYYY-MM-DDTHH:MM... position 16 would be after HH:MM *)
    let has_t = len > 10 && (s.[10] = 'T' || s.[10] = 't' || s.[10] = ' ') in
    if not has_t then s
    else if len >= 17 && s.[16] = ':' then s (* Already has seconds *)
    else if len = 16 then
      (* YYYY-MM-DDTHH:MM - local datetime without seconds, add :00 *)
      s ^ ":00"
    else
      let c16 = s.[16] in
      if c16 = 'Z' || c16 = 'z' || c16 = '+' || c16 = '-' then
        (* YYYY-MM-DDTHH:MMZ or YYYY-MM-DDTHH:MM+... - insert :00 before tz *)
        String.sub s 0 16 ^ ":00" ^ String.sub s 16 (len - 16)
      else if c16 = '.' then
        (* YYYY-MM-DDTHH:MM.fraction - unusual but handle it *)
        s
      else
        s

let to_ptime_tz = function
  | Datetime s ->
      let normalized = normalize_datetime_for_ptime s in
      (match Ptime.of_rfc3339 ~strict:false normalized with
       | Ok (t, tz, _) -> Some (t, tz)
       | Error _ -> None)
  | _ -> None

let to_ptime_opt = function
  | Datetime s ->
      let normalized = normalize_datetime_for_ptime s in
      (match Ptime.of_rfc3339 ~strict:false normalized with
       | Ok (t, _, _) -> Some t
       | Error _ -> None)
  | _ -> None

let to_ptime t =
  match to_ptime_opt t with
  | Some ptime -> ptime
  | None ->
      match t with
      | Datetime _ -> invalid_arg "Toml.to_ptime: cannot parse datetime"
      | Datetime_local _ -> invalid_arg "Toml.to_ptime: local datetime has no timezone"
      | Date_local _ -> invalid_arg "Toml.to_ptime: date_local is not a datetime"
      | Time_local _ -> invalid_arg "Toml.to_ptime: time_local is not a datetime"
      | _ -> invalid_arg "Toml.to_ptime: not a datetime"

let to_date_opt = function
  | Date_local s when String.length s >= 10 ->
      (try
        let year = int_of_string (String.sub s 0 4) in
        let month = int_of_string (String.sub s 5 2) in
        let day = int_of_string (String.sub s 8 2) in
        (* Validate using Ptime.of_date *)
        match Ptime.of_date (year, month, day) with
        | Some _ -> Some (year, month, day)
        | None -> None
      with _ -> None)
  | _ -> None

let to_date t =
  match to_date_opt t with
  | Some date -> date
  | None ->
      match t with
      | Date_local _ -> invalid_arg "Toml.to_date: cannot parse date"
      | _ -> invalid_arg "Toml.to_date: not a date_local"

(* Unified ptime datetime type *)

type ptime_datetime = [
  | `Datetime of Ptime.t * Ptime.tz_offset_s option
  | `Datetime_local of Ptime.t
  | `Date of Ptime.date
  | `Time of int * int * int * int  (* hour, minute, second, nanoseconds *)
]

(* Parse local datetime string to ptime using given timezone offset *)
let parse_local_datetime_with_tz tz_offset_s s =
  let normalized = normalize_datetime_for_ptime s in
  (* Append timezone to make it parseable by ptime *)
  let tz_str =
    if tz_offset_s = 0 then "Z"
    else
      let sign = if tz_offset_s >= 0 then '+' else '-' in
      let abs_offset = abs tz_offset_s in
      let hours = abs_offset / 3600 in
      let minutes = (abs_offset mod 3600) / 60 in
      Printf.sprintf "%c%02d:%02d" sign hours minutes
  in
  let with_tz = normalized ^ tz_str in
  match Ptime.of_rfc3339 ~strict:false with_tz with
  | Ok (t, _, _) -> Some t
  | Error _ -> None

(* Parse local time string to (hour, minute, second, nanoseconds) *)
let parse_local_time s =
  let len = String.length s in
  if len < 5 then None
  else
    try
      let hour = int_of_string (String.sub s 0 2) in
      let minute = int_of_string (String.sub s 3 2) in
      let second, frac =
        if len >= 8 then
          let sec = int_of_string (String.sub s 6 2) in
          let frac =
            if len > 9 && s.[8] = '.' then
              let frac_str = String.sub s 9 (len - 9) in
              (* Pad or truncate to 9 digits for nanoseconds *)
              let padded =
                if String.length frac_str >= 9 then String.sub frac_str 0 9
                else frac_str ^ String.make (9 - String.length frac_str) '0'
              in
              int_of_string padded
            else 0
          in
          (sec, frac)
        else
          (* TOML 1.1: optional seconds *)
          (0, 0)
      in
      if hour >= 0 && hour <= 23 &&
         minute >= 0 && minute <= 59 &&
         second >= 0 && second <= 60 then  (* 60 for leap second *)
        Some (hour, minute, second, frac)
      else
        None
    with _ -> None

let to_ptime_datetime ?tz_offset_s t =
  let get_tz () =
    match tz_offset_s with
    | Some tz -> tz
    | None -> 0  (* Default to UTC when no timezone provided *)
  in
  match t with
  | Datetime s ->
      let normalized = normalize_datetime_for_ptime s in
      (match Ptime.of_rfc3339 ~strict:false normalized with
       | Ok (ptime, tz, _) -> Some (`Datetime (ptime, tz))
       | Error _ -> None)
  | Datetime_local s ->
      let tz = get_tz () in
      (match parse_local_datetime_with_tz tz s with
       | Some ptime -> Some (`Datetime_local ptime)
       | None -> None)
  | Date_local _ ->
      (match to_date_opt t with
       | Some date -> Some (`Date date)
       | None -> None)
  | Time_local s ->
      (match parse_local_time s with
       | Some time -> Some (`Time time)
       | None -> None)
  | _ -> None

let ptime_datetime_to_toml = function
  | `Datetime (ptime, tz) ->
      let tz_offset_s = Option.value ~default:0 tz in
      Datetime (Ptime.to_rfc3339 ~tz_offset_s ptime)
  | `Datetime_local ptime ->
      (* Convert to local time string without timezone *)
      let ((year, month, day), ((hour, minute, second), _)) =
        Ptime.to_date_time ptime
      in
      Datetime_local (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
        year month day hour minute second)
  | `Date (year, month, day) ->
      Date_local (Printf.sprintf "%04d-%02d-%02d" year month day)
  | `Time (hour, minute, second, ns) ->
      if ns = 0 then
        Time_local (Printf.sprintf "%02d:%02d:%02d" hour minute second)
      else
        (* Format nanoseconds, trimming trailing zeros *)
        let ns_str = Printf.sprintf "%09d" ns in
        let rec trim_end i =
          if i <= 0 then 1
          else if ns_str.[i] <> '0' then i + 1
          else trim_end (i - 1)
        in
        let ns_trimmed = String.sub ns_str 0 (trim_end 8) in
        Time_local (Printf.sprintf "%02d:%02d:%02d.%s" hour minute second ns_trimmed)

let pp_ptime_datetime fmt = function
  | `Datetime (ptime, tz) ->
      let tz_offset_s = Option.value ~default:0 tz in
      Format.fprintf fmt "`Datetime %s" (Ptime.to_rfc3339 ~tz_offset_s ptime)
  | `Datetime_local ptime ->
      Format.fprintf fmt "`Datetime_local %s" (Ptime.to_rfc3339 ~tz_offset_s:0 ptime)
  | `Date (year, month, day) ->
      Format.fprintf fmt "`Date %04d-%02d-%02d" year month day
  | `Time (hour, minute, second, ns) ->
      if ns = 0 then
        Format.fprintf fmt "`Time %02d:%02d:%02d" hour minute second
      else
        Format.fprintf fmt "`Time %02d:%02d:%02d.%09d" hour minute second ns

(* ============================================
   Value Accessors
   ============================================ *)

let to_string = function
  | String s -> s
  | _ -> invalid_arg "Toml.to_string: not a string"

let to_string_opt = function
  | String s -> Some s
  | _ -> None

let to_int = function
  | Int i -> i
  | _ -> invalid_arg "Toml.to_int: not an integer"

let to_int_opt = function
  | Int i -> Some i
  | _ -> None

let to_float = function
  | Float f -> f
  | _ -> invalid_arg "Toml.to_float: not a float"

let to_float_opt = function
  | Float f -> Some f
  | _ -> None

let to_bool = function
  | Bool b -> b
  | _ -> invalid_arg "Toml.to_bool: not a boolean"

let to_bool_opt = function
  | Bool b -> Some b
  | _ -> None

let to_array = function
  | Array vs -> vs
  | _ -> invalid_arg "Toml.to_array: not an array"

let to_array_opt = function
  | Array vs -> Some vs
  | _ -> None

let to_table = function
  | Table pairs -> pairs
  | _ -> invalid_arg "Toml.to_table: not a table"

let to_table_opt = function
  | Table pairs -> Some pairs
  | _ -> None

let to_datetime = function
  | Datetime s | Datetime_local s | Date_local s | Time_local s -> s
  | _ -> invalid_arg "Toml.to_datetime: not a datetime"

let to_datetime_opt = function
  | Datetime s | Datetime_local s | Date_local s | Time_local s -> Some s
  | _ -> None

(* ============================================
   Type Predicates
   ============================================ *)

let is_string = function String _ -> true | _ -> false
let is_int = function Int _ -> true | _ -> false
let is_float = function Float _ -> true | _ -> false
let is_bool = function Bool _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_table = function Table _ -> true | _ -> false
let is_datetime = function
  | Datetime _ | Datetime_local _ | Date_local _ | Time_local _ -> true
  | _ -> false

(* ============================================
   Table Navigation
   ============================================ *)

let find key = function
  | Table pairs -> List.assoc key pairs
  | _ -> invalid_arg "Toml.find: not a table"

let find_opt key = function
  | Table pairs -> List.assoc_opt key pairs
  | _ -> None

let mem key = function
  | Table pairs -> List.mem_assoc key pairs
  | _ -> false

let keys = function
  | Table pairs -> List.map fst pairs
  | _ -> invalid_arg "Toml.keys: not a table"

let rec get path t =
  match path with
  | [] -> t
  | key :: rest ->
      match t with
      | Table pairs ->
          (match List.assoc_opt key pairs with
          | Some v -> get rest v
          | None -> raise Not_found)
      | _ -> invalid_arg "Toml.get: intermediate value is not a table"

let get_opt path t =
  try Some (get path t) with Not_found | Invalid_argument _ -> None

let ( .%{} ) t path = get path t

let rec set_at_path path v t =
  match path with
  | [] -> v
  | [key] ->
      (match t with
      | Table pairs ->
          let pairs' = List.filter (fun (k, _) -> k <> key) pairs in
          Table ((key, v) :: pairs')
      | _ -> invalid_arg "Toml.(.%{}<-): not a table")
  | key :: rest ->
      match t with
      | Table pairs ->
          let existing = List.assoc_opt key pairs in
          let subtable = match existing with
            | Some (Table _ as sub) -> sub
            | Some _ -> invalid_arg "Toml.(.%{}<-): intermediate value is not a table"
            | None -> Table []
          in
          let updated = set_at_path rest v subtable in
          let pairs' = List.filter (fun (k, _) -> k <> key) pairs in
          Table ((key, updated) :: pairs')
      | _ -> invalid_arg "Toml.(.%{}<-): not a table"

let ( .%{}<- ) t path v = set_at_path path v t

(* ============================================
   Pretty Printing
   ============================================ *)

let rec pp_value fmt = function
  | String s ->
      Format.fprintf fmt "\"%s\"" (String.escaped s)
  | Int i ->
      Format.fprintf fmt "%Ld" i
  | Float f ->
      if Float.is_nan f then Format.fprintf fmt "nan"
      else if f = Float.infinity then Format.fprintf fmt "inf"
      else if f = Float.neg_infinity then Format.fprintf fmt "-inf"
      else Format.fprintf fmt "%g" f
  | Bool b ->
      Format.fprintf fmt "%s" (if b then "true" else "false")
  | Datetime s | Datetime_local s | Date_local s | Time_local s ->
      Format.fprintf fmt "%s" s
  | Array items ->
      Format.fprintf fmt "[";
      List.iteri (fun i item ->
        if i > 0 then Format.fprintf fmt ", ";
        pp_value fmt item
      ) items;
      Format.fprintf fmt "]"
  | Table pairs ->
      Format.fprintf fmt "{";
      List.iteri (fun i (k, v) ->
        if i > 0 then Format.fprintf fmt ", ";
        Format.fprintf fmt "%s = " k;
        pp_value fmt v
      ) pairs;
      Format.fprintf fmt "}"

let pp = pp_value

(* ============================================
   Equality and Comparison
   ============================================ *)

let rec equal a b =
  match a, b with
  | String s1, String s2 -> String.equal s1 s2
  | Int i1, Int i2 -> Int64.equal i1 i2
  | Float f1, Float f2 ->
      (* NaN = NaN for TOML equality *)
      (Float.is_nan f1 && Float.is_nan f2) || Float.equal f1 f2
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | Datetime s1, Datetime s2 -> String.equal s1 s2
  | Datetime_local s1, Datetime_local s2 -> String.equal s1 s2
  | Date_local s1, Date_local s2 -> String.equal s1 s2
  | Time_local s1, Time_local s2 -> String.equal s1 s2
  | Array vs1, Array vs2 ->
      List.length vs1 = List.length vs2 &&
      List.for_all2 equal vs1 vs2
  | Table ps1, Table ps2 ->
      List.length ps1 = List.length ps2 &&
      List.for_all2 (fun (k1, v1) (k2, v2) ->
        String.equal k1 k2 && equal v1 v2
      ) ps1 ps2
  | _ -> false

let type_order = function
  | String _ -> 0
  | Int _ -> 1
  | Float _ -> 2
  | Bool _ -> 3
  | Datetime _ -> 4
  | Datetime_local _ -> 5
  | Date_local _ -> 6
  | Time_local _ -> 7
  | Array _ -> 8
  | Table _ -> 9

let rec compare a b =
  let ta, tb = type_order a, type_order b in
  if ta <> tb then Int.compare ta tb
  else match a, b with
  | String s1, String s2 -> String.compare s1 s2
  | Int i1, Int i2 -> Int64.compare i1 i2
  | Float f1, Float f2 -> Float.compare f1 f2
  | Bool b1, Bool b2 -> Bool.compare b1 b2
  | Datetime s1, Datetime s2 -> String.compare s1 s2
  | Datetime_local s1, Datetime_local s2 -> String.compare s1 s2
  | Date_local s1, Date_local s2 -> String.compare s1 s2
  | Time_local s1, Time_local s2 -> String.compare s1 s2
  | Array vs1, Array vs2 ->
      List.compare compare vs1 vs2
  | Table ps1, Table ps2 ->
      List.compare (fun (k1, v1) (k2, v2) ->
        let c = String.compare k1 k2 in
        if c <> 0 then c else compare v1 v2
      ) ps1 ps2
  | _ -> 0  (* Impossible - handled by type_order check *)

(* ============================================
   Error Module
   ============================================ *)

module Error = Toml_error
