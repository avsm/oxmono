(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* TOML value representation. Every constructor carries a [Loc.Meta.t] via the
   [node] wrapper: parsers fill it with source positions and surrounding
   whitespace; programmatic constructors default it to [Loc.Meta.none]. *)

module Meta = Loc.Meta

type 'a node = 'a Loc.node
type name = string node

type t =
  | String of string node
  | Int of int64 node
  | Float of float node
  | Bool of bool node
  | Datetime of string node (* Offset datetime *)
  | Datetime_local of string node (* Local datetime *)
  | Date_local of string node (* Local date *)
  | Time_local of string node (* Local time *)
  | Array of t list node
  | Table of (name * t) list node

(* Inner helpers shared by constructors and accessors.

   A [?meta] carried over from a parsed node keeps where that node was and loses
   the document it was read from. Saying where a value sits is not saying that
   the bytes there are still what it means, and a value a caller built is not:
   [Toml.to_string ~preserve:true] writes it rather than copying the bytes it
   would otherwise be claiming. *)
let n ?(meta = Meta.none) v = (v, Meta.clear_text meta)

(* ============================================ Value Constructors
   ============================================ *)

let string ?meta s = String (n ?meta s)
let int ?meta i = Int (n ?meta i)
let int_of_int ?meta i = Int (n ?meta (Int64.of_int i))
let float ?meta f = Float (n ?meta f)
let bool ?meta b = Bool (n ?meta b)
let array ?meta vs = Array (n ?meta vs)

let table ?meta pairs =
  Table (n ?meta (List.map (fun (k, v) -> (n k, v)) pairs))

let datetime ?meta s = Datetime (n ?meta s)
let datetime_local ?meta s = Datetime_local (n ?meta s)
let date_local ?meta s = Date_local (n ?meta s)
let time_local ?meta s = Time_local (n ?meta s)

(* ============================================ Metadata accessors
   ============================================ *)

let meta = function
  | String (_, m)
  | Int (_, m)
  | Float (_, m)
  | Bool (_, m)
  | Datetime (_, m)
  | Datetime_local (_, m)
  | Date_local (_, m)
  | Time_local (_, m)
  | Array (_, m)
  | Table (_, m) ->
      m

let sort = function
  | String _ -> Sort.String
  | Int _ -> Sort.Int
  | Float _ -> Sort.Float
  | Bool _ -> Sort.Bool
  | Datetime _ -> Sort.Datetime
  | Datetime_local _ -> Sort.Datetime_local
  | Date_local _ -> Sort.Date
  | Time_local _ -> Sort.Time
  | Array _ -> Sort.Array
  | Table _ -> Sort.Table

(* ============================================ Ptime Conversions
   ============================================ *)

let datetime_of_ptime ?(tz_offset_s = 0) ?(frac_s = 0) ptime =
  datetime (Ptime.to_rfc3339 ~tz_offset_s ~frac_s ptime)

let date_of_ptime ?(tz_offset_s = 0) ptime =
  let year, month, day = Ptime.to_date ~tz_offset_s ptime in
  Fmt.kstr date_local "%04d-%02d-%02d" year month day

(* Helper to normalize TOML datetime for ptime parsing. TOML 1.1 allows optional
   seconds (e.g., "1979-05-27T07:32Z"), but ptime requires seconds. We add ":00"
   when missing. *)
let normalize_datetime_for_ptime s =
  let len = String.length s in
  if len < 16 then s (* Too short, let ptime handle the error *)
  else
    let has_t = len > 10 && (s.[10] = 'T' || s.[10] = 't' || s.[10] = ' ') in
    if not has_t then s
    else if len >= 17 && s.[16] = ':' then s (* Already has seconds *)
    else if len = 16 then s ^ ":00"
    else
      let c16 = s.[16] in
      if c16 = 'Z' || c16 = 'z' || c16 = '+' || c16 = '-' then
        String.sub s 0 16 ^ ":00" ^ String.sub s 16 (len - 16)
      else if c16 = '.' then s
      else s

let to_ptime_tz = function
  | Datetime (s, _) -> (
      let normalized = normalize_datetime_for_ptime s in
      match Ptime.of_rfc3339 ~strict:false normalized with
      | Ok (t, tz, _) -> Some (t, tz)
      | Error _ -> None)
  | _ -> None

let to_ptime_opt = function
  | Datetime (s, _) -> (
      let normalized = normalize_datetime_for_ptime s in
      match Ptime.of_rfc3339 ~strict:false normalized with
      | Ok (t, _, _) -> Some t
      | Error _ -> None)
  | _ -> None

let to_ptime t =
  match to_ptime_opt t with
  | Some ptime -> ptime
  | None -> (
      match t with
      | Datetime _ -> invalid_arg "Toml.to_ptime: cannot parse datetime"
      | Datetime_local _ ->
          invalid_arg "Toml.to_ptime: local datetime has no timezone"
      | Date_local _ ->
          invalid_arg "Toml.to_ptime: date_local is not a datetime"
      | Time_local _ ->
          invalid_arg "Toml.to_ptime: time_local is not a datetime"
      | _ -> invalid_arg "Toml.to_ptime: not a datetime")

let to_date_opt = function
  | Date_local (s, _) when String.length s >= 10 -> (
      try
        let year = int_of_string (String.sub s 0 4) in
        let month = int_of_string (String.sub s 5 2) in
        let day = int_of_string (String.sub s 8 2) in
        match Ptime.of_date (year, month, day) with
        | Some _ -> Some (year, month, day)
        | None -> None
      with Failure _ -> None)
  | _ -> None

let to_date t =
  match to_date_opt t with
  | Some date -> date
  | None -> (
      match t with
      | Date_local _ -> invalid_arg "Toml.to_date: cannot parse date"
      | _ -> invalid_arg "Toml.to_date: not a date_local")

(* Unified ptime datetime type *)

type ptime_datetime =
  [ `Datetime of Ptime.t * Ptime.tz_offset_s option
  | `Datetime_local of Ptime.t
  | `Date of Ptime.date
  | `Time of int * int * int * int (* hour, minute, second, nanoseconds *) ]

(* Parse local datetime string to ptime using given timezone offset *)
let parse_local_datetime_with_tz tz_offset_s s =
  let normalized = normalize_datetime_for_ptime s in
  let tz_str =
    if tz_offset_s = 0 then "Z"
    else
      let sign = if tz_offset_s >= 0 then '+' else '-' in
      let abs_offset = abs tz_offset_s in
      let hours = abs_offset / 3600 in
      let minutes = abs_offset mod 3600 / 60 in
      Fmt.str "%c%02d:%02d" sign hours minutes
  in
  let with_tz = normalized ^ tz_str in
  match Ptime.of_rfc3339 ~strict:false with_tz with
  | Ok (t, _, _) -> Some t
  | Error _ -> None

(* Parse the fractional-seconds tail of a local time, starting at s.[8] which
   must be the leading '.'. Returns nanoseconds (zero-padded to 9 digits or
   truncated to 9). *)
let parse_local_time_frac s len =
  if len > 9 && s.[8] = '.' then
    let frac_str = String.sub s 9 (len - 9) in
    let padded =
      if String.length frac_str >= 9 then String.sub frac_str 0 9
      else frac_str ^ String.make (9 - String.length frac_str) '0'
    in
    int_of_string padded
  else 0

(* Parse the optional [:SS[.fff]] tail of a local time. *)
let parse_local_time_seconds s len =
  if len >= 8 then
    let sec = int_of_string (String.sub s 6 2) in
    (sec, parse_local_time_frac s len)
  else (0, 0)

(* Parse local time string to (hour, minute, second, nanoseconds) *)
let parse_local_time s =
  let len = String.length s in
  if len < 5 then None
  else
    try
      let hour = int_of_string (String.sub s 0 2) in
      let minute = int_of_string (String.sub s 3 2) in
      let second, frac = parse_local_time_seconds s len in
      if
        hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0
        && second <= 60
      then Some (hour, minute, second, frac)
      else None
    with Failure _ -> None

let to_ptime_datetime ?tz_offset_s t =
  let get_tz () = match tz_offset_s with Some tz -> tz | None -> 0 in
  match t with
  | Datetime (s, _) -> (
      let normalized = normalize_datetime_for_ptime s in
      match Ptime.of_rfc3339 ~strict:false normalized with
      | Ok (ptime, tz, _) -> Some (`Datetime (ptime, tz))
      | Error _ -> None)
  | Datetime_local (s, _) -> (
      let tz = get_tz () in
      match parse_local_datetime_with_tz tz s with
      | Some ptime -> Some (`Datetime_local ptime)
      | None -> None)
  | Date_local _ -> (
      match to_date_opt t with Some date -> Some (`Date date) | None -> None)
  | Time_local (s, _) -> (
      match parse_local_time s with
      | Some time -> Some (`Time time)
      | None -> None)
  | _ -> None

let toml_of_ptime_datetime = function
  | `Datetime (ptime, tz) ->
      let tz_offset_s = Option.value ~default:0 tz in
      datetime (Ptime.to_rfc3339 ~tz_offset_s ptime)
  | `Datetime_local ptime ->
      let (year, month, day), ((hour, minute, second), _) =
        Ptime.to_date_time ptime
      in
      Fmt.kstr datetime_local "%04d-%02d-%02dT%02d:%02d:%02d" year month day
        hour minute second
  | `Date (year, month, day) ->
      Fmt.kstr date_local "%04d-%02d-%02d" year month day
  | `Time (hour, minute, second, ns) ->
      if ns = 0 then Fmt.kstr time_local "%02d:%02d:%02d" hour minute second
      else
        let ns_str = Fmt.str "%09d" ns in
        let rec trim_end i =
          if i <= 0 then 1
          else if ns_str.[i] <> '0' then i + 1
          else trim_end (i - 1)
        in
        let ns_trimmed = String.sub ns_str 0 (trim_end 8) in
        Fmt.kstr time_local "%02d:%02d:%02d.%s" hour minute second ns_trimmed

let pp_ptime_datetime fmt = function
  | `Datetime (ptime, tz) ->
      let tz_offset_s = Option.value ~default:0 tz in
      Fmt.pf fmt "`Datetime %s" (Ptime.to_rfc3339 ~tz_offset_s ptime)
  | `Datetime_local ptime ->
      Fmt.pf fmt "`Datetime_local %s" (Ptime.to_rfc3339 ~tz_offset_s:0 ptime)
  | `Date (year, month, day) -> Fmt.pf fmt "`Date %04d-%02d-%02d" year month day
  | `Time (hour, minute, second, ns) ->
      if ns = 0 then Fmt.pf fmt "`Time %02d:%02d:%02d" hour minute second
      else Fmt.pf fmt "`Time %02d:%02d:%02d.%09d" hour minute second ns

(* ============================================ Value Accessors
   ============================================ *)

let to_string = function
  | String (s, _) -> s
  | _ -> invalid_arg "Toml.to_string: not a string"

let to_string_opt = function String (s, _) -> Some s | _ -> None

let to_int = function
  | Int (i, _) -> i
  | _ -> invalid_arg "Toml.to_int: not an integer"

let to_int_opt = function Int (i, _) -> Some i | _ -> None

let to_float = function
  | Float (f, _) -> f
  | _ -> invalid_arg "Toml.to_float: not a float"

let to_float_opt = function Float (f, _) -> Some f | _ -> None

let to_bool = function
  | Bool (b, _) -> b
  | _ -> invalid_arg "Toml.to_bool: not a boolean"

let to_bool_opt = function Bool (b, _) -> Some b | _ -> None

let to_array = function
  | Array (vs, _) -> vs
  | _ -> invalid_arg "Toml.to_array: not an array"

let to_array_opt = function Array (vs, _) -> Some vs | _ -> None

let to_table = function
  | Table (pairs, _) -> List.map (fun ((k, _), v) -> (k, v)) pairs
  | _ -> invalid_arg "Toml.to_table: not a table"

let to_table_opt = function
  | Table (pairs, _) -> Some (List.map (fun ((k, _), v) -> (k, v)) pairs)
  | _ -> None

let to_datetime = function
  | Datetime (s, _)
  | Datetime_local (s, _)
  | Date_local (s, _)
  | Time_local (s, _) ->
      s
  | _ -> invalid_arg "Toml.to_datetime: not a datetime"

let to_datetime_opt = function
  | Datetime (s, _)
  | Datetime_local (s, _)
  | Date_local (s, _)
  | Time_local (s, _) ->
      Some s
  | _ -> None

(* ============================================ Type Predicates
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

(* ============================================ Table Navigation
   ============================================ *)

let rec assoc_name k = function
  | [] -> raise Not_found
  | ((k', _), v) :: rest -> if k = k' then v else assoc_name k rest

let rec assoc_opt_name k = function
  | [] -> None
  | ((k', _), v) :: rest -> if k = k' then Some v else assoc_opt_name k rest

let rec mem_name k = function
  | [] -> false
  | ((k', _), _) :: rest -> if k = k' then true else mem_name k rest

let get key = function
  | Table (pairs, _) -> assoc_name key pairs
  | _ -> invalid_arg "Toml.get: not a table"

let opt key = function
  | Table (pairs, _) -> assoc_opt_name key pairs
  | _ -> None

let mem key = function Table (pairs, _) -> mem_name key pairs | _ -> false

let keys = function
  | Table (pairs, _) -> List.map (fun ((k, _), _) -> k) pairs
  | _ -> invalid_arg "Toml.keys: not a table"

let rec path path_keys t =
  match path_keys with
  | [] -> t
  | key :: rest -> (
      match t with
      | Table (pairs, _) -> (
          match assoc_opt_name key pairs with
          | Some v -> path rest v
          | None -> raise Not_found)
      | _ -> invalid_arg "Toml.path: intermediate value is not a table")

let path_opt path_keys t =
  try Some (path path_keys t) with Not_found | Invalid_argument _ -> None

let ( .%{} ) t path_keys = path path_keys t

let rec set_at_path p v t =
  match p with
  | [] -> v
  | [ key ] -> (
      match t with
      | Table (pairs, m) ->
          let pairs' = List.filter (fun ((k, _), _) -> k <> key) pairs in
          Table ((n key, v) :: pairs', m)
      | _ -> invalid_arg "Toml.(.%{}<-): not a table")
  | key :: rest -> (
      match t with
      | Table (pairs, m) ->
          let existing = assoc_opt_name key pairs in
          let subtable =
            match existing with
            | Some (Table _ as sub) -> sub
            | Some _ ->
                invalid_arg "Toml.(.%{}<-): intermediate value is not a table"
            | None -> Table ([], Meta.none)
          in
          let updated = set_at_path rest v subtable in
          let pairs' = List.filter (fun ((k, _), _) -> k <> key) pairs in
          Table ((n key, updated) :: pairs', m)
      | _ -> invalid_arg "Toml.(.%{}<-): not a table")

let ( .%{}<- ) t p v = set_at_path p v t

(* ============================================ Pretty Printing
   ============================================ *)

let rec pp_value fmt = function
  | String (s, _) -> Fmt.pf fmt "\"%s\"" (String.escaped s)
  | Int (i, _) -> Fmt.pf fmt "%Ld" i
  | Float (f, _) ->
      if Float.is_nan f then Fmt.pf fmt "nan"
      else if f = Float.infinity then Fmt.pf fmt "inf"
      else if f = Float.neg_infinity then Fmt.pf fmt "-inf"
      else Fmt.pf fmt "%g" f
  | Bool (b, _) -> Fmt.pf fmt "%s" (if b then "true" else "false")
  | Datetime (s, _)
  | Datetime_local (s, _)
  | Date_local (s, _)
  | Time_local (s, _) ->
      Fmt.pf fmt "%s" s
  | Array (items, _) ->
      Fmt.pf fmt "[";
      List.iteri
        (fun i item ->
          if i > 0 then Fmt.pf fmt ", ";
          pp_value fmt item)
        items;
      Fmt.pf fmt "]"
  | Table (pairs, _) ->
      Fmt.pf fmt "{";
      List.iteri
        (fun i ((k, _), v) ->
          if i > 0 then Fmt.pf fmt ", ";
          Fmt.pf fmt "%s = " k;
          pp_value fmt v)
        pairs;
      Fmt.pf fmt "}"

let pp = pp_value

(* ============================================ Equality and Comparison

   Both operations ignore [Meta.t]: TOML equality is by value, source locations
   are incidental. ============================================ *)

let rec equal a b =
  match (a, b) with
  | String (s1, _), String (s2, _) -> String.equal s1 s2
  | Int (i1, _), Int (i2, _) -> Int64.equal i1 i2
  | Float (f1, _), Float (f2, _) ->
      (Float.is_nan f1 && Float.is_nan f2) || Float.equal f1 f2
  | Bool (b1, _), Bool (b2, _) -> Bool.equal b1 b2
  | Datetime (s1, _), Datetime (s2, _) -> String.equal s1 s2
  | Datetime_local (s1, _), Datetime_local (s2, _) -> String.equal s1 s2
  | Date_local (s1, _), Date_local (s2, _) -> String.equal s1 s2
  | Time_local (s1, _), Time_local (s2, _) -> String.equal s1 s2
  | Array (vs1, _), Array (vs2, _) ->
      List.length vs1 = List.length vs2 && List.for_all2 equal vs1 vs2
  | Table (ps1, _), Table (ps2, _) ->
      List.length ps1 = List.length ps2
      && List.for_all2
           (fun ((k1, _), v1) ((k2, _), v2) ->
             String.equal k1 k2 && equal v1 v2)
           ps1 ps2
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

let rec compare_same_type a b =
  match (a, b) with
  | String (s1, _), String (s2, _) -> String.compare s1 s2
  | Int (i1, _), Int (i2, _) -> Int64.compare i1 i2
  | Float (f1, _), Float (f2, _) -> Float.compare f1 f2
  | Bool (b1, _), Bool (b2, _) -> Bool.compare b1 b2
  | Datetime (s1, _), Datetime (s2, _) -> String.compare s1 s2
  | Datetime_local (s1, _), Datetime_local (s2, _) -> String.compare s1 s2
  | Date_local (s1, _), Date_local (s2, _) -> String.compare s1 s2
  | Time_local (s1, _), Time_local (s2, _) -> String.compare s1 s2
  | Array (vs1, _), Array (vs2, _) -> List.compare compare vs1 vs2
  | Table (ps1, _), Table (ps2, _) ->
      List.compare
        (fun ((k1, _), v1) ((k2, _), v2) ->
          let c = String.compare k1 k2 in
          if c <> 0 then c else compare v1 v2)
        ps1 ps2
  | _ -> 0

and compare a b =
  let ta, tb = (type_order a, type_order b) in
  if ta <> tb then Int.compare ta tb else compare_same_type a b
