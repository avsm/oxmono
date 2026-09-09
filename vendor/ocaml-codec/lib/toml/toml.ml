(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative TOML codecs *)

(* ---- Preliminaries ---- *)

type 'a fmt = Format.formatter -> 'a -> unit

let ( <?> ) c c' = if c <> 0 then c else c'

(* Find first char matching predicate *)
let string_index_opt p s =
  let len = String.length s in
  let rec loop i =
    if i >= len then None else if p s.[i] then Some i else loop (i + 1)
  in
  loop 0

(* Find separator (T, t, or space) for datetime parsing *)
let datetime_sep s = string_index_opt (fun c -> c = 'T' || c = 't' || c = ' ') s

(* ---- Datetime structured types ---- *)

module Tz = struct
  type t = UTC | Offset of { hours : int; minutes : int }

  let utc = UTC
  let offset ~hours ~minutes = Offset { hours; minutes }

  let equal a b =
    match (a, b) with
    | UTC, UTC -> true
    | Offset { hours = h1; minutes = m1 }, Offset { hours = h2; minutes = m2 }
      ->
        h1 = h2 && m1 = m2
    | _ -> false

  let compare a b =
    match (a, b) with
    | UTC, UTC -> 0
    | UTC, Offset _ -> -1
    | Offset _, UTC -> 1
    | Offset { hours = h1; minutes = m1 }, Offset { hours = h2; minutes = m2 }
      ->
        Int.compare h1 h2 <?> Int.compare m1 m2

  let to_string = function
    | UTC -> "Z"
    | Offset { hours; minutes } ->
        let sign = if hours >= 0 then '+' else '-' in
        Fmt.str "%c%02d:%02d" sign (abs hours) (abs minutes)

  let pp fmt t = Fmt.string fmt (to_string t)

  let of_string s =
    let len = String.length s in
    if len = 0 then Error "empty timezone"
    else if s = "Z" || s = "z" then Ok UTC
    else
      let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
      (* The slices below read two digits at [start] and two more at [start + 3],
         so a signed offset needs six characters and an unsigned one five. *)
      if len < start + 5 then Error ("invalid timezone: " ^ s)
      else
        let sign = if s.[0] = '-' then -1 else 1 in
        try
          let hours = int_of_string (String.sub s start 2) * sign in
          let minutes = int_of_string (String.sub s (start + 3) 2) in
          Ok (Offset { hours; minutes })
        with Failure _ | Invalid_argument _ -> Error ("invalid timezone: " ^ s)
end

module Date = struct
  type t = { year : int; month : int; day : int }

  let v ~year ~month ~day = { year; month; day }
  let equal a b = a.year = b.year && a.month = b.month && a.day = b.day

  let compare a b =
    Int.compare a.year b.year
    <?> Int.compare a.month b.month
    <?> Int.compare a.day b.day

  let to_string d = Fmt.str "%04d-%02d-%02d" d.year d.month d.day
  let pp fmt d = Fmt.string fmt (to_string d)

  let of_string s =
    if String.length s < 10 then Error "date too short"
    else
      try
        let year = int_of_string (String.sub s 0 4) in
        let month = int_of_string (String.sub s 5 2) in
        let day = int_of_string (String.sub s 8 2) in
        Ok { year; month; day }
      with Failure _ | Invalid_argument _ -> Error ("invalid date: " ^ s)
end

module Time = struct
  type t = { hour : int; minute : int; second : int; frac : float }

  let v ~hour ~minute ~second ?(frac = 0.0) () = { hour; minute; second; frac }

  let equal a b =
    a.hour = b.hour && a.minute = b.minute && a.second = b.second
    && a.frac = b.frac

  let compare a b =
    Int.compare a.hour b.hour
    <?> Int.compare a.minute b.minute
    <?> Int.compare a.second b.second
    <?> Float.compare a.frac b.frac

  let rstrip_zeros s =
    let rec find_end i =
      if i <= 0 then 1 else if s.[i] <> '0' then i + 1 else find_end (i - 1)
    in
    String.sub s 0 (find_end (String.length s - 1))

  let to_string t =
    match t.frac with
    | 0.0 -> Fmt.str "%02d:%02d:%02d" t.hour t.minute t.second
    | frac ->
        let frac_str = Fmt.str "%.9f" frac in
        let frac_digits = String.sub frac_str 2 (String.length frac_str - 2) in
        Fmt.str "%02d:%02d:%02d.%s" t.hour t.minute t.second
          (rstrip_zeros frac_digits)

  let pp fmt t = Fmt.string fmt (to_string t)

  let of_string s =
    if String.length s < 8 then Error "time too short"
    else
      try
        let hour = int_of_string (String.sub s 0 2) in
        let minute = int_of_string (String.sub s 3 2) in
        let second = int_of_string (String.sub s 6 2) in
        let frac =
          if String.length s > 8 && s.[8] = '.' then
            float_of_string ("0" ^ String.sub s 8 (String.length s - 8))
          else 0.0
        in
        Ok { hour; minute; second; frac }
      with Failure _ | Invalid_argument _ -> Error ("invalid time: " ^ s)
end

module Datetime = struct
  type t = { date : Date.t; time : Time.t; tz : Tz.t }

  let v ~date ~time ~tz = { date; time; tz }

  let equal a b =
    Date.equal a.date b.date && Time.equal a.time b.time && Tz.equal a.tz b.tz

  let compare a b =
    Date.compare a.date b.date <?> Time.compare a.time b.time
    <?> Tz.compare a.tz b.tz

  let to_string dt =
    Fmt.str "%sT%s%s" (Date.to_string dt.date) (Time.to_string dt.time)
      (Tz.to_string dt.tz)

  let pp fmt dt = Fmt.string fmt (to_string dt)

  let of_string s =
    let ( let* ) = Result.bind in
    let ( let+ ) result f = Result.map f result in
    match datetime_sep s with
    | None -> Error "missing date/time separator"
    | Some idx ->
        let date_str = String.sub s 0 idx in
        let rest = String.sub s (idx + 1) (String.length s - idx - 1) in
        let is_tz_start i c =
          c = 'Z' || c = 'z' || c = '+' || (c = '-' && i > 2)
        in
        let tz_idx =
          let len = String.length rest in
          let rec find i =
            if i >= len then len
            else if is_tz_start i rest.[i] then i
            else find (i + 1)
          in
          find 0
        in
        let time_str = String.sub rest 0 tz_idx in
        let tz_str = String.sub rest tz_idx (String.length rest - tz_idx) in
        let* date = Date.of_string date_str in
        let* time = Time.of_string time_str in
        let+ tz = Tz.of_string tz_str in
        { date; time; tz }
end

module Datetime_local = struct
  type t = { date : Date.t; time : Time.t }

  let v ~date ~time = { date; time }
  let equal a b = Date.equal a.date b.date && Time.equal a.time b.time
  let compare a b = Date.compare a.date b.date <?> Time.compare a.time b.time

  let to_string dt =
    Fmt.str "%sT%s" (Date.to_string dt.date) (Time.to_string dt.time)

  let pp fmt dt = Fmt.string fmt (to_string dt)

  let of_string s =
    let ( let* ) = Result.bind in
    let ( let+ ) result f = Result.map f result in
    match datetime_sep s with
    | None -> Error "missing date/time separator"
    | Some idx ->
        let date_str = String.sub s 0 idx in
        let time_str = String.sub s (idx + 1) (String.length s - idx - 1) in
        let* date = Date.of_string date_str in
        let+ time = Time.of_string time_str in
        { date; time }
end

(* ---- Module aliases ----

   Re-exports. Placed below the datetime modules so the [Ok] / [Error]
   constructors of [result] are not shadowed by the [Error] exception/module
   alias while parsing those types. *)

module Loc = Loc
module Meta = Loc.Meta
module Path = Loc.Path
module Error = Error
module Sort = Sort
module Codec = Codec
module Parser = Parser

exception Error = Loc.Error
exception Invalid_utf8_encode of int

type 'a codec = 'a Codec.t

(* Value module: re-export plus identity codec. The identity codec lives here
   rather than in [value.ml] to avoid a circular dependency between [value.ml]
   and [codec.ml] (the codec type mentions [Value.t]). *)
module Value = struct
  include Value

  let codec : t Codec.t = Codec.Value.t
end

type t = Value.t

let pp = Value.pp

module Cursor = Cursor

(* ---- UTF-8 validation ---- *)

let validate_utf8 s =
  match Utf8.validate s with
  | Ok () -> ()
  | Error i -> raise (Invalid_utf8_encode i)

let rec validate_utf8_toml : Value.t -> unit = function
  | Value.String (s, _)
  | Value.Datetime (s, _)
  | Value.Datetime_local (s, _)
  | Value.Date_local (s, _)
  | Value.Time_local (s, _) ->
      validate_utf8 s
  | Value.Array (l, _) -> List.iter validate_utf8_toml l
  | Value.Table (kvs, _) ->
      List.iter
        (fun ((k, _), v) ->
          validate_utf8 k;
          validate_utf8_toml v)
        kvs
  | Value.Int _ | Value.Float _ | Value.Bool _ -> ()

(* ---- Decoding and encoding at the Value layer ---- *)

let decode = Codec.of_toml
let decode_exn = Codec.of_toml_exn
let encode = Codec.to_toml

(* ---- Top-level I/O ---- *)

let of_parsed ?max_depth ?max_nodes c parse =
  try
    let toml = parse ~max_depth ~max_nodes in
    Ok (Codec.of_toml_exn ?max_depth ?max_nodes c toml)
  with
  | Loc.Error e -> Error e
  | Failure msg -> Error (Error.msg ~ctx:Loc.Context.empty ~meta:Meta.none msg)

let of_string ?max_depth ?max_nodes c s =
  of_parsed ?max_depth ?max_nodes c (fun ~max_depth ~max_nodes ->
      Parser.parse ?max_depth ?max_nodes s)

let of_string_exn ?max_depth ?max_nodes c s =
  match of_string ?max_depth ?max_nodes c s with
  | Ok v -> v
  | Error e -> raise (Loc.Error e)

let of_reader ?max_depth ?max_nodes c reader =
  of_parsed ?max_depth ?max_nodes c (fun ~max_depth ~max_nodes ->
      Parser.parse_reader ?max_depth ?max_nodes reader)

let of_reader_exn ?max_depth ?max_nodes c reader =
  match of_reader ?max_depth ?max_nodes c reader with
  | Ok v -> v
  | Error e -> raise (Loc.Error e)

let to_writer ?indent ?preserve c v w =
  let toml = Codec.to_toml c v in
  validate_utf8_toml toml;
  Parser.to_writer ?indent ?preserve w toml

let to_string ?indent ?preserve c v =
  let toml = Codec.to_toml c v in
  validate_utf8_toml toml;
  Parser.to_string ?indent ?preserve toml
