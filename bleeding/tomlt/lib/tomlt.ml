(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative TOML codecs *)

(* ---- Preliminaries ---- *)

type 'a fmt = Format.formatter -> 'a -> unit

(* Local result syntax for binding operators *)
module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r
end

module Sort = struct
  type t =
    | String
    | Int
    | Float
    | Bool
    | Datetime
    | Datetime_local
    | Date
    | Time
    | Array
    | Table

  let to_string = function
    | String -> "string"
    | Int -> "integer"
    | Float -> "float"
    | Bool -> "boolean"
    | Datetime -> "datetime"
    | Datetime_local -> "datetime-local"
    | Date -> "date-local"
    | Time -> "time-local"
    | Array -> "array"
    | Table -> "table"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let of_toml = function
    | Toml.String _ -> String
    | Toml.Int _ -> Int
    | Toml.Float _ -> Float
    | Toml.Bool _ -> Bool
    | Toml.Datetime _ -> Datetime
    | Toml.Datetime_local _ -> Datetime_local
    | Toml.Date_local _ -> Date
    | Toml.Time_local _ -> Time
    | Toml.Array _ -> Array
    | Toml.Table _ -> Table

  let or_kind ~kind sort =
    if kind = "" then to_string sort else kind

  let kinded ~kind sort =
    if kind = "" then to_string sort
    else kind ^ " " ^ to_string sort
end

let ( <?> ) c c' = if c <> 0 then c else c'

(* Find first char matching predicate *)
let string_index_opt p s =
  let len = String.length s in
  let rec loop i =
    if i >= len then None
    else if p s.[i] then Some i
    else loop (i + 1)
  in
  loop 0

(* Find separator (T, t, or space) for datetime parsing *)
let find_datetime_sep s =
  string_index_opt (fun c -> c = 'T' || c = 't' || c = ' ') s

(* ---- Datetime structured types ---- *)

module Tz = struct
  type t =
    | UTC
    | Offset of { hours : int; minutes : int }

  let utc = UTC
  let offset ~hours ~minutes = Offset { hours; minutes }

  let equal a b = match a, b with
    | UTC, UTC -> true
    | Offset { hours = h1; minutes = m1 }, Offset { hours = h2; minutes = m2 } ->
        h1 = h2 && m1 = m2
    | _ -> false

  let compare a b = match a, b with
    | UTC, UTC -> 0
    | UTC, Offset _ -> -1
    | Offset _, UTC -> 1
    | Offset { hours = h1; minutes = m1 }, Offset { hours = h2; minutes = m2 } ->
        Int.compare h1 h2 <?> Int.compare m1 m2

  let to_string = function
    | UTC -> "Z"
    | Offset { hours; minutes } ->
        let sign = if hours >= 0 then '+' else '-' in
        Printf.sprintf "%c%02d:%02d" sign (abs hours) (abs minutes)

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let of_string s =
    let len = String.length s in
    if len = 0 then Error "empty timezone"
    else if s = "Z" || s = "z" then Ok UTC
    else if len >= 5 then
      let sign = if s.[0] = '-' then -1 else 1 in
      let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
      try
        let hours = int_of_string (String.sub s start 2) * sign in
        let minutes = int_of_string (String.sub s (start + 3) 2) in
        Ok (Offset { hours; minutes })
      with _ -> Error ("invalid timezone: " ^ s)
    else Error ("invalid timezone: " ^ s)
end

module Date = struct
  type t = { year : int; month : int; day : int }

  let make ~year ~month ~day = { year; month; day }

  let equal a b = a.year = b.year && a.month = b.month && a.day = b.day

  let compare a b =
    Int.compare a.year b.year
    <?> Int.compare a.month b.month
    <?> Int.compare a.day b.day

  let to_string d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day

  let pp fmt d = Format.pp_print_string fmt (to_string d)

  let of_string s =
    if String.length s < 10 then Error "date too short"
    else
      try
        let year = int_of_string (String.sub s 0 4) in
        let month = int_of_string (String.sub s 5 2) in
        let day = int_of_string (String.sub s 8 2) in
        Ok { year; month; day }
      with _ -> Error ("invalid date: " ^ s)
end

module Time = struct
  type t = {
    hour : int;
    minute : int;
    second : int;
    frac : float;
  }

  let make ~hour ~minute ~second ?(frac = 0.0) () =
    { hour; minute; second; frac }

  let equal a b =
    a.hour = b.hour && a.minute = b.minute &&
    a.second = b.second && a.frac = b.frac

  let compare a b =
    Int.compare a.hour b.hour
    <?> Int.compare a.minute b.minute
    <?> Int.compare a.second b.second
    <?> Float.compare a.frac b.frac

  (* Remove trailing zeros from a string, keeping at least one char *)
  let rstrip_zeros s =
    let rec find_end i =
      if i <= 0 then 1
      else if s.[i] <> '0' then i + 1
      else find_end (i - 1)
    in
    String.sub s 0 (find_end (String.length s - 1))

  let to_string t =
    match t.frac with
    | 0.0 -> Printf.sprintf "%02d:%02d:%02d" t.hour t.minute t.second
    | frac ->
        (* Format fractional seconds: "0.123456789" -> "123456789" -> trim zeros *)
        let frac_str = Printf.sprintf "%.9f" frac in
        let frac_digits = String.sub frac_str 2 (String.length frac_str - 2) in
        Printf.sprintf "%02d:%02d:%02d.%s" t.hour t.minute t.second (rstrip_zeros frac_digits)

  let pp fmt t = Format.pp_print_string fmt (to_string t)

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
      with _ -> Error ("invalid time: " ^ s)
end

module Datetime = struct
  type t = { date : Date.t; time : Time.t; tz : Tz.t }

  let make ~date ~time ~tz = { date; time; tz }

  let equal a b =
    Date.equal a.date b.date && Time.equal a.time b.time && Tz.equal a.tz b.tz

  let compare a b =
    Date.compare a.date b.date
    <?> Time.compare a.time b.time
    <?> Tz.compare a.tz b.tz

  let to_string dt =
    Printf.sprintf "%sT%s%s"
      (Date.to_string dt.date)
      (Time.to_string dt.time)
      (Tz.to_string dt.tz)

  let pp fmt dt = Format.pp_print_string fmt (to_string dt)

  let of_string s =
    let open Result_syntax in
    match find_datetime_sep s with
    | None -> Error "missing date/time separator"
    | Some idx ->
        let date_str = String.sub s 0 idx in
        let rest = String.sub s (idx + 1) (String.length s - idx - 1) in
        (* Find timezone: Z, z, +, or - (but not - in first 2 chars of time) *)
        let is_tz_start i c = c = 'Z' || c = 'z' || c = '+' || (c = '-' && i > 2) in
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

  let make ~date ~time = { date; time }

  let equal a b = Date.equal a.date b.date && Time.equal a.time b.time

  let compare a b =
    Date.compare a.date b.date <?> Time.compare a.time b.time

  let to_string dt =
    Printf.sprintf "%sT%s" (Date.to_string dt.date) (Time.to_string dt.time)

  let pp fmt dt = Format.pp_print_string fmt (to_string dt)

  let of_string s =
    let open Result_syntax in
    match find_datetime_sep s with
    | None -> Error "missing date/time separator"
    | Some idx ->
        let date_str = String.sub s 0 idx in
        let time_str = String.sub s (idx + 1) (String.length s - idx - 1) in
        let* date = Date.of_string date_str in
        let+ time = Time.of_string time_str in
        { date; time }
end

(* ---- Codec error type ---- *)

type codec_error =
  | Type_mismatch of { expected : string; got : string }
  | Missing_member of string
  | Unknown_member of string [@warning "-37"]
  | Value_error of string
  | Int_overflow of int64
  | Parse_error of string [@warning "-37"]

let codec_error_to_string = function
  | Type_mismatch { expected; got } ->
      Printf.sprintf "type mismatch: expected %s, got %s" expected got
  | Missing_member name ->
      Printf.sprintf "missing required member: %s" name
  | Unknown_member name ->
      Printf.sprintf "unknown member: %s" name
  | Value_error msg -> msg
  | Int_overflow n ->
      Printf.sprintf "integer overflow: %Ld" n
  | Parse_error msg ->
      Printf.sprintf "parse error: %s" msg

(* ---- Codec type ---- *)

type 'a t = {
  kind : string;
  doc : string;
  dec : Toml.t -> ('a, codec_error) result;
  enc : 'a -> Toml.t;
}

let kind c = c.kind
let doc c = c.doc

let with_doc ?kind:k ?doc:d c =
  { c with
    kind = Option.value ~default:c.kind k;
    doc = Option.value ~default:c.doc d }

(* ---- Type helpers ---- *)

let type_name = function
  | Toml.String _ -> "string"
  | Toml.Int _ -> "integer"
  | Toml.Float _ -> "float"
  | Toml.Bool _ -> "boolean"
  | Toml.Datetime _ -> "datetime"
  | Toml.Datetime_local _ -> "datetime-local"
  | Toml.Date_local _ -> "date-local"
  | Toml.Time_local _ -> "time-local"
  | Toml.Array _ -> "array"
  | Toml.Table _ -> "table"

(* Helpers for codec error construction *)
let type_error ~expected v =
  Error (Type_mismatch { expected; got = type_name v })

let value_error msg = Error (Value_error msg)
let int_overflow n = Error (Int_overflow n)
let missing_member name = Error (Missing_member name)

(* ---- Base codecs ---- *)

let bool = {
  kind = "boolean";
  doc = "";
  dec = (function
    | Toml.Bool b -> Ok b
    | v -> type_error ~expected:"boolean" v);
  enc = (fun b -> Toml.Bool b);
}

let int = {
  kind = "integer";
  doc = "";
  dec = (function
    | Toml.Int i ->
        if i >= Int64.of_int min_int && i <= Int64.of_int max_int then
          Ok (Int64.to_int i)
        else int_overflow i
    | v -> type_error ~expected:"integer" v);
  enc = (fun i -> Toml.Int (Int64.of_int i));
}

let int32 = {
  kind = "integer";
  doc = "";
  dec = (function
    | Toml.Int i ->
        if i >= Int64.of_int32 Int32.min_int && i <= Int64.of_int32 Int32.max_int then
          Ok (Int64.to_int32 i)
        else int_overflow i
    | v -> type_error ~expected:"integer" v);
  enc = (fun i -> Toml.Int (Int64.of_int32 i));
}

let int64 = {
  kind = "integer";
  doc = "";
  dec = (function
    | Toml.Int i -> Ok i
    | v -> type_error ~expected:"integer" v);
  enc = (fun i -> Toml.Int i);
}

let float = {
  kind = "float";
  doc = "";
  dec = (function
    | Toml.Float f -> Ok f
    | v -> type_error ~expected:"float" v);
  enc = (fun f -> Toml.Float f);
}

let number = {
  kind = "number";
  doc = "";
  dec = (function
    | Toml.Float f -> Ok f
    | Toml.Int i -> Ok (Int64.to_float i)
    | v -> type_error ~expected:"number" v);
  enc = (fun f -> Toml.Float f);
}

let string = {
  kind = "string";
  doc = "";
  dec = (function
    | Toml.String s -> Ok s
    | v -> type_error ~expected:"string" v);
  enc = (fun s -> Toml.String s);
}

let int_as_string = {
  kind = "integer (as string)";
  doc = "";
  dec = (function
    | Toml.String s ->
        (match int_of_string_opt s with
        | Some i -> Ok i
        | None -> value_error ("cannot parse integer: " ^ s))
    | v -> type_error ~expected:"string" v);
  enc = (fun i -> Toml.String (Int.to_string i));
}

let int64_as_string = {
  kind = "int64 (as string)";
  doc = "";
  dec = (function
    | Toml.String s ->
        (match Int64.of_string_opt s with
        | Some i -> Ok i
        | None -> value_error ("cannot parse int64: " ^ s))
    | v -> type_error ~expected:"string" v);
  enc = (fun i -> Toml.String (Int64.to_string i));
}

(* ---- Ptime codecs ---- *)

(* Helper to get current timezone offset from explicit value or function *)
let get_tz_offset ?tz_offset_s ?get_tz () =
  tz_offset_s
  |> Option.fold ~none:(Option.bind get_tz (fun f -> f ())) ~some:Option.some
  |> Option.value ~default:0  (* Default to UTC when no timezone source provided *)

(* Helper to get today's date in the given timezone *)
let today_date ?now tz_offset_s =
  let t = Option.fold ~none:Ptime.epoch ~some:(fun f -> f ()) now in
  Ptime.to_date ~tz_offset_s t

(* Helper to create a ptime from date at midnight *)
let ptime_of_date ?(tz_offset_s = 0) (year, month, day) =
  match Ptime.of_date_time ((year, month, day), ((0, 0, 0), tz_offset_s)) with
  | Some t -> t
  | None ->
      (* Fallback to epoch if date is invalid *)
      Ptime.epoch

(* Helper to create a ptime from time on today's date *)
let ptime_of_time ?now ~tz_offset_s ~hour ~minute ~second ~ns () =
  let frac = Float.of_int ns /. 1_000_000_000.0 in
  let date = today_date ?now tz_offset_s in
  let time = ((hour, minute, second), tz_offset_s) in
  match Ptime.of_date_time (date, time) with
  | Some t ->
      (* Add fractional seconds *)
      (match Ptime.Span.of_float_s frac with
      | Some span -> Option.value ~default:t (Ptime.add_span t span)
      | None -> t)
  | None -> Ptime.epoch

(* Unified ptime codec - accepts any TOML datetime, fills in defaults *)
let ptime ?tz_offset_s ?get_tz ?now ?(frac_s = 0) () =
  let tz () = get_tz_offset ?tz_offset_s ?get_tz () in
  {
    kind = "datetime (ptime)";
    doc = "";
    dec = (fun v ->
      let tz_s = tz () in
      match v with
      | Toml.Datetime _ ->
          (match Toml.to_ptime_opt v with
          | Some t -> Ok t
          | None -> value_error "cannot parse offset datetime")
      | Toml.Datetime_local _ ->
          (match Toml.to_ptime_datetime ~tz_offset_s:tz_s v with
          | Some (`Datetime_local t) -> Ok t
          | _ -> value_error "cannot parse local datetime")
      | Toml.Date_local _ ->
          (match Toml.to_date_opt v with
          | Some date -> Ok (ptime_of_date ~tz_offset_s:tz_s date)
          | None -> value_error "cannot parse local date")
      | Toml.Time_local _ ->
          (match Toml.to_ptime_datetime ~tz_offset_s:tz_s v with
          | Some (`Time (h, m, s, ns)) ->
              Ok (ptime_of_time ?now ~tz_offset_s:tz_s ~hour:h ~minute:m ~second:s ~ns ())
          | _ -> value_error "cannot parse local time")
      | v -> type_error ~expected:"datetime" v);
    enc = (fun t -> Toml.datetime_of_ptime ~tz_offset_s:(tz ()) ~frac_s t);
  }

(* Strict ptime codec - only accepts offset datetimes *)
let ptime_opt ?(tz_offset_s = 0) ?(frac_s = 0) () = {
  kind = "datetime (ptime offset only)";
  doc = "";
  dec = (function
    | Toml.Datetime _ as v ->
        (match Toml.to_ptime_opt v with
        | Some t -> Ok t
        | None -> value_error "cannot parse offset datetime")
    | Toml.Datetime_local _ ->
        value_error "local datetime requires timezone; use ptime() instead"
    | Toml.Date_local _ ->
        value_error "local date requires timezone; use ptime() instead"
    | Toml.Time_local _ ->
        value_error "local time requires timezone; use ptime() instead"
    | v -> type_error ~expected:"datetime" v);
  enc = (fun t -> Toml.datetime_of_ptime ~tz_offset_s ~frac_s t);
}

(* Ptime span codec for local times (duration from midnight) *)
let ptime_span = {
  kind = "time-local (ptime span)";
  doc = "";
  dec = (function
    | Toml.Time_local _ as v ->
        (match Toml.to_ptime_datetime v with
        | Some (`Time (h, m, s, ns)) ->
            let total_secs = (h * 3600) + (m * 60) + s in
            let frac = Float.of_int ns /. 1_000_000_000.0 in
            (match Ptime.Span.of_float_s (Float.of_int total_secs +. frac) with
            | Some span -> Ok span
            | None -> value_error "cannot create span from time")
        | _ -> value_error "cannot parse local time")
    | v -> type_error ~expected:"time-local" v);
  enc = (fun span ->
    let secs = Ptime.Span.to_float_s span in
    (* Clamp to 0-24 hours *)
    let secs = Float.max 0.0 (Float.min secs 86399.999999999) in
    let total_secs = Float.to_int secs in
    let frac = secs -. Float.of_int total_secs in
    let h = total_secs / 3600 in
    let m = (total_secs mod 3600) / 60 in
    let s = total_secs mod 60 in
    if frac > 0.0 then
      Toml.Time_local (Printf.sprintf "%02d:%02d:%02d%s" h m s
        (String.sub (Printf.sprintf "%.9f" frac) 1 10))
    else
      Toml.Time_local (Printf.sprintf "%02d:%02d:%02d" h m s));
}

(* Ptime date codec *)
let ptime_date = {
  kind = "date-local (ptime)";
  doc = "";
  dec = (function
    | Toml.Date_local _ as v ->
        (match Toml.to_date_opt v with
        | Some d -> Ok d
        | None -> value_error "cannot parse local date")
    | v -> type_error ~expected:"date-local" v);
  enc = (fun (year, month, day) ->
    Toml.Date_local (Printf.sprintf "%04d-%02d-%02d" year month day));
}

(* Full ptime datetime codec - preserves variant information *)
let ptime_full ?tz_offset_s ?get_tz () =
  let tz_offset_s =
    Option.fold ~none:(Option.bind get_tz (fun f -> f ())) ~some:Option.some tz_offset_s
  in
  {
    kind = "datetime (unified ptime)";
    doc = "";
    dec = (fun v ->
      match Toml.to_ptime_datetime ?tz_offset_s v with
      | Some pdt -> Ok pdt
      | None ->
          match v with
          | Toml.Datetime _ | Toml.Datetime_local _
          | Toml.Date_local _ | Toml.Time_local _ ->
              value_error "cannot parse datetime"
          | _ -> type_error ~expected:"datetime" v);
    enc = Toml.ptime_datetime_to_toml;
  }

(* ---- Combinators ---- *)

let map ?kind:k ?doc:d ?dec ?enc c =
  let kind = Option.value ~default:c.kind k in
  let doc = Option.value ~default:c.doc d in
  let dec_fn = match dec with
    | Some f -> fun v -> Result.map f (c.dec v)
    | None -> fun _ -> value_error "decode not supported"
  in
  let enc_fn = match enc with
    | Some f -> fun v -> c.enc (f v)
    | None -> fun _ -> failwith "encode not supported"
  in
  { kind; doc; dec = dec_fn; enc = enc_fn }

let const ?kind ?doc v =
  let kind = Option.value ~default:"constant" kind in
  let doc = Option.value ~default:"" doc in
  { kind; doc; dec = (fun _ -> Ok v); enc = (fun _ -> Toml.Table []) }

let enum ?cmp ?kind ?doc assoc =
  let cmp = Option.value ~default:Stdlib.compare cmp in
  let kind = Option.value ~default:"enum" kind in
  let doc = Option.value ~default:"" doc in
  let rev_assoc = List.map (fun (s, v) -> (v, s)) assoc in
  {
    kind; doc;
    dec = (function
      | Toml.String s ->
          (match List.assoc_opt s assoc with
          | Some v -> Ok v
          | None -> value_error ("unknown enum value: " ^ s))
      | v -> type_error ~expected:"string" v);
    enc = (fun v ->
      match List.find_opt (fun (v', _) -> cmp v v' = 0) rev_assoc with
      | Some (_, s) -> Toml.String s
      | None -> failwith "enum value not in association list");
  }

let option ?kind ?doc c =
  let kind = Option.value ~default:("optional " ^ c.kind) kind in
  let doc = Option.value ~default:c.doc doc in
  {
    kind; doc;
    dec = (fun v -> Result.map Option.some (c.dec v));
    enc = (function
      | Some v -> c.enc v
      | None -> Toml.Table []);  (* Should not be called for None *)
  }

let result ~ok ~error =
  {
    kind = ok.kind ^ " or " ^ error.kind;
    doc = "";
    dec = (fun v ->
      match ok.dec v with
      | Ok x -> Ok (Ok x)
      | Error _ ->
          match error.dec v with
          | Ok x -> Ok (Error x)
          | Error e -> Error e);
    enc = (function
      | Ok x -> ok.enc x
      | Error x -> error.enc x);
  }

let rec' lazy_c =
  {
    kind = "recursive";
    doc = "";
    dec = (fun v -> (Lazy.force lazy_c).dec v);
    enc = (fun v -> (Lazy.force lazy_c).enc v);
  }

let iter ?kind ?doc ?dec ?enc c =
  let kind = Option.value ~default:c.kind kind in
  let doc = Option.value ~default:c.doc doc in
  {
    kind;
    doc;
    dec = (fun v ->
      match c.dec v with
      | Ok x ->
          (match dec with Some f -> f x | None -> ());
          Ok x
      | Error e -> Error e);
    enc = (fun x ->
      (match enc with Some f -> f x | None -> ());
      c.enc x);
  }

let recode ~dec:dec_codec f ~enc:enc_codec =
  {
    kind = dec_codec.kind;
    doc = dec_codec.doc;
    dec = (fun v ->
      match dec_codec.dec v with
      | Ok x -> Ok (f x)
      | Error e -> Error e);
    enc = enc_codec.enc;
  }

(* ---- Query combinators ---- *)

let nth ?absent n elt_codec =
  {
    kind = elt_codec.kind;
    doc = "";
    dec = (function
      | Toml.Array arr ->
          if n >= 0 && n < List.length arr then
            elt_codec.dec (List.nth arr n)
          else
            (match absent with
            | Some v -> Ok v
            | None -> value_error (Printf.sprintf "array index %d out of bounds" n))
      | v -> type_error ~expected:"array" v);
    enc = (fun x -> Toml.Array [elt_codec.enc x]);
  }

let mem ?absent name value_codec =
  {
    kind = value_codec.kind;
    doc = "";
    dec = (function
      | Toml.Table pairs ->
          (match List.assoc_opt name pairs with
          | Some v -> value_codec.dec v
          | None ->
              match absent with
              | Some v -> Ok v
              | None -> value_error (Printf.sprintf "missing member %S" name))
      | v -> type_error ~expected:"table" v);
    enc = (fun x -> Toml.Table [(name, value_codec.enc x)]);
  }

let fold_array elt_codec f init =
  {
    kind = "array";
    doc = "";
    dec = (function
      | Toml.Array arr ->
          let rec loop acc i = function
            | [] -> Ok acc
            | x :: xs ->
                match elt_codec.dec x with
                | Ok v -> loop (f i v acc) (i + 1) xs
                | Error e -> Error e
          in
          loop init 0 arr
      | v -> type_error ~expected:"array" v);
    enc = (fun _ -> Toml.Array []);  (* Encoding not supported for folds *)
  }

let fold_table value_codec f init =
  {
    kind = "table";
    doc = "";
    dec = (function
      | Toml.Table pairs ->
          let rec loop acc = function
            | [] -> Ok acc
            | (k, v) :: rest ->
                match value_codec.dec v with
                | Ok x -> loop (f k x acc) rest
                | Error e -> Error e
          in
          loop init pairs
      | v -> type_error ~expected:"table" v);
    enc = (fun _ -> Toml.Table []);  (* Encoding not supported for folds *)
  }

(* ---- Ignoring and placeholders ---- *)

let ignore = {
  kind = "ignored";
  doc = "";
  dec = (fun _ -> Ok ());
  enc = (fun () -> failwith "cannot encode ignored value");
}

let zero = {
  kind = "zero";
  doc = "";
  dec = (fun _ -> Ok ());
  enc = (fun () -> Toml.Table []);
}

let todo ?kind ?doc ?dec_stub () =
  let kind = Option.value ~default:"todo" kind in
  let doc = Option.value ~default:"" doc in
  {
    kind;
    doc;
    dec = (fun _ ->
      match dec_stub with
      | Some v -> Ok v
      | None -> value_error "TODO: codec not implemented");
    enc = (fun _ -> failwith "TODO: codec not implemented");
  }

(* ---- Array codecs ---- *)

module Array = struct
  type 'a codec = 'a t

  type ('array, 'elt) enc = {
    fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc
  }

  type ('array, 'elt, 'builder) map = {
    kind : string;
    doc : string;
    elt : 'elt codec;
    dec_empty : unit -> 'builder;
    dec_add : 'elt -> 'builder -> 'builder;
    dec_finish : 'builder -> 'array;
    enc : ('array, 'elt) enc;
  }

  let map ?kind ?doc
      ?(dec_empty = fun () -> failwith "decode not supported")
      ?(dec_add = fun _ _ -> failwith "decode not supported")
      ?(dec_finish = fun _ -> failwith "decode not supported")
      ?(enc = { fold = fun _ _ _ -> failwith "encode not supported" })
      (elt : 'elt codec) : ('array, 'elt, 'builder) map =
    let kind = Option.value ~default:("array of " ^ elt.kind) kind in
    let doc = Option.value ~default:"" doc in
    { kind; doc; elt; dec_empty; dec_add; dec_finish; enc }

  let list ?kind ?doc (elt : 'a codec) : ('a list, 'a, 'a list) map =
    let kind = Option.value ~default:("list of " ^ elt.kind) kind in
    let doc = Option.value ~default:"" doc in
    {
      kind; doc; elt;
      dec_empty = (fun () -> []);
      dec_add = (fun x xs -> x :: xs);
      dec_finish = List.rev;
      enc = { fold = (fun f acc xs -> List.fold_left f acc xs) };
    }

  let array ?kind ?doc (elt : 'a codec) : ('a array, 'a, 'a list) map =
    let kind = Option.value ~default:("array of " ^ elt.kind) kind in
    let doc = Option.value ~default:"" doc in
    {
      kind; doc; elt;
      dec_empty = (fun () -> []);
      dec_add = (fun x xs -> x :: xs);
      dec_finish = (fun xs -> Stdlib.Array.of_list (List.rev xs));
      enc = { fold = (fun f acc arr -> Stdlib.Array.fold_left f acc arr) };
    }

  let finish m =
    {
      kind = m.kind;
      doc = m.doc;
      dec = (function
        | Toml.Array items ->
            let rec decode_items builder = function
              | [] -> Ok (m.dec_finish builder)
              | item :: rest ->
                  match m.elt.dec item with
                  | Ok v -> decode_items (m.dec_add v builder) rest
                  | Error e -> Error e
            in
            decode_items (m.dec_empty ()) items
        | v -> type_error ~expected:"array" v);
      enc = (fun arr ->
        let items = m.enc.fold (fun acc elt -> m.elt.enc elt :: acc) [] arr in
        Toml.Array (List.rev items));
    }
end

let list ?kind ?doc c = Array.(finish (list ?kind ?doc c))
let array ?kind ?doc c = Array.(finish (array ?kind ?doc c))

(* ---- Table codecs ---- *)

module Table = struct
  type 'a codec = 'a t

  (* Unknown member handling *)
  type unknown_handling =
    | Skip
    | Error_on_unknown
    | Keep of (string -> Toml.t -> unit)  (* Callback to collect *)

  (* Member specification - existential type for storing typed member info *)
  type 'o mem_encoder = {
    mem_enc : 'o -> Toml.t;
    mem_should_omit : 'o -> bool;
  }

  type ('o, 'a) mem_spec = {
    name : string;
    mem_doc : string;
    mem_codec : 'a codec;
    dec_absent : 'a option;
    enc_typed : 'o mem_encoder option;
  }

  (* Helper to create enc_typed from encoder and optional omit function *)
  let make_enc_typed (codec : 'a codec) enc enc_omit =
    match enc with
    | None -> None
    | Some f ->
        let omit = Option.value ~default:(fun _ -> false) enc_omit in
        Some {
          mem_enc = (fun o -> codec.enc (f o));
          mem_should_omit = (fun o -> omit (f o));
        }

  module Mem = struct
    type 'a codec = 'a t

    type ('o, 'a) t = ('o, 'a) mem_spec

    let v ?doc ?(dec_absent : 'a option) ?enc ?enc_omit name (codec : 'a codec) =
      { name;
        mem_doc = Option.value ~default:"" doc;
        mem_codec = codec;
        dec_absent;
        enc_typed = make_enc_typed codec enc enc_omit }

    let opt ?doc ?enc name (codec : 'a codec) =
      let opt_codec = option codec in
      { name;
        mem_doc = Option.value ~default:"" doc;
        mem_codec = opt_codec;
        dec_absent = Some None;
        enc_typed = make_enc_typed opt_codec enc (Some Option.is_none) }
  end

  (* Map state for building table codecs *)
  type ('o, 'dec) map = {
    map_kind : string;
    map_doc : string;
    members : ('o, Toml.t) mem_spec list;  (* Stored in reverse order *)
    dec : Toml.t list -> ('dec, codec_error) result;
    unknown : unknown_handling;
    keep_unknown_enc : ('o -> (string * Toml.t) list) option;
  }

  let obj ?kind ?doc dec =
    let kind = Option.value ~default:"table" kind in
    let doc = Option.value ~default:"" doc in
    {
      map_kind = kind;
      map_doc = doc;
      members = [];
      dec = (fun _ -> Ok dec);
      unknown = Skip;
      keep_unknown_enc = None;
    }

  let obj' ?kind ?doc dec_fn =
    let kind = Option.value ~default:"table" kind in
    let doc = Option.value ~default:"" doc in
    {
      map_kind = kind;
      map_doc = doc;
      members = [];
      dec = (fun _ -> Ok (dec_fn ()));
      unknown = Skip;
      keep_unknown_enc = None;
    }

  (* Marker to indicate a missing member with a default *)
  let missing_marker_str = "__TOMLT_MISSING_WITH_DEFAULT__"
  let missing_marker = Toml.String missing_marker_str

  let is_missing_marker = function
    | Toml.String s -> String.equal s missing_marker_str
    | _ -> false

  let mem ?doc ?dec_absent ?enc ?enc_omit name (c : 'a codec) m =
    (* Create a member spec that stores raw TOML for later processing *)
    let raw_spec = {
      name;
      mem_doc = Option.value ~default:"" doc;
      mem_codec = { kind = c.kind; doc = c.doc;
                    dec = (fun v -> Ok v); enc = (fun v -> v) };
      (* We use the marker value when member is missing but has a default *)
      dec_absent = Option.map (fun _ -> missing_marker) dec_absent;
      enc_typed = make_enc_typed c enc enc_omit;
    } in
    {
      m with
      members = raw_spec :: m.members;
      dec = (function
        | [] -> value_error "internal: not enough values"
        | v :: rest ->
            Result.bind (m.dec rest) @@ fun f ->
            (* Check if this is the missing marker - use default directly *)
            if is_missing_marker v then
              match dec_absent with
              | Some default -> Ok (f default)
              | None -> value_error "internal: missing marker without default"
            else
              Result.map f (c.dec v));
    }

  let opt_mem ?doc ?enc name (c : 'a codec) m =
      (* dec_absent parameter is ('a option) option.
         Some None means "the default decoded value is None : 'a option"
         None would mean "no default, member is required" *)
      let default : 'a option = None in
      mem ?doc ?enc ~dec_absent:default ~enc_omit:Option.is_none name (option c) m

  (* Unknown member handling *)
  module Mems = struct
    type 'a codec = 'a t

    type ('mems, 'a) enc = {
      fold : 'acc. ('acc -> string -> 'a -> 'acc) -> 'acc -> 'mems -> 'acc
    }

    type ('mems, 'a, 'builder) map = {
      mems_kind : string;
      mems_doc : string;
      elt : 'a codec;
      dec_empty : unit -> 'builder;
      dec_add : string -> 'a -> 'builder -> 'builder;
      dec_finish : 'builder -> 'mems;
      enc : ('mems, 'a) enc;
    }

    let map ?kind ?doc
        ?(dec_empty = fun () -> failwith "decode not supported")
        ?(dec_add = fun _ _ _ -> failwith "decode not supported")
        ?(dec_finish = fun _ -> failwith "decode not supported")
        ?(enc = { fold = fun _ _ _ -> failwith "encode not supported" })
        elt =
      let kind = Option.value ~default:("members of " ^ elt.kind) kind in
      let doc = Option.value ~default:"" doc in
      { mems_kind = kind; mems_doc = doc; elt; dec_empty; dec_add; dec_finish; enc }

    module StringMap = Map.Make(String)

    let string_map ?kind ?doc elt =
      let kind = Option.value ~default:("string map of " ^ elt.kind) kind in
      let doc = Option.value ~default:"" doc in
      {
        mems_kind = kind; mems_doc = doc; elt;
        dec_empty = (fun () -> []);
        dec_add = (fun k v acc -> (k, v) :: acc);
        dec_finish = (fun pairs ->
          List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty pairs);
        enc = { fold = (fun f acc m -> StringMap.fold (fun k v acc -> f acc k v) m acc) };
      }

    let assoc ?kind ?doc elt =
      let kind = Option.value ~default:("assoc of " ^ elt.kind) kind in
      let doc = Option.value ~default:"" doc in
      {
        mems_kind = kind; mems_doc = doc; elt;
        dec_empty = (fun () -> []);
        dec_add = (fun k v acc -> (k, v) :: acc);
        dec_finish = List.rev;
        enc = { fold = (fun f acc pairs -> List.fold_left (fun acc (k, v) -> f acc k v) acc pairs) };
      }
  end

  let skip_unknown m = { m with unknown = Skip }
  let error_unknown m = { m with unknown = Error_on_unknown }

  let keep_unknown ?enc mems m =
    (* Add a pseudo-member that collects unknown members *)
    let unknown_vals = ref [] in
    let collector name v =
      match mems.Mems.elt.dec v with
      | Ok decoded -> unknown_vals := (name, decoded) :: !unknown_vals
      | Error _ -> ()  (* Skip values that don't decode *)
    in
    (* Create a raw spec for unknown members *)
    let raw_spec = {
      name = "";  (* Special marker for unknown members *)
      mem_doc = "";
      mem_codec = { kind = "unknown"; doc = "";
                    dec = (fun _ -> Ok (Toml.Table []));
                    enc = (fun _ -> Toml.Table []) };
      dec_absent = Some (Toml.Table []);
      enc_typed = None;
    } in
    {
      m with
      members = raw_spec :: m.members;
      unknown = Keep collector;
      keep_unknown_enc = Option.map (fun f o ->
        let mems_val = f o in
        mems.Mems.enc.fold (fun acc k v -> (k, mems.Mems.elt.enc v) :: acc) [] mems_val
        |> List.rev
      ) enc;
      dec = (function
        | [] -> value_error "internal: not enough values"
        | _ :: rest ->
            Result.map (fun f ->
              let collected = mems.Mems.dec_finish (
                List.fold_left (fun acc (k, v) -> mems.Mems.dec_add k v acc)
                  (mems.Mems.dec_empty ())
                  (List.rev !unknown_vals)
              ) in
              unknown_vals := [];
              f collected
            ) (m.dec rest));
    }

  (* Check for duplicates in a list *)
  let find_dup xs =
    let rec loop seen = function
      | [] -> None
      | x :: rest -> if List.mem x seen then Some x else loop (x :: seen) rest
    in
    loop [] xs

  let finish_common ~inline m =
    let _ = inline in  (* For future inline table support *)
    (* members_ordered is for display (reversed to get declaration order) *)
    let members_ordered = List.rev m.members in
    let known_names =
      List.filter_map (fun spec -> if spec.name = "" then None else Some spec.name) members_ordered
    in
    (* Check for duplicate member names *)
    Option.iter (fun name -> invalid_arg ("duplicate member name: " ^ name)) (find_dup known_names);
    {
      kind = m.map_kind;
      doc = m.map_doc;
      dec = (function
        | Toml.Table pairs ->
            (* Build list of values in the order expected by the dec chain.
               m.members is in reverse declaration order, which matches
               how the dec chain was built (outer = last added). *)
            let vals = List.map (fun spec ->
              if spec.name = "" then
                (* Unknown members placeholder *)
                Toml.Table []
              else
                match List.assoc_opt spec.name pairs with
                | Some v -> v
                | None ->
                    match spec.dec_absent with
                    | Some default -> default
                    | None ->
                        (* Will cause error during decoding *)
                        Toml.Table []
            ) m.members in
            (* Check for unknown members *)
            (match m.unknown with
            | Skip -> ()
            | Error_on_unknown ->
                List.iter (fun (name, _) ->
                  if not (List.mem name known_names) then
                    raise (Toml.Error.Error (Toml.Error.make
                      (Toml.Error.Semantic (Toml.Error.Duplicate_key name))))
                ) pairs
            | Keep collector ->
                List.iter (fun (name, v) ->
                  if not (List.mem name known_names) then
                    collector name v
                ) pairs);
            (* Check for missing required members *)
            let missing = List.filter_map (fun spec ->
              if spec.name = "" then None
              else if spec.dec_absent = None &&
                      not (List.exists (fun (n, _) -> n = spec.name) pairs) then
                Some spec.name
              else None
            ) members_ordered in
            (match missing with
            | name :: _ -> missing_member name
            | [] -> m.dec vals)
        | v -> type_error ~expected:"table" v);
      enc = (fun o ->
        let pairs = List.filter_map (fun spec ->
          if spec.name = "" then None  (* Skip unknown member placeholder *)
          else
            match spec.enc_typed with
            | None -> None
            | Some enc_info ->
                (* Check should_omit on original object, not encoded value *)
                if enc_info.mem_should_omit o then None
                else Some (spec.name, enc_info.mem_enc o)
        ) members_ordered in
        (* Add unknown members if keep_unknown was used *)
        let pairs = match m.keep_unknown_enc with
          | None -> pairs
          | Some get_unknown -> pairs @ get_unknown o
        in
        Toml.Table pairs);
    }

  let finish m = finish_common ~inline:false m
  let inline m = finish_common ~inline:true m
end

(* ---- Array of tables ---- *)

let array_of_tables ?kind ?doc c =
  let kind = Option.value ~default:("array of " ^ c.kind) kind in
  let doc = Option.value ~default:"" doc in
  {
    kind; doc;
    dec = (function
      | Toml.Array items ->
          let rec decode_items acc = function
            | [] -> Ok (List.rev acc)
            | item :: rest ->
                match c.dec item with
                | Ok v -> decode_items (v :: acc) rest
                | Error e -> Error e
          in
          decode_items [] items
      | v -> type_error ~expected:"array" v);
    enc = (fun xs -> Toml.Array (List.map c.enc xs));
  }

(* ---- Any / Generic value codecs ---- *)

let value = {
  kind = "value";
  doc = "";
  dec = (fun v -> Ok v);
  enc = (fun v -> v);
}

let value_mems = {
  kind = "value members";
  doc = "";
  dec = (function
    | Toml.Table pairs -> Ok pairs
    | v -> type_error ~expected:"table" v);
  enc = (fun pairs -> Toml.Table pairs);
}

let any ?kind ?doc ?dec_string ?dec_int ?dec_float ?dec_bool
    ?dec_datetime ?dec_array ?dec_table ?enc () =
  let kind = Option.value ~default:"any" kind in
  let doc = Option.value ~default:"" doc in
  let type_error expected got =
    Error (Type_mismatch { expected; got = type_name got })
  in
  {
    kind; doc;
    dec = (fun v ->
      match v with
      | Toml.String _ ->
          (match dec_string with Some c -> c.dec v | None -> type_error "string" v)
      | Toml.Int _ ->
          (match dec_int with Some c -> c.dec v | None -> type_error "integer" v)
      | Toml.Float _ ->
          (match dec_float with Some c -> c.dec v | None -> type_error "float" v)
      | Toml.Bool _ ->
          (match dec_bool with Some c -> c.dec v | None -> type_error "boolean" v)
      | Toml.Datetime _ | Toml.Datetime_local _
      | Toml.Date_local _ | Toml.Time_local _ ->
          (match dec_datetime with Some c -> c.dec v | None -> type_error "datetime" v)
      | Toml.Array _ ->
          (match dec_array with Some c -> c.dec v | None -> type_error "array" v)
      | Toml.Table _ ->
          (match dec_table with Some c -> c.dec v | None -> type_error "table" v));
    enc = (fun v ->
      match enc with
      | Some selector -> (selector v).enc v
      | None -> failwith "any: enc not provided");
  }

let to_tomlt_error e =
  Toml.Error.make (Toml.Error.Semantic (Toml.Error.Duplicate_key (codec_error_to_string e)))

let decode c v = Result.map_error to_tomlt_error (c.dec v)

let decode_exn c v =
  match c.dec v with
  | Ok x -> x
  | Error e -> raise (Toml.Error.Error (to_tomlt_error e))

let encode c v = c.enc v

module Toml = Toml
module Error = Toml.Error
