(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Utc = struct
  type t = Ptime.t

  (* RFC 9553 Section 1.4.5: fractional second values "MUST NOT be included
     unless they are non-zero, and they MUST NOT have trailing zeros". *)
  let frac_string t =
    let _, sub = Ptime.Span.to_d_ps (Ptime.frac_s t) in
    if Int64.equal sub 0L then ""
    else
      let digits = Printf.sprintf "%012Ld" sub in
      let last = ref (String.length digits - 1) in
      while !last > 0 && digits.[!last] = '0' do
        decr last
      done;
      "." ^ String.sub digits 0 (!last + 1)

  let to_string t =
    let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ~tz_offset_s:0 t in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d%sZ" y m d hh mm ss
      (frac_string t)

  let of_string s =
    match Ptime.of_rfc3339 ~strict:true s with
    | Ok (t, _, _) -> Ok t
    | Error _ ->
        Error
          (Printf.sprintf
             "%S is not a UTCDateTime, which is an RFC 3339 date-time with \
              uppercase letters and an offset, canonically \"Z\""
             s)

  let is_canonical s =
    match of_string s with
    | Ok t -> String.equal s (to_string t)
    | Error _ -> false

  let equal = Ptime.equal
  let compare = Ptime.compare
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  let jsont = Jsont.of_of_string ~kind:"UTCDateTime" ~enc:to_string of_string
end

module Partial_date = struct
  type t = {
    year : int option;
    month : int option;
    day : int option;
    calendar_scale : string option;
    unknown : Jscontact_unknown.t;
  }

  let make ?year ?month ?day ?calendar_scale
      ?(unknown = Jscontact_unknown.empty) () =
    { year; month; day; calendar_scale; unknown }

  let equal a b =
    a.year = b.year && a.month = b.month && a.day = b.day
    && a.calendar_scale = b.calendar_scale
    && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf d =
    let field ppf = function
      | None -> Format.pp_print_string ppf "-"
      | Some i -> Format.pp_print_int ppf i
    in
    Format.fprintf ppf "@[%a-%a-%a@]" field d.year field d.month field d.day

  let kind = "PartialDate"

  let validate d =
    Jscontact_valid.(
      let* () =
        match d.year with
        | None -> ok ()
        | Some y -> check (y >= 0) "year: %d is negative" y
      in
      let* () =
        match d.month with
        | None -> ok ()
        | Some m ->
            let* () =
              check (m >= 1 && m <= 12) "month: %d is outside 1 to 12" m
            in
            check
              (d.year <> None || d.day <> None)
              "month: is set but neither year nor day is"
      in
      let* () =
        match d.day with
        | None -> ok ()
        | Some day ->
            let* () =
              check (day >= 1 && day <= 31) "day: %d is outside 1 to 31" day
            in
            check (d.month <> None) "day: is set but month is not"
      in
      let* () =
        match d.calendar_scale with
        | None -> ok ()
        | Some c ->
            (* RFC 9553 Section 2.8.1 requires the name in lowercase. A CLDR
               calendar system name registered by RFC 7529 is then lowercase
               ASCII letters, digits and hyphens, hyphens joining two of the
               others. The registry itself is not carried here. *)
            let is_cldr_char ch =
              (ch >= 'a' && ch <= 'z')
              || Jscontact_ascii.is_digit ch
              || ch = '-'
            in
            let is_alnum ch = ch <> '-' && is_cldr_char ch in
            let is_cldr c =
              c <> ""
              && String.for_all is_cldr_char c
              && is_alnum c.[0]
              && is_alnum c.[String.length c - 1]
            in
            check
              (is_cldr c || Jscontact_vendor.is_extension c)
              "calendarScale: %S is neither a CLDR calendar system name nor a \
               vendor-specific value"
              c
      in
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind d.unknown) in
      ok d)

  let ctor year month day calendar_scale unknown =
    { year; month; day; calendar_scale; unknown }

  let mems m =
    let u = Jscontact_json.unsigned in
    m
    |> Jsont.Object.opt_mem "year" (u ~kind:"year") ~enc:(fun t -> t.year)
    |> Jsont.Object.opt_mem "month" (u ~kind:"month") ~enc:(fun t -> t.month)
    |> Jsont.Object.opt_mem "day" (u ~kind:"day") ~enc:(fun t -> t.day)
    |> Jsont.Object.opt_mem "calendarScale" Jsont.string ~enc:(fun t ->
        t.calendar_scale)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> mems |> Jsont.Object.finish

  let case_jsont = Jsont.Object.map ~kind ctor |> mems |> Jsont.Object.finish
end

module Timestamp = struct
  type t = { utc : Utc.t; unknown : Jscontact_unknown.t }

  let make ?(unknown = Jscontact_unknown.empty) utc = { utc; unknown }

  let equal a b =
    Utc.equal a.utc b.utc && Jscontact_unknown.equal a.unknown b.unknown

  let pp ppf t = Utc.pp ppf t.utc
  let kind = "Timestamp"

  let validate t =
    Jscontact_valid.(
      let* _ = in_ kind (Jscontact_unknown.validate ~in_type:kind t.unknown) in
      ok t)

  let ctor utc unknown = { utc; unknown }

  let mems m =
    m
    |> Jsont.Object.mem "utc" Utc.jsont ~enc:(fun t -> t.utc)
    |> Jsont.Object.keep_unknown Jscontact_unknown.mems ~enc:(fun t ->
        t.unknown)

  let jsont =
    Jsont.Object.map ~kind (fun () -> ctor)
    |> Jscontact_json.type_mem kind
    |> mems |> Jsont.Object.finish

  let case_jsont = Jsont.Object.map ~kind ctor |> mems |> Jsont.Object.finish
end

type t = Partial of Partial_date.t | Timestamp of Timestamp.t

let equal a b =
  match (a, b) with
  | Partial a, Partial b -> Partial_date.equal a b
  | Timestamp a, Timestamp b -> Timestamp.equal a b
  | _ -> false

let pp ppf = function
  | Partial d -> Partial_date.pp ppf d
  | Timestamp t -> Timestamp.pp ppf t

let validate = function
  | Partial d -> Result.map (fun d -> Partial d) (Partial_date.validate d)
  | Timestamp t -> Result.map (fun t -> Timestamp t) (Timestamp.validate t)

(* RFC 9553 Section 1.3.4: a property typed "A|B (defaultType: A)" implies A
   when @type is absent, and requires @type on an instance of B. *)
let jsont =
  let partial =
    Jsont.Object.Case.map "PartialDate" Partial_date.case_jsont ~dec:(fun d ->
        Partial d)
  in
  let timestamp =
    Jsont.Object.Case.map "Timestamp" Timestamp.case_jsont ~dec:(fun t ->
        Timestamp t)
  in
  let enc_case = function
    | Partial d -> Jsont.Object.Case.value partial d
    | Timestamp t -> Jsont.Object.Case.value timestamp t
  in
  let cases = Jsont.Object.Case.[ make partial; make timestamp ] in
  Jsont.Object.map ~kind:"Date" Fun.id
  |> Jsont.Object.case_mem "@type" Jsont.string ~dec_absent:"PartialDate"
       ~enc:Fun.id ~enc_case
       ~enc_omit:(fun tag -> String.equal tag "PartialDate")
       cases
  |> Jsont.Object.finish
