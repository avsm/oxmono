(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let error fmt = Format.kasprintf (fun s -> Error s) fmt
let ( let* ) = Result.bind
let is_digit c = c >= '0' && c <= '9'

let parse_signed_int s =
  let n = String.length s in
  if n = 0 then None
  else
    let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    if
      start >= n
      || not (String.for_all is_digit (String.sub s start (n - start)))
    then None
    else int_of_string_opt s

(* RFC 5545 Section 3.3.10 gives COUNT, INTERVAL, BYSECOND, BYMINUTE, BYHOUR
   and BYMONTH as bare digits, so a sign is not a valid wire form for them. *)
let parse_unsigned_int s =
  if s = "" || not (String.for_all is_digit s) then None
  else int_of_string_opt s

type freq =
  [ `Secondly | `Minutely | `Hourly | `Daily | `Weekly | `Monthly | `Yearly ]

type weekday = [ `Su | `Mo | `Tu | `We | `Th | `Fr | `Sa ]

let weekday_of_string s =
  match String.uppercase_ascii s with
  | "SU" -> Some `Su
  | "MO" -> Some `Mo
  | "TU" -> Some `Tu
  | "WE" -> Some `We
  | "TH" -> Some `Th
  | "FR" -> Some `Fr
  | "SA" -> Some `Sa
  | _ -> None

let weekday_to_string = function
  | `Su -> "SU"
  | `Mo -> "MO"
  | `Tu -> "TU"
  | `We -> "WE"
  | `Th -> "TH"
  | `Fr -> "FR"
  | `Sa -> "SA"

let equal_weekday a b =
  match (a, b) with
  | `Su, `Su | `Mo, `Mo | `Tu, `Tu | `We, `We | `Th, `Th | `Fr, `Fr | `Sa, `Sa
    ->
      true
  | _ -> false

let freq_of_string s =
  match String.uppercase_ascii s with
  | "SECONDLY" -> Some `Secondly
  | "MINUTELY" -> Some `Minutely
  | "HOURLY" -> Some `Hourly
  | "DAILY" -> Some `Daily
  | "WEEKLY" -> Some `Weekly
  | "MONTHLY" -> Some `Monthly
  | "YEARLY" -> Some `Yearly
  | _ -> None

let freq_to_string = function
  | `Secondly -> "SECONDLY"
  | `Minutely -> "MINUTELY"
  | `Hourly -> "HOURLY"
  | `Daily -> "DAILY"
  | `Weekly -> "WEEKLY"
  | `Monthly -> "MONTHLY"
  | `Yearly -> "YEARLY"

let equal_freq a b =
  match (a, b) with
  | `Secondly, `Secondly
  | `Minutely, `Minutely
  | `Hourly, `Hourly
  | `Daily, `Daily
  | `Weekly, `Weekly
  | `Monthly, `Monthly
  | `Yearly, `Yearly ->
      true
  | _ -> false

type t = {
  freq : freq;
  until : Ical_date.t option;
  count : int option;
  interval : int option;
  by_second : int list;
  by_minute : int list;
  by_hour : int list;
  by_day : (int option * weekday) list;
  by_month_day : int list;
  by_year_day : int list;
  by_week_no : int list;
  by_month : int list;
  by_set_pos : int list;
  wkst : weekday option;
  other : (string * string) list;
}

let v ?until ?count ?interval freq =
  {
    freq;
    until;
    count;
    interval;
    by_second = [];
    by_minute = [];
    by_hour = [];
    by_day = [];
    by_month_day = [];
    by_year_day = [];
    by_week_no = [];
    by_month = [];
    by_set_pos = [];
    wkst = None;
    other = [];
  }

type acc = {
  a_freq : freq option;
  a_until : Ical_date.t option;
  a_count : int option;
  a_interval : int option;
  a_by_second : int list;
  a_by_minute : int list;
  a_by_hour : int list;
  a_by_day : (int option * weekday) list;
  a_by_month_day : int list;
  a_by_year_day : int list;
  a_by_week_no : int list;
  a_by_month : int list;
  a_by_set_pos : int list;
  a_wkst : weekday option;
  a_other : (string * string) list;
}

let empty_acc =
  {
    a_freq = None;
    a_until = None;
    a_count = None;
    a_interval = None;
    a_by_second = [];
    a_by_minute = [];
    a_by_hour = [];
    a_by_day = [];
    a_by_month_day = [];
    a_by_year_day = [];
    a_by_week_no = [];
    a_by_month = [];
    a_by_set_pos = [];
    a_wkst = None;
    a_other = [];
  }

let parse_int_list ?(one = parse_signed_int) name s =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | p :: rest -> (
        match one p with
        | Some n -> go (n :: acc) rest
        | None -> error "%s: %S is not an integer" name p)
  in
  go [] (String.split_on_char ',' s)

let parse_by_day_one s =
  let n = String.length s in
  if n < 2 then None
  else
    match weekday_of_string (String.sub s (n - 2) 2) with
    | None -> None
    | Some w ->
        if n = 2 then Some (None, w)
        else
          Option.map
            (fun o -> (Some o, w))
            (parse_signed_int (String.sub s 0 (n - 2)))

let parse_by_day_list s =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | p :: rest -> (
        match parse_by_day_one p with
        | Some v -> go (v :: acc) rest
        | None -> error "BYDAY: %S is not a weekday, with an optional ordinal" p
        )
  in
  go [] (String.split_on_char ',' s)

let of_acc acc =
  match acc.a_freq with
  | None -> error "a RECUR value requires FREQ"
  | Some freq ->
      Ok
        {
          freq;
          until = acc.a_until;
          count = acc.a_count;
          interval = acc.a_interval;
          by_second = acc.a_by_second;
          by_minute = acc.a_by_minute;
          by_hour = acc.a_by_hour;
          by_day = acc.a_by_day;
          by_month_day = acc.a_by_month_day;
          by_year_day = acc.a_by_year_day;
          by_week_no = acc.a_by_week_no;
          by_month = acc.a_by_month;
          by_set_pos = acc.a_by_set_pos;
          wkst = acc.a_wkst;
          other = List.rev acc.a_other;
        }

(* RFC 5545 Section 3.3.10: recur = recur-rule-part *( ";" recur-rule-part ),
   recur-rule-part = "NAME" "=" value, with FREQ required and UNTIL/COUNT
   mutually exclusive. *)
let of_string s =
  let rec go acc = function
    | [] -> of_acc acc
    | part :: rest -> (
        match String.index_opt part '=' with
        | None -> error "%S is not a valid RECUR part" part
        | Some i -> (
            let name = String.uppercase_ascii (String.sub part 0 i) in
            let value = String.sub part (i + 1) (String.length part - i - 1) in
            let dup () =
              error "%s appears more than once in a RECUR value" name
            in
            match name with
            | "FREQ" -> (
                if Option.is_some acc.a_freq then dup ()
                else
                  match freq_of_string value with
                  | Some f -> go { acc with a_freq = Some f } rest
                  | None -> error "%S is not a FREQ" value)
            | "UNTIL" ->
                if Option.is_some acc.a_until then dup ()
                else
                  let* d = Ical_date.of_string value in
                  go { acc with a_until = Some d } rest
            | "COUNT" -> (
                if Option.is_some acc.a_count then dup ()
                else
                  match parse_unsigned_int value with
                  | Some n -> go { acc with a_count = Some n } rest
                  | None -> error "%S is not a COUNT" value)
            | "INTERVAL" -> (
                if Option.is_some acc.a_interval then dup ()
                else
                  match parse_unsigned_int value with
                  | Some n -> go { acc with a_interval = Some n } rest
                  | None -> error "%S is not an INTERVAL" value)
            | "BYSECOND" ->
                if not (List.is_empty acc.a_by_second) then dup ()
                else
                  let* l =
                    parse_int_list ~one:parse_unsigned_int "BYSECOND" value
                  in
                  go { acc with a_by_second = l } rest
            | "BYMINUTE" ->
                if not (List.is_empty acc.a_by_minute) then dup ()
                else
                  let* l =
                    parse_int_list ~one:parse_unsigned_int "BYMINUTE" value
                  in
                  go { acc with a_by_minute = l } rest
            | "BYHOUR" ->
                if not (List.is_empty acc.a_by_hour) then dup ()
                else
                  let* l =
                    parse_int_list ~one:parse_unsigned_int "BYHOUR" value
                  in
                  go { acc with a_by_hour = l } rest
            | "BYDAY" ->
                if not (List.is_empty acc.a_by_day) then dup ()
                else
                  let* l = parse_by_day_list value in
                  go { acc with a_by_day = l } rest
            | "BYMONTHDAY" ->
                if not (List.is_empty acc.a_by_month_day) then dup ()
                else
                  let* l = parse_int_list "BYMONTHDAY" value in
                  go { acc with a_by_month_day = l } rest
            | "BYYEARDAY" ->
                if not (List.is_empty acc.a_by_year_day) then dup ()
                else
                  let* l = parse_int_list "BYYEARDAY" value in
                  go { acc with a_by_year_day = l } rest
            | "BYWEEKNO" ->
                if not (List.is_empty acc.a_by_week_no) then dup ()
                else
                  let* l = parse_int_list "BYWEEKNO" value in
                  go { acc with a_by_week_no = l } rest
            | "BYMONTH" ->
                if not (List.is_empty acc.a_by_month) then dup ()
                else
                  let* l =
                    parse_int_list ~one:parse_unsigned_int "BYMONTH" value
                  in
                  go { acc with a_by_month = l } rest
            | "BYSETPOS" ->
                if not (List.is_empty acc.a_by_set_pos) then dup ()
                else
                  let* l = parse_int_list "BYSETPOS" value in
                  go { acc with a_by_set_pos = l } rest
            | "WKST" -> (
                if Option.is_some acc.a_wkst then dup ()
                else
                  match weekday_of_string value with
                  | Some w -> go { acc with a_wkst = Some w } rest
                  | None -> error "%S is not a WKST" value)
            | _ ->
                if List.mem_assoc name acc.a_other then dup ()
                else go { acc with a_other = (name, value) :: acc.a_other } rest
            ))
  in
  go empty_acc (String.split_on_char ';' s)

let int_list_to_string l = String.concat "," (List.map string_of_int l)

let by_day_to_string l =
  String.concat ","
    (List.map
       (fun (ord, wd) ->
         (match ord with None -> "" | Some o -> string_of_int o)
         ^ weekday_to_string wd)
       l)

let to_string t =
  let parts = ref [] in
  let add s = parts := s :: !parts in
  add ("FREQ=" ^ freq_to_string t.freq);
  Option.iter (fun d -> add ("UNTIL=" ^ Ical_date.to_string d)) t.until;
  Option.iter (fun n -> add ("COUNT=" ^ string_of_int n)) t.count;
  Option.iter (fun n -> add ("INTERVAL=" ^ string_of_int n)) t.interval;
  if not (List.is_empty t.by_second) then
    add ("BYSECOND=" ^ int_list_to_string t.by_second);
  if not (List.is_empty t.by_minute) then
    add ("BYMINUTE=" ^ int_list_to_string t.by_minute);
  if not (List.is_empty t.by_hour) then
    add ("BYHOUR=" ^ int_list_to_string t.by_hour);
  if not (List.is_empty t.by_day) then add ("BYDAY=" ^ by_day_to_string t.by_day);
  if not (List.is_empty t.by_month_day) then
    add ("BYMONTHDAY=" ^ int_list_to_string t.by_month_day);
  if not (List.is_empty t.by_year_day) then
    add ("BYYEARDAY=" ^ int_list_to_string t.by_year_day);
  if not (List.is_empty t.by_week_no) then
    add ("BYWEEKNO=" ^ int_list_to_string t.by_week_no);
  if not (List.is_empty t.by_month) then
    add ("BYMONTH=" ^ int_list_to_string t.by_month);
  if not (List.is_empty t.by_set_pos) then
    add ("BYSETPOS=" ^ int_list_to_string t.by_set_pos);
  Option.iter (fun w -> add ("WKST=" ^ weekday_to_string w)) t.wkst;
  List.iter (fun (n, v) -> add (n ^ "=" ^ v)) t.other;
  String.concat ";" (List.rev !parts)

let check_unsigned name lst lo hi =
  if List.for_all (fun v -> v >= lo && v <= hi) lst then Ok ()
  else error "%s: a value is outside %d to %d" name lo hi

let check_signed name lst lo hi =
  if List.for_all (fun v -> abs v >= lo && abs v <= hi) lst then Ok ()
  else error "%s: a value is outside %d to %d, or its negation" name lo hi

let validate t =
  let* () =
    if Option.is_some t.until && Option.is_some t.count then
      error "a RECUR may not have both UNTIL and COUNT"
    else Ok ()
  in
  let* () = check_unsigned "BYSECOND" t.by_second 0 60 in
  let* () = check_unsigned "BYMINUTE" t.by_minute 0 59 in
  let* () = check_unsigned "BYHOUR" t.by_hour 0 23 in
  let* () =
    if
      List.for_all
        (fun (ord, _) ->
          match ord with None -> true | Some o -> abs o >= 1 && abs o <= 53)
        t.by_day
    then Ok ()
    else error "BYDAY: an ordinal is outside 1 to 53, or its negation"
  in
  let* () = check_signed "BYMONTHDAY" t.by_month_day 1 31 in
  let* () = check_signed "BYYEARDAY" t.by_year_day 1 366 in
  let* () = check_signed "BYWEEKNO" t.by_week_no 1 53 in
  let* () = check_unsigned "BYMONTH" t.by_month 1 12 in
  let* () = check_signed "BYSETPOS" t.by_set_pos 1 366 in
  let* () =
    match t.interval with
    | Some n when n <= 0 -> error "INTERVAL must be a positive integer"
    | _ -> Ok ()
  in
  let* () =
    match t.count with
    | Some n when n <= 0 -> error "COUNT must be a positive integer"
    | _ -> Ok ()
  in
  let has_ordinal = List.exists (fun (ord, _) -> Option.is_some ord) t.by_day in
  let* () =
    if has_ordinal then
      match t.freq with
      | `Monthly | `Yearly -> Ok ()
      | _ ->
          error "BYDAY: an ordinal appears only with a MONTHLY or YEARLY FREQ"
    else Ok ()
  in
  let* () =
    if has_ordinal && not (List.is_empty t.by_week_no) then
      error "BYDAY: an ordinal may not appear with BYWEEKNO"
    else Ok ()
  in
  let* () =
    if List.is_empty t.by_week_no then Ok ()
    else
      match t.freq with
      | `Yearly -> Ok ()
      | _ -> error "BYWEEKNO appears only with a YEARLY FREQ"
  in
  let* () =
    if List.is_empty t.by_year_day then Ok ()
    else
      match t.freq with
      | `Daily | `Weekly | `Monthly ->
          error "BYYEARDAY may not appear with a DAILY, WEEKLY or MONTHLY FREQ"
      | _ -> Ok ()
  in
  let* () =
    if List.is_empty t.by_month_day then Ok ()
    else
      match t.freq with
      | `Weekly -> error "BYMONTHDAY may not appear with a WEEKLY FREQ"
      | _ -> Ok ()
  in
  let* () =
    if List.is_empty t.by_set_pos then Ok ()
    else if
      List.is_empty t.by_second && List.is_empty t.by_minute
      && List.is_empty t.by_hour && List.is_empty t.by_day
      && List.is_empty t.by_month_day
      && List.is_empty t.by_year_day
      && List.is_empty t.by_week_no && List.is_empty t.by_month
    then error "BYSETPOS may only appear with another BY part"
    else Ok ()
  in
  Ok t

let equal a b =
  equal_freq a.freq b.freq
  && Option.equal Ical_date.equal a.until b.until
  && Option.equal Int.equal a.count b.count
  && Option.equal Int.equal a.interval b.interval
  && List.equal Int.equal a.by_second b.by_second
  && List.equal Int.equal a.by_minute b.by_minute
  && List.equal Int.equal a.by_hour b.by_hour
  && List.equal
       (fun (o1, w1) (o2, w2) ->
         Option.equal Int.equal o1 o2 && equal_weekday w1 w2)
       a.by_day b.by_day
  && List.equal Int.equal a.by_month_day b.by_month_day
  && List.equal Int.equal a.by_year_day b.by_year_day
  && List.equal Int.equal a.by_week_no b.by_week_no
  && List.equal Int.equal a.by_month b.by_month
  && List.equal Int.equal a.by_set_pos b.by_set_pos
  && Option.equal equal_weekday a.wkst b.wkst
  && List.equal
       (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && String.equal v1 v2)
       a.other b.other

let pp ppf t = Format.pp_print_string ppf (to_string t)
