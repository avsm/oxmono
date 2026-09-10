module D = Httpz_dav
module P = Ical.Property
module C = Ical.Component

type window = { start : Ptime.t; finish : Ptime.t }

let window ~start ~finish =
  let whole_seconds t = Ptime.equal t (Ptime.truncate ~frac_s:0 t) in
  if not (whole_seconds start && whole_seconds finish) then
    invalid_arg "Agenda bounds must use whole seconds.";
  let seconds = Ptime.Span.to_float_s (Ptime.diff finish start) in
  if seconds <= 0. || seconds > 31. *. 86400. then
    invalid_arg "Agenda ranges must be positive and at most 31 days.";
  { start; finish }

let of_strings ~start ~finish =
  let parse s =
    match Ptime.of_rfc3339 s with
    | Ok (t, Some _, n) when n = String.length s -> t
    | _ ->
        invalid_arg
          "Use RFC3339 start and end with UTC or an explicit offset, for \
           example 2026-09-10T00:00:00+01:00."
  in
  window ~start:(parse start) ~finish:(parse finish)

let bounds w =
  ( Ptime.to_rfc3339 ~tz_offset_s:0 w.start,
    Ptime.to_rfc3339 ~tz_offset_s:0 w.finish )

let query w =
  let range =
    Caldav.Filter.time_range
      ~start:(Ical.Date.of_ptime w.start)
      ~finish:(Ical.Date.of_ptime w.finish)
      ()
  in
  Caldav.Report.query
    ~data:(Caldav.Calendar_data.v ~expand:range ())
    (Caldav.Filter.components ~time_range:range "VEVENT")

let property (c : Caldav_data.collection) name =
  match D.parse_xml c.properties with
  | Error _ -> None
  | Ok root ->
      List.find_map
        (function D.Element e when e.name = name -> Some e | _ -> None)
        root.children

let supports_events c =
  match property c Caldav.Property.supported_calendar_component_set with
  | None -> true
  | Some p -> List.mem "VEVENT" (Caldav.Property.component_set p)

let timezone c =
  match
    Option.bind
      (property c Caldav.Property.calendar_timezone)
      Caldav.Property.timezone
  with
  | None -> None
  | Some text -> (
      match Ical.one_of_string text with
      | Ok cal ->
          List.find_map (fun tz -> C.text tz "TZID") (Ical.timezones cal)
      | Error _ -> None)

type occurrence = { starts : string; summary : Jsont.json }

type resource = {
  href : string;
  etag : string option;
  raw : string;
  occurrences : occurrence list;
}

let obj fields =
  Jsont.Json.object' (List.map (fun (k, v) -> ((k, Jsont.Meta.none), v)) fields)

let str = Jsont.Json.string
let opt = Option.fold ~none:(Jsont.Json.null ()) ~some:str

let checked = function
  | Ok x -> x
  | Error _ -> invalid_arg "Invalid expanded calendar data."

let date d =
  match d with
  | Ical.Date.Date d -> Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day
  | Date_time dt ->
      let p =
        match Ical.Date.to_ptime dt with
        | Some p -> p
        | None -> invalid_arg "Invalid occurrence date."
      in
      if dt.time.utc then Ptime.to_rfc3339 ~tz_offset_s:0 p
      else String.sub (Ptime.to_rfc3339 p) 0 19

let parse ~href ~etag raw =
  let cal = checked (Ical.one_of_string raw) in
  let occurrences =
    Ical.events cal
    |> List.filter_map (fun e ->
        if
          List.exists
            (fun name -> C.find e name <> None)
            [ "RRULE"; "RDATE"; "EXDATE"; "EXRULE" ]
        then
          invalid_arg
            "Server returned an unexpanded recurrence. An agenda cannot be \
             confirmed.";
        if C.text e "STATUS" = Some "CANCELLED" then None
        else
          let starts =
            match checked (C.dtstart e) with
            | Some d -> d
            | None -> invalid_arg "Expanded event has no start."
          in
          let ends =
            match checked (C.dtend e) with
            | Some d -> d
            | None -> invalid_arg "Expanded event has no end."
          in
          let all_day =
            match starts with Ical.Date.Date _ -> true | _ -> false
          in
          let floating =
            match starts with
            | Ical.Date.Date_time dt -> not dt.time.utc
            | _ -> false
          in
          let tzid = Option.bind (C.find e "DTSTART") P.tzid in
          let clipped key max =
            Option.map (Plugin.clip ~bytes:max) (C.text e key)
          in
          let summary =
            obj
              [
                ("start", str (date starts));
                ("end", str (date ends));
                ("all_day", Jsont.Json.bool all_day);
                ("floating", Jsont.Json.bool floating);
                ("timezone", opt (Option.map (Plugin.clip ~bytes:64) tzid));
                ("uid", opt (clipped "UID" 256));
                ( "recurrence_id",
                  opt
                    (Option.map
                       (fun p -> Plugin.clip ~bytes:64 (P.value p))
                       (C.find e "RECURRENCE-ID")) );
                ("title", opt (clipped "SUMMARY" 360));
                ("location", opt (clipped "LOCATION" 240));
                ("status", opt (clipped "STATUS" 40));
                ("transparency", opt (clipped "TRANSP" 40));
              ]
          in
          Some { starts = date starts; summary })
  in
  { href; etag; raw; occurrences }
