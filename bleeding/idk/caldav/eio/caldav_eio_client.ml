(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module S = Fetch_dav.Session
module O = Fetch_dav.Objects

module Href = struct
  let resolve ~base href =
    match Httpz_dav.resolve_href ~base href with Ok r -> r | Error _ -> href

  let path = Httpz_dav.href_path
end

module Quirks = Caldav_eio_quirks

type t = { session : S.t; quirks : Quirks.t }

type error = S.error =
  | Http of int * string
  | Dav of int * Httpz_dav.element list
  | Precondition_failed of string
  | Not_found of string
  | Xml of string
  | Data of string
  | Discovery of string
  | Transport of Fetch.error * string

let pp_error ppf e = S.pp_error ~describe:Caldav.Error.describe ppf e
let error_to_string e = Format.asprintf "%a" pp_error e
let ( let* ) = Result.bind

let connect ~sw ?credentials ?allow_insecure ?limits ?quirks fetch url =
  let quirks = match quirks with Some q -> q | None -> Quirks.of_url url in
  let* session =
    S.connect ~sw ?credentials ?allow_insecure ?limits
      ~lenient_hrefs:quirks.lenient_hrefs ~service:`Caldav
      ~home_set:Caldav.Property.calendar_home_set fetch url
  in
  Ok { session; quirks }

let principal t = S.principal t.session
let home_sets t = S.home_sets t.session
let quirks t = t.quirks
let session t = t.session
let dav t = S.client t.session
let download t url = S.download t.session url
let propfind t ?depth url query = S.propfind t.session ?depth url query
let report t ?depth url body = S.report t.session ?depth url body

let calendar t url =
  let* m = propfind t ~depth:`Zero url Caldav.Calendar.propfind in
  match Caldav.Calendar.of_multistatus m with
  | c :: _ -> Ok c
  | [] -> Error (Discovery (url ^ " is not a calendar"))

let calendars t =
  let rec go acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | home :: rest ->
        let* m = propfind t ~depth:`One home Caldav.Calendar.propfind in
        let cals =
          List.map
            (fun (c : Caldav.Calendar.t) ->
              { c with href = Href.resolve ~base:home c.href })
            (Caldav.Calendar.of_multistatus m)
        in
        go (cals :: acc) rest
  in
  go [] (home_sets t)

let create_calendar t ?display_name ?description ?timezone ?components url =
  S.mkcalendar t.session
    ~props:
      (Caldav.Calendar.mkcalendar ?display_name ?description ?timezone
         ?components ())
    url

let delete_calendar t url = S.delete t.session url
let set_props t url updates = S.proppatch t.session url updates

type member = S.member = {
  href : string;
  etag : string option;
  content_type : string option;
}

type 'a entry = 'a O.entry = { href : string; etag : string option; value : 'a }
type 'a page = 'a O.page = { entries : 'a entry list; truncated : bool }

let list t url = S.members t.session url

let objects (codec : _ Caldav.Data.t) =
  {
    O.content_type = Printf.sprintf "%s; charset=utf-8" codec.content_type;
    decode = codec.decode;
    encode = codec.encode;
  }

let calendar_data (r : Httpz_dav.response) =
  Option.bind
    (Httpz_dav.find_property Caldav.Calendar_data.name r)
    Caldav.Calendar_data.data

let get codec t url = O.get t.session (objects codec) url

(* A server with a fixed component set answers 403 to anything else, so the
   component is checked against the calendar first. *)
let acceptable t url v (codec : _ Caldav.Data.t) =
  if not t.quirks.fixed_components then Ok ()
  else
    match Result.bind (codec.encode v) Ical.one_of_string with
    | Error _ -> Ok ()
    | Ok cal -> (
        let kinds =
          List.sort_uniq compare
            (List.filter_map
               (fun (c : Ical.Component.t) ->
                 if c.name = "VTIMEZONE" then None else Some c.name)
               (Ical.components cal))
        in
        let* cal = calendar t url in
        match
          List.filter
            (fun k -> not (Caldav.Calendar.accepts ~component:k cal))
            kinds
        with
        | [] -> Ok ()
        | k :: _ ->
            Error
              (Data (Printf.sprintf "the calendar %s does not store %s" url k)))

let put codec t ?etag ?create url v =
  let* () = acceptable t (Httpz_dav.href_parent url) v codec in
  O.put t.session (objects codec) ?etag ?create url v

let add codec t ?name calendar v =
  let* () = acceptable t calendar v codec in
  O.add t.session (objects codec) ?name ~uid:(Caldav.Data.uid codec) ~ext:".ics"
    calendar v

let delete t ?etag url = S.delete t.session ?etag url

let page_of_multistatus codec ~base m =
  O.page_of_multistatus (objects codec) ~data:calendar_data ~base m

let rec has_param_filter (c : Caldav.Filter.comp_filter) =
  match c.comp_condition with
  | `Defined | `Not_defined -> false
  | `Matches (_, comps, props) ->
      List.exists has_param_filter comps
      || List.exists
           (fun (p : Caldav.Filter.prop_filter) ->
             match p.prop_condition with
             | `Matches (_, _ :: _) -> true
             | _ -> false)
           props

let rec without_param_filters (c : Caldav.Filter.comp_filter) =
  match c.comp_condition with
  | `Defined | `Not_defined -> c
  | `Matches (tr, comps, props) ->
      {
        c with
        comp_condition =
          `Matches
            ( tr,
              List.map without_param_filters comps,
              List.map
                (fun (p : Caldav.Filter.prop_filter) ->
                  match p.prop_condition with
                  | `Matches (m, _) ->
                      { p with prop_condition = `Matches (m, []) }
                  | _ -> p)
                props );
      }

(* A server that cannot evaluate a param-filter is asked the rest of the
   filter, and the parameter conditions are applied here to what it returns,
   for which the whole object is needed. *)
let query (codec : _ Caldav.Data.t) t ?data ?timezone url filter =
  let data =
    match data with Some d -> d | None -> Caldav.Data.calendar_data codec
  in
  let locally = (not t.quirks.param_filter) && has_param_filter filter in
  let sent = if locally then without_param_filters filter else filter in
  let q = Caldav.Report.query ~data ?timezone sent in
  let* m = report t ~depth:`One url (Caldav.Report.query_to_xml q) in
  let* p = page_of_multistatus codec ~base:url m in
  if not locally then Ok p
  else
    let keep (e : _ entry) =
      match Result.bind (codec.encode e.value) Ical.one_of_string with
      | Ok cal -> Caldav.Filter.matches filter cal
      | Error _ -> true
    in
    Ok { p with entries = List.filter keep p.entries }

(* RFC 4791 Section 9.6.5: an expanded instance carries a RECURRENCE-ID. A
   server that leaves it off the first instance gets one from its DTSTART. *)
let with_recurrence_ids (codec : _ Caldav.Data.t) v =
  match Result.bind (codec.encode v) Ical.one_of_string with
  | Error _ -> v
  | Ok cal -> (
      let fix (c : Ical.Component.t) =
        if c.name <> "VEVENT" || Ical.Component.find c "RECURRENCE-ID" <> None
        then c
        else
          match Ical.Component.find c "DTSTART" with
          | Some start ->
              Ical.Component.add c
                (Ical.Property.v
                   ~params:(Ical.Property.params start)
                   "RECURRENCE-ID"
                   (Ical.Property.value start))
          | None -> c
      in
      let comps = Ical.components cal in
      let events =
        List.filter (fun (c : Ical.Component.t) -> c.name = "VEVENT") comps
      in
      if List.length events < 2 then v
      else
        let fixed = Ical.with_components cal (List.map fix comps) in
        match codec.decode (Ical.to_string fixed) with
        | Ok v -> v
        | Error _ -> v)

let events codec t ?start ?finish ?(expand = false) url =
  let range =
    if start = None && finish = None then None
    else Some (Caldav.Filter.time_range ?start ?finish ())
  in
  let filter = Caldav.Filter.components ?time_range:range "VEVENT" in
  let data =
    let d = Caldav.Data.calendar_data codec in
    if expand then { d with Caldav.Calendar_data.expand = range } else d
  in
  let* p = query codec t ~data url filter in
  (* Section 9.6.5 makes start and end required on CALDAV:expand, so without a
     range nothing was expanded and an instance must not be stamped. *)
  if expand && range <> None && not t.quirks.recurrence_id_on_first then
    Ok
      (List.map
         (fun e -> { e with value = with_recurrence_ids codec e.value })
         p.entries)
  else Ok p.entries

let multiget codec t url hrefs =
  if hrefs = [] then Ok []
  else
    let hrefs =
      List.map (fun h -> Href.path (Href.resolve ~base:url h)) hrefs
    in
    let mg =
      Caldav.Report.multiget ~data:(Caldav.Data.calendar_data codec) hrefs
    in
    let* m = report t ~depth:`Zero url (Caldav.Report.multiget_to_xml mg) in
    let* p = page_of_multistatus codec ~base:url m in
    Ok p.entries

(* RFC 4791 Section 7.10: a free-busy-query answers 200 with a calendar
   object rather than a multistatus. *)
let free_busy t ?start ?finish url =
  (* Section 7.10 requires a time range, and time_range raises without one. *)
  if start = None && finish = None then
    Error (Data "a free-busy query needs a start or an end")
  else
    let range = Caldav.Filter.time_range ?start ?finish () in
    let* text =
      S.report_body t.session ~depth:`Zero url
        (Caldav.Report.free_busy_to_xml range)
    in
    Result.map_error (fun m -> Data m) (Ical.one_of_string text)

type 'a change = 'a O.change = Changed of 'a entry | Removed of string

type 'a sync = 'a O.sync = {
  token : string option;
  changes : 'a change list;
  truncated : bool;
}

let sync codec t ?token ?limit url =
  O.sync t.session ~multiget:(multiget codec t) ?token ?limit url

let sync_token t url = S.sync_token t.session url
