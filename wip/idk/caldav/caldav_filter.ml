(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Component = Ical.Component
module Property = Ical.Property
module Date = Ical.Date

type time_range = {
  start : Date.date_time option;
  finish : Date.date_time option;
}

type text_match = { text : string; collation : string option; negate : bool }

type param_filter = {
  param : string;
  param_test : [ `Defined | `Not_defined | `Match of text_match ];
}

type prop_filter = {
  prop : string;
  prop_condition :
    [ `Defined
    | `Not_defined
    | `Matches of
      [ `Time_range of time_range | `Text of text_match ] option
      * param_filter list ];
}

type comp_filter = {
  comp : string;
  comp_condition :
    [ `Defined
    | `Not_defined
    | `Matches of time_range option * comp_filter list * prop_filter list ];
}

type t = comp_filter

(* {1 Constructors} *)

let time_range ?start ?finish () =
  if start = None && finish = None then
    invalid_arg "Caldav_filter.time_range: neither bound is given";
  { start; finish }

let text_match ?collation ?(negate = false) text = { text; collation; negate }

let prop ?(params = []) name matched =
  let prop_condition =
    match (matched, params) with
    | None, [] -> `Defined
    | _ -> `Matches (matched, params)
  in
  { prop = name; prop_condition }

let prop_not_defined name = { prop = name; prop_condition = `Not_defined }

let param name m =
  {
    param = name;
    param_test = (match m with Some tm -> `Match tm | None -> `Defined);
  }

let param_not_defined name = { param = name; param_test = `Not_defined }

let comp ?time_range ?(comps = []) ?(props = []) name =
  let comp_condition =
    match (time_range, comps, props) with
    | None, [], [] -> `Defined
    | tr, comps, props -> `Matches (tr, comps, props)
  in
  { comp = name; comp_condition }

let comp_not_defined name = { comp = name; comp_condition = `Not_defined }
let v comps = comp ~comps "VCALENDAR"
let components ?time_range ?props name = v [ comp ?time_range ?props name ]
let all = v []

(* {1 XML} *)

let equal_time_range (a : time_range) (b : time_range) =
  Option.equal Date.equal_date_time a.start b.start
  && Option.equal Date.equal_date_time a.finish b.finish

let equal_text_match (a : text_match) (b : text_match) =
  String.equal a.text b.text
  && Option.equal String.equal a.collation b.collation
  && Bool.equal a.negate b.negate

let equal_param_filter (a : param_filter) (b : param_filter) =
  String.equal a.param b.param
  &&
  match (a.param_test, b.param_test) with
  | `Defined, `Defined | `Not_defined, `Not_defined -> true
  | `Match x, `Match y -> equal_text_match x y
  | (`Defined | `Not_defined | `Match _), _ -> false

let equal_matched a b =
  match (a, b) with
  | `Time_range x, `Time_range y -> equal_time_range x y
  | `Text x, `Text y -> equal_text_match x y
  | (`Time_range _ | `Text _), _ -> false

let equal_prop_filter (a : prop_filter) (b : prop_filter) =
  String.equal a.prop b.prop
  &&
  match (a.prop_condition, b.prop_condition) with
  | `Defined, `Defined | `Not_defined, `Not_defined -> true
  | `Matches (m0, p0), `Matches (m1, p1) ->
      Option.equal equal_matched m0 m1 && List.equal equal_param_filter p0 p1
  | (`Defined | `Not_defined | `Matches _), _ -> false

let rec equal_comp_filter (a : comp_filter) (b : comp_filter) =
  String.equal a.comp b.comp
  &&
  match (a.comp_condition, b.comp_condition) with
  | `Defined, `Defined | `Not_defined, `Not_defined -> true
  | `Matches (r0, c0, p0), `Matches (r1, c1, p1) ->
      Option.equal equal_time_range r0 r1
      && List.equal equal_comp_filter c0 c1
      && List.equal equal_prop_filter p0 p1
  | (`Defined | `Not_defined | `Matches _), _ -> false

let equal = equal_comp_filter
let cname local = Httpz_dav.caldav local
let attr_name = ("", "name")
let attr_start = ("", "start")
let attr_end = ("", "end")
let attr_collation = ("", "collation")
let attr_negate = ("", "negate-condition")
let time_range_name = cname "time-range"
let ( let* ) = Result.bind

let rec all_ok = function
  | [] -> Ok []
  | x :: xs ->
      let* x = x in
      let* xs = all_ok xs in
      Ok (x :: xs)

let time_range_to_xml tr =
  let attrs =
    (match tr.start with
      | None -> []
      | Some s -> [ (attr_start, Date.date_time_to_string s) ])
    @
    match tr.finish with
    | None -> []
    | Some f -> [ (attr_end, Date.date_time_to_string f) ]
  in
  Httpz_dav.el ~attrs time_range_name []

let attr_date_time n x =
  match Httpz_dav.attr n x with
  | None -> Ok None
  | Some s -> (
      match Date.date_time_of_string s with
      (* RFC 4791 Sections 9.6.5 and 9.9 require a date with UTC time. *)
      | Ok dt when not dt.Date.time.Date.utc ->
          Error (Printf.sprintf "%S is not a date-time in UTC" s)
      | Ok dt -> Ok (Some dt)
      | Error e -> Error e)

let time_range_of_xml x =
  if not (Httpz_dav.is time_range_name x) then
    Error "the document is not a CALDAV:time-range"
  else
    let* start = attr_date_time attr_start x in
    let* finish = attr_date_time attr_end x in
    if start = None && finish = None then
      Error "a time-range has neither a start nor an end"
    else Ok { start; finish }

let bool_of_yes_no = function
  | None -> Ok false
  | Some "yes" -> Ok true
  | Some "no" -> Ok false
  | Some s -> Error (Printf.sprintf "%S is not yes or no" s)

let text_match_to_xml m =
  let attrs =
    (match m.collation with
      | None | Some "i;ascii-casemap" -> []
      | Some c -> [ (attr_collation, c) ])
    @ if m.negate then [ (attr_negate, "yes") ] else []
  in
  Httpz_dav.element ~attrs (cname "text-match") [ Httpz_dav.Text m.text ]

let text_match_of_xml x =
  let* negate = bool_of_yes_no (Httpz_dav.attr attr_negate x) in
  Ok
    {
      text = Httpz_dav.content x;
      collation = Httpz_dav.attr attr_collation x;
      negate;
    }

let param_filter_to_xml pf =
  let child =
    match pf.param_test with
    | `Defined -> []
    | `Not_defined -> [ Httpz_dav.empty (cname "is-not-defined") ]
    | `Match tm -> [ text_match_to_xml tm ]
  in
  Httpz_dav.el ~attrs:[ (attr_name, pf.param) ] (cname "param-filter") child

let param_filter_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a param-filter has no name"
  | Some param -> (
      match Httpz_dav.find (cname "is-not-defined") x with
      | Some _ -> Ok { param; param_test = `Not_defined }
      | None -> (
          match Httpz_dav.find (cname "text-match") x with
          | None -> Ok { param; param_test = `Defined }
          | Some tm ->
              let* tm = text_match_of_xml tm in
              Ok { param; param_test = `Match tm }))

let prop_filter_to_xml pf =
  let children =
    match pf.prop_condition with
    | `Defined -> []
    | `Not_defined -> [ Httpz_dav.empty (cname "is-not-defined") ]
    | `Matches (matched, params) ->
        (match matched with
          | None -> []
          | Some (`Time_range tr) -> [ time_range_to_xml tr ]
          | Some (`Text tm) -> [ text_match_to_xml tm ])
        @ List.map param_filter_to_xml params
  in
  Httpz_dav.el ~attrs:[ (attr_name, pf.prop) ] (cname "prop-filter") children

let prop_filter_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a prop-filter has no name"
  | Some prop -> (
      match Httpz_dav.find (cname "is-not-defined") x with
      | Some _ -> Ok { prop; prop_condition = `Not_defined }
      | None ->
          let* matched =
            match Httpz_dav.find time_range_name x with
            | Some tr ->
                let* tr = time_range_of_xml tr in
                Ok (Some (`Time_range tr))
            | None -> (
                match Httpz_dav.find (cname "text-match") x with
                | None -> Ok None
                | Some tm ->
                    let* tm = text_match_of_xml tm in
                    Ok (Some (`Text tm)))
          in
          let* params =
            all_ok
              (List.map param_filter_of_xml
                 (Httpz_dav.children (cname "param-filter") x))
          in
          let prop_condition =
            match (matched, params) with
            | None, [] -> `Defined
            | _ -> `Matches (matched, params)
          in
          Ok { prop; prop_condition })

let rec comp_filter_to_xml cf =
  let children =
    match cf.comp_condition with
    | `Defined -> []
    | `Not_defined -> [ Httpz_dav.empty (cname "is-not-defined") ]
    | `Matches (tr, comps, props) ->
        Option.to_list (Option.map time_range_to_xml tr)
        @ List.map prop_filter_to_xml props
        @ List.map comp_filter_to_xml comps
  in
  Httpz_dav.el ~attrs:[ (attr_name, cf.comp) ] (cname "comp-filter") children

let rec comp_filter_of_xml x =
  match Httpz_dav.attr attr_name x with
  | None -> Error "a comp-filter has no name"
  | Some comp -> (
      match Httpz_dav.find (cname "is-not-defined") x with
      | Some _ -> Ok { comp; comp_condition = `Not_defined }
      | None ->
          let* tr =
            match Httpz_dav.find time_range_name x with
            | None -> Ok None
            | Some tr ->
                let* tr = time_range_of_xml tr in
                Ok (Some tr)
          in
          let* props =
            all_ok
              (List.map prop_filter_of_xml
                 (Httpz_dav.children (cname "prop-filter") x))
          in
          let* comps =
            all_ok
              (List.map comp_filter_of_xml
                 (Httpz_dav.children (cname "comp-filter") x))
          in
          let comp_condition =
            match (tr, comps, props) with
            | None, [], [] -> `Defined
            | _ -> `Matches (tr, comps, props)
          in
          Ok { comp; comp_condition })

let to_xml t = Httpz_dav.el (cname "filter") [ comp_filter_to_xml t ]

let of_xml x =
  if not (Httpz_dav.is (cname "filter") x) then
    Error "the document is not a CALDAV:filter"
  else
    match Httpz_dav.find (cname "comp-filter") x with
    | None -> Error "a filter has no comp-filter"
    | Some cf -> comp_filter_of_xml cf

(* {1 Matching} *)

let ptime_of_date d =
  match d with
  | Date.Date date -> Date.to_ptime (Date.start_of_day date)
  | Date.Date_time dt -> Date.to_ptime dt

let start_ptime c =
  match Component.dtstart c with
  | Error _ | Ok None -> None
  | Ok (Some d) -> ptime_of_date d

let end_ptime c =
  match Component.dtend c with
  | Error _ | Ok None -> None
  | Ok (Some d) -> ptime_of_date d

let bound f = function
  | None -> true
  | Some b -> ( match Date.to_ptime b with Some rt -> f rt | None -> true)

(* RFC 4791 Section 9.9: a component of non-zero length matches a
   CALDAV:time-range if its start is before the range's end and its end after
   the range's start, with an absent range bound unbounded. A component of
   zero length is a point in time, which the tables match with
   (start <= DTSTART), so the range's start is inclusive there and exclusive
   for an interval. *)
let overlaps ~start ~finish tr =
  let point = Ptime.equal start finish in
  bound (fun rt -> Ptime.compare start rt < 0) tr.finish
  && bound
       (fun rt ->
         if point then Ptime.compare finish rt >= 0
         else Ptime.compare finish rt > 0)
       tr.start

(* The VTODO row of Section 9.9 for a to-do with a DUE and no DTSTART, which
   matches on (start < DUE) AND (end >= DUE). *)
let due_time_range_matches d tr =
  bound (fun rt -> Ptime.compare d rt <= 0) tr.finish
  && bound (fun rt -> Ptime.compare d rt > 0) tr.start

(* Section 9.9 gives a table per component type. The rows on DTSTART, DTEND,
   DURATION and DUE are here. The rows on COMPLETED and CREATED, and the
   FREEBUSY periods of a VFREEBUSY, are not, so a to-do with none of the four
   and a free-busy with only FREEBUSY properties do not match. *)
let component_time_range_matches c tr =
  match start_ptime c with
  | Some s ->
      let e = Option.value (end_ptime c) ~default:s in
      overlaps ~start:s ~finish:e tr
  | None -> (
      match end_ptime c with
      | None -> false
      | Some d -> due_time_range_matches d tr)

let property_time_range_matches p tr =
  match Property.date_time p with
  | Error _ -> false
  | Ok d -> (
      match ptime_of_date d with
      | None -> false
      | Some t -> overlaps ~start:t ~finish:t tr)

let normalize collation s =
  match collation with
  | Some "i;octet" -> s
  | Some "i;ascii-casemap" | Some "i;unicode-casemap" | None ->
      String.lowercase_ascii s
  | Some _ -> s

let is_substring ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  nl = 0
  ||
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  go 0

let text_matches tm value =
  let a = normalize tm.collation value and b = normalize tm.collation tm.text in
  let m = is_substring ~needle:b a in
  if tm.negate then not m else m

let param_filter_matches p pf =
  match pf.param_test with
  | `Defined -> Property.find_values p pf.param <> None
  | `Not_defined -> Property.find_values p pf.param = None
  | `Match tm -> (
      match Property.find_values p pf.param with
      | None -> false
      | Some values ->
          (* RFC 4791 Section 9.7.5 negates the condition, so a parameter
             holding several values matches a negated text-match only when
             none of them matches. *)
          let any =
            List.exists (text_matches { tm with negate = false }) values
          in
          if tm.negate then not any else any)

let prop_filter_matches c pf =
  let props = Component.find_all c pf.prop in
  match pf.prop_condition with
  | `Defined -> props <> []
  | `Not_defined -> props = []
  | `Matches (matched, params) ->
      List.exists
        (fun p ->
          (match matched with
            | None -> true
            | Some (`Text tm) -> text_matches tm (Property.text p)
            | Some (`Time_range tr) -> property_time_range_matches p tr)
          && List.for_all (param_filter_matches p) params)
        props

let rec comp_filter_matches (siblings : Component.t list) cf =
  let named = String.uppercase_ascii cf.comp in
  let candidates = List.filter (fun c -> c.Component.name = named) siblings in
  match cf.comp_condition with
  | `Defined -> candidates <> []
  | `Not_defined -> candidates = []
  | `Matches (tr, comps, props) ->
      List.exists (fun c -> component_matches c tr comps props) candidates

and component_matches c tr comps props =
  (match tr with None -> true | Some tr -> component_time_range_matches c tr)
  && List.for_all (comp_filter_matches c.Component.components) comps
  && List.for_all (prop_filter_matches c) props

let matches t (cal : Ical.t) =
  String.uppercase_ascii t.comp = "VCALENDAR"
  &&
  match t.comp_condition with
  | `Defined -> true
  | `Not_defined -> false
  | `Matches (tr, comps, props) ->
      component_matches (cal :> Component.t) tr comps props
