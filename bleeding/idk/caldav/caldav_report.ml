(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Calendar_data = Caldav_calendar_data
module Filter = Caldav_filter

type props =
  | Prop of Httpz_dav.name list * Calendar_data.t option
  | Allprop
  | Propname

type query = { props : props; filter : Filter.t; timezone : string option }
type multiget = { props : props; hrefs : string list }

let dav = Httpz_dav.dav
let cdav local = Httpz_dav.caldav local
let timezone_name = cdav "timezone"
let default_props = [ Httpz_dav.Prop.getetag ]
let default_data () = Calendar_data.v ()
let ( let* ) = Result.bind

let query ?(props = default_props) ?data ?timezone filter =
  {
    props = Prop (props, Some (Option.value data ~default:(default_data ())));
    filter;
    timezone;
  }

let multiget ?(props = default_props) ?data hrefs =
  {
    props = Prop (props, Some (Option.value data ~default:(default_data ())));
    hrefs;
  }

let props_children = function
  | Prop (names, data) ->
      [
        Httpz_dav.el (dav "prop")
          (List.map Httpz_dav.empty names
          @ Option.to_list (Option.map Calendar_data.to_xml data));
      ]
  | Allprop -> [ Httpz_dav.empty (dav "allprop") ]
  | Propname -> [ Httpz_dav.empty (dav "propname") ]

let props_of_xml x =
  match Httpz_dav.find (dav "propname") x with
  | Some _ -> Ok Propname
  | None -> (
      match Httpz_dav.find (dav "allprop") x with
      | Some _ -> Ok Allprop
      | None -> (
          match Httpz_dav.find (dav "prop") x with
          | None -> Error "a report names no properties"
          | Some p -> (
              let names =
                List.map
                  (fun (e : Httpz_dav.element) -> e.name)
                  (List.filter
                     (fun e -> not (Httpz_dav.is Calendar_data.name e))
                     (Httpz_dav.elements p))
              in
              match Httpz_dav.find Calendar_data.name p with
              | None -> Ok (Prop (names, None))
              | Some d ->
                  let* d = Calendar_data.of_xml d in
                  Ok (Prop (names, Some d)))))

let timezone_to_xml = function
  | None -> []
  | Some tz -> [ Httpz_dav.leaf timezone_name tz ]

let timezone_of_xml x =
  Option.map (fun t -> Httpz_dav.content t) (Httpz_dav.find timezone_name x)

let equal_props a b =
  match (a, b) with
  | Prop (n0, d0), Prop (n1, d1) ->
      List.equal
        (fun (ns0, l0) (ns1, l1) -> String.equal ns0 ns1 && String.equal l0 l1)
        n0 n1
      && Option.equal Calendar_data.equal d0 d1
  | Allprop, Allprop | Propname, Propname -> true
  | (Prop _ | Allprop | Propname), _ -> false

let equal_query (a : query) (b : query) =
  equal_props a.props b.props
  && Filter.equal a.filter b.filter
  && Option.equal String.equal a.timezone b.timezone

let equal_multiget (a : multiget) (b : multiget) =
  equal_props a.props b.props && List.equal String.equal a.hrefs b.hrefs

let query_to_xml (q : query) =
  Httpz_dav.el (cdav "calendar-query")
    (props_children q.props
    @ [ Filter.to_xml q.filter ]
    @ timezone_to_xml q.timezone)

let multiget_to_xml (m : multiget) =
  Httpz_dav.el (cdav "calendar-multiget")
    (props_children m.props @ List.map (Httpz_dav.leaf (dav "href")) m.hrefs)

let free_busy_to_xml tr =
  Httpz_dav.el (cdav "free-busy-query") [ Filter.time_range_to_xml tr ]

let query_of_xml x =
  if not (Httpz_dav.is (cdav "calendar-query") x) then
    Error "the document is not a CALDAV:calendar-query"
  else
    let* props = props_of_xml x in
    match Httpz_dav.find (cdav "filter") x with
    | None -> Error "a calendar-query has no filter"
    | Some f ->
        let* filter = Filter.of_xml f in
        Ok { props; filter; timezone = timezone_of_xml x }

let multiget_of_xml x =
  if not (Httpz_dav.is (cdav "calendar-multiget") x) then
    Error "the document is not a CALDAV:calendar-multiget"
  else
    let* props = props_of_xml x in
    match
      List.map
        (fun h -> Httpz_dav.content h)
        (Httpz_dav.children (dav "href") x)
    with
    | [] -> Error "a calendar-multiget has no href"
    | hrefs -> Ok { props; hrefs }

let free_busy_of_xml x =
  if not (Httpz_dav.is (cdav "free-busy-query") x) then
    Error "the document is not a CALDAV:free-busy-query"
  else
    match Httpz_dav.find (cdav "time-range") x with
    | None -> Error "a free-busy-query has no time-range"
    | Some tr -> Filter.time_range_of_xml tr

type entry = {
  href : string;
  etag : string option;
  data : string option;
  response : Httpz_dav.response;
}

type outcome = { entries : entry list; truncated : bool }

let outcome_of_multistatus ~base (m : Httpz_dav.multistatus) =
  let truncated = ref false in
  let entries =
    List.filter_map
      (fun (r : Httpz_dav.response) ->
        let href = Httpz_dav.href r in
        if Httpz_dav.same_href href base then (
          (match r.outcome with
          | Httpz_dav.Status 507 -> truncated := true
          | _ -> ());
          None)
        else if
          not
            (let s = Httpz_dav.response_status r in
             s >= 200 && s < 300)
        then None
        else
          let etag = Httpz_dav.etag r in
          let data =
            Option.bind
              (Httpz_dav.find_property Calendar_data.name r)
              Calendar_data.data
          in
          Some { href; etag; data; response = r })
      m.responses
  in
  { entries; truncated = !truncated }

let missing (m : Httpz_dav.multistatus) =
  List.filter_map
    (fun (r : Httpz_dav.response) ->
      match r.outcome with
      | Httpz_dav.Status 404 -> Some (Httpz_dav.href r)
      | _ -> None)
    m.responses
