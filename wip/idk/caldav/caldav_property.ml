(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Property = Httpz_dav.Prop

type name = Httpz_dav.name

let caldav local = Httpz_dav.caldav local
let calendar = caldav "calendar"
let calendar_query = caldav "calendar-query"
let calendar_multiget = caldav "calendar-multiget"
let free_busy_query = caldav "free-busy-query"
let calendar_home_set = caldav "calendar-home-set"
let calendar_description = caldav "calendar-description"
let calendar_timezone = caldav "calendar-timezone"
let supported_calendar_component_set = caldav "supported-calendar-component-set"
let supported_calendar_data = caldav "supported-calendar-data"
let max_resource_size = caldav "max-resource-size"
let min_date_time = caldav "min-date-time"
let max_date_time = caldav "max-date-time"
let max_instances = caldav "max-instances"
let max_attendees_per_instance = caldav "max-attendees-per-instance"
let supported_collation_set = caldav "supported-collation-set"
let calendar_user_address_set = caldav "calendar-user-address-set"
let getctag = ("http://calendarserver.org/ns/", "getctag")
let comp_name = caldav "comp"
let calendar_data_name = caldav "calendar-data"
let supported_collation = caldav "supported-collation"
let attr_name = ("", "name")
let attr_content_type = ("", "content-type")
let attr_version = ("", "version")
let xml_lang = ("http://www.w3.org/XML/1998/namespace", "lang")

let is_calendar p =
  Httpz_dav.is Property.resourcetype p
  && List.mem calendar (Property.resource_types p)

let component_set p =
  if not (Httpz_dav.is supported_calendar_component_set p) then []
  else
    List.filter_map (Httpz_dav.attr attr_name) (Httpz_dav.children comp_name p)

let calendar_data_types p =
  if not (Httpz_dav.is supported_calendar_data p) then []
  else
    List.map
      (fun e ->
        ( Option.value ~default:"text/calendar"
            (Httpz_dav.attr attr_content_type e),
          Option.value ~default:"2.0" (Httpz_dav.attr attr_version e) ))
      (Httpz_dav.children calendar_data_name p)

let timezone p =
  if Httpz_dav.is calendar_timezone p then Some (Httpz_dav.content p) else None

let collations p =
  if not (Httpz_dav.is supported_collation_set p) then []
  else
    List.map
      (fun e -> String.trim (Httpz_dav.content e))
      (Httpz_dav.children supported_collation p)

let max_size p =
  if Httpz_dav.is max_resource_size p then
    int_of_string_opt (String.trim (Httpz_dav.content p))
  else None

let is_date_time_bound p =
  Httpz_dav.is min_date_time p || Httpz_dav.is max_date_time p

let date_time_bound p =
  if not (is_date_time_bound p) then None
  else
    match Ical.Date.date_time_of_string (String.trim (Httpz_dav.content p)) with
    | Ok dt -> Some dt
    | Error _ -> None

let is_integer_prop p =
  Httpz_dav.is max_instances p || Httpz_dav.is max_attendees_per_instance p

let integer p =
  if not (is_integer_prop p) then None
  else int_of_string_opt (String.trim (Httpz_dav.content p))

let description ?lang s =
  let attrs = match lang with None -> [] | Some l -> [ (xml_lang, l) ] in
  Httpz_dav.element ~attrs calendar_description [ Httpz_dav.Text s ]

let timezone_prop ical = Httpz_dav.leaf calendar_timezone ical

let supported_components names =
  Httpz_dav.el supported_calendar_component_set
    (List.map
       (fun n -> Httpz_dav.el ~attrs:[ (attr_name, n) ] comp_name [])
       names)

let collection_props =
  [
    Property.resourcetype;
    Property.displayname;
    Property.getetag;
    Property.sync_token;
    Property.supported_report_set;
    Property.current_user_privilege_set;
    getctag;
    calendar_description;
    calendar_timezone;
    supported_calendar_component_set;
    supported_calendar_data;
    max_resource_size;
    min_date_time;
    max_date_time;
    max_instances;
    max_attendees_per_instance;
    supported_collation_set;
  ]

let principal_props =
  [
    calendar_home_set;
    calendar_user_address_set;
    Property.displayname;
    Property.principal_url;
  ]
