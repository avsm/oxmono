(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Property = Httpz_dav.Prop

type t = {
  href : string;
  display_name : string option;
  description : string option;
  timezone : string option;
  components : string list;
  etag : string option;
  ctag : string option;
  sync_token : string option;
  reports : Httpz_dav.name list;
  data_types : (string * string) list;
  max_size : int option;
  min_date_time : Ical.Date.date_time option;
  max_date_time : Ical.Date.date_time option;
  max_instances : int option;
  max_attendees : int option;
  collations : string list;
  privileges : Httpz_dav.name list;
}

let non_empty = function "" -> None | s -> Some s

(* RFC 4791 Section 5.2.3: a calendar collection without a
   supported-calendar-component-set property accepts every component. *)
let default_components = [ "VEVENT"; "VTODO"; "VJOURNAL"; "VFREEBUSY" ]

let of_response (r : Httpz_dav.response) =
  match Httpz_dav.find_property Property.resourcetype r with
  | Some rt when Caldav_property.is_calendar rt ->
      let prop name = Httpz_dav.find_property name r in
      let display_name =
        Option.bind (prop Property.displayname) (fun p ->
            non_empty (Httpz_dav.content p))
      in
      let description =
        Option.map Httpz_dav.content (prop Caldav_property.calendar_description)
      in
      let timezone =
        Option.bind
          (prop Caldav_property.calendar_timezone)
          Caldav_property.timezone
      in
      let components =
        match prop Caldav_property.supported_calendar_component_set with
        | Some p -> Caldav_property.component_set p
        | None -> default_components
      in
      let ctag = Option.map Httpz_dav.content (prop Caldav_property.getctag) in
      let sync_token =
        Option.map Httpz_dav.content (prop Property.sync_token)
      in
      let reports =
        match prop Property.supported_report_set with
        | Some p -> Property.reports p
        | None -> []
      in
      let data_types =
        match prop Caldav_property.supported_calendar_data with
        | Some p -> Caldav_property.calendar_data_types p
        (* RFC 4791 Section 5.2.4: without the property a server accepts
           text/calendar 2.0 and nothing else. *)
        | None -> [ ("text/calendar", "2.0") ]
      in
      let max_size =
        match prop Caldav_property.max_resource_size with
        | Some p -> Caldav_property.max_size p
        | None -> None
      in
      let min_date_time =
        match prop Caldav_property.min_date_time with
        | Some p -> Caldav_property.date_time_bound p
        | None -> None
      in
      let max_date_time =
        match prop Caldav_property.max_date_time with
        | Some p -> Caldav_property.date_time_bound p
        | None -> None
      in
      let max_instances =
        match prop Caldav_property.max_instances with
        | Some p -> Caldav_property.integer p
        | None -> None
      in
      let max_attendees =
        match prop Caldav_property.max_attendees_per_instance with
        | Some p -> Caldav_property.integer p
        | None -> None
      in
      let collations =
        match prop Caldav_property.supported_collation_set with
        | Some p -> Caldav_property.collations p
        | None -> []
      in
      let privileges =
        match prop Property.current_user_privilege_set with
        | None -> []
        | Some p -> Property.privileges p
      in
      Some
        {
          href = Httpz_dav.href r;
          display_name;
          description;
          timezone;
          components;
          etag = Httpz_dav.etag r;
          ctag;
          sync_token;
          reports;
          data_types;
          max_size;
          min_date_time;
          max_date_time;
          max_instances;
          max_attendees;
          collations;
          privileges;
        }
  | Some _ | None -> None

let of_multistatus (m : Httpz_dav.multistatus) =
  List.filter_map of_response m.responses

let supports report t = List.mem report t.reports
let accepts ~component t = List.mem component t.components

let mkcalendar ?display_name ?description ?timezone ?components () =
  Httpz_dav.el Property.resourcetype
    [
      Httpz_dav.empty (Httpz_dav.dav "collection");
      Httpz_dav.empty Caldav_property.calendar;
    ]
  :: (Option.to_list
        (Option.map (Httpz_dav.leaf Property.displayname) display_name)
     @ Option.to_list (Option.map Caldav_property.description description)
     @ Option.to_list (Option.map Caldav_property.timezone_prop timezone)
     @ Option.to_list
         (Option.map Caldav_property.supported_components components))

let propfind = Httpz_dav.Prop Caldav_property.collection_props

let pp ppf t =
  match t.display_name with
  | Some n -> Format.fprintf ppf "%s (%s)" t.href n
  | None -> Format.pp_print_string ppf t.href
