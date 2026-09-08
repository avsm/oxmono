(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* RFC 5545 Sections 3.7 and 3.8, and RFC 7986 Section 5: the default value
   type of every registered property. *)
let value_types =
  let open Ical_value_type in
  [
    ("CALSCALE", Text);
    ("METHOD", Text);
    ("PRODID", Text);
    ("VERSION", Text);
    ("ATTACH", Uri);
    ("CATEGORIES", Text);
    ("CLASS", Text);
    ("COMMENT", Text);
    ("DESCRIPTION", Text);
    ("GEO", Float);
    ("LOCATION", Text);
    ("PERCENT-COMPLETE", Integer);
    ("PRIORITY", Integer);
    ("RESOURCES", Text);
    ("STATUS", Text);
    ("SUMMARY", Text);
    ("COMPLETED", Date_time);
    ("DTEND", Date_time);
    ("DUE", Date_time);
    ("DTSTART", Date_time);
    ("DURATION", Duration);
    ("FREEBUSY", Period);
    ("TRANSP", Text);
    ("TZID", Text);
    ("TZNAME", Text);
    ("TZOFFSETFROM", Utc_offset);
    ("TZOFFSETTO", Utc_offset);
    ("TZURL", Uri);
    ("ATTENDEE", Cal_address);
    ("CONTACT", Text);
    ("ORGANIZER", Cal_address);
    ("RECURRENCE-ID", Date_time);
    ("RELATED-TO", Text);
    ("URL", Uri);
    ("UID", Text);
    ("EXDATE", Date_time);
    ("RDATE", Date_time);
    ("RRULE", Recur);
    ("ACTION", Text);
    ("REPEAT", Integer);
    ("TRIGGER", Duration);
    ("CREATED", Date_time);
    ("DTSTAMP", Date_time);
    ("LAST-MODIFIED", Date_time);
    ("SEQUENCE", Integer);
    ("REQUEST-STATUS", Text);
    ("NAME", Text);
    ("REFRESH-INTERVAL", Duration);
    ("SOURCE", Uri);
    ("COLOR", Text);
    (* RFC 7986 Section 5.10 gives IMAGE "URI or BINARY -- no default" and
       requires an explicit VALUE parameter. URI is the reading used when a
       sender omits it, since the alternative default is TEXT. *)
    ("IMAGE", Uri);
    ("CONFERENCE", Uri);
  ]

let value_type name = List.assoc_opt (String.uppercase_ascii name) value_types
let names = List.map fst value_types

type cardinality = One | At_most_one | Many

let components =
  [
    "VCALENDAR";
    "VEVENT";
    "VTODO";
    "VJOURNAL";
    "VFREEBUSY";
    "VTIMEZONE";
    "STANDARD";
    "DAYLIGHT";
    "VALARM";
  ]

let children name =
  match String.uppercase_ascii name with
  | "VCALENDAR" -> [ "VEVENT"; "VTODO"; "VJOURNAL"; "VFREEBUSY"; "VTIMEZONE" ]
  | "VEVENT" | "VTODO" -> [ "VALARM" ]
  | "VTIMEZONE" -> [ "STANDARD"; "DAYLIGHT" ]
  | _ -> []

let required name =
  match String.uppercase_ascii name with
  | "VEVENT" | "VTODO" | "VJOURNAL" | "VFREEBUSY" -> [ "UID"; "DTSTAMP" ]
  | "VTIMEZONE" -> [ "TZID" ]
  | "STANDARD" | "DAYLIGHT" -> [ "DTSTART"; "TZOFFSETTO"; "TZOFFSETFROM" ]
  | "VALARM" -> [ "ACTION"; "TRIGGER" ]
  | "VCALENDAR" -> [ "PRODID"; "VERSION" ]
  | _ -> []

(* RFC 5545 Section 3.6.1: eventprop. *)
let vevent =
  [
    ("COLOR", At_most_one);
    ("DTSTAMP", One);
    ("UID", One);
    ("DTSTART", At_most_one);
    ("CLASS", At_most_one);
    ("CREATED", At_most_one);
    ("DESCRIPTION", At_most_one);
    ("GEO", At_most_one);
    ("LAST-MODIFIED", At_most_one);
    ("LOCATION", At_most_one);
    ("ORGANIZER", At_most_one);
    ("PRIORITY", At_most_one);
    ("SEQUENCE", At_most_one);
    ("STATUS", At_most_one);
    ("SUMMARY", At_most_one);
    ("TRANSP", At_most_one);
    ("URL", At_most_one);
    ("RECURRENCE-ID", At_most_one);
    ("RRULE", At_most_one);
    ("DTEND", At_most_one);
    ("DURATION", At_most_one);
    ("ATTACH", Many);
    ("ATTENDEE", Many);
    ("CATEGORIES", Many);
    ("COMMENT", Many);
    ("CONTACT", Many);
    ("EXDATE", Many);
    ("REQUEST-STATUS", Many);
    ("RELATED-TO", Many);
    ("RESOURCES", Many);
    ("RDATE", Many);
  ]

(* RFC 5545 Section 3.6.2: todoprop. *)
let vtodo =
  [
    ("COLOR", At_most_one);
    ("DTSTAMP", One);
    ("UID", One);
    ("CLASS", At_most_one);
    ("COMPLETED", At_most_one);
    ("CREATED", At_most_one);
    ("DESCRIPTION", At_most_one);
    ("DTSTART", At_most_one);
    ("GEO", At_most_one);
    ("LAST-MODIFIED", At_most_one);
    ("LOCATION", At_most_one);
    ("ORGANIZER", At_most_one);
    ("PERCENT-COMPLETE", At_most_one);
    ("PRIORITY", At_most_one);
    ("RECURRENCE-ID", At_most_one);
    ("SEQUENCE", At_most_one);
    ("STATUS", At_most_one);
    ("SUMMARY", At_most_one);
    ("URL", At_most_one);
    ("RRULE", At_most_one);
    ("DUE", At_most_one);
    ("DURATION", At_most_one);
    ("ATTACH", Many);
    ("ATTENDEE", Many);
    ("CATEGORIES", Many);
    ("COMMENT", Many);
    ("CONTACT", Many);
    ("EXDATE", Many);
    ("REQUEST-STATUS", Many);
    ("RELATED-TO", Many);
    ("RESOURCES", Many);
    ("RDATE", Many);
  ]

(* RFC 5545 Section 3.6.3: jourprop. *)
let vjournal =
  [
    ("COLOR", At_most_one);
    ("DTSTAMP", One);
    ("UID", One);
    ("CLASS", At_most_one);
    ("CREATED", At_most_one);
    ("DTSTART", At_most_one);
    ("LAST-MODIFIED", At_most_one);
    ("ORGANIZER", At_most_one);
    ("RECURRENCE-ID", At_most_one);
    ("SEQUENCE", At_most_one);
    ("STATUS", At_most_one);
    ("SUMMARY", At_most_one);
    ("URL", At_most_one);
    ("RRULE", At_most_one);
    ("ATTACH", Many);
    ("ATTENDEE", Many);
    ("CATEGORIES", Many);
    ("COMMENT", Many);
    ("CONTACT", Many);
    ("DESCRIPTION", Many);
    ("EXDATE", Many);
    ("RELATED-TO", Many);
    ("RDATE", Many);
    ("REQUEST-STATUS", Many);
  ]

(* RFC 5545 Section 3.6.4: fbprop. *)
let vfreebusy =
  [
    ("DTSTAMP", One);
    ("UID", One);
    ("CONTACT", At_most_one);
    ("DTSTART", At_most_one);
    ("DTEND", At_most_one);
    ("ORGANIZER", At_most_one);
    ("URL", At_most_one);
    ("ATTENDEE", Many);
    ("COMMENT", Many);
    ("FREEBUSY", Many);
    ("REQUEST-STATUS", Many);
  ]

(* RFC 5545 Section 3.6.5: the properties of VTIMEZONE itself. *)
let vtimezone =
  [ ("TZID", One); ("LAST-MODIFIED", At_most_one); ("TZURL", At_most_one) ]

(* RFC 5545 Section 3.6.5: tzprop, shared by STANDARD and DAYLIGHT. *)
let tzprop =
  [
    ("DTSTART", One);
    ("TZOFFSETTO", One);
    ("TZOFFSETFROM", One);
    ("RRULE", At_most_one);
    ("COMMENT", Many);
    ("RDATE", Many);
    ("TZNAME", Many);
  ]

(* RFC 5545 Section 3.6.6: audioprop, dispprop and emailprop, merged. *)
let valarm =
  [
    ("ACTION", One);
    ("TRIGGER", One);
    ("DURATION", At_most_one);
    ("REPEAT", At_most_one);
    ("DESCRIPTION", At_most_one);
    ("SUMMARY", At_most_one);
    ("ATTACH", Many);
    ("ATTENDEE", Many);
  ]

(* RFC 5545 Section 3.7: calprops. *)
(* RFC 7986 Sections 5.3 to 5.9 add properties to the calendar object itself,
   each "specified once in an iCalendar object". *)
let vcalendar =
  [
    ("PRODID", One);
    ("VERSION", One);
    ("CALSCALE", At_most_one);
    ("METHOD", At_most_one);
    ("UID", At_most_one);
    ("LAST-MODIFIED", At_most_one);
    ("URL", At_most_one);
    ("REFRESH-INTERVAL", At_most_one);
    ("SOURCE", At_most_one);
    ("COLOR", At_most_one);
  ]

let cardinality ~component name =
  let table =
    match String.uppercase_ascii component with
    | "VEVENT" -> vevent
    | "VTODO" -> vtodo
    | "VJOURNAL" -> vjournal
    | "VFREEBUSY" -> vfreebusy
    | "VTIMEZONE" -> vtimezone
    | "STANDARD" | "DAYLIGHT" -> tzprop
    | "VALARM" -> valarm
    | "VCALENDAR" -> vcalendar
    | _ -> []
  in
  List.assoc_opt (String.uppercase_ascii name) table
