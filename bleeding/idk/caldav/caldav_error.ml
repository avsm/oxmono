(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let caldav local = Httpz_dav.caldav local
let supported_calendar_data = caldav "supported-calendar-data"
let valid_calendar_data = caldav "valid-calendar-data"
let valid_calendar_object_resource = caldav "valid-calendar-object-resource"
let supported_calendar_component = caldav "supported-calendar-component"
let no_uid_conflict = caldav "no-uid-conflict"
let calendar_collection_location_ok = caldav "calendar-collection-location-ok"
let max_resource_size = caldav "max-resource-size"
let min_date_time = caldav "min-date-time"
let max_date_time = caldav "max-date-time"
let max_instances = caldav "max-instances"
let max_attendees_per_instance = caldav "max-attendees-per-instance"
let supported_filter = caldav "supported-filter"
let valid_filter = caldav "valid-filter"
let supported_collation = caldav "supported-collation"

let conflicting_uid e =
  match Httpz_dav.Condition.hrefs no_uid_conflict e with
  | h :: _ -> Some h
  | [] -> None

let describe (ns, local) =
  if ns = "urn:ietf:params:xml:ns:caldav" then
    match local with
    | "supported-calendar-data" ->
        "The resource's media type is not one this calendar collection accepts."
    | "valid-calendar-data" -> "The submitted data is not valid iCalendar data."
    | "valid-calendar-object-resource" ->
        "The calendar object resource does not obey the restrictions of RFC \
         4791 Section 4.1."
    | "supported-calendar-component" ->
        "The calendar object resource holds a component type this calendar \
         collection does not accept."
    | "no-uid-conflict" ->
        "The UID is already in use by another resource in this calendar \
         collection."
    | "calendar-collection-location-ok" ->
        "A calendar collection cannot be created at that destination."
    | "max-resource-size" ->
        "The resource is larger than this calendar collection accepts."
    | "min-date-time" ->
        "A date or date-time in the request is earlier than this calendar \
         collection accepts."
    | "max-date-time" ->
        "A date or date-time in the request is later than this calendar \
         collection accepts."
    | "max-instances" ->
        "The resource generates more recurrence instances than this calendar \
         collection accepts."
    | "max-attendees-per-instance" ->
        "An instance of the resource has more ATTENDEE properties than this \
         calendar collection accepts."
    | "supported-filter" ->
        "The filter names a calendar component, property or parameter this \
         server does not support querying."
    | "valid-filter" -> "The filter is not a valid CALDAV:filter."
    | "supported-collation" ->
        "The collation named is not one this server supports."
    | _ -> local
  else if ns = "DAV:" then
    match local with
    | "cannot-modify-protected-property" ->
        "The property is protected and cannot be set."
    | "preserved-live-properties" ->
        "A live property of the resource could not be preserved."
    | "propfind-finite-depth" ->
        "The server does not support an infinite-depth PROPFIND here."
    | "no-external-entities" ->
        "The request body must not reference an external XML entity."
    | "no-conflicting-lock" ->
        "a lock on the resource conflicts with the one requested"
    | "lock-token-submitted" ->
        "A resource is locked and no valid lock token was submitted."
    | "allow-client-defined-uri" ->
        "The collection does not allow the client to name the new member."
    | "valid-sync-token" -> "The sync token is not valid for this collection."
    | "number-of-matches-within-limits" ->
        "The number of matches did not fit within the server's limits."
    | "supported-report" ->
        "The report named is not supported on this resource."
    | "sync-traversal-supported" ->
        "The requested synchronisation depth is not supported here."
    | "need-privileges" ->
        "The current user lacks the privileges the request requires."
    | _ -> local
  else local
