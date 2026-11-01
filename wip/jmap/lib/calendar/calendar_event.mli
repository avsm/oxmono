@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Typed CalendarEvent properties. [None] represents a property omitted by a
    projection or returned as null. Unrecognised fields are retained in
    [unknown], including fields of nested extensible objects. [meta] locates the
    original object for {!Proto_response.source_fragment}. *)

module Recurrence_rule = Calendar_types.Recurrence_rule
module Participant = Calendar_types.Participant
module Location = Calendar_types.Location
module Virtual_location = Calendar_types.Virtual_location
module Link = Calendar_types.Link
module Alert = Calendar_types.Alert
module Icalendar = Calendar_types.Icalendar

type t = {
  meta : Jsont.Meta.t;
  type_ : unit option;
  id : Proto_id.t option;
  base_event_id : Proto_id.t option;
  uid : string option;
  calendar_ids : (Proto_id.t * bool) list option;
  is_draft : bool option;
  is_origin : bool option;
  title : string option;
  description : string option;
  description_content_type : string option;
  start : string option;
  duration : string option;
  time_zone : string option;
  end_time_zone : string option;
  utc_start : Ptime.t option;
  utc_end : Ptime.t option;
  show_without_time : bool option;
  created : Ptime.t option;
  updated : Ptime.t option;
  sequence : int64 option;
  prod_id : string option;
  method_ : string option;
  locale : string option;
  status : string option;
  free_busy_status : string option;
  privacy : string option;
  color : string option;
  keywords : (string * bool) list option;
  categories : (string * bool) list option;
  recurrence_id : string option;
  recurrence_id_time_zone : string option;
  recurrence_rule : Recurrence_rule.t option;
  recurrence_rules : Recurrence_rule.t list option;
  excluded_recurrence_rules : Recurrence_rule.t list option;
  recurrence_overrides : (string * Proto_patch.t) list option;
  excluded : bool option;
  participants : (Proto_id.t * Participant.t) list option;
  organizer_calendar_address : string option;
  reply_to : (string * string) list option;
  locations : (Proto_id.t * Location.t) list option;
  main_location_id : Proto_id.t option;
  virtual_locations : (Proto_id.t * Virtual_location.t) list option;
  links : (Proto_id.t * Link.t) list option;
  use_default_alerts : bool option;
  alerts : (Proto_id.t * Alert.t) list option;
  may_invite_self : bool option;
  may_invite_others : bool option;
  hide_attendees : bool option;
  icalendar : Icalendar.t option;
  unknown : Proto_unknown.t;
}

val jsont : t Jsont.t
(** [jsont] codes CalendarEvent properties and retains unrecognised members. *)

val empty : t
(** [empty] has no properties set. *)

type property =
  [ `Type_
  | `Id
  | `Base_event_id
  | `Uid
  | `Calendar_ids
  | `Is_draft
  | `Is_origin
  | `Title
  | `Description
  | `Description_content_type
  | `Start
  | `Duration
  | `Time_zone
  | `End_time_zone
  | `Utc_start
  | `Utc_end
  | `Show_without_time
  | `Created
  | `Updated
  | `Sequence
  | `Prod_id
  | `Method_
  | `Locale
  | `Status
  | `Free_busy_status
  | `Privacy
  | `Color
  | `Keywords
  | `Categories
  | `Recurrence_id
  | `Recurrence_id_time_zone
  | `Recurrence_rule
  | `Recurrence_rules
  | `Excluded_recurrence_rules
  | `Recurrence_overrides
  | `Excluded
  | `Participants
  | `Organizer_calendar_address
  | `Reply_to
  | `Locations
  | `Main_location_id
  | `Virtual_locations
  | `Links
  | `Use_default_alerts
  | `Alerts
  | `May_invite_self
  | `May_invite_others
  | `Hide_attendees
  | `Icalendar ]

val property_to_string : [< property ] -> string

type filter_condition = {
  in_calendar : Proto_id.t option;
  after : string option;
  before : string option;
  text : string option;
  title : string option;
  description : string option;
  location : string option;
  owner : string option;
  attendee : string option;
  uid : string option;
  unknown : Proto_unknown.t;
}

type filter = filter_condition Proto_filter.filter

val filter_jsont : filter Jsont.t
(** [filter_jsont] codes nested filter operators and typed conditions. *)

val filter :
  ?in_calendar:Proto_id.t ->
  ?after:string ->
  ?before:string ->
  ?text:string ->
  ?title:string ->
  ?description:string ->
  ?location:string ->
  ?owner:string ->
  ?attendee:string ->
  ?uid:string ->
  ?unknown:Proto_unknown.t ->
  unit ->
  filter
(** [filter ()] matches all events. Dates are local times in the query's zone.
*)

type sort_property = [ `Start | `Uid | `Recurrence_id | `Created | `Updated ]

val sort : ?is_ascending:bool -> sort_property -> Proto_filter.comparator
(** [sort property] orders events by [property]. *)
