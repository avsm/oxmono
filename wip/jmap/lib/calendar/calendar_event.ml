(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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

let jsont =
  let make meta type_ id base_event_id uid calendar_ids is_draft is_origin title
      description description_content_type start duration time_zone
      end_time_zone utc_start utc_end show_without_time created updated sequence
      prod_id method_ locale status free_busy_status privacy color keywords
      categories recurrence_id recurrence_id_time_zone recurrence_rule
      recurrence_rules excluded_recurrence_rules recurrence_overrides excluded
      participants organizer_calendar_address reply_to locations
      main_location_id virtual_locations links use_default_alerts alerts
      may_invite_self may_invite_others hide_attendees icalendar unknown =
    {
      meta;
      type_;
      id;
      base_event_id;
      uid;
      calendar_ids;
      is_draft;
      is_origin;
      title;
      description;
      description_content_type;
      start;
      duration;
      time_zone;
      end_time_zone;
      utc_start;
      utc_end;
      show_without_time;
      created;
      updated;
      sequence;
      prod_id;
      method_;
      locale;
      status;
      free_busy_status;
      privacy;
      color;
      keywords;
      categories;
      recurrence_id;
      recurrence_id_time_zone;
      recurrence_rule;
      recurrence_rules;
      excluded_recurrence_rules;
      recurrence_overrides;
      excluded;
      participants;
      organizer_calendar_address;
      reply_to;
      locations;
      main_location_id;
      virtual_locations;
      links;
      use_default_alerts;
      alerts;
      may_invite_self;
      may_invite_others;
      hide_attendees;
      icalendar;
      unknown;
    }
  in
  Jsont.Object.map' ~kind:"CalendarEvent" ~enc_meta:(fun t -> t.meta) make
  |> Proto_json_map.nullable_mem "@type"
       (Jsont.enum [ ("Event", ()) ])
       ~enc:(fun t -> t.type_)
  |> Proto_json_map.nullable_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Proto_json_map.nullable_mem "baseEventId" Proto_id.jsont ~enc:(fun t ->
      t.base_event_id)
  |> Proto_json_map.nullable_mem "uid" Jsont.string ~enc:(fun t -> t.uid)
  |> Proto_json_map.nullable_mem "calendarIds" (Proto_json_map.of_id Jsont.bool)
       ~enc:(fun t -> t.calendar_ids)
  |> Proto_json_map.nullable_mem "isDraft" Jsont.bool ~enc:(fun t -> t.is_draft)
  |> Proto_json_map.nullable_mem "isOrigin" Jsont.bool ~enc:(fun t ->
      t.is_origin)
  |> Proto_json_map.nullable_mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun t ->
      t.description)
  |> Proto_json_map.nullable_mem "descriptionContentType" Jsont.string
       ~enc:(fun t -> t.description_content_type)
  |> Proto_json_map.nullable_mem "start" Jsont.string ~enc:(fun t -> t.start)
  |> Proto_json_map.nullable_mem "duration" Jsont.string ~enc:(fun t ->
      t.duration)
  |> Proto_json_map.nullable_mem "timeZone" Jsont.string ~enc:(fun t ->
      t.time_zone)
  |> Proto_json_map.nullable_mem "endTimeZone" Jsont.string ~enc:(fun t ->
      t.end_time_zone)
  |> Proto_json_map.nullable_mem "utcStart" Proto_date.utc_jsont ~enc:(fun t ->
      t.utc_start)
  |> Proto_json_map.nullable_mem "utcEnd" Proto_date.utc_jsont ~enc:(fun t ->
      t.utc_end)
  |> Proto_json_map.nullable_mem "showWithoutTime" Jsont.bool ~enc:(fun t ->
      t.show_without_time)
  |> Proto_json_map.nullable_mem "created" Proto_date.utc_jsont ~enc:(fun t ->
      t.created)
  |> Proto_json_map.nullable_mem "updated" Proto_date.utc_jsont ~enc:(fun t ->
      t.updated)
  |> Proto_json_map.nullable_mem "sequence" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.sequence)
  |> Proto_json_map.nullable_mem "prodId" Jsont.string ~enc:(fun t -> t.prod_id)
  |> Proto_json_map.nullable_mem "method" Jsont.string ~enc:(fun t -> t.method_)
  |> Proto_json_map.nullable_mem "locale" Jsont.string ~enc:(fun t -> t.locale)
  |> Proto_json_map.nullable_mem "status" Jsont.string ~enc:(fun t -> t.status)
  |> Proto_json_map.nullable_mem "freeBusyStatus" Jsont.string ~enc:(fun t ->
      t.free_busy_status)
  |> Proto_json_map.nullable_mem "privacy" Jsont.string ~enc:(fun t ->
      t.privacy)
  |> Proto_json_map.nullable_mem "color" Jsont.string ~enc:(fun t -> t.color)
  |> Proto_json_map.nullable_mem "keywords"
       (Proto_json_map.of_string Jsont.bool) ~enc:(fun t -> t.keywords)
  |> Proto_json_map.nullable_mem "categories"
       (Proto_json_map.of_string Jsont.bool) ~enc:(fun t -> t.categories)
  |> Proto_json_map.nullable_mem "recurrenceId" Jsont.string ~enc:(fun t ->
      t.recurrence_id)
  |> Proto_json_map.nullable_mem "recurrenceIdTimeZone" Jsont.string
       ~enc:(fun t -> t.recurrence_id_time_zone)
  |> Proto_json_map.nullable_mem "recurrenceRule" Recurrence_rule.jsont
       ~enc:(fun t -> t.recurrence_rule)
  |> Proto_json_map.nullable_mem "recurrenceRules"
       (Jsont.list Recurrence_rule.jsont) ~enc:(fun t -> t.recurrence_rules)
  |> Proto_json_map.nullable_mem "excludedRecurrenceRules"
       (Jsont.list Recurrence_rule.jsont) ~enc:(fun t ->
         t.excluded_recurrence_rules)
  |> Proto_json_map.nullable_mem "recurrenceOverrides"
       (Proto_json_map.of_string Proto_patch.jsont) ~enc:(fun t ->
         t.recurrence_overrides)
  |> Proto_json_map.nullable_mem "excluded" Jsont.bool ~enc:(fun t ->
      t.excluded)
  |> Proto_json_map.nullable_mem "participants"
       (Proto_json_map.of_id Participant.jsont) ~enc:(fun t -> t.participants)
  |> Proto_json_map.nullable_mem "organizerCalendarAddress" Jsont.string
       ~enc:(fun t -> t.organizer_calendar_address)
  |> Proto_json_map.nullable_mem "replyTo"
       (Proto_json_map.of_string Jsont.string) ~enc:(fun t -> t.reply_to)
  |> Proto_json_map.nullable_mem "locations"
       (Proto_json_map.of_id Location.jsont) ~enc:(fun t -> t.locations)
  |> Proto_json_map.nullable_mem "mainLocationId" Proto_id.jsont ~enc:(fun t ->
      t.main_location_id)
  |> Proto_json_map.nullable_mem "virtualLocations"
       (Proto_json_map.of_id Virtual_location.jsont) ~enc:(fun t ->
         t.virtual_locations)
  |> Proto_json_map.nullable_mem "links" (Proto_json_map.of_id Link.jsont)
       ~enc:(fun t -> t.links)
  |> Proto_json_map.nullable_mem "useDefaultAlerts" Jsont.bool ~enc:(fun t ->
      t.use_default_alerts)
  |> Proto_json_map.nullable_mem "alerts" (Proto_json_map.of_id Alert.jsont)
       ~enc:(fun t -> t.alerts)
  |> Proto_json_map.nullable_mem "mayInviteSelf" Jsont.bool ~enc:(fun t ->
      t.may_invite_self)
  |> Proto_json_map.nullable_mem "mayInviteOthers" Jsont.bool ~enc:(fun t ->
      t.may_invite_others)
  |> Proto_json_map.nullable_mem "hideAttendees" Jsont.bool ~enc:(fun t ->
      t.hide_attendees)
  |> Proto_json_map.nullable_mem "iCalendar" Icalendar.jsont ~enc:(fun t ->
      t.icalendar)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let empty =
  {
    meta = Jsont.Meta.none;
    type_ = None;
    id = None;
    base_event_id = None;
    uid = None;
    calendar_ids = None;
    is_draft = None;
    is_origin = None;
    title = None;
    description = None;
    description_content_type = None;
    start = None;
    duration = None;
    time_zone = None;
    end_time_zone = None;
    utc_start = None;
    utc_end = None;
    show_without_time = None;
    created = None;
    updated = None;
    sequence = None;
    prod_id = None;
    method_ = None;
    locale = None;
    status = None;
    free_busy_status = None;
    privacy = None;
    color = None;
    keywords = None;
    categories = None;
    recurrence_id = None;
    recurrence_id_time_zone = None;
    recurrence_rule = None;
    recurrence_rules = None;
    excluded_recurrence_rules = None;
    recurrence_overrides = None;
    excluded = None;
    participants = None;
    organizer_calendar_address = None;
    reply_to = None;
    locations = None;
    main_location_id = None;
    virtual_locations = None;
    links = None;
    use_default_alerts = None;
    alerts = None;
    may_invite_self = None;
    may_invite_others = None;
    hide_attendees = None;
    icalendar = None;
    unknown = Proto_unknown.empty;
  }

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

let property_to_string : [< property ] -> string = function
  | `Type_ -> "@type"
  | `Id -> "id"
  | `Base_event_id -> "baseEventId"
  | `Uid -> "uid"
  | `Calendar_ids -> "calendarIds"
  | `Is_draft -> "isDraft"
  | `Is_origin -> "isOrigin"
  | `Title -> "title"
  | `Description -> "description"
  | `Description_content_type -> "descriptionContentType"
  | `Start -> "start"
  | `Duration -> "duration"
  | `Time_zone -> "timeZone"
  | `End_time_zone -> "endTimeZone"
  | `Utc_start -> "utcStart"
  | `Utc_end -> "utcEnd"
  | `Show_without_time -> "showWithoutTime"
  | `Created -> "created"
  | `Updated -> "updated"
  | `Sequence -> "sequence"
  | `Prod_id -> "prodId"
  | `Method_ -> "method"
  | `Locale -> "locale"
  | `Status -> "status"
  | `Free_busy_status -> "freeBusyStatus"
  | `Privacy -> "privacy"
  | `Color -> "color"
  | `Keywords -> "keywords"
  | `Categories -> "categories"
  | `Recurrence_id -> "recurrenceId"
  | `Recurrence_id_time_zone -> "recurrenceIdTimeZone"
  | `Recurrence_rule -> "recurrenceRule"
  | `Recurrence_rules -> "recurrenceRules"
  | `Excluded_recurrence_rules -> "excludedRecurrenceRules"
  | `Recurrence_overrides -> "recurrenceOverrides"
  | `Excluded -> "excluded"
  | `Participants -> "participants"
  | `Organizer_calendar_address -> "organizerCalendarAddress"
  | `Reply_to -> "replyTo"
  | `Locations -> "locations"
  | `Main_location_id -> "mainLocationId"
  | `Virtual_locations -> "virtualLocations"
  | `Links -> "links"
  | `Use_default_alerts -> "useDefaultAlerts"
  | `Alerts -> "alerts"
  | `May_invite_self -> "mayInviteSelf"
  | `May_invite_others -> "mayInviteOthers"
  | `Hide_attendees -> "hideAttendees"
  | `Icalendar -> "iCalendar"

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

let filter_condition_jsont =
  Jsont.Object.map ~kind:"CalendarEventFilter"
    (fun
      in_calendar
      after
      before
      text
      title
      description
      location
      owner
      attendee
      uid
      unknown
    ->
      {
        in_calendar;
        after;
        before;
        text;
        title;
        description;
        location;
        owner;
        attendee;
        uid;
        unknown;
      })
  |> Proto_json_map.nullable_mem "inCalendar" Proto_id.jsont ~enc:(fun f ->
      f.in_calendar)
  |> Proto_json_map.nullable_mem "after" Jsont.string ~enc:(fun f -> f.after)
  |> Proto_json_map.nullable_mem "before" Jsont.string ~enc:(fun f -> f.before)
  |> Proto_json_map.nullable_mem "text" Jsont.string ~enc:(fun f -> f.text)
  |> Proto_json_map.nullable_mem "title" Jsont.string ~enc:(fun f -> f.title)
  |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun f ->
      f.description)
  |> Proto_json_map.nullable_mem "location" Jsont.string ~enc:(fun f ->
      f.location)
  |> Proto_json_map.nullable_mem "owner" Jsont.string ~enc:(fun f -> f.owner)
  |> Proto_json_map.nullable_mem "attendee" Jsont.string ~enc:(fun f ->
      f.attendee)
  |> Proto_json_map.nullable_mem "uid" Jsont.string ~enc:(fun f -> f.uid)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun f -> f.unknown)
  |> Jsont.Object.finish

let filter_jsont = Proto_filter.filter_jsont filter_condition_jsont

let filter ?in_calendar ?after ?before ?text ?title ?description ?location
    ?owner ?attendee ?uid ?(unknown = Proto_unknown.empty) () =
  Proto_filter.Condition
    {
      in_calendar;
      after;
      before;
      text;
      title;
      description;
      location;
      owner;
      attendee;
      uid;
      unknown;
    }

type sort_property = [ `Start | `Uid | `Recurrence_id | `Created | `Updated ]

let sort ?is_ascending property =
  let name =
    match property with
    | `Start -> "start"
    | `Uid -> "uid"
    | `Recurrence_id -> "recurrenceId"
    | `Created -> "created"
    | `Updated -> "updated"
  in
  Proto_filter.comparator ?is_ascending name
