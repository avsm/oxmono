(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type frequency =
  [ `Yearly | `Monthly | `Weekly | `Daily | `Hourly | `Minutely | `Secondly ]

let frequency_jsont : frequency Jsont.t =
  Jsont.enum
    ([
       ("yearly", `Yearly);
       ("monthly", `Monthly);
       ("weekly", `Weekly);
       ("daily", `Daily);
       ("hourly", `Hourly);
       ("minutely", `Minutely);
       ("secondly", `Secondly);
     ]
      : (string * frequency) list)

type weekday = [ `Mo | `Tu | `We | `Th | `Fr | `Sa | `Su ]

let weekday_jsont : weekday Jsont.t =
  Jsont.enum
    ([
       ("mo", `Mo);
       ("tu", `Tu);
       ("we", `We);
       ("th", `Th);
       ("fr", `Fr);
       ("sa", `Sa);
       ("su", `Su);
     ]
      : (string * weekday) list)

module Nday = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    day : weekday;
    nth_of_period : int64 option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ day nth_of_period unknown =
      { meta; type_; day; nth_of_period; unknown }
    in
    Jsont.Object.map' ~kind:"NDay" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("NDay", ()) ])
         ~enc:(fun t -> t.type_)
    |> Jsont.Object.mem "day" weekday_jsont ~enc:(fun t -> t.day)
    |> Proto_json_map.nullable_mem "nthOfPeriod" Proto_int53.Signed.jsont
         ~enc:(fun t -> t.nth_of_period)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end

module Recurrence_rule = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    frequency : frequency;
    interval : int64 option;
    rscale : string option;
    skip : string option;
    first_day_of_week : weekday option;
    by_day : Nday.t list option;
    by_month : string list option;
    by_month_day : int64 list option;
    by_year_day : int64 list option;
    by_week_no : int64 list option;
    by_hour : int64 list option;
    by_minute : int64 list option;
    by_second : int64 list option;
    by_set_position : int64 list option;
    count : int64 option;
    until : string option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ frequency interval rscale skip first_day_of_week by_day
        by_month by_month_day by_year_day by_week_no by_hour by_minute by_second
        by_set_position count until unknown =
      {
        meta;
        type_;
        frequency;
        interval;
        rscale;
        skip;
        first_day_of_week;
        by_day;
        by_month;
        by_month_day;
        by_year_day;
        by_week_no;
        by_hour;
        by_minute;
        by_second;
        by_set_position;
        count;
        until;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"RecurrenceRule" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("RecurrenceRule", ()) ])
         ~enc:(fun t -> t.type_)
    |> Jsont.Object.mem "frequency" frequency_jsont ~enc:(fun t -> t.frequency)
    |> Proto_json_map.nullable_mem "interval" Proto_int53.Unsigned.jsont
         ~enc:(fun t -> t.interval)
    |> Proto_json_map.nullable_mem "rscale" Jsont.string ~enc:(fun t ->
        t.rscale)
    |> Proto_json_map.nullable_mem "skip" Jsont.string ~enc:(fun t -> t.skip)
    |> Proto_json_map.nullable_mem "firstDayOfWeek" weekday_jsont ~enc:(fun t ->
        t.first_day_of_week)
    |> Proto_json_map.nullable_mem "byDay" (Jsont.list Nday.jsont)
         ~enc:(fun t -> t.by_day)
    |> Proto_json_map.nullable_mem "byMonth" (Jsont.list Jsont.string)
         ~enc:(fun t -> t.by_month)
    |> Proto_json_map.nullable_mem "byMonthDay"
         (Jsont.list Proto_int53.Signed.jsont) ~enc:(fun t -> t.by_month_day)
    |> Proto_json_map.nullable_mem "byYearDay"
         (Jsont.list Proto_int53.Signed.jsont) ~enc:(fun t -> t.by_year_day)
    |> Proto_json_map.nullable_mem "byWeekNo"
         (Jsont.list Proto_int53.Signed.jsont) ~enc:(fun t -> t.by_week_no)
    |> Proto_json_map.nullable_mem "byHour"
         (Jsont.list Proto_int53.Unsigned.jsont) ~enc:(fun t -> t.by_hour)
    |> Proto_json_map.nullable_mem "byMinute"
         (Jsont.list Proto_int53.Unsigned.jsont) ~enc:(fun t -> t.by_minute)
    |> Proto_json_map.nullable_mem "bySecond"
         (Jsont.list Proto_int53.Unsigned.jsont) ~enc:(fun t -> t.by_second)
    |> Proto_json_map.nullable_mem "bySetPosition"
         (Jsont.list Proto_int53.Signed.jsont) ~enc:(fun t -> t.by_set_position)
    |> Proto_json_map.nullable_mem "count" Proto_int53.Unsigned.jsont
         ~enc:(fun t -> t.count)
    |> Proto_json_map.nullable_mem "until" Jsont.string ~enc:(fun t -> t.until)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end

module Link = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    href : string option;
    blob_id : Proto_id.t option;
    content_type : string option;
    size : int64 option;
    rel : string option;
    display : string option;
    title : string option;
    cid : string option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ href blob_id content_type size rel display title cid
        unknown =
      {
        meta;
        type_;
        href;
        blob_id;
        content_type;
        size;
        rel;
        display;
        title;
        cid;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"Link" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("Link", ()) ])
         ~enc:(fun t -> t.type_)
    |> Proto_json_map.nullable_mem "href" Jsont.string ~enc:(fun t -> t.href)
    |> Proto_json_map.nullable_mem "blobId" Proto_id.jsont ~enc:(fun t ->
        t.blob_id)
    |> Proto_json_map.nullable_mem "contentType" Jsont.string ~enc:(fun t ->
        t.content_type)
    |> Proto_json_map.nullable_mem "size" Proto_int53.Unsigned.jsont
         ~enc:(fun t -> t.size)
    |> Proto_json_map.nullable_mem "rel" Jsont.string ~enc:(fun t -> t.rel)
    |> Proto_json_map.nullable_mem "display" Jsont.string ~enc:(fun t ->
        t.display)
    |> Proto_json_map.nullable_mem "title" Jsont.string ~enc:(fun t -> t.title)
    |> Proto_json_map.nullable_mem "cid" Jsont.string ~enc:(fun t -> t.cid)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish

  let empty =
    {
      meta = Jsont.Meta.none;
      type_ = None;
      href = None;
      blob_id = None;
      content_type = None;
      size = None;
      rel = None;
      display = None;
      title = None;
      cid = None;
      unknown = Proto_unknown.empty;
    }
end

module Location = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string option;
    description : string option;
    description_content_type : string option;
    coordinates : string option;
    time_zone : string option;
    relative_to : string option;
    location_types : (string * bool) list option;
    links : (Proto_id.t * Link.t) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ name description description_content_type coordinates
        time_zone relative_to location_types links unknown =
      {
        meta;
        type_;
        name;
        description;
        description_content_type;
        coordinates;
        time_zone;
        relative_to;
        location_types;
        links;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"Location" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("Location", ()) ])
         ~enc:(fun t -> t.type_)
    |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun t ->
        t.description)
    |> Proto_json_map.nullable_mem "descriptionContentType" Jsont.string
         ~enc:(fun t -> t.description_content_type)
    |> Proto_json_map.nullable_mem "coordinates" Jsont.string ~enc:(fun t ->
        t.coordinates)
    |> Proto_json_map.nullable_mem "timeZone" Jsont.string ~enc:(fun t ->
        t.time_zone)
    |> Proto_json_map.nullable_mem "relativeTo" Jsont.string ~enc:(fun t ->
        t.relative_to)
    |> Proto_json_map.nullable_mem "locationTypes"
         (Proto_json_map.of_string Jsont.bool) ~enc:(fun t -> t.location_types)
    |> Proto_json_map.nullable_mem "links" (Proto_json_map.of_id Link.jsont)
         ~enc:(fun t -> t.links)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish

  let empty =
    {
      meta = Jsont.Meta.none;
      type_ = None;
      name = None;
      description = None;
      description_content_type = None;
      coordinates = None;
      time_zone = None;
      relative_to = None;
      location_types = None;
      links = None;
      unknown = Proto_unknown.empty;
    }
end

module Virtual_location = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string option;
    description : string option;
    description_content_type : string option;
    uri : string option;
    features : (string * bool) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ name description description_content_type uri features
        unknown =
      {
        meta;
        type_;
        name;
        description;
        description_content_type;
        uri;
        features;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"VirtualLocation" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("VirtualLocation", ()) ])
         ~enc:(fun t -> t.type_)
    |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun t ->
        t.description)
    |> Proto_json_map.nullable_mem "descriptionContentType" Jsont.string
         ~enc:(fun t -> t.description_content_type)
    |> Proto_json_map.nullable_mem "uri" Jsont.string ~enc:(fun t -> t.uri)
    |> Proto_json_map.nullable_mem "features"
         (Proto_json_map.of_string Jsont.bool) ~enc:(fun t -> t.features)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish

  let empty =
    {
      meta = Jsont.Meta.none;
      type_ = None;
      name = None;
      description = None;
      description_content_type = None;
      uri = None;
      features = None;
      unknown = Proto_unknown.empty;
    }
end

module Participant = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string option;
    description : string option;
    description_content_type : string option;
    email : string option;
    calendar_address : string option;
    send_to : (string * string) list option;
    kind : string option;
    roles : (string * bool) list option;
    location_id : Proto_id.t option;
    language : string option;
    participation_status : string option;
    participation_comment : string option;
    expect_reply : bool option;
    schedule_agent : string option;
    schedule_force_send : bool option;
    schedule_status : string list option;
    schedule_sequence : int64 option;
    schedule_updated : Ptime.t option;
    delegated_to : (Proto_id.t * bool) list option;
    delegated_from : (Proto_id.t * bool) list option;
    member_of : (Proto_id.t * bool) list option;
    links : (Proto_id.t * Link.t) list option;
    invited_by : Proto_id.t option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ name description description_content_type email
        calendar_address send_to kind roles location_id language
        participation_status participation_comment expect_reply schedule_agent
        schedule_force_send schedule_status schedule_sequence schedule_updated
        delegated_to delegated_from member_of links invited_by unknown =
      {
        meta;
        type_;
        name;
        description;
        description_content_type;
        email;
        calendar_address;
        send_to;
        kind;
        roles;
        location_id;
        language;
        participation_status;
        participation_comment;
        expect_reply;
        schedule_agent;
        schedule_force_send;
        schedule_status;
        schedule_sequence;
        schedule_updated;
        delegated_to;
        delegated_from;
        member_of;
        links;
        invited_by;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"Participant" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("Participant", ()) ])
         ~enc:(fun t -> t.type_)
    |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun t ->
        t.description)
    |> Proto_json_map.nullable_mem "descriptionContentType" Jsont.string
         ~enc:(fun t -> t.description_content_type)
    |> Proto_json_map.nullable_mem "email" Jsont.string ~enc:(fun t -> t.email)
    |> Proto_json_map.nullable_mem "calendarAddress" Jsont.string ~enc:(fun t ->
        t.calendar_address)
    |> Proto_json_map.nullable_mem "sendTo"
         (Proto_json_map.of_string Jsont.string) ~enc:(fun t -> t.send_to)
    |> Proto_json_map.nullable_mem "kind" Jsont.string ~enc:(fun t -> t.kind)
    |> Proto_json_map.nullable_mem "roles" (Proto_json_map.of_string Jsont.bool)
         ~enc:(fun t -> t.roles)
    |> Proto_json_map.nullable_mem "locationId" Proto_id.jsont ~enc:(fun t ->
        t.location_id)
    |> Proto_json_map.nullable_mem "language" Jsont.string ~enc:(fun t ->
        t.language)
    |> Proto_json_map.nullable_mem "participationStatus" Jsont.string
         ~enc:(fun t -> t.participation_status)
    |> Proto_json_map.nullable_mem "participationComment" Jsont.string
         ~enc:(fun t -> t.participation_comment)
    |> Proto_json_map.nullable_mem "expectReply" Jsont.bool ~enc:(fun t ->
        t.expect_reply)
    |> Proto_json_map.nullable_mem "scheduleAgent" Jsont.string ~enc:(fun t ->
        t.schedule_agent)
    |> Proto_json_map.nullable_mem "scheduleForceSend" Jsont.bool ~enc:(fun t ->
        t.schedule_force_send)
    |> Proto_json_map.nullable_mem "scheduleStatus" (Jsont.list Jsont.string)
         ~enc:(fun t -> t.schedule_status)
    |> Proto_json_map.nullable_mem "scheduleSequence" Proto_int53.Unsigned.jsont
         ~enc:(fun t -> t.schedule_sequence)
    |> Proto_json_map.nullable_mem "scheduleUpdated" Proto_date.utc_jsont
         ~enc:(fun t -> t.schedule_updated)
    |> Proto_json_map.nullable_mem "delegatedTo"
         (Proto_json_map.of_id Jsont.bool) ~enc:(fun t -> t.delegated_to)
    |> Proto_json_map.nullable_mem "delegatedFrom"
         (Proto_json_map.of_id Jsont.bool) ~enc:(fun t -> t.delegated_from)
    |> Proto_json_map.nullable_mem "memberOf" (Proto_json_map.of_id Jsont.bool)
         ~enc:(fun t -> t.member_of)
    |> Proto_json_map.nullable_mem "links" (Proto_json_map.of_id Link.jsont)
         ~enc:(fun t -> t.links)
    |> Proto_json_map.nullable_mem "invitedBy" Proto_id.jsont ~enc:(fun t ->
        t.invited_by)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish

  let empty =
    {
      meta = Jsont.Meta.none;
      type_ = None;
      name = None;
      description = None;
      description_content_type = None;
      email = None;
      calendar_address = None;
      send_to = None;
      kind = None;
      roles = None;
      location_id = None;
      language = None;
      participation_status = None;
      participation_comment = None;
      expect_reply = None;
      schedule_agent = None;
      schedule_force_send = None;
      schedule_status = None;
      schedule_sequence = None;
      schedule_updated = None;
      delegated_to = None;
      delegated_from = None;
      member_of = None;
      links = None;
      invited_by = None;
      unknown = Proto_unknown.empty;
    }
end

module Trigger = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : string;
    offset : string option;
    relative_to : string option;
    when_ : Ptime.t option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ offset relative_to when_ unknown =
      { meta; type_; offset; relative_to; when_; unknown }
    in
    Jsont.Object.map' ~kind:"Alert trigger" ~enc_meta:(fun t -> t.meta) make
    |> Jsont.Object.mem "@type" Jsont.string
         ~dec_absent:(fun () -> "OffsetTrigger")
         ~enc:(fun t -> t.type_)
    |> Proto_json_map.nullable_mem "offset" Jsont.string ~enc:(fun t ->
        t.offset)
    |> Proto_json_map.nullable_mem "relativeTo" Jsont.string ~enc:(fun t ->
        t.relative_to)
    |> Proto_json_map.nullable_mem "when" Proto_date.utc_jsont ~enc:(fun t ->
        t.when_)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
    |> Jsont.iter ~dec:(fun t ->
        match t.type_ with
        | "OffsetTrigger" when t.offset = None ->
            Jsont.Error.msg t.meta "OffsetTrigger requires offset."
        | "AbsoluteTrigger" when t.when_ = None ->
            Jsont.Error.msg t.meta "AbsoluteTrigger requires when."
        | _ -> ())
end

module Relation = struct
  type t = {
    meta : Jsont.Meta.t;
    relation : (string * bool) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    Jsont.Object.map' ~kind:"Relation"
      ~enc_meta:(fun t -> t.meta)
      (fun meta relation unknown -> { meta; relation; unknown })
    |> Proto_json_map.nullable_mem "relation" Proto_json_map.string_to_bool
         ~enc:(fun t -> t.relation)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end

module Alert = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    trigger : Trigger.t;
    action : string option;
    acknowledged : Ptime.t option;
    related_to : (string * Relation.t) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ trigger action acknowledged related_to unknown =
      { meta; type_; trigger; action; acknowledged; related_to; unknown }
    in
    Jsont.Object.map' ~kind:"Alert" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("Alert", ()) ])
         ~enc:(fun t -> t.type_)
    |> Jsont.Object.mem "trigger" Trigger.jsont ~enc:(fun t -> t.trigger)
    |> Proto_json_map.nullable_mem "action" Jsont.string ~enc:(fun t ->
        t.action)
    |> Proto_json_map.nullable_mem "acknowledged" Proto_date.utc_jsont
         ~enc:(fun t -> t.acknowledged)
    |> Proto_json_map.nullable_mem "relatedTo"
         (Proto_json_map.of_string Relation.jsont) ~enc:(fun t -> t.related_to)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end

type parameter = One of string | Many of string list

let parameter_jsont =
  let one =
    Jsont.map
      ~dec:(fun s -> One s)
      ~enc:(function One s -> s | Many _ -> assert false)
      Jsont.string
  in
  let many =
    Jsont.map
      ~dec:(fun s -> Many s)
      ~enc:(function Many s -> s | One _ -> assert false)
      (Jsont.list Jsont.string)
  in
  Jsont.any ~dec_string:one ~dec_array:many
    ~enc:(function One _ -> one | Many _ -> many)
    ()

module Ical_property = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string;
    value_type : string option;
    parameters : (string * parameter) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ name value_type parameters unknown =
      { meta; type_; name; value_type; parameters; unknown }
    in
    Jsont.Object.map' ~kind:"ICalProperty" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("ICalProperty", ()) ])
         ~enc:(fun t -> t.type_)
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Proto_json_map.nullable_mem "valueType" Jsont.string ~enc:(fun t ->
        t.value_type)
    |> Proto_json_map.nullable_mem "parameters"
         (Proto_json_map.of_string parameter_jsont) ~enc:(fun t -> t.parameters)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end

module Icalendar = struct
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string;
    components : Jsont.json list list option;
    properties : Jsont.json list list option;
    converted_properties : (string * Ical_property.t) list option;
    unknown : Proto_unknown.t;
  }

  let jsont =
    let make meta type_ name components properties converted_properties unknown
        =
      {
        meta;
        type_;
        name;
        components;
        properties;
        converted_properties;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"ICalComponent" ~enc_meta:(fun t -> t.meta) make
    |> Proto_json_map.nullable_mem "@type"
         (Jsont.enum [ ("ICalComponent", ()) ])
         ~enc:(fun t -> t.type_)
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Proto_json_map.nullable_mem "components"
         (Jsont.list (Jsont.list Jsont.json))
         ~enc:(fun t -> t.components)
    |> Proto_json_map.nullable_mem "properties"
         (Jsont.list (Jsont.list Jsont.json))
         ~enc:(fun t -> t.properties)
    |> Proto_json_map.nullable_mem "convertedProperties"
         (Proto_json_map.of_string Ical_property.jsont) ~enc:(fun t ->
           t.converted_properties)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish
end
