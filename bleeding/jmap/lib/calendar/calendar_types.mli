@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Common JSCalendar object codecs. Local dates, time zones and durations
    retain their wire strings. These codecs validate member shapes without
    expanding recurrences or interpreting time zones. *)

type frequency =
  [ `Yearly | `Monthly | `Weekly | `Daily | `Hourly | `Minutely | `Secondly ]

val frequency_jsont : frequency Jsont.t

type weekday = [ `Mo | `Tu | `We | `Th | `Fr | `Sa | `Su ]

val weekday_jsont : weekday Jsont.t

module Nday : sig
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    day : weekday;
    nth_of_period : int64 option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes NDay properties and retains unrecognised members. *)
end

module Recurrence_rule : sig
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

  val jsont : t Jsont.t
  (** [jsont] codes RecurrenceRule properties and retains unrecognised members.
  *)
end

module Link : sig
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

  val jsont : t Jsont.t
  (** [jsont] codes Link properties and retains unrecognised members. *)

  val empty : t
  (** [empty] has no properties set. *)
end

module Location : sig
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

  val jsont : t Jsont.t
  (** [jsont] codes Location properties and retains unrecognised members. *)

  val empty : t
  (** [empty] has no properties set. *)
end

module Virtual_location : sig
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

  val jsont : t Jsont.t
  (** [jsont] codes VirtualLocation properties and retains unrecognised members.
  *)

  val empty : t
  (** [empty] has no properties set. *)
end

module Participant : sig
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

  val jsont : t Jsont.t
  (** [jsont] codes Participant properties and retains unrecognised members. *)

  val empty : t
  (** [empty] has no properties set. *)
end

module Trigger : sig
  type t = {
    meta : Jsont.Meta.t;
    type_ : string;
    offset : string option;
    relative_to : string option;
    when_ : Ptime.t option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes Alert trigger properties and retains unrecognised members.
  *)
end

module Relation : sig
  type t = {
    meta : Jsont.Meta.t;
    relation : (string * bool) list option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
end

module Alert : sig
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    trigger : Trigger.t;
    action : string option;
    acknowledged : Ptime.t option;
    related_to : (string * Relation.t) list option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes Alert properties and retains unrecognised members. *)
end

type parameter = One of string | Many of string list

val parameter_jsont : parameter Jsont.t

module Ical_property : sig
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string;
    value_type : string option;
    parameters : (string * parameter) list option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes ICalProperty properties and retains unrecognised members. *)
end

module Icalendar : sig
  type t = {
    meta : Jsont.Meta.t;
    type_ : unit option;
    name : string;
    components : Jsont.json list list option;
    properties : Jsont.json list list option;
        (** jCal arrays for properties and components without a JSCalendar
            mapping. Their heterogeneous extension values remain JSON. *)
    converted_properties : (string * Ical_property.t) list option;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes ICalComponent properties and retains unrecognised members.
  *)
end
