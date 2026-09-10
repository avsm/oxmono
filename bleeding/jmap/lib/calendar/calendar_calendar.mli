@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Typed Calendar properties. [None] represents a property omitted by a
    projection or returned as null. Unrecognised fields are retained in
    [unknown], including fields of nested extensible objects. [meta] locates the
    original object for {!Proto_response.source_fragment}. *)

module Rights : sig
  type t = {
    meta : Jsont.Meta.t;
    may_read_free_busy : bool;
    may_read_items : bool;
    may_write_all : bool;
    may_write_own : bool;
    may_update_private : bool;
    may_rsvp : bool;
    may_share : bool;
    may_delete : bool;
    unknown : Proto_unknown.t;
  }

  val jsont : t Jsont.t
  (** [jsont] codes CalendarRights properties and retains unrecognised members.
  *)

  val empty : t
  (** [empty] grants no rights. *)
end

type availability = [ `All | `Attending | `None ]

val availability_jsont : availability Jsont.t

type t = {
  meta : Jsont.Meta.t;
  id : Proto_id.t option;
  name : string option;
  description : string option;
  color : string option;
  sort_order : int64 option;
  is_subscribed : bool option;
  is_visible : bool option;
  is_default : bool option;
  include_in_availability : availability option;
  default_alerts_with_time : (Proto_id.t * Calendar_types.Alert.t) list option;
  default_alerts_without_time :
    (Proto_id.t * Calendar_types.Alert.t) list option;
  time_zone : string option;
  share_with : (Proto_id.t * Rights.t) list option;
  my_rights : Rights.t option;
  unknown : Proto_unknown.t;
}

val jsont : t Jsont.t
(** [jsont] codes Calendar properties and retains unrecognised members. *)

val empty : t
(** [empty] has no properties set. *)

type property =
  [ `Id
  | `Name
  | `Description
  | `Color
  | `Sort_order
  | `Is_subscribed
  | `Is_visible
  | `Is_default
  | `Include_in_availability
  | `Default_alerts_with_time
  | `Default_alerts_without_time
  | `Time_zone
  | `Share_with
  | `My_rights ]

val property_to_string : [< property ] -> string
