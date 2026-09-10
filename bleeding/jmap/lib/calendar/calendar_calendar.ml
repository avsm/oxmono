(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Rights = struct
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

  let jsont =
    let make meta may_read_free_busy may_read_items may_write_all may_write_own
        may_update_private may_rsvp may_share may_delete unknown =
      {
        meta;
        may_read_free_busy;
        may_read_items;
        may_write_all;
        may_write_own;
        may_update_private;
        may_rsvp;
        may_share;
        may_delete;
        unknown;
      }
    in
    Jsont.Object.map' ~kind:"CalendarRights" ~enc_meta:(fun t -> t.meta) make
    |> Jsont.Object.mem "mayReadFreeBusy" Jsont.bool ~enc:(fun t ->
        t.may_read_free_busy)
    |> Jsont.Object.mem "mayReadItems" Jsont.bool ~enc:(fun t ->
        t.may_read_items)
    |> Jsont.Object.mem "mayWriteAll" Jsont.bool ~enc:(fun t -> t.may_write_all)
    |> Jsont.Object.mem "mayWriteOwn" Jsont.bool ~enc:(fun t -> t.may_write_own)
    |> Jsont.Object.mem "mayUpdatePrivate" Jsont.bool ~enc:(fun t ->
        t.may_update_private)
    |> Jsont.Object.mem "mayRSVP" Jsont.bool ~enc:(fun t -> t.may_rsvp)
    |> Jsont.Object.mem "mayShare" Jsont.bool ~enc:(fun t -> t.may_share)
    |> Jsont.Object.mem "mayDelete" Jsont.bool ~enc:(fun t -> t.may_delete)
    |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
    |> Jsont.Object.finish

  let empty =
    {
      meta = Jsont.Meta.none;
      may_read_free_busy = false;
      may_read_items = false;
      may_write_all = false;
      may_write_own = false;
      may_update_private = false;
      may_rsvp = false;
      may_share = false;
      may_delete = false;
      unknown = Proto_unknown.empty;
    }
end

type availability = [ `All | `Attending | `None ]

let availability_jsont : availability Jsont.t =
  Jsont.enum
    ([ ("all", `All); ("attending", `Attending); ("none", `None) ]
      : (string * availability) list)

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

let jsont =
  let make meta id name description color sort_order is_subscribed is_visible
      is_default include_in_availability default_alerts_with_time
      default_alerts_without_time time_zone share_with my_rights unknown =
    {
      meta;
      id;
      name;
      description;
      color;
      sort_order;
      is_subscribed;
      is_visible;
      is_default;
      include_in_availability;
      default_alerts_with_time;
      default_alerts_without_time;
      time_zone;
      share_with;
      my_rights;
      unknown;
    }
  in
  Jsont.Object.map' ~kind:"Calendar" ~enc_meta:(fun t -> t.meta) make
  |> Proto_json_map.nullable_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Proto_json_map.nullable_mem "description" Jsont.string ~enc:(fun t ->
      t.description)
  |> Proto_json_map.nullable_mem "color" Jsont.string ~enc:(fun t -> t.color)
  |> Proto_json_map.nullable_mem "sortOrder" Proto_int53.Unsigned.jsont
       ~enc:(fun t -> t.sort_order)
  |> Proto_json_map.nullable_mem "isSubscribed" Jsont.bool ~enc:(fun t ->
      t.is_subscribed)
  |> Proto_json_map.nullable_mem "isVisible" Jsont.bool ~enc:(fun t ->
      t.is_visible)
  |> Proto_json_map.nullable_mem "isDefault" Jsont.bool ~enc:(fun t ->
      t.is_default)
  |> Proto_json_map.nullable_mem "includeInAvailability" availability_jsont
       ~enc:(fun t -> t.include_in_availability)
  |> Proto_json_map.nullable_mem "defaultAlertsWithTime"
       (Proto_json_map.of_id Calendar_types.Alert.jsont) ~enc:(fun t ->
         t.default_alerts_with_time)
  |> Proto_json_map.nullable_mem "defaultAlertsWithoutTime"
       (Proto_json_map.of_id Calendar_types.Alert.jsont) ~enc:(fun t ->
         t.default_alerts_without_time)
  |> Proto_json_map.nullable_mem "timeZone" Jsont.string ~enc:(fun t ->
      t.time_zone)
  |> Proto_json_map.nullable_mem "shareWith" (Proto_json_map.of_id Rights.jsont)
       ~enc:(fun t -> t.share_with)
  |> Proto_json_map.nullable_mem "myRights" Rights.jsont ~enc:(fun t ->
      t.my_rights)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let empty =
  {
    meta = Jsont.Meta.none;
    id = None;
    name = None;
    description = None;
    color = None;
    sort_order = None;
    is_subscribed = None;
    is_visible = None;
    is_default = None;
    include_in_availability = None;
    default_alerts_with_time = None;
    default_alerts_without_time = None;
    time_zone = None;
    share_with = None;
    my_rights = None;
    unknown = Proto_unknown.empty;
  }

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

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Name -> "name"
  | `Description -> "description"
  | `Color -> "color"
  | `Sort_order -> "sortOrder"
  | `Is_subscribed -> "isSubscribed"
  | `Is_visible -> "isVisible"
  | `Is_default -> "isDefault"
  | `Include_in_availability -> "includeInAvailability"
  | `Default_alerts_with_time -> "defaultAlertsWithTime"
  | `Default_alerts_without_time -> "defaultAlertsWithoutTime"
  | `Time_zone -> "timeZone"
  | `Share_with -> "shareWith"
  | `My_rights -> "myRights"
