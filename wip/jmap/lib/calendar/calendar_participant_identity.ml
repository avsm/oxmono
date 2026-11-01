(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  meta : Jsont.Meta.t;
  id : Proto_id.t option;
  name : string option;
  calendar_address : string option;
  is_default : bool option;
  unknown : Proto_unknown.t;
}

let jsont =
  let make meta id name calendar_address is_default unknown =
    { meta; id; name; calendar_address; is_default; unknown }
  in
  Jsont.Object.map' ~kind:"ParticipantIdentity" ~enc_meta:(fun t -> t.meta) make
  |> Proto_json_map.nullable_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Proto_json_map.nullable_mem "name" Jsont.string ~enc:(fun t -> t.name)
  |> Proto_json_map.nullable_mem "calendarAddress" Jsont.string ~enc:(fun t ->
      t.calendar_address)
  |> Proto_json_map.nullable_mem "isDefault" Jsont.bool ~enc:(fun t ->
      t.is_default)
  |> Jsont.Object.keep_unknown Proto_unknown.mems ~enc:(fun t -> t.unknown)
  |> Jsont.Object.finish

let empty =
  {
    meta = Jsont.Meta.none;
    id = None;
    name = None;
    calendar_address = None;
    is_default = None;
    unknown = Proto_unknown.empty;
  }

type property = [ `Id | `Name | `Calendar_address | `Is_default ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Name -> "name"
  | `Calendar_address -> "calendarAddress"
  | `Is_default -> "isDefault"
