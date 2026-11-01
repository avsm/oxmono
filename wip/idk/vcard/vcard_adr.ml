(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  po_box : string list;
  extended : string list;
  street : string list;
  locality : string list;
  region : string list;
  postal_code : string list;
  country : string list;
  room : string list;
  apartment : string list;
  floor : string list;
  street_number : string list;
  street_name : string list;
  building : string list;
  block : string list;
  subdistrict : string list;
  district : string list;
  landmark : string list;
  direction : string list;
}

let empty =
  {
    po_box = [];
    extended = [];
    street = [];
    locality = [];
    region = [];
    postal_code = [];
    country = [];
    room = [];
    apartment = [];
    floor = [];
    street_number = [];
    street_name = [];
    building = [];
    block = [];
    subdistrict = [];
    district = [];
    landmark = [];
    direction = [];
  }

let v ?(po_box = []) ?(extended = []) ?(street = []) ?(locality = [])
    ?(region = []) ?(postal_code = []) ?(country = []) ?(room = [])
    ?(apartment = []) ?(floor = []) ?(street_number = []) ?(street_name = [])
    ?(building = []) ?(block = []) ?(subdistrict = []) ?(district = [])
    ?(landmark = []) ?(direction = []) () =
  {
    po_box;
    extended;
    street;
    locality;
    region;
    postal_code;
    country;
    room;
    apartment;
    floor;
    street_number;
    street_name;
    building;
    block;
    subdistrict;
    district;
    landmark;
    direction;
  }

let nth = Vcard_text.component

let of_components cs =
  {
    po_box = nth cs 0;
    extended = nth cs 1;
    street = nth cs 2;
    locality = nth cs 3;
    region = nth cs 4;
    postal_code = nth cs 5;
    country = nth cs 6;
    room = nth cs 7;
    apartment = nth cs 8;
    floor = nth cs 9;
    street_number = nth cs 10;
    street_name = nth cs 11;
    building = nth cs 12;
    block = nth cs 13;
    subdistrict = nth cs 14;
    district = nth cs 15;
    landmark = nth cs 16;
    direction = nth cs 17;
  }

let extended_components a =
  [
    a.room;
    a.apartment;
    a.floor;
    a.street_number;
    a.street_name;
    a.building;
    a.block;
    a.subdistrict;
    a.district;
    a.landmark;
    a.direction;
  ]

let to_components a =
  [
    a.po_box;
    a.extended;
    a.street;
    a.locality;
    a.region;
    a.postal_code;
    a.country;
  ]
  @ extended_components a

let has_extended a = List.exists (fun c -> c <> []) (extended_components a)
let of_value s = of_components (Vcard_text.structured_of_string s)
let to_value a = Vcard_text.structured_to_string (to_components a)

let of_property p =
  if String.equal (Vcard_property.name p) "ADR" then
    Ok (of_value (Vcard_property.value p))
  else
    Error (Printf.sprintf "%s is not an ADR property" (Vcard_property.name p))

let to_property ?group ?params a =
  Vcard_property.v ?group ?params "ADR" (to_value a)

let equal a b =
  List.equal (List.equal String.equal) (to_components a) (to_components b)

let pp ppf a = Format.pp_print_string ppf (to_value a)
