(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  family : string list;
  given : string list;
  additional : string list;
  prefixes : string list;
  suffixes : string list;
  surname2 : string list;
  generation : string list;
}

let empty =
  {
    family = [];
    given = [];
    additional = [];
    prefixes = [];
    suffixes = [];
    surname2 = [];
    generation = [];
  }

let v ?(family = []) ?(given = []) ?(additional = []) ?(prefixes = [])
    ?(suffixes = []) ?(surname2 = []) ?(generation = []) () =
  { family; given; additional; prefixes; suffixes; surname2; generation }

let nth = Vcard_text.component

let of_components cs =
  {
    family = nth cs 0;
    given = nth cs 1;
    additional = nth cs 2;
    prefixes = nth cs 3;
    suffixes = nth cs 4;
    surname2 = nth cs 5;
    generation = nth cs 6;
  }

let to_components n =
  [
    n.family;
    n.given;
    n.additional;
    n.prefixes;
    n.suffixes;
    n.surname2;
    n.generation;
  ]

let of_value s = of_components (Vcard_text.structured_of_string s)
let to_value n = Vcard_text.structured_to_string (to_components n)

let of_property p =
  if String.equal (Vcard_property.name p) "N" then
    Ok (of_value (Vcard_property.value p))
  else Error (Printf.sprintf "%s is not an N property" (Vcard_property.name p))

let to_property ?group ?params n =
  Vcard_property.v ?group ?params "N" (to_value n)

let equal a b =
  let l = List.equal String.equal in
  l a.family b.family && l a.given b.given
  && l a.additional b.additional
  && l a.prefixes b.prefixes && l a.suffixes b.suffixes
  && l a.surname2 b.surname2
  && l a.generation b.generation

let pp ppf n = Format.pp_print_string ppf (to_value n)
