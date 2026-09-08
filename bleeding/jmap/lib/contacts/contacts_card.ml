(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  id : Proto_id.t option;
  address_book_ids : (Proto_id.t * bool) list option;
  card : Jscontact.Card.t;
}

let make ?id ?address_book_ids card = { id; address_book_ids; card }
let creation s : t Proto_id.creation = Proto_id.creation s
let kind = "ContactCard"

(* RFC 9610 Section 3 adds "id" and "addressBookIds" to a JSContact Card,
   which RFC 9553 Section 1.7.4 has the Card codec keep among the members it
   does not define. Decoding lifts the two out of there and encoding puts
   them back, so neither is ever seen twice. *)

let lift unknown name codec =
  match Jscontact.Unknown.find unknown name with
  | None -> None
  | Some json -> (
      match Jsont.Json.decode codec json with
      | Ok v -> Some v
      | Error e ->
          Jsont.Error.msgf (Jsont.Json.meta json) "%s: %s: %s" kind name e)

let lower unknown name codec = function
  | None -> unknown
  | Some v -> (
      match Jsont.Json.encode codec v with
      | Ok json -> Jscontact.Unknown.add unknown name json
      | Error e -> Jsont.Error.msgf Jsont.Meta.none "%s: %s: %s" kind name e)

let jsont =
  let dec (card : Jscontact.Card.t) =
    let unknown = card.unknown in
    let id = lift unknown "id" Proto_id.jsont in
    let address_book_ids =
      lift unknown "addressBookIds" Proto_json_map.id_to_bool
    in
    let unknown =
      Jscontact.Unknown.remove
        (Jscontact.Unknown.remove unknown "id")
        "addressBookIds"
    in
    { id; address_book_ids; card = { card with unknown } }
  in
  let enc t =
    let unknown = lower t.card.unknown "id" Proto_id.jsont t.id in
    let unknown =
      lower unknown "addressBookIds" Proto_json_map.id_to_bool
        t.address_book_ids
    in
    { t.card with unknown }
  in
  Jsont.map ~kind ~dec ~enc Jscontact.Card.partial_jsont

module Filter_condition = struct
  type t = {
    in_address_book : Proto_id.t option;
    uid : string option;
    has_member : string option;
    kind : string option;
    created_before : Proto_date.t option;
    created_after : Proto_date.t option;
    updated_before : Proto_date.t option;
    updated_after : Proto_date.t option;
    text : string option;
    name : string option;
    name_given : string option;
    name_surname : string option;
    name_surname2 : string option;
    nickname : string option;
    organization : string option;
    email : string option;
    phone : string option;
    online_service : string option;
    address : string option;
    note : string option;
  }

  let empty =
    {
      in_address_book = None;
      uid = None;
      has_member = None;
      kind = None;
      created_before = None;
      created_after = None;
      updated_before = None;
      updated_after = None;
      text = None;
      name = None;
      name_given = None;
      name_surname = None;
      name_surname2 = None;
      nickname = None;
      organization = None;
      email = None;
      phone = None;
      online_service = None;
      address = None;
      note = None;
    }

  let jsont =
    let kind = "ContactCardFilterCondition" in
    let make in_address_book uid has_member kind created_before created_after
        updated_before updated_after text name name_given name_surname
        name_surname2 nickname organization email phone online_service address
        note =
      {
        in_address_book;
        uid;
        has_member;
        kind;
        created_before;
        created_after;
        updated_before;
        updated_after;
        text;
        name;
        name_given;
        name_surname;
        name_surname2;
        nickname;
        organization;
        email;
        phone;
        online_service;
        address;
        note;
      }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.opt_mem "inAddressBook" Proto_id.jsont ~enc:(fun f ->
        f.in_address_book)
    |> Jsont.Object.opt_mem "uid" Jsont.string ~enc:(fun f -> f.uid)
    |> Jsont.Object.opt_mem "hasMember" Jsont.string ~enc:(fun f ->
        f.has_member)
    |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun f -> f.kind)
    |> Jsont.Object.opt_mem "createdBefore" Proto_date.utc_jsont ~enc:(fun f ->
        f.created_before)
    |> Jsont.Object.opt_mem "createdAfter" Proto_date.utc_jsont ~enc:(fun f ->
        f.created_after)
    |> Jsont.Object.opt_mem "updatedBefore" Proto_date.utc_jsont ~enc:(fun f ->
        f.updated_before)
    |> Jsont.Object.opt_mem "updatedAfter" Proto_date.utc_jsont ~enc:(fun f ->
        f.updated_after)
    |> Jsont.Object.opt_mem "text" Jsont.string ~enc:(fun f -> f.text)
    |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun f -> f.name)
    (* The solidus in these three wire names is part of the member name, not
       a nesting: RFC 9610 Section 3.3.1 spells them so. *)
    |> Jsont.Object.opt_mem "name/given" Jsont.string ~enc:(fun f ->
        f.name_given)
    |> Jsont.Object.opt_mem "name/surname" Jsont.string ~enc:(fun f ->
        f.name_surname)
    |> Jsont.Object.opt_mem "name/surname2" Jsont.string ~enc:(fun f ->
        f.name_surname2)
    |> Jsont.Object.opt_mem "nickname" Jsont.string ~enc:(fun f -> f.nickname)
    |> Jsont.Object.opt_mem "organization" Jsont.string ~enc:(fun f ->
        f.organization)
    |> Jsont.Object.opt_mem "email" Jsont.string ~enc:(fun f -> f.email)
    |> Jsont.Object.opt_mem "phone" Jsont.string ~enc:(fun f -> f.phone)
    |> Jsont.Object.opt_mem "onlineService" Jsont.string ~enc:(fun f ->
        f.online_service)
    |> Jsont.Object.opt_mem "address" Jsont.string ~enc:(fun f -> f.address)
    |> Jsont.Object.opt_mem "note" Jsont.string ~enc:(fun f -> f.note)
    |> Jsont.Object.finish
end

type filter = Filter_condition.t Proto_filter.filter

let filter_jsont = Proto_filter.filter_jsont Filter_condition.jsont

let filter ?in_address_book ?uid ?has_member ?kind ?created_before
    ?created_after ?updated_before ?updated_after ?text ?name ?name_given
    ?name_surname ?name_surname2 ?nickname ?organization ?email ?phone
    ?online_service ?address ?note () =
  Proto_filter.Condition
    {
      Filter_condition.in_address_book;
      uid;
      has_member;
      kind;
      created_before;
      created_after;
      updated_before;
      updated_after;
      text;
      name;
      name_given;
      name_surname;
      name_surname2;
      nickname;
      organization;
      email;
      phone;
      online_service;
      address;
      note;
    }

module Sort = struct
  type t = [ `Created | `Updated | `Name_given | `Name_surname | `Name_surname2 ]

  let to_string = function
    | `Created -> "created"
    | `Updated -> "updated"
    | `Name_given -> "name/given"
    | `Name_surname -> "name/surname"
    | `Name_surname2 -> "name/surname2"

  let of_string = function
    | "created" -> Some `Created
    | "updated" -> Some `Updated
    | "name/given" -> Some `Name_given
    | "name/surname" -> Some `Name_surname
    | "name/surname2" -> Some `Name_surname2
    | _ -> None

  let jsont =
    let dec s =
      match of_string s with
      | Some p -> p
      | None ->
          Jsont.Error.msgf Jsont.Meta.none
            "Unknown ContactCard sort property: %s" s
    in
    Jsont.map ~kind:"ContactCardSortProperty" ~dec ~enc:to_string Jsont.string
end
