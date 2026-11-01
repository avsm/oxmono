(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type property =
  [ `Id
  | `Name
  | `Description
  | `Sort_order
  | `Is_default
  | `Is_subscribed
  | `Share_with
  | `My_rights ]

let property_to_string : [< property ] -> string = function
  | `Id -> "id"
  | `Name -> "name"
  | `Description -> "description"
  | `Sort_order -> "sortOrder"
  | `Is_default -> "isDefault"
  | `Is_subscribed -> "isSubscribed"
  | `Share_with -> "shareWith"
  | `My_rights -> "myRights"

let property_of_string s : property option =
  match s with
  | "id" -> Some `Id
  | "name" -> Some `Name
  | "description" -> Some `Description
  | "sortOrder" -> Some `Sort_order
  | "isDefault" -> Some `Is_default
  | "isSubscribed" -> Some `Is_subscribed
  | "shareWith" -> Some `Share_with
  | "myRights" -> Some `My_rights
  | _ -> None

module Rights = struct
  type t = {
    may_read : bool;
    may_write : bool;
    may_share : bool;
    may_delete : bool;
  }

  let jsont =
    let kind = "AddressBookRights" in
    let make may_read may_write may_share may_delete =
      { may_read; may_write; may_share; may_delete }
    in
    Jsont.Object.map ~kind make
    |> Jsont.Object.mem "mayRead" Jsont.bool ~enc:(fun t -> t.may_read)
    |> Jsont.Object.mem "mayWrite" Jsont.bool ~enc:(fun t -> t.may_write)
    |> Jsont.Object.mem "mayShare" Jsont.bool ~enc:(fun t -> t.may_share)
    |> Jsont.Object.mem "mayDelete" Jsont.bool ~enc:(fun t -> t.may_delete)
    |> Jsont.Object.finish
end

type t = {
  id : Proto_id.t option;
  name : string option;
  description : string option;
  sort_order : int64 option;
  is_default : bool option;
  is_subscribed : bool option;
  share_with : (Proto_id.t * Rights.t) list option;
  my_rights : Rights.t option;
}

let empty =
  {
    id = None;
    name = None;
    description = None;
    sort_order = None;
    is_default = None;
    is_subscribed = None;
    share_with = None;
    my_rights = None;
  }

(* RFC 9610 Section 2: [name] "MUST NOT be the empty string and MUST NOT be
   greater than 255 octets in size when encoded as UTF-8", and [sortOrder]
   "MUST be an integer in the range 0 <= sortOrder < 2^31".  Unlike the
   Mailbox name of RFC 8621 Section 2, whose maximum length is server
   policy, the cap here is in the specification and so is checked. *)
let max_name_octets = 255
let max_sort_order = 0x8000_0000L

let create ~name ?description ?sort_order ?is_subscribed ?share_with () =
  if String.equal name "" then Error "an AddressBook name must not be empty"
  else if String.length name > max_name_octets then
    Error
      (Printf.sprintf
         "an AddressBook name must not be greater than %d octets when encoded \
          as UTF-8, but is %d"
         max_name_octets (String.length name))
  else if Result.is_error (Proto_json.check_value (Jsont.Json.string name)) then
    Error "an AddressBook name must be a valid UTF-8 string"
  else
    match sort_order with
    | Some o when o < 0L || o >= max_sort_order ->
        Error
          (Printf.sprintf
             "sortOrder must be in the range 0 <= sortOrder < 2^31, but is %Ld"
             o)
    | _ ->
        Ok
          {
            empty with
            name = Some name;
            description;
            sort_order;
            is_subscribed;
            share_with;
          }

let create_exn ~name ?description ?sort_order ?is_subscribed ?share_with () =
  match create ~name ?description ?sort_order ?is_subscribed ?share_with () with
  | Ok a -> a
  | Error e -> invalid_arg ("Contacts_addressbook.create_exn: " ^ e)

let jsont =
  let kind = "AddressBook" in
  let make id name description sort_order is_default is_subscribed share_with
      my_rights =
    {
      id;
      name;
      description;
      sort_order;
      is_default;
      is_subscribed;
      share_with;
      my_rights;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.opt_mem "id" Proto_id.jsont ~enc:(fun t -> t.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun t -> t.name)
  (* RFC 9610 Section 2 types description String|null and shareWith
     Id[AddressBookRights]|null, null meaning no description and shared with
     nobody, so an omitted member would lose a fact the server stated. *)
  |> Proto_json_map.nullable_mem_null "description" Jsont.string ~enc:(fun t ->
      t.description)
  |> Jsont.Object.opt_mem "sortOrder" Proto_int53.Unsigned.jsont ~enc:(fun t ->
      t.sort_order)
  |> Jsont.Object.opt_mem "isDefault" Jsont.bool ~enc:(fun t -> t.is_default)
  |> Jsont.Object.opt_mem "isSubscribed" Jsont.bool ~enc:(fun t ->
      t.is_subscribed)
  |> Proto_json_map.nullable_mem_null "shareWith"
       (Proto_json_map.of_id Rights.jsont) ~enc:(fun t -> t.share_with)
  |> Jsont.Object.opt_mem "myRights" Rights.jsont ~enc:(fun t -> t.my_rights)
  |> Jsont.Object.finish

let creation = Proto_id.creation
