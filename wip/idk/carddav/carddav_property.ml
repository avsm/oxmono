(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Property = Httpz_dav.Prop

type name = Httpz_dav.name

let carddav local = Httpz_dav.carddav local
let addressbook = carddav "addressbook"
let addressbook_query = carddav "addressbook-query"
let addressbook_multiget = carddav "addressbook-multiget"
let addressbook_home_set = carddav "addressbook-home-set"
let principal_address = carddav "principal-address"
let addressbook_description = carddav "addressbook-description"
let supported_address_data = carddav "supported-address-data"
let max_resource_size = carddav "max-resource-size"
let supported_collation_set = carddav "supported-collation-set"
let address_data_type = carddav "address-data-type"
let supported_collation = carddav "supported-collation"
let attr_content_type = ("", "content-type")
let attr_version = ("", "version")
let xml_lang = ("http://www.w3.org/XML/1998/namespace", "lang")

let is_addressbook p =
  Httpz_dav.is Property.resourcetype p
  && List.mem addressbook (Property.resource_types p)

let address_data_types p =
  if not (Httpz_dav.is supported_address_data p) then []
  else
    List.map
      (fun e ->
        ( Option.value ~default:"text/vcard" (Httpz_dav.attr attr_content_type e),
          Option.value ~default:"3.0" (Httpz_dav.attr attr_version e) ))
      (Httpz_dav.children address_data_type p)

let collations p =
  if not (Httpz_dav.is supported_collation_set p) then []
  else
    List.map
      (fun e -> String.trim (Httpz_dav.content e))
      (Httpz_dav.children supported_collation p)

let max_size p =
  if Httpz_dav.is max_resource_size p then
    int_of_string_opt (String.trim (Httpz_dav.content p))
  else None

let description ?lang s =
  let attrs = match lang with None -> [] | Some l -> [ (xml_lang, l) ] in
  Httpz_dav.element ~attrs addressbook_description [ Httpz_dav.Text s ]

let collection_props =
  [
    Property.resourcetype;
    Property.displayname;
    Property.getetag;
    Property.sync_token;
    Property.supported_report_set;
    Property.current_user_privilege_set;
    addressbook_description;
    supported_address_data;
    max_resource_size;
    supported_collation_set;
  ]

let principal_props =
  [
    addressbook_home_set;
    principal_address;
    Property.displayname;
    Property.principal_url;
  ]
