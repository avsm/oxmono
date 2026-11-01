(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Property = Httpz_dav.Prop

type t = {
  href : string;
  display_name : string option;
  description : string option;
  etag : string option;
  sync_token : string option;
  reports : Httpz_dav.name list;
  data_types : (string * string) list;
  max_size : int option;
  collations : string list;
  privileges : Httpz_dav.name list;
}

let non_empty = function "" -> None | s -> Some s

let of_response (r : Httpz_dav.response) =
  match Httpz_dav.find_property Property.resourcetype r with
  | Some rt when Carddav_property.is_addressbook rt ->
      let prop name = Httpz_dav.find_property name r in
      let display_name =
        Option.bind (prop Property.displayname) (fun p ->
            non_empty (Httpz_dav.content p))
      in
      let description =
        Option.map Httpz_dav.content
          (prop Carddav_property.addressbook_description)
      in
      let sync_token =
        Option.map Httpz_dav.content (prop Property.sync_token)
      in
      let reports =
        match prop Property.supported_report_set with
        | Some p -> Property.reports p
        | None -> []
      in
      let data_types =
        match prop Carddav_property.supported_address_data with
        | Some p -> Carddav_property.address_data_types p
        | None -> []
      in
      let max_size =
        match prop Carddav_property.max_resource_size with
        | Some p -> Carddav_property.max_size p
        | None -> None
      in
      let collations =
        match prop Carddav_property.supported_collation_set with
        | Some p -> Carddav_property.collations p
        | None -> []
      in
      let privileges =
        match prop Property.current_user_privilege_set with
        | None -> []
        | Some p -> Property.privileges p
      in
      Some
        {
          href = Httpz_dav.href r;
          display_name;
          description;
          etag = Httpz_dav.etag r;
          sync_token;
          reports;
          data_types;
          max_size;
          collations;
          privileges;
        }
  | Some _ | None -> None

let of_multistatus (m : Httpz_dav.multistatus) =
  List.filter_map of_response m.responses

let supports report t = List.mem report t.reports

let accepts ~version t =
  match t.data_types with
  | [] -> version = "3.0"
  | dts ->
      (* RFC 2045 makes a media type case insensitive. *)
      List.exists
        (fun (ct, v) ->
          String.equal (String.lowercase_ascii ct) "text/vcard"
          && String.equal v version)
        dts

let mkcol ?display_name ?description () =
  Httpz_dav.el Property.resourcetype
    [
      Httpz_dav.empty (Httpz_dav.dav "collection");
      Httpz_dav.empty Carddav_property.addressbook;
    ]
  :: Option.to_list
       (Option.map (Httpz_dav.leaf Property.displayname) display_name)
  @ Option.to_list (Option.map Carddav_property.description description)

let propfind = Httpz_dav.Prop Carddav_property.collection_props

let pp ppf t =
  match t.display_name with
  | Some n -> Format.fprintf ppf "%s (%s)" t.href n
  | None -> Format.pp_print_string ppf t.href
