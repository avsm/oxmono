(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module S = Fetch_dav.Session
module O = Fetch_dav.Objects

module Href = struct
  let resolve ~base href =
    match Httpz_dav.resolve_href ~base href with Ok r -> r | Error _ -> href

  let path = Httpz_dav.href_path
end

module Quirks = Carddav_eio_quirks

type t = { session : S.t; quirks : Quirks.t }

type error = S.error =
  | Http of int * string
  | Dav of int * Httpz_dav.element list
  | Precondition_failed of string
  | Not_found of string
  | Xml of string
  | Data of string
  | Discovery of string
  | Transport of Fetch.error * string

let pp_error ppf e = S.pp_error ~describe:Carddav.Error.describe ppf e
let error_to_string e = Format.asprintf "%a" pp_error e
let ( let* ) = Result.bind

let connect ~sw ?credentials ?allow_insecure ?limits ?quirks fetch url =
  let quirks = match quirks with Some q -> q | None -> Quirks.of_url url in
  let* session =
    S.connect ~sw ?credentials ?allow_insecure ?limits
      ~lenient_hrefs:quirks.lenient_hrefs ~service:`Carddav
      ~home_set:Carddav.Property.addressbook_home_set fetch url
  in
  Ok { session; quirks }

let principal t = S.principal t.session
let home_sets t = S.home_sets t.session
let quirks t = t.quirks
let session t = t.session
let dav t = S.client t.session
let download t url = S.download t.session url
let propfind t ?depth url query = S.propfind t.session ?depth url query
let report t ?depth url body = S.report t.session ?depth url body

let addressbook t url =
  let* m = propfind t ~depth:`Zero url Carddav.Addressbook.propfind in
  match Carddav.Addressbook.of_multistatus m with
  | a :: _ -> Ok a
  | [] -> Error (Discovery (url ^ " is not an address book"))

let addressbooks t =
  let rec go acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | home :: rest ->
        let* m = propfind t ~depth:`One home Carddav.Addressbook.propfind in
        let books =
          List.map
            (fun (a : Carddav.Addressbook.t) ->
              { a with href = Href.resolve ~base:home a.href })
            (Carddav.Addressbook.of_multistatus m)
        in
        go (books :: acc) rest
  in
  go [] (home_sets t)

let create_addressbook t ?display_name ?description url =
  S.mkcol t.session
    ~props:(Carddav.Addressbook.mkcol ?display_name ?description ())
    url

let delete_addressbook t url = S.delete t.session url
let set_props t url updates = S.proppatch t.session url updates

type member = S.member = {
  href : string;
  etag : string option;
  content_type : string option;
}

type 'a entry = 'a O.entry = { href : string; etag : string option; value : 'a }
type 'a page = 'a O.page = { entries : 'a entry list; truncated : bool }

let list t url = S.members t.session url

(* A server that serves vCard 3.0 is read through the upgrade of the profile,
   so that a card round trips whatever version it came back as. *)
let objects t (codec : _ Carddav.Data.t) =
  {
    O.content_type = Printf.sprintf "%s; charset=utf-8" codec.content_type;
    decode =
      (fun text ->
        codec.decode
          (if t.quirks.vcard3 then Quirks.upgrade_vcard3 text else text));
    encode = codec.encode;
  }

let address_data (r : Httpz_dav.response) =
  Option.bind
    (Httpz_dav.find_property Carddav.Address_data.name r)
    Carddav.Address_data.data

let get codec t url = O.get t.session (objects t codec) url

let put codec t ?etag ?create url v =
  O.put t.session (objects t codec) ?etag ?create url v

let add codec t ?name addressbook v =
  O.add t.session (objects t codec) ?name ~uid:(Carddav.Data.uid codec)
    ~ext:".vcf" addressbook v

let delete t ?etag url = S.delete t.session ?etag url

let page_of_multistatus t codec ~base m =
  O.page_of_multistatus (objects t codec) ~data:address_data ~base m

let query codec t ?limit url filter =
  let q =
    Carddav.Report.query ~data:(Carddav.Data.address_data codec) ?limit filter
  in
  let* m = report t ~depth:`One url (Carddav.Report.query_to_xml q) in
  page_of_multistatus t codec ~base:url m

let multiget codec t url hrefs =
  if hrefs = [] then Ok []
  else
    let hrefs =
      List.map (fun h -> Href.path (Href.resolve ~base:url h)) hrefs
    in
    let mg =
      Carddav.Report.multiget ~data:(Carddav.Data.address_data codec) hrefs
    in
    let* m = report t ~depth:`Zero url (Carddav.Report.multiget_to_xml mg) in
    let* p = page_of_multistatus t codec ~base:url m in
    Ok p.entries

type 'a change = 'a O.change = Changed of 'a entry | Removed of string

type 'a sync = 'a O.sync = {
  token : string option;
  changes : 'a change list;
  truncated : bool;
}

let sync codec t ?token ?limit url =
  O.sync t.session ~multiget:(multiget codec t) ?token ?limit url

let sync_token t url = S.sync_token t.session url
