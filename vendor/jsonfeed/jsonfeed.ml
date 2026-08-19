(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Rfc3339 = Rfc3339
module Cito = Cito
module Author = Author
module Attachment = Attachment
module Hub = Hub
module Reference = Reference
module Item = Item

module Unknown = struct
  type t = Jsont.json

  let empty = Jsont.Object ([], Jsont.Meta.none)
  let is_empty = function Jsont.Object ([], _) -> true | _ -> false
end

type t = {
  version : string;
  title : string;
  home_page_url : string option;
  feed_url : string option;
  description : string option;
  user_comment : string option;
  next_url : string option;
  icon : string option;
  favicon : string option;
  authors : Author.t list option;
  language : string option;
  expired : bool option;
  hubs : Hub.t list option;
  items : Item.t list;
  unknown : Unknown.t;
}

let create ~title ?home_page_url ?feed_url ?description ?user_comment ?next_url
    ?icon ?favicon ?authors ?language ?expired ?hubs ~items
    ?(unknown = Unknown.empty) () =
  {
    version = "https://jsonfeed.org/version/1.1";
    title;
    home_page_url;
    feed_url;
    description;
    user_comment;
    next_url;
    icon;
    favicon;
    authors;
    language;
    expired;
    hubs;
    items;
    unknown;
  }

let version t = t.version
let title t = t.title
let home_page_url t = t.home_page_url
let feed_url t = t.feed_url
let description t = t.description
let user_comment t = t.user_comment
let next_url t = t.next_url
let icon t = t.icon
let favicon t = t.favicon
let authors t = t.authors
let language t = t.language
let expired t = t.expired
let hubs t = t.hubs
let items t = t.items
let unknown t = t.unknown
let equal a b = a.title = b.title && a.items = b.items

let pp ppf t =
  Format.fprintf ppf "Feed: %s (%d items)" t.title (List.length t.items)

let pp_summary ppf t =
  Format.fprintf ppf "%s (%d items)" t.title (List.length t.items)

(* Jsont type *)

let jsont =
  let kind = "JSON Feed" in
  let doc = "A JSON Feed document" in

  (* Helper constructor that sets version automatically *)
  let make_from_json _version title home_page_url feed_url description
      user_comment next_url icon favicon authors language expired hubs items
      unknown =
    {
      version = "https://jsonfeed.org/version/1.1";
      title;
      home_page_url;
      feed_url;
      description;
      user_comment;
      next_url;
      icon;
      favicon;
      authors;
      language;
      expired;
      hubs;
      items;
      unknown;
    }
  in

  Jsont.Object.map ~kind ~doc make_from_json
  |> Jsont.Object.mem "version" Jsont.string ~enc:version
  |> Jsont.Object.mem "title" Jsont.string ~enc:title
  |> Jsont.Object.opt_mem "home_page_url" Jsont.string ~enc:home_page_url
  |> Jsont.Object.opt_mem "feed_url" Jsont.string ~enc:feed_url
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:description
  |> Jsont.Object.opt_mem "user_comment" Jsont.string ~enc:user_comment
  |> Jsont.Object.opt_mem "next_url" Jsont.string ~enc:next_url
  |> Jsont.Object.opt_mem "icon" Jsont.string ~enc:icon
  |> Jsont.Object.opt_mem "favicon" Jsont.string ~enc:favicon
  |> Jsont.Object.opt_mem "authors" (Jsont.list Author.jsont) ~enc:authors
  |> Jsont.Object.opt_mem "language" Jsont.string ~enc:language
  |> Jsont.Object.opt_mem "expired" Jsont.bool ~enc:expired
  |> Jsont.Object.opt_mem "hubs" (Jsont.list Hub.jsont) ~enc:hubs
  |> Jsont.Object.mem "items" (Jsont.list Item.jsont) ~enc:items
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown
  |> Jsont.Object.finish

(* Encoding and Decoding *)

let decode ?layout ?locs ?file r =
  Jsont_bytesrw.decode' ?layout ?locs ?file jsont r

let decode_string ?layout ?locs ?file s =
  Jsont_bytesrw.decode_string' ?layout ?locs ?file jsont s

let encode ?format ?number_format feed ~eod w =
  Jsont_bytesrw.encode' ?format ?number_format jsont feed ~eod w

let encode_string ?format ?number_format feed =
  Jsont_bytesrw.encode_string' ?format ?number_format jsont feed

let of_string s = decode_string s

let to_string ?(minify = false) feed =
  let format = if minify then Jsont.Minify else Jsont.Indent in
  encode_string ~format feed

(* Validation *)

let validate feed =
  let errors = ref [] in
  let add_error msg = errors := msg :: !errors in

  (* Check required fields *)
  if feed.title = "" then add_error "title is required and cannot be empty";

  (* Check items have unique IDs *)
  let ids = List.map Item.id feed.items in
  let unique_ids = List.sort_uniq String.compare ids in
  if List.length ids <> List.length unique_ids then
    add_error "items must have unique IDs";

  (* Validate authors *)
  Option.iter
    (List.iteri (fun i author ->
         if not (Author.is_valid author) then
           add_error
             (Printf.sprintf
                "feed author %d is invalid (needs at least one field)" i)))
    feed.authors;

  (* Validate items *)
  List.iteri
    (fun i item ->
      if Item.id item = "" then
        add_error (Printf.sprintf "item %d has empty ID" i);

      (* Validate item authors *)
      Option.iter
        (List.iteri (fun j author ->
             if not (Author.is_valid author) then
               add_error (Printf.sprintf "item %d author %d is invalid" i j)))
        (Item.authors item))
    feed.items;

  match !errors with [] -> Ok () | errs -> Error (List.rev errs)
