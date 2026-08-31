(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  slug : string;
  title : string;
  published_date : Ptime.t;
  uuid : string;
  description : string;
  url : string;
  talk : bool;
  vertical : bool;
  paper : string option;
  project : string option;
  tags : string list;
  social : Bushel_types.social option;
}

type ts = t list

let slug { slug; _ } = slug
let title { title; _ } = title
let uuid { uuid; _ } = uuid
let url { url; _ } = url
let description { description; _ } = description
let talk { talk; _ } = talk
let vertical { vertical; _ } = vertical
let paper { paper; _ } = paper
let project { project; _ } = project
let tags { tags; _ } = tags
let social { social; _ } = social

let date { published_date; _ } = Ptime.to_date published_date
let datetime { published_date; _ } = published_date

let compare a b = Ptime.compare b.published_date a.published_date

let jsont : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title published_date uuid url talk vertical tags paper project social =
    { slug = uuid; title; published_date; uuid; description = ""; url;
      talk; vertical; paper; project; tags; social }
  in
  map ~kind:"Video" make
  |> mem "title" string ~enc:(fun v -> v.title)
  |> mem "published_date" Bushel_types.ptime_jsont ~enc:(fun v -> v.published_date)
  |> mem "uuid" string ~enc:(fun v -> v.uuid)
  |> mem "url" string ~dec_absent:"" ~enc:(fun v -> v.url)
  |> mem "talk" bool ~dec_absent:false ~enc:(fun v -> v.talk)
  |> mem "vertical" bool ~dec_absent:false ~enc:(fun v -> v.vertical)
  |> mem "tags" (list string) ~dec_absent:[] ~enc:(fun v -> v.tags)
  |> mem "paper" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun v -> v.paper)
  |> mem "project" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun v -> v.project)
  |> mem "social" (option Bushel_types.social_jsont) ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun v -> v.social)
  |> finish

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  match Frontmatter.decode jsont fm with
  | Error e -> Error e
  | Ok v ->
    Ok { v with slug = v.uuid; description = Frontmatter.body fm }

let pp ppf v =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Video";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string (slug v);
  pf ppf "%a: %a@," (styled `Bold string) "UUID" string (uuid v);
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title v);
  let (year, month, day) = date v in
  pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Date" year month day;
  pf ppf "%a: %a@," (styled `Bold string) "URL" string (url v);
  pf ppf "%a: %b@," (styled `Bold string) "Talk" (talk v);
  if vertical v then
    pf ppf "%a: %b@," (styled `Bold string) "Vertical" true;
  (match paper v with
   | Some p -> pf ppf "%a: %a@," (styled `Bold string) "Paper" string p
   | None -> ());
  (match project v with
   | Some p -> pf ppf "%a: %a@," (styled `Bold string) "Project" string p
   | None -> ());
  let t = tags v in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  pf ppf "@,";
  pf ppf "%a:@," (styled `Bold string) "Description";
  pf ppf "%a@," string v.description;
  pf ppf "@]"
