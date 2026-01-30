(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Video entry type for Bushel *)

type t = {
  slug : string;
  title : string;
  published_date : Ptime.t;
  uuid : string;
  description : string;
  url : string;
  talk : bool;
  paper : string option;
  project : string option;
  tags : string list;
}

type ts = t list

(** {1 Accessors} *)

let slug { slug; _ } = slug
let title { title; _ } = title
let uuid { uuid; _ } = uuid
let url { url; _ } = url
let description { description; _ } = description
let body = description  (* Alias for consistency *)
let talk { talk; _ } = talk
let paper { paper; _ } = paper
let project { project; _ } = project
let tags { tags; _ } = tags

let date { published_date; _ } = Ptime.to_date published_date
let datetime { published_date; _ } = published_date

(** {1 Comparison} *)

let compare a b = Ptime.compare b.published_date a.published_date

(** {1 Lookup} *)

let lookup videos uuid = List.find_opt (fun v -> v.uuid = uuid) videos
let lookup_by_slug videos slug = List.find_opt (fun v -> v.slug = slug) videos

(** {1 Jsont Codec} *)

let jsont : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title published_date uuid url talk tags paper project =
    { slug = uuid; title; published_date; uuid; description = ""; url;
      talk; paper; project; tags }
  in
  map ~kind:"Video" make
  |> mem "title" string ~enc:(fun v -> v.title)
  |> mem "published_date" Bushel_types.ptime_jsont ~enc:(fun v -> v.published_date)
  |> mem "uuid" string ~enc:(fun v -> v.uuid)
  |> mem "url" string ~enc:(fun v -> v.url)
  |> mem "talk" bool ~dec_absent:false ~enc:(fun v -> v.talk)
  |> mem "tags" (list string) ~dec_absent:[] ~enc:(fun v -> v.tags)
  |> mem "paper" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun v -> v.paper)
  |> mem "project" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun v -> v.project)
  |> finish

(** {1 Parsing} *)

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  match Frontmatter.decode jsont fm with
  | Error e -> Error e
  | Ok v ->
    Ok { v with
         slug = v.uuid;
         description = Frontmatter.body fm }

(** {1 YAML Serialization} *)

let to_yaml t =
  let open Yamlrw.Util in
  let fields = [
    ("title", string t.title);
    ("description", string t.description);
    ("url", string t.url);
    ("uuid", string t.uuid);
    ("slug", string t.slug);
    ("published_date", string (Ptime.to_rfc3339 t.published_date));
    ("talk", bool t.talk);
    ("tags", strings t.tags);
  ] in
  let fields = match t.paper with
    | None -> fields
    | Some p -> ("paper", string p) :: fields
  in
  let fields = match t.project with
    | None -> fields
    | Some p -> ("project", string p) :: fields
  in
  obj fields

(** {1 Pretty Printing} *)

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
