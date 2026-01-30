(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Note entry type for Bushel *)

type t = {
  title : string;
  date : Ptime.date;
  slug : string;
  body : string;
  tags : string list;
  draft : bool;
  updated : Ptime.date option;
  sidebar : string option;
  index_page : bool;
  perma : bool;              (** Permanent article that will receive a DOI *)
  doi : string option;       (** DOI identifier for permanent articles *)
  synopsis : string option;
  titleimage : string option;
  via : (string * string) option;  (** (label, url) for link-style notes *)
  slug_ent : string option;  (** Reference to another entry *)
  source : string option;    (** Source for news-style notes *)
  url : string option;       (** External URL for news-style notes *)
  author : string option;    (** Author for news-style notes *)
  category : string option;  (** Category for news-style notes *)
  standardsite : string option;  (** Standards body site reference *)
}

type ts = t list

(** {1 Accessors} *)

let title { title; _ } = title
let slug { slug; _ } = slug
let body { body; _ } = body
let tags { tags; _ } = tags
let draft { draft; _ } = draft
let sidebar { sidebar; _ } = sidebar
let synopsis { synopsis; _ } = synopsis
let perma { perma; _ } = perma
let doi { doi; _ } = doi
let titleimage { titleimage; _ } = titleimage
let slug_ent { slug_ent; _ } = slug_ent
let source { source; _ } = source
let url { url; _ } = url
let author { author; _ } = author
let category { category; _ } = category
let standardsite { standardsite; _ } = standardsite

let origdate { date; _ } = Bushel_types.ptime_of_date_exn date

let date { date; updated; _ } =
  match updated with
  | None -> date
  | Some v -> v

let datetime v = Bushel_types.ptime_of_date_exn (date v)

let link { body; via; slug; _ } =
  match body, via with
  | "", Some (l, u) -> `Ext (l, u)
  | "", None -> failwith (slug ^ ": note external without via, via-url")
  | _, _ -> `Local slug

let words { body; _ } = Bushel_util.count_words body

(** {1 Comparison} *)

let compare a b = Ptime.compare (datetime b) (datetime a)

(** {1 Lookup} *)

let lookup slug notes = List.find_opt (fun n -> n.slug = slug) notes

(** {1 Jsont Codec} *)

let via_jsont : (string * string) option Jsont.t =
  (* via is encoded as two separate fields: via and via-url *)
  Jsont.null None  (* Handled specially in of_frontmatter *)

let jsont ~default_date ~default_slug : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title date slug tags draft updated index_page perma doi synopsis titleimage
           slug_ent source url author category standardsite =
    { title; date; slug; body = ""; tags; draft; updated; sidebar = None;
      index_page; perma; doi; synopsis; titleimage; via = None;
      slug_ent; source; url; author; category; standardsite }
  in
  map ~kind:"Note" make
  |> mem "title" string ~enc:(fun n -> n.title)
  |> mem "date" Bushel_types.ptime_date_jsont ~dec_absent:default_date ~enc:(fun n -> n.date)
  |> mem "slug" string ~dec_absent:default_slug ~enc:(fun n -> n.slug)
  |> mem "tags" (list string) ~dec_absent:[] ~enc:(fun n -> n.tags)
  |> mem "draft" bool ~dec_absent:false ~enc:(fun n -> n.draft)
  |> mem "updated" (option Bushel_types.ptime_date_jsont) ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.updated)
  |> mem "index_page" bool ~dec_absent:false ~enc:(fun n -> n.index_page)
  |> mem "perma" bool ~dec_absent:false ~enc:(fun n -> n.perma)
  |> mem "doi" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.doi)
  |> mem "synopsis" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.synopsis)
  |> mem "titleimage" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.titleimage)
  |> mem "slug_ent" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.slug_ent)
  |> mem "source" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.source)
  |> mem "url" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.url)
  |> mem "author" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.author)
  |> mem "category" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.category)
  |> mem "standardsite" Bushel_types.string_option_jsont ~dec_absent:None
       ~enc_omit:Option.is_none ~enc:(fun n -> n.standardsite)
  |> finish

(** {1 Parsing} *)

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  (* Extract slug and date from filename to use as defaults *)
  let default_slug, default_date =
    match Frontmatter.fname fm with
    | Some fname ->
      (match Frontmatter.slug_of_fname fname with
       | Ok (s, d) -> (s, Option.fold ~none:(1, 1, 1) ~some:Ptime.to_date d)
       | Error _ -> ("", (1, 1, 1)))
    | None -> ("", (1, 1, 1))
  in
  (* Get via fields manually since they're two separate fields *)
  let via =
    match Frontmatter.find_string "via" fm, Frontmatter.find_string "via-url" fm with
    | Some a, Some b -> Some (a, b)
    | None, Some b -> Some ("", b)
    | _ -> None
  in
  match Frontmatter.decode (jsont ~default_date ~default_slug) fm with
  | Error e -> Error e
  | Ok n -> Ok { n with body = Frontmatter.body fm; via }

(** {1 Pretty Printing} *)

let pp ppf n =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Note";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string (slug n);
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title n);
  let (year, month, day) = date n in
  pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Date" year month day;
  (match n.updated with
   | Some (y, m, d) -> pf ppf "%a: %04d-%02d-%02d@," (styled `Bold string) "Updated" y m d
   | None -> ());
  pf ppf "%a: %b@," (styled `Bold string) "Draft" (draft n);
  pf ppf "%a: %b@," (styled `Bold string) "Index Page" n.index_page;
  pf ppf "%a: %b@," (styled `Bold string) "Perma" (perma n);
  (match doi n with
   | Some d -> pf ppf "%a: %a@," (styled `Bold string) "DOI" string d
   | None -> ());
  (match synopsis n with
   | Some syn -> pf ppf "%a: %a@," (styled `Bold string) "Synopsis" string syn
   | None -> ());
  (match titleimage n with
   | Some img -> pf ppf "%a: %a@," (styled `Bold string) "Title Image" string img
   | None -> ());
  (match n.via with
   | Some (label, url) ->
     if label <> "" then
       pf ppf "%a: %a (%a)@," (styled `Bold string) "Via" string label string url
     else
       pf ppf "%a: %a@," (styled `Bold string) "Via" string url
   | None -> ());
  (match standardsite n with
   | Some site -> pf ppf "%a: %a@," (styled `Bold string) "Standard Site" string site
   | None -> ());
  let t = tags n in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  (match sidebar n with
   | Some sb ->
     pf ppf "@,";
     pf ppf "%a:@," (styled `Bold string) "Sidebar";
     pf ppf "%a@," string sb
   | None -> ());
  let bd = body n in
  if bd <> "" then begin
    pf ppf "@,";
    pf ppf "%a:@," (styled `Bold string) "Body";
    pf ppf "%a@," string bd;
  end;
  pf ppf "@]"
