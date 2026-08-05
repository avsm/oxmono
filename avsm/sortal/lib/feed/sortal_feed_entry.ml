(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  id : string;
  title : string option;
  date : Ptime.t option;
  summary : string option;
  content : string option;
  url : Uri.t option;
  source_feed : string;
  source_type : Sortal_schema.Feed.feed_type;
}

let xhtml_ns_prefix = function
  | "http://www.w3.org/1999/xhtml" -> Some ""
  | "http://www.w3.org/2005/Atom" -> Some ""
  | s -> Some s

let xhtml_to_string nodes =
  String.concat "" (List.map (Syndic.XML.to_string ~ns_prefix:xhtml_ns_prefix) nodes)

let text_of_text_construct (tc : Syndic.Atom.text_construct) =
  match tc with
  | Text s -> Some s
  | Html (_, s) -> Some s
  | Xhtml (_, nodes) -> Some (xhtml_to_string nodes)

let text_of_content (c : Syndic.Atom.content) =
  match c with
  | Text s -> Some s
  | Html (_, s) -> Some s
  | Xhtml (_, nodes) -> Some (xhtml_to_string nodes)
  | Mime (_, s) -> Some s
  | Src _ -> None

let of_atom_entry ~source_feed (entry : Syndic.Atom.entry) =
  let id = Uri.to_string entry.id in
  let title = text_of_text_construct entry.title in
  let date = match entry.published with
    | Some d -> Some d
    | None -> Some entry.updated
  in
  let summary = Option.bind entry.summary text_of_text_construct in
  let content = Option.bind entry.content text_of_content in
  let url =
    List.find_map (fun (link : Syndic.Atom.link) ->
      match link.rel with
      | Syndic.Atom.Alternate -> Some link.href
      | _ -> None
    ) entry.links
  in
  { id; title; date; summary; content; url;
    source_feed; source_type = Atom }

let of_rss2_item ~source_feed (item : Syndic.Rss2.item) =
  let id = match item.guid with
    | Some guid -> Uri.to_string guid.data
    | None -> match item.link with
      | Some link -> Uri.to_string link
      | None -> source_feed ^ "#unknown"
  in
  let title, summary = match item.story with
    | Syndic.Rss2.All (t, _, d) -> (Some t, Some d)
    | Syndic.Rss2.Title t -> (Some t, None)
    | Syndic.Rss2.Description (_, d) -> (None, Some d)
  in
  let date = item.pubDate in
  let content = let (_, c) = item.content in
    if String.length c > 0 then Some c else None
  in
  let url = item.link in
  { id; title; date; summary; content; url;
    source_feed; source_type = Rss }

let of_jsonfeed_item ~source_feed (item : Jsonfeed.Item.t) =
  let id = Jsonfeed.Item.id item in
  let title = Jsonfeed.Item.title item in
  let date = Jsonfeed.Item.date_published item in
  let summary = Jsonfeed.Item.summary item in
  let content = match Jsonfeed.Item.content_text item with
    | Some _ as c -> c
    | None -> Jsonfeed.Item.content_html item
  in
  let url = Option.map Uri.of_string (Jsonfeed.Item.url item) in
  { id; title; date; summary; content; url;
    source_feed; source_type = Json }

let compare_by_date a b =
  match a.date, b.date with
  | Some da, Some db -> Ptime.compare db da
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let pp ppf t =
  let date_str = match t.date with
    | Some d -> Ptime.to_rfc3339 ~space:false d
    | None -> "no-date"
  in
  let title_str = Option.value ~default:"(untitled)" t.title in
  let type_str = Sortal_schema.Feed.feed_type_to_string t.source_type in
  Fmt.pf ppf "%a  %a  [%s] %a"
    (Fmt.styled (`Fg `Cyan) Fmt.string) date_str
    Fmt.string title_str
    type_str
    (Fmt.styled (`Fg `White) Fmt.string) t.id

let pp_full ppf t =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %s@," (styled `Bold string) "ID" t.id;
  Option.iter (fun title -> pf ppf "%a: %s@," (styled `Bold string) "Title" title) t.title;
  Option.iter (fun d -> pf ppf "%a: %s@," (styled `Bold string) "Date" (Ptime.to_rfc3339 d)) t.date;
  Option.iter (fun u -> pf ppf "%a: %s@," (styled `Bold string) "URL" (Uri.to_string u)) t.url;
  pf ppf "%a: %s (%s)@,"
    (styled `Bold string) "Source"
    t.source_feed
    (Sortal_schema.Feed.feed_type_to_string t.source_type);
  Option.iter (fun s -> pf ppf "%a: %s@," (styled `Bold string) "Summary" s) t.summary;
  Option.iter (fun c ->
    let preview = if String.length c > 200 then String.sub c 0 200 ^ "..." else c in
    pf ppf "%a: %s@," (styled `Bold string) "Content" preview
  ) t.content;
  pf ppf "@]"
