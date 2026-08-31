(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Video components. *)

open Htmlit

module Video = Bushel.Video
module I = Arod.Icons

let strip_scheme url =
  if String.starts_with ~prefix:"https://" url then
    String.sub url 8 (String.length url - 8)
  else if String.starts_with ~prefix:"http://" url then
    String.sub url 7 (String.length url - 7)
  else url

(** [video_card ~ctx v] is a list card for [v]. *)
let video_card ~ctx v =
  let (y, m, _d) = Video.date v in
  let date_str = Printf.sprintf "%s %d" (Common.month_name m) y in
  let slug = Video.slug v in
  let url = "/videos/" ^ slug in
  let embed_md = Printf.sprintf "![%%c](:%s)" slug in
  let embed_html = fst (Arod.Md.to_html ~ctx embed_md) in
  let embed_el =
    El.div ~at:[At.class' "vid-card-embed"]
      [El.unsafe_raw embed_html]
  in
  let desc = Bushel.Util.first_hunk (Video.description v) in
  let desc_el =
    if desc = "" then El.void
    else
      El.div ~at:[At.class' "vid-card-desc"]
        [El.unsafe_raw (Arod.Md.to_plain_html ~ctx desc)]
  in
  let tags_el = Common.card_tags (Video.tags v) in
  let links_els = List.filter_map Fun.id [
    (match Video.project v with
     | Some proj_slug ->
       let title = match Arod.Ctx.lookup ctx proj_slug with
         | Some (`Project proj) -> Bushel.Project.title proj
         | _ -> proj_slug
       in
       Some (Common.card_entry_row
               ~icon:(I.outline ~size:11 I.folder_o)
               ~href:("/projects/" ^ proj_slug) ~title)
     | None -> None);
    (match Video.paper v with
     | Some paper_slug ->
       let title = match Arod.Ctx.lookup ctx paper_slug with
         | Some (`Paper paper) -> Bushel.Paper.title paper
         | _ -> paper_slug
       in
       Some (Common.card_entry_row
               ~icon:(I.outline ~size:11 I.paper_o)
               ~href:("/papers/" ^ paper_slug) ~title)
     | None -> None);
  ] in
  let entries = Arod.Ctx.entries ctx in
  let backlink_slugs = Arod.Ctx.backlinks ctx slug in
  let outbound_slugs = Arod.Ctx.outbound ctx slug in
  let all_linked =
    List.filter_map (Bushel.Entry.lookup entries) (backlink_slugs @ outbound_slugs)
  in
  let seen = Hashtbl.create 8 in
  let exclude_slugs = List.filter_map Fun.id [
    Video.project v; Video.paper v
  ] in
  List.iter (fun s -> Hashtbl.replace seen s ()) (slug :: exclude_slugs);
  let backlink_rows = List.filter_map (fun ent ->
    let s = Bushel.Entry.slug ent in
    if Hashtbl.mem seen s then None
    else begin
      Hashtbl.replace seen s ();
      Some (Common.card_entry_row
              ~icon:(Sidebar.entry_type_icon ~size:11 ent)
              ~href:(Bushel.Entry.site_url ent)
              ~title:(Bushel.Entry.title ent))
    end
  ) all_linked in
  let all_refs = links_els @ backlink_rows in
  let refs_el = match all_refs with
    | [] -> El.void
    | els -> El.div ~at:[At.class' "vid-card-refs"] els
  in
  El.div ~at:[At.class' "vid-card not-prose h-entry"] [
    Common.card_header ~title_cls:"p-name u-url"
      ~prompt:"\xe2\x96\xb6" ~title:(Video.title v) ~href:url
      (El.time ~at:[At.class' "proj-card-date dt-published";
                    At.v "datetime" (Printf.sprintf "%04d-%02d" y m)]
         [El.txt date_str]);
    embed_el;
    El.div ~at:[At.class' "vid-card-body"] [
      desc_el; tags_el; refs_el]]

(** [videos_list ~ctx] is the list of talks. *)
let videos_list ~ctx =
  let all_entries = Arod.Ctx.all_entries ctx in
  let talks = List.filter_map (fun e ->
    match e with
    | `Video v when Video.talk v -> Some v
    | _ -> None
  ) all_entries in
  let talks = List.sort (fun a b ->
    compare (Video.date b) (Video.date a)
  ) talks in
  let cards = List.map (fun v -> video_card ~ctx v) talks in
  El.article ~at:[At.class' "h-feed"]
    [Common.hidden_feed_meta ~ctx "Talks";
     El.div ~at:[At.class' "vid-grid"] cards]

(** [full_page ~ctx v] is the article and sidebar for [v]. *)
let full_page ~ctx v =
  let slug = Video.slug v in
  let (y, m, d) = Video.date v in
  let embed_md = Printf.sprintf "![%%c](:%s)" slug in
  let embed_html = fst (Arod.Md.to_html ~ctx embed_md) in
  let desc_html = Arod.Md.to_plain_html ~ctx (Video.description v) in
  let tags_el = Common.detail_tags (Video.tags v) in
  let hidden_author = Common.hidden_author_hcard ~ctx in
  let hidden_dt = Common.hidden_dt_published (y, m, d) in
  let hidden_meta = Common.hidden_entry_meta ~ctx (`Video v) in
  let article = El.div ~at:[At.class' "h-entry"] [
    Common.page_title ~cls:"page-title text-xl font-semibold mb-1 p-name"
      (Video.title v);
    hidden_author; hidden_dt; hidden_meta;
    tags_el;
    El.div ~at:[At.class' "vid-embed mb-6"] [El.unsafe_raw embed_html];
    El.div ~at:[At.class' "e-content p-summary"] [El.unsafe_raw desc_html]]
  in
  let datetime_str = Printf.sprintf "%04d-%02d-%02d" y m d in
  let date_el =
    Sidebar.meta_line
      ~icon:(I.outline ~cl:"opacity-50" ~size:12 I.calendar_o)
      (El.time ~at:[At.v "datetime" datetime_str]
         [El.txt (Printf.sprintf "%d %s %d" d (Common.month_name_full m) y)])
  in
  let type_el =
    let label = if Video.talk v then "Conference talk" else "Video" in
    let icon = if Video.talk v then I.presentation_o else I.video_o in
    Sidebar.meta_line ~icon:(I.outline ~cl:"opacity-50" ~size:12 icon)
      (El.txt label)
  in
  let url_el =
    let host =
      let u = strip_scheme (Video.url v) in
      match String.index_opt u '/' with
      | Some i -> String.sub u 0 i
      | None -> if u = "" then "Watch" else u
    in
    Sidebar.meta_line
      ~icon:(I.outline ~cl:"opacity-50" ~size:12 I.external_link_o)
      (El.a ~at:[At.href (Video.url v);
                 At.class' "sidebar-meta-link"]
         [El.txt host])
  in
  let proj_el = match Video.project v with
    | Some proj_slug ->
      let title = match Arod.Ctx.lookup ctx proj_slug with
        | Some (`Project proj) -> Bushel.Project.title proj
        | _ -> proj_slug
      in
      Sidebar.meta_line
        ~icon:(I.outline ~cl:"opacity-50" ~size:12 I.folder_o)
        (El.a ~at:[At.href ("/projects/" ^ proj_slug);
                   At.class' "sidebar-meta-link"]
           [El.txt title])
    | None -> El.void
  in
  let paper_el = match Video.paper v with
    | Some paper_slug ->
      let title = match Arod.Ctx.lookup ctx paper_slug with
        | Some (`Paper paper) -> Bushel.Paper.title paper
        | _ -> paper_slug
      in
      Sidebar.meta_line
        ~icon:(I.outline ~cl:"opacity-50" ~size:12 I.paper_o)
        (El.a ~at:[At.href ("/papers/" ^ paper_slug);
                   At.class' "sidebar-meta-link"]
           [El.txt title])
    | None -> El.void
  in
  let links_el, links_modal_el = Sidebar.entry_links ~ctx slug in
  let abbrev_url =
    let stripped = strip_scheme (Video.url v) in
    if String.length stripped > 30 then
      String.sub stripped 0 30 ^ "\xe2\x80\xa6"
    else stripped
  in
  let sidebar =
    El.aside ~at:[At.class' "lg:w-72 shrink-0 min-w-0"] [
      El.div ~at:[At.class' "relative h-full"] [
        Common.meta_box
          ~header:[El.txt " ";
                   El.a ~at:[At.href (Video.url v);
                             At.class' "sidebar-meta-link"] [El.txt abbrev_url]]
          [date_el; type_el; url_el; proj_el; paper_el; links_el];
        links_modal_el]]
  in
  (article, sidebar)

(** [brief ~ctx v] is a brief rendering of [v]. *)
let brief ~ctx v =
  let md =
    Printf.sprintf "![%%c](:%s)\n\n%s" v.Video.slug v.Video.description
  in
  let heading =
    let y, m, _ = Video.date v in
    El.h2 ~at:[At.class' "text-xl font-semibold mb-2"] [
      El.a ~at:[At.href (Bushel.Entry.site_url (`Video v));
                At.class' "p-name u-url"] [
        El.txt (Video.title v)];
      El.span ~at:[At.class' "text-sm text-secondary"] [
        El.txt " / ";
        El.time ~at:[At.v "datetime" (Printf.sprintf "%04d-%02d" y m);
                     At.class' "dt-published"]
          [El.txt (Printf.sprintf "%s %4d" (Common.month_name m) y)]]]
  in
  let body = [
    heading;
    El.unsafe_raw (fst (Arod.Md.to_html ~ctx md))] in
  (El.div body, None)

(** [for_feed ~ctx v] is the feed rendering of [v]. *)
let for_feed ~ctx v =
  let md = Printf.sprintf "![%%c](:%s)\n\n" v.Video.slug in
  (El.unsafe_raw (fst (Arod.Md.to_html ~ctx md)), None)
