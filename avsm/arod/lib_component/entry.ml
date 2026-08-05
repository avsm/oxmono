(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared entry components for the Arod site.

    Provides common types, utilities, and rendering functions for
    Bushel entries including headings, metadata, date formatting,
    and body rendering. *)

open Htmlit

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Note = Bushel.Note
module Video = Bushel.Video
module Idea = Bushel.Idea
module Project = Bushel.Project
module Tags = Bushel.Tags
module Img = Srcsetter
module Contact = Sortal_schema.Contact

(** {1 Entry Types} *)

type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

let entry_type_to_string = function
  | `Paper -> "paper" | `Note -> "note" | `Video -> "video"
  | `Idea -> "idea" | `Project -> "project"

let entry_type_of_string = function
  | "paper" -> Some `Paper | "note" -> Some `Note | "video" -> Some `Video
  | "idea" -> Some `Idea | "project" -> Some `Project | _ -> None

(** {1 Date Formatting} *)

let int_to_date_suffix ~r n =
  let suffix =
    if n mod 10 = 1 && n mod 100 <> 11 then "st"
    else if n mod 10 = 2 && n mod 100 <> 12 then "nd"
    else if n mod 10 = 3 && n mod 100 <> 13 then "rd"
    else "th"
  in
  let x = string_of_int n in
  let x = if r && String.length x = 1 then " " ^ x else x in
  x ^ suffix

let ptime_date ?(r=false) ?(with_d=false) (y,m,d) =
  let ms = Common.month_name_full m in
  match with_d with
  | false -> Printf.sprintf "%s %4d" ms y
  | true -> Printf.sprintf "%s %s %4d" (int_to_date_suffix ~r d) ms y

(** {1 Icon Helpers} *)

let ent_to_icon = function
  | `Paper _ -> "paper.svg" | `Note _ -> "note.svg"
  | `Project _ -> "project.svg" | `Idea _ -> "idea.svg"
  | `Video _ -> "video.svg"

(** {1 Entry Filtering} *)

let entry_matches_type types ent =
  if types = [] then true
  else List.exists (fun typ ->
    match typ, ent with
    | `Paper, `Paper _ -> true | `Note, `Note _ -> true
    | `Video, `Video _ -> true | `Idea, `Idea _ -> true
    | `Project, `Project _ -> true | _ -> false
  ) types

let get_entries ~(ctx : Arod.Ctx.t) ~types =
  let filterent = entry_matches_type types in
  let select ent =
    let only_talks = function
      | `Video { Video.talk; _ } -> talk
      | _ -> true
    in
    let not_index_page = function
      | `Note { Note.index_page; _ } -> not index_page
      | _ -> true
    in
    only_talks ent && not_index_page ent
  in
  Arod.Ctx.all_entries ctx
  |> List.filter (fun ent -> select ent && filterent ent)
  |> List.sort Entry.compare
  |> List.rev

let perma_entries ~(ctx : Arod.Ctx.t) =
  Arod.Ctx.all_entries ctx
  |> List.filter (function `Note n -> Note.perma n | _ -> false)
  |> List.sort Entry.compare
  |> List.rev

(** {1 Markdown Rendering} *)

let md_to_html ~ctx content = fst (Arod.Md.to_html ~ctx content)

(** {1 Body Rendering} *)

let truncated_body ~ctx ent =
  let markdown_content, word_count_info = Common.truncate_body_parts ent in
  let markdown_with_link = match word_count_info with
    | Some (total, true) ->
      let url = Entry.site_url ent in
      markdown_content ^ "\n\n*[Read full note... (" ^ string_of_int total ^
      " words](" ^ url ^ "))*\n"
    | _ -> markdown_content
  in
  (El.unsafe_raw (md_to_html ~ctx markdown_with_link), word_count_info)

let full_body ~ctx ent =
  El.unsafe_raw (md_to_html ~ctx (Entry.body ent))

(** {1 Entry Heading} *)

let heading ~ctx ?tag ent =
  let via, via_url = match ent with
    | `Note n -> (match n.Note.via with None -> None, None | Some (t,u) -> Some t, Some u)
    | _ -> None, None
  in
  let via_el = match via, via_url with
    | Some t, Some u when t <> "" ->
      El.a ~at:[At.href u; At.class' "text-sm text-secondary"]
        [El.txt (Printf.sprintf "(via %s)" t)]
    | _, Some u ->
      El.a ~at:[At.href u; At.class' "text-sm text-secondary"]
        [El.txt "(via)"]
    | _ -> El.void
  in
  let title_text = Entry.title ent in
  match ent with
  | `Note {index_page=true;_} -> El.void
  | _ ->
    let h_fn = match tag with
      | Some "h1" -> El.h1 | Some "h3" -> El.h3
      | Some "h4" -> El.h4 | Some "h5" -> El.h5 | Some "h6" -> El.h6
      | _ -> El.h2
    in
    let (ey, em, ed) = Entry.date ent in
    let date_str = ptime_date ~with_d:false (ey, em, ed) in
    let doi_el = match ent with
      | `Note n when Note.perma n ->
        (match Note.doi n with
         | Some doi_str ->
           El.span ~at:[At.class' "text-sm"] [
             El.txt " / ";
             El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt "DOI"]]
         | None -> El.void)
      | _ -> El.void
    in
    h_fn ~at:[At.class' "text-2xl font-semibold tracking-tight mb-2"] [
      El.a ~at:[At.href (Entry.site_url ent)] [El.txt title_text];
      El.txt " "; via_el;
      El.span ~at:[At.class' "text-sm text-secondary"] [
        El.txt " / ";
        El.time ~at:[At.v "datetime" (Printf.sprintf "%04d-%02d-%02d" ey em ed)]
          [El.txt date_str]];
      doi_el]

(** {1 Metadata Row} *)

let meta ~ctx ?backlinks_content ent =
  let date_str = ptime_date ~with_d:true (Entry.date ent) in
  let all_tags = Arod.Ctx.tags_of_ent ctx ent in
  (* Date element *)
  let date_el =
    El.time ~at:[At.v "datetime" (let (y,m,d) = Entry.date ent in
      Printf.sprintf "%04d-%02d-%02d" y m d)]
      [El.txt date_str]
  in
  (* DOI element *)
  let doi_el = match ent with
    | `Note n when Note.perma n ->
      (match Note.doi n with
       | Some doi_str ->
         [El.span ~at:[At.class' "mx-2"] [El.txt "\xC2\xB7"];
          El.txt "DOI: ";
          El.a ~at:[At.href ("https://doi.org/" ^ doi_str);
                    At.class' "text-secondary"]
            [El.txt doi_str]]
       | None -> [])
    | _ -> []
  in
  (* Tag elements *)
  let tag_els = match all_tags with
    | [] -> []
    | tags ->
      let sep = El.span ~at:[At.class' "mx-2"] [El.txt "\xC2\xB7"] in
      sep ::
      List.concat (List.mapi (fun i tag ->
        let tag_str = Tags.to_raw_string tag in
        let el = El.a ~at:[At.href ("#tag=" ^ tag_str);
                       At.v "data-tag" tag_str;
                       At.class' "text-secondary"]
                   [El.txt ("#" ^ tag_str)] in
        if i > 0 then [El.txt " "; el] else [el]
      ) tags)
  in
  (* Backlinks element *)
  let backlinks_el = match backlinks_content with
    | Some content ->
      let entry_slug = Entry.slug ent in
      let checkbox_id = "sidenote__checkbox--backlinks-" ^ entry_slug in
      let content_id = "sidenote-backlinks-" ^ entry_slug in
      [El.span ~at:[At.class' "mx-2"] [El.txt "\xC2\xB7"];
       El.span ~at:[At.v "class" "sidenote"; At.role "note"] [
         El.input ~at:[At.type' "checkbox"; At.id checkbox_id;
                    At.v "class" "sidenote__checkbox";
                    At.v "aria-label" "Show backlinks";
                    At.v "aria-hidden" "true"; At.hidden] ();
         El.label ~at:[At.for' checkbox_id;
                    At.v "class" "sidenote__button";
                    At.v "data-sidenote-number" "\xe2\x86\x91";
                    At.v "aria-describedby" content_id;
                    At.tabindex 0]
           [El.txt "backlinks"];
         El.span ~at:[At.id content_id;
                   At.v "class" "sidenote__content";
                   At.v "aria-hidden" "true"; At.hidden;
                   At.v "data-sidenote-number" "\xe2\x86\x91"]
           [content]]]
    | None -> []
  in
  El.p ~at:[At.class' "text-sm text-secondary mb-2"]
    ([date_el] @ doi_el @ tag_els @ backlinks_el)

(** {1 Entry Type Label} *)

let sort_of_ent ent =
  match ent with
  | `Paper p -> (match Paper.bibtype p with
    | "inproceedings" -> "conference paper" | "article" | "journal" -> "journal paper"
    | "misc" -> "preprint" | "techreport" -> "technical report" | _ -> "paper"), ""
  | `Note {Note.updated=Some _;date=u; _} ->
    "note", Printf.sprintf " (originally on %s)" (ptime_date ~with_d:true u)
  | `Note _ -> "note", "" | `Project _ -> "project", ""
  | `Idea _ -> "research idea", "" | `Video _ -> "video", ""

(** {1 Backlinks Rendering} *)

let render_backlinks_content ~ctx ent =
  let slug = Entry.slug ent in
  let entry_type = match ent with
    | `Paper _ -> "paper" | `Note _ -> "note" | `Idea _ -> "idea"
    | `Project _ -> "project" | `Video _ -> "video"
  in
  let entries = Arod.Ctx.entries ctx in
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug slug in
  if backlink_slugs = [] then None
  else
    let backlink_items = List.filter_map (fun backlink_slug ->
      match Entry.lookup entries backlink_slug with
      | Some entry ->
        let title = Entry.title entry in
        let url = Entry.site_url entry in
        let link = El.a ~at:[At.href url] [El.txt title] in
        Some (El.li [link])
      | None -> None
    ) backlink_slugs in
    if backlink_items = [] then None
    else Some (
      El.div [
        El.txt (Printf.sprintf "The following entries link to this %s: " entry_type);
        El.ul ~at:[At.class' "text-sm mt-1"] backlink_items])
