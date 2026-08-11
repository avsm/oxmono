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

(** {1 Metadata Row} *)

let meta ~ctx ent =
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
  El.p ~at:[At.class' "text-sm text-secondary mb-2"]
    ([date_el] @ doi_el @ tag_els)
