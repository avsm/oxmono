(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** List and feed view components using htmlit. *)

open Htmlit

(** {1 Entry Types} *)

type entry_type = Entry.entry_type

let entry_type_to_string = Entry.entry_type_to_string
let entry_type_of_string = Entry.entry_type_of_string

(** Truncate the body of an entry. *)
let truncated_body ~ctx ent =
  let markdown_content, word_count_info = Common.truncate_body_parts ent in
  let markdown_with_link = match word_count_info with
    | Some (total, true) ->
      let url = Bushel.Entry.site_url ent in
      markdown_content ^ "\n\n*[Read full note... (" ^ string_of_int total ^
      " words](" ^ url ^ "))*\n"
    | _ -> markdown_content
  in
  (El.unsafe_raw (fst (Arod.Md.to_html ~ctx markdown_with_link)), word_count_info)

(** {1 Entry Filtering} *)

let entry_matches_type = Entry.entry_matches_type
let get_entries = Entry.get_entries

(** {1 Entry Heading} *)

let entry_heading ~ctx:_ ent =
  let via, via_url = match ent with
    | `Note n ->
      (match n.Bushel.Note.via with
       | None -> None, None
       | Some (t, u) -> Some t, Some u)
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
  match ent with
  | `Note { index_page = true; _ } -> El.void
  | _ ->
    let doi_el = match ent with
      | `Note n when Bushel.Note.perma n ->
        (match Bushel.Note.doi n with
         | Some doi_str ->
           El.span ~at:[At.class' "text-sm text-secondary"] [
             El.txt " / ";
             El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt "DOI"]]
         | None -> El.void)
      | _ -> El.void
    in
    El.h2 ~at:[At.class' "text-xl font-semibold mb-2"] [
      El.a ~at:[At.href (Bushel.Entry.site_url ent)] [
        El.txt (Bushel.Entry.title ent)];
      El.txt " "; via_el;
      El.span ~at:[At.class' "text-sm text-secondary"] [
        El.txt " / ";
        El.txt (Common.ptime_date_short (Bushel.Entry.date ent))];
      doi_el]

(** {1 Tags Metadata} *)

let tags_meta ~ctx ent =
  let all_tags = Arod.Ctx.tags_of_ent ctx ent in
  let date_str = Common.ptime_date_short (Bushel.Entry.date ent) in
  let link_el =
    El.a ~at:[At.href (Bushel.Entry.site_url ent);
              At.class' "text-sm text-secondary"] [El.txt "#"]
  in
  let bullet = El.span ~at:[At.class' "mx-2 text-gray-400"] [El.txt "\u{2022}"] in
  let tag_els = match all_tags with
    | [] -> El.void
    | tags ->
      let tag_spans = List.map (fun tag ->
        let tag_str = Bushel.Tags.to_raw_string tag in
        El.a ~at:[At.v "data-tag" tag_str;
                  At.href ("#tag=" ^ tag_str);
                  At.class' "text-xs text-secondary"] [
          El.span ~at:[At.class' "hash-prefix opacity-50"] [El.txt "#"];
          El.txt tag_str]
      ) tags in
      let rec intersperse_comma = function
        | [] -> [] | [x] -> [x]
        | x :: xs -> x :: El.txt " " :: intersperse_comma xs
      in
      El.span (bullet :: intersperse_comma tag_spans)
  in
  El.div ~at:[At.class' "text-sm text-secondary mt-2 flex items-center flex-wrap"] [
    link_el; El.txt " "; El.txt date_str; tag_els]

(** {1 Single Entry Rendering} *)

let render_entry ~ctx ent =
  let entry_html = match ent with
    | `Paper p -> Paper.card ~ctx p
    | `Note n -> fst (Note.brief ~ctx n)
    | `Video v -> fst (Video.brief ~ctx v)
    | `Idea i -> fst (Idea.brief ~ctx i)
    | `Project p -> fst (Project.for_feed ~ctx p)
  in
  El.div [entry_html; tags_meta ~ctx ent]

(** Render an entry for feed view. *)
let render_feed ~ctx ent =
  let entry_html = match ent with
    | `Paper p -> Paper.for_feed ~ctx p
    | `Note n -> fst (Note.for_feed ~ctx n)
    | `Video v -> fst (Video.for_feed ~ctx v)
    | `Idea i -> fst (Idea.for_feed ~ctx i)
    | `Project p -> fst (Project.for_feed ~ctx p)
  in
  El.div [entry_heading ~ctx ent; entry_html; tags_meta ~ctx ent]

(** {1 Page Functions} *)

(** Paginated entry list page content. *)
let entries_page ~ctx ~title:page_title ~types =
  let ents = get_entries ~ctx ~types in
  let ents' = if List.length ents > 25 then Common.take 25 ents else ents in
  let rendered = List.map (render_entry ~ctx) ents' in
  let rec add_separators = function
    | [] -> [] | [x] -> [x]
    | x :: xs -> x :: El.hr ~at:[At.class' "my-3 border-t"] () :: add_separators xs
  in
  let main_content = add_separators rendered in
  let types_str = String.concat "," (List.map entry_type_to_string types) in
  El.article ~at:[
    At.v "data-pagination" "true";
    At.v "data-collection-type" "entries";
    At.v "data-total-count" (string_of_int (List.length ents));
    At.v "data-current-count" (string_of_int (List.length ents'));
    At.v "data-types" types_str] [
    El.h1 ~at:[At.class' "text-2xl font-semibold mb-4"] [El.txt page_title];
    El.div main_content]

(** Chronological feed view page content. *)
let feed_page ~ctx ~title:page_title ~types =
  let feed = get_entries ~ctx ~types in
  let feed' = if List.length feed > 25 then Common.take 25 feed else feed in
  let rec intersperse_hr = function
    | [] -> [] | [x] -> [render_feed ~ctx x]
    | x :: xs -> render_feed ~ctx x :: El.hr ~at:[At.class' "my-3 border-t"] () :: intersperse_hr xs
  in
  let main_content = intersperse_hr feed' in
  let types_str = String.concat "," (List.map entry_type_to_string types) in
  El.article ~at:[
    At.v "data-pagination" "true";
    At.v "data-collection-type" "feed";
    At.v "data-total-count" (string_of_int (List.length feed));
    At.v "data-current-count" (string_of_int (List.length feed'));
    At.v "data-types" types_str] [
    El.h1 ~at:[At.class' "text-2xl font-semibold mb-4"] [El.txt page_title];
    El.div main_content]

(** HTML string fragment for pagination API (entry list). *)
let render_entries_html ~ctx ents =
  let rendered = List.map (render_entry ~ctx) ents in
  let rec add_separators = function
    | [] -> [] | [x] -> [x]
    | x :: xs -> x :: El.hr ~at:[At.class' "my-3 border-t"] () :: add_separators xs
  in
  let html_elements = El.hr ~at:[At.class' "my-3 border-t"] () :: add_separators rendered in
  El.to_string ~doctype:false (El.div html_elements)

(** HTML string fragment for pagination API (feed view). *)
let render_feeds_html ~ctx feeds =
  let rec intersperse_hr = function
    | [] -> [] | [x] -> [render_feed ~ctx x]
    | x :: xs -> render_feed ~ctx x :: El.hr ~at:[At.class' "my-3 border-t"] () :: intersperse_hr xs
  in
  let html_elements = El.hr ~at:[At.class' "my-3 border-t"] () :: intersperse_hr feeds in
  El.to_string ~doctype:false (El.div html_elements)
