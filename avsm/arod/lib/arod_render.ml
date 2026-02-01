(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Consolidated content rendering for Arod webserver.

    All rendering functions take a context parameter instead of using global state. *)

open Htmlit
open Printf

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

(** {1 Utilities} *)

let take n l =
  let[@tail_mod_cons] rec aux n l =
    match n, l with
    | 0, _ | _, [] -> []
    | n, x::l -> x :: aux (n - 1) l
  in
  if n < 0 then invalid_arg "take"; aux n l

let map_and fn l =
  let ll = List.length l in
  List.mapi (fun i v ->
    match i with
    | 0 -> fn v
    | _ when i + 1 = ll -> " and " ^ (fn v)
    | _ -> ", " ^ (fn v)
  ) l |> String.concat ""

(** {1 Date Formatting} *)

let month_name = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr"
  | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug"
  | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _ -> ""

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
  let ms = month_name m in
  match with_d with
  | false -> sprintf "%s %4d" ms y
  | true -> sprintf "%s %s %4d" (int_to_date_suffix ~r d) ms y

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

let get_entries ~(ctx : Arod_ctx.t) ~types =
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
  Arod_ctx.all_entries ctx
  |> List.filter (fun ent -> select ent && filterent ent)
  |> List.sort Entry.compare
  |> List.rev

let perma_entries ~(ctx : Arod_ctx.t) =
  Arod_ctx.all_entries ctx
  |> List.filter (function `Note n -> Note.perma n | _ -> false)
  |> List.sort Entry.compare
  |> List.rev

(** {1 Markdown Rendering} *)

let md_to_html ~ctx content = Arod_md.to_html ~ctx content

(** {1 Image Rendering} *)

let img ~ctx:_ ?cl ?(alt="") ?(title="") img_ent =
  let origin_url = sprintf "/images/%s.webp"
    (Filename.chop_extension (Img.origin img_ent)) in
  let srcsets = String.concat ","
    (List.map (fun (f,(w,_h)) -> sprintf "/images/%s %dw" f w)
      (Img.MS.bindings img_ent.Img.variants)) in
  let base_attrs = [
    At.v "loading" "lazy"; At.src origin_url;
    At.v "srcset" srcsets; At.v "sizes" "(max-width: 768px) 100vw, 33vw"
  ] in
  let attrs = match cl with Some c -> At.class' c :: base_attrs | None -> base_attrs in
  match alt with
  | "%r" -> El.figure ~at:[At.class' "image-right"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]]
  | "%c" -> El.figure ~at:[At.class' "image-center"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]]
  | "%lc" -> El.figure ~at:[At.class' "image-left-float"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]]
  | "%rc" -> El.figure ~at:[At.class' "image-right-float"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]]
  | _ -> El.img ~at:(At.alt alt :: At.title title :: attrs) ()

(** {1 Body Rendering} *)

let truncated_body ~ctx ent =
  let body = Entry.body ent in
  let first, last = Bushel.Util.first_and_last_hunks body in
  let remaining_words = Bushel.Util.count_words last in
  let total_words = Bushel.Util.count_words first + remaining_words in
  let is_note = match ent with `Note _ -> true | _ -> false in
  let is_truncated = remaining_words > 1 in
  let word_count_info =
    if is_truncated || (is_note && total_words > 0) then
      Some (total_words, is_truncated)
    else None
  in
  let markdown_with_link =
    let footnote_lines = Bushel.Util.find_footnote_lines last in
    let footnotes_text =
      if footnote_lines = [] then ""
      else "\n\n" ^ String.concat "\n" footnote_lines
    in
    match word_count_info with
    | Some (total, true) ->
      let url = Entry.site_url ent in
      first ^ "\n\n*[Read full note... (" ^ string_of_int total ^ " words](" ^ url ^ "))*\n" ^ footnotes_text
    | _ -> first ^ footnotes_text
  in
  (El.unsafe_raw (md_to_html ~ctx markdown_with_link), word_count_info)

let full_body ~ctx ent =
  El.unsafe_raw (md_to_html ~ctx (Entry.body ent))

(** {1 Entry Heading} *)

let entry_href ~ctx:_ ?title ?(tag="h2") ent =
  let via, via_url = match ent with
    | `Note n -> (match n.Note.via with None -> None, None | Some (t,u) -> Some t, Some u)
    | _ -> None, None
  in
  let via_el = match via, via_url with
    | Some t, Some u when t <> "" ->
      El.a ~at:[At.class' "via"; At.href u] [El.txt (sprintf "(via %s)" t)]
    | _, Some u -> El.a ~at:[At.class' "via"; At.href u] [El.txt "(via)"]
    | _ -> El.splice []
  in
  let title_text = match title with None -> Entry.title ent | Some t -> t in
  match ent with
  | `Note {index_page=true;_} -> El.splice []
  | _ ->
    let h_fn = match tag with
      | "h1" -> El.h1 | "h2" -> El.h2 | "h3" -> El.h3
      | "h4" -> El.h4 | "h5" -> El.h5 | "h6" -> El.h6 | _ -> El.h2
    in
    let doi_el = match ent with
      | `Note n when Note.perma n ->
        (match Note.doi n with
         | Some doi_str -> El.span ~at:[At.class' "title-doi"] [
             El.txt " / "; El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt "DOI"]]
         | None -> El.splice [])
      | _ -> El.splice []
    in
    h_fn [
      El.a ~at:[At.href (Entry.site_url ent)] [El.txt title_text];
      El.txt " "; via_el;
      El.span ~at:[At.class' "title-date"] [
        El.txt " / "; El.txt (ptime_date ~with_d:false (Entry.date ent))];
      doi_el]

(** {1 Tags Metadata} *)

let tags_meta ~ctx ?extra ?link ?date ?backlinks_content ent =
  let link_el = match link with
    | None -> El.a ~at:[At.href (Entry.site_url ent)] [El.txt "#"]
    | Some l -> El.a ~at:[At.href l] [El.txt "#"]
  in
  let date_str = ptime_date ~with_d:true
    (match date with None -> Entry.date ent | Some d -> d) in
  let bullet = El.span ~at:[At.class' "meta-bullet"] [El.txt "•"] in
  let sections = [[link_el; El.txt " "; El.txt date_str]] in
  let sections = match ent with
    | `Note n when Note.perma n ->
      (match Note.doi n with
       | Some doi_str -> sections @ [[El.txt "DOI: ";
           El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt doi_str]]]
       | None -> sections)
    | _ -> sections
  in
  let sections = match extra with Some v -> sections @ [[El.txt v]] | None -> sections in
  let sections = match backlinks_content with
    | Some content ->
      let entry_slug = Entry.slug ent in
      let checkbox_id = "sidenote__checkbox--backlinks-" ^ entry_slug in
      let content_id = "sidenote-backlinks-" ^ entry_slug in
      sections @ [[El.span ~at:[At.class' "sidenote"; At.v "role" "note"] [
        El.input ~at:[At.type' "checkbox"; At.id checkbox_id; At.class' "sidenote__checkbox";
          At.v "aria-label" "Show backlinks"; At.v "aria-hidden" "true"; At.v "hidden" ""] ();
        El.label ~at:[At.v "for" checkbox_id; At.class' "sidenote__button";
          At.v "data-sidenote-number" "↑"; At.v "aria-describedby" content_id; At.v "tabindex" "0"]
          [El.txt "backlinks"];
        El.span ~at:[At.id content_id; At.class' "sidenote__content"; At.v "aria-hidden" "true";
          At.v "hidden" ""; At.v "data-sidenote-number" "↑"] [content]]]]
    | None -> sections
  in
  let all_tags = Arod_ctx.tags_of_ent ctx ent in
  let sections = match all_tags with
    | [] -> sections
    | tags ->
      let tag_elements = List.map (fun tag ->
        let tag_str = Tags.to_raw_string tag in
        El.span ~at:[At.v "data-tag" tag_str; At.class' "tag-label"] [El.txt tag_str]
      ) tags in
      let tags_section = List.fold_left (fun acc el ->
        if acc = [] then [el] else acc @ [El.txt ", "; el]
      ) [] tag_elements in
      sections @ [tags_section]
  in
  let meta_parts = List.fold_left (fun acc section ->
    if acc = [] then section else acc @ [bullet] @ section
  ) [] sections in
  El.div ~at:[At.class' "note-meta"] meta_parts

(** {1 Paper Rendering} *)

module Paper_render = struct
  let author_name name = El.span ~at:[At.style "text-wrap:nowrap"] [El.txt name]

  let one_author ~ctx author_name_str =
    match Arod_ctx.lookup_by_name ctx author_name_str with
    | None -> El.span ~at:[At.class' "author"] [author_name author_name_str]
    | Some contact ->
      let name = Contact.name contact in
      match Contact.best_url contact with
      | None -> El.span ~at:[At.class' "author"] [author_name name]
      | Some url -> El.a ~at:[At.href url] [author_name name]

  let authors ~ctx p =
    let author_names = Paper.authors p in
    let author_els = List.map (one_author ~ctx) author_names in
    match author_els with
    | [] -> El.splice []
    | [a] -> a
    | els ->
      let rec make_list = function
        | [] -> [] | [x] -> [El.txt " and "; x]
        | x :: xs -> x :: El.txt ", " :: make_list xs
      in El.splice (make_list els)

  let publisher p =
    let bibty = Paper.bibtype p in
    let ourl l = function None -> l | Some u -> sprintf {|<a href="%s">%s</a>|} u l in
    let string_of_vol_issue p =
      match (Paper.volume p), (Paper.number p) with
      | Some v, Some n -> sprintf " (vol %s issue %s)" v n
      | Some v, None -> sprintf " (vol %s)" v
      | None, Some n -> sprintf " (issue %s)" n
      | _ -> ""
    in
    let result = match String.lowercase_ascii bibty with
      | "misc" -> sprintf {|Working paper at %s|} (ourl (Paper.publisher p) (Paper.url p))
      | "inproceedings" -> sprintf {|Paper in the %s|} (ourl (Paper.booktitle p) (Paper.url p))
      | "proceedings" -> sprintf {|%s|} (ourl (Paper.title p) (Paper.url p))
      | "abstract" -> sprintf {|Abstract in the %s|} (ourl (Paper.booktitle p) (Paper.url p))
      | "article" | "journal" ->
        sprintf {|Journal paper in %s%s|} (ourl (Paper.journal p) (Paper.url p)) (string_of_vol_issue p)
      | "book" -> sprintf {|Book published by %s|} (ourl (Paper.publisher p) (Paper.url p))
      | "techreport" ->
        sprintf {|Technical report%s at %s|}
          (match Paper.number p with None -> "" | Some n -> " (" ^ n ^ ")")
          (ourl (Paper.institution p) (Paper.url p))
      | _ -> sprintf {|Publication in %s|} (ourl (Paper.publisher p) (Paper.url p))
    in El.unsafe_raw result

  let host_without_www u =
    match Uri.host (Uri.of_string u) with
    | None -> ""
    | Some h -> if String.starts_with ~prefix:"www." h then String.sub h 4 (String.length h - 4) else h

  let bar ~ctx ?(nopdf=false) p =
    let cfg = Arod_ctx.config ctx in
    let pdf =
      let pdf_path = Filename.concat cfg.paths.static_dir (sprintf "papers/%s.pdf" (Paper.slug p)) in
      if Sys.file_exists pdf_path && not nopdf then
        Some (El.a ~at:[At.href (sprintf "/papers/%s.pdf" (Paper.slug p))] [
          El.span ~at:[At.class' "nobreak"] [
            El.txt "PDF"; El.img ~at:[At.class' "inline-icon"; At.alt "pdf"; At.src "/assets/pdf.svg"] ()]])
      else None
    in
    let bib = if nopdf then None
      else Some (El.a ~at:[At.href (sprintf "/papers/%s.bib" (Paper.slug p))] [El.txt "BIB"]) in
    let url = match Paper.url p with
      | None -> None
      | Some u -> Some (El.splice [El.a ~at:[At.href u] [El.txt "URL"]; El.txt " ";
          El.unsafe_raw (sprintf {|<i style="color: #666666">(%s)</i>|} (host_without_www u))])
    in
    let doi = match Paper.doi p with
      | None -> None | Some d -> Some (El.a ~at:[At.href ("https://doi.org/" ^ d)] [El.txt "DOI"])
    in
    let bits = [url; doi; bib; pdf] |> List.filter_map Fun.id in
    El.splice ~sep:(El.unsafe_raw " &nbsp; ") bits

  let for_feed ~ctx p =
    let title_el = El.p ~at:[At.class' "paper-title"] [
      El.a ~at:[At.href (Entry.site_url (`Paper p))] [El.txt (Paper.title p)]] in
    (El.blockquote ~at:[At.class' "paper noquote"] [
      El.div ~at:[At.class' "paper-info"] [
        title_el; El.p [authors ~ctx p; El.txt "."];
        El.p [publisher p; El.txt "."]; El.p [bar ~ctx p]]], None)

  let for_entry ~ctx ?nopdf p =
    (El.div ~at:[At.class' "paper"] [
      El.div ~at:[At.class' "paper-info"] [
        El.p ~at:[At.class' "paper-title"] [
          El.a ~at:[At.href (Entry.site_url (`Paper p))] [El.txt (Paper.title p)]];
        El.p [authors ~ctx p; El.txt "."]; El.p [publisher p; El.txt "."];
        El.p [bar ~ctx ?nopdf p]]], None)

  let extra ~ctx p =
    let entries = Arod_ctx.entries ctx in
    let all = Entry.old_papers entries |> List.filter (fun op -> Paper.slug op = Paper.slug p) in
    match all with
    | [] -> El.splice []
    | all ->
      let older_versions = List.map (fun op ->
        let (paper_html, _) = for_entry ~ctx ~nopdf:true op in
        El.splice [
          El.hr ();
          El.p [El.txt ("This is " ^ op.Paper.ver ^ " of the publication from " ^
                        ptime_date ~with_d:false (Paper.date op) ^ ".")];
          El.blockquote ~at:[At.class' "noquote"] [paper_html];
          tags_meta ~ctx (`Paper op)]
      ) all in
      El.splice [
        El.h1 [El.txt "Older versions"];
        El.p [El.txt "There are earlier revisions of this paper available below for historical reasons. ";
              El.txt "Please cite the latest version of the paper above instead of these."];
        El.splice older_versions]

  let full ~ctx p =
    let img_el = match Arod_ctx.lookup_image ctx (Paper.slug p) with
      | Some img_ent -> El.p [El.a ~at:[At.href (Option.value ~default:"#" (Paper.best_url p))] [
          img ~ctx ~cl:"image-center" img_ent]]
      | None -> El.splice []
    in
    let abstract_html =
      let abstract = Paper.abstract p in
      if abstract <> "" then El.p [El.unsafe_raw (md_to_html ~ctx abstract)] else El.splice []
    in
    El.div ~at:[At.class' "paper"] [
      El.div ~at:[At.class' "paper-info"] [
        El.h2 [El.txt (Paper.title p)];
        El.p [authors ~ctx p; El.txt "."]; El.p [publisher p; El.txt "."];
        El.p [bar ~ctx p]];
      img_el; abstract_html]
end

(** {1 Note Rendering} *)

module Note_render = struct
  let for_feed ~ctx n = truncated_body ~ctx (`Note n)

  let brief ~ctx n =
    let (body_html, word_count_info) = truncated_body ~ctx (`Note n) in
    (El.splice [entry_href ~ctx (`Note n); body_html], word_count_info)

  let full ~ctx n =
    let body = Note.body n in
    let body_with_ref = match Note.slug_ent n with
      | None -> body
      | Some slug_ent ->
        let parent_ent = Arod_ctx.lookup_exn ctx slug_ent in
        let parent_title = Entry.title parent_ent in
        body ^ "\n\nRead more about [" ^ parent_title ^ "](:" ^ slug_ent ^ ")."
    in
    El.div ~at:[At.class' "note"] [El.unsafe_raw (md_to_html ~ctx body_with_ref)]

  let references_html ~ctx note =
    let is_perma = Note.perma note in
    let has_doi = match Note.doi note with Some _ -> true | None -> false in
    if not (is_perma || has_doi) then El.splice []
    else
      let cfg = Arod_ctx.config ctx in
      let me = Arod_ctx.lookup_by_handle ctx cfg.site.author_handle in
      match me with
      | None -> El.splice []
      | Some author_contact ->
        let entries = Arod_ctx.entries ctx in
        let references = Bushel.Md.note_references entries author_contact note in
        if List.length references > 0 then
          let ref_items = List.map (fun (doi, citation, _is_paper) ->
            let doi_url = sprintf "https://doi.org/%s" doi in
            El.li [El.txt citation; El.a ~at:[At.href doi_url; At.v "target" "_blank"] [El.i [El.txt doi]]]
          ) references in
          El.div ~at:[At.class' "references-section"] [
            El.h3 ~at:[At.class' "references-heading"] [El.txt "References"];
            El.ul ~at:[At.class' "references-list"] ref_items]
        else El.splice []
end

(** {1 Video Rendering} *)

module Video_render = struct
  let for_feed ~ctx v =
    let md = sprintf "![%%c](:%s)\n\n" v.Video.slug in
    (El.unsafe_raw (md_to_html ~ctx md), None)

  let brief ~ctx v =
    let md = sprintf "![%%c](:%s)\n\n%s" v.Video.slug v.Video.description in
    (El.splice [entry_href ~ctx (`Video v); El.unsafe_raw (md_to_html ~ctx md)], None)

  let full ~ctx v = fst (brief ~ctx v)
end

(** {1 Idea Rendering} *)

module Idea_render = struct
  let status_to_long_string s = function
    | Idea.Available -> sprintf {|is <span class="idea-available">available</span> for being worked on|}
    | Discussion -> sprintf {|is <span class="idea-discussion">under discussion</span> with a student but not yet confirmed|}
    | Ongoing -> sprintf {|is currently <span class="idea-ongoing">being worked on</span> by %s|} s
    | Completed -> sprintf {|has been <span class="idea-completed">completed</span> by %s|} s
    | Expired -> sprintf {|has <span class="idea-expired">expired</span>|}

  let level_to_long_string = function
    | Idea.Any -> " as a good starter project"
    | PartII -> " as a Cambridge Computer Science Part II project"
    | MPhil -> " as a Cambridge Computer Science Part III or MPhil project"
    | PhD -> " as a Cambridge Computer Science PhD topic"
    | Postdoc -> " as a postdoctoral project"

  let sups_for i =
    let v = match Idea.status i with Completed -> "was" | Ongoing -> "is" | _ -> "may be" in
    let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisors in
    match sups with
    | [] -> ""
    | s -> " It " ^ v ^ " co-supervised with " ^ (map_and (sprintf "[@%s]") s) ^ "."

  let render_contacts ~ctx contacts =
    match contacts with
    | [] -> El.splice []
    | cs ->
      let contact_links = List.filter_map (fun handle ->
        match Arod_ctx.lookup_by_handle ctx handle with
        | Some contact ->
          let name = Contact.name contact in
          (match Contact.best_url contact with
           | Some url -> Some (El.a ~at:[At.href url] [El.txt name])
           | None -> Some (El.txt name))
        | None -> Some (El.txt ("@" ^ handle))
      ) cs in
      let rec intersperse_and = function
        | [] -> [] | [x] -> [x] | [x; y] -> [x; El.txt " and "; y]
        | x :: xs -> x :: El.txt ", " :: intersperse_and xs
      in El.splice (intersperse_and contact_links)

  let to_html_no_sidenotes ~ctx idea =
    let idea_url = "/ideas/" ^ idea.Idea.slug in
    let sups = List.filter (fun x -> x <> "avsm") idea.Idea.supervisors in
    let sups_el = match sups with
      | [] -> El.splice []
      | _ -> El.splice [El.txt " and cosupervised with "; render_contacts ~ctx sups]
    in
    let studs_el = match idea.Idea.students with
      | [] -> El.splice []
      | _ -> El.splice [render_contacts ~ctx idea.Idea.students]
    in
    let lev = match idea.Idea.level with
      | Any -> "" | PartII -> " (Part II)" | MPhil -> " (MPhil)" | PhD -> " (PhD)" | Postdoc -> ""
    in
    match idea.Idea.status with
    | Available -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (Idea.title idea)]; El.txt " "; El.br ();
        El.span ~at:[At.class' "idea-available"] [El.txt ("Available" ^ lev)]; El.txt " "; sups_el]
    | Discussion -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (Idea.title idea)]; El.txt " "; El.br ();
        El.span ~at:[At.class' "idea-discussion"] [El.txt ("Under discussion" ^ lev)]; El.txt " "; sups_el]
    | Ongoing -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (Idea.title idea)]; El.txt " "; El.br ();
        El.span ~at:[At.class' "idea-ongoing"] [El.txt ("Currently ongoing" ^ lev)];
        El.txt " with "; studs_el; El.txt " "; sups_el]
    | Completed -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (Idea.title idea)]; El.txt " "; El.br ();
        El.span ~at:[At.class' "idea-completed"] [El.txt ("Completed" ^ lev)];
        El.txt " by "; studs_el; El.txt " "; sups_el;
        El.txt (" in " ^ string_of_int idea.Idea.year)]
    | Expired -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (Idea.title idea)]; El.txt " "; El.br ();
        El.span ~at:[At.class' "idea-expired"] [El.txt ("Expired" ^ lev)]; El.txt " "; sups_el]

  let for_feed ~ctx i =
    let studs = map_and (sprintf "[@%s]") (Idea.students i) in
    let r = sprintf "This is an idea proposed %s, and %s.%s"
      (level_to_long_string @@ Idea.level i) (status_to_long_string studs (Idea.status i)) (sups_for i) in
    let (body_html, word_count_info) = truncated_body ~ctx (`Idea i) in
    (El.splice [El.unsafe_raw (md_to_html ~ctx r); body_html], word_count_info)

  let brief ~ctx i =
    let studs = map_and (sprintf "[@%s]") (Idea.students i) in
    let r = sprintf "This is an idea proposed in %d%s, and %s.%s"
      (Idea.year i) (level_to_long_string @@ Idea.level i) (status_to_long_string studs (Idea.status i)) (sups_for i) in
    let (body_html, word_count_info) = truncated_body ~ctx (`Idea i) in
    (El.splice [
      entry_href ~ctx (`Idea i);
      El.div ~at:[At.class' "idea"] [El.unsafe_raw (md_to_html ~ctx r); body_html]
    ], word_count_info)

  let full ~ctx i =
    let studs = map_and (sprintf "[@%s]") (Idea.students i) in
    let r = sprintf "# %s\n\nThis is an idea proposed in %d%s, and %s.%s\n\n%s"
      (Idea.title i) (Idea.year i) (level_to_long_string @@ Idea.level i)
      (status_to_long_string studs (Idea.status i)) (sups_for i) (Idea.body i) in
    El.div ~at:[At.class' "idea"] [El.unsafe_raw (md_to_html ~ctx r)]
end

(** {1 Project Rendering} *)

module Project_render = struct
  module StringSet = Set.Make(String)

  let ideas_for_project ~ctx project =
    List.filter (fun i -> Idea.project i = project.Project.slug) (Arod_ctx.ideas ctx)

  let for_feed ~ctx p =
    let (body_html, word_count_info) = truncated_body ~ctx (`Project p) in
    (El.div [body_html], word_count_info)

  let brief ~ctx p =
    let idea_items = ideas_for_project ~ctx p
      |> List.sort Idea.compare
      |> List.map (fun i -> El.li [Idea_render.to_html_no_sidenotes ~ctx i]) in
    let (body_html, word_count_info) = truncated_body ~ctx (`Project p) in
    (El.splice [entry_href ~ctx (`Project p); body_html; El.ul idea_items], word_count_info)

  let full ~ctx p =
    let entries = Arod_ctx.entries ctx in
    let project_slug = p.Project.slug in
    let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
    let backlink_set = List.fold_left (fun acc slug -> StringSet.add slug acc) StringSet.empty backlink_slugs in
    let all_entries = Arod_ctx.all_entries ctx in

    let project_papers = List.filter (fun e ->
      match e with `Paper paper -> List.mem project_slug (Paper.project_slugs paper) | _ -> false
    ) all_entries |> List.sort (fun a b -> compare (Entry.date b) (Entry.date a)) in

    let recent_activity = List.filter (fun e ->
      match e with `Paper _ -> false | _ -> StringSet.mem (Entry.slug e) backlink_set
    ) all_entries |> List.sort (fun a b -> compare (Entry.date b) (Entry.date a)) in

    let activity_section =
      if recent_activity = [] then El.splice []
      else
        let activity_items = List.map (fun ent ->
          let icon_name = ent_to_icon ent in
          let date_str = ptime_date ~with_d:false (Entry.date ent) in
          let lookup_title slug = match Entry.lookup entries slug with
            | Some ent -> Some (Entry.title ent) | None -> None in
          let description = match ent with
            | `Paper paper -> Bushel.Description.paper_description paper ~date_str
            | `Note n -> Bushel.Description.note_description n ~date_str ~lookup_fn:lookup_title
            | `Idea i -> Bushel.Description.idea_description i ~date_str
            | `Video v -> Bushel.Description.video_description v ~date_str ~lookup_fn:lookup_title
            | `Project pr -> Bushel.Description.project_description pr
          in
          El.li [
            El.img ~at:[At.alt "icon"; At.class' "inline-icon"; At.src (sprintf "/assets/%s" icon_name)] ();
            El.a ~at:[At.href (Entry.site_url ent)] [El.txt (Entry.title ent)];
            El.txt " – "; El.span ~at:[At.class' "activity-description"] [El.txt description]]
        ) recent_activity in
        El.splice [El.h1 [El.txt "Activity"]; El.ul ~at:[At.class' "activity-list"] activity_items]
    in

    let references_section =
      if project_papers = [] then El.splice []
      else
        let paper_items = List.map (fun ent ->
          match ent with `Paper paper -> fst (Paper_render.for_entry ~ctx paper) | _ -> El.splice []
        ) project_papers in
        El.splice [El.h1 [El.txt "References"]; El.splice paper_items]
    in

    El.div ~at:[At.class' "project"] [
      El.h1 [El.txt (Project.title p)];
      El.p [full_body ~ctx (`Project p)];
      activity_section; references_section]
end

(** {1 Backlinks} *)

let render_backlinks_content ~ctx ent =
  let slug = Entry.slug ent in
  let entry_type = match ent with
    | `Paper _ -> "paper" | `Note _ -> "note" | `Idea _ -> "idea"
    | `Project _ -> "project" | `Video _ -> "video"
  in
  let entries = Arod_ctx.entries ctx in
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug slug in
  if backlink_slugs = [] then None
  else
    let backlink_items = List.filter_map (fun backlink_slug ->
      match Entry.lookup entries backlink_slug with
      | Some entry ->
        let title = Entry.title entry in
        let url = Entry.site_url entry in
        Some (El.li [El.a ~at:[At.href url] [El.txt title]])
      | None -> None
    ) backlink_slugs in
    if backlink_items = [] then None
    else Some (El.splice [
      El.span ~at:[At.class' "sidenote-number"] [El.txt "↑"];
      El.span ~at:[At.class' "sidenote-icon"] [El.txt ""];
      El.txt (sprintf "The following entries link to this %s: " entry_type);
      El.ul backlink_items])

(** {1 Entry Dispatch} *)

let render_entry ~ctx ent =
  let (t, _) = match ent with
    | `Paper p -> Paper_render.for_entry ~ctx p
    | `Note n -> Note_render.brief ~ctx n
    | `Video v -> Video_render.brief ~ctx v
    | `Idea i -> Idea_render.brief ~ctx i
    | `Project p -> Project_render.brief ~ctx p
  in El.splice [t; tags_meta ~ctx ent]

let render_entry_for_feed ~ctx ent =
  match ent with
  | `Paper p -> fst (Paper_render.for_feed ~ctx p)
  | `Note n -> fst (Note_render.for_feed ~ctx n)
  | `Video v -> fst (Video_render.for_feed ~ctx v)
  | `Idea i -> fst (Idea_render.for_feed ~ctx i)
  | `Project p -> fst (Project_render.for_feed ~ctx p)

let render_feed ~ctx ent =
  let (entry_html, _) = match ent with
    | `Paper p -> Paper_render.for_feed ~ctx p
    | `Note n -> Note_render.for_feed ~ctx n
    | `Video v -> Video_render.for_feed ~ctx v
    | `Idea i -> Idea_render.for_feed ~ctx i
    | `Project p -> Project_render.for_feed ~ctx p
  in El.splice [entry_href ~ctx ent; entry_html; tags_meta ~ctx ent]

let render_one_entry ~ctx ent =
  match ent with
  | `Paper p -> Paper_render.full ~ctx p, Paper_render.extra ~ctx p
  | `Idea i -> Idea_render.full ~ctx i, El.splice []
  | `Note n -> Note_render.full ~ctx n, El.splice []
  | `Video v -> Video_render.full ~ctx v, El.splice []
  | `Project p -> Project_render.full ~ctx p, El.splice []

let sort_of_ent ent =
  match ent with
  | `Paper p -> (match Paper.bibtype p with
    | "inproceedings" -> "conference paper" | "article" | "journal" -> "journal paper"
    | "misc" -> "preprint" | "techreport" -> "technical report" | _ -> "paper"), ""
  | `Note {Note.updated=Some _;date=u; _} ->
    "note", sprintf " (originally on %s)" (ptime_date ~with_d:true u)
  | `Note _ -> "note", "" | `Project _ -> "project", ""
  | `Idea _ -> "research idea", "" | `Video _ -> "video", ""

(** {1 List Rendering} *)

let render_entries_html ~ctx ents =
  let rendered = List.map (render_entry ~ctx) ents in
  let rec add_separators = function
    | [] -> [] | [x] -> [x] | x :: xs -> x :: El.hr () :: add_separators xs
  in
  let html_elements = El.hr () :: add_separators rendered in
  El.to_string ~doctype:false (El.splice html_elements)

let render_feeds_html ~ctx feeds =
  let rec intersperse_hr = function
    | [] -> [] | [x] -> [render_feed ~ctx x]
    | x::xs -> render_feed ~ctx x :: El.hr () :: intersperse_hr xs
  in
  let html_elements = El.hr () :: intersperse_hr feeds in
  El.to_string ~doctype:false (El.splice html_elements)

(** {1 Page Rendering} *)

let footer = Arod_footer.footer

let view_news ~ctx ~types =
  let feed = get_entries ~ctx ~types in
  let feed' = if List.length feed > 25 then take 25 feed else feed in
  let title = "News" in
  let description = sprintf "Showing %d news item(s)" (List.length feed') in
  let main_content =
    let rec intersperse_hr = function
      | [] -> [] | [x] -> [render_feed ~ctx x]
      | x::xs -> render_feed ~ctx x :: El.hr () :: intersperse_hr xs
    in intersperse_hr feed' in
  let page_footer = El.splice [footer] in
  let pagination_attrs =
    let types_str = String.concat "," (List.map entry_type_to_string types) in
    [At.v "data-pagination" "true"; At.v "data-collection-type" "feed";
     At.v "data-total-count" (string_of_int (List.length feed));
     At.v "data-current-count" (string_of_int (List.length feed'));
     At.v "data-types" types_str]
  in
  let page_content = El.splice [El.article ~at:pagination_attrs main_content; El.aside []] in
  Arod_page.page ~ctx ~title ~page_content ~page_footer ~description ()

let view_entries ~ctx ~types =
  let ents = get_entries ~ctx ~types in
  let ents' = if List.length ents > 25 then take 25 ents else ents in
  let title = "Entries" in
  let description = sprintf "Showing %d item(s)" (List.length ents') in
  let main_content =
    let rendered = List.map (render_entry ~ctx) ents' in
    let rec add_separators = function
      | [] -> [] | [x] -> [x] | x :: xs -> x :: El.hr () :: add_separators xs
    in add_separators rendered in
  let page_footer = El.splice [footer] in
  let pagination_attrs =
    let types_str = String.concat "," (List.map entry_type_to_string types) in
    [At.v "data-pagination" "true"; At.v "data-collection-type" "entries";
     At.v "data-total-count" (string_of_int (List.length ents));
     At.v "data-current-count" (string_of_int (List.length ents'));
     At.v "data-types" types_str]
  in
  let page_content = El.splice [El.article ~at:pagination_attrs main_content; El.aside []] in
  Arod_page.page ~ctx ~title ~page_content ~page_footer ~description ()

let view_one ~ctx ent =
  let cfg = Arod_ctx.config ctx in
  let entries = Arod_ctx.entries ctx in
  let title = Entry.title ent in
  let description = match Entry.synopsis ent with Some v -> v | None -> "" in
  let eh, extra = render_one_entry ~ctx ent in
  let is_index = Entry.is_index_entry ent in
  let standardsite = match ent with `Note n -> Note.standardsite n | _ -> None in
  let backlinks_content = if is_index then None else render_backlinks_content ~ctx ent in
  let related_container = match ent with
    | `Project _ -> El.splice []
    | _ when is_index -> El.splice []
    | `Note _ ->
      let tags = Arod_ctx.tags_of_ent ctx ent in
      let tag_strings = List.map Tags.to_raw_string tags |> String.concat " " in
      El.div ~at:[At.class' "related-items"; At.v "data-entry-title" title;
        At.v "data-entry-id" (Entry.slug ent); At.v "data-entry-tags" tag_strings] []
    | _ ->
      let tags = Arod_ctx.tags_of_ent ctx ent in
      let tag_strings = List.map Tags.to_raw_string tags |> String.concat " " in
      El.splice [El.hr (); El.div ~at:[At.class' "related-items"; At.v "data-entry-title" title;
        At.v "data-entry-id" (Entry.slug ent); At.v "data-entry-tags" tag_strings] []]
  in
  let breadcrumbs_list = ("Home", cfg.site.base_url ^ "/") :: Arod_richdata.(breadcrumb_of_ent cfg ent) in
  let bs = Arod_richdata.breadcrumbs breadcrumbs_list in
  let jsonld = bs ^ (Arod_richdata.json_of_entry ~ctx cfg ent) in
  let image = match Entry.thumbnail entries ent with
    | Some thumb -> cfg.site.base_url ^ thumb
    | None -> cfg.site.base_url ^ "/assets/imagetitle-default.jpg"
  in
  let page_footer, page_content =
    if is_index then
      footer, El.splice [El.article [eh]; El.aside []]
    else
      let references_html = match ent with
        | `Note n -> El.splice [El.hr (); Note_render.references_html ~ctx n]
        | _ -> El.splice []
      in
      footer, El.splice [
        El.article [eh; tags_meta ~ctx ?backlinks_content ent; references_html; related_container; extra];
        El.aside []]
  in
  Arod_page.page ~ctx ~image ~title ~jsonld ?standardsite ~page_content ~page_footer ~description ()

(** {1 Special Views} *)

let view_ideas_by_project ~ctx =
  let all_ideas = Arod_ctx.ideas ctx in
  let all_projects = Arod_ctx.projects ctx |> List.sort Project.compare |> List.rev in

  let ideas_by_project = Hashtbl.create 32 in
  List.iter (fun i ->
    let proj_slug = Idea.project i in
    let existing = try Hashtbl.find ideas_by_project proj_slug with Not_found -> [] in
    Hashtbl.replace ideas_by_project proj_slug (i :: existing)
  ) all_ideas;

  Hashtbl.iter (fun proj_slug ideas ->
    Hashtbl.replace ideas_by_project proj_slug (List.sort Idea.compare ideas)
  ) ideas_by_project;

  let project_sections = List.filter_map (fun p ->
    let proj_slug = p.Project.slug in
    match Hashtbl.find_opt ideas_by_project proj_slug with
    | None -> None
    | Some ideas ->
      let idea_items = List.map (fun i ->
        El.li ~at:[At.class' "idea-item"; At.v "data-status" (Idea.status_to_string (Idea.status i))] [
          Idea_render.to_html_no_sidenotes ~ctx i]
      ) ideas in
      let thumbnail_md = sprintf "![%%lc](:project-%s \"%s\")" proj_slug p.Project.title in
      let thumbnail_html = El.unsafe_raw (md_to_html ~ctx thumbnail_md) in
      Some (El.div ~at:[At.class' "project-section"] [
        El.h2 [El.a ~at:[At.href ("/projects/" ^ proj_slug)] [El.txt p.Project.title]];
        thumbnail_html;
        El.p [fst (truncated_body ~ctx (`Project p))];
        El.ul ~at:[At.class' "ideas-list"] idea_items])
  ) all_projects in

  let status_filter = El.div ~at:[At.class' "status-filter"] [
    El.h3 [El.txt "Filter by status:"];
    El.label [El.input ~at:[At.type' "checkbox"; At.id "filter-available"; At.checked;
      At.class' "status-checkbox"; At.v "data-status" "Available"] ();
      El.span ~at:[At.class' "status-label idea-available"] [El.txt "Available"]];
    El.label [El.input ~at:[At.type' "checkbox"; At.id "filter-discussion"; At.checked;
      At.class' "status-checkbox"; At.v "data-status" "Discussion"] ();
      El.span ~at:[At.class' "status-label idea-discussion"] [El.txt "Discussion"]];
    El.label [El.input ~at:[At.type' "checkbox"; At.id "filter-ongoing"; At.checked;
      At.class' "status-checkbox"; At.v "data-status" "Ongoing"] ();
      El.span ~at:[At.class' "status-label idea-ongoing"] [El.txt "Ongoing"]];
    El.label [El.input ~at:[At.type' "checkbox"; At.id "filter-completed"; At.checked;
      At.class' "status-checkbox"; At.v "data-status" "Completed"] ();
      El.span ~at:[At.class' "status-label idea-completed"] [El.txt "Completed"]];
    El.label [El.input ~at:[At.type' "checkbox"; At.id "filter-expired";
      At.class' "status-checkbox"; At.v "data-status" "Expired"] ();
      El.span ~at:[At.class' "status-label idea-expired"] [El.txt "Expired"]]] in

  let title = "Research Ideas" in
  let description = "Research ideas grouped by project" in
  let intro = El.p [El.txt "These are research ideas for students at various levels (Part II, MPhil, PhD, and postdoctoral). Browse through the ideas below to find projects that interest you. You're also welcome to propose your own research ideas that align with our ongoing projects."] in
  let page_footer = footer in
  let page_content = El.splice [
    El.article [El.h1 [El.txt title]; intro; El.splice project_sections];
    El.aside [status_filter]] in
  Arod_page.page ~ctx ~title ~page_content ~page_footer ~description ()

let view_projects_timeline ~ctx =
  let all_projects = Arod_ctx.projects ctx |> List.sort Project.compare |> List.rev in

  if all_projects = [] then El.div [El.txt "No projects found"]
  else
    let current_year = let (y, _, _), _ = Ptime.to_date_time (Ptime_clock.now ()) in y in

    let project_cards = List.map (fun p ->
      let start_year = p.Project.start in
      let end_year = match p.Project.finish with Some y -> y | None -> current_year in
      let duration = end_year - start_year in
      let all_entries = Arod_ctx.all_entries ctx in
      let project_slug = p.Project.slug in

      let recent_papers = List.filter (fun e ->
        match e with `Paper paper -> List.mem project_slug (Paper.project_slugs paper) | _ -> false
      ) all_entries |> List.sort (fun a b -> compare (Entry.date b) (Entry.date a))
        |> (fun l -> if List.length l > 3 then List.filteri (fun i _ -> i < 3) l else l) in

      let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
      let backlink_set = List.fold_left (fun acc slug ->
        Project_render.StringSet.add slug acc) Project_render.StringSet.empty backlink_slugs in

      let recent_notes = List.filter (fun e ->
        match e with `Note _ -> Project_render.StringSet.mem (Entry.slug e) backlink_set | _ -> false
      ) all_entries |> List.sort (fun a b -> compare (Entry.date b) (Entry.date a))
        |> (fun l -> if List.length l > 3 then List.filteri (fun i _ -> i < 3) l else l) in

      let recent_items_display =
        let paper_items = List.map (fun ent ->
          El.li [El.a ~at:[At.href (Entry.site_url ent)] [El.txt (Entry.title ent)]]
        ) recent_papers in
        let note_items = List.map (fun ent ->
          El.li [El.a ~at:[At.href (Entry.site_url ent)] [El.txt (Entry.title ent)]]
        ) recent_notes in

        if paper_items = [] && note_items = [] then El.splice []
        else El.div ~at:[At.class' "project-recent-items"] [
          (if paper_items <> [] then
            El.div ~at:[At.class' "project-recent-column"] [
              El.h4 [El.txt "Recent papers"]; El.ul paper_items]
          else El.splice []);
          (if note_items <> [] then
            El.div ~at:[At.class' "project-recent-column"] [
              El.h4 [El.txt "Recent notes"]; El.ul note_items]
          else El.splice [])]
      in

      let thumbnail_md = sprintf "![%%lc](:project-%s \"%s\")" p.Project.slug p.Project.title in
      let thumbnail_html = El.unsafe_raw (md_to_html ~ctx thumbnail_md) in
      let date_range = match p.Project.finish with
        | Some y -> sprintf "%d–%d" start_year y | None -> sprintf "%d–present" start_year in
      let duration_height = max 40 (duration * 8) in

      El.div ~at:[At.class' "timeline-project"] [
        El.div ~at:[At.class' "timeline-marker-wrapper"] [
          El.div ~at:[At.class' "timeline-dot"] [];
          El.div ~at:[At.class' "timeline-duration"; At.v "style" (sprintf "height: %dpx" duration_height)] [];
          El.span ~at:[At.class' "timeline-year"] [El.txt (string_of_int start_year)]];
        El.div ~at:[At.class' "project-card"] [
          El.div ~at:[At.class' "project-header"] [
            El.h3 [El.a ~at:[At.href ("/projects/" ^ p.Project.slug)] [El.txt p.Project.title]];
            El.span ~at:[At.class' "project-dates"] [El.txt date_range]];
          thumbnail_html;
          El.div ~at:[At.class' "project-body"] [fst (truncated_body ~ctx (`Project p))];
          recent_items_display]]
    ) all_projects in

    let title = "Projects" in
    let description = "Research projects timeline" in
    let intro = El.p [El.txt "Research projects and relevant publications, ideas and notes."] in
    let page_footer = footer in
    let page_content = El.splice [
      El.article [El.h1 [El.txt title]; intro;
        El.div ~at:[At.class' "projects-timeline"] project_cards];
      El.aside []] in
    Arod_page.page ~ctx ~title ~page_content ~page_footer ~description ()
