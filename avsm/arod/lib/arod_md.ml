(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown rendering with Bushel extensions *)

module Img = Srcsetter

(** {1 HTML Escaping} *)

let html_escape_attr s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** {1 String Helpers} *)

let doi_to_id doi =
  let buf = Buffer.create (String.length doi + 5) in
  Buffer.add_string buf "cite-";
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then
      Buffer.add_char buf c
    else
      Buffer.add_char buf '-'
  ) doi;
  Buffer.contents buf

let string_drop_prefix ~prefix str =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  if str_len >= prefix_len && String.sub str 0 prefix_len = prefix then
    String.sub str prefix_len (str_len - prefix_len)
  else
    str

(** {1 Image Rendering} *)

(* SVG expand icon for lightbox button — arrows-pointing-out *)
let expand_icon = {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor"><path d="M13.28 7.78l3.22-3.22v2.69a.75.75 0 001.5 0v-4.5a.75.75 0 00-.75-.75h-4.5a.75.75 0 000 1.5h2.69l-3.22 3.22a.75.75 0 001.06 1.06zM2 17.25v-4.5a.75.75 0 011.5 0v2.69l3.22-3.22a.75.75 0 011.06 1.06L4.56 16.5h2.69a.75.75 0 010 1.5h-4.5a.75.75 0 01-.75-.75z"/></svg>|}

let expand_btn attrs =
  Printf.sprintf {|<span class="lightbox-expand"%s>%s</span>|} attrs expand_icon

let render_image_html ?(cl="content-image") ?link_url ~alt ~title img_ent =
  let origin_url = Printf.sprintf "/images/%s" (Img.name img_ent) in

  let srcsets = String.concat ","
    (List.map (fun (f,(w,_h)) -> Printf.sprintf "/images/%s %dw" f w)
      (Img.MS.bindings img_ent.Img.variants)) in

  (* Build JSON-encoded variant list for lightbox download links *)
  let variants_json =
    let items = List.map (fun (f,(w,h)) ->
      Printf.sprintf {|{"url":"/images/%s","w":%d,"h":%d}|} f w h
    ) (Img.MS.bindings img_ent.Img.variants) in
    "[" ^ String.concat "," items ^ "]"
  in

  let lightbox_attrs = Printf.sprintf
    {| data-lightbox="%s" data-caption="%s" data-variants='%s'|}
    (html_escape_attr origin_url) (html_escape_attr title) variants_json
  in

  match alt with
  | "%lc" | "%rc" ->
    let float_class = if alt = "%lc" then "float-left mr-3 mb-1 mt-0.5"
      else "float-right ml-3 mb-1 mt-0.5" in
    (match link_url with
     | Some url ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-full w-28 h-28 object-cover" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="112px">|}
         cl origin_url alt title srcsets
       in
       Printf.sprintf {|<figure class="float-img %s"><div class="relative"><a href="%s">%s</a>%s</div></figure>|}
         float_class (html_escape_attr url) img_html (expand_btn lightbox_attrs)
     | None ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-full w-28 h-28 object-cover lightbox-trigger cursor-pointer" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="112px"%s>|}
         cl origin_url alt title srcsets lightbox_attrs
       in
       Printf.sprintf
         {|<figure class="float-img %s">%s</figure>|}
         float_class img_html)
  | "%c" | "%r" ->
    let fig_class = if alt = "%c" then "my-8 text-center" else "my-8" in
    let img_extra = if alt = "%c" then " mx-auto" else "" in
    (match link_url with
     | Some url ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-lg%s" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="(max-width: 768px) 100vw, 768px">|}
         cl img_extra origin_url title title srcsets
       in
       Printf.sprintf {|<figure class="%s"><div class="relative inline-block w-full"><a href="%s">%s</a>%s</div><figcaption class="text-sm text-secondary mt-2 text-center">%s</figcaption></figure>|}
         fig_class (html_escape_attr url) img_html (expand_btn lightbox_attrs) title
     | None ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-lg%s lightbox-trigger" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="(max-width: 768px) 100vw, 768px"%s>|}
         cl img_extra origin_url title title srcsets lightbox_attrs
       in
       Printf.sprintf {|<figure class="%s">%s<figcaption class="text-sm text-secondary mt-2 text-center">%s</figcaption></figure>|}
         fig_class img_html title)
  | _ ->
    (match link_url with
     | Some url ->
       let img_html = Printf.sprintf
         {|<img class="%s" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="(max-width: 768px) 100vw, 768px">|}
         cl origin_url alt title srcsets
       in
       Printf.sprintf {|<span class="relative inline-block"><a href="%s">%s</a>%s</span>|}
         (html_escape_attr url) img_html (expand_btn lightbox_attrs)
     | None ->
       Printf.sprintf
         {|<img class="%s lightbox-trigger" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="(max-width: 768px) 100vw, 768px"%s>|}
         cl origin_url alt title srcsets lightbox_attrs)

let render_image_html_simple ?link_url ~cl ~alt ~title ~src () =
  match alt with
  | "%lc" | "%rc" ->
    let float_class = if alt = "%lc" then "float-left mr-3 mb-1 mt-0.5"
      else "float-right ml-3 mb-1 mt-0.5" in
    (match link_url with
     | Some url ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-full w-28 h-28 object-cover" src="%s" alt="%s" title="%s" loading="lazy" sizes="112px">|}
         cl src alt title
       in
       let lb_attrs = Printf.sprintf {| data-lightbox="%s" data-caption="%s"|} (html_escape_attr src) (html_escape_attr title) in
       Printf.sprintf {|<figure class="float-img %s"><div class="relative"><a href="%s">%s</a>%s</div></figure>|}
         float_class (html_escape_attr url) img_html (expand_btn lb_attrs)
     | None ->
       let img_html = Printf.sprintf
         {|<img class="%s rounded-full w-28 h-28 object-cover lightbox-trigger cursor-pointer" src="%s" alt="%s" title="%s" loading="lazy" sizes="112px" data-lightbox="%s" data-caption="%s">|}
         cl src alt title (html_escape_attr src) (html_escape_attr title)
       in
       Printf.sprintf {|<figure class="float-img %s">%s</figure>|}
         float_class img_html)
  | "%c" | "%r" ->
    let fig_class = if alt = "%c" then "my-8 text-center" else "my-8" in
    let img_extra = if alt = "%c" then " mx-auto" else "" in
    let img_html = Printf.sprintf
      {|<img class="%s rounded-lg%s" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 768px">|}
      cl img_extra src title title
    in
    (match link_url with
     | Some url ->
       let lb_attrs = Printf.sprintf {| data-lightbox="%s" data-caption="%s"|} (html_escape_attr src) (html_escape_attr title) in
       Printf.sprintf {|<figure class="%s"><div class="relative inline-block w-full"><a href="%s">%s</a>%s</div><figcaption class="text-sm text-secondary mt-2 text-center">%s</figcaption></figure>|}
         fig_class (html_escape_attr url) img_html (expand_btn lb_attrs) title
     | None ->
       Printf.sprintf {|<figure class="%s">%s<figcaption class="text-sm text-secondary mt-2 text-center">%s</figcaption></figure>|}
         fig_class img_html title)
  | _ ->
    (match link_url with
     | Some url ->
       let img_html = Printf.sprintf
         {|<img class="%s" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 768px">|}
         cl src alt title
       in
       let lb_attrs = Printf.sprintf {| data-lightbox="%s" data-caption="%s"|} (html_escape_attr src) (html_escape_attr title) in
       Printf.sprintf {|<span class="relative inline-block"><a href="%s">%s</a>%s</span>|}
         (html_escape_attr url) img_html (expand_btn lb_attrs)
     | None ->
       Printf.sprintf
         {|<img class="%s" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 768px">|}
         cl src alt title)

(** {1 Video Embedding} *)

let rewrite_watch_to_embed url =
  let uri = Uri.of_string url in
  let path = Uri.path uri |> String.split_on_char '/' in
  let path = List.map (function "watch" -> "embed" | v -> v) path in
  Uri.with_path uri (String.concat "/" path) |> Uri.to_string

let render_video_iframe ?(vertical=false) ~title url =
  let embed_url = rewrite_watch_to_embed url in
  if vertical then
    Printf.sprintf
      {|<div class="video-vertical"><iframe title="%s" src="%s" frameborder="0" allowfullscreen sandbox="allow-same-origin allow-scripts allow-popups allow-forms" style="aspect-ratio: 9/16; width: 100%%; height: 100%%; max-width: 325px;"></iframe></div>|}
      title embed_url
  else
    Printf.sprintf
      {|<div class="video-center"><iframe title="%s" width="100%%" height="315px" src="%s" frameborder="0" allowfullscreen sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div>|}
      title embed_url

(** {1 Sidenote Types} *)

type sidenote = {
  slug : string;
  content_html : string;
  thumb_url : string option;
}

let sidenote_div_class =
  "sidenote absolute right-0 w-full text-xs leading-snug text-gray-400 \
   border-l-2 border-gray-200 pl-2 py-0.5 transition-colors duration-200 \
   hover:border-green-500 hover:text-text sidenote-hidden"

(** {1 Sidenote Rendering} *)

let sidenote_seen sidenotes slug =
  List.exists (fun sn -> sn.slug = slug) !sidenotes

let add_sidenote sidenotes sn =
  if not (sidenote_seen sidenotes sn.slug) then
    sidenotes := sn :: !sidenotes

let render_sidenote ~entries ~sidenotes c = function
  | Bushel.Md.Contact_note (contact, trigger_text) ->
    let open Sortal_schema.Contact in
    let handle = handle contact in
    let name = name contact in
    let link_url = best_url contact |> Option.value ~default:"" in
    let thumbnail_url = Bushel.Entry.contact_thumbnail entries contact in
    (* Emit inline ref as clickable link *)
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      (if link_url <> "" then link_url else "#") handle trigger_text);

    (* Build sidebar content — icon + name + social icons (thumbnail on hover) *)
    let html = Buffer.create 256 in
    Buffer.add_string html {|<span class="sn-contact-row">|};
    Buffer.add_string html Arod_icons.sn_contact;
    if link_url <> "" then
      Buffer.add_string html (Printf.sprintf {|<a href="%s">%s</a>|} link_url (html_escape_attr name))
    else
      Buffer.add_string html (html_escape_attr name);
    (* Social icons *)
    let social_icon href title svg =
      Printf.sprintf {|<a href="%s" title="%s" class="sn-social-icon">%s</a>|}
        href title (Arod_icons.outline ~size:11 svg)
    in
    let social_brand href title svg =
      Printf.sprintf {|<a href="%s" title="%s" class="sn-social-icon">%s</a>|}
        href title (Arod_icons.brand ~size:11 svg)
    in
    let socials = List.filter_map Fun.id [
      (match github_handle contact with
       | Some g -> Some (social_icon ("https://github.com/" ^ g) "GitHub" Arod_icons.github_o)
       | None -> None);
      (match linkedin contact with
       | Some (svc : Sortal_schema.Contact.service) -> Some (social_brand svc.url "LinkedIn" Arod_icons.linkedin_brand)
       | None -> None);
      (match mastodon contact with
       | Some svc when svc.url <> "" ->
         Some (social_brand svc.url "Mastodon" Arod_icons.mastodon_brand)
       | _ -> None);
      (match bluesky_handle contact with
       | Some b -> Some (social_brand ("https://bsky.app/profile/" ^ b) "Bluesky" Arod_icons.bluesky_brand)
       | None -> None);
      (match twitter_handle contact with
       | Some t -> Some (social_brand ("https://twitter.com/" ^ t) "X" Arod_icons.x_brand)
       | None -> None);
      (match orcid contact with
       | Some o -> Some (social_brand ("https://orcid.org/" ^ o) "ORCID" Arod_icons.orcid_brand)
       | None -> None);
      (match current_url contact with
       | Some u -> Some (social_icon u "Website" Arod_icons.world_o)
       | None -> None);
      (match current_email contact with
       | Some addr -> Some (social_icon ("mailto:" ^ addr) "Email" Arod_icons.mail_o)
       | None -> None);
    ] in
    if socials <> [] then begin
      Buffer.add_string html {|<span class="sn-contact-socials">|};
      List.iter (Buffer.add_string html) socials;
      Buffer.add_string html {|</span>|}
    end;
    Buffer.add_string html {|</span>|};
    add_sidenote sidenotes { slug = handle; content_html = Buffer.contents html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Paper_note (paper, trigger_text) ->
    let paper_slug = paper.Bushel.Paper.slug in
    let title = Bushel.Paper.title paper in
    let authors = Bushel.Paper.authors paper in
    let year = Bushel.Paper.year paper in
    let doi = Bushel.Paper.doi paper in
    let link_url = Printf.sprintf "/papers/%s" paper_slug in
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Paper paper) in

    let author_str = match authors with
      | [] -> ""
      | [a] ->
        let parts = String.split_on_char ' ' a in
        List.nth parts (List.length parts - 1)
      | a :: _ ->
        let parts = String.split_on_char ' ' a in
        let last_name = List.nth parts (List.length parts - 1) in
        last_name ^ " et al"
    in

    (* Emit inline ref as clickable link *)
    let id_attr = match doi with
      | Some d -> Printf.sprintf {| id="%s"|} (doi_to_id d)
      | None -> ""
    in
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"%s><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      id_attr link_url paper_slug trigger_text);

    (* Build sidebar content *)
    let html = Printf.sprintf {|%s<a href="%s">%s</a>|} Arod_icons.sn_paper link_url (html_escape_attr title) in
    let html = if author_str <> "" || year > 0 then
      html ^ " · " ^ (html_escape_attr author_str) ^ (if year > 0 then Printf.sprintf " (%d)" year else "")
    else html in
    let html = match doi with
      | Some d -> html ^ Printf.sprintf {| · <a href="https://doi.org/%s">DOI</a>|} (html_escape_attr d)
      | None -> html
    in
    add_sidenote sidenotes { slug = paper_slug; content_html = html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Idea_note (idea, trigger_text) ->
    let idea_slug = idea.Bushel.Idea.slug in
    let title = Bushel.Idea.title idea in
    let status = Bushel.Idea.status idea |> Bushel.Idea.status_to_string in
    let level = Bushel.Idea.level idea |> Bushel.Idea.level_to_string in
    let link_url = Printf.sprintf "/ideas/%s" idea_slug in
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Idea idea) in
    (* Emit inline ref as clickable link *)
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      link_url idea_slug trigger_text);

    (* Build sidebar content *)
    let html = Printf.sprintf {|%s<a href="%s">%s</a>|} Arod_icons.sn_idea link_url (html_escape_attr title) in
    let html = if status <> "" then html ^ " · " ^ (html_escape_attr status) else html in
    let html = if level <> "" then html ^ " · " ^ (html_escape_attr level) else html in
    add_sidenote sidenotes { slug = idea_slug; content_html = html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Note_note (note, trigger_text) ->
    let note_slug = note.Bushel.Note.slug in
    let title = Bushel.Note.title note in
    let year, _month, _day = Bushel.Note.date note in
    let word_count = Bushel.Note.words note in
    let note_doi = Bushel.Note.doi note in
    let link_url = Printf.sprintf "/notes/%s" note_slug in
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Note note) in
    (* Emit inline ref as clickable link *)
    let id_attr = match note_doi with
      | Some d -> Printf.sprintf {| id="%s"|} (doi_to_id d)
      | None -> ""
    in
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"%s><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      id_attr link_url note_slug trigger_text);

    (* Build sidebar content *)
    let html = Printf.sprintf {|%s<a href="%s">%s</a>|} Arod_icons.sn_note link_url (html_escape_attr title) in
    let html = if year > 0 then html ^ Printf.sprintf " · %d" year else html in
    let html = if word_count > 0 then html ^ Printf.sprintf " · %dw" word_count else html in
    add_sidenote sidenotes { slug = note_slug; content_html = html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Project_note (project, trigger_text) ->
    let project_slug = project.Bushel.Project.slug in
    let title = Bushel.Project.title project in
    let start = project.Bushel.Project.start in
    let finish = project.Bushel.Project.finish in
    let link_url = Printf.sprintf "/projects/%s" project_slug in
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Project project) in
    (* Emit inline ref as clickable link *)
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      link_url project_slug trigger_text);

    (* Build sidebar content *)
    let html = Printf.sprintf {|%s<a href="%s">%s</a>|} Arod_icons.sn_project link_url (html_escape_attr title) in
    let html = if start > 0 then
      html ^ " · " ^ string_of_int start ^
      (match finish with Some f -> "–" ^ string_of_int f | None -> "–present")
    else html in
    add_sidenote sidenotes { slug = project_slug; content_html = html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Video_note (video, trigger_text) ->
    let video_slug = video.Bushel.Video.slug in
    let title = Bushel.Video.title video in
    let is_talk = Bushel.Video.talk video in
    let year, _month, _day = Bushel.Video.date video in
    let link_url = Printf.sprintf "/videos/%s" video_slug in
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Video video) in
    (* Emit inline ref as clickable link *)
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"><a href="%s" class="sidenote-ref" data-sidenote="%s">%s</a></span>|}
      link_url video_slug trigger_text);

    (* Build sidebar content *)
    let html = Printf.sprintf {|%s<a href="%s">%s</a>|} Arod_icons.sn_video link_url (html_escape_attr title) in
    let html = if year > 0 then html ^ Printf.sprintf " · %d" year else html in
    let html = html ^ " · " ^ (if is_talk then "talk" else "video") in
    add_sidenote sidenotes { slug = video_slug; content_html = html; thumb_url = thumbnail_url };
    true

  | Bushel.Md.Footnote_note (slug, block, trigger_text) ->
    let temp_doc = Cmarkit.Doc.make block in
    let footnote_renderer = Cmarkit_html.renderer ~safe:false () in
    let content_html = Cmarkit_renderer.doc_to_string footnote_renderer temp_doc in

    (* Emit inline ref with label *)
    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<span class="sidenote-anchor"><span class="sidenote-ref" data-sidenote="%s">%s</span></span>|}
      slug trigger_text);

    let content_html = Arod_icons.sn_footnote ^ content_html in
    add_sidenote sidenotes { slug; content_html; thumb_url = None };
    true

(** {1 Link Renderers} *)

let bushel_link c l =
  let inline_text l =
    Cmarkit.Inline.Link.text l |>
    Cmarkit.Inline.to_plain_text ~break_on_soft:false |> fun r ->
    String.concat "\n" (List.map (String.concat "") r)
  in
  let defs = Cmarkit_renderer.Context.get_defs c in
  match Cmarkit.Inline.Link.reference_definition defs l with
  | Some Cmarkit.Link_definition.Def (ld, _) -> begin
      match Cmarkit.Link_definition.dest ld with
      | Some (dest, _) when String.starts_with ~prefix:"###" dest ->
        (* Kind/category link: ###papers -> opens search with kind:papers *)
        let kind = String.sub dest 3 (String.length dest - 3) in
        let text = inline_text l in
        Cmarkit_renderer.Context.string c
          (Printf.sprintf {|<a href="#kind=%s" data-kind="%s" class="kind-search-link">%s</a>|}
            (html_escape_attr kind) (html_escape_attr kind) (html_escape_attr text));
        true
      | Some (dest, _) when String.starts_with ~prefix:"##" dest
                         && not (String.starts_with ~prefix:"###" dest) ->
        (* Tag link: ##tag → opens search with #tag *)
        let tag = String.sub dest 2 (String.length dest - 2) in
        let text = inline_text l in
        Cmarkit_renderer.Context.string c
          (Printf.sprintf {|<a href="#tag=%s" data-tag="%s" class="tag-search-link"><span class="hash-prefix">#</span>%s</a>|}
            (html_escape_attr tag) (html_escape_attr tag) (html_escape_attr text));
        true
      | _ -> false
    end
  | _ -> false

let media_link ~entries c l =
  let is_bushel_image = String.starts_with ~prefix:"/images/" in
  let is_bushel_video = String.starts_with ~prefix:"/videos/" in
  let defs = Cmarkit_renderer.Context.get_defs c in
  match Cmarkit.Inline.Link.reference_definition defs l with
  | Some Cmarkit.Link_definition.Def (ld, _) -> begin
      match Cmarkit.Link_definition.dest ld with
      | Some (src, _) when is_bushel_image src ->
        let title = match Cmarkit.Link_definition.title ld with
          | None -> ""
          | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title) in
        let alt =
          Cmarkit.Inline.Link.text l |>
          Cmarkit.Inline.to_plain_text ~break_on_soft:false |> fun r ->
          String.concat "\n" (List.map (String.concat "") r) in
        let img_path = string_drop_prefix ~prefix:"/images/" src in
        let img_slug = Filename.chop_extension img_path in
        (match Bushel.Entry.lookup_image entries img_slug with
         | Some img_ent ->
           let html = render_image_html ~alt ~title img_ent in
           Cmarkit_renderer.Context.string c html;
           true
         | None ->
           let html = render_image_html_simple ~cl:"content-image" ~alt ~title ~src () in
           Cmarkit_renderer.Context.string c html;
           true)
      | Some (src, _) when is_bushel_video src ->
        let title = match Cmarkit.Link_definition.title ld with
          | None -> ""
          | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title) in
        (match Bushel.Entry.lookup entries (string_drop_prefix ~prefix:"/videos/" src) with
         | Some (`Video v) ->
           let vertical = Bushel.Video.vertical v in
           let html = render_video_iframe ~vertical ~title (Bushel.Video.url v) in
           Cmarkit_renderer.Context.string c html;
           true
         | Some _ -> failwith "slug not a video"
         | None -> failwith "video not found")
      | None | Some _ -> false
    end
  | None | Some _ -> false

(** {1 Custom Heading Renderer} *)

let custom_heading_renderer ~h2_count ~h3_count ~h4_count c h =
  let open Cmarkit in
  let level = Block.Heading.level h in
  let level_str = string_of_int level in
  (* Update numbering counters *)
  let number_str = match level with
    | 2 ->
      incr h2_count; h3_count := 0; h4_count := 0;
      string_of_int !h2_count
    | 3 ->
      incr h3_count; h4_count := 0;
      Printf.sprintf "%d.%d" !h2_count !h3_count
    | 4 ->
      incr h4_count;
      Printf.sprintf "%d.%d.%d" !h2_count !h3_count !h4_count
    | _ -> ""
  in
  let cls = match level with
    | 1 -> " class=\"group relative page-title text-xl font-semibold mt-4 mb-3\""
    | 2 -> " class=\"group relative text-lg font-semibold mt-5 mb-2\""
    | 3 -> " class=\"group relative text-base font-medium mt-4 mb-1.5\""
    | 4 -> " class=\"group relative text-sm font-semibold mt-3 mb-1\""
    | _ -> ""
  in
  Cmarkit_renderer.Context.string c "<h";
  Cmarkit_renderer.Context.string c level_str;
  (match Block.Heading.id h with
   | None -> ()
   | Some (`Auto id | `Id id) ->
     Cmarkit_renderer.Context.string c " id=\"";
     Cmarkit_renderer.Context.string c id;
     Cmarkit_renderer.Context.string c "\"");
  Cmarkit_renderer.Context.string c cls;
  Cmarkit_renderer.Context.string c ">";
  (* Section number prefix before heading text *)
  (if number_str <> "" then
     match Block.Heading.id h with
     | Some (`Auto id | `Id id) when id <> "" ->
       Cmarkit_renderer.Context.string c
         (Printf.sprintf
            {|<a href="#%s" class="heading-number" aria-label="Link to this section">%s</a> |}
            id number_str)
     | _ ->
       Cmarkit_renderer.Context.string c
         (Printf.sprintf {|<span class="heading-number">%s</span> |} number_str));
  Cmarkit_renderer.Context.inline c (Block.Heading.inline h);
  Cmarkit_renderer.Context.string c "</h";
  Cmarkit_renderer.Context.string c level_str;
  Cmarkit_renderer.Context.string c ">\n";
  true

(** {1 Linked Image Handler}

    When a Link wraps an image ([![...](/images/...)](url)), we handle
    the entire Link ourselves: the <a> wraps the <img> so clicking
    follows the link, and a separate expand button triggers the lightbox. *)

let try_render_linked_image ~entries c l =
  match Cmarkit.Inline.Link.text l with
  | Cmarkit.Inline.Image (img_l, _) ->
    let alt =
      Cmarkit.Inline.Link.text img_l
      |> Cmarkit.Inline.to_plain_text ~break_on_soft:false
      |> fun r -> String.concat "\n" (List.map (String.concat "") r) in
    begin
      let defs = Cmarkit_renderer.Context.get_defs c in
      let link_url = match Cmarkit.Inline.Link.reference_definition defs l with
        | Some (Cmarkit.Link_definition.Def (ld, _)) ->
          (match Cmarkit.Link_definition.dest ld with
           | Some (url, _) -> Some url | None -> None)
        | _ -> None
      in
      match Cmarkit.Inline.Link.reference_definition defs img_l with
      | Some (Cmarkit.Link_definition.Def (ild, _)) ->
        let src = match Cmarkit.Link_definition.dest ild with
          | Some (s, _) -> s | None -> "" in
        let title = match Cmarkit.Link_definition.title ild with
          | None -> ""
          | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title) in
        if not (String.starts_with ~prefix:"/images/" src) then false
        else begin
          let img_path = string_drop_prefix ~prefix:"/images/" src in
          let img_slug = Filename.chop_extension img_path in
          let html = match Bushel.Entry.lookup_image entries img_slug with
            | Some img_ent -> render_image_html ?link_url ~alt ~title img_ent
            | None -> render_image_html_simple ?link_url ~cl:"content-image" ~alt ~title ~src ()
          in
          Cmarkit_renderer.Context.string c html;
          true
        end
      | _ -> false
    end
  | _ -> false

(** {1 Custom HTML Renderer} *)

let custom_inline_renderer ~entries ~sidenotes c = function
  | Cmarkit.Inline.Link (l, _) ->
    if try_render_linked_image ~entries c l then true
    else bushel_link c l
  | Cmarkit.Inline.Image (l, _) -> media_link ~entries c l
  | Bushel.Md.Side_note data -> render_sidenote ~entries ~sidenotes c data
  | _ -> false

let custom_block_quote c bq =
  Cmarkit_renderer.Context.string c "<blockquote class=\"my-4\">\n";
  Cmarkit_renderer.Context.block c (Cmarkit.Block.Block_quote.block bq);
  Cmarkit_renderer.Context.string c "</blockquote>\n";
  true

(** Check if an inline is a single image/linked-image that will produce a
    block-level element (figure or video iframe), so we can skip the [<p>]
    wrapper and avoid invalid [<p><figure>…</figure></p>]. *)
let is_block_level_image inline =
  let is_figure_alt s =
    s = "%r" || s = "%c" || s = "%lc" || s = "%rc" in
  let alt_of l =
    Cmarkit.Inline.Link.text l
    |> Cmarkit.Inline.to_plain_text ~break_on_soft:false
    |> fun r -> String.concat "\n" (List.map (String.concat "") r)
  in
  let is_bushel_media_dest l =
    match Cmarkit.Inline.Link.reference l with
    | `Inline (ld, _) ->
      (match Cmarkit.Link_definition.dest ld with
       | Some (src, _) ->
         String.starts_with ~prefix:"/images/" src
         || String.starts_with ~prefix:"/videos/" src
       | None -> false)
    | _ -> false
  in
  match inline with
  | Cmarkit.Inline.Image (l, _) ->
    is_bushel_media_dest l && is_figure_alt (alt_of l)
  | Cmarkit.Inline.Link (l, _) ->
    (* Linked image: [![%r](/images/...)](url) *)
    (match Cmarkit.Inline.Link.text l with
     | Cmarkit.Inline.Image (img_l, _) ->
       is_bushel_media_dest img_l && is_figure_alt (alt_of img_l)
     | _ -> false)
  | _ -> false

let custom_paragraph c p =
  let inline = Cmarkit.Block.Paragraph.inline p in
  if is_block_level_image inline then begin
    Cmarkit_renderer.Context.inline c inline;
    Cmarkit_renderer.Context.string c "\n";
    true
  end else
    false

let custom_block_renderer ~h2_count ~h3_count ~h4_count c = function
  | Cmarkit.Block.Heading (h, _) -> custom_heading_renderer ~h2_count ~h3_count ~h4_count c h
  | Cmarkit.Block.Block_quote (bq, _) -> custom_block_quote c bq
  | Cmarkit.Block.Paragraph (p, _) -> custom_paragraph c p
  | _ -> false

let custom_html_renderer ~entries ~sidenotes =
  let h2_count = ref 0 in
  let h3_count = ref 0 in
  let h4_count = ref 0 in
  let default = Cmarkit_html.renderer ~safe:false () in
  Cmarkit_renderer.compose default
    (Cmarkit_renderer.make
       ~inline:(custom_inline_renderer ~entries ~sidenotes)
       ~block:(custom_block_renderer ~h2_count ~h3_count ~h4_count)
       ())

(** {1 Markdown to HTML} *)

(** Strip leading empty paragraph tags from rendered HTML. *)
let rec strip_leading_empty_p s =
  let s = String.trim s in
  let prefix = "<p></p>" in
  if String.starts_with ~prefix s then
    strip_leading_empty_p (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else s

let to_html ~(ctx : Arod_ctx.t) content =
  let open Cmarkit in
  let entries = Arod_ctx.entries ctx in
  let sidenotes = ref [] in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true ~resolver:Bushel.Md.with_bushel_links content in
  let mapper = Mapper.make ~inline:(Bushel.Md.make_sidenote_mapper entries) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  let renderer = custom_html_renderer ~entries ~sidenotes in
  let html = Cmarkit_renderer.doc_to_string renderer mapped_doc in
  (strip_leading_empty_p html, List.rev !sidenotes)

let to_plain_html ~(ctx : Arod_ctx.t) content =
  let open Cmarkit in
  let entries = Arod_ctx.entries ctx in
  let sidenotes = ref [] in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true ~resolver:Bushel.Md.with_bushel_links content in
  let mapper = Mapper.make ~inline:(Bushel.Md.make_link_only_mapper entries) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  let renderer = custom_html_renderer ~entries ~sidenotes in
  strip_leading_empty_p (Cmarkit_renderer.doc_to_string renderer mapped_doc)

(** {1 Heading Extraction}

    Extract h2 headings from markdown content for TOC generation. *)

let extract_headings content =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true content in
  let headings = ref [] in
  let collect_heading _mapper = function
    | Block.Heading (h, _) when Block.Heading.level h = 2 ->
      let text =
        Block.Heading.inline h
        |> Inline.to_plain_text ~break_on_soft:false
        |> fun r -> String.concat " " (List.map (String.concat "") r)
      in
      (match Block.Heading.id h with
       | Some (`Auto id | `Id id) when id <> "" ->
         headings := (id, text) :: !headings
       | _ -> ());
      Mapper.default
    | _ -> Mapper.default
  in
  let mapper = Mapper.make ~block:collect_heading () in
  let _ = Mapper.map_doc mapper doc in
  List.rev !headings

(** {1 Atom Feed HTML}

    For feeds, we need to handle footnotes differently and ensure absolute URLs. *)

let to_atom_html ~(ctx : Arod_ctx.t) content =
  let open Cmarkit in
  let entries = Arod_ctx.entries ctx in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true ~resolver:Bushel.Md.with_bushel_links content in
  let defs = Doc.defs doc in
  let footnote_map = Hashtbl.create 7 in

  let atom_bushel_mapper _m inline =
    match inline with
    | Inline.Image (lb, meta) ->
      (match Inline.Link.reference lb with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when Bushel.Md.is_bushel_slug url ->
            let slug = Bushel.Md.strip_handle url in
            (match Bushel.Entry.lookup entries slug with
             | Some (`Video _) ->
               let dest = Printf.sprintf "/videos/%s" slug in
               let title = Link_definition.title ld in
               let alt_text = Inline.Link.text lb |> Inline.to_plain_text ~break_on_soft:false
                             |> fun r -> String.concat "\n" (List.map (String.concat "") r) in
               let txt = Inline.Text (alt_text, meta) in
               let new_ld = Link_definition.make ?title ~dest:(dest, meta) () in
               let ll = `Inline (new_ld, meta) in
               let new_lb = Inline.Link.make txt ll in
               Mapper.ret (Inline.Image (new_lb, meta))
             | Some ent ->
               let dest = Bushel.Entry.site_url ent in
               let title = Link_definition.title ld in
               let alt_text = Inline.Link.text lb |> Inline.to_plain_text ~break_on_soft:false
                             |> fun r -> String.concat "\n" (List.map (String.concat "") r) in
               let txt = Inline.Text (alt_text, meta) in
               let new_ld = Link_definition.make ?title ~dest:(dest, meta) () in
               let ll = `Inline (new_ld, meta) in
               let new_lb = Inline.Link.make txt ll in
               Mapper.ret (Inline.Image (new_lb, meta))
             | None ->
               (match Bushel.Entry.lookup_image entries slug with
                | Some img ->
                  let dest = Printf.sprintf "/images/%s" (Img.name img) in
                  let title = Link_definition.title ld in
                  let alt_text = Inline.Link.text lb |> Inline.to_plain_text ~break_on_soft:false
                                |> fun r -> String.concat "\n" (List.map (String.concat "") r) in
                  let txt = Inline.Text (alt_text, meta) in
                  let new_ld = Link_definition.make ?title ~dest:(dest, meta) () in
                  let ll = `Inline (new_ld, meta) in
                  let new_lb = Inline.Link.make txt ll in
                  Mapper.ret (Inline.Image (new_lb, meta))
                | None ->
                  failwith (Printf.sprintf "%s slug not found in atom markdown" slug)))
          | _ -> Mapper.default)
       | _ -> Mapper.default)
    | _ ->
      Bushel.Md.make_bushel_link_only_mapper defs entries _m inline
  in
  let doc =
    Mapper.map_doc
      (Mapper.make ~inline:atom_bushel_mapper ())
      doc
  in

  let footnotes = ref [] in
  let atom_inline c = function
    | Inline.Image (lb, _meta) ->
      (match Inline.Link.reference lb with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (dest, _) when String.starts_with ~prefix:"/videos/" dest ->
            let slug = string_drop_prefix ~prefix:"/videos/" dest in
            (match Bushel.Entry.lookup entries slug with
             | Some (`Video v) ->
               let title = Bushel.Video.title v in
               let vertical = Bushel.Video.vertical v in
               let iframe_html = render_video_iframe ~vertical ~title (Bushel.Video.url v) in
               Cmarkit_renderer.Context.string c iframe_html;
               true
             | _ -> false)
          | _ -> false)
       | _ -> false)
    | Inline.Link (lb, _meta) ->
      (match Inline.Link.referenced_label lb with
       | Some l when String.starts_with ~prefix:"^" (Label.key l) ->
         (match Inline.Link.reference_definition defs lb with
          | Some (Block.Footnote.Def (fn, _)) ->
            let label_key = Label.key l in
            let num, text =
              match Hashtbl.find_opt footnote_map label_key with
              | Some (n, t) -> (n, t)
              | None ->
                let n = Hashtbl.length footnote_map + 1 in
                let t = Printf.sprintf "[%d]" n in
                Hashtbl.add footnote_map label_key (n, t);
                footnotes := (n, label_key, Block.Footnote.block fn) :: !footnotes;
                (n, t)
            in
            let sup_id = Printf.sprintf "fnref:%d" num in
            let href_attr = Printf.sprintf "#fn:%d" num in
            Cmarkit_renderer.Context.string c (Printf.sprintf "<sup id=\"%s\"><a href=\"%s\" class=\"footnote\">%s</a></sup>" sup_id href_attr text);
            true
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
  let atom_renderer = Cmarkit_renderer.make ~inline:atom_inline () in
  let default = Cmarkit_html.renderer ~safe:false () in
  let renderer = Cmarkit_renderer.compose default atom_renderer in
  let main_html = strip_leading_empty_p (Cmarkit_renderer.doc_to_string renderer doc) in

  if !footnotes = [] then main_html
  else
    let sorted_footnotes = List.sort (fun (a,_,_) (b,_,_) -> compare a b) !footnotes in
    let footnote_content_renderer = Cmarkit_html.renderer ~safe:false () in
    let footnote_items =
      String.concat "\n" (List.map (fun (num, _label, block) ->
        let fn_id = Printf.sprintf "fn:%d" num in
        let fnref_id = Printf.sprintf "fnref:%d" num in
        let temp_doc = Cmarkit.Doc.make block in
        let processed_doc = Mapper.map_doc (Mapper.make ~inline:atom_bushel_mapper ()) temp_doc in
        let block_html = Cmarkit_renderer.doc_to_string footnote_content_renderer processed_doc in
        Printf.sprintf "<li id=\"%s\"><p>%s <a href=\"#%s\" class=\"reversefootnote\">&#8617;</a></p></li>" fn_id block_html fnref_id
      ) sorted_footnotes)
    in
    let footnotes_html = Printf.sprintf "<div class=\"footnotes\"><ol>%s</ol></div>" footnote_items in
    main_html ^ "\n" ^ footnotes_html

(** {1 Feed References}

    Append an HTML references section to feed content for perma/DOI notes. *)

let with_feed_references ~(ctx : Arod_ctx.t) note base_html =
  let me = Arod_ctx.author_exn ctx in
    let entries = Arod_ctx.entries ctx in
    let references = Bushel.Md.note_references entries me note in
    if references = [] then base_html
    else
      let ref_items = List.map (fun (doi, citation, _) ->
        let doi_url = Printf.sprintf "https://doi.org/%s" doi in
        Printf.sprintf "<li>%s<a href=\"%s\" target=\"_blank\"><i>%s</i></a></li>"
          citation doi_url doi
      ) references |> String.concat "\n" in
      let refs_html = Printf.sprintf "<h1>References</h1><ul>%s</ul>" ref_items in
      base_html ^ refs_html
