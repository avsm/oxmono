(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Core view rendering for Arod webserver *)

open Htmlit

(** {1 Attribute Helpers} *)

let class_ c = At.class' c

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

(** {1 Icon Helpers} *)

let ent_to_icon = function
  | `Paper _ -> "paper.svg"
  | `Note _ -> "note.svg"
  | `Project _ -> "project.svg"
  | `Idea _ -> "idea.svg"
  | `Video _ -> "video.svg"

let set_to_icon = function
  | "papers" -> Some "paper.svg"
  | "notes" -> Some "note.svg"
  | "projects" -> Some "project.svg"
  | "ideas" -> Some "idea.svg"
  | "videos" -> Some "video.svg"
  | "talks" -> Some "video.svg"
  | _ -> None

(** {1 Tag Rendering} *)

let render_tag ?(relevant=false) ?(active=false) ?fnum ?num t =
  let active_cl = if active then " tag-active" else "" in
  let relevant_cl = if relevant then " tag-relevant" else "" in

  let icon, text =
    match t with
    | `Slug t ->
      let ent = Arod_model.lookup_exn t in
      let icon_name = match ent with
        | `Paper _ -> "paper.svg"
        | `Note _ -> "note.svg"
        | `Project _ -> "project.svg"
        | `Idea _ -> "idea.svg"
        | `Video _ -> "video.svg"
      in
      Some icon_name, Arod_model.Entry.slug ent
    | `Set slug ->
      let icon_name = match slug with
        | "papers" -> Some "paper.svg"
        | "notes" -> Some "note.svg"
        | "projects" -> Some "project.svg"
        | "ideas" -> Some "idea.svg"
        | "videos" | "talks" -> Some "video.svg"
        | _ -> None
      in
      icon_name, slug
    | _ -> None, Arod_model.Tags.to_string t
  in

  let t_str = Arod_model.Tags.to_string t in
  let icon_el = match icon with
    | None -> El.splice []
    | Some icon_name ->
      El.img ~at:[
        At.alt "icon";
        At.class' "hide-mobile inline-icon";
        At.src (Printf.sprintf "/assets/%s" icon_name)
      ] ()
  in

  let count_els = match num, fnum with
    | None, None -> []
    | None, Some fn ->
      [El.span ~at:[At.class' "tag-count-container"] [
        El.span ~at:[At.class' "tag-count-bg"] [El.txt (string_of_int fn)]
      ]]
    | Some n, Some fn when fn <> n ->
      [El.span ~at:[At.class' "tag-count-container"] [
        El.span ~at:[At.class' "tag-count"] [El.txt (string_of_int n)];
        El.span ~at:[At.class' "tag-count-bg"] [El.txt (string_of_int fn)]
      ]]
    | Some n, _ ->
      [El.span ~at:[At.class' "tag-count-container"] [
        El.span ~at:[At.class' "tag-count"] [El.txt (string_of_int n)]
      ]]
  in

  El.span ~at:[
    At.v "data-tag" t_str;
    At.class' ("tag-label" ^ active_cl ^ relevant_cl)
  ] ([icon_el; El.txt text] @ count_els)

let render_tags (ts:Arod_model.Tags.t list) =
  let ts = List.filter (function
    | `Text _
    | `Set _ -> true
    | _ -> false
  ) ts in
  El.splice ~sep:(El.txt " ") (List.map render_tag ts)

(** {1 Image Rendering} *)

let img ?cl ?(alt="") ?(title="") img_ent =
  let origin_url = Printf.sprintf "/images/%s.webp"
    (Filename.chop_extension (Arod_model.Img.origin img_ent)) in

  let open Arod_model.Img in
  let srcsets = String.concat ","
    (List.map (fun (f,(w,_h)) -> Printf.sprintf "/images/%s %dw" f w)
      (MS.bindings img_ent.variants)) in

  let base_attrs = [
    At.v "loading" "lazy";
    At.src origin_url;
    At.v "srcset" srcsets;
    At.v "sizes" "(max-width: 768px) 100vw, 33vw"
  ] in

  let attrs = match cl with
    | Some c -> At.class' c :: base_attrs
    | None -> base_attrs
  in

  match alt with
  | "%r" ->
    El.figure ~at:[At.class' "image-right"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]
    ]
  | "%c" ->
    El.figure ~at:[At.class' "image-center"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]
    ]
  | "%lc" ->
    El.figure ~at:[At.class' "image-left-float"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]
    ]
  | "%rc" ->
    El.figure ~at:[At.class' "image-right-float"] [
      El.img ~at:(At.alt title :: At.title title :: attrs) ();
      El.figcaption [El.txt title]
    ]
  | _ ->
    El.img ~at:(At.alt alt :: At.title title :: attrs) ()

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

let month_name = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr"
  | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug"
  | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _ -> ""

let ptime_date ?(r=false) ?(with_d=false) (y,m,d) =
  let ms = month_name m in
  match with_d with
  | false -> Printf.sprintf "%s %4d" ms y
  | true -> Printf.sprintf "%s %s %4d" (int_to_date_suffix ~r d) ms y

(** {1 String Helpers} *)

let string_drop_prefix ~prefix str =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  if str_len >= prefix_len && String.sub str 0 prefix_len = prefix then
    String.sub str prefix_len (str_len - prefix_len)
  else
    str

let map_and fn l =
  let ll = List.length l in
  List.mapi (fun i v ->
    match i with
    | 0 -> fn v
    | _ when i + 1 = ll -> " and " ^ (fn v)
    | _ -> ", " ^ (fn v)
  ) l |> String.concat ""

(** {1 Link Renderers for Cmarkit} *)

let bushel_link c l =
  let defs = Cmarkit_renderer.Context.get_defs c in
  match Cmarkit.Inline.Link.reference_definition defs l with
  | Some Cmarkit.Link_definition.Def (ld, _) -> begin
      match Cmarkit.Link_definition.dest ld with
      | Some ("#", _) ->
        let text =
          Cmarkit.Inline.Link.text l |>
          Cmarkit.Inline.to_plain_text ~break_on_soft:false |> fun r ->
          String.concat "\n" (List.map (String.concat "") r) in
        Cmarkit_renderer.Context.string c
          (Printf.sprintf {|<a href="#" class="tag-search-link" data-search-tag="%s"><span class="hash-prefix">#</span>%s</a>|}
            (html_escape_attr text) (html_escape_attr text));
        true
      | Some (dest, _) when String.starts_with ~prefix:"###" dest ->
        let type_filter = String.sub dest 3 (String.length dest - 3) in
        let text =
          Cmarkit.Inline.Link.text l |>
          Cmarkit.Inline.to_plain_text ~break_on_soft:false |> fun r ->
          String.concat "\n" (List.map (String.concat "") r) in
        Cmarkit_renderer.Context.string c
          (Printf.sprintf {|<a href="#" class="type-filter-link" data-filter-type="%s">%s</a>|}
            (html_escape_attr type_filter) (html_escape_attr text));
        true
      | Some (dest, _) when String.starts_with ~prefix:"##" dest ->
        let tag = String.sub dest 2 (String.length dest - 2) in
        let text =
          Cmarkit.Inline.Link.text l |>
          Cmarkit.Inline.to_plain_text ~break_on_soft:false |> fun r ->
          String.concat "\n" (List.map (String.concat "") r) in
        Cmarkit_renderer.Context.string c
          (Printf.sprintf {|<a href="#" class="tag-search-link" data-search-tag="%s"><span class="hash-prefix">#</span>%s</a>|}
            (html_escape_attr tag) (html_escape_attr text));
        true
      | _ -> false
    end
  | _ -> false

let media_link c l =
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
        (* Strip /images/ prefix and .webp extension to get the slug *)
        let img_path = string_drop_prefix ~prefix:"/images/" src in
        let img_slug = Filename.chop_extension img_path in
        let img_ent = Arod_model.lookup_image img_slug in
        (match img_ent with
         | Some img_ent ->
           let html = El.to_string ~doctype:false (img ~title ~alt ~cl:"content-image" img_ent) in
           Cmarkit_renderer.Context.string c html;
           true
         | None ->
           (* Image not in index - still handle positioning directives *)
           let html = match alt with
             | "%c" ->
               Printf.sprintf
                 {|<figure class="image-center"><img class="content-image" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw"><figcaption>%s</figcaption></figure>|}
                 src title title title
             | "%r" ->
               Printf.sprintf
                 {|<figure class="image-right"><img class="content-image" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw"><figcaption>%s</figcaption></figure>|}
                 src title title title
             | "%lc" ->
               Printf.sprintf
                 {|<figure class="image-left-float"><img class="content-image" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw"><figcaption>%s</figcaption></figure>|}
                 src title title title
             | "%rc" ->
               Printf.sprintf
                 {|<figure class="image-right-float"><img class="content-image" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw"><figcaption>%s</figcaption></figure>|}
                 src title title title
             | _ ->
               Printf.sprintf
                 {|<img class="content-image" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw">|}
                 src alt title
           in
           Cmarkit_renderer.Context.string c html;
           true)
      | Some (src, _) when is_bushel_video src ->
        let title = match Cmarkit.Link_definition.title ld with
          | None -> ""
          | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title) in
        let url =
          match Arod_model.lookup (string_drop_prefix ~prefix:"/videos/" src) with
          | Some (`Video v) ->
            let rewrite_watch_to_embed url =
              let url = Uri.of_string url in
              let path = Uri.path url |> String.split_on_char '/' in
              let path = List.map (function "watch" -> "embed" |v -> v) path in
              Uri.with_path url (String.concat "/" path) |> Uri.to_string in
            rewrite_watch_to_embed (Arod_model.Video.url v)
          | Some _ -> failwith "slug not a video"
          | None -> failwith "video not found"
        in
        let html = El.to_string ~doctype:false (El.div ~at:[At.class' "video-center"] [
          El.iframe ~at:[
            At.title title;
            At.v "width" "100%";
            At.v "height" "315px";
            At.src url;
            At.v "frameborder" "0";
            At.v "allowfullscreen" "";
            At.v "sandbox" "allow-same-origin allow-scripts allow-popups allow-forms"
          ] []
        ]) in
        Cmarkit_renderer.Context.string c html;
        true
      | None | Some _ -> false
    end
  | None | Some _ -> false

(** {1 Sidenote Rendering} *)

let rec render_sidenote c = function
  | Bushel.Md.Contact_note (contact, trigger_text) ->
    let open Sortal_schema.Contact in
    let handle = handle contact in
    let name = name contact in
    let link_url = best_url contact |> Option.value ~default:"" in
    let thumbnail_url = Arod_model.Entry.contact_thumbnail (Arod_model.get_entries ()) contact in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} handle;
      Printf.sprintf {|data-handle="%s"|} handle;
      Printf.sprintf {|data-name="%s"|} (html_escape_attr name);
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    let data_attrs = match thumbnail_url with
      | Some url -> data_attrs @ [Printf.sprintf {|data-image="%s"|} url]
      | None -> data_attrs
    in

    let data_attrs = match emails contact with
      | e :: _ -> data_attrs @ [Printf.sprintf {|data-email="%s"|} (html_escape_attr e.address)]
      | [] -> data_attrs
    in

    let data_attrs = match github_handle contact with
      | Some g -> data_attrs @ [Printf.sprintf {|data-github="%s"|} (html_escape_attr g)]
      | None -> data_attrs
    in

    let data_attrs = match orcid contact with
      | Some o -> data_attrs @ [Printf.sprintf {|data-orcid="%s"|} (html_escape_attr o)]
      | None -> data_attrs
    in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="contact" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Paper_note (paper, trigger_text) ->
    let paper_slug = paper.Bushel.Paper.slug in
    let title = Bushel.Paper.title paper in
    let authors = Bushel.Paper.authors paper in
    let year = Bushel.Paper.year paper in
    let doi = Bushel.Paper.doi paper in

    let link_url = Printf.sprintf "/papers/%s" paper_slug in

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

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} paper_slug;
      Printf.sprintf {|data-title="%s"|} (html_escape_attr title);
      Printf.sprintf {|data-authors="%s"|} (html_escape_attr author_str);
      Printf.sprintf {|data-year="%d"|} year;
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    let data_attrs = match doi with
      | Some d -> data_attrs @ [Printf.sprintf {|data-doi="%s"|} (html_escape_attr d)]
      | None -> data_attrs
    in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="paper" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Idea_note (idea, trigger_text) ->
    let idea_slug = idea.Bushel.Idea.slug in
    let title = Bushel.Idea.title idea in
    let year = Bushel.Idea.year idea in
    let status = Bushel.Idea.status idea |> Bushel.Idea.status_to_string in
    let level = Bushel.Idea.level idea |> Bushel.Idea.level_to_string in

    let link_url = Printf.sprintf "/ideas/%s" idea_slug in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} idea_slug;
      Printf.sprintf {|data-title="%s"|} (html_escape_attr title);
      Printf.sprintf {|data-year="%d"|} year;
      Printf.sprintf {|data-status="%s"|} (html_escape_attr status);
      Printf.sprintf {|data-level="%s"|} (html_escape_attr level);
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="idea" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Note_note (note, trigger_text) ->
    let note_slug = note.Bushel.Note.slug in
    let title = Bushel.Note.title note in
    let year, month, day = Bushel.Note.date note in
    let word_count = Bushel.Note.words note in

    let link_url = Printf.sprintf "/notes/%s" note_slug in
    let thumbnail_url = Arod_model.Entry.thumbnail (Arod_model.get_entries ()) (`Note note) in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} note_slug;
      Printf.sprintf {|data-title="%s"|} (html_escape_attr title);
      Printf.sprintf {|data-year="%d"|} year;
      Printf.sprintf {|data-month="%d"|} month;
      Printf.sprintf {|data-day="%d"|} day;
      Printf.sprintf {|data-words="%d"|} word_count;
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    let data_attrs = match thumbnail_url with
      | Some url -> data_attrs @ [Printf.sprintf {|data-image="%s"|} url]
      | None -> data_attrs
    in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="note" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Project_note (project, trigger_text) ->
    let project_slug = project.Bushel.Project.slug in
    let title = Bushel.Project.title project in
    let start = project.Bushel.Project.start in
    let finish = project.Bushel.Project.finish in
    let ideas = Bushel.Project.ideas project in

    let link_url = Printf.sprintf "/projects/%s" project_slug in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} project_slug;
      Printf.sprintf {|data-title="%s"|} (html_escape_attr title);
      Printf.sprintf {|data-start="%d"|} start;
      Printf.sprintf {|data-ideas="%s"|} (html_escape_attr ideas);
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    let data_attrs = match finish with
      | Some f -> data_attrs @ [Printf.sprintf {|data-finish="%d"|} f]
      | None -> data_attrs
    in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="project" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Video_note (video, trigger_text) ->
    let video_slug = video.Bushel.Video.slug in
    let title = Bushel.Video.title video in
    let is_talk = Bushel.Video.talk video in
    let year, month, day = Bushel.Video.date video in

    let link_url = Printf.sprintf "/videos/%s" video_slug in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} video_slug;
      Printf.sprintf {|data-title="%s"|} (html_escape_attr title);
      Printf.sprintf {|data-year="%d"|} year;
      Printf.sprintf {|data-month="%d"|} month;
      Printf.sprintf {|data-day="%d"|} day;
      Printf.sprintf {|data-talk="%b"|} is_talk;
      Printf.sprintf {|data-link="%s"|} link_url;
    ] in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="video" %s>%s</side-note>|}
      (String.concat " " data_attrs) trigger_text);
    true

  | Bushel.Md.Footnote_note (slug, block, trigger_text) ->
    let temp_doc = Cmarkit.Doc.make block in
    let footnote_inline c = function
      | Cmarkit.Inline.Image (l, _) -> media_link c l
      | Cmarkit.Inline.Link (l, _) -> bushel_link c l
      | _ -> false
    in
    let footnote_renderer = Cmarkit_html.renderer ~safe:false () in
    let footnote_renderer = Cmarkit_renderer.compose footnote_renderer (Cmarkit_renderer.make ~inline:footnote_inline ()) in
    let content_html = Cmarkit_renderer.doc_to_string footnote_renderer temp_doc in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} slug;
      Printf.sprintf {|data-label="%s"|} (html_escape_attr trigger_text);
    ] in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="footnote" %s><template class="footnote-content">%s</template></side-note>|}
      (String.concat " " data_attrs) content_html);
    true

and custom_inline_renderer c = function
  | Cmarkit.Inline.Image (l, _) -> media_link c l
  | Cmarkit.Inline.Link (l, _) -> bushel_link c l
  | Bushel.Md.Side_note data -> render_sidenote c data
  | _ -> false

(** Custom HTML renderer that handles sidenotes and bushel extensions *)
let custom_html_renderer () =
  let default = Cmarkit_html.renderer ~safe:false () in
  Cmarkit_renderer.compose default (Cmarkit_renderer.make ~inline:custom_inline_renderer ())

(** {1 Markdown to HTML} *)

let md_to_html content =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~resolver:Bushel.Md.with_bushel_links content in
  let entries = Arod_model.get_entries () in
  (* Use sidenote mapper to create Side_note inlines *)
  let mapper = Mapper.make ~inline:(Bushel.Md.make_sidenote_mapper entries) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  let renderer = custom_html_renderer () in
  Cmarkit_renderer.doc_to_string renderer mapped_doc

let md_to_atom_html content =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~heading_auto_ids:true ~resolver:Bushel.Md.with_bushel_links content in
  let defs = Doc.defs doc in
  let footnote_map = Hashtbl.create 7 in
  let entries = Arod_model.get_entries () in

  let atom_bushel_mapper _m inline =
    match inline with
    | Inline.Image (lb, meta) ->
      (match Inline.Link.reference lb with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) when Bushel.Md.is_bushel_slug url ->
            let slug = Bushel.Md.strip_handle url in
            (match Arod_model.Entry.lookup entries slug with
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
               let dest = Arod_model.Entry.site_url ent in
               let title = Link_definition.title ld in
               let alt_text = Inline.Link.text lb |> Inline.to_plain_text ~break_on_soft:false
                             |> fun r -> String.concat "\n" (List.map (String.concat "") r) in
               let txt = Inline.Text (alt_text, meta) in
               let new_ld = Link_definition.make ?title ~dest:(dest, meta) () in
               let ll = `Inline (new_ld, meta) in
               let new_lb = Inline.Link.make txt ll in
               Mapper.ret (Inline.Image (new_lb, meta))
             | None ->
               (match Arod_model.Entry.lookup_image entries slug with
                | Some img ->
                  let dest = Printf.sprintf "/images/%s.webp" (Filename.chop_extension (Arod_model.Img.origin img)) in
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
            (match Arod_model.lookup slug with
             | Some (`Video v) ->
               let video_url =
                 let url_str = Arod_model.Video.url v in
                 let url = Uri.of_string url_str in
                 let path = Uri.path url |> String.split_on_char '/' in
                 let path = List.map (function "watch" -> "embed" | v -> v) path in
                 Uri.with_path url (String.concat "/" path) |> Uri.to_string in
               let title = Arod_model.Video.title v in
               let iframe_html = Printf.sprintf
                 {|<div class="video-center"><iframe title="%s" src="%s" frameborder="0" allowfullscreen="" sandbox="allow-same-origin allow-scripts allow-popups allow-forms" style="aspect-ratio: 16/9; width: 100%%;"></iframe></div>|}
                 title video_url in
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
  let main_html = Cmarkit_renderer.doc_to_string renderer doc in

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

(** {1 Body Rendering} *)

let truncated_body ent =
  let body = Arod_model.Entry.body ent in
  let first, last = Arod_model.Util.first_and_last_hunks body in
  let remaining_words = Arod_model.Util.count_words last in
  let total_words = Arod_model.Util.count_words first + remaining_words in
  let is_note = match ent with `Note _ -> true | _ -> false in
  let is_truncated = remaining_words > 1 in
  let word_count_info =
    if is_truncated || (is_note && total_words > 0) then
      Some (total_words, is_truncated)
    else
      None
  in
  let markdown_with_link =
    let footnote_lines = Arod_model.Util.find_footnote_lines last in
    let footnotes_text =
      if footnote_lines = [] then ""
      else "\n\n" ^ String.concat "\n" footnote_lines
    in
    match word_count_info with
    | Some (total, true) ->
      let url = Arod_model.Entry.site_url ent in
      first ^ "\n\n*[Read full note... (" ^ string_of_int total ^ " words](" ^ url ^ "))*\n" ^ footnotes_text
    | _ -> first ^ footnotes_text
  in
  (El.unsafe_raw (md_to_html markdown_with_link), word_count_info)

let full_body ent =
  El.unsafe_raw (md_to_html (Arod_model.Entry.body ent))

(** {1 Entry Heading} *)

let entry_href ?title ?(tag="h2") ent =
  let via, via_url =
    match ent with
    | `Note n ->
      ( match n.Arod_model.Note.via with
        | None -> None, None
        | Some (t,u) -> Some t, Some u )
    | _ -> None, None
  in

  let via_el =
    match via, via_url with
    | Some t, Some u when t <> "" ->
      El.a ~at:[At.class' "via"; At.href u] [El.txt (Printf.sprintf "(via %s)" t)]
    | _, Some u ->
      El.a ~at:[At.class' "via"; At.href u] [El.txt "(via)"]
    | _ -> El.splice []
  in

  let title_text = match title with
    | None -> Arod_model.Entry.title ent
    | Some t -> t
  in

  match ent with
  | `Note {index_page=true;_} -> El.splice []
  | _ ->
    let h_fn = match tag with
      | "h1" -> El.h1
      | "h2" -> El.h2
      | "h3" -> El.h3
      | "h4" -> El.h4
      | "h5" -> El.h5
      | "h6" -> El.h6
      | _ -> El.h2
    in

    let doi_el = match ent with
      | `Note n when Arod_model.Note.perma n ->
        (match Arod_model.Note.doi n with
         | Some doi_str ->
           El.span ~at:[At.class' "title-doi"] [
             El.txt " / ";
             El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt "DOI"];
           ]
         | None -> El.splice [])
      | _ -> El.splice []
    in

    h_fn [
      El.a ~at:[At.href (Arod_model.Entry.site_url ent)] [El.txt title_text];
      El.txt " ";
      via_el;
      El.span ~at:[At.class' "title-date"] [
        El.txt " / ";
        El.txt (ptime_date ~with_d:false (Arod_model.Entry.date ent))
      ];
      doi_el
    ]

(** {1 Tags Metadata} *)

let tags_meta ?extra ?link ?(tags=[]) ?date ?backlinks_content ent =
  let tags = List.map Arod_model.Tags.of_string tags in
  let link_el = match link with
    | None -> El.a ~at:[At.href (Arod_model.Entry.site_url ent)] [El.txt "#"]
    | Some l -> El.a ~at:[At.href l] [El.txt "#"]
  in

  let date_str = ptime_date ~with_d:true
    (match date with None -> Arod_model.Entry.date ent | Some d -> d) in

  let bullet = El.span ~at:[At.class' "meta-bullet"] [El.txt "•"] in

  let sections = [] in
  let sections = sections @ [[link_el; El.txt " "; El.txt date_str]] in

  let sections = match ent with
    | `Note n when Arod_model.Note.perma n ->
      (match Arod_model.Note.doi n with
       | Some doi_str ->
         let doi_section = [
           El.txt "DOI: ";
           El.a ~at:[At.href ("https://doi.org/" ^ doi_str)] [El.txt doi_str]
         ] in
         sections @ [doi_section]
       | None -> sections)
    | _ -> sections
  in

  let sections = match extra with
    | Some v -> sections @ [[El.txt v]]
    | None -> sections
  in

  let sections = match backlinks_content with
    | Some content ->
      let entry_slug = Arod_model.Entry.slug ent in
      let checkbox_id = "sidenote__checkbox--backlinks-" ^ entry_slug in
      let content_id = "sidenote-backlinks-" ^ entry_slug in
      let backlinks_section = [
        El.span ~at:[At.class' "sidenote"; At.v "role" "note"] [
          El.input ~at:[
            At.type' "checkbox";
            At.id checkbox_id;
            At.class' "sidenote__checkbox";
            At.v "aria-label" "Show backlinks";
            At.v "aria-hidden" "true";
            At.v "hidden" ""
          ] ();
          El.label ~at:[
            At.v "for" checkbox_id;
            At.class' "sidenote__button";
            At.v "data-sidenote-number" "↑";
            At.v "aria-describedby" content_id;
            At.v "tabindex" "0"
          ] [El.txt "backlinks"];
          El.span ~at:[
            At.id content_id;
            At.class' "sidenote__content";
            At.v "aria-hidden" "true";
            At.v "hidden" "";
            At.v "data-sidenote-number" "↑"
          ] [content]
        ]
      ] in
      sections @ [backlinks_section]
    | None -> sections
  in

  let all_tags = Arod_model.concat_tags tags (Arod_model.tags_of_ent ent) in
  let sections = match all_tags with
    | [] -> sections
    | tags ->
      let tag_elements = List.map (fun tag ->
        let tag_str = Arod_model.Tags.to_raw_string tag in
        El.span ~at:[At.v "data-tag" tag_str; At.class' "tag-label"] [
          El.txt tag_str
        ]
      ) tags in
      let tags_section = List.fold_left (fun acc el ->
        if acc = [] then [el]
        else acc @ [El.txt ", "; el]
      ) [] tag_elements in
      sections @ [tags_section]
  in

  let meta_parts = List.fold_left (fun acc section ->
    if acc = [] then section
    else acc @ [bullet] @ section
  ) [] sections in

  El.div ~at:[At.class' "note-meta"] meta_parts

(** {1 References Section} *)

let note_references_html note =
  let is_perma = Arod_model.Note.perma note in
  let has_doi = match Arod_model.Note.doi note with Some _ -> true | None -> false in
  if not (is_perma || has_doi) then
    El.splice []
  else
    let cfg = Arod_model.get_config () in
    let me = Arod_model.lookup_by_handle cfg.site.author_handle in
    match me with
    | None -> El.splice []
    | Some author_contact ->
      let references = Bushel.Md.note_references (Arod_model.get_entries ()) author_contact note in
      if List.length references > 0 then
        let ref_items = List.map (fun (doi, citation, _is_paper) ->
          let doi_url = Printf.sprintf "https://doi.org/%s" doi in
          El.li [
            El.txt citation;
            El.a ~at:[At.href doi_url; At.v "target" "_blank"] [El.i [El.txt doi]];
          ]
        ) references in
        El.div ~at:[At.class' "references-section"] [
          El.h3 ~at:[At.class' "references-heading"] [El.txt "References"];
          El.ul ~at:[At.class' "references-list"] ref_items
        ]
      else
        El.splice []
