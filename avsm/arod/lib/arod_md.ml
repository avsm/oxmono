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

let string_drop_prefix ~prefix str =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  if str_len >= prefix_len && String.sub str 0 prefix_len = prefix then
    String.sub str prefix_len (str_len - prefix_len)
  else
    str

(** {1 Image Rendering} *)

let render_image_html ?(cl="content-image") ~alt ~title img_ent =
  let origin_url = Printf.sprintf "/images/%s.webp"
    (Filename.chop_extension (Img.origin img_ent)) in

  let srcsets = String.concat ","
    (List.map (fun (f,(w,_h)) -> Printf.sprintf "/images/%s %dw" f w)
      (Img.MS.bindings img_ent.Img.variants)) in

  let figure_class, use_figure = match alt with
    | "%c" -> "image-center", true
    | "%r" -> "image-right", true
    | "%lc" -> "image-left-float", true
    | "%rc" -> "image-right-float", true
    | _ -> "", false
  in

  let img_html = Printf.sprintf
    {|<img class="%s" src="%s" alt="%s" title="%s" loading="lazy" srcset="%s" sizes="(max-width: 768px) 100vw, 33vw">|}
    cl origin_url (if use_figure then title else alt) title srcsets
  in

  if use_figure then
    Printf.sprintf {|<figure class="%s">%s<figcaption>%s</figcaption></figure>|}
      figure_class img_html title
  else
    img_html

let render_image_html_simple ~cl ~alt ~title ~src =
  let figure_class, use_figure = match alt with
    | "%c" -> "image-center", true
    | "%r" -> "image-right", true
    | "%lc" -> "image-left-float", true
    | "%rc" -> "image-right-float", true
    | _ -> "", false
  in
  let img_html = Printf.sprintf
    {|<img class="%s" src="%s" alt="%s" title="%s" loading="lazy" sizes="(max-width: 768px) 100vw, 33vw">|}
    cl src (if use_figure then title else alt) title
  in
  if use_figure then
    Printf.sprintf {|<figure class="%s">%s<figcaption>%s</figcaption></figure>|}
      figure_class img_html title
  else
    img_html

(** {1 Video Embedding} *)

let rewrite_watch_to_embed url =
  let uri = Uri.of_string url in
  let path = Uri.path uri |> String.split_on_char '/' in
  let path = List.map (function "watch" -> "embed" | v -> v) path in
  Uri.with_path uri (String.concat "/" path) |> Uri.to_string

let render_video_iframe ~title url =
  let embed_url = rewrite_watch_to_embed url in
  Printf.sprintf
    {|<div class="video-center"><iframe title="%s" width="100%%" height="315px" src="%s" frameborder="0" allowfullscreen sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div>|}
    title embed_url

(** {1 Sidenote Rendering} *)

let render_sidenote ~entries c = function
  | Bushel.Md.Contact_note (contact, trigger_text) ->
    let open Sortal_schema.Contact in
    let handle = handle contact in
    let name = name contact in
    let link_url = best_url contact |> Option.value ~default:"" in
    let thumbnail_url = Bushel.Entry.contact_thumbnail entries contact in

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
    let thumbnail_url = Bushel.Entry.thumbnail entries (`Note note) in

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
    let footnote_renderer = Cmarkit_html.renderer ~safe:false () in
    let content_html = Cmarkit_renderer.doc_to_string footnote_renderer temp_doc in

    let data_attrs = [
      Printf.sprintf {|data-slug="%s"|} slug;
      Printf.sprintf {|data-label="%s"|} (html_escape_attr trigger_text);
    ] in

    Cmarkit_renderer.Context.string c (Printf.sprintf
      {|<side-note type="footnote" %s><template class="footnote-content">%s</template></side-note>|}
      (String.concat " " data_attrs) content_html);
    true

(** {1 Link Renderers} *)

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
           let html = render_image_html_simple ~cl:"content-image" ~alt ~title ~src in
           Cmarkit_renderer.Context.string c html;
           true)
      | Some (src, _) when is_bushel_video src ->
        let title = match Cmarkit.Link_definition.title ld with
          | None -> ""
          | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title) in
        (match Bushel.Entry.lookup entries (string_drop_prefix ~prefix:"/videos/" src) with
         | Some (`Video v) ->
           let html = render_video_iframe ~title (Bushel.Video.url v) in
           Cmarkit_renderer.Context.string c html;
           true
         | Some _ -> failwith "slug not a video"
         | None -> failwith "video not found")
      | None | Some _ -> false
    end
  | None | Some _ -> false

(** {1 Custom HTML Renderer} *)

let custom_inline_renderer ~entries c = function
  | Cmarkit.Inline.Image (l, _) -> media_link ~entries c l
  | Cmarkit.Inline.Link (l, _) -> bushel_link c l
  | Bushel.Md.Side_note data -> render_sidenote ~entries c data
  | _ -> false

let custom_html_renderer ~entries =
  let default = Cmarkit_html.renderer ~safe:false () in
  Cmarkit_renderer.compose default
    (Cmarkit_renderer.make ~inline:(custom_inline_renderer ~entries) ())

(** {1 Markdown to HTML} *)

let to_html ~(ctx : Arod_ctx.t) content =
  let open Cmarkit in
  let entries = Arod_ctx.entries ctx in
  let doc = Doc.of_string ~strict:false ~resolver:Bushel.Md.with_bushel_links content in
  let mapper = Mapper.make ~inline:(Bushel.Md.make_sidenote_mapper entries) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  let renderer = custom_html_renderer ~entries in
  Cmarkit_renderer.doc_to_string renderer mapped_doc

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
                  let dest = Printf.sprintf "/images/%s.webp" (Filename.chop_extension (Img.origin img)) in
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
               let iframe_html = render_video_iframe ~title (Bushel.Video.url v) in
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
