(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type sidenote_data =
  | Contact_note of Sortal_schema.Contact.t * string
  | Paper_note of Bushel_paper.t * string
  | Idea_note of Bushel_idea.t * string
  | Note_note of Bushel_note.t * string
  | Project_note of Bushel_project.t * string
  | Video_note of Bushel_video.t * string

type Cmarkit.Inline.t += Side_note of sidenote_data

let is_bushel_slug s = String.starts_with ~prefix:":" s
let is_tag_slug link =
  String.starts_with ~prefix:"##" link &&
  not (String.starts_with ~prefix:"###" link)
let is_kind_slug link =
  String.starts_with ~prefix:"###" link
let is_contact_slug s = String.starts_with ~prefix:"@" s

let strip_handle s =
  if String.length s = 0 then s
  else if s.[0] = '@' || s.[0] = ':' then
    String.sub s 1 (String.length s - 1)
  else if String.length s > 2 && s.[0] = '#' && s.[1] = '#' && s.[2] = '#' then
    String.sub s 3 (String.length s - 3)
  else if String.length s > 1 && s.[0] = '#' && s.[1] = '#' then
    String.sub s 2 (String.length s - 2)
  else s

let authorlink = Cmarkit.Meta.key ()
let sluglink = Cmarkit.Meta.key ()

let make_authorlink label =
  let meta = Cmarkit.Meta.tag authorlink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label

let make_sluglink label =
  let meta = Cmarkit.Meta.tag sluglink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label

let with_bushel_links = function
  | `Def _ as ctx -> Cmarkit.Label.default_resolver ctx
  | `Ref (_, _, (Some _ as def)) -> def
  | `Ref (_, ref, None) ->
    let txt = Cmarkit.Label.key ref in
    if String.length txt = 0 then None
    else match txt.[0] with
      | '@' -> Some (make_authorlink ref)
      | ':' -> Some (make_sluglink ref)
      | '#' -> if String.length txt > 1 && txt.[1] = '#' then Some (make_sluglink ref) else None
      | _ -> None

let text_of_inline lb =
  Cmarkit.Inline.to_plain_text ~break_on_soft:false lb
  |> fun r -> String.concat "\n" (List.map (String.concat "") r)

let link_target_is_bushel ?slugs lb =
  let open Cmarkit in
  let ref = Inline.Link.reference lb in
  match ref with
  | `Inline (ld, _) ->
    let dest = Link_definition.dest ld in
    (match dest with
     | Some (url, _) when is_bushel_slug url ->
       (match slugs with Some s -> Hashtbl.replace s url () | _ -> ());
       Some (url, Inline.Link.text lb |> text_of_inline)
     | Some (url, _) when is_tag_slug url ->
       Some (url, Inline.Link.text lb |> text_of_inline)
     | Some (url, _) when is_kind_slug url ->
       Some (url, Inline.Link.text lb |> text_of_inline)
     | Some (url, _) when is_contact_slug url ->
       Some (url, Inline.Link.text lb |> text_of_inline)
     | _ -> None)
  | _ -> None

let image_target_is_bushel lb =
  let open Cmarkit in
  let ref = Inline.Link.reference lb in
  match ref with
  | `Inline (ld, _) ->
    let dest = Link_definition.dest ld in
    (match dest with
     | Some (url, _) when is_bushel_slug url ->
       let alt = Link_definition.title ld in
       let dir =
         Inline.Link.text lb
         |> Inline.to_plain_text ~break_on_soft:false
         |> fun r -> String.concat "\n" (List.map (String.concat "") r)
       in
       Some (url, alt, dir)
     | _ -> None)
  | _ -> None

let inline_to_plain_text i =
  let lines = Cmarkit.Inline.to_plain_text ~break_on_soft:true i in
  String.concat "\n" (List.map (String.concat "") lines)

let make_plain_text_mapper ?contact_name () =
  let open Cmarkit in
  let text_inline s = Inline.Text (s, Meta.none) in
  let expand_contact key =
    let handle = strip_handle key in
    match contact_name with
    | Some f -> (match f handle with Some n -> n | None -> handle)
    | None -> handle
  in
  let handle_link lb _meta =
    match link_target_is_bushel lb with
    | Some (url, _) ->
      if is_contact_slug url then
        Mapper.ret (text_inline (expand_contact url))
      else
        let title = Inline.Link.text lb |> text_of_inline in
        Mapper.ret (text_inline title)
    | None ->
      (match Inline.Link.referenced_label lb with
       | Some l ->
         let m = Label.meta l in
         (match Meta.find authorlink m with
          | Some () ->
            Mapper.ret (text_inline (expand_contact (Label.key l)))
          | None ->
            (match Meta.find sluglink m with
             | Some () ->
               let title = Inline.Link.text lb |> text_of_inline in
               Mapper.ret (text_inline title)
             | None -> `Default))
       | None -> `Default)
  in
  let inline _m = function
    | Inline.Link (lb, meta) -> handle_link lb meta
    | Inline.Image (lb, _) ->
      (match image_target_is_bushel lb with
       | Some _ -> `Map None
       | None -> `Default)
    | _ -> `Default
  in
  Mapper.make ~inline ()

let plain_text_of_markdown ?contact_name md =
  let doc = Cmarkit.Doc.of_string ~strict:false
      ~resolver:with_bushel_links md in
  let mapper = make_plain_text_mapper ?contact_name () in
  let doc = Cmarkit.Mapper.map_doc mapper doc in
  let block _f acc = function
    | Cmarkit.Block.Paragraph (p, _) ->
      let text = inline_to_plain_text (Cmarkit.Block.Paragraph.inline p) in
      `Fold (text :: acc)
    | Cmarkit.Block.Heading (h, _) ->
      let text = inline_to_plain_text (Cmarkit.Block.Heading.inline h) in
      `Fold (text :: "" :: acc)
    | _ -> `Default
  in
  let folder = Cmarkit.Folder.make ~block () in
  let parts = Cmarkit.Folder.fold_doc folder [] doc in
  String.concat "\n" (List.rev parts)

let resolve_link_text entries title =
  if is_bushel_slug title then
    match Bushel_entry.lookup entries (strip_handle title) with
    | Some ent -> Bushel_entry.title ent
    | None -> title
  else title

let make_sidenote_mapper entries =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, meta) ->
      (match link_target_is_bushel lb with
       | Some (url, raw_title) ->
         let s = strip_handle url in
         let title = resolve_link_text entries raw_title in
         if is_tag_slug url || is_kind_slug url then
           let txt = Inline.Text (title, meta) in
           let ld = Link_definition.make ~dest:(url, meta) () in
           let ll = `Inline (ld, meta) in
           let link = Inline.Link.make txt ll in
           Mapper.ret (Inline.Link (link, meta))
         else if is_contact_slug url then
           (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
            | Some c ->
              let sidenote = Side_note (Contact_note (c, title)) in
              Mapper.ret sidenote
            | None ->
              let txt = Inline.Text (title, meta) in
              Mapper.ret txt)
         else
           (match Bushel_entry.lookup entries s with
            | Some (`Paper p) ->
              let sidenote = Side_note (Paper_note (p, title)) in
              Mapper.ret sidenote
            | Some (`Idea i) ->
              let sidenote = Side_note (Idea_note (i, title)) in
              Mapper.ret sidenote
            | Some (`Note n) ->
              let sidenote = Side_note (Note_note (n, title)) in
              Mapper.ret sidenote
            | Some (`Project p) ->
              let sidenote = Side_note (Project_note (p, title)) in
              Mapper.ret sidenote
            | Some (`Video v) ->
              let sidenote = Side_note (Video_note (v, title)) in
              Mapper.ret sidenote
            | None ->
              let dest = Bushel_entry.lookup_site_url entries s in
              let txt = Inline.Text (title, meta) in
              let ld = Link_definition.make ~dest:(dest, meta) () in
              let ll = `Inline (ld, meta) in
              let link = Inline.Link.make txt ll in
              Mapper.ret (Inline.Link (link, meta)))
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let s = strip_handle slug in
               (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
                | Some c ->
                  let name = Sortal_schema.Contact.name c in
                  let sidenote = Side_note (Contact_note (c, name)) in
                  Mapper.ret sidenote
                | None ->
                  let title = Inline.Link.text lb |> text_of_inline in
                  let txt = Inline.Text (title, meta) in
                  Mapper.ret txt)
             | None ->
               (match Meta.find sluglink m with
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug then
                    let s = strip_handle slug in
                    let raw_title = Inline.Link.text lb |> text_of_inline in
                    let title = resolve_link_text entries raw_title in
                    (match Bushel_entry.lookup entries s with
                     | Some (`Paper p) -> Mapper.ret (Side_note (Paper_note (p, title)))
                     | Some (`Idea i) -> Mapper.ret (Side_note (Idea_note (i, title)))
                     | Some (`Note n) -> Mapper.ret (Side_note (Note_note (n, title)))
                     | Some (`Project p) -> Mapper.ret (Side_note (Project_note (p, title)))
                     | Some (`Video v) -> Mapper.ret (Side_note (Video_note (v, title)))
                     | None ->
                       let dest = Bushel_entry.lookup_site_url entries s in
                       let txt = Inline.Text (title, meta) in
                       let ld = Link_definition.make ~dest:(dest, meta) () in
                       let ll = `Inline (ld, meta) in
                       let link = Inline.Link.make txt ll in
                       Mapper.ret (Inline.Link (link, meta)))
                  else if is_tag_slug slug || is_kind_slug slug then
                    let txt = Inline.Text (strip_handle slug, meta) in
                    let ld = Link_definition.make ~dest:(slug, meta) () in
                    let ll = `Inline (ld, meta) in
                    let link = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (link, meta))
                  else `Default
                | None -> `Default))
          | None -> `Default))
    | Inline.Image (lb, meta) ->
      (match image_target_is_bushel lb with
       | Some (url, alt, caption) ->
         let s = strip_handle url in
         (match Bushel_entry.lookup entries s with
          | Some (`Video _) ->
            let dest = Printf.sprintf "/videos/%s" s in
            let txt = Inline.Text (caption, meta) in
            let ld = Link_definition.make ?title:alt ~dest:(dest, meta) () in
            let ll = `Inline (ld, meta) in
            let img = Inline.Link.make txt ll in
            Mapper.ret (Inline.Image (img, meta))
          | _ ->
            let dest = Printf.sprintf "/images/%s.webp" s in
            let txt = Inline.Text (caption, meta) in
            let ld = Link_definition.make ?title:alt ~dest:(dest, meta) () in
            let ll = `Inline (ld, meta) in
            let img = Inline.Link.make txt ll in
            Mapper.ret (Inline.Image (img, meta)))
       | None -> `Default)
    | _ -> `Default

let make_link_only_mapper entries =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, meta) ->
      (match link_target_is_bushel lb with
       | Some (url, title) ->
         let s = strip_handle url in
         let dest = Bushel_entry.lookup_site_url entries s in
         let link_text =
           if is_bushel_slug title then
             match Bushel_entry.lookup entries (strip_handle title) with
             | Some ent -> Bushel_entry.title ent
             | None -> title
           else title
         in
         let txt = Inline.Text (link_text, meta) in
         let ld = Link_definition.make ~dest:(dest, meta) () in
         let ll = `Inline (ld, meta) in
         let ld = Inline.Link.make txt ll in
         Mapper.ret (Inline.Link (ld, meta))
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let s = strip_handle slug in
               (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
                | Some c ->
                  let name = Sortal_schema.Contact.name c in
                  (match Sortal_schema.Contact.best_url c with
                   | Some dest ->
                     let txt = Inline.Text (name, meta) in
                     let ld = Link_definition.make ~dest:(dest, meta) () in
                     let ll = `Inline (ld, meta) in
                     let ld = Inline.Link.make txt ll in
                     Mapper.ret (Inline.Link (ld, meta))
                   | None ->
                     let txt = Inline.Text (name, meta) in
                     Mapper.ret txt)
                | None ->
                  let title = Inline.Link.text lb |> text_of_inline in
                  let txt = Inline.Text (title, meta) in
                  Mapper.ret txt)
             | None ->
               (match Meta.find sluglink m with
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug || is_tag_slug slug || is_kind_slug slug || is_contact_slug slug then
                    let s = strip_handle slug in
                    let dest = Bushel_entry.lookup_site_url entries s in
                    let title = Inline.Link.text lb |> text_of_inline in
                    let link_text =
                      let trimmed = String.trim title in
                      if is_bushel_slug trimmed then
                        match Bushel_entry.lookup entries (strip_handle trimmed) with
                        | Some ent -> Bushel_entry.title ent
                        | None -> title
                      else title
                    in
                    let txt = Inline.Text (link_text, meta) in
                    let ld = Link_definition.make ~dest:(dest, meta) () in
                    let ll = `Inline (ld, meta) in
                    let ld = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (ld, meta))
                  else `Default
                | None -> `Default))
          | None -> `Default))
    | _ -> `Default

let scan_for_slugs md =
  let open Cmarkit in
  let slugs = Hashtbl.create 7 in
  let doc = Doc.of_string ~strict:false ~resolver:with_bushel_links md in
  let inline_mapper _m = function
    | Inline.Link (lb, _meta) ->
      (match link_target_is_bushel ~slugs lb with
       | Some _ -> `Default
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (match Meta.find sluglink m with
             | Some () ->
               let slug = Label.key l in
               if is_bushel_slug slug then
                 Hashtbl.replace slugs slug ();
               `Default
             | None -> `Default)
          | None -> `Default))
    | _ -> `Default
  in
  let mapper = Mapper.make ~inline:inline_mapper () in
  let _ = Mapper.map_doc mapper doc in
  Hashtbl.fold (fun k () a -> k :: a) slugs []

let extract_all_links text =
  let open Cmarkit in
  let doc = Doc.of_string ~resolver:with_bushel_links text in
  let links = ref [] in

  let find_links_in_inline _mapper = function
    | Inline.Link (lb, _) | Inline.Image (lb, _) ->
      (match Inline.Link.reference lb with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (url, _) ->
            links := url :: !links;
            `Default
          | None -> `Default)
       | `Ref _ ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let key = Label.key l in
            if String.length key > 0 && (key.[0] = ':' || key.[0] = '@' ||
               (String.length key > 1 && key.[0] = '#' && key.[1] = '#')) then
              links := key :: !links;
            `Default
          | None -> `Default))
    | _ -> `Default
  in

  let mapper = Mapper.make ~inline:find_links_in_inline () in
  let _ = Mapper.map_doc mapper doc in

  let module StringSet = Set.Make(String) in
  StringSet.elements (StringSet.of_list !links)

let make_validation_mapper entries broken_slugs broken_contacts =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, _meta) ->
      (match link_target_is_bushel lb with
       | Some (url, _title) ->
         let s = strip_handle url in
         if is_contact_slug url then
           (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
            | None -> Hashtbl.replace broken_contacts url ()
            | Some _ -> ())
         else if is_bushel_slug url then
           (match Bushel_entry.lookup entries s with
            | None -> Hashtbl.replace broken_slugs url ()
            | Some _ -> ());
         `Default
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let handle = strip_handle slug in
               (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) (Bushel_entry.contacts entries) with
                | None -> Hashtbl.replace broken_contacts slug ()
                | Some _ -> ());
               `Default
             | None ->
               (match Meta.find sluglink m with
                | None -> `Default
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug then begin
                    let s = strip_handle slug in
                    match Bushel_entry.lookup entries s with
                     | None -> Hashtbl.replace broken_slugs slug ()
                     | Some _ -> ()
                  end;
                  `Default))
          | None -> `Default))
    | _ -> `Default

let validate_references entries md =
  let open Cmarkit in
  let broken_slugs = Hashtbl.create 7 in
  let broken_contacts = Hashtbl.create 7 in
  let doc = Doc.of_string ~strict:false ~resolver:with_bushel_links md in
  let mapper = Mapper.make ~inline:(make_validation_mapper entries broken_slugs broken_contacts) () in
  let _ = Mapper.map_doc mapper doc in
  let slugs = Hashtbl.fold (fun k () a -> k :: a) broken_slugs [] in
  let contacts = Hashtbl.fold (fun k () a -> k :: a) broken_contacts [] in
  (slugs, contacts)

let make_to_markdown_mapper ?(base_url="") ?(image_base="/images") entries =
  let open Cmarkit in
  fun _m ->
    function
    | Inline.Link (lb, meta) ->
      (match link_target_is_bushel lb with
       | Some (url, title) ->
         let s = strip_handle url in
         if is_tag_slug url then
           let dest = base_url ^ "/tags/" ^ s in
           let txt = Inline.Text (title, meta) in
           let ld = Link_definition.make ~dest:(dest, meta) () in
           let ll = `Inline (ld, meta) in
           let link = Inline.Link.make txt ll in
           Mapper.ret (Inline.Link (link, meta))
         else if is_kind_slug url then
           let dest = base_url ^ "/" ^ s in
           let txt = Inline.Text (title, meta) in
           let ld = Link_definition.make ~dest:(dest, meta) () in
           let ll = `Inline (ld, meta) in
           let link = Inline.Link.make txt ll in
           Mapper.ret (Inline.Link (link, meta))
         else if is_contact_slug url then
           (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
            | Some c ->
              let name = Sortal_schema.Contact.name c in
              (match Sortal_schema.Contact.best_url c with
               | Some dest ->
                 let txt = Inline.Text (name, meta) in
                 let ld = Link_definition.make ~dest:(dest, meta) () in
                 let ll = `Inline (ld, meta) in
                 let link = Inline.Link.make txt ll in
                 Mapper.ret (Inline.Link (link, meta))
               | None ->
                 let txt = Inline.Text (name, meta) in
                 Mapper.ret txt)
            | None ->
              let txt = Inline.Text (title, meta) in
              Mapper.ret txt)
         else
           let dest = base_url ^ Bushel_entry.lookup_site_url entries s in
           let link_text =
             if is_bushel_slug title then
               match Bushel_entry.lookup entries (strip_handle title) with
               | Some ent -> Bushel_entry.title ent
               | None -> title
             else title
           in
           let txt = Inline.Text (link_text, meta) in
           let ld = Link_definition.make ~dest:(dest, meta) () in
           let ll = `Inline (ld, meta) in
           let link = Inline.Link.make txt ll in
           Mapper.ret (Inline.Link (link, meta))
       | None ->
         (match Inline.Link.referenced_label lb with
          | Some l ->
            let m = Label.meta l in
            (match Meta.find authorlink m with
             | Some () ->
               let slug = Label.key l in
               let s = strip_handle slug in
               (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = s) (Bushel_entry.contacts entries) with
                | Some c ->
                  let name = Sortal_schema.Contact.name c in
                  (match Sortal_schema.Contact.best_url c with
                   | Some dest ->
                     let txt = Inline.Text (name, meta) in
                     let ld = Link_definition.make ~dest:(dest, meta) () in
                     let ll = `Inline (ld, meta) in
                     let link = Inline.Link.make txt ll in
                     Mapper.ret (Inline.Link (link, meta))
                   | None ->
                     let txt = Inline.Text (name, meta) in
                     Mapper.ret txt)
                | None ->
                  let title = Inline.Link.text lb |> text_of_inline in
                  let txt = Inline.Text (title, meta) in
                  Mapper.ret txt)
             | None ->
               (match Meta.find sluglink m with
                | Some () ->
                  let slug = Label.key l in
                  if is_bushel_slug slug then
                    let s = strip_handle slug in
                    let dest = base_url ^ Bushel_entry.lookup_site_url entries s in
                    let title = Inline.Link.text lb |> text_of_inline in
                    let link_text =
                      let trimmed = String.trim title in
                      if is_bushel_slug trimmed then
                        match Bushel_entry.lookup entries (strip_handle trimmed) with
                        | Some ent -> Bushel_entry.title ent
                        | None -> title
                      else title
                    in
                    let txt = Inline.Text (link_text, meta) in
                    let ld = Link_definition.make ~dest:(dest, meta) () in
                    let ll = `Inline (ld, meta) in
                    let link = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (link, meta))
                  else if is_tag_slug slug then
                    let s = strip_handle slug in
                    let dest = base_url ^ "/tags/" ^ s in
                    let title = Inline.Link.text lb |> text_of_inline in
                    let txt = Inline.Text (title, meta) in
                    let ld = Link_definition.make ~dest:(dest, meta) () in
                    let ll = `Inline (ld, meta) in
                    let link = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (link, meta))
                  else if is_kind_slug slug then
                    let s = strip_handle slug in
                    let dest = base_url ^ "/" ^ s in
                    let title = Inline.Link.text lb |> text_of_inline in
                    let txt = Inline.Text (title, meta) in
                    let ld = Link_definition.make ~dest:(dest, meta) () in
                    let ll = `Inline (ld, meta) in
                    let link = Inline.Link.make txt ll in
                    Mapper.ret (Inline.Link (link, meta))
                  else `Default
                | None -> `Default))
          | None -> `Default))
    | Inline.Image (lb, meta) ->
      (match image_target_is_bushel lb with
       | Some (url, alt, caption) ->
         let s = strip_handle url in
         let title_text = match alt with
           | Some lines ->
             String.concat "" (List.map Cmarkit.Block_line.tight_to_string lines)
           | None -> ""
         in
         (match Bushel_entry.lookup entries s with
          | Some (`Video v) ->
            let video_url = Bushel_video.url v in
            let embed_url =
              match Uriz.of_string video_url with
              | Null -> video_url
              | This uri ->
                let path = Uriz.path uri |> String.split_on_char '/' in
                let path = List.map (function "watch" -> "embed" | p -> p) path in
                Uriz.with_path uri (String.concat "/" path) |> Uriz.to_string
            in
            let html = Printf.sprintf
              {|<div class="video-center"><iframe title="%s" width="100%%" height="315px" src="%s" frameborder="0" allowfullscreen sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div>|}
              title_text embed_url
            in
            let raw_html = Cmarkit.Block_line.tight_list_of_string html in
            Mapper.ret (Inline.Raw_html (raw_html, meta))
          | _ ->
            let img_info = Bushel_entry.lookup_image entries s in
            let dest = match img_info with
              | Some img -> image_base ^ "/" ^ Srcsetter.name img
              | None -> image_base ^ "/" ^ s ^ ".webp"
            in
            (match caption with
             | "%c" | "%r" | "%lc" | "%rc" ->
               let fig_class = match caption with
                 | "%c" -> "image-center"
                 | "%r" -> "image-right"
                 | "%lc" -> "image-left-float"
                 | "%rc" -> "image-right-float"
                 | _ -> "image-center"
               in
               let srcset_attr = match img_info with
                 | Some img ->
                   let variants = Srcsetter.variants img in
                   let srcset_parts = Srcsetter.MS.fold (fun name (w, _h) acc ->
                     Printf.sprintf "%s/%s %dw" image_base name w :: acc
                   ) variants [] in
                   if srcset_parts = [] then ""
                   else Printf.sprintf " srcset=\"%s\"" (String.concat ", " srcset_parts)
                 | None -> ""
               in
               let html = Printf.sprintf
                 {|<figure class="%s"><img src="%s" alt="%s" title="%s" loading="lazy"%s><figcaption>%s</figcaption></figure>|}
                 fig_class dest title_text title_text srcset_attr title_text
               in
               let raw_html = Cmarkit.Block_line.tight_list_of_string html in
               Mapper.ret (Inline.Raw_html (raw_html, meta))
             | _ ->
               let txt = Inline.Text (caption, meta) in
               let ld = Link_definition.make ?title:alt ~dest:(dest, meta) () in
               let ll = `Inline (ld, meta) in
               let img = Inline.Link.make txt ll in
               Mapper.ret (Inline.Image (img, meta))))
       | None -> `Default)
    | _ -> `Default

let to_markdown ?(base_url="") ?(image_base="/images") ~entries md =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~resolver:with_bushel_links md in
  let mapper = Mapper.make ~inline:(make_to_markdown_mapper ~base_url ~image_base entries) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  Cmarkit_commonmark.of_doc mapped_doc

type reference_source =
  | Paper
  | Note
  | External

let note_references entries (default_author:Sortal_schema.Contact.t) note =
  let refs = ref [] in

  let format_author_last name =
    let parts = String.split_on_char ' ' name in
    List.nth parts (List.length parts - 1)
  in

  let format_citation ~authors ~year ~title ~publisher =
    let author_str = match authors with
      | [] -> ""
      | [author] -> format_author_last author ^ " "
      | author :: _ -> (format_author_last author) ^ " et al "
    in
    let pub_str = match publisher with
      | None | Some "" -> ""
      | Some p -> p ^ ". "
    in
    Printf.sprintf "%s(%d). %s. %s" author_str year title pub_str
  in

  let reference_of_entry = function
    | `Paper paper ->
      Option.map
        (fun doi ->
          let citation =
            format_citation ~authors:(Bushel_paper.authors paper)
              ~year:(Bushel_paper.year paper) ~title:(Bushel_paper.title paper)
              ~publisher:(Some (Bushel_paper.publisher paper))
          in
          (doi, citation, Paper))
        (Bushel_paper.doi paper)
    | `Note cited_note ->
      Option.map
        (fun doi ->
          let authors =
            match Bushel_note.author cited_note with
            | Some author -> [ author ]
            | None -> [ Sortal_schema.Contact.name default_author ]
          in
          let year, _, _ = Bushel_note.date cited_note in
          let citation =
            format_citation ~authors ~year ~title:(Bushel_note.title cited_note)
              ~publisher:None
          in
          (doi, citation, Note))
        (Bushel_note.doi cited_note)
    | _ -> None
  in

  let add_reference ((doi, _, _) as reference) =
    if not (List.exists (fun (existing, _, _) -> String.equal doi existing) !refs)
    then refs := reference :: !refs
  in

  let add_entry_reference entry =
    Option.iter add_reference (reference_of_entry entry)
  in

  (match Bushel_note.slug_ent note with
   | Some slug -> Option.iter add_entry_reference (Bushel_entry.lookup entries slug)
   | None -> ());

  let slugs = scan_for_slugs (Bushel_note.body note) in
  List.iter (fun slug ->
    let normalized_slug = strip_handle slug in
    Option.iter add_entry_reference (Bushel_entry.lookup entries normalized_slug)
  ) slugs;

  let body = Bushel_note.body note in
  let doi_url_pattern = Re.Perl.compile_pat "https?://(?:dx\\.)?doi\\.org/([^)\\s\"'>]+)" in
  let doi_matches = Re.all doi_url_pattern body in
  let doi_entries = Bushel_entry.doi_entries entries in
  List.iter (fun group ->
    try
      let encoded_doi = Re.Group.get group 1 in
      let doi = Uri.pct_decode encoded_doi in
      if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
        match Bushel_doi_entry.find_by_doi doi_entries doi with
        | Some doi_entry when doi_entry.status = Resolved ->
          let citation = format_citation
            ~authors:doi_entry.authors
            ~year:doi_entry.year
            ~title:doi_entry.title
            ~publisher:(Some doi_entry.publisher)
          in
          add_reference (doi, citation, External)
        | _ ->
          add_reference (doi, doi, External)
    with _ -> ()
  ) doi_matches;

  let publisher_pattern = Re.Perl.compile_pat "https?://(?:(?:www\\.)?(?:linkinghub\\.elsevier\\.com|(?:www\\.)?sciencedirect\\.com/science/article|ieeexplore\\.ieee\\.org|academic\\.oup\\.com|nature\\.com|journals\\.sagepub\\.com|garfield\\.library\\.upenn\\.edu|link\\.springer\\.com|arxiv\\.org/abs)/[^)\\s\"'>]+|(?:dl\\.acm\\.org|(?:www\\.)?tandfonline\\.com)/doi(?:/pdf)?/10\\.[^)\\s\"'>]+)" in
  let publisher_matches = Re.all publisher_pattern body in
  List.iter (fun group ->
    try
      let url = Re.Group.get group 0 in
      match Bushel_doi_entry.find_by_url doi_entries url with
      | Some doi_entry when doi_entry.status = Resolved ->
        let doi = doi_entry.doi in
        if not (List.exists (fun (d, _, _) -> d = doi) !refs) then
          let citation = format_citation
            ~authors:doi_entry.authors
            ~year:doi_entry.year
            ~title:doi_entry.title
            ~publisher:(Some doi_entry.publisher)
          in
          add_reference (doi, citation, External)
      | _ -> ()
    with _ -> ()
  ) publisher_matches;

  let own_doi = Bushel_note.doi note in
  let filtered_refs = List.filter (fun (doi, _, _) ->
    match own_doi with
    | Some own -> doi <> own
    | None -> true
  ) !refs in
  List.rev filtered_refs
