(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON feed generation for Arod webserver *)

module E = Bushel.Entry
module N = Bushel.Note
module C = Sortal_schema.Contact
module P = Bushel.Paper
module J = Jsonfeed
module Img = Srcsetter

let form_uri cfg path = cfg.Arod_config.site.base_url ^ path

let author cfg c =
  let name = C.name c in
  let url = match C.orcid c with
    | Some orcid -> Some (Printf.sprintf "https://orcid.org/%s" orcid)
    | None -> C.best_url c
  in
  let avatar = Some (form_uri cfg "/images/anil-headshot.webp") in
  Jsonfeed.Author.create ?name:(Some name) ?url ?avatar ()

let item_of_note ~ctx cfg note =
  let e = `Note note in
  let id = match N.doi note with
    | Some doi ->
      let is_valid_doi =
        not (String.contains doi ' ') &&
        not (String.contains doi '\t') &&
        not (String.contains doi '\n') &&
        String.length doi > 0
      in
      if is_valid_doi then
        Printf.sprintf "https://doi.org/%s" doi
      else
        let note_title = N.title note in
        failwith (Printf.sprintf "Invalid DOI in note '%s': '%s'" note_title doi)
    | None -> form_uri cfg (E.site_url e)
  in
  let url = form_uri cfg (E.site_url e) in
  let title = N.title note in
  let date_published = N.origdate note in
  let date_modified = N.datetime note in
  let tags = N.tags note in

  let base_html = Arod_md.to_atom_html ~ctx note.N.body in

  let is_perma = N.perma note in
  let has_doi = match N.doi note with Some _ -> true | None -> false in
  let html_with_refs =
    if is_perma || has_doi then
      let me = match Arod_ctx.lookup_by_handle ctx cfg.Arod_config.site.author_handle with
        | Some c -> c
        | None -> failwith "Author not found"
      in
      let entries = Arod_ctx.entries ctx in
      let references = Bushel.Md.note_references entries me note in
      if List.length references > 0 then
        let refs_html =
          let ref_items = List.map (fun (doi, citation, _) ->
            let doi_url = Printf.sprintf "https://doi.org/%s" doi in
            Printf.sprintf "<li>%s<a href=\"%s\" target=\"_blank\"><i>%s</i></a></li>"
              citation doi_url doi
          ) references |> String.concat "\n" in
          Printf.sprintf "<h1>References</h1><ul>%s</ul>" ref_items
        in
        base_html ^ refs_html
      else
        base_html
    else
      base_html
  in
  let content = `Html html_with_refs in

  let external_url = match note.N.via with
    | Some (_title, via_url) -> Some via_url
    | None ->
      match N.link note with
      | `Local _ -> None
      | `Ext (_l, u) -> Some u
  in

  let image = match note.N.titleimage with
    | Some img_slug ->
      (try
        let entries = Arod_ctx.entries ctx in
        (match E.lookup_image entries img_slug with
         | Some img_ent ->
           let target_width = 1280 in
           let variants = Img.MS.bindings img_ent.Img.variants in
           let best_variant =
             match variants with
             | [] ->
               Printf.sprintf "%s.webp" (Filename.chop_extension (Img.origin img_ent))
             | _ ->
               let sorted = List.sort (fun (_f1,(w1,_h1)) (_f2,(w2,_h2)) ->
                 let diff1 = abs (w1 - target_width) in
                 let diff2 = abs (w2 - target_width) in
                 compare diff1 diff2
               ) variants in
               fst (List.hd sorted)
           in
           Some (Printf.sprintf "%s/images/%s" cfg.Arod_config.site.base_url best_variant)
         | None -> None)
      with Not_found -> None)
    | None -> None
  in

  let summary = note.N.synopsis in

  let attachments = match N.slug_ent note with
    | Some slug ->
      (match Arod_ctx.lookup ctx slug with
       | Some (`Paper p) ->
         let slug = P.slug p in
         let pdf_path = Filename.concat cfg.Arod_config.paths.static_dir
           (Printf.sprintf "papers/%s.pdf" slug) in
         if Sys.file_exists pdf_path then
           let pdf_url = form_uri cfg (Printf.sprintf "/papers/%s.pdf" slug) in
           let pdf_title = P.title p in
           [J.Attachment.create ~url:pdf_url ~mime_type:"application/pdf" ~title:pdf_title ()]
         else
           (match P.best_url p with
            | Some url when String.ends_with ~suffix:".pdf" url ->
              let pdf_url = form_uri cfg url in
              let pdf_title = P.title p in
              [J.Attachment.create ~url:pdf_url ~mime_type:"application/pdf" ~title:pdf_title ()]
            | _ -> [])
       | _ -> [])
    | None -> []
  in

  let references =
    let me = match Arod_ctx.lookup_by_handle ctx cfg.Arod_config.site.author_handle with
      | Some c -> c
      | None -> failwith "Author not found"
    in
    let entries = Arod_ctx.entries ctx in
    Bushel.Md.note_references entries me note
    |> List.map (fun (doi, _citation, ref_source) ->
      let doi_url = Printf.sprintf "https://doi.org/%s" doi in
      let cito = match ref_source with
        | Bushel.Md.Paper -> [`CitesAsSourceDocument]
        | Bushel.Md.Note -> [`CitesAsRelated]
        | Bushel.Md.External -> [`Cites]
      in
      J.Reference.create ~url:doi_url ~doi ~cito ()
    )
  in

  let json_author = author cfg (Arod_ctx.lookup_by_handle ctx cfg.site.author_handle |> Option.get) in

  Jsonfeed.Item.create
    ~id ~content ~url ?external_url ?image ?summary ~title
    ~date_published ~date_modified ~authors:[json_author] ~tags ~attachments ~references ()

let item_of_entry ~ctx cfg (e:E.entry) =
  match e with
  | `Note n -> Some (item_of_note ~ctx cfg n)
  | _ -> None

let feed ~ctx cfg uri entries =
  let title = cfg.Arod_config.site.name ^ "'s feed" in
  let home_page_url = cfg.site.base_url in
  let feed_url = form_uri cfg uri in
  let icon = cfg.site.base_url ^ "/assets/favicon.ico" in
  let json_author = author cfg (Arod_ctx.lookup_by_handle ctx cfg.site.author_handle |> Option.get) in
  let authors = [json_author] in
  let language = "en-US" in
  let items = List.filter_map (item_of_entry ~ctx cfg) entries in
  Jsonfeed.create ~title ~home_page_url ~feed_url ~icon ~authors ~language ~items ()

let feed_string ~ctx cfg uri entries =
  let f = feed ~ctx cfg uri entries in
  match Jsonfeed.to_string f with
  | Ok s -> s
  | Error e ->
    let msg = Fmt.str "Failed to encode JSON Feed: %a" Jsont.Error.pp e in
    failwith msg
