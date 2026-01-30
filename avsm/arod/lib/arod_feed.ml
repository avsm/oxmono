(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Atom feed generation for Arod webserver *)

module E = Arod_model.Entry
module N = Arod_model.Note
module C = Sortal_schema.Contact
module X = Syndic.Atom

let anil_copyright = "(c) 1998-2025 Anil Madhavapeddy, all rights reserved"

let author c =
  let uri = Option.map Uri.of_string (C.best_url c) in
  let email = match C.emails c with e :: _ -> Some e.C.address | [] -> None in
  {X.name=(C.name c); email; uri}

let form_uri cfg path = Uri.of_string (cfg.Arod_config.site.base_url ^ path)

let atom_id cfg e = form_uri cfg @@ E.site_url e

let generator = {
  X.version = Some "1.0";
  uri = Some (Uri.of_string "https://github.com/avsm/bushel");
  content = "Bushel"
}

let link cfg e =
  let href = form_uri cfg @@ E.site_url e in
  let rel = X.Self in
  let type_media = None in
  let title = E.title e in
  let length = None in
  let hreflang = None in
  {X.href; rel; type_media; title; length; hreflang}

let news_feed_link cfg =
  let href = form_uri cfg "/news.xml" in
  let rel = X.Self in
  let type_media = None in
  let title = cfg.Arod_config.site.name in
  let length = None in
  let hreflang = None in
  {X.href; rel; type_media; title; length; hreflang}

let ext_link ~title l =
  let href = Uri.of_string l in
  let rel = X.Alternate in
  let type_media = None in
  let title = title in
  let length = None in
  let hreflang = None in
  [{X.href; rel; type_media; title; length; hreflang}]

let atom_of_note cfg ~author note =
  let e = `Note note in
  let id = atom_id cfg e in
  let categories = List.map (fun tag ->
    X.category tag
  ) (N.tags note) in
  let rights : X.title = X.Text anil_copyright in
  let source = None in
  let title : X.title = X.Text note.N.title in
  let published = N.origdate note in
  let updated = N.datetime note in
  let authors = author, [] in

  let base_html = Arod_view.md_to_atom_html note.N.body in

  let is_perma = N.perma note in
  let has_doi = match N.doi note with Some _ -> true | None -> false in
  let html_with_refs =
    if is_perma || has_doi then
      let me = match Arod_model.lookup_by_handle cfg.Arod_config.site.author_handle with
        | Some c -> c
        | None -> failwith "Author not found"
      in
      let references = Bushel.Md.note_references (Arod_model.get_entries ()) me note in
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

  let html_base_uri = Some (Uri.of_string (cfg.site.base_url ^ "/")) in
  let content, links =
    match N.link note with
    | `Local _ ->
      let content = Some (X.Html (html_base_uri, html_with_refs)) in
      let links = [link cfg e] in
      content, links
    | `Ext (_l,u) ->
      let content = Some (X.Html (html_base_uri, html_with_refs)) in
      let links = ext_link ~title:note.N.title u in
      content, links
  in
  let entry = Syndic.Atom.entry
    ~categories ~links ~published ~rights ?content
    ?source ~title ~updated
    ~id ~authors ()
  in
  entry

let atom_of_entry cfg ~author (e:Arod_model.Entry.entry) =
  match e with
  | `Note n -> Some (atom_of_note cfg ~author n)
  | _ -> None

let feed cfg uri entries =
  try
    let author = author @@ (Arod_model.lookup_by_handle cfg.Arod_config.site.author_handle |> Option.get) in
    let authors = [author] in
    let icon = Uri.of_string (cfg.site.base_url ^ "/assets/favicon.ico") in
    let links = [news_feed_link cfg] in
    let atom_entries = List.filter_map (atom_of_entry cfg ~author) entries in
    let title : X.text_construct = X.Text (cfg.site.name ^ "'s feed") in
    let updated = Arod_model.Entry.datetime (List.hd entries) in
    let id = form_uri cfg uri in
    let rights : X.title = X.Text anil_copyright in
    X.feed ~id ~rights ~authors ~title ~updated ~icon ~links atom_entries
  with exn -> Printexc.print_backtrace stdout; print_endline "x"; raise exn

let feed_string cfg uri f =
  let buf = Buffer.create 1024 in
  X.output (feed cfg uri f) (`Buffer buf);
  Buffer.contents buf
