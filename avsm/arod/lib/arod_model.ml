(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model layer bridging Bushel to the Arod webserver *)

(** Re-export Bushel modules for convenience *)
module Paper = Bushel.Paper
module Note = Bushel.Note
module Idea = Bushel.Idea
module Project = Bushel.Project
module Video = Bushel.Video
module Entry = Bushel.Entry
module Tags = Bushel.Tags
module Md = Bushel.Md
module Util = Bushel.Util
module Img = Srcsetter
module Contact = Sortal_schema.Contact

(** {1 Global State} *)

(** The loaded entries - set once at startup *)
let entries : Bushel.Entry.t option ref = ref None

(** The site configuration *)
let config : Arod_config.t option ref = ref None

(** Get the loaded entries, raising if not initialized *)
let get_entries () =
  match !entries with
  | Some e -> e
  | None -> failwith "Arod_model: entries not loaded"

(** Get the site config *)
let get_config () =
  match !config with
  | Some c -> c
  | None -> Arod_config.default

(** {1 Initialization} *)

(** Load entries from the configured data directory *)
let init ~cfg fs =
  config := Some cfg;
  let image_output_dir = cfg.Arod_config.paths.images_dir in
  let data_dir = cfg.paths.data_dir in
  let loaded = Bushel_eio.Bushel_loader.load ~image_output_dir fs data_dir in
  entries := Some loaded;
  loaded

(** {1 Lookup Functions} *)

let lookup slug =
  Entry.lookup (get_entries ()) slug

let lookup_exn slug =
  Entry.lookup_exn (get_entries ()) slug

let lookup_image slug =
  Entry.lookup_image (get_entries ()) slug

let lookup_by_handle handle =
  let contacts = Entry.contacts (get_entries ()) in
  List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) contacts

let lookup_by_name name =
  Entry.lookup_by_name (get_entries ()) name

(** {1 Entry Accessors} *)

let papers () = Entry.papers (get_entries ())
let notes () = Entry.notes (get_entries ())
let ideas () = Entry.ideas (get_entries ())
let projects () = Entry.projects (get_entries ())
let videos () = Entry.videos (get_entries ())
let contacts () = Entry.contacts (get_entries ())
let images () = Entry.images (get_entries ())
let all_entries () = Entry.all_entries (get_entries ())

(** {1 Author/Site Identity} *)

let author () =
  let cfg = get_config () in
  lookup_by_handle cfg.site.author_handle

let author_name () =
  match author () with
  | Some c -> Sortal_schema.Contact.name c
  | None -> (get_config ()).site.author_name

let base_url () = (get_config ()).site.base_url
let site_name () = (get_config ()).site.name
let site_description () = (get_config ()).site.description

(** {1 Markdown Rendering} *)

(** Custom HTML renderer for Cmarkit that handles Bushel extensions *)
let custom_html_renderer () =
  let open Cmarkit in
  let open Cmarkit_renderer.Context in
  let inline c = function
    | Inline.Image (img, _meta) ->
      (* Handle bushel image syntax - :slug format *)
      (match Inline.Link.reference img with
       | `Inline (ld, _) ->
         (match Link_definition.dest ld with
          | Some (src, _) when Md.is_bushel_slug src ->
            let slug = Md.strip_handle src in
            let title = match Link_definition.title ld with
              | Some lines -> String.concat "" (List.map Block_line.tight_to_string lines)
              | None -> ""
            in
            let caption =
              Inline.Link.text img
              |> Inline.to_plain_text ~break_on_soft:false
              |> fun r -> String.concat "\n" (List.map (String.concat "") r)
            in
            (* Check if this is a video *)
            (match lookup slug with
             | Some (`Video v) ->
               let video_url = Video.url v in
               let embed_url =
                 let uri = Uri.of_string video_url in
                 let path = Uri.path uri |> String.split_on_char '/' in
                 let path = List.map (function "watch" -> "embed" | p -> p) path in
                 Uri.with_path uri (String.concat "/" path) |> Uri.to_string
               in
               let html = Printf.sprintf
                 {|<div class="video-center"><iframe title="%s" width="100%%" height="315px" src="%s" frameborder="0" allowfullscreen sandbox="allow-same-origin allow-scripts allow-popups allow-forms"></iframe></div>|}
                 title embed_url
               in
               string c html;
               true
             | _ ->
               (* Image handling *)
               let img_info = lookup_image slug in
               let dest = match img_info with
                 | Some img -> "/images/" ^ Img.name img
                 | None -> "/images/" ^ slug ^ ".webp"
               in
               let srcset_attr = match img_info with
                 | Some img ->
                   let variants = Img.variants img in
                   let parts = Img.MS.fold (fun name (w, _) acc ->
                     Printf.sprintf "/images/%s %dw" name w :: acc
                   ) variants [] in
                   if parts = [] then ""
                   else Printf.sprintf " srcset=\"%s\"" (String.concat ", " parts)
                 | None -> ""
               in
               (* Check for positioning directive *)
               (match caption with
                | "%c" | "%r" | "%lc" | "%rc" ->
                  let fig_class = match caption with
                    | "%c" -> "image-center"
                    | "%r" -> "image-right"
                    | "%lc" -> "image-left-float"
                    | "%rc" -> "image-right-float"
                    | _ -> "image-center"
                  in
                  let html = Printf.sprintf
                    {|<figure class="%s"><img class="content-image" src="%s" alt="%s" title="%s" loading="lazy"%s sizes="(max-width: 768px) 100vw, 33vw"><figcaption>%s</figcaption></figure>|}
                    fig_class dest title title srcset_attr title
                  in
                  string c html;
                  true
                | _ ->
                  (* Regular image with content-image class for lightbox *)
                  let html = Printf.sprintf
                    {|<img class="content-image" src="%s" alt="%s" title="%s" loading="lazy"%s sizes="(max-width: 768px) 100vw, 33vw">|}
                    dest caption title srcset_attr
                  in
                  string c html;
                  true))
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
  let default = Cmarkit_html.renderer ~safe:false () in
  Cmarkit_renderer.compose default (Cmarkit_renderer.make ~inline ())

(** Convert markdown to HTML with Bushel link resolution *)
let md_to_html ?renderer md =
  let open Cmarkit in
  let doc = Doc.of_string ~strict:false ~resolver:Md.with_bushel_links md in
  let mapper = Mapper.make ~inline:(Md.make_link_only_mapper (get_entries ())) () in
  let mapped_doc = Mapper.map_doc mapper doc in
  let r = match renderer with Some r -> r | None -> custom_html_renderer () in
  Cmarkit_renderer.doc_to_string r mapped_doc

(** {1 Tag Helpers} *)

let tags_of_ent ent =
  Entry.tags_of_ent (get_entries ()) ent

let concat_tags tags1 tags2 =
  List.sort_uniq compare (tags1 @ tags2)

(** Count tags across all entries *)
let count_tags_for_ents entries =
  let counts = Hashtbl.create 32 in
  List.iter (fun ent ->
    let tags = Entry.tags_of_ent (get_entries ()) ent in
    List.iter (fun tag ->
      let current = Hashtbl.find_opt counts tag |> Option.value ~default:0 in
      Hashtbl.replace counts tag (current + 1)
    ) tags
  ) entries;
  counts

(** Get category tags with counts for the header navigation *)
let cats () =
  let entries = all_entries () in
  let counts = count_tags_for_ents entries in
  Hashtbl.fold (fun k v acc ->
    match k with
    | `Set "videos" -> acc  (* Skip videos, use talks instead *)
    | `Set _ -> (k, v) :: acc
    | _ -> acc
  ) counts []
  |> List.sort (fun (a, _) (b, _) -> compare (Tags.to_string a) (Tags.to_string b))
