(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Context record for Arod - replaces global state *)

type t = {
  config : Arod_config.t;
  entries : Bushel.Entry.t;
}

let create ~config fs =
  let image_output_dir = config.Arod_config.paths.images_dir in
  let data_dir = config.paths.data_dir in
  let entries = Bushel_eio.Bushel_loader.load ~image_output_dir fs data_dir in
  { config; entries }

(** {1 Config Accessors} *)

let config t = t.config
let base_url t = t.config.site.base_url
let site_name t = t.config.site.name
let site_description t = t.config.site.description

let author t =
  let contacts = Bushel.Entry.contacts t.entries in
  List.find_opt (fun c ->
    Sortal_schema.Contact.handle c = t.config.site.author_handle
  ) contacts

let author_name t =
  match author t with
  | Some c -> Sortal_schema.Contact.name c
  | None -> t.config.site.author_name

(** {1 Entry Lookup} *)

let lookup t slug = Bushel.Entry.lookup t.entries slug
let lookup_exn t slug = Bushel.Entry.lookup_exn t.entries slug
let lookup_image t slug = Bushel.Entry.lookup_image t.entries slug
let lookup_by_name t name = Bushel.Entry.lookup_by_name t.entries name

let lookup_by_handle t handle =
  let contacts = Bushel.Entry.contacts t.entries in
  List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) contacts

(** {1 Entry Lists} *)

let entries t = t.entries
let papers t = Bushel.Entry.papers t.entries
let notes t = Bushel.Entry.notes t.entries
let ideas t = Bushel.Entry.ideas t.entries
let projects t = Bushel.Entry.projects t.entries
let videos t = Bushel.Entry.videos t.entries
let contacts t = Bushel.Entry.contacts t.entries
let images t = Bushel.Entry.images t.entries
let all_entries t = Bushel.Entry.all_entries t.entries

(** {1 Tags} *)

let tags_of_ent t ent = Bushel.Entry.tags_of_ent t.entries ent
