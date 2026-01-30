(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Union entry type for all Bushel content *)

(** A single entry in the knowledge base. *)
type entry =
  [ `Paper of Bushel_paper.t
  | `Project of Bushel_project.t
  | `Idea of Bushel_idea.t
  | `Video of Bushel_video.t
  | `Note of Bushel_note.t
  ]

(** Slug-to-entry lookup table. *)
type slugs = (string, entry) Hashtbl.t

(** The complete entry collection. *)
type t

(** {1 Constructors} *)

val v :
  papers:Bushel_paper.t list ->
  notes:Bushel_note.t list ->
  projects:Bushel_project.t list ->
  ideas:Bushel_idea.t list ->
  videos:Bushel_video.t list ->
  contacts:Sortal_schema.Contact.t list ->
  ?images:Srcsetter.t list ->
  ?doi_entries:Bushel_doi_entry.ts ->
  data_dir:string ->
  unit ->
  t
(** Create an entry collection from lists of each entry type. *)

(** {1 Accessors} *)

val contacts : t -> Sortal_schema.Contact.t list
val videos : t -> Bushel_video.ts
val ideas : t -> Bushel_idea.ts
val papers : t -> Bushel_paper.ts
val notes : t -> Bushel_note.ts
val projects : t -> Bushel_project.ts
val old_papers : t -> Bushel_paper.ts
val images : t -> Srcsetter.t list
val data_dir : t -> string
val doi_entries : t -> Bushel_doi_entry.ts

(** {1 Lookup Functions} *)

val lookup_image : t -> string -> Srcsetter.t option
(** [lookup_image entries slug] finds an image by its slug. *)

val lookup : t -> string -> entry option
(** [lookup entries slug] finds an entry by its slug. *)

val lookup_exn : t -> string -> entry
(** Like {!lookup} but raises [Not_found] if the slug doesn't exist. *)

(** {1 Entry Properties} *)

val to_type_string : entry -> string
(** [to_type_string entry] returns the type name as a string. *)

val slug : entry -> string
(** [slug entry] returns the entry's slug. *)

val title : entry -> string
(** [title entry] returns the entry's title. *)

val body : entry -> string
(** [body entry] returns the entry's body content. *)

val sidebar : entry -> string option
(** [sidebar entry] returns the entry's sidebar content if present. *)

val synopsis : entry -> string option
(** [synopsis entry] returns the entry's synopsis if present. *)

val site_url : entry -> string
(** [site_url entry] returns the site URL path for the entry. *)

val date : entry -> int * int * int
(** [date entry] returns the entry's date as (year, month, day). *)

val datetime : entry -> Ptime.t
(** [datetime entry] returns the entry's date as a timestamp. *)

val year : entry -> int
(** [year entry] returns the entry's year. *)

val is_index_entry : entry -> bool
(** [is_index_entry entry] returns true if this is an index page. *)

(** {1 Derived Lookups} *)

val lookup_site_url : t -> string -> string
(** [lookup_site_url entries slug] returns the site URL for a slug. *)

val lookup_title : t -> string -> string
(** [lookup_title entries slug] returns the title for a slug. *)

val notes_for_slug : t -> string -> Bushel_note.t list
(** [notes_for_slug entries slug] returns notes that reference the given slug. *)

val all_entries : t -> entry list
(** [all_entries entries] returns all entries as a list. *)

val all_papers : t -> entry list
(** [all_papers entries] returns all papers including old versions. *)

(** {1 Comparison} *)

val compare : entry -> entry -> int
(** Compare entries by date, then by title. *)

(** {1 Contact Lookups} *)

val lookup_by_name : t -> string -> Sortal_schema.Contact.t option
(** [lookup_by_name entries name] finds a contact by name. *)

(** {1 Tag Functions} *)

val tags_of_ent : t -> entry -> Bushel_tags.t list
(** [tags_of_ent entries entry] returns the entry's tags. *)

val mention_entries : t -> Bushel_tags.t list -> entry list
(** [mention_entries entries tags] returns entries mentioned in the tags. *)

(** {1 Thumbnail Functions} *)

val smallest_webp_variant : Srcsetter.t -> string
(** [smallest_webp_variant img] returns URL path to smallest webp variant above 480px. *)

val contact_thumbnail_slug : Sortal_schema.Contact.t -> string option
(** [contact_thumbnail_slug contact] returns the image slug for a contact. *)

val contact_thumbnail : t -> Sortal_schema.Contact.t -> string option
(** [contact_thumbnail entries contact] returns the thumbnail URL for a contact. *)

val thumbnail_slug : t -> entry -> string option
(** [thumbnail_slug entries entry] returns the image slug for an entry. *)

val thumbnail : t -> entry -> string option
(** [thumbnail entries entry] returns the thumbnail URL for an entry. *)
