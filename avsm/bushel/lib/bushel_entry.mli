(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Union entry type for all Bushel content.

    Everything here is portable, which is what lets a renderer hold a loaded
    collection and read entries out of it from inside a function marked
    [portable]. *)

@@ portable

type entry =
  [ `Paper of Bushel_paper.t
  | `Project of Bushel_project.t
  | `Idea of Bushel_idea.t
  | `Video of Bushel_video.t
  | `Note of Bushel_note.t
  ]
(** A single entry in the knowledge base. *)

type slugs = entry Bushel_smap.t
(** Slug-to-entry lookup table. A slug claimed by more than one entry resolves
    to the entry that was loaded last, in the order note, project, idea, video,
    paper. *)

type t : immutable_data
(** The complete entry collection. The kind is what lets a loaded collection be
    captured and read by a portable function: every field is an immutable list,
    string or {!Bushel_smap.t}, and the collection is built once by {!v} and
    never written to again. *)

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
(** [v ~papers ~notes ~projects ~ideas ~videos ~contacts ~data_dir ()] is the
    collection holding those entries. A paper whose [latest] field is unset is
    filed as an old version and is left out of {!all_entries}. *)

(** {1 Accessors} *)

val contacts : t -> Sortal_schema.Contact.t list
(** [contacts es] is the contacts of [es]. *)

val videos : t -> Bushel_video.ts
(** [videos es] is the videos of [es], in load order. *)

val ideas : t -> Bushel_idea.ts
(** [ideas es] is the ideas of [es], in load order. *)

val papers : t -> Bushel_paper.ts
(** [papers es] is the latest version of each paper of [es]. *)

val notes : t -> Bushel_note.ts
(** [notes es] is the notes of [es], in load order. *)

val projects : t -> Bushel_project.ts
(** [projects es] is the projects of [es], in load order. *)

val old_papers : t -> Bushel_paper.ts
(** [old_papers es] is the superseded paper versions of [es]. *)

val images : t -> Srcsetter.t list
(** [images es] is the image entries of [es]. *)

val data_dir : t -> string
(** [data_dir es] is the directory [es] was loaded from. *)

val doi_entries : t -> Bushel_doi_entry.ts
(** [doi_entries es] is the resolved DOI records of [es]. *)

(** {1 Lookup Functions} *)

val lookup_image : t -> string -> Srcsetter.t option
(** [lookup_image es slug] is the image of [es] with slug [slug]. *)

val lookup : t -> string -> entry option
(** [lookup es slug] is the entry of [es] with slug [slug]. *)

val lookup_exn : t -> string -> entry
(** [lookup_exn es slug] is the entry of [es] with slug [slug].

    @raise Not_found if [slug] is not bound. *)

(** {1 Entry Properties} *)

val to_type_string : entry -> string
(** [to_type_string e] is the kind of [e], one of ["paper"], ["note"],
    ["project"], ["idea"] and ["video"]. *)

val slug : entry -> string
(** [slug e] is the slug of [e]. *)

val title : entry -> string
(** [title e] is the title of [e]. *)

val body : entry -> string
(** [body e] is the Bushel markdown body of [e], which for a paper is its
    abstract and for a video its description. *)

val sidebar : entry -> string option
(** [sidebar e] is the sidebar markdown of [e]. Only a note has one. *)

val synopsis : entry -> string option
(** [synopsis e] is the synopsis of [e]. Only a note has one. *)

val site_url : entry -> string
(** [site_url e] is the path [e] is served under. *)

val date : entry -> int * int * int
(** [date e] is the date of [e] as [(year, month, day)]. A project dates from
    the first of January of its start year and an idea from the first of its
    month. *)

val datetime : entry -> Ptime.t
(** [datetime e] is {!date} as a timestamp at midnight UTC. *)

val year : entry -> int
(** [year e] is the year of {!date}. *)

val is_index_entry : entry -> bool
(** [is_index_entry e] is [true] if [e] is a note marked as a section index. *)

(** {1 Derived Lookups} *)

val lookup_site_url : t -> string -> string
(** [lookup_site_url es slug] is the path [slug] is served under, or the empty
    string if [es] has no such entry. *)

val lookup_title : t -> string -> string
(** [lookup_title es slug] is the title of [slug], or the empty string if [es]
    has no such entry. *)

val notes_for_slug : t -> string -> Bushel_note.t list
(** [notes_for_slug es slug] is the notes of [es] whose [slug_ent] field names
    [slug]. *)

val all_entries : t -> entry list
(** [all_entries es] is every entry, grouped by kind in the order note,
    project, idea, video and paper, and within a kind in load order. Old paper
    versions are not included. Callers that need another order must sort. *)

val all_papers : t -> entry list
(** [all_papers es] is every paper of [es], latest versions first and
    superseded versions after. *)

(** {1 Comparison} *)

val compare : entry -> entry -> int
(** [compare a b] orders by {!datetime}, oldest first, and by title where the
    timestamps are equal. *)

(** {1 Contact Lookups} *)

val lookup_by_name : t -> string -> Sortal_schema.Contact.t option
(** [lookup_by_name es name] is the one contact of [es] carrying [name], case
    insensitively. It is [None] when no contact or more than one carries it,
    because an ambiguous name must not silently pick a side. *)

(** {1 Tag Functions} *)

val tags_of_ent : t -> entry -> Bushel_tags.t list
(** [tags_of_ent es e] is the parsed tags of [e]. *)

val mention_entries : t -> Bushel_tags.t list -> entry list
(** [mention_entries es tags] is the entries of [es] named by the slug tags in
    [tags]. A slug with no entry is skipped and reported on stderr. *)

(** {1 Thumbnail Functions} *)

val smallest_webp_variant : Srcsetter.t -> string
(** [smallest_webp_variant img] is the path of the narrowest WebP variant of
    [img] wider than 480 pixels, falling back to the narrowest variant of any
    width and then to the image itself. *)

val contact_thumbnail_slug : Sortal_schema.Contact.t -> string option
(** [contact_thumbnail_slug c] is the image slug of [c], which is its handle.
    It is never [None]. The option is there to match {!thumbnail_slug}, which
    the same callers use and which can answer [None]. *)

val contact_thumbnail : t -> Sortal_schema.Contact.t -> string option
(** [contact_thumbnail es c] is the path of the thumbnail of [c], or [None] if
    [es] holds no image under the handle of [c]. *)

val thumbnail_slug : t -> entry -> string option
(** [thumbnail_slug es e] is the image slug of [e]. A note falls back through
    its title image, the first image in its body, the first video it links and
    the entry it is about. *)

val thumbnail : t -> entry -> string option
(** [thumbnail es e] is the path of the thumbnail of [e]. A project with no
    image of its own falls back to the face of a supervisor of one of its
    ideas, preferring one other than [avsm]. *)
