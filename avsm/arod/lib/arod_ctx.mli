(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Loaded content and site configuration. *)

@@ portable

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

(** A feed entry that mentions a Bushel entry. *)
type feed_backlink = {
  contact : Sortal_schema.Contact.t;
  feed_entry : Sortal_feed.Entry.t;
}

type t : immutable_data
(** A loaded Arod context. *)

val create :
  config:Arod_config.t -> Eio.Fs.dir_ty Eio.Path.t -> t @@ nonportable
(** [create ~config fs] is the context loaded from [config]'s data directory. *)

val of_entries : config:Arod_config.t -> Bushel.Entry.t -> t @@ nonportable
(** [of_entries ~config entries] is a filesystem-free context over [entries].
    Feed items, backlinks and link metadata are empty. *)

(** {1 Config Accessors} *)

val config : t -> Arod_config.t
(** [config t] is the configuration of [t]. *)
val base_url : t -> string
(** [base_url t] is the configured public URL. *)
val author : t -> Sortal_schema.Contact.t option
(** [author t] is the configured author contact, if present. *)
val author_exn : t -> Sortal_schema.Contact.t
(** [author_exn t] is the configured author contact.

    @raise Not_found if it is absent. *)
val author_name : t -> string
(** [author_name t] is the configured author's display name. *)

(** {1 Entry Lookup} *)

val lookup : t -> string -> Bushel.Entry.entry option @@ portable
(** [lookup t slug] is the entry named by [slug], if present. *)
val lookup_exn : t -> string -> Bushel.Entry.entry
(** [lookup_exn t slug] is the entry named by [slug].

    @raise Not_found if it is absent. *)
val lookup_image : t -> string -> Srcsetter.t option @@ portable
(** [lookup_image t slug] is the image named by [slug], if present. *)
val lookup_by_name : t -> string -> Sortal_schema.Contact.t option
(** [lookup_by_name t name] is the unambiguous contact named [name], if any. *)
val lookup_by_handle : t -> string -> Sortal_schema.Contact.t option
(** [lookup_by_handle t handle] is the contact named by [handle], if any. *)

(** {1 Entry Lists} *)

val entries : t -> Bushel.Entry.t
(** [entries t] is the Bushel collection of [t]. *)
val papers : t -> Bushel.Paper.t list
(** [papers t] is the current papers of [t]. *)
val notes : t -> Bushel.Note.t list
(** [notes t] is the notes of [t]. *)
val ideas : t -> Bushel.Idea.t list
(** [ideas t] is the ideas of [t]. *)
val projects : t -> Bushel.Project.t list
(** [projects t] is the projects of [t]. *)
val videos : t -> Bushel.Video.t list
(** [videos t] is the videos of [t]. *)
val contacts : t -> Sortal_schema.Contact.t list
(** [contacts t] is the contacts of [t]. *)
val images : t -> Srcsetter.t list
(** [images t] is the images of [t]. *)
val all_entries : t -> Bushel.Entry.entry list
(** [all_entries t] is {!Bushel.Entry.all_entries} over the context's entries,
    so it is grouped by kind rather than sorted. *)

(** {1 Link Graph} *)

val backlinks : t -> string -> string list
(** [backlinks t slug] is the slugs of the entries that link to [slug], sorted
    and without repeats. *)

val outbound : t -> string -> string list
(** [outbound t slug] is the slugs and contact handles that [slug] links to,
    sorted and without repeats. *)

val all_external_links : t -> Bushel.Link_graph.external_link list
(** [all_external_links t] is every web link written in an entry. *)

(** {1 References} *)

val note_references :
  t -> string -> (string * string * Bushel.Md.reference_source) list
(** [note_references t slug] is the works cited by the note [slug]. *)

(** {1 Feed Items} *)

val feed_items : t -> feed_item list
(** [feed_items t] is every contact feed entry, newest first. *)

val feed_items_for_contact : t -> string -> feed_item list
(** [feed_items_for_contact t handle] is the feed of contact [handle]. *)

val feed_backlinks_for_slug : t -> string -> feed_backlink list
(** [feed_backlinks_for_slug t slug] is the feed entries that link to [slug]. *)

val feed_items_for_outbound : t -> string -> feed_backlink list
(** [feed_items_for_outbound t slug] is the feed entries linked from [slug],
    without repeats. *)

val forward_slugs : t -> string -> string list
(** [forward_slugs t url] is the slugs of entries whose bodies link to [url]. *)

(** {1 Feed Annotations} *)

val normalise_url : string -> string @@ nonportable
(** [normalise_url u] is [u] without a [www.] host prefix or trailing slash. *)

type annotation_index
(** An annotations file re-keyed by {!normalise_url}. *)

val annotation_index : Sortal_feed.Annotations.t -> annotation_index @@ nonportable
(** [annotation_index ann] is [ann] keyed by {!normalise_url}. Duplicate URL
    spellings are unioned. *)

val annotation_slugs : annotation_index -> string -> string list @@ nonportable
(** [annotation_slugs idx url] is the slugs annotated for [url], or the empty
    list if there are none. *)

(** {1 Links} *)

val link_for_url : t -> string -> Bushel.Link.t option
(** [link_for_url t url] is the metadata for [url], if present. *)

val all_links : t -> Bushel.Link.t list
(** [all_links t] is every link loaded from links.yml, in increasing URL
    order. *)

(** {1 Entry Filtering} *)

type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

val get_entries : t -> types:entry_type list -> Bushel.Entry.entry list
(** [get_entries t ~types] is the matching public entries, newest first. An
    empty [types] selects every entry type. *)

val perma_entries : t -> Bushel.Entry.entry list
(** [perma_entries t] is the permanent notes, newest first. *)
