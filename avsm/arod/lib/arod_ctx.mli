(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Context record for Arod - replaces global state.

    The context holds loaded Bushel entries and site configuration.
    Created once at server startup and passed to all handlers. *)

@@ portable

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

(** A record of a feed entry that mentions a bushel entry. *)
type feed_backlink = {
  contact : Sortal_schema.Contact.t;
  feed_entry : Sortal_feed.Entry.t;
}

type t : immutable_data
(** The context type containing entries and configuration. The kind is what
    lets a handler marked [portable] capture a context and read through it. It
    is honest because the context is built once, by {!create} or
    {!of_entries}, and every field is a config record, a {!Bushel.Entry.t}, a
    list or a {!Bushel.Smap.t}. Adding a mutable or function-valued field would
    make the compiler reject this line. *)

val create :
  config:Arod_config.t -> Eio.Fs.dir_ty Eio.Path.t -> t @@ nonportable
(** [create ~config fs] loads Bushel entries from the configured data directory
    and returns a context. This should be called once at server startup. *)

val of_entries : config:Arod_config.t -> Bushel.Entry.t -> t @@ nonportable
(** [of_entries ~config entries] is a context over [entries] alone, with no
    feed items, no feed backlinks and no link table. It reads no filesystem
    and needs no Eio. It is not portable because it scans every note for the
    works it cites, which {!note_references} then serves.

    {!Arod_md.to_html}, {!Arod_md.to_plain_html} and {!Arod_md.to_atom_html}
    take nothing else out of a context, so this is enough to render a body.
    It is not enough for the rest of {!Arod_md}: {!Arod_md.with_feed_references}
    reads {!author_exn}, which raises unless [entries] holds a contact whose
    handle is the configured one. The server builds its context with
    {!create}. *)

(** {1 Config Accessors} *)

val config : t -> Arod_config.t
val base_url : t -> string
val author : t -> Sortal_schema.Contact.t option
val author_exn : t -> Sortal_schema.Contact.t
val author_name : t -> string

(** {1 Entry Lookup} *)

val lookup : t -> string -> Bushel.Entry.entry option @@ portable
val lookup_exn : t -> string -> Bushel.Entry.entry
val lookup_image : t -> string -> Srcsetter.t option @@ portable
val lookup_by_name : t -> string -> Sortal_schema.Contact.t option
val lookup_by_handle : t -> string -> Sortal_schema.Contact.t option

(** {1 Entry Lists} *)

val entries : t -> Bushel.Entry.t
val papers : t -> Bushel.Paper.t list
val notes : t -> Bushel.Note.t list
val ideas : t -> Bushel.Idea.t list
val projects : t -> Bushel.Project.t list
val videos : t -> Bushel.Video.t list
val contacts : t -> Sortal_schema.Contact.t list
val images : t -> Srcsetter.t list
val all_entries : t -> Bushel.Entry.entry list
(** [all_entries t] is {!Bushel.Entry.all_entries} over the context's entries,
    so it is grouped by kind rather than sorted. *)

(** {1 Link Graph}

    The graph the loader built over the context's entries. A context made by
    {!of_entries} has no graph, so each of these answers the empty list. *)

val backlinks : t -> string -> string list
(** [backlinks t slug] is the slugs of the entries that link to [slug], sorted
    and without repeats. *)

val outbound : t -> string -> string list
(** [outbound t slug] is the slugs and contact handles that [slug] links to,
    sorted and without repeats. *)

val all_external_links : t -> Bushel.Link_graph.external_link list
(** [all_external_links t] is every web link written in an entry, in the order
    the graph was built with, which for a loaded collection is increasing
    source slug and then URL order. *)

(** {1 References} *)

val note_references :
  t -> string -> (string * string * Bushel.Md.reference_source) list
(** [note_references t slug] is the works the note [slug] cites, as
    {!Bushel.Md.note_references} answers them, and the empty list for a note
    that cites nothing or a slug that names no note. The scan runs on [Re] and
    opam [Uri], so it is settled once when the context is built and the render
    reads it from here. A context whose entries hold no contact under the
    configured author handle has no references at all, matching what the
    render did when it looked the author up itself. *)

(** {1 Feed Items} *)

val feed_items : t -> feed_item list
(** [feed_items t] returns all feed entries from contacts, sorted newest first. *)

val feed_items_for_contact : t -> string -> feed_item list
(** [feed_items_for_contact t handle] returns feed entries for a given contact handle. *)

val feed_backlinks_for_slug : t -> string -> feed_backlink list
(** [feed_backlinks_for_slug t slug] returns feed entries that link to [slug]. *)

val feed_items_for_outbound : t -> string -> feed_backlink list
(** [feed_items_for_outbound t slug] is the feed entries whose URL matches an
    outbound external link from [slug], in the order the sorted outbound URLs
    reach them and without repeats. The match is made through
    {!normalise_url} when the context is built, for the reason given on
    {!forward_slugs}. *)

val forward_slugs : t -> string -> string list
(** [forward_slugs t url] is the slugs of the entries whose bodies link to
    [url], where [url] is a feed entry URL spelled as {!Uriz.to_string}
    spells it, and the empty list when no entry links to it. The match is
    made through {!normalise_url} when the context is built, because the
    render that reads it is portable and {!normalise_url} is not. *)

(** {1 Feed Annotations} *)

val normalise_url : string -> string @@ nonportable
(** [normalise_url u] is [u] with a [www.] host prefix and a trailing slash
    removed, re-parsed and re-rendered through a URI parser. Every table in
    this module that is looked up by URL is keyed on this form, and so is the
    annotation file that [arod feed associate] writes. *)

type annotation_index
(** An annotations file re-keyed by {!normalise_url}. *)

val annotation_index : Sortal_feed.Annotations.t -> annotation_index @@ nonportable
(** [annotation_index ann] re-keys [ann] on {!normalise_url}. The stored keys
    are entry URLs as some earlier run rendered them, and that rendering
    changed when Syndic moved from [uri] to [uriz]. A key written under either
    spelling resolves through this index. Slugs recorded under two spellings
    of one URL are unioned. *)

val annotation_slugs : annotation_index -> string -> string list @@ nonportable
(** [annotation_slugs idx url] is the slugs annotated for [url], or the empty
    list if there are none. *)

(** {1 Tags} *)

val tags_of_ent : t -> Bushel.Entry.entry -> Bushel.Tags.t list

(** {1 Links} *)

val link_for_url : t -> string -> Bushel.Link.t option
(** [link_for_url t url] returns the link metadata for [url] if present in links.yml. *)

val all_links : t -> Bushel.Link.t list
(** [all_links t] is every link loaded from links.yml, in increasing URL
    order. *)

(** {1 Entry Filtering} *)

type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

val get_entries : t -> types:entry_type list -> Bushel.Entry.entry list
(** [get_entries t ~types] returns entries matching [types] (or all if empty),
    filtered to exclude non-talk videos and index pages, sorted newest first. *)

val perma_entries : t -> Bushel.Entry.entry list
(** [perma_entries t] returns permanent notes sorted newest first. *)
