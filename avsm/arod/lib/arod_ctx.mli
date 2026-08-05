(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Context record for Arod - replaces global state.

    The context holds loaded Bushel entries and site configuration.
    Created once at server startup and passed to all handlers. *)

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

type t
(** The context type containing entries and configuration. *)

val create : config:Arod_config.t -> Eio.Fs.dir_ty Eio.Path.t -> t
(** [create ~config fs] loads Bushel entries from the configured data directory
    and returns a context. This should be called once at server startup. *)

(** {1 Config Accessors} *)

val config : t -> Arod_config.t
val base_url : t -> string
val site_name : t -> string
val site_description : t -> string
val author : t -> Sortal_schema.Contact.t option
val author_exn : t -> Sortal_schema.Contact.t
val author_name : t -> string

(** {1 Entry Lookup} *)

val lookup : t -> string -> Bushel.Entry.entry option
val lookup_exn : t -> string -> Bushel.Entry.entry
val lookup_image : t -> string -> Srcsetter.t option
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

(** {1 Feed Items} *)

val feed_items : t -> feed_item list
(** [feed_items t] returns all feed entries from contacts, sorted newest first. *)

val feed_items_for_contact : t -> string -> feed_item list
(** [feed_items_for_contact t handle] returns feed entries for a given contact handle. *)

val feed_backlinks_for_slug : t -> string -> feed_backlink list
(** [feed_backlinks_for_slug t slug] returns feed entries that link to [slug]. *)

val feed_items_for_outbound : t -> string -> feed_backlink list
(** [feed_items_for_outbound t slug] returns feed entries whose URL matches
    an outbound external link from [slug]. *)

(** {1 Tags} *)

val tags_of_ent : t -> Bushel.Entry.entry -> Bushel.Tags.t list

(** {1 Links} *)

val link_for_url : t -> string -> Bushel.Link.t option
(** [link_for_url t url] returns the link metadata for [url] if present in links.yml. *)

val all_links : t -> Bushel.Link.t list
(** [all_links t] returns all links loaded from links.yml. *)

(** {1 Entry Filtering} *)

type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

val get_entries : t -> types:entry_type list -> Bushel.Entry.entry list
(** [get_entries t ~types] returns entries matching [types] (or all if empty),
    filtered to exclude non-talk videos and index pages, sorted newest first. *)

val perma_entries : t -> Bushel.Entry.entry list
(** [perma_entries t] returns permanent notes sorted newest first. *)
