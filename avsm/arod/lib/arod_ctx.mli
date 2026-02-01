(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Context record for Arod - replaces global state.

    The context holds loaded Bushel entries and site configuration.
    Created once at server startup and passed to all handlers. *)

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

(** {1 Tags} *)

val tags_of_ent : t -> Bushel.Entry.entry -> Bushel.Tags.t list
