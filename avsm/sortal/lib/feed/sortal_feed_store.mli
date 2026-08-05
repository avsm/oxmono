(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Native format feed storage.

    Stores feeds in their native format (Atom XML, RSS XML, JSON Feed JSON)
    under the XDG data directory, organized per-contact. *)

type t

val create : Eio.Fs.dir_ty Eio.Path.t -> t

val create_from_xdg : Xdge.t -> t

val url_to_filename : string -> string

val feed_dir : t -> string -> Eio.Fs.dir_ty Eio.Path.t

val ensure_feed_dir : t -> string -> unit

val feed_file : t -> string -> Sortal_schema.Feed.t -> Eio.Fs.dir_ty Eio.Path.t

val meta_file : t -> string -> Sortal_schema.Feed.t -> Eio.Fs.dir_ty Eio.Path.t

val annotations_file : t -> string -> Sortal_schema.Feed.t -> Eio.Fs.dir_ty Eio.Path.t

val save_atom : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Atom.feed -> unit

val load_atom : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Atom.feed option

val save_rss_raw : Eio.Fs.dir_ty Eio.Path.t -> string -> unit

val load_rss : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Rss2.channel option

val save_jsonfeed : Eio.Fs.dir_ty Eio.Path.t -> Jsonfeed.t -> unit

val load_jsonfeed : Eio.Fs.dir_ty Eio.Path.t -> Jsonfeed.t option

val entries_of_feed : t -> handle:string -> Sortal_schema.Feed.t -> Sortal_feed_entry.t list

val all_entries : t -> handle:string -> Sortal_schema.Feed.t list -> Sortal_feed_entry.t list
