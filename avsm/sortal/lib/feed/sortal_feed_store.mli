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

val effective_type : t -> string -> Sortal_schema.Feed.t -> Sortal_schema.Feed.feed_type
(** [effective_type t handle feed] is the feed type of the most recently
    written file among every known format for [feed]'s URL, or [feed]'s
    recorded type if nothing has been synced yet. A file left behind by a
    feed whose format has genuinely changed is still found this way, just
    no longer preferred once a fresher file exists at another type.
    Always [Manual] for a [Manual] feed: those are never reclassified. *)

val relocate : t -> string -> Sortal_schema.Feed.t -> Sortal_schema.Feed.feed_type -> unit
(** [relocate t handle feed to_type] moves the feed file, its
    [.meta.json] and its [.annotations.json] from wherever
    {!effective_type} finds them to the path implied by [to_type], but
    only if the existing content genuinely parses as [to_type]. Call this
    when sync finds a feed's actual format disagrees with what is on
    disk. A feed simply mislabelled from the start moves over and keeps
    merging normally. A feed whose format has truly changed over time is
    left exactly where it is rather than moved somewhere it can only fail
    to parse and then get silently overwritten: it stays reachable
    through {!effective_type}, which is why that function does not
    simply trust [feed]'s recorded type either. A no-op for a [Manual]
    feed, for a destination that already has a file, or once the move has
    already happened. *)

val save_atom : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Atom.feed -> unit

val load_atom : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Atom.feed option

val save_rss_raw : Eio.Fs.dir_ty Eio.Path.t -> string -> unit

val load_rss : Eio.Fs.dir_ty Eio.Path.t -> Syndic.Rss2.channel option

val save_jsonfeed : Eio.Fs.dir_ty Eio.Path.t -> Jsonfeed.t -> unit

val load_jsonfeed : Eio.Fs.dir_ty Eio.Path.t -> Jsonfeed.t option

val entries_of_feed : t -> handle:string -> Sortal_schema.Feed.t -> Sortal_feed_entry.t list
(** [entries_of_feed t ~handle feed] is every entry stored for [feed],
    read via {!effective_type} rather than [feed]'s recorded type, so a
    feed a sync has reclassified is still found. *)

val all_entries : t -> handle:string -> Sortal_schema.Feed.t list -> Sortal_feed_entry.t list
