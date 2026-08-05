(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unified feed entry type.

    Provides a common representation across Atom, RSS, and JSON Feed entries
    with conversion functions from each native format. *)

type t = {
  id : string;
  title : string option;
  date : Ptime.t option;
  summary : string option;
  content : string option;
  url : Uri.t option;
  source_feed : string;
  source_type : Sortal_schema.Feed.feed_type;
}

val of_atom_entry : source_feed:string -> Syndic.Atom.entry -> t

val of_rss2_item : source_feed:string -> Syndic.Rss2.item -> t

val of_jsonfeed_item : source_feed:string -> Jsonfeed.Item.t -> t

val compare_by_date : t -> t -> int

val pp : Format.formatter -> t -> unit

val pp_full : Format.formatter -> t -> unit
