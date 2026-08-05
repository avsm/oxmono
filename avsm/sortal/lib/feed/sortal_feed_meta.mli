(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Per-feed sync metadata.

    Tracks last-sync time, HTTP conditional GET headers, and entry count
    for each feed file. Stored as JSON alongside the feed data. *)

type t = {
  feed_url : string;
  feed_type : Sortal_schema.Feed.feed_type;
  last_sync : Ptime.t option;
  etag : string option;
  last_modified : string option;
  entry_count : int;
}

val json_t : t Jsont.t

val save : Eio.Fs.dir_ty Eio.Path.t -> t -> unit

val load : Eio.Fs.dir_ty Eio.Path.t -> t option
