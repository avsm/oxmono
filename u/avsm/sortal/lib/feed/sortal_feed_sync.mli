(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed sync orchestration.

    Fetches feeds, merges with existing data, and updates metadata.
    Each format uses an appropriate merge strategy:
    - Atom: aggregate via {!Syndic.Atom.aggregate}
    - RSS: overwrite raw XML (no RSS serializer available)
    - JSON Feed: merge items by ID, keeping newer entries *)

type sync_result = {
  new_entries : int;
  total_entries : int;
  feed_name : string option;
  paused : bool;  (** [true] if the feed was skipped without fetching. *)
}

val sync_feed :
  session:Fetch.plain ->
  store:Sortal_feed_store.t ->
  handle:string ->
  ?force:bool ->
  Sortal_schema.Feed.t ->
  (sync_result, string) result
(** [sync_feed ~session ~store ~handle feed] fetches [feed] and merges it
    into [store]. A paused feed is returned as [Ok { paused = true; ... }]
    without fetching, keeping everything already downloaded. *)

val sync_all :
  session:Fetch.plain ->
  store:Sortal_feed_store.t ->
  handle:string ->
  ?force:bool ->
  Sortal_schema.Feed.t list ->
  (sync_result list, string) result
