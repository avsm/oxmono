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
}

val sync_feed :
  session:Requests.t ->
  store:Sortal_feed_store.t ->
  handle:string ->
  ?force:bool ->
  Sortal_schema.Feed.t ->
  (sync_result, string) result

val sync_all :
  session:Requests.t ->
  store:Sortal_feed_store.t ->
  handle:string ->
  ?force:bool ->
  Sortal_schema.Feed.t list ->
  (sync_result list, string) result
