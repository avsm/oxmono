(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Manual feed discovery using Claude.

    This module drives Claude to inspect a contact's website and generate
    Atom feed entries for content that has no structured feed.  It is used
    for feeds with {!Sortal_schema.Feed.feed_type} = [Manual]. *)

val discover :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  store:Sortal_feed.Store.t ->
  handle:string ->
  contact_yaml:string ->
  Sortal_schema.Feed.t ->
  (Sortal_feed.Sync.sync_result, string) result
(** [discover ~sw ~process_mgr ~clock ~store ~handle ~contact_yaml feed]
    uses Claude to inspect the URL of a Manual [feed], discover new content,
    and merge the resulting Atom entries into the feed store.

    @param contact_yaml  Raw YAML text for the contact (passed to Claude for context)
    @return [Ok result] with entry counts, or [Error msg] on failure *)
