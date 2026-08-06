(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Stub: the claude binding this depended on has been removed from the
   monorepo.  Manual feed discovery is disabled until it is reimplemented. *)

let discover ~sw:_ ~process_mgr:_ ~clock:_ ~store:_ ~handle:_ ~contact_yaml:_
    (_ : Sortal_schema.Feed.t) : (Sortal_feed.Sync.sync_result, string) result =
  Error "manual feed discovery is disabled (claude binding removed)"
