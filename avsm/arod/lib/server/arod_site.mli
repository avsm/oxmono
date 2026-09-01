(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod route table. *)

val build : Arod.Config.t -> Arod_handlers.Env.t Proffer.Site.t
(** [build cfg] is the site configured by [cfg]. Statistics routes use Basic
    authentication when a password is configured. *)
