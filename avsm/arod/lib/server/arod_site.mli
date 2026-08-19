(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The arod route table, compiled for serving. *)

val build : Arod.Config.t -> Arod_handlers.Env.t Proffer.Compiled.t
(** [build cfg] is the site every domain serves. Its routes are the handlers
    of {!Arod_handlers}, everything under [/action] is behind HTTP Basic
    authentication when [cfg] names a stats password, and every response
    carries the site's security headers. Compile once, at startup. *)
