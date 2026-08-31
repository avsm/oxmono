(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Proffer-httpz serving. *)

module Site = Arod_site
(** The served route table. *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  config:Arod.Config.t ->
  log:Arod_log.t ->
  env:'env ->
  'env Proffer.Compiled.t ->
  unit
(** [run ~sw ~net ~clock ~config ~log ~env compiled] serves [compiled] on the
    configured port until [sw] is cancelled. Requests are written to [log]. *)
