(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Proffer-httpz serving. *)

module Site = Arod_site
(** The served route table. *)

val run :
  sw:Eio.Switch.t ->
  stdenv:<
    net:_ Eio.Net.t;
    clock:_ Eio.Time.clock;
    mono_clock:_ Eio.Time.Mono.t;
    .. > ->
  config:Arod.Config.t ->
  log:Arod_log.t ->
  env:'env ->
  'env Proffer.Site.t ->
  unit
(** [run ~sw ~stdenv ~config ~log ~env site] serves [site] on the
    configured port until [sw] is cancelled. Requests are written to [log]. *)
