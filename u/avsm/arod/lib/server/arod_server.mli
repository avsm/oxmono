(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Proffer-httpz serving. *)

module Site = Arod_site
(** The served route table. *)

module Dashboard = Arod_dashboard
(** The live terminal dashboard. *)

val run :
  sw:Eio.Switch.t ->
  stdenv:<
    net:_ Eio.Net.t;
    clock:_ Eio.Time.clock;
    mono_clock:_ Eio.Time.Mono.t;
    .. > ->
  config:Arod.Config.t ->
  log:Proffer_log.t ->
  ?dashboard:Arod_dashboard.t ->
  env:'env ->
  'env Proffer.Site.t ->
  unit
(** [run ~sw ~stdenv ~config ~log ?dashboard ~env site] serves [site] on
    the configured port until [sw] is cancelled. Requests are written to
    [log], and to [dashboard] when one is given, which also counts open
    connections and server errors. *)
