(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Serving the arod site over proffer-httpz. *)

module Site = Arod_site
(** The compiled route table this module serves. *)

val run :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  config:Arod.Config.t ->
  log:Arod_log.t ->
  env:'env ->
  'env Proffer.Compiled.t ->
  unit
(** [run ~sw ~net ~clock ~config ~log ~env compiled] listens on every address
    at [config]'s port and serves [compiled] until [sw] is cancelled, which is
    the only way it returns. Each served request is written to [log] and
    reported on the [arod.server] log source. [env] is the capability record
    the handlers read, and it may hold state bound to the calling domain,
    since serving is single-domain. *)
