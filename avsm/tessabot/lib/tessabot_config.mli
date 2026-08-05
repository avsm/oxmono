(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tessabot configuration management.

    Configuration is loaded from [~/.config/tessabot/config.toml] by default.

    {1 Example config.toml}

    {v
    # Sortal contact handles to monitor
    contacts = ["avsm", "pf341", "tessera"]

    [buffer]
    api_key = "your-buffer-api-key"
    v} *)

type buffer = {
  api_key : string;
}

type t = {
  contacts : string list;
  buffer : buffer;
}

val contacts : t -> Sortal.Contact.t list -> Sortal.Contact.t list
(** [contacts cfg all_contacts] filters [all_contacts] to only those
    whose handle appears in [cfg.contacts]. Returns all contacts with
    feeds if [cfg.contacts] is empty. *)

val config_file : unit -> string

val load : unit -> (t, string) result

val load_or_fail : unit -> t

val sample_config : string

val pp : t Fmt.t
