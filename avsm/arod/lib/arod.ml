(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Proffer web server for Bushel content. *)

module Config = Arod_config
(** TOML-based configuration for the webserver. *)

module Ctx = Arod_ctx
(** Context record holding entries and configuration. *)

module Md = Arod_md
(** Markdown rendering with Bushel extensions. *)

module Icons = Arod_icons
(** SVG icon helpers (Tabler Icons). *)

module Text = Arod_text
(** Plaintext extraction from HTML. *)

module Feed = Arod_feed
(** Atom feed generation. *)

module Jsonfeed = Arod_jsonfeed
(** JSON feed generation. *)

module Jsonld = Arod_jsonld
(** Schema.org JSON-LD structured data generation. *)
