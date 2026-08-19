(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod - Webserver for Bushel content

    Arod is a proffer-based webserver that serves Bushel content
    (notes, papers, projects, ideas, videos) as a website.

    {1 Core Modules}

    - {!Config} - TOML configuration
    - {!Ctx} - Context record (replaces global state) *)

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

(* Handlers are in the separate arod.handlers library to avoid
   circular dependency with arod.component. *)

