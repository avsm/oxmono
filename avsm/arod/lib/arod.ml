(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod - Webserver for Bushel content

    Arod is an httpz-based webserver that serves Bushel content
    (notes, papers, projects, ideas, videos) as a website.

    {1 Core Modules}

    - {!Config} - TOML configuration
    - {!Ctx} - Context record (replaces global state)
    - {!Cache} - TTL cache for rendered HTML
    - {!Render} - Content rendering
    - {!Handlers} - Route handlers *)

module Config = Arod_config
(** TOML-based configuration for the webserver. *)

module Ctx = Arod_ctx
(** Context record holding entries and configuration. *)

module Cache = Arod_cache
(** TTL-based cache for rendered HTML responses. *)

module Md = Arod_md
(** Markdown rendering with Bushel extensions. *)

module Render = Arod_render
(** Consolidated content rendering. *)

module Page = Arod_page
(** Page layout. *)

module Footer = Arod_footer
(** Standard footer. *)

module Feed = Arod_feed
(** Atom feed generation. *)

module Jsonfeed = Arod_jsonfeed
(** JSON feed generation. *)

module Richdata = Arod_richdata
(** JSON-LD rich data for SEO. *)

module Route = Httpz_server.Route
(** HTTP routing (re-exported from httpz). *)

module Handlers = Arod_handlers
(** Pure route handlers. *)

