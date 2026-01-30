(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod - Webserver for Bushel content

    Arod is a tiny_httpd-based webserver that serves Bushel content
    (notes, papers, projects, ideas, videos) as a website.

    {1 Core Modules}

    - {!Config} - TOML configuration
    - {!Model} - Bushel bridge layer
    - {!View} - Core rendering utilities
    - {!Page} - Page layout
    - {!Entries} - Entry type filtering and rendering *)

module Config = Arod_config
(** TOML-based configuration for the webserver. *)

module Model = Arod_model
(** Model layer bridging Bushel to the webserver. *)

module View = Arod_view
(** Core view rendering utilities. *)

module Page = Arod_page
(** Page layout. *)

module Footer = Arod_footer
(** Standard footer. *)

module Notes = Arod_notes
(** Note rendering. *)

module Papers = Arod_papers
(** Paper rendering. *)

module Ideas = Arod_ideas
(** Idea rendering. *)

module Projects = Arod_projects
(** Project rendering. *)

module Videos = Arod_videos
(** Video rendering. *)

module Entries = Arod_entries
(** Entry type filtering and rendering. *)

module Feed = Arod_feed
(** Atom feed generation. *)

module Jsonfeed = Arod_jsonfeed
(** JSON feed generation. *)

module Richdata = Arod_richdata
(** JSON-LD rich data for SEO. *)

module Html = Arod_html
(** Legacy HTML generation (for compatibility). *)

module Route = Arod_route
(** Framework-agnostic HTTP routing. *)

module Memo = Arod_memo
(** Memoization cache with TTL. *)

module Handlers = Arod_handlers
(** Pure route handlers. *)
