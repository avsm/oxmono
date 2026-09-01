(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured research entries and Bushel markdown. *)

module Note = Bushel_note
(** Blog post and research note entries. *)

module Paper = Bushel_paper
(** Academic paper entries with BibTeX-style metadata. *)

module Project = Bushel_project
(** Research project entries. *)

module Idea = Bushel_idea
(** Research idea/proposal entries. *)

module Video = Bushel_video
(** Video/talk recording entries. *)

module Entry = Bushel_entry
(** Union type for all entry types with common accessors. *)

module Tags = Bushel_tags
(** Tag parsing and serialization. *)

module Md = Bushel_md
(** Markdown processing with Bushel link extensions. *)

module Link = Bushel_link
(** External link tracking and merging. *)

module Release = Bushel_release
(** Code releases, tracked per repository. *)

module Link_graph = Bushel_link_graph
(** Links between entries and external URLs. *)

module Types = Bushel_types
(** Common types and Jsont codecs. *)

module Doi_entry = Bushel_doi_entry
(** DOI entries resolved from external sources. *)

module Reference = Bushel_reference
(** Structured reference types for citations. *)

module Smap = Bushel_smap
(** Build-once lookup tables keyed by string. *)

module Util = Bushel_util
(** Utility functions (word counting, text processing). *)

module Lint = Bushel_lint
(** Lint checks for broken references, unknown fields, and missing content. *)
