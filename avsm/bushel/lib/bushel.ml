(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel - Personal knowledge base and research entry management

    Bushel is a library for managing structured research entries including
    notes, papers, projects, ideas, videos, and contacts. It provides typed
    access to markdown files with YAML frontmatter and supports link graphs,
    markdown processing with custom extensions, and search integration.

    {1 Entry Types}

    - {!Note} - Blog posts and research notes
    - {!Paper} - Academic papers with BibTeX metadata
    - {!Project} - Research projects
    - {!Idea} - Research ideas/proposals
    - {!Video} - Talk videos and recordings

    {1 Core Modules}

    - {!Entry} - Union type for all entry types with common operations
    - {!Tags} - Tag parsing and filtering
    - {!Md} - Markdown processing with Bushel link extensions
    - {!Link_graph} - Bidirectional link tracking between entries

    {1 Quick Start}

    {[
      (* Load entries using bushel-eio *)
      let entries = Bushel_loader.load fs "/path/to/data" in

      (* Look up entries by slug *)
      match Bushel.Entry.lookup entries "my-note" with
      | Some (`Note n) -> Printf.printf "Title: %s\n" (Bushel.Note.title n)
      | _ -> ()

      (* Get backlinks *)
      let backlinks = Bushel.Link_graph.get_backlinks_for_slug "my-note" in
      List.iter print_endline backlinks
    ]}
*)

(** {1 Entry Types} *)

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

(** {1 Core Modules} *)

module Entry = Bushel_entry
(** Union type for all entry types with common accessors. *)

module Tags = Bushel_tags
(** Tag parsing, filtering, and counting. *)

module Md = Bushel_md
(** Markdown processing with Bushel link extensions. *)

module Link = Bushel_link
(** External link tracking and merging. *)

module Link_graph = Bushel_link_graph
(** Bidirectional link graph for entry relationships. *)

module Description = Bushel_description
(** Generate descriptive text for entries. *)

(** {1 Utilities} *)

module Types = Bushel_types
(** Common types and Jsont codecs. *)

module Doi_entry = Bushel_doi_entry
(** DOI entries resolved from external sources. *)

module Reference = Bushel_reference
(** Structured reference types for citations. *)

module Util = Bushel_util
(** Utility functions (word counting, text processing). *)
