(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown rendering with Bushel extensions.

    Converts Bushel-flavored markdown to HTML with support for:
    - Internal links to entries ([:slug] syntax)
    - Image handling with responsive srcset
    - Video embedding
    - Sidenotes (contact, paper, idea, note, project, video popups)
    - Tag search links
    - Footnotes *)

val to_html : ctx:Arod_ctx.t -> string -> string
(** [to_html ~ctx content] converts markdown to HTML with full Bushel
    extension support including sidenotes and media embeds. *)

val to_atom_html : ctx:Arod_ctx.t -> string -> string
(** [to_atom_html ~ctx content] converts markdown to feed-safe HTML.
    Handles footnotes with numbered references and ensures proper
    link resolution for feed readers. *)

(** {1 Utilities} *)

val html_escape_attr : string -> string
(** Escape a string for use in an HTML attribute. *)

val string_drop_prefix : prefix:string -> string -> string
(** [string_drop_prefix ~prefix s] removes [prefix] from [s] if present. *)
