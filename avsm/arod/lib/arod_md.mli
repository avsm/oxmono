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

(** A sidenote extracted during markdown rendering. *)
type sidenote = {
  slug : string;
  content_html : string;
  thumb_url : string option;
}

val sidenote_div_class : string
(** CSS classes for sidenote sidebar divs. *)

val to_html : ctx:Arod_ctx.t -> string -> string * sidenote list
(** [to_html ~ctx content] converts markdown to HTML with full Bushel
    extension support. Returns the article HTML and a list of sidenotes
    collected during rendering for sidebar placement. *)

val to_plain_html : ctx:Arod_ctx.t -> string -> string
(** [to_plain_html ~ctx content] converts markdown to HTML with Bushel
    link resolution but without sidenotes. Bushel references become
    plain links. Suitable for summaries and excerpts. *)

val to_atom_html : ctx:Arod_ctx.t -> string -> string
(** [to_atom_html ~ctx content] converts markdown to feed-safe HTML.
    Handles footnotes with numbered references and ensures proper
    link resolution for feed readers. *)

val extract_headings : string -> (string * string) list
(** [extract_headings content] extracts h2 headings from markdown content
    as [(id, text)] pairs, for use in table-of-contents generation. *)

(** {1 Utilities} *)

val html_escape_attr : string -> string
(** Escape a string for use in an HTML attribute. *)

val doi_to_id : string -> string
(** [doi_to_id doi] converts a DOI to a CSS-safe HTML id like ["cite-10-1234-abc"]. *)

val string_drop_prefix : prefix:string -> string -> string
(** [string_drop_prefix ~prefix s] removes [prefix] from [s] if present. *)

val with_feed_references : ctx:Arod_ctx.t -> Bushel.Note.t -> string -> string
(** [with_feed_references ~ctx note base_html] appends an HTML references
    section to [base_html] if the note is a perma or DOI entry with
    references. Used by both Atom and JSON feed generators. *)
