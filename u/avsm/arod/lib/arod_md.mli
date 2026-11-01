(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTML rendering of Bushel markdown. *)

@@ portable

(** A sidenote extracted from a rendered document. *)
type sidenote = {
  slug : string;
  content_html : string;
  thumb_url : string option;
}

val sidenote_div_class : string
(** [sidenote_div_class] is the CSS class list for a sidenote. *)

val to_html : ctx:Arod_ctx.t -> string -> string * sidenote list
(** [to_html ~ctx content] is the rendered article and its sidenotes. *)

val to_plain_html : ctx:Arod_ctx.t -> string -> string
(** [to_plain_html ~ctx content] is rendered HTML with Bushel links but no
    sidenotes. *)

val to_atom_html : ctx:Arod_ctx.t -> string -> string
(** [to_atom_html ~ctx content] is feed-safe HTML with numbered footnotes. *)

type heading = {
  id : string;  (** Anchor id, matching the [id] on the rendered heading. *)
  level : int;  (** 2 or 3. *)
  number : string;  (** Section number, as ["2"] or ["2.1"]. *)
  text : string;  (** Heading text, with inline markup stripped. *)
}
(** A heading in a table of contents. *)

val extract_headings : string -> heading list
(** [extract_headings content] is its numbered h2 and h3 headings in document
    order. *)

(** {1 Utilities} *)

val html_escape_attr : string -> string
(** [html_escape_attr s] is [s] escaped for an HTML attribute. *)

val doi_to_id : string -> string
(** [doi_to_id doi] is a CSS-safe HTML identifier for [doi]. *)

val string_drop_prefix : prefix:string -> string -> string
(** [string_drop_prefix ~prefix s] is [s] without a leading [prefix]. *)

val with_feed_references :
  ctx:Arod_ctx.t -> Bushel.Note.t -> string -> string @@ portable
(** [with_feed_references ~ctx note html] is [html] followed by the references
    of [note] when it is a permanent or DOI entry. *)
