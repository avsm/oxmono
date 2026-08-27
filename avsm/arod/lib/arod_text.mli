(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plaintext extraction helpers for HTML content.

    The floating [@@ portable] is scaffolding. Nothing portable calls into
    this module yet, because the renders that would are blocked by what
    {!Arod_render}'s header names. It is here so that the chain is paid ahead
    of them rather than found again. *)

@@ portable

val strip_html : string -> string
(** Remove all HTML tags, keeping only text content. *)

val collapse_whitespace : string -> string
(** Collapse runs of whitespace (spaces, newlines, tabs) into single spaces. *)

val html_unescape : string -> string
(** [html_unescape s] is [s] with [&lt;], [&gt;], [&quot;], [&#39;], [&apos;]
    and [&amp;] decoded. No other entity is recognised, since the site's own
    renderer emits only these. *)

val truncate : int -> string -> string
(** [truncate n s] returns [s] if [String.length s <= n], otherwise the first
    [n] characters followed by an ellipsis. *)

val plain_summary : ?max_len:int -> string -> string option
(** [plain_summary ?max_len html] strips HTML tags, decodes entities, collapses
    whitespace, trims, and truncates to [max_len] (default 150). The result is
    text, so a caller puts it in a text node rather than back into markup.
    Returns [None] if the result is empty. *)
