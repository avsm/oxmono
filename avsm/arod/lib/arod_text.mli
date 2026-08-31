(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plain-text extraction from HTML. *)

@@ portable

val strip_html : string -> string
(** [strip_html s] is [s] without HTML tags. *)

val collapse_whitespace : string -> string
(** [collapse_whitespace s] is [s] with whitespace runs collapsed. *)

val html_unescape : string -> string
(** [html_unescape s] is [s] with the HTML entities emitted by Arod decoded. *)

val truncate : int -> string -> string
(** [truncate n s] is [s] limited to [n] characters, with an ellipsis when
    truncated. *)

val plain_summary : ?max_len:int -> string -> string option
(** [plain_summary ?max_len html] is a plain-text summary of [html], or [None]
    if it contains no text. [max_len] defaults to 150. *)
