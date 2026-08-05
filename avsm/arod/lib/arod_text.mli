(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Plaintext extraction helpers for HTML content. *)

val strip_html : string -> string
(** Remove all HTML tags, keeping only text content. *)

val collapse_whitespace : string -> string
(** Collapse runs of whitespace (spaces, newlines, tabs) into single spaces. *)

val truncate : int -> string -> string
(** [truncate n s] returns [s] if [String.length s <= n], otherwise the first
    [n] characters followed by an ellipsis. *)

val plain_summary : ?max_len:int -> string -> string option
(** [plain_summary ?max_len html] strips HTML tags, collapses whitespace, trims,
    and truncates to [max_len] (default 150). Returns [None] if the result is empty. *)
