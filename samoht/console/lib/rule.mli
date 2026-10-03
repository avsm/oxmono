(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal rules with optional styled labels. *)

type align = [ `Left | `Center | `Right ]
(** Label alignment within a rule. *)

type t
(** A rule with an exact terminal-cell width. *)

val v :
  ?char_width:(Uchar.t -> int) ->
  ?theme:Theme.t ->
  ?style:Style.t ->
  ?glyph:string ->
  ?align:align ->
  ?label:Span.t ->
  width:int ->
  unit ->
  t
(** [v ~width ()] is a [width]-cell horizontal rule. [glyph] defaults to the
    Unicode horizontal line, and must occupy exactly one terminal cell. A
    [label] is separated from the rule by spaces and aligned as requested. An
    explicit [style] wins; otherwise a theme supplies its accent colour and an
    unthemed rule is dim.

    {b Raises.} [Invalid_argument] if [width] is negative, [glyph] is not one
    safe terminal cell, or [label] is wider than [width]. *)

val pp : t Fmt.t
(** [pp] renders the rule, following the formatter's style renderer. *)

val to_string : t -> string
(** [to_string t] is the plain rule. *)

val to_ansi_string : t -> string
(** [to_ansi_string t] is the rule with ANSI styling. *)

val anim : t -> string Anim.t
(** [anim t] is the ANSI-rendered rule as a still animation. *)
