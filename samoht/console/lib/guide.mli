(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Guide glyphs for nested terminal views. *)

type t
(** The four glyphs used to draw one level of nesting. *)

val v : branch:string -> last:string -> pipe:string -> space:string -> t
(** [v ~branch ~last ~pipe ~space] is a guide with the given glyphs, drawn
    unstyled ({!with_style} styles it). *)

val branch : t -> string
(** [branch t] is the non-final child connector. *)

val last : t -> string
(** [last t] is the final child connector. *)

val pipe : t -> string
(** [pipe t] is the continuation for an ancestor with following siblings. *)

val space : t -> string
(** [space t] is the continuation for a final ancestor. *)

val style : t -> Style.t
(** [style t] is the style [t]'s glyphs are drawn in. A gradient in it is laid
    over the tree's own cells, row [0] being the root's. *)

val with_style : Style.t -> t -> t
(** [with_style s t] is [t] drawn in [s]. *)

val ascii : t
(** [ascii] uses portable ASCII glyphs. *)

val unicode : t
(** [unicode] uses Unicode box-drawing glyphs. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if every glyph and the style are equal. *)

val pp : t Fmt.t
(** [pp] prints the four glyphs for diagnostics. *)
