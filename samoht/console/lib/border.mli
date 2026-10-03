(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Border styles for panels and tables.

    Provides ASCII and Unicode box-drawing character sets. *)

(** {1:types Types} *)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  cross : string;  (** Intersection of horizontal and vertical lines *)
  top_cross : string;  (** T pointing down (top edge intersection) *)
  bottom_cross : string;  (** T pointing up (bottom edge intersection) *)
  left_cross : string;  (** T pointing right (left edge intersection) *)
  right_cross : string;  (** T pointing left (right edge intersection) *)
}
(** Box-drawing characters. *)

type t
(** A border specification with characters and styling. *)

val v : ?style:Style.t -> chars -> t
(** [v ?style chars] is a border with the given character set. *)

val chars : t -> chars
(** [chars t] is [t]'s box-drawing character set. *)

val style : t -> Style.t
(** [style t] is the style applied to [t]'s characters. *)

(** {1:predefined_borders Predefined borders} *)

val none : t
(** [none] is the border drawn with empty strings. *)

val ascii : t
(** [ascii] is the border drawn with [+], [-] and [|]. *)

val single : t
(** [single] is the border drawn with Unicode single-line box-drawing
    characters. *)

val double : t
(** [double] is the border drawn with Unicode double-line box-drawing
    characters. *)

val rounded : t
(** [rounded] is the border drawn with Unicode rounded-corner box-drawing
    characters. *)

val heavy : t
(** [heavy] is the border drawn with Unicode heavy-line box-drawing characters.
*)

val hidden : t
(** [hidden] is the border drawn with spaces, which keeps the spacing of a
    border without showing it. *)

(** {1:styling Styling} *)

val with_style : Style.t -> t -> t
(** [with_style style border] applies a style to the border characters. *)

(** {1:operations Operations} *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] have the same characters and style.
*)

val pp : t Fmt.t
(** [pp ppf border] renders the border character set for diagnostics. *)
