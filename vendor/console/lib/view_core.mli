(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Views, with the operations the widgets and the display share. {!View} is
    their public face and documents the values both have. *)

type t
(** The type for views. *)

type align = [ `Left | `Center | `Right ]
(** See {!View.align}. *)

(** {1:room Room and time} *)

type kind = { live : bool; tasks : bool; timed : bool }
(** The type for what a deferred view reads besides its room: state outside the
    view, which changes between frames, the tasks among it, and the time. A view
    that reads none caches its layout for its room. *)

val static : kind
(** [static] reads none. *)

val timed : kind
(** [timed] reads the time alone. *)

val tasks : kind
(** [tasks] reads the tasks and the time, and no other state. *)

val moving : kind
(** [moving] reads any state and the time. *)

val join : kind -> kind -> kind
(** [join k0 k1] reads what [k0] or [k1] reads. *)

val deferred :
  ?kind:(Look.t -> kind) ->
  ?grow:int ->
  ?min_width:int ->
  ?still:bool ->
  (Ctx.t -> t) ->
  t
(** [deferred ~kind ~grow ~min_width ~still lay] is the view [lay c] where it is
    laid out in [c]. [kind] (defaults to {!static} in every look) says what it
    reads, [grow] (defaults to [0]) its weight in an {!hcat} that shares a room,
    [min_width] the width below which it should not be laid out. With [still]
    (defaults to [false]), every frame takes the cells of its frame at time
    [0.], each line cut or padded to them. *)

val kind : t -> Look.t -> kind
(** [kind v look] is what [v] reads in [look]: what any of its parts reads. *)

val kinds : t list -> Look.t -> kind
(** [kinds vs look] is what any of [vs] reads in [look]. *)

val is_flat : t -> bool
(** [is_flat v] is [true] iff [v] has no deferred part. *)

val min_width : Ctx.t -> t -> int
(** [min_width c v] is the width below which [v] should not be laid out in [c]:
    the widest word of a flat view, the hint of a deferred one, or else the
    [min_width] of the view it lays out in [c]. *)

val resolve : Ctx.t -> t -> t
(** [resolve c v] is [v] laid out in [c], with no deferred part. A part drawn in
    its look ({!drawn}) stays one. *)

(** {1:views Views} *)

val empty : t
val is_empty : t -> bool
val text : string -> t

val ink : Style.t -> string -> t
(** [ink style s] is [styled style (text s)]. *)

val link : uri:string -> t -> t
val styled : Style.t -> t -> t
val role : Theme.role -> t -> t
val gradient : ?colors:Color.t list -> t -> t
val ( ++ ) : t -> t -> t
val concat : ?sep:t -> t list -> t
val hcat : ?gap:int -> t list -> t
val vcat : ?gap:int -> t list -> t
val fit : ?min_width:int -> ?grow:int -> (Ctx.t -> t) -> t
val sized : ?width:int -> ?height:int -> t -> t
val place : ?align:align -> ?width:int -> ?height:int -> t -> t
val width : t -> int
val height : t -> int

val ellipsis : string
(** [ellipsis] is U+2026. *)

val truncate : ?ellipsis:string -> int -> t -> t
(** [truncate ~ellipsis w v] is like {!View.truncate} but ends a cut line with
    [ellipsis] (defaults to the {!Look.ellipsis} of the look it is printed in).
    An ellipsis wider than [w] cells keeps only its first grapheme clusters that
    fit in them. *)

val crop : ?left:int -> ?width:int -> t -> t
val wrap : int -> t -> t list
val pp : t Fmt.t
val to_string : ?styled:bool -> t -> string

(** {1:widgets For the widgets}

    These lay a deferred view out at its natural size, in {!Look.default} at
    time [0.], when they are given one. *)

val lines : t -> t list
(** [lines v] are the lines of [v], without their line feeds. It is never empty.
*)

val block : t list -> t
(** [block lines] is [lines] one under the other, as one block: a display whose
    region is too short for all of a view drops the lines of a block together.
*)

val seamed : t -> (bool * t) list
(** [seamed v] are the {!lines} of [v], each after [true] iff it goes on the
    {!block} of the line above it. *)

val pad_to : int -> t -> t
(** [pad_to w line] is the line [line] cut to its first [w] cells, between two
    clusters, then padded with spaces to exactly [w] cells. *)

val color_at : Color.t list -> float -> Color.t
(** [color_at stops p] is the colour at [p], clamped to 0 to 1, of [stops]
    spread evenly from 0 to 1: the {!Color.blend} of the two stops around [p],
    or the stop at [p] itself. [stops] is not empty. *)

val wrap_hanging : int -> t -> t list
(** [wrap_hanging w v] is like {!wrap} but the lines after the first of each
    line start under the text after its first word, at most [w / 2] cells in. *)

val chop : int -> t -> t list
(** [chop w v] is like {!wrap} but breaks each line of [v] every [w] cells,
    mid-word, as a terminal wraps a line wider than it. *)

val widest_word : t -> int
(** [widest_word v] is the width of the widest run of [v] without a space. *)

val drawn : (Look.t -> t) -> t
(** [drawn draw] is the view [draw look] when it is printed in [look], and
    measured as [draw Look.default]. Every [draw look] must take the same cells
    as [draw Look.default], one cluster for one. A part of it that {!truncate}
    or {!wrap} keep is drawn as the same cells of [draw look]. *)

val paint : Look.t -> t -> t
(** [paint look v] is [v] with each {!drawn} part drawn in [look]. *)

val recell :
  (cell:int -> (Style.t -> string -> Style.t * string) option) -> t -> t
(** [recell f line] is the line [line] with each cluster [g] in style [s] that
    starts [cell] cells in, when [f ~cell] is [Some r], drawn as [r s g]: a new
    style and glyph. A glyph that does not take the cells of [g], or holds a
    control character, is [g]; a cluster of a {!drawn} part is kept. *)

val render : look:Look.t -> ?depth:Color.depth -> t -> string
(** [render ~look ~depth v] is [v] drawn in [look] with its styles at [depth],
    or its text alone without [depth]. *)

val pp_lines : t list Fmt.t
(** [pp_lines] formats views one per line, with no line end after the last. *)

val string_of_lines : ?styled:bool -> t list -> string
(** [string_of_lines lines] is [lines] joined by line feeds, each as
    {!to_string} gives it. *)
