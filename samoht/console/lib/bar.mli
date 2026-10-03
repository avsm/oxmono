(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Horizontal progress bars.

    A bar fills left to right to a percentage. {!render} draws a determinate bar
    and {!at} a moving, indeterminate one -- the bar analogue of {!Spinner}. The
    look (blocky, smooth or a rainbow gradient) and the fill colour come from a
    {!Theme.t}, or are given explicitly. {!Console.Display} draws a row's bar
    through this module. *)

type style = Theme.bar
(** A bar's look: [`Blocky] (full blocks on a shaded track), [`Smooth]
    (fractional eighth-cell fills) or [`Rainbow] (a spectrum gradient). *)

type color = Color.t
(** The fill colour. *)

val render :
  ?theme:Theme.t ->
  ?style:style ->
  ?color:color ->
  ?styled:bool ->
  width:int ->
  pct:int ->
  unit ->
  string
(** [render ~width ~pct ()] is a [width]-cell bar filled to [pct] (clamped to
    the range 0 to 100). The look comes from the style argument if given, else
    [theme]'s, else [`Smooth]; the fill from the color argument if given, else
    [theme]'s accent, else {!Color.cyan}. [styled] (default [true]) is whether
    the bar carries ANSI: [false] renders the same glyphs with no SGR at all,
    for a target whose [Fmt.style_renderer] is [`None]. {b Raises.}
    [Invalid_argument] if [width] is negative. *)

val at :
  ?theme:Theme.t ->
  ?style:style ->
  ?color:color ->
  ?styled:bool ->
  width:int ->
  elapsed:float ->
  unit ->
  string
(** [at ~width ~elapsed ()] is a moving, indeterminate bar [elapsed] seconds
    into the animation: the fill sweeps up and back over a two-second period,
    for work whose progress is unknown. Style, colour and [styled] resolve as in
    {!render}. Non-finite or negative elapsed time renders the initial frame.
    {b Raises.} [Invalid_argument] if [width] is negative. *)

val anim :
  ?theme:Theme.t ->
  ?style:style ->
  ?color:color ->
  ?styled:bool ->
  width:int ->
  unit ->
  string Anim.t
(** [anim ~width ()] is the moving bar as an {!Anim.t}: its frame at [elapsed]
    is [at ~width ~elapsed ()]. Style, colour and [styled] resolve as in
    {!render}. *)
