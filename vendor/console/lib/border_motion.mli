(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Frames that move: the border cells of a table or a panel drawn in their
    theme's {!Theme.motion}, by the time of the look they are printed in. *)

val frame :
  Look.t ->
  time:float ->
  Border.t ->
  left:int ->
  right:int ->
  ?gaps:(int * int * int) list ->
  View_core.t list ->
  View_core.t list
(** [frame look ~time border ~left ~right lines] is [lines], a frame of [border]
    laid out in [look] whose edges take [left] and [right] cells, with its
    border cells drawn as [look]'s motion says at [time], when [look] is live. A
    sweep that is not live keeps its colours at time [0.], and the other motions
    stand still. The border cells are those of the first and last lines and of
    the edges, but for each [(y, first, last)] of [gaps] (defaults to [[]])
    cells [first] to [last - 1] of line [y]. It is [lines] unchanged when the
    theme has no motion, an edge is empty, or the lines are fewer than two or
    not all as wide. *)
