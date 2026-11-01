(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tree of a task, as {!Tree.view} draws it. {!Progress} documents the
    rules; this module gives the view. *)

val draws_label : Progress_layout.t -> Task_core.t -> bool
(** [draws_label l r] is [true] iff the row of [r] by [l] draws its label. *)

val view : layout:Progress_layout.t -> Task_core.t -> View_core.t
(** [view ~layout r] is the tree of [r] laid out in its room: [r] and the tasks
    under it, or the top tasks if [r] is the root. In a room of a height, the
    tasks that ended under each task fold into one line when they do not fit,
    then the lines of the windows give way, the oldest of all first, then only
    the latest running tasks show, after a line that counts the others. Without
    a height, the tree is whole, as the scrollback keeps it: every task, none
    folded, each failure or cancellation with its output under it, a label too
    wide going on over the next lines. *)
