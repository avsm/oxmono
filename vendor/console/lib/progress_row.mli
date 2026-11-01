(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The lines that show tasks, in a look.

    A task is drawn three ways: {{!live}live}, a running task in a tree;
    {{!ended}ended}, a task that ended in a tree, in the columns of the live
    rows; and {{!appended}appended}, printed in turn without a terminal,
    compact. A line in a room is at most that wide; without one, it is whole. *)

open Task_core

(** {1:columns Columns} *)

val bar_max : int
(** [bar_max] is the widest a bar that grows gets, in cells. *)

val bar_min : int
(** [bar_min] is the narrowest a bar that grows gets before the pieces of its
    row give way. *)

type columns = {
  label : int;  (** The cells of the label column. *)
  lead : int;  (** The cells of the widest spinner frame or mark. *)
}
(** The type for the columns a row shares with the rows around it. *)

val lead : Look.t -> int
(** [lead look] is the cells of the widest spinner frame or mark of [look]. *)

val columns : Look.t -> t -> columns
(** [columns look r] are the columns of a row of [r] alone: its label column is
    as wide as the widest label of [r]'s work. *)

(** {1:rows Tasks} *)

val live :
  Look.t ->
  time:float ->
  ?width:int ->
  ?columns:columns ->
  Progress_layout.t ->
  t ->
  View_core.t
(** [live look ~time ~width ~columns layout r] is the row of the running task
    [r] at [time], packed from the left: its label padded to the label column of
    [columns], if given, its bar grown to fill the room up to {!bar_max} cells,
    and its label cut to the room the other pieces leave once its bar has shrunk
    to {!bar_min} cells and they have given way in the order of their stages. *)

val tail : Look.t -> t -> (int * View_core.t) list
(** [tail look r] are the lines of [r]'s window, oldest first, each after its
    {!field-seq}. *)

val ended :
  Look.t ->
  ?width:int ->
  ?columns:columns ->
  whole:bool ->
  t ->
  View_core.t list
(** [ended look ~width ~columns ~whole r] is the line of [r], which ended: the
    row {!Progress_layout.ended} draws, its mark, then its label, bar, count,
    rate and time in the columns of the live rows. A label that does not fit is
    cut, or with [whole] goes on over the lines below. *)

val output : Look.t -> width:int -> t -> View_core.t list
(** [output look ~width r] is the output [r] kept, whole: a line that does not
    fit goes on over the lines below. *)

val finished :
  Look.t -> failed:int -> cancelled:int -> int -> Theme.mark -> View_core.t
(** [finished look ~failed ~cancelled n m] is the line, after the mark [m], that
    stands for [n] tasks that ended. *)

val shed : Look.t -> int -> View_core.t
(** [shed look n] says that [n] tasks were dropped. *)

val more : Look.t -> int -> View_core.t
(** [more look n] says that [n] more tasks run. *)

(** {1:lines Lines printed in turn} *)

val started : Look.t -> t -> View_core.t
(** [started look r] is the line printed when [r] starts without a terminal. *)

val appended : Look.t -> t -> View_core.t
(** [appended look r] is the line printed when [r] ends without a terminal. *)

val labelled : Look.t -> ?width:int -> t -> line -> View_core.t list
(** [labelled look ~width r l] is [l] printed above the region or in turn, after
    [r]'s path, folded to [width] if given. *)

val under : Look.t -> ?width:int -> line -> View_core.t list
(** [under look ~width l] is [l] printed under a task's line. *)

val dropped : Look.t -> int -> View_core.t
(** [dropped look n] says that [n] lines of output were dropped. *)

val message : Look.t -> ?width:int -> line -> View_core.t list
(** [message look ~width l] is the line [l] of no task, after its level's mark,
    folded to [width] if given. *)

(** {1:summary The summary and the verdict} *)

val state : t -> Theme.mark
(** [state root] is the mark of the work: running, or the worst of how the root
    and its tasks ended. *)

val summary : Look.t -> ?width:int -> t -> View_core.t option
(** [summary look ~width root] is the summary line of [root], if it has a label.
    In a room, its time and count give way before the label. *)

val verdict : Look.t -> ?width:int -> t -> View_core.t list
(** [verdict look ~width root] is the line of the message the work ended with,
    if any. *)
