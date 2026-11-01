(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Row layouts: the representation behind {!Progress.Layout}, which documents
    the values both have. *)

type kind =
  | Spinner
  | Mark  (** The mark of how the task ended. *)
  | Label of Style.t
  | Bar of int option  (** A bar that many cells wide, or that grows. *)
  | Percent
  | Count
  | Rate
  | Eta
  | Elapsed
  | Spacer
  | Const of View_core.t
  | Custom of (Task_core.t -> View_core.t)
  | Choose of (Task_core.t -> piece list)

and piece = { kind : kind; stage : int option; blank : bool }
(** The type for the pieces of a row: what each shows, the stage at which it
    gives way, if it does, and whether it is blank text. *)

type t
(** The type for layouts: at most one {!constructor-Label} and one
    {!constructor-Spacer}. *)

val pieces : t -> piece list
(** [pieces l] are the pieces of [l], left to right. *)

val for_task : t -> Task_core.t -> piece list
(** [for_task l r] are the pieces of [l] on the row of [r], each
    {!constructor-Choose} replaced by the pieces it chooses for [r].

    @raise Invalid_argument if they hold two labels or two spacers. *)

val has_label : piece list -> bool
(** [has_label ps] is [true] iff [ps] hold a {!constructor-Label}. *)

val reads : t -> Look.t -> View_core.kind
(** [reads l look] is what a row that [l] draws in [look] reads: the tasks and
    the time, and any state with a {!constructor-Custom} piece or a
    {!constructor-Const} view that reads it. *)

val spinner : t
val label : ?style:Style.t -> unit -> t
val bar : ?width:int -> unit -> t
val percent : t
val count : t
val rate : t
val eta : t
val elapsed : t
val spacer : t
val const : View.t -> t
val custom : (Task_core.t -> View.t) -> t
val choose : (Task_core.t -> t) -> t
val give_way : int -> t -> t
val ( ++ ) : t -> t -> t
val list : ?sep:t -> t list -> t
val default : t

val ended : t
(** [ended] is {!default} with the mark of how the task ended in place of the
    spinner: the line of a task that ended, in the columns of the rows. *)

val pp : t Fmt.t
(** [pp] formats a layout for diagnostics, in an unspecified format. *)
