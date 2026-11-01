(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What the journal says about the tasks.

    Nothing in the schedule file is state. Whether a task has fired, whether a
    [once] task is done and whether a [run_now] has been honoured are read out
    of the journal, which is what keeps a firing idempotent under a reread and
    survivable across a restart. This is that read. *)

type fired = {
  due : float;  (** the due time the last firing was for *)
  serial : int;  (** the [run_now] serial that firing carried, and 0 for none *)
}
(** The last [wake] record for one task. *)

type t
(** What each task last did. *)

val read : _ Eio.Path.t -> t
(** [read dir] reads the whole journal in [dir] for the last [wake] record of
    each task.

    Every segment is read, since a task that fires monthly has its last firing
    months back and a schedule that fired it again would be firing it twice. It
    is done once, at startup. *)

val empty : t
(** [empty] is what a journal with no [wake] record in it says. *)

val fired : t -> string -> fired option
(** [fired t id] is what the task [id] last did, and [None] if it never has. *)

val note : t -> string -> fired -> unit
(** [note t id f] records that [id] has fired, so that a poll in the same run
    does not fire it again before the journal is read afresh. *)
