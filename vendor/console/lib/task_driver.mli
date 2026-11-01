(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Running tasks for a scheduler.

    This module is internal to console: [Console] does not expose it, and
    [console-eio] reads it through [Console__].

    A driver runs the tasks of a root as [Console_eio.run] does for Eio: it says
    with a {!type-scope} which task is current in a fiber, passes the program's
    [Logs] messages to {!report}, and gives the current task what the program
    prints ({!current}). *)

type scope = Task_core.scope
(** The type for what the tasks know of their scheduler. *)

val scope :
  current:(unit -> Task.t option) ->
  within:(Task.t -> (unit -> unit) -> unit) ->
  cancelled:(exn -> bool) ->
  scope
(** [scope ~current ~within ~cancelled] is the scope in which:
    - [current ()] is the task current where it is called;
    - [within task f] calls [f] with [task] current there and in what [f]
      starts;
    - [cancelled e] is [true] iff [e] says that the fiber raising it was
      cancelled: a {!Task.run} whose function raises it cancels its task rather
      than failing it. *)

val root : ?label:string -> scope:scope -> now:(unit -> float) -> unit -> Task.t
(** [root ~scope ~now ()] is {!Task.root}[ ~now ()] in which [scope] says which
    task is current. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of messages, those of [Logs]. *)

val report : Task.t -> level -> string -> unit
(** [report root level text] gives each line of [text], a message of [level], to
    the task of [root]'s work current where it is called, as its output, or to
    [root] where none is. *)

val current : Task.t -> Task.t option
(** [current root] is the task of [root]'s work current where it is called, if
    it runs and is not the root. A driver gives it what the program prints
    meanwhile, with {!Task.log}. *)

val ending : Task.t -> (Task.t -> unit) -> unit
(** [ending root f] has [f task] called as each task of [root]'s work ends,
    before it ends, so that a driver gives it the last of its output. *)

val id : Task.t -> int
(** [id task] names [task] in its work: negative for the root. *)
