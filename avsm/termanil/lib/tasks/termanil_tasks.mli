(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val task : ?store:Dooit.Store.t -> Dooit.Doc.t -> Termanil_model.task
val list : Dooit.Store.t -> Termanil_model.task list * string list

val capture :
  ?tags:string list ->
  Dooit.Store.t ->
  Termanil_model.message ->
  Termanil_model.task list
(** [capture store message] uses Dooit's service/account/email identity.
    Repeated capture returns existing tasks, including completed tasks. *)

val complete : Dooit.Store.t -> Termanil_model.task -> Termanil_model.task
(** [complete store task] uses the displayed revision and the operation journal.
    A concurrent human or agent edit is refused and retained. *)

val sync : dry_run:bool -> Dooit.Store.t -> Dooit.Remote.t -> string list
