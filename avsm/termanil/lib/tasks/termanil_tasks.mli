(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val task : ?store:Dooit.Store.t -> Dooit.Doc.t -> Termanil_model.task
val list : Dooit.Store.t -> Termanil_model.task list * string list

val capture :
  ?tags:string list ->
  Dooit.Client.t ->
  Termanil_model.message ->
  Termanil_model.task list
(** [capture client message] uses Dooit's service/account/email identity.
    Repeated capture returns existing tasks, including completed tasks. New
    tasks use the client's synchronized write policy. *)

val complete : Dooit.Client.t -> Termanil_model.task -> Termanil_model.task
(** [complete client task] uses the displayed revision and the operation
    journal. A concurrent human or agent edit is refused and retained. *)
