(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term

val component :
  handler:(Event.t -> unit Effect.t) Bonsai.t ->
  can_batch:(Event.t -> bool) Bonsai.t ->
  ready:(Event.t -> bool) Bonsai.t ->
  local_ Bonsai.graph ->
  (Event.t -> unit Effect.t) Bonsai.t
(** [component ~handler ~can_batch graph] queues input in arrival order and
    crosses unbatchable events one per frame. Consecutive batchable events use
    the same handler snapshot, with at most 4096 events per frame. An event
    waits at the head of the queue until [ready] allows it to run. *)
