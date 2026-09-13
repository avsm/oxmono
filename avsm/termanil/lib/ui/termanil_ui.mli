(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term
module Reply_editor = Reply_editor
module Metadata = Metadata

val app :
  ?initial:Termanil_model.t ->
  ?autoload:bool ->
  execute:
    (Termanil_model.request ->
    (Termanil_model.response, string) result Effect.t) ->
  exit:(unit -> unit Effect.t) ->
  dimensions:Dimensions.t Bonsai.t ->
  local_ Bonsai.graph ->
  (view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t)
(** [app ~execute ~exit ~dimensions graph] has no filesystem or network effects
    of its own. [execute] is the sole backend boundary and is injectable in
    Bonsai terminal expect tests. *)

val render : Dimensions.t -> Termanil_model.t -> View.t
val key_action : Termanil_model.t -> Event.t -> Termanil_model.action option
val wrap : width:int -> string -> string list
