(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term

val component :
  ?initial_text:string Bonsai.t ->
  width:int Bonsai.t ->
  height:int Bonsai.t ->
  on_change:(string -> unit Effect.t) Bonsai.t ->
  local_ Bonsai.graph ->
  (View.t
  * (Event.t -> Captured_or_ignored.t Effect.t)
  * (View.t -> Position.t option))
  Bonsai.t
(** [component ~width ~height ~on_change graph] edits a multiline reply with
    Bonsai's standard editor, Unicode cursor movement, undo and buffered paste.
    Keep the component keyed by message identity to retain drafts and cursors.
*)
