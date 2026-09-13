(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Bonsai_term

val content_height : fields:(string * string) list -> width:int -> int

val render :
  fields:(string * string) list ->
  width:int ->
  height:int ->
  scroll:int ->
  View.t

val component :
  fields:(string * string) list Bonsai.t ->
  width:int Bonsai.t ->
  height:int Bonsai.t ->
  scroll:int Bonsai.t ->
  local_ Bonsai.graph ->
  View.t Bonsai.t
(** [component ~fields ~width ~height ~scroll graph] displays labelled metadata
    with terminal-safe text, Unicode wrapping and bounded scrolling. *)
