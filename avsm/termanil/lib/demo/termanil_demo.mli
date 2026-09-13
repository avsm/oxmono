(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

val messages : Termanil_model.message list
val contacts : Termanil_model.contact list

val create :
  unit -> Termanil_model.request -> (Termanil_model.response, string) result
(** [create ()] is a deterministic, isolated in-memory backend. It never
    accesses files, credentials or the network. *)

val body : string -> string
