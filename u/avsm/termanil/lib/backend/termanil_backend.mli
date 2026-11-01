(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module Config = Config

val execute :
  Config.t -> Termanil_model.request -> (Termanil_model.response, string) result
(** [execute config request] runs one native Eio operation in the worker. No Eio
    capability crosses the process boundary. Mail credentials use the shared
    JMAP profiles. Contacts are read-only. Task writes use Dooit's revision
    checks and sync journal. *)
