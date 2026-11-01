(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
val service : string
val account : string

val connect : sw:Eio.Switch.t -> root:string -> Jmap_eio.Client.t
(** [connect ~sw ~root] connects the production JMAP client to an embedded
    protocol server using an in-process Fetch transport. Synthetic messages,
    flags and accepted submissions persist in [root]. No socket or real mail
    service is used. *)
