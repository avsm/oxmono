(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Apple-specific schemas for the workspace, network and memory handlers. *)

val of_ds4 : Ds4.Tool.t -> Agentkit_apple_fm.Tool.t
(** [of_ds4 tool] wraps a known handler with an Apple Foundation Models codec.
    Unsupported names raise [Invalid_argument]. The tool's description and
    handler remain those supplied by the application. *)

val supported : string -> bool
(** [supported name] is true when [of_ds4] has an Apple codec for [name]. *)

val context_size : string -> int
(** [context_size model] is the context size of ["default"]. Other model names
    are rejected. *)

val create :
  sw:Eio.Switch.t ->
  model:string ->
  system:string ->
  Ds4.Tool.t list ->
  Agentkit.Driver.session * int
(** [create ~sw ~model ~system tools] builds an Apple agent with [tools]. It
    refuses a tool without an Apple codec. It returns the session and context
    size. [model] must be ["default"]. *)
