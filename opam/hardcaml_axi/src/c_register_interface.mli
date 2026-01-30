(** C-code generators for a memory-mapped register space. *)

open Base
open Hardcaml

module Make (Regs : Interface.S_with_ast) : sig
  (** Output a C structure representing the Hardcaml interface. *)
  val output_struct_type
    :  ?don't_add_trailing_semicolon:bool
    -> ?indent:string
    -> ?typ_name:string
    -> Stdio.Out_channel.t
    -> unit

  (** Output address offset of a Hardcaml interface. Assumes that there is a register
      interface of [typ_name] created prior. If [c90] is false gcc style initializers are
      used. *)
  val output_struct_address_offsets
    :  ?c90:bool (** default is [true] *)
    -> ?indent:string
    -> ?typ_name:string
    -> ?name:string
    -> Stdio.Out_channel.t
    -> unit
end

module Make_read_write (Read : Interface.S_with_ast) (Write : Interface.S_with_ast) : sig
  (** Output a C union representing the Hardcaml read/write interface. *)
  val output : ?name:string -> Stdio.Out_channel.t -> unit
end
