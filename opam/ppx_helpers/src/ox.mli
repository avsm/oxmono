open! Stdppx

include module type of struct
  include Modes_lib
end

module Modes : sig
  include module type of struct
    include Modes
  end

  val default : t
  val local : t
end

val of_ast_modes : Ppxlib_jane.modes -> Modes.t

val to_ast_modes
  :  ?simplify:bool (** default [true] *)
  -> Modes.t
  -> loc:Ppxlib.location
  -> Ppxlib_jane.modes

val of_ast_modalities : mutable_implied:bool -> Ppxlib_jane.modalities -> Modalities.t

val to_ast_modalities
  :  ?simplify:bool (** default [true] *)
  -> mutable_implied:bool
  -> Modalities.t
  -> loc:Ppxlib.location
  -> Ppxlib_jane.modalities

val extract_modalities_from_label_declaration
  :  ?handle_globalized:bool (** default [true] *)
  -> Ppxlib.label_declaration
  -> Modalities.t

val extract_modalities_from_construct_arguments
  :  ?handle_globalized:bool (** default [true] *)
  -> Ppxlib_jane.Shim.Pcstr_tuple_arg.t
  -> Modalities.t

(** [@@deriving my_ppx ~modes:[ local; shared ]] produces
    {[
      [ [ Global; Uncontended ]
      ; [ Local; Uncontended ]
      ; [ Global; Shared ]
      ; [ Local; Shared ]
      ]
    ]} *)
val deriving_arg_modes : Modes.t list option Ppxlib.Deriving.Args.param

(** [@@deriving my_ppx ~localize] produces [[[ Local ]]] *)
val deriving_arg_localize : Modes.t list option Ppxlib.Deriving.Args.param

module Attr : sig
  val globalized_ct : Ppxlib.core_type Ppxlib.Attribute.flag
  val globalized_ld : Ppxlib.label_declaration Ppxlib.Attribute.flag
end
