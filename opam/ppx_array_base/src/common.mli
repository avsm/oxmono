open! Ppxlib
open! Stdppx

val raise_unsupported : location -> why:label -> _

val validate_cannot_overwrite_output_kinds
  :  location
  -> overwrite_output_kinds:expression option
  -> function_name:label
  -> unit

val map_type_variables
  :  core_type
  -> core_type * (label * Ppxlib_jane.Shim.jkind_annotation option) list

val map_type_decl_variables : type_declaration -> type_declaration
