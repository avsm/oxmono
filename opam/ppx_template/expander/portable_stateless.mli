open! Stdppx
open! Import

type t =
  | Portable
  | Stateless

val all : t list
val to_string : t -> string
val module_binding : t -> loc:Location.t -> mod_:module_binding -> structure_item
val module_declaration : t -> loc:Location.t -> mod_:module_declaration -> signature_item
