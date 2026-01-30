open! Core
open! Ppxlib

val ocaml_expression : string Ppxlib.Loc.t -> expression
val of_string : loc:Location.t -> string -> Model.Node.t list
