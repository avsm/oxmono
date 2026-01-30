open! Base
open! Ppxlib
open! Ast_builder.Default
include Expander.S with type input := expression and type t := expression

val expand_local : expression -> loc:location -> expression
