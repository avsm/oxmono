open! Core
open! Ppxlib
open Ppx_html_syntax

val code
  :  loc:Location.t
  -> html_syntax_module:Ppxlib.Longident.t loc option
  -> runtime_kind:Runtime_kind.t
  -> Model.Node.t list
  -> expression
