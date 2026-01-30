open! Core
open! Ppxlib
open Ppx_html_syntax
open Model

module Type : sig
  type t =
    | String
    | Attr of { interpolation_kind : Interpolation_kind.t }
    | Node of { interpolation_kind : Interpolation_kind.t }
    | Argument

  val core_type : loc:location -> runtime_kind:Runtime_kind.t -> t -> core_type option
end

type modul := longident loc option

val expr
  :  ?type_:Type.t
  -> html_syntax_module:modul
  -> runtime_kind:Runtime_kind.t
  -> Model.Expr.t
  -> expression

val quote
  :  html_syntax_module:modul
  -> runtime_kind:Runtime_kind.t
  -> Model.Quote.t
  -> expression

val with_immediate_quote : Model.Quote.t -> f:(Model.Quote.t -> expression) -> expression
