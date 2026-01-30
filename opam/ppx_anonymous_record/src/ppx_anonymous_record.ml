open! Base
open! Ppxlib
open! Ast_builder.Default

let anon = [%string "%{Common.ppx_anonymous_record}.anon"]
let anon_local = [%string "%{anon}.local"]

let () =
  Driver.register_transformation
    Common.ppx_anonymous_record
    ~extensions:
      [ Extension.declare
          anon
          Extension.Context.pattern
          Pattern.payload
          (fun ~loc ~path:_ pattern ->
             Merlin_helpers.hide_pattern (Pattern.expand ~loc pattern))
      ; Extension.declare
          anon
          Extension.Context.expression
          Expression.payload
          (fun ~loc ~path:_ expression ->
             Merlin_helpers.hide_expression (Expression.expand ~loc expression))
      ; Extension.declare
          anon_local
          Extension.Context.expression
          Expression.payload
          (fun ~loc ~path:_ expression ->
             Merlin_helpers.hide_expression (Expression.expand_local ~loc expression))
      ; Extension.declare
          anon
          Extension.Context.core_type
          Core_type.payload
          (fun ~loc ~path:_ expression -> Core_type.expand ~loc expression)
      ]
;;

module For_odoc = struct
  module Expander = Expander
end
