open! Core
open Ppxlib

let declare name pattern k =
  Attribute.declare ("quick_test." ^ name) Value_binding pattern k
;;

let consume attr node =
  match Attribute.consume attr node with
  | None -> node, None
  | Some (node, result) -> node, Some result
;;

module Passthroughs = struct
  type t = expression Map.M(Quick_test_parameter).t

  let declare name parameter =
    declare name Ast_pattern.(single_expr_payload __) (fun value -> parameter, value)
  ;;

  let passthroughs =
    let open Quick_test_parameter in
    List.map
      ~f:(fun (name, parameter) -> declare name parameter)
      [ "config", Config; "trials", Trials; "cr", Cr; "hide_positions", Hide_positions ]
  ;;

  let consume value_binding =
    let value_binding, passthroughs =
      List.fold
        passthroughs
        ~init:(value_binding, [])
        ~f:(fun (value_binding, acc) attr ->
          let value_binding, t_opt = consume attr value_binding in
          let acc = List.append (Option.to_list t_opt) acc in
          value_binding, acc)
    in
    value_binding, Map.of_alist_exn (module Quick_test_parameter) passthroughs
  ;;
end

module Remember_failures = struct
  module One = struct
    type t =
      { expr : expression
      ; body : string
      ; body_location : location
      ; delimiters : string option
      }

    let pattern () =
      let open Ast_pattern in
      map
        (as__ (pexp_constant (pconst_string __ __ __)))
        ~f:(fun k expr body body_location delimiters ->
          k { expr; body; body_location; delimiters })
    ;;
  end

  type t =
    | Ignore
    | Empty of location
    | List of One.t Nonempty_list.t

  let remember_failures =
    declare
      "remember_failures"
      Ast_pattern.(
        map0' (pstr nil) ~f:(fun loc -> Empty loc)
        ||| map1 (single_expr_payload (One.pattern ())) ~f:(fun one -> List [ one ])
        ||| map2
              (single_expr_payload
                 (pexp_apply (One.pattern ()) (many (pair nolabel (One.pattern ())))))
              ~f:(fun hd tl -> List (hd :: tl)))
      Fn.id
  ;;

  let remember_failures_ignore =
    declare "@remember_failures.ignore" Ast_pattern.drop Ignore
  ;;

  let consume value_binding =
    let value_binding, remember_failures = consume remember_failures value_binding in
    let value_binding, remember_failures_ignore =
      consume remember_failures_ignore value_binding
    in
    match remember_failures, remember_failures_ignore with
    | None, None -> value_binding, Ignore
    | Some t, None | None, Some t -> value_binding, t
    | Some _, Some _ -> Location.raise_errorf ~loc:value_binding.pvb_loc "duplicate"
  ;;
end

module Examples = struct
  type t = expression option

  let attr = declare "examples" Ast_pattern.(single_expr_payload __) Fn.id
  let consume value_binding = consume attr value_binding
end

type t =
  { passthroughs : Passthroughs.t
  ; remember_failures : Remember_failures.t
  ; examples : Examples.t
  }

let consume value_binding =
  let value_binding, passthroughs = Passthroughs.consume value_binding in
  let value_binding, remember_failures = Remember_failures.consume value_binding in
  let value_binding, examples = Examples.consume value_binding in
  value_binding, { passthroughs; remember_failures; examples }
;;
