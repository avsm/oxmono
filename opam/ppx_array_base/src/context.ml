open! Ppxlib
open Ast_builder.Default

type t =
  | Base
  | Deriving of core_type

let runtime_fun t loc label =
  let longident =
    match t with
    | Base -> label
    | Deriving _ -> "Ppx_array_runtime." ^ label
  in
  pexp_ident ~loc (Loc.make ~loc (Longident.parse longident))
;;

let how_to_vary_kinds t ~input ~output : How_to_vary_kinds.t =
  let input : _ How_to_vary_kinds.Whether_to_vary.t =
    match t with
    | Base -> Vary { kinds = input }
    | Deriving input_type -> Do_not_vary input_type
  in
  let output : _ How_to_vary_kinds.Whether_to_vary.t =
    match output with
    | None -> Do_not_vary ()
    | Some output -> Vary { kinds = output }
  in
  { input; output }
;;
