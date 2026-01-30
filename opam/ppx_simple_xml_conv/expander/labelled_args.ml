open! Base
open! Ppxlib
include Ast_pattern

type ('a, 'result) t =
  | Required :
      { label : string
      ; pattern : (expression, 'a) Ast_pattern.Packed.t
      ; rest : ('b, 'result) t
      }
      -> ('a -> 'b, 'result) t
  | Optional :
      { label : string
      ; pattern : (expression, 'a) Ast_pattern.Packed.t
      ; rest : ('b, 'result) t
      }
      -> ('a option -> 'b, 'result) t
  | Nil : ('result, 'result) t

let rec run
  : type func result.
    (func, result) t
    -> expression_loc:Location.t
    -> unparsed_args:expression Map.M(String).t
    -> f:func
    -> result
  =
  fun t ~expression_loc ~unparsed_args ~f ->
  let extract label = Map.find unparsed_args label, Map.remove unparsed_args label in
  match t with
  | Nil ->
    (* We processed all the arguments required by our type. *)
    (match Map.to_alist unparsed_args with
     | [] -> f
     | (label, { pexp_loc = loc; _ }) :: _ ->
       Location.raise_errorf ~loc "unknown argument %S" label)
  | Required { label; pattern; rest } ->
    (match extract label with
     | None, _ ->
       Location.raise_errorf
         ~loc:expression_loc
         "argument %S required, not provided"
         label
     | Some expression, unparsed_args ->
       run
         rest
         ~expression_loc
         ~f:(f (Ast_pattern.Packed.parse pattern expression.pexp_loc expression))
         ~unparsed_args)
  | Optional { label; pattern; rest } ->
    let arg, unparsed_args = extract label in
    let arg =
      Option.map arg ~f:(fun expression ->
        Ast_pattern.Packed.parse pattern expression.pexp_loc expression)
    in
    run rest ~expression_loc ~f:(f arg) ~unparsed_args
;;

let ( ^-> ) (label, pattern, build_arg) rest =
  Required { label; pattern = Ast_pattern.Packed.create pattern build_arg; rest }
;;

let ( ^?-> ) (label, pattern, build_arg) rest =
  Optional { label; pattern = Ast_pattern.Packed.create pattern build_arg; rest }
;;

let ret = Nil

let declare
  (type f)
  ?(allow_no_args = false)
  attribute
  context
  (parse_func : (expression, f -> f, f) Ast_pattern.t)
  t
  f
  =
  let expression = pexp_apply __ (many (pack2 (pair (labelled __) __))) in
  let expression =
    if allow_no_args
    then
      expression
      ||| map __ ~f:(fun expression_to_result expression ->
        (* This just transforms the expression to look like it was applied with 0
           arguments. *)
        expression_to_result expression [])
    else expression
  in
  Attribute.declare attribute context (single_expr_payload expression) (fun func args ->
    let loc = func.pexp_loc in
    let func = Ast_pattern.parse parse_func loc func Fn.id in
    let args =
      match Map.of_alist (module String) args with
      | `Duplicate_key tag ->
        Location.raise_errorf ~loc "Duplicate argument provided %S" tag
      | `Ok args -> args
    in
    run t ~expression_loc:loc ~unparsed_args:args ~f:(f loc func))
;;

let just_label name =
  Ast_pattern.(name, map0 ~f:() (Ast_pattern.pexp_ident (lident (string name))), Fn.id)
;;
