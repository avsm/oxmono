open! Base
open Ppxlib

let ghostify ~loc = { loc with loc_ghost = true }

let infer_module { pexp_desc = desc; pexp_loc = loc; _ } =
  let lident txt = { txt = Lident txt; loc = ghostify ~loc } in
  match desc with
  | Pexp_constant (Pconst_unboxed_integer (_, 'l')) -> Some (lident "Int32_u")
  | Pexp_constant (Pconst_unboxed_integer (_, 'L')) -> Some (lident "Int64_u")
  | Pexp_constant (Pconst_unboxed_integer (_, 'n')) -> Some (lident "Nativeint_u")
  | _ -> None
;;

let exclusive = Attribute.declare_flag "for.exclusive" Expression

let expand ~loc ~path:(_ : string) ~arg ~idx ~from ~to_ ~dir ~expr =
  let loc = ghostify ~loc in
  let module_ =
    match arg with
    | Some { txt; loc } -> Some { txt; loc = ghostify ~loc }
    | None -> Option.first_some (infer_module from) (infer_module to_)
  in
  match module_ with
  | None ->
    Ast_builder.Default.pexp_extension
      ~loc
      (Location.error_extensionf ~loc "couldn't infer unboxed integer module")
  | Some module_ ->
    let pidx =
      let { txt; loc } = idx in
      let loc = ghostify ~loc in
      Ast_builder.Default.pvar ~loc txt
    in
    let pexp_module_dot f =
      Ast_builder.Default.pexp_ident
        ~loc:module_.loc
        { module_ with txt = Ldot (module_.txt, f dir) }
    in
    let step =
      pexp_module_dot (function
        | Upto -> "succ"
        | Downto -> "pred")
    in
    let neq = pexp_module_dot (fun _ -> "<>") in
    let cmp =
      pexp_module_dot (function
        | Upto -> "<"
        | Downto -> ">")
    in
    let to_, to_is_excl =
      match Attribute.consume exclusive to_ with
      | None -> to_, false
      | Some (to_, ()) -> to_, true
    in
    let to_excl =
      match to_is_excl with
      | false -> [%expr [%e step] to_]
      | true -> [%expr to_]
    in
    let should_loop =
      match to_is_excl with
      | false -> [%expr Stdlib.not ([%e cmp] to_ from)]
      | true -> [%expr [%e cmp] from to_]
    in
    [%expr
      let from = [%e from]
      and to_ = [%e to_]
      and[@inline] local_ for_loop_body [%p pidx] = [%e expr] in
      if [%e should_loop]
      then (
        let to_excl = [%e to_excl] in
        let rec local_ for_loop idx = exclave_
          for_loop_body idx;
          let idx = [%e step] idx in
          (* Using neq rather than < is important -- [to_excl] might wrap. *)
          if [%e neq] idx to_excl then for_loop idx
        in
        for_loop from [@nontail])]
;;

let extension =
  Extension.declare_with_path_arg
    "for.loop"
    Expression
    Ast_pattern.(
      let index =
        ppat_var __' ||| map0' ppat_any ~f:(fun loc -> { txt = "__idx"; loc })
      in
      single_expr_payload (pexp_for index __ __ __ __)
      |> map ~f:(fun f idx from to_ dir expr -> f ~idx ~from ~to_ ~dir ~expr))
    expand
;;

let () = Driver.register_transformation "for_loop" ~extensions:[ extension ]
