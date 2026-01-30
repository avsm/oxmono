open! Ppxlib
open! Stdppx
open Ast_builder.Default

type t =
  | Map
  | Mapi
  | Iter
  | Iteri
  | Fold

let map = Map
let mapi = Mapi
let iter = Iter
let iteri = Iteri
let fold = Fold
let all = [ map; mapi; iter; iteri; fold ]

let module_ : t -> (module Function_types.S) = function
  | Map -> (module Map)
  | Mapi -> (module Mapi)
  | Iter -> (module Iter)
  | Iteri -> (module Iteri)
  | Fold -> (module Fold)
;;

let none = Ppx_helpers.ghoster#location Location.none
let output_kinds = "output_kinds"

let maybe_overwrite_output_kinds ({ ptyp_attributes; ptyp_loc; _ } as type_) =
  let attributes_we_care_about, other_attributes =
    List.partition_map
      (fun ({ attr_name; _ } as attribute) ->
        if String.equal attr_name.txt output_kinds
        then Left attribute
        else Right attribute)
      ptyp_attributes
  in
  match attributes_we_care_about with
  | [] -> type_, None
  | [ ({ attr_name; attr_payload = PStr [%str [%e? expr]]; _ } as attribute) ]
    when String.equal attr_name.txt output_kinds ->
    Ppxlib.Attribute.mark_as_handled_manually attribute;
    { type_ with ptyp_attributes = other_attributes }, Some expr
  | _ ->
    Common.raise_unsupported
      ptyp_loc
      ~why:
        "You can only have one [@output_kinds] attribute, and its payload must be valid \
         on the RHS of a ppx_template [@kind] binding, for example [@output_kinds \
         (immediate, float64)]"
;;

let rule t ~implementation ~interface =
  let (module F : Function_types.S) = module_ t in
  let name = "@array." ^ F.name in
  [ implementation ~name ~f:F.implementation; interface ~name ~f:F.interface ]
;;

let extensions t =
  let implementation ~name ~f =
    Extension.declare_inline
      name
      Extension.Context.structure_item
      Ast_pattern.(ptyp __)
      (fun ~loc ~path:_ type_ ->
        let type_, overwrite_output_kinds = maybe_overwrite_output_kinds type_ in
        f
          (Ppx_helpers.ghoster#location loc)
          (Context.Deriving (Ppx_helpers.ghoster#core_type type_))
          ~overwrite_output_kinds)
    |> Context_free.Rule.extension
  in
  let interface ~name ~f =
    Extension.declare
      name
      Extension.Context.signature_item
      Ast_pattern.(ptyp __)
      (fun ~loc ~path:_ type_ ->
        let type_, overwrite_output_kinds = maybe_overwrite_output_kinds type_ in
        f
          (Ppx_helpers.ghoster#location loc)
          (Context.Deriving (Ppx_helpers.ghoster#core_type type_))
          ~overwrite_output_kinds)
    |> Context_free.Rule.extension
  in
  rule t ~implementation ~interface
;;

let attributes t =
  let implementation ~name ~f =
    Context_free.Rule.attr_str_floating_expect
      (Attribute.Floating.declare
         name
         Attribute.Floating.Context.structure_item
         (let open Ast_pattern in
          pstr __)
         (fun x -> x))
      (fun ~ctxt:_ _ -> f none Context.Base ~overwrite_output_kinds:None)
  in
  let interface ~name ~f =
    Context_free.Rule.attr_sig_floating_expect
      (Attribute.Floating.declare
         name
         Attribute.Floating.Context.signature_item
         (let open Ast_pattern in
          pstr __)
         (fun x -> x))
      (fun ~ctxt:_ _ -> [ f none Context.Base ~overwrite_output_kinds:None ])
  in
  rule t ~implementation ~interface
;;

module For_deriving = struct
  type function_ = t

  type t =
    { function_ : function_
    ; overwrite_output_kinds : expression option
    }

  let flag function_ =
    let (module S) = module_ function_ in
    let with_arg =
      Ast_pattern.(
        __
        |> map1 ~f:(fun overwrite_output_kinds ->
          { function_; overwrite_output_kinds = Some overwrite_output_kinds }))
    in
    let no_arg =
      Ast_pattern.(
        lident (string S.name)
        |> pexp_ident
        |> map0 ~f:{ function_; overwrite_output_kinds = None })
    in
    let pattern = Ast_pattern.( ||| ) no_arg with_arg in
    Deriving.Args.arg S.name pattern
  ;;

  let extensions { function_ = t; overwrite_output_kinds } loc type_ ~creator =
    let (module S : Function_types.S) = module_ t in
    let name = "array." ^ S.name in
    let attributes =
      match overwrite_output_kinds with
      | None -> type_.ptyp_attributes
      | Some expr ->
        attribute
          ~loc
          ~name:(Loc.make ~loc output_kinds)
          ~payload:(PStr [ pstr_eval ~loc expr [] ])
        :: type_.ptyp_attributes
    in
    [ creator
        ~loc
        (Loc.make ~loc name, PTyp { type_ with ptyp_attributes = attributes })
        []
    ]
  ;;

  let structure_extensions t loc type_ = extensions t loc type_ ~creator:pstr_extension
  let signature_extensions t loc type_ = extensions t loc type_ ~creator:psig_extension
end
