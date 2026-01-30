open! Stdppx
include Modes_lib

module Modes = struct
  include Modes

  let default = Modes.of_modals []
  let local = Modes.of_modals [ Local ]
end

module Attr = struct
  let globalized_name = "modes.globalized"
  let globalized_ct = Ppxlib.Attribute.declare_flag globalized_name Core_type
  let globalized_ld = Ppxlib.Attribute.declare_flag globalized_name Label_declaration
end

let of_ast_modes (modes : Ppxlib_jane.modes) =
  let modals = List.map modes ~f:(fun { txt = Mode modal; _ } -> Modal.of_string modal) in
  Modes_lib.Modes.of_modals modals
;;

let to_ast_modes ?(simplify = true) (modes : Modes.t) ~loc =
  let modals =
    if simplify then Modes.to_modals modes else Modes.to_modals_explicit modes
  in
  List.map modals ~f:(fun modal : Ppxlib_jane.mode Ppxlib.loc ->
    { txt = Mode (Modal.to_string modal); loc })
;;

let of_ast_modalities ~mutable_implied (modalities : Ppxlib_jane.modalities) =
  let modals =
    List.map modalities ~f:(fun { txt = Modality modal; _ } -> Modal.of_string modal)
  in
  Modes_lib.Modalities.of_modals ~mutable_implied modals
;;

let to_ast_modalities ?(simplify = true) ~mutable_implied (modalities : Modalities.t) ~loc
  =
  let modals =
    if simplify
    then Modalities.to_modals ~mutable_implied modalities
    else Modalities.to_modals_explicit modalities
  in
  List.map modals ~f:(fun modal : Ppxlib_jane.modality Ppxlib.loc ->
    { txt = Modality (Modal.to_string modal); loc })
;;

let add_global_modality ~loc modalities : Ppxlib_jane.modalities =
  if List.exists
       modalities
       ~f:(fun ({ txt = Modality modality; _ } : Ppxlib_jane.modality Ppxlib.loc) ->
         String.equal modality "global")
  then modalities
  else { txt = Modality "global"; loc } :: modalities
;;

let extract_modalities_from_label_declaration ?(handle_globalized = true) ld =
  let modalities, ld = Ppxlib_jane.Shim.Label_declaration.extract_modalities ld in
  let modalities =
    if handle_globalized
       && (Ppxlib.Attribute.has_flag Attr.globalized_ld ld
           || Ppxlib.Attribute.has_flag Attr.globalized_ct ld.pld_type)
    then add_global_modality ~loc:ld.pld_loc modalities
    else modalities
  in
  of_ast_modalities modalities ~mutable_implied:(Poly.equal ld.pld_mutable Mutable)
;;

let extract_modalities_from_construct_arguments ?(handle_globalized = true) ca =
  let modalities, ct = Ppxlib_jane.Shim.Pcstr_tuple_arg.extract_modalities ca in
  let modalities =
    if handle_globalized && Ppxlib.Attribute.has_flag Attr.globalized_ct ct
    then add_global_modality ~loc:ct.ptyp_loc modalities
    else modalities
  in
  of_ast_modalities modalities ~mutable_implied:false
;;

let pat_modal () =
  let open Ppxlib.Ast_pattern in
  pexp_ident (lident __) |> map1 ~f:(fun ident -> Modal.of_string ident)
;;

let modes_of_modal_power_set (modals : Modal.t list) : Modes.t list =
  let rec loop modals acc =
    match modals with
    | [] -> [ Modes.of_modals acc ]
    | modal :: modals -> loop modals acc @ loop modals (modal :: acc)
  in
  loop modals []
;;

let deriving_arg_modes =
  Ppxlib.Deriving.Args.(
    arg "modes" (elist (pat_modal ()) |> map1 ~f:modes_of_modal_power_set))
;;

let deriving_arg_localize =
  Ppxlib.Deriving.Args.(
    arg
      "localize"
      (pexp_ident (lident (string "localize")) |> map0 ~f:[ Modes.default; Modes.local ]))
;;
