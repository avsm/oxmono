open! Ppxlib
open! Stdppx

let raise_unsupported loc ~why =
  Location.raise_errorf ~loc "Unsupported use of ppx_array: %s." why
;;

let raise_function_cannot_vary_output_kinds loc ~function_name =
  raise_unsupported
    loc
    ~why:(Printf.sprintf "function %s cannot vary output kinds" function_name)
;;

let validate_cannot_overwrite_output_kinds loc ~overwrite_output_kinds ~function_name =
  match overwrite_output_kinds with
  | None -> ()
  | Some (_ : expression) -> raise_function_cannot_vary_output_kinds loc ~function_name
;;

let mapper table =
  object
    inherit Ast_traverse.map as super

    method! core_type_desc desc =
      let desc = super#core_type_desc desc in
      match Ppxlib_jane.Shim.Core_type_desc.of_parsetree desc with
      | Ptyp_var (name, jkind_annotation) ->
        (* If multiple different jkinds are given, pick one arbitrarily and let the
           compiler error if they are incompatible *)
        let new_name, jkind_annotation' =
          Hashtbl.find_or_add table name ~default:(fun () ->
            gen_symbol ~prefix:"x" (), jkind_annotation)
        in
        let jkind_annotation =
          match jkind_annotation, jkind_annotation' with
          | (Some _ as jkind_annotation), _ | None, (Some _ as jkind_annotation) ->
            jkind_annotation
          | None, None -> None
        in
        Ppxlib_jane.Shim.Core_type_desc.to_parsetree
          (Ptyp_var (new_name, jkind_annotation))
      | Ptyp_any jkind_annotation ->
        let name = gen_symbol ~prefix:"x" () in
        Hashtbl.set table ~key:name ~data:(name, jkind_annotation);
        Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_var (name, jkind_annotation))
      | _ -> desc
  end
;;

let map_type_variables core_type =
  let table = Hashtbl.create 2 in
  let mapper = mapper table in
  let core_type = mapper#core_type core_type in
  core_type, table |> Hashtbl.to_seq_values |> List.of_seq
;;

let map_type_decl_variables decl =
  let table = Hashtbl.create 2 in
  let mapper = mapper table in
  mapper#type_declaration decl
;;
