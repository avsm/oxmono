open Core
open Ppxlib
open Ast_builder.Default

let file_path_to_absolute_string ~loc compile_time_file_path =
  let open (val Ast_builder.make loc) in
  let entire_file = In_channel.with_file compile_time_file_path ~f:In_channel.input_all in
  estring entire_file
;;

let file_path_to_absolute_string_with_filename ~loc compile_time_file_path =
  let entire_file = In_channel.with_file compile_time_file_path ~f:In_channel.input_all in
  [%expr
    [%e estring ~loc:{ loc with loc_ghost = true } compile_time_file_path]
    , [%e estring ~loc entire_file]]
;;

let embed_file_as_string =
  Extension.V3.declare
    "embed_file_as_string"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let embed_file_as_string_with_filename =
  Extension.V3.declare
    "embed_file_as_string_with_filename"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    (fun ~ctxt relative_file_path ->
      file_path_to_absolute_string_with_filename
        ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
        relative_file_path)
  |> Ppxlib.Context_free.Rule.extension
;;

let () =
  Driver.register_transformation
    ~rules:[ embed_file_as_string; embed_file_as_string_with_filename ]
    "ppx_embed_file"
;;
