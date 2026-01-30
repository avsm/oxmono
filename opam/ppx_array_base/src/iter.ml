open! Ppxlib
open! Stdppx

let name = "iter"

let validate_cannot_overwrite_output_kinds =
  Common.validate_cannot_overwrite_output_kinds ~function_name:name
;;

let implementation loc context ~overwrite_output_kinds =
  validate_cannot_overwrite_output_kinds loc ~overwrite_output_kinds;
  let how_to_vary_kinds =
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_or_null_layouts loc)
      ~output:None
  in
  let runtime_fun = Context.runtime_fun context loc in
  [ How_to_vary_kinds.structure_item
      how_to_vary_kinds
      loc
      ~function_name:name
      ~function_implementation:(fun ~input_type ~output_type:(_ : core_type) ->
        [%expr
          fun (t : [%t input_type] array) ~(f : (_ -> _) @ local) ->
            for i = 0 to [%e runtime_fun "length"] t - 1 do
              f ([%e runtime_fun "unsafe_get"] t i)
            done])
  ]
;;

let interface loc context ~overwrite_output_kinds =
  validate_cannot_overwrite_output_kinds loc ~overwrite_output_kinds;
  let how_to_vary_kinds =
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_or_null_layouts loc)
      ~output:None
  in
  How_to_vary_kinds.signature_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_type:(fun ~input_type ~output_type:(_ : core_type) ->
      [%type: [%t input_type] array -> f:([%t input_type] -> unit) @ local -> unit])
;;
