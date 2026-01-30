open! Ppxlib
open! Stdppx

let name = "fold"

let implementation loc context ~overwrite_output_kinds =
  let runtime_fun = Context.runtime_fun context loc in
  let how_to_vary_kinds =
    let output =
      Option.value overwrite_output_kinds ~default:(How_to_vary_kinds.base_layouts loc)
    in
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output:(Some output)
  in
  [ How_to_vary_kinds.structure_item
      how_to_vary_kinds
      loc
      ~function_name:name
      ~function_implementation:(fun ~input_type ~output_type ->
        [%expr
          fun (t : [%t input_type] array)
            ~init
            ~(f : (_ -> _ -> _) @ local)
            : [%t output_type] ->
            let length = [%e runtime_fun "length"] t in
            let rec loop i acc =
              if i < length
              then
                loop (i + 1) ((f [@inlined hint]) acc ([%e runtime_fun "unsafe_get"] t i))
              else acc
            in
            (loop [@inlined]) 0 init [@nontail]])
  ]
;;

let interface loc context ~overwrite_output_kinds =
  let how_to_vary_kinds =
    let output =
      Option.value overwrite_output_kinds ~default:(How_to_vary_kinds.base_layouts loc)
    in
    Context.how_to_vary_kinds
      context
      ~input:(How_to_vary_kinds.base_layouts loc)
      ~output:(Some output)
  in
  How_to_vary_kinds.signature_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_type:(fun ~input_type ~output_type ->
      [%type:
        [%t input_type] array
        -> init:[%t output_type]
        -> f:([%t output_type] -> [%t input_type] -> [%t output_type]) @ local
        -> [%t output_type]])
;;
