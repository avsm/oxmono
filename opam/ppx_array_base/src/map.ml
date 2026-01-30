open! Ppxlib
open! Stdppx

let name = "map"

let function_body loc ~create ~lower_bound ~runtime_fun =
  [%expr
    let len = [%e runtime_fun "length"] t in
    if len = 0
    then [||]
    else (
      let r = [%e create] in
      for i = [%e lower_bound] to len - 1 do
        [%e runtime_fun "unsafe_set"] r i (f ([%e runtime_fun "unsafe_get"] t i))
      done;
      r)]
;;

let implement_via_create loc how_to_vary_kinds ~create ~lower_bound ~runtime_fun =
  How_to_vary_kinds.structure_item
    how_to_vary_kinds
    loc
    ~function_name:name
    ~function_implementation:(fun ~input_type ~output_type ->
      [%expr
        fun (t : [%t input_type] array @ local)
          ~(f : (_ -> _) @ local)
          : [%t output_type] array ->
          [%e function_body loc ~create ~lower_bound ~runtime_fun]])
;;

let implementation loc context ~overwrite_output_kinds =
  let how_to_vary_kinds =
    Context.how_to_vary_kinds context ~input:(How_to_vary_kinds.base_layouts loc)
  in
  let runtime_fun = Context.runtime_fun context loc in
  let implement_via_create = implement_via_create loc ~runtime_fun in
  let safe_implementation ~output_kinds =
    implement_via_create
      (how_to_vary_kinds ~output:(Some output_kinds))
      ~create:
        [%expr [%e runtime_fun "create"] ~len (f ([%e runtime_fun "unsafe_get"] t 0))]
      ~lower_bound:[%expr 1]
  in
  match overwrite_output_kinds with
  | None ->
    [ implement_via_create
        (how_to_vary_kinds ~output:(Some [%expr base_non_value]))
        ~create:[%expr [%e runtime_fun "magic_create_uninitialized"] ~len]
        ~lower_bound:[%expr 0]
    ; safe_implementation ~output_kinds:[%expr value, value mod external64]
    ]
  | Some output_kinds -> [ safe_implementation ~output_kinds ]
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
        [%t input_type] array @ local
        -> f:([%t input_type] -> [%t output_type]) @ local
        -> [%t output_type] array])
;;
