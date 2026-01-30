open! Base
open! Ppxlib

let ppx_name = "ppx_rope"
let fully_qualified_runtime_module = Lident "Ppx_rope_runtime"

let config_for_rope ~preprocess_before_parsing : Ppx_string.Config.t =
  { fully_qualified_runtime_module
  ; conversion_function_name = "to_string"
  ; preprocess_before_parsing
  ; assert_list_is_stack_allocated = false
  }
;;

let extension extension_name ~preprocess_before_parsing =
  Ppx_string.extension
    ~name:(ppx_name ^ "." ^ extension_name)
    ~config:(config_for_rope ~preprocess_before_parsing)
;;

let () =
  Driver.register_transformation
    ppx_name
    ~extensions:
      [ extension "rope" ~preprocess_before_parsing:None
      ; extension "rope_dedent" ~preprocess_before_parsing:(Some Dedent.string)
      ]
;;
