open! Core

let () =
  Ppxlib.Driver.register_transformation
    "ppx_html_kernel"
    ~extensions:
      [ Ppx_html_expander.Extension.extension
          ~name:"ppx_html_kernel.html"
          ~runtime_kind:Kernel
          ~experimental_features_allowed:false
      ]
;;
