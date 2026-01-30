open! Core

(** [Runtime_kind.t] represents the runtime that ppx_html is running in. ppx_html expects
    [Js_of_ocaml] and ppx_html_kernel expects [Kernel]. *)
type t =
  | Js_of_ocaml
  | Kernel
