open! Core
open Ppxlib
open Ppx_html_syntax

let test ?html_syntax_module s =
  let open Ppx_html_expander in
  let loc = Location.none in
  let html_syntax_module =
    Option.map html_syntax_module ~f:(fun s -> { txt = Lident s; loc })
  in
  let parsed runtime_kind =
    s
    |> Model_parser.of_string ~loc
    |> Model_code_gen.code ~loc ~html_syntax_module ~runtime_kind
  in
  let js_of_ocaml = Pprintast.string_of_expression (parsed Js_of_ocaml) in
  let kernel = Pprintast.string_of_expression (parsed Kernel) in
  match String.equal js_of_ocaml kernel with
  | true ->
    print_endline "same output between ppx_html and ppx_html_kernel\n";
    print_endline js_of_ocaml
  | false ->
    print_endline "Difference between ppx_html and ppx_html_kernel\n";
    print_endline "PPX_HTML:";
    print_endline js_of_ocaml;
    print_endline "\nPPX_HTML_KERNEL (diff):";
    Expect_test_patdiff.print_patdiff js_of_ocaml kernel
;;
