open! Core
open! Test_utils

let%expect_test "Literal key" =
  test {|<div key=1234></div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ~key:"1234" []
    |}]
;;

let%expect_test "Explicit quoted key" =
  test {|<div key="a-key"></div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ~key:"a-key" []
    |}]
;;

let%expect_test "Interpolated key" =
  test {|<div key=%{SOME_OCAML_EXPR}></div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ~key:SOME_OCAML_EXPR []
    |}]
;;

let%expect_test "Two keys provided results in error" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div key=%{SOME_OCAML_EXPR} key="something-else"></div>|});
  [%expect {| ("Error: There can only be one key. Please remove this duplicate key.") |}]
;;

let%expect_test "key needs a value" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<div key></div>|});
  [%expect {| ("Error: The attribute key needs a value. (e.g. key=a-unique-key)") |}]
;;

let%expect_test "Interpolated key with module interpolation" =
  test {|<div key=%{SOME_OCAML_EXPR#Foo}></div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ~key:(Foo.to_string SOME_OCAML_EXPR) []
    |}]
;;

let%expect_test "Interpolated tag and interpolated key." =
  test {|<%{INTERPOLATED_TAG} key=%{INTERPOLATED_KEY}></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    INTERPOLATED_TAG ~key:INTERPOLATED_KEY []
    |}]
;;
