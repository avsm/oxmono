open! Core
open Test_utils

let%expect_test "HTML Quote escaping is handled correctly (implicit)" =
  test {|<div>Capybara's are the world's largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybara's are the world's largest living rodent."]
    |}]
;;

let%expect_test "HTML Quote escaping is handled correctly (implicit)" =
  (* NOTE: We are choosing to keep HTML-based escaping (e.g. &quot;) out of the V1 of
     ppx_html and might consider it in the future. *)
  test {|<div>Capybaras are the world&quot;s largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent."]
    |}]
;;

let%expect_test "HTML Quote escaping is handled correctly (explicit)" =
  (* NOTE: There seems to be a bug in the underlying library (html_escape) that does not
     encode/decode &nbsp's ... We ended up choosing not to include html based
     unescaping... *)
  test {|<div>Capybaras are the world&nbsp;s largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&nbsp;s largest living rodent."]
    |}]
;;
