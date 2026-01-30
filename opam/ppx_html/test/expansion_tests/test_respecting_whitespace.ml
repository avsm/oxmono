open! Core
open Test_utils
(* This file tests that whitespace is respected according to:

   https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace

   1. First, all spaces and tabs immediately before and after a line break are ignored.
   2. Next, all tab characters are handled as space characters.
   3. Next, line breaks are converted to spaces.
   4. After that, any space immediately following another space (even across two separate
      inline elements) is ignored.
   5. And finally, sequences of spaces at the beginning and end of an element are removed.

   We do up to 4 (except the "even across separate inline elements" part...) as doing 5 is
   harder. A minimal viable approach is just not trimming any whitespace, although this
   makes expect tests uglier. Doing up to 4 results in the easiest cleaner expect test
   without needing to do the trimming need to depen on the context of consecutive
   elements.
*)

let%expect_test "Whitespace is respected (no whitespace)" =
  test
    {|
    <div>
      <span>Hello</span>world
    </div>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
      Html_syntax.Node.Primitives.text "world "]
    |}]
;;

let%expect_test "Whitepace is respected (whitespace exists and is not eaten up)" =
  test
    {|
    <div>
      <span>Hello</span> world
    </div>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
      Html_syntax.Node.Primitives.text " world "]
    |}]
;;

let%expect_test "Semi confusingly documented behavior" =
  test
    {|
<div>
<span>Hello</span>
world
</div>
|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
      Html_syntax.Node.Primitives.text " world "]
    |}]
;;

module%test [@name "Handling of whitespace around non-text tags"] _ = struct
  let%expect_test "This is ok" =
    test
      {|
<div><span>Hello</span><span> world</span>world</div>
|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
        Html_syntax.Node.span [Html_syntax.Node.Primitives.text " world"];
        Html_syntax.Node.Primitives.text "world"]
      |}]
  ;;

  let%expect_test "This is ok" =
    test
      {|
<div><span>Hello</span><span>world</span>world</div>
|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
        Html_syntax.Node.span [Html_syntax.Node.Primitives.text "world"];
        Html_syntax.Node.Primitives.text "world"]
      |}]
  ;;

  let%expect_test {|regression: spaces between two spans shouldn't be trimmed|} =
    test
      {|
<div><span>Hello</span> <span>world</span>world</div>
|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
        Html_syntax.Node.Primitives.text " ";
        Html_syntax.Node.span [Html_syntax.Node.Primitives.text "world"];
        Html_syntax.Node.Primitives.text "world"]
      |}]
  ;;

  let%expect_test {|newlines between two elements are ignored|} =
    test
      {|
<div>
  <span>Hello</span>
  <span>world</span>
</div>
|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "Hello"];
        Html_syntax.Node.span [Html_syntax.Node.Primitives.text "world"]]
      |}]
  ;;
end
