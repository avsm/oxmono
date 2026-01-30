open! Core
module Xml = Simple_xml

let input =
  `String
    {|<root xmlns:bar="http://www.bar.org" xmlns:foo="http://www.foo.org/">
    <actors>
        <actor id="1">Christian Bale</actor>
        <actor id="2">Liam Neeson</actor>
        <actor id="3">Michael Caine</actor>
    </actors>
    <foo:singers>
        <foo:singer id="4">Tom Waits</foo:singer>
        <foo:singer id="5">B.B. King</foo:singer>
        <foo:singer id="6">Ray Charles</foo:singer>
    </foo:singers>
</root>|}
  |> Xml.parse
;;

let context =
  Xpath.Context.create_exn
    ~prefixes:
      (String.Map.of_alist_exn
         [ "foo", "http://www.foo.org/"; "bar", "http://www.bar.org/" ])
;;

let test ?non_standard_qname_name_test_ignores_namespace xpath =
  let expression = Xpath.parse_ascii_exn xpath in
  let value =
    Xpath.run_exn
      (context ?non_standard_qname_name_test_ignores_namespace ())
      input
      expression
  in
  Xpath.Value.to_string_hum
    ~ns_prefix:(function
      | "http://www.foo.org/" -> Some "foo"
      | "http://www.bar.org" -> Some "bar"
      | _ -> None)
    value
  |> print_endline
;;

let%expect_test "Document" =
  test "/";
  [%expect
    {|
    Node_set:
    Root
    |}]
;;

let%expect_test "Root element" =
  test "/root";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <root xmlns:bar="http://www.bar.org" xmlns:foo="http://www.foo.org/">
        <actors>
            <actor id="1">Christian Bale</actor>
            <actor id="2">Liam Neeson</actor>
            <actor id="3">Michael Caine</actor>
        </actors>
        <foo:singers>
            <foo:singer id="4">Tom Waits</foo:singer>
            <foo:singer id="5">B.B. King</foo:singer>
            <foo:singer id="6">Ray Charles</foo:singer>
        </foo:singers>
    </root>
    |}]
;;

let%expect_test "Select all 'actor' elements that are direct children of the 'actors' \
                 element"
  =
  test "/root/actors/actor";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    |}]
;;

let%expect_test "Select all 'singer' elements regardless of their positions in the \
                 document"
  =
  test "//foo:singer";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="4">Tom Waits</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="5">B.B. King</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="6">Ray Charles</foo:singer>
    |}]
;;

let%expect_test "Select the 'id' attributes of the 'singer' elements regardless of their \
                 positions in the document"
  =
  test "//foo:singer/@id";
  [%expect
    {|
    Node_set:
    (attribute((key id)(value 4)))
    (attribute((key id)(value 5)))
    (attribute((key id)(value 6)))
    |}]
;;

let%expect_test "Select the textual value of first 'actor' element" =
  test "//actor[1]/text()";
  [%expect
    {|
    Node_set:
    "Christian Bale"
    |}]
;;

let%expect_test "Select the last 'actor' element" =
  test "//actor[last()]";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    |}]
;;

let%expect_test "Select the first and second 'actor' elements using their position" =
  test "//actor[position() < 3]";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    |}]
;;

let%expect_test "Select all 'actor' elements that have an 'id' attribute" =
  test "//actor[@id]";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    |}]
;;

let%expect_test "Select all 'actor' elements that have an 'id' attribute with value '3'" =
  test "//actor[@id = '3']";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    |}]
;;

let%expect_test "Select all 'actor' elements that have an 'id' attribute with value \
                 lower or equal to '3'"
  =
  test "//actor[@id <= 3]";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    |}]
;;

let%expect_test "Select all the children of the 'singers' node" =
  test "/root/foo:singers/*";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="4">Tom Waits</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="5">B.B. King</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="6">Ray Charles</foo:singer>
    |}]
;;

let%expect_test "Select all the elements in the document" =
  test "//*";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <root xmlns:bar="http://www.bar.org" xmlns:foo="http://www.foo.org/">
        <actors>
            <actor id="1">Christian Bale</actor>
            <actor id="2">Liam Neeson</actor>
            <actor id="3">Michael Caine</actor>
        </actors>
        <foo:singers>
            <foo:singer id="4">Tom Waits</foo:singer>
            <foo:singer id="5">B.B. King</foo:singer>
            <foo:singer id="6">Ray Charles</foo:singer>
        </foo:singers>
    </root>
    <?xml version="1.0" encoding="UTF-8"?>
    <actors>
            <actor id="1">Christian Bale</actor>
            <actor id="2">Liam Neeson</actor>
            <actor id="3">Michael Caine</actor>
        </actors>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singers>
            <foo:singer id="4">Tom Waits</foo:singer>
            <foo:singer id="5">B.B. King</foo:singer>
            <foo:singer id="6">Ray Charles</foo:singer>
        </foo:singers>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="4">Tom Waits</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="5">B.B. King</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="6">Ray Charles</foo:singer>
    |}]
;;

let%expect_test "Select all the 'actor' elements AND the 'singer' elements" =
  test "//actor|//foo:singer";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="1">Christian Bale</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="2">Liam Neeson</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <actor id="3">Michael Caine</actor>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="4">Tom Waits</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="5">B.B. King</foo:singer>
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="6">Ray Charles</foo:singer>
    |}]
;;

let%expect_test "Select the name of the first element in the document" =
  test "name(//*[1])";
  [%expect {| String: root |}]
;;

let%expect_test "Select the numeric value of the 'id' attribute of the first 'actor' \
                 element"
  =
  test "number(//actor[1]/@id)";
  [%expect {| Number: 1 |}]
;;

let%expect_test "Select the string representation value of the 'id' attribute of the \
                 first 'actor' element"
  =
  test "string(//actor[1]/@id)";
  [%expect {| String: 1 |}]
;;

let%expect_test "Select the length of the first 'actor' element's textual value" =
  test "string-length(//actor[1]/text())";
  [%expect {| Number: 14 |}]
;;

let%expect_test "Select the local name of the first 'singer' element, i.e. without the \
                 namespace"
  =
  test "local-name(//foo:singer[1])";
  [%expect {| String: singer |}]
;;

let%expect_test "Select the number of 'singer' elements" =
  test "count(//foo:singer)";
  [%expect {| Number: 3 |}]
;;

let%expect_test "Select the sum of the 'id' attributes of the 'singer' elements" =
  test "sum(//foo:singer/@id)";
  [%expect {| Number: 15 |}]
;;

let%expect_test "Do not ignore namespace, omit prefix" =
  test "//singer[1]";
  [%expect {| Node_set: |}]
;;

let%expect_test "Ignore namespace, omit prefix" =
  test ~non_standard_qname_name_test_ignores_namespace:true "//singer[1]";
  [%expect
    {|
    Node_set:
    <?xml version="1.0" encoding="UTF-8"?>
    <foo:singer id="4">Tom Waits</foo:singer>
    |}]
;;
