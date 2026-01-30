open! Core

let xpaths =
  [ "para"
  ; "*"
  ; "text()"
  ; "@name"
  ; "@*"
  ; "para[1]"
  ; "para[last()]"
  ; "*/para"
  ; "/doc/chapter[5]/section[2]"
  ; "chapter//para"
  ; "//para"
  ; "//olist/item"
  ; "."
  ; ".//para"
  ; ".."
  ; "../@lang"
  ; "para[@type=\"warning\"]"
  ; "para[@type=\"warning\"][5]"
  ; "para[5][@type=\"warning\"]"
  ; "chapter[title=\"Introduction\"]"
  ; "chapter[title]"
  ; "employee[@secretary and @assistant]"
  ; "/"
  ; "////" (* This is an error *)
  ; ".////" (* This is an error *)
  ; "//a//" (* This is an error *)
  ; ".//a//" (* This is an error *)
  ; "//a//a" (* This is fine *)
  ; ".//a//a" (* This is fine *)
  ; "preceding::foo[1]"
  ; "(preceding::foo)[1]"
  ]
;;

let%expect_test _ =
  let test input =
    print_endline "--------------------";
    print_endline [%string "input: %{input}"];
    match Xpath.parse_ascii_exn input with
    | exception Failure error -> print_endline [%string "error: %{error}"]
    | expression ->
      print_endline "Abbreviated expression:";
      Xpath.Expression.to_string_abbreviated expression |> print_endline;
      print_endline "Unabbreviated expression:";
      Xpath.Expression.to_string expression |> print_endline
  in
  List.iter xpaths ~f:test;
  [%expect
    {|
    --------------------
    input: para
    Abbreviated expression:
    para
    Unabbreviated expression:
    child::para
    --------------------
    input: *
    Abbreviated expression:
    *
    Unabbreviated expression:
    child::*
    --------------------
    input: text()
    Abbreviated expression:
    text()
    Unabbreviated expression:
    child::text()
    --------------------
    input: @name
    Abbreviated expression:
    @name
    Unabbreviated expression:
    attribute::name
    --------------------
    input: @*
    Abbreviated expression:
    @*
    Unabbreviated expression:
    attribute::*
    --------------------
    input: para[1]
    Abbreviated expression:
    para[1]
    Unabbreviated expression:
    child::para[1]
    --------------------
    input: para[last()]
    Abbreviated expression:
    para[last()]
    Unabbreviated expression:
    child::para[last()]
    --------------------
    input: */para
    Abbreviated expression:
    */para
    Unabbreviated expression:
    child::*/child::para
    --------------------
    input: /doc/chapter[5]/section[2]
    Abbreviated expression:
    /doc/chapter[5]/section[2]
    Unabbreviated expression:
    /child::doc/child::chapter[5]/child::section[2]
    --------------------
    input: chapter//para
    Abbreviated expression:
    chapter//para
    Unabbreviated expression:
    child::chapter/descendant-or-self::node()/child::para
    --------------------
    input: //para
    Abbreviated expression:
    //para
    Unabbreviated expression:
    /descendant-or-self::node()/child::para
    --------------------
    input: //olist/item
    Abbreviated expression:
    //olist/item
    Unabbreviated expression:
    /descendant-or-self::node()/child::olist/child::item
    --------------------
    input: .
    Abbreviated expression:
    .
    Unabbreviated expression:
    self::node()
    --------------------
    input: .//para
    Abbreviated expression:
    .//para
    Unabbreviated expression:
    self::node()/descendant-or-self::node()/child::para
    --------------------
    input: ..
    Abbreviated expression:
    ..
    Unabbreviated expression:
    parent::node()
    --------------------
    input: ../@lang
    Abbreviated expression:
    ../@lang
    Unabbreviated expression:
    parent::node()/attribute::lang
    --------------------
    input: para[@type="warning"]
    Abbreviated expression:
    para[@type = 'warning']
    Unabbreviated expression:
    child::para[attribute::type = 'warning']
    --------------------
    input: para[@type="warning"][5]
    Abbreviated expression:
    para[@type = 'warning'][5]
    Unabbreviated expression:
    child::para[attribute::type = 'warning'][5]
    --------------------
    input: para[5][@type="warning"]
    Abbreviated expression:
    para[5][@type = 'warning']
    Unabbreviated expression:
    child::para[5][attribute::type = 'warning']
    --------------------
    input: chapter[title="Introduction"]
    Abbreviated expression:
    chapter[title = 'Introduction']
    Unabbreviated expression:
    child::chapter[child::title = 'Introduction']
    --------------------
    input: chapter[title]
    Abbreviated expression:
    chapter[title]
    Unabbreviated expression:
    child::chapter[child::title]
    --------------------
    input: employee[@secretary and @assistant]
    Abbreviated expression:
    employee[@secretary or @assistant]
    Unabbreviated expression:
    child::employee[attribute::secretary or attribute::assistant]
    --------------------
    input: /
    Abbreviated expression:
    /
    Unabbreviated expression:
    /
    --------------------
    input: ////
    error: : end_of_input
    --------------------
    input: .////
    error: : end_of_input
    --------------------
    input: //a//
    error: : end_of_input
    --------------------
    input: .//a//
    error: : end_of_input
    --------------------
    input: //a//a
    Abbreviated expression:
    //a//a
    Unabbreviated expression:
    /descendant-or-self::node()/child::a/descendant-or-self::node()/child::a
    --------------------
    input: .//a//a
    Abbreviated expression:
    .//a//a
    Unabbreviated expression:
    self::node()/descendant-or-self::node()/child::a/descendant-or-self::node()/child::a
    --------------------
    input: preceding::foo[1]
    Abbreviated expression:
    preceding::foo[1]
    Unabbreviated expression:
    preceding::foo[1]
    --------------------
    input: (preceding::foo)[1]
    Abbreviated expression:
    (preceding::foo)[1]
    Unabbreviated expression:
    (preceding::foo)[1]
    |}]
;;

let benchmark_params =
  let%bind.List encoding_name, encoding = [ "utf8", `Utf8; "ascii", `Ascii ] in
  let%map.List xpath = xpaths in
  [%string "%{encoding_name} %{xpath}"], (encoding, xpath)
;;

let%bench ("parse path" [@params param = benchmark_params]) =
  match param with
  | `Utf8, input -> Xpath.parse_utf8_exn input
  | `Ascii, input -> Xpath.parse_ascii_exn input
;;
