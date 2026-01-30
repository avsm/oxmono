open! Core
open Ppx_html_syntax.For_testing.Ocaml_parsing

let test string =
  match rsplit_on_hash string with
  | None -> print_endline "no split!"
  | Some (before_hash, after_hash) ->
    Expectable.print [ [%message (before_hash : string) (after_hash : string)] ]
;;

let%expect_test "Normal cases" =
  test {|a|};
  [%expect {| no split! |}];
  test {|a#b|};
  [%expect
    {|
    ┌─────────────┬────────────┐
    │ before_hash │ after_hash │
    ├─────────────┼────────────┤
    │ a           │ b          │
    └─────────────┴────────────┘
    |}];
  test {|EXPR#MODUL|};
  [%expect
    {|
    ┌─────────────┬────────────┐
    │ before_hash │ after_hash │
    ├─────────────┼────────────┤
    │ EXPR        │ MODUL      │
    └─────────────┴────────────┘
    |}]
;;

let%expect_test "Many hashes" =
  test {|a#b#c#d#e#f#g#h#i#j#k|};
  [%expect
    {|
    ┌─────────────────────┬────────────┐
    │ before_hash         │ after_hash │
    ├─────────────────────┼────────────┤
    │ a#b#c#d#e#f#g#h#i#j │ k          │
    └─────────────────────┴────────────┘
    |}]
;;

let%expect_test "Escaping" =
  test {|"Capy#Capy2"|};
  [%expect {| no split! |}];
  test {|"Capy#Capy2"#Actual_capybara|};
  [%expect
    {|
    ┌──────────────┬─────────────────┐
    │ before_hash  │ after_hash      │
    ├──────────────┼─────────────────┤
    │ "Capy#Capy2" │ Actual_capybara │
    └──────────────┴─────────────────┘
    |}]
;;

let%expect_test "Syntax error" =
  (* This has an unclosed quotation mark *)
  test {|"Capy#Capy2" "|};
  [%expect {| no split! |}]
;;
